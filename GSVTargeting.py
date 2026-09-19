import arcpy
import math
import os
import csv
import datetime
from collections import defaultdict

# =========================================================================
# SCRIPT CONFIGURATION & WEIGHTS (AHP + Value Functions)
# =========================================================================
BASE_SIGHTLINE_SCORE = 100
FACADE_BONUS = 460       # Scaled dynamically by PRED_CONFIDENCE
PERPENDICULAR_BONUS = 100 # Overall view framing bonus (Per Visible Point)
BARRIER_PENALTY_MULTIPLIER = 0.20 # 80% score reduction for blocked lines

# Perpendicularity Logic
CONF_PERP_THRESHOLD = 0.764 # Facade must be strictly > 76.4% confident to earn Perp score

# Distance Configurations
DIST_MIN = 3.0   
SEARCH_RADIUS_M = 35.0 # Max distance is tied to search radius
DIST_TOO_CLOSE_PENALTY = 150 # Fixed flat penalty for being under 3m
DISTANCE_PENALTY_MAX = 100 # Linear flat penalty scaling up to 35m

# --- MODIFIER CONFIGURATIONS ---
FOV_MIN_ACCEPTABLE = 20.0
FOV_ABSOLUTE_MIN = 10.0
FOV_MAX_PENALTY_MODIFIER = 0.20  # Linear scaling down to 80% score destruction at FOV 10

FOV_EXCESSIVE_THRESHOLD = 120.0  # Buildings that spill out of the 120-degree GSV max limit
FOV_EXCESSIVE_MODIFIER = 0.60    # 40% score destruction if the building is too wide for the lens

DATE_CUTOFF = datetime.datetime(2021, 9, 1) # Only accept > Aug 2021
DATE_MAX_PENALTY_MODIFIER = 0.70 # Linear scaling down to 30% score destruction at cutoff

# Processing thresholds
AREA_MIN = 24.0 # Minimum building area (sq meters)
AREA_MAX = 900.0 # Maximum building area (sq meters)
DENSIFY_DISTANCE = "0.5 Meters"  
MIN_VISIBLE_POINTS = 2
FOV_BUFFER_MULTIPLIER = 1.2 # Adds 20% width to the final FOV

# =========================================================================
# MAIN PIPELINE FUNCTION
# =========================================================================
def run_gsv_targeting(in_buildings, in_facades, in_gsv, in_barriers, in_districts, 
                      out_workspace, bldg_guid_field, gsv_pano_field, ed_id_fld, ed_name_fld, in_target_buildings=None):
    
    arcpy.env.workspace = out_workspace
    arcpy.env.overwriteOutput = True
    arcpy.env.addOutputsToMap = False 
    
    # --- LOCK BUSTER ---
    arcpy.AddMessage("Executing Lock Buster (Clearing old map layers)...")
    try:
        aprx = arcpy.mp.ArcGISProject("CURRENT")
        active_map = aprx.activeMap
        if active_map:
            for lyr in active_map.listLayers():
                if lyr.name.startswith("step"):
                    active_map.removeLayer(lyr)
    except Exception: pass 
    
    sr_itm = arcpy.SpatialReference(2157)
    sr_wgs84 = arcpy.SpatialReference(4326)
    arcpy.env.outputCoordinateSystem = sr_itm

    arcpy.AddMessage("Starting HIGH-PERFORMANCE GSV Targeting Pipeline...")

    # ---------------------------------------------------------
    # PHASE 0 & 0.5: Filter Buildings & Tag Electoral Districts
    # ---------------------------------------------------------
    arcpy.AddMessage("Phase 0 & 0.5: Processing Target Buildings and Tagging Districts...")
    
    arcpy.management.MakeFeatureLayer(in_buildings, "all_bldgs_lyr_global")
    
    if in_target_buildings and str(in_target_buildings).strip() != "" and arcpy.Exists(in_target_buildings):
        arcpy.AddMessage("--> Optional Target Buildings layer detected. Isolating FOVs to targeted sites only...")
        arcpy.management.MakeFeatureLayer(in_target_buildings, "target_bldgs_lyr", f"Shape_Area >= {AREA_MIN} And Shape_Area <= {AREA_MAX}")
    else:
        arcpy.AddMessage("--> No Target Buildings layer provided. Calculating FOVs for ALL buildings in the main layer...")
        arcpy.management.MakeFeatureLayer(in_buildings, "target_bldgs_lyr", f"Shape_Area >= {AREA_MIN} And Shape_Area <= {AREA_MAX}")

    target_buildings = arcpy.management.CopyFeatures("target_bldgs_lyr", "step00_TargetBuildings")
    
    arcpy.management.AddField(target_buildings, "Chunk_ED_ID", "TEXT")
    
    sj_bldgs = r"memory\sj_bldgs_ed"
    arcpy.analysis.SpatialJoin(target_buildings, in_districts, sj_bldgs, "JOIN_ONE_TO_ONE", "KEEP_ALL", match_option="INTERSECT")
    
    ed_lookup = {}
    unique_eds = set()
    
    with arcpy.da.SearchCursor(sj_bldgs, [bldg_guid_field, ed_id_fld, ed_name_fld]) as cursor:
        for row in cursor:
            ed_id_val = str(row[1]) if row[1] else "Unknown_ID"
            ed_name_val = str(row[2]) if row[2] else "Unknown_ED"
            ed_lookup[row[0]] = (ed_id_val, ed_name_val)
            unique_eds.add(ed_id_val)
            
    with arcpy.da.UpdateCursor(target_buildings, [bldg_guid_field, "Chunk_ED_ID"]) as cursor:
        for row in cursor:
            row[1] = ed_lookup.get(row[0], ("Unknown_ID", "Unknown_ED"))[0]
            cursor.updateRow(row)

    # ---------------------------------------------------------
    # PHASE 0.8: Pre-Filter GSV Points & Parse Native Dates
    # ---------------------------------------------------------
    arcpy.AddMessage(f"Phase 0.8: Pre-Filtering GSV dates and isolating {SEARCH_RADIUS_M}m spatial boundaries...")
    arcpy.management.MakeFeatureLayer(in_gsv, "gsv_lyr_global")
    arcpy.management.SelectLayerByLocation("gsv_lyr_global", "WITHIN_A_DISTANCE", target_buildings, f"{SEARCH_RADIUS_M} Meters")
    gsv_active = arcpy.management.CopyFeatures("gsv_lyr_global", r"memory\GSV_Active")
    
    gsv_lookup = {}
    gsv_dates = []
    
    with arcpy.da.SearchCursor(gsv_active, ["OID@", gsv_pano_field, "Date_Converted"]) as cursor:
        for row in cursor:
            pano_id = row[1]
            dt_obj = row[2]
            
            if dt_obj is None: 
                continue
                
            if dt_obj < DATE_CUTOFF:
                continue 
                
            gsv_dates.append(dt_obj)
            gsv_lookup[row[0]] = (pano_id, dt_obj)

    if not gsv_dates:
        arcpy.AddError(f"Zero GSV points found strictly after {DATE_CUTOFF.strftime('%Y-%m-%d')} within the target area! Pipeline Aborted.")
        return
        
    max_gsv_date = max(gsv_dates)
    max_days_spread = float((max_gsv_date - DATE_CUTOFF).days)
    if max_days_spread <= 0: max_days_spread = 1.0 

    # ---------------------------------------------------------
    # PREPARE OUTPUT FEATURE CLASSES & CSV
    # ---------------------------------------------------------
    arcpy.AddMessage("Initializing Output Schemas...")
    
    out_lines = arcpy.management.CreateFeatureclass(out_workspace, "step08_WinningSightlines", "POLYLINE", spatial_reference=sr_itm)
    arcpy.management.AddFields(out_lines, [["Bldg_GUID", "TEXT"], ["PanoID", "TEXT"], ["Score", "DOUBLE"]])
    
    # NEW SCHEMA: Added Actual_FOV
    out_cand_triangles = arcpy.management.CreateFeatureclass(out_workspace, "step09_ALL_Candidate_Triangles", "POLYGON", spatial_reference=sr_itm)
    arcpy.management.AddFields(out_cand_triangles, [
        ["Bldg_GUID", "TEXT"], ["ED_ID", "TEXT"], ["ED_English", "TEXT"], ["PanoID", "TEXT"], ["Pano_Date", "TEXT"], 
        ["Total_Score", "DOUBLE"], ["Pre_Mod_Score", "DOUBLE"], ["FOV_Mod", "DOUBLE"], ["Date_Mod", "DOUBLE"],
        ["Winning_Score", "DOUBLE"], ["Norm_Ratio", "DOUBLE"], ["Overall_Perp_Score", "DOUBLE"], 
        ["Median_Dist", "DOUBLE"], ["Heading", "DOUBLE"], ["Actual_FOV", "DOUBLE"], ["FOV", "DOUBLE"], ["GSV_Link", "TEXT", "", 500] 
    ])
    
    out_win_triangles = arcpy.management.CreateFeatureclass(out_workspace, "step10_Winning_FOV_Triangles", "POLYGON", spatial_reference=sr_itm)
    arcpy.management.AddFields(out_win_triangles, [
        ["Bldg_GUID", "TEXT"], ["ED_ID", "TEXT"], ["ED_English", "TEXT"], ["PanoID", "TEXT"], ["Pano_Date", "TEXT"], 
        ["Total_Score", "DOUBLE"], ["Pre_Mod_Score", "DOUBLE"], ["FOV_Mod", "DOUBLE"], ["Date_Mod", "DOUBLE"],
        ["Overall_Perp_Score", "DOUBLE"], ["Median_Dist", "DOUBLE"], ["Heading", "DOUBLE"], ["Actual_FOV", "DOUBLE"], ["FOV", "DOUBLE"], ["GSV_Link", "TEXT", "", 500]
    ])
    
    out_cand_pts = arcpy.management.CreateFeatureclass(out_workspace, "step11_ALL_Candidate_Target_Points", "POINT", spatial_reference=sr_itm)
    arcpy.management.AddFields(out_cand_pts, [["Bldg_GUID", "TEXT"], ["ED_ID", "TEXT"], ["ED_English", "TEXT"], ["PanoID", "TEXT"], ["Total_Score", "DOUBLE"], ["Winning_Score", "DOUBLE"], ["Norm_Ratio", "DOUBLE"]])
    
    out_win_pts = arcpy.management.CreateFeatureclass(out_workspace, "step12_Winning_Target_Points", "POINT", spatial_reference=sr_itm)
    arcpy.management.AddFields(out_win_pts, [["Bldg_GUID", "TEXT"], ["ED_ID", "TEXT"], ["ED_English", "TEXT"], ["PanoID", "TEXT"], ["Total_Score", "DOUBLE"]])
    
    csv_path = os.path.join(os.path.dirname(out_workspace), "GSV_Targeting_Output.csv")
    csv_file = open(csv_path, mode='w', newline='', encoding='utf-8')
    csv_writer = csv.writer(csv_file)
    csv_writer.writerow([
        "Bldg_GUID", "ED_ID", "ED_English", "PanoID", "Date", "Cam_Lat", "Cam_Lon", 
        "Target_Lat", "Target_Lon", "Heading", "Actual_FOV", "FOV", "Median_Dist_m", 
        "Pre_Mod_Score", "FOV_Mod", "Date_Mod", "Total_Score", "Overall_Perp_Score", "GSV_Link"
    ])


    # =========================================================================
    # THE CHUNK LOOP
    # =========================================================================
    total_eds = len(unique_eds)
    arcpy.management.MakeFeatureLayer(target_buildings, "loop_bldgs_lyr")
    
    for idx, ed_id in enumerate(unique_eds, 1):
        arcpy.AddMessage(f"Processing Electoral District {idx}/{total_eds} (ID: {ed_id})...")
        
        # 1. Isolate the Target Chunk
        arcpy.management.SelectLayerByAttribute("loop_bldgs_lyr", "NEW_SELECTION", f"Chunk_ED_ID = '{ed_id}'")
        if int(arcpy.management.GetCount("loop_bldgs_lyr")[0]) == 0: continue
        arcpy.management.CopyFeatures("loop_bldgs_lyr", r"memory\chk_bldgs")
        
        # 2. Extract Ghost Obstacles
        arcpy.management.SelectLayerByLocation("all_bldgs_lyr_global", "WITHIN_A_DISTANCE", r"memory\chk_bldgs", f"{SEARCH_RADIUS_M + 5} Meters")
        arcpy.management.CopyFeatures("all_bldgs_lyr_global", r"memory\chk_all_bldgs")
        arcpy.analysis.Buffer(r"memory\chk_all_bldgs", r"memory\chk_shrunk", "-0.01 Meters")
        
        # 3. Densify Target Facades
        arcpy.management.PolygonToLine(r"memory\chk_bldgs", r"memory\chk_lines", "IGNORE_NEIGHBORS")
        arcpy.management.GeneratePointsAlongLines(r"memory\chk_lines", r"memory\chk_pts", "DISTANCE", DENSIFY_DISTANCE)
        arcpy.analysis.SpatialJoin(r"memory\chk_pts", in_facades, r"memory\chk_tagged", "JOIN_ONE_TO_ONE", "KEEP_ALL", match_option="INTERSECT", search_radius="0.1 Meters")
        
        arcpy.management.CalculateField(r"memory\chk_tagged", "Is_Facade", "1 if !PRED_CONFIDENCE! else 0", "PYTHON3", field_type="SHORT")
        arcpy.management.CalculateField(r"memory\chk_tagged", "Facade_Conf", "(!PRED_CONFIDENCE! / 100.0) if !PRED_CONFIDENCE! and !PRED_CONFIDENCE! > 1 else (!PRED_CONFIDENCE! if !PRED_CONFIDENCE! else 0)", "PYTHON3", field_type="DOUBLE")
        
        # 4. Party Wall Drop
        arcpy.analysis.GenerateNearTable(r"memory\chk_tagged", r"memory\chk_bldgs", r"memory\chk_near1", "0.1 Meters", "NO_LOCATION", "NO_ANGLE", "ALL")
        pt_touches = defaultdict(int)
        for row in arcpy.da.SearchCursor(r"memory\chk_near1", ["IN_FID"]): pt_touches[row[0]] += 1
        
        pt_lookup = {}
        with arcpy.da.UpdateCursor(r"memory\chk_tagged", ["OID@", bldg_guid_field, "Is_Facade", "Facade_Conf", "Wall_Bearing"]) as cursor:
            for row in cursor:
                if pt_touches[row[0]] > 1: cursor.deleteRow()
                else: pt_lookup[row[0]] = (row[1], row[2], row[3], row[4])
                
        # 5. Generate Sightlines
        arcpy.analysis.GenerateNearTable(gsv_active, r"memory\chk_tagged", r"memory\chk_near_gsv", f"{SEARCH_RADIUS_M} Meters", "LOCATION", "NO_ANGLE", "ALL")
        
        arcpy.management.AddField(r"memory\chk_near_gsv", "Line_ID", "LONG")
        arcpy.management.CalculateField(r"memory\chk_near_gsv", "Line_ID", "!OBJECTID!", "PYTHON3")
        
        arcpy.management.XYToLine(r"memory\chk_near_gsv", r"memory\chk_raw_lines", "FROM_X", "FROM_Y", "NEAR_X", "NEAR_Y", "PLANAR", "Line_ID", sr_itm)
        
        # 6. Fast Occlusion (Against Ghost Obstacles)
        arcpy.management.MakeFeatureLayer(r"memory\chk_raw_lines", "raw_lines_lyr")
        arcpy.management.SelectLayerByLocation("raw_lines_lyr", "INTERSECT", r"memory\chk_shrunk")
        occluded_ids = set([row[0] for row in arcpy.da.SearchCursor("raw_lines_lyr", ["Line_ID"])])
        
        arcpy.management.SelectLayerByLocation("raw_lines_lyr", "INTERSECT", in_barriers)
        barrier_ids = set([row[0] for row in arcpy.da.SearchCursor("raw_lines_lyr", ["Line_ID"])])
        arcpy.management.SelectLayerByAttribute("raw_lines_lyr", "CLEAR_SELECTION")

        # 7. Line-Level Scoring Math
        bldg_gsv_groups = defaultdict(lambda: {"line_scores": 0, "dists": [], "pts": [], "gsv": None, "date": None, "is_facs": [], "f_confs": [], "wall_bearings": [], "winning_line_tuples": []})
        
        search_flds = ["Line_ID", "IN_FID", "NEAR_FID", "NEAR_DIST", "FROM_X", "FROM_Y", "NEAR_X", "NEAR_Y"]
        
        for row in arcpy.da.SearchCursor(r"memory\chk_near_gsv", search_flds):
            line_id, gsv_oid, pt_oid, dist, fx, fy, nx, ny = row
            
            if line_id in occluded_ids: continue
            
            b_guid, is_fac, f_conf, wb = pt_lookup.get(pt_oid, (None, 0, 0, None))
            if not b_guid: continue
            
            gsv_data = gsv_lookup.get(gsv_oid)
            if not gsv_data: continue 
            p_id, p_date_obj = gsv_data
            
            score_facade = (FACADE_BONUS * f_conf) if is_fac == 1 else 0
            pre_dist_score = BASE_SIGHTLINE_SCORE + score_facade
            
            if dist < DIST_MIN: 
                score_dist = -DIST_TOO_CLOSE_PENALTY
            else: 
                score_dist = -(DISTANCE_PENALTY_MAX * ((dist - DIST_MIN) / (SEARCH_RADIUS_M - DIST_MIN)))
                
            line_final_score = pre_dist_score + score_dist
            
            if line_id in barrier_ids: line_final_score *= BARRIER_PENALTY_MULTIPLIER
            
            key = (b_guid, p_id)
            grp = bldg_gsv_groups[key]
            grp["line_scores"] += line_final_score
            grp["dists"].append(dist)
            grp["pts"].append(arcpy.Point(nx, ny))
            grp["gsv"] = arcpy.Point(fx, fy)
            grp["date"] = p_date_obj
            grp["is_facs"].append(is_fac)
            grp["f_confs"].append(f_conf)
            grp["wall_bearings"].append(wb)
            grp["winning_line_tuples"].append((fx, fy, nx, ny, line_final_score))

        # 8. Holistic Modifiers and Winners
        winning_gsv_per_bldg = {}
        all_candidates = [] 
        
        for (bldg_guid, pano_id), data in bldg_gsv_groups.items():
            if len(data["pts"]) < MIN_VISIBLE_POINTS: continue
            
            dists = sorted(data["dists"])
            mid = len(dists) // 2
            median_dist = dists[mid] if len(dists) % 2 != 0 else (dists[mid-1] + dists[mid]) / 2.0
            
            pts, gsv = data["pts"], data["gsv"]
            bearings = []
            sum_sin, sum_cos = 0, 0
            
            for pt in pts:
                dx, dy = pt.X - gsv.X, pt.Y - gsv.Y
                brg = (math.degrees(math.atan2(dx, dy)) + 360) % 360
                bearings.append({"pt": pt, "brg": brg})
                rad = math.radians(brg)
                sum_sin += math.sin(rad)
                sum_cos += math.cos(rad)
                
            mean_brg = (math.degrees(math.atan2(sum_sin, sum_cos)) + 360) % 360
            left_pt, right_pt = None, None
            min_diff, max_diff = float('inf'), float('-inf')
            
            for item in bearings:
                diff = (item["brg"] - mean_brg + 180) % 360 - 180
                if diff < min_diff: min_diff, left_pt = diff, item["pt"]
                if diff > max_diff: max_diff, right_pt = diff, item["pt"]
                    
            heading = (mean_brg + (min_diff + max_diff) / 2.0 + 360) % 360
            
            # --- FOV DECOUPLING ---
            actual_fov = (max_diff - min_diff) * FOV_BUFFER_MULTIPLIER
            clamped_fov = max(10, min(120, actual_fov))
            
            mid_x, mid_y = (left_pt.X + right_pt.X) / 2.0, (left_pt.Y + right_pt.Y) / 2.0
            target_pt = arcpy.Point(mid_x, mid_y)
            gsv_link = f"https://www.google.com/maps/@?api=1&map_action=pano&pano={pano_id}&heading={round(heading, 2)}&pitch=0&fov={round(clamped_fov, 2)}"
            
            valid_f_brgs = [data["wall_bearings"][i] for i, is_fac in enumerate(data["is_facs"]) if is_fac == 1 and data["f_confs"][i] > CONF_PERP_THRESHOLD and data["wall_bearings"][i] is not None]
            if valid_f_brgs:
                s_sin, s_cos = sum(math.sin(math.radians(b)) for b in valid_f_brgs), sum(math.cos(math.radians(b)) for b in valid_f_brgs)
                mean_wb = (math.degrees(math.atan2(s_sin, s_cos)) + 360) % 360
                camera_bearing = (math.degrees(math.atan2(mid_x - gsv.X, mid_y - gsv.Y)) + 360) % 360
                diff = abs(camera_bearing - mean_wb) % 180
                if diff > 90: diff = 180 - diff
                overall_perp_score = PERPENDICULAR_BONUS * (diff / 90.0) * len(pts)
            else: overall_perp_score = 0.0
                
            pre_mod_score = data["line_scores"] + overall_perp_score
            
            # --- MODIFIER MATH ---
            # 1. Check for Excessive FOV Penalty (The 120-degree spillover)
            if actual_fov > FOV_EXCESSIVE_THRESHOLD:
                fov_mod = FOV_EXCESSIVE_MODIFIER
            # 2. Check for Narrow FOV Penalty
            elif clamped_fov >= FOV_MIN_ACCEPTABLE:
                fov_mod = 1.0
            else:
                penalty_ratio = (FOV_MIN_ACCEPTABLE - clamped_fov) / (FOV_MIN_ACCEPTABLE - FOV_ABSOLUTE_MIN)
                fov_mod = 1.0 - ((1.0 - FOV_MAX_PENALTY_MODIFIER) * (penalty_ratio**2))
                fov_mod = max(FOV_MAX_PENALTY_MODIFIER, fov_mod)
                
            # Date Linear Modifier
            img_dt = data["date"]
            date_ratio = (img_dt - DATE_CUTOFF).days / max_days_spread
            date_ratio = max(0.0, min(1.0, date_ratio))
            date_mod = DATE_MAX_PENALTY_MODIFIER + ((1.0 - DATE_MAX_PENALTY_MODIFIER) * date_ratio)
            
            final_total_score = pre_mod_score * fov_mod * date_mod
            polygon = arcpy.Polygon(arcpy.Array([gsv, left_pt, right_pt, gsv]), sr_itm)
            
            all_candidates.append({
                "bldg": bldg_guid, "pano": pano_id, "date_obj": img_dt, "pre_mod": pre_mod_score, 
                "fov_mod": fov_mod, "date_mod": date_mod, "final_sc": final_total_score, 
                "geom": polygon, "heading": heading, "actual_fov": actual_fov, "fov": clamped_fov, 
                "dist": median_dist, "perp": overall_perp_score, "gsv_link": gsv_link, "target_pt": target_pt
            })
            
            if bldg_guid not in winning_gsv_per_bldg or final_total_score > winning_gsv_per_bldg[bldg_guid]["final_score"]:
                winning_gsv_per_bldg[bldg_guid] = {
                    "pano_id": pano_id, "date_obj": img_dt, "pre_mod": pre_mod_score,
                    "fov_mod": fov_mod, "date_mod": date_mod, "final_score": final_total_score, 
                    "gsv_pt": gsv, "median_dist": median_dist, "heading": heading, 
                    "actual_fov": actual_fov, "fov": clamped_fov, 
                    "geom": polygon, "perp": overall_perp_score, "gsv_link": gsv_link, 
                    "target_pt": target_pt, "win_line_tuples": data["winning_line_tuples"]
                }

        # ---------------------------------------------------------
        # 9. Write Data via Sequential Cursors (Chunk by Chunk)
        # ---------------------------------------------------------
        
        with arcpy.da.InsertCursor(out_cand_triangles, ["Bldg_GUID", "ED_ID", "ED_English", "PanoID", "Pano_Date", "Total_Score", "Pre_Mod_Score", "FOV_Mod", "Date_Mod", "Winning_Score", "Norm_Ratio", "Overall_Perp_Score", "Median_Dist", "Heading", "Actual_FOV", "FOV", "GSV_Link", "SHAPE@"]) as cur:
            for c in all_candidates:
                b_guid = c["bldg"]
                ed_id_str, ed_name_str = ed_lookup.get(b_guid, (ed_id, "Unknown ED"))
                win_score = winning_gsv_per_bldg[b_guid]["final_score"]
                norm_ratio = (c["final_sc"] / win_score) if win_score != 0 else 0.0
                date_str = c["date_obj"].strftime("%Y-%m-%d")
                cur.insertRow([b_guid, ed_id_str, ed_name_str, c["pano"], date_str, c["final_sc"], c["pre_mod"], c["fov_mod"], c["date_mod"], win_score, norm_ratio, c["perp"], c["dist"], c["heading"], c["actual_fov"], c["fov"], c["gsv_link"], c["geom"]])
                
        with arcpy.da.InsertCursor(out_cand_pts, ["Bldg_GUID", "ED_ID", "ED_English", "PanoID", "Total_Score", "Winning_Score", "Norm_Ratio", "SHAPE@"]) as cur:
            for c in all_candidates:
                b_guid = c["bldg"]
                ed_id_str, ed_name_str = ed_lookup.get(b_guid, (ed_id, "Unknown ED"))
                win_score = winning_gsv_per_bldg[b_guid]["final_score"]
                norm_ratio = (c["final_sc"] / win_score) if win_score != 0 else 0.0
                cur.insertRow([b_guid, ed_id_str, ed_name_str, c["pano"], c["final_sc"], win_score, norm_ratio, c["target_pt"]])

        with arcpy.da.InsertCursor(out_win_triangles, ["Bldg_GUID", "ED_ID", "ED_English", "PanoID", "Pano_Date", "Total_Score", "Pre_Mod_Score", "FOV_Mod", "Date_Mod", "Overall_Perp_Score", "Median_Dist", "Heading", "Actual_FOV", "FOV", "GSV_Link", "SHAPE@"]) as cur:
             for b_guid, d in winning_gsv_per_bldg.items():
                ed_id_str, ed_name_str = ed_lookup.get(b_guid, (ed_id, "Unknown ED"))
                date_str = d["date_obj"].strftime("%Y-%m-%d")
                cur.insertRow([b_guid, ed_id_str, ed_name_str, d["pano_id"], date_str, d["final_score"], d["pre_mod"], d["fov_mod"], d["date_mod"], d["perp"], d["median_dist"], d["heading"], d["actual_fov"], d["fov"], d["gsv_link"], d["geom"]])
                
        with arcpy.da.InsertCursor(out_win_pts, ["Bldg_GUID", "ED_ID", "ED_English", "PanoID", "Total_Score", "SHAPE@"]) as cur:
             for b_guid, d in winning_gsv_per_bldg.items():
                ed_id_str, ed_name_str = ed_lookup.get(b_guid, (ed_id, "Unknown ED"))
                cur.insertRow([b_guid, ed_id_str, ed_name_str, d["pano_id"], d["final_score"], d["target_pt"]])

        with arcpy.da.InsertCursor(out_lines, ["Bldg_GUID", "PanoID", "Score", "SHAPE@"]) as cur:
            for b_guid, d in winning_gsv_per_bldg.items():
                for fx, fy, nx, ny, l_score in d["win_line_tuples"]:
                    line_geom = arcpy.Polyline(arcpy.Array([arcpy.Point(fx, fy), arcpy.Point(nx, ny)]), sr_itm)
                    cur.insertRow([b_guid, d["pano_id"], l_score, line_geom])
            
        for b_guid, d in winning_gsv_per_bldg.items():
            ed_id_str, ed_name_str = ed_lookup.get(b_guid, (ed_id, "Unknown ED"))
            date_str = d["date_obj"].strftime("%Y-%m-%d")
            c_wgs = arcpy.PointGeometry(d["gsv_pt"], sr_itm).projectAs(sr_wgs84, "IRENET95_To_WGS_1984_1").firstPoint
            t_wgs = arcpy.PointGeometry(d["target_pt"], sr_itm).projectAs(sr_wgs84, "IRENET95_To_WGS_1984_1").firstPoint
            csv_writer.writerow([b_guid, ed_id_str, ed_name_str, d["pano_id"], date_str, round(c_wgs.Y, 6), round(c_wgs.X, 6), round(t_wgs.Y, 6), round(t_wgs.X, 6), round(d["heading"], 2), round(d["actual_fov"], 2), round(d["fov"], 2), round(d["median_dist"], 2), round(d["pre_mod"], 2), round(d["fov_mod"], 3), round(d["date_mod"], 3), round(d["final_score"], 2), round(d["perp"], 2), d["gsv_link"]])

        # Aggressive Memory Cleanup
        for temp_layer in ["chk_bldgs", "chk_all_bldgs", "chk_shrunk", "chk_lines", "chk_pts", "chk_tagged", "chk_near1", "chk_near_gsv", "chk_raw_lines"]:
            try: arcpy.management.Delete(rf"memory\{temp_layer}")
            except: pass

    # =========================================================================
    # WRAP UP & MAP DRAWING
    # =========================================================================
    csv_file.close()
    
    arcpy.AddMessage("Phase 8: Adding generated layers to the active map...")
    try:
        aprx = arcpy.mp.ArcGISProject("CURRENT")
        active_map = aprx.activeMap
        if active_map:
            try: group_lyr = active_map.createGroupLayer("GSV Targeting Outputs")
            except AttributeError: group_lyr = None 
                
            layers_to_add = ["step12_Winning_Target_Points", "step11_ALL_Candidate_Target_Points", "step10_Winning_FOV_Triangles", "step09_ALL_Candidate_Triangles", "step08_WinningSightlines", "step00_TargetBuildings"]
            
            for fc_name in layers_to_add:
                fc_path = os.path.join(out_workspace, fc_name)
                if arcpy.Exists(fc_path):
                    lyr = active_map.addDataFromPath(fc_path)
                    if group_lyr:
                        active_map.addLayerToGroup(group_lyr, lyr, "TOP")
                        active_map.removeLayer(lyr)
    except Exception as e:
        arcpy.AddWarning(f"Note: Layers are in your Geodatabase, but could not auto-add to map: {e}")
        
    arcpy.AddMessage(f"Pipeline Complete! Output CSV saved to: {csv_path}")

if __name__ == '__main__':
    in_bldgs = arcpy.GetParameterAsText(0)
    in_facades = arcpy.GetParameterAsText(1)
    in_gsv = arcpy.GetParameterAsText(2)
    in_barriers = arcpy.GetParameterAsText(3)
    out_gdb = arcpy.GetParameterAsText(4)
    bldg_id_fld = arcpy.GetParameterAsText(5)
    pano_id_fld = arcpy.GetParameterAsText(6)
    in_districts = arcpy.GetParameterAsText(7) 
    ed_id_fld = arcpy.GetParameterAsText(8)
    ed_name_fld = arcpy.GetParameterAsText(9)
    in_target_buildings = arcpy.GetParameterAsText(10)
    
    run_gsv_targeting(in_bldgs, in_facades, in_gsv, in_barriers, in_districts, out_gdb, bldg_id_fld, pano_id_fld, ed_id_fld, ed_name_fld, in_target_buildings)