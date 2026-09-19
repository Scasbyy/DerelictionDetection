import arcpy
import math
import os

# =========================================================================
# SCRIPT CONFIGURATION & PARAMETERS
# VERSION: 4.0 (Global Obstacle Fix & Dual GUID Preservation)
# Edit these values directly. No ArcGIS Pro Toolbox parameters needed.
# =========================================================================

# 1. Area Constraints
MIN_AREA = 24.0            # Minimum building footprint area (sqm)
MAX_AREA = 900.0           # Maximum building footprint area (sqm)

# 2. Topology & Adjacency Tolerances
TOUCH_TOL = 0.5            # Distance to consider buildings "attached" (m)
SNAP_TOL = 0.5             # Distance to snap T-junctions/Cul-de-sacs (m)

# 3. Search & Exclusion Distances
ROAD_SEARCH_DIST = 50.0    # Max distance to search for a road (m)
CUL_DE_SAC_RAD = 1.5       # Radius to exclude dead-end snapping (m)
AMBIGUITY_RADIUS = 25.0    # Primary radius to check for conflicting roads (m)

# 4. Raycast & Facade Scoring
FACADE_ANGLE_TOL = 30.0      # Max angle diff to be considered "parallel" (Degrees)
RAYCAST_INTERVAL = 1.0       # Spacing between sightline lasers (m)
RECOV_MIN_SHIFT = 25.0       # Min angle diff required to abandon a bad road (Degrees)
LOW_ANGLE_CONFIDENCE = 0.75  # Threshold to trigger alternative road search (0.0 to 1.0)
ANGLE_RECOVERY_RADIUS = 35.0 # Search radius for low angle confidence recovery (m)
ANGLE_RECOVERY_TARGET = 0.95 # Target angle score to accept new road segment (0.0 to 1.0)

# 5. Terrace End-Cap Logic
MIN_TERRACE = 3            # Minimum units to classify as a terrace block
TERRACE_TOL = 30.0         # Angle tolerance for end-cap alignment (Degrees)
TERRACE_PEN_ANGLE = 25.0   # Angle diff to flag a divergent terrace building (Degrees)

# 6. Roundabout Exclusion
ROUNDABOUT_SQL = "junction = 'roundabout'"        # SQL to identify roundabouts (e.g., "FormOfWay = 'Roundabout'")
# =========================================================================

# ==========================================
# CONFIDENCE METRIC ENGINE
# ==========================================
def calculate_confidence(angle, raycast, dist, ambig, touch_count, is_occ, used_r2, used_p3, is_terrace, peer_score, fail_reason):
    # 1. Hard filter: If it failed completely, confidence is strictly 0
    if fail_reason and fail_reason.strip() != "":
        return 0.0
        
    # 2. Calculate the Log-Odds (L) using the final bootstrapped coefficients
    L = (-15.524467) + \
        (angle * 8.703318) + \
        (raycast * 3.735590) + \
        (dist * 3.328981) + \
        (is_occ * -2.901840) + \
        (used_p3 * -1.149311) + \
        (is_terrace * 1.457412) + \
        (peer_score * 2.377360)
        
    # 3. Convert Log-Odds to a Probability using the Sigmoid function
    probability = 1 / (1 + math.exp(-L))
    
    return probability


def extract_facades(buildings, roads, output_facades, out_centroids, bldg_guid_field, road_guid_field, save_intermediates):
    arcpy.env.overwriteOutput = True
    arcpy.AddMessage("Starting Self-Healing Centroid Front Facade Pipeline (Version 4.0 - Global Obstacles)...")
    
    score_threshold = round(2.0 - (FACADE_ANGLE_TOL / 90.0), 4)
    diag_capture_radius = "0.5 Meters"
    shrink_buffer = "-0.01 Meters"

    out_gdb = os.path.dirname(output_facades)
    sr = arcpy.Describe(roads).spatialReference

    # ==========================================
    # GLOBAL CODE BLOCKS (For Reusability)
    # ==========================================
    road_bearing_code = """def get_bearing(shape):
        if not shape: return 0
        p1, p2 = shape.firstPoint, shape.lastPoint
        dX, dY = p2.X - p1.X, p2.Y - p1.Y
        angle = math.degrees(math.atan2(dX, dY))
        return angle if angle >= 0 else angle + 360"""

    score_code = """def get_score(wall, road):
        if wall is None or road is None: return 1.0
        diff = abs(wall - road) % 180
        if diff > 90: diff = 180 - diff
        return 2.0 - (diff / 90.0)"""

    # ==========================================
    # MAP GROUPING SETUP
    # ==========================================
    active_map = None
    group_layer = None
    occ_group_layer = None

    if save_intermediates:
        try:
            aprx = arcpy.mp.ArcGISProject("CURRENT")
            active_map = aprx.activeMap
            
            existing_groups = [lyr for lyr in active_map.listLayers() if lyr.isGroupLayer and lyr.name == "PythonAngle"]
            if existing_groups: group_layer = existing_groups[0]
            else: group_layer = active_map.createGroupLayer("PythonAngle")

            existing_occ_groups = [lyr for lyr in active_map.listLayers() if lyr.isGroupLayer and lyr.name == "OccluderDebug"]
            if existing_occ_groups: occ_group_layer = existing_occ_groups[0]
            else: occ_group_layer = active_map.createGroupLayer("OccluderDebug")
        except Exception as e:
            arcpy.AddWarning("Could not setup Map Group. Layers will save to GDB.")

    def add_to_group(path, layer_name):
        if active_map and group_layer:
            temp_lyr = active_map.addDataFromPath(path)
            temp_lyr.name = layer_name
            active_map.addLayerToGroup(group_layer, temp_lyr, "BOTTOM")
            active_map.removeLayer(temp_lyr)
            
    def add_to_occ_group(path, layer_name):
        if active_map and occ_group_layer:
            temp_lyr = active_map.addDataFromPath(path)
            temp_lyr.name = layer_name
            active_map.addLayerToGroup(occ_group_layer, temp_lyr, "BOTTOM")
            active_map.removeLayer(temp_lyr)

    # ==========================================
    # PHASE 1: PREP, SIMPLIFY, FILTER & COMPLEXITY
    # ==========================================
    arcpy.AddMessage("Step 1: Preparing datasets, simplifying geometry, and filtering by area...")
    
    # Ingest raw buildings to memory first
    arcpy.conversion.FeatureClassToFeatureClass(buildings, "memory", "Bldgs_Raw")
    
    # Repair and Simplify to iron out micro-wobbles before breaking into walls
    arcpy.AddMessage("   -> Repairing geometry and simplifying polygons (0.15m tolerance)...")
    arcpy.management.RepairGeometry("memory\\Bldgs_Raw")
    arcpy.cartography.SimplifyPolygon(
        in_features="memory\\Bldgs_Raw", 
        out_feature_class="memory\\Bldgs_Base", 
        algorithm="POINT_REMOVE", 
        tolerance="0.15 Meters"
    )

    arcpy.management.CalculateField("memory\\Bldgs_Base", "Bldg_ID", "!OBJECTID!", "PYTHON3", field_type="LONG")
    
    # V4.0 FIX: Generate the obstacle buffer NOW, before any rows are deleted. 
    # This guarantees massive buildings and tiny sheds are preserved as line-of-sight obstacles.
    arcpy.AddMessage("   -> Generating global obstacle buffer for raycasts (-0.01m)...")
    arcpy.analysis.Buffer("memory\\Bldgs_Base", "memory\\Bldgs_Shrunk_Master", shrink_buffer)

    # Securely copy the external Building GUID over to a standardised internal text field
    if bldg_guid_field and bldg_guid_field not in ["", "#"]:
        arcpy.management.AddField("memory\\Bldgs_Base", "ORIG_GUID", "TEXT", field_length=255)
        calc_code = "def safe_str(val):\n    if val is None: return None\n    return str(val)"
        arcpy.management.CalculateField("memory\\Bldgs_Base", "ORIG_GUID", f"safe_str(!{bldg_guid_field}!)", "PYTHON3", code_block=calc_code)
        arcpy.AddMessage(f"   -> Preserved original persistent Building IDs from field: [{bldg_guid_field}]")
    
    tracking_fields = [
        ["ROAD_X", "DOUBLE"], ["ROAD_Y", "DOUBLE"], ["BEARING", "DOUBLE"], ["Segment_ID", "LONG"],
        ["Recovery_Type", "TEXT", "Recovery_Type", 100], ["Failure_Reason", "TEXT", "Failure_Reason", 255]
    ]
    arcpy.management.AddFields("memory\\Bldgs_Base", tracking_fields)

    arcpy.management.AddField("memory\\Bldgs_Base", "POLY_AREA", "DOUBLE")
    arcpy.management.CalculateField("memory\\Bldgs_Base", "POLY_AREA", "!shape.geodesicArea@squaremeters!", "PYTHON3")
    
    bldg_metrics = {}
    dropped_by_area = 0
    with arcpy.da.UpdateCursor("memory\\Bldgs_Base", ["POLY_AREA", "Bldg_ID", "SHAPE@"]) as cur:
        for row in cur:
            if row[0] < MIN_AREA or row[0] > MAX_AREA:
                cur.deleteRow()
                dropped_by_area += 1
            else:
                b_id, shape = row[1], row[2]
                peri = shape.length
                ch_area = shape.convexHull().area
                comp_idx = 1.0 - (row[0] / ch_area) if ch_area > 0 else 0.0
                
                bldg_metrics[b_id] = {
                    'dist': 0.0, 'ambig': 0.0, 'angle': 0.0, 'raycast': 0.0, 
                    'perimeter': peri, 'complexity': comp_idx, 'facade_ratio': 0.0, 
                    't_boost': False, 'peer_consensus_score': 0.0
                }
                
    arcpy.AddMessage(f"   -> Dropped {dropped_by_area} buildings outside area constraints ({MIN_AREA} - {MAX_AREA} sqm).")

    arcpy.conversion.FeatureClassToFeatureClass(roads, "memory", "Roads_Raw")
    arcpy.management.AddField("memory\\Roads_Raw", "Parent_Road_ID", "LONG")
    arcpy.management.CalculateField("memory\\Roads_Raw", "Parent_Road_ID", "!OBJECTID!", "PYTHON3")

    if road_guid_field and road_guid_field not in ["", "#"]:
        arcpy.management.AddField("memory\\Roads_Raw", "ROAD_ORIG_GUID", "TEXT", field_length=255)
        calc_code = "def safe_str(val):\n    if val is None: return None\n    return str(val)"
        arcpy.management.CalculateField("memory\\Roads_Raw", "ROAD_ORIG_GUID", f"safe_str(!{road_guid_field}!)", "PYTHON3", code_block=calc_code)
        arcpy.AddMessage(f"   -> Preserved original persistent Road IDs from field: [{road_guid_field}]")

    arcpy.AddMessage(f"   -> Generating Building Adjacency Matrix ({TOUCH_TOL}m tolerance)...")
    arcpy.analysis.GenerateNearTable(
        in_features="memory\\Bldgs_Base", near_features="memory\\Bldgs_Base", out_table="memory\\Bldgs_Near", 
        search_radius=f"{TOUCH_TOL} Meters", location="NO_LOCATION", angle="NO_ANGLE", closest="ALL"
    )

    oid_to_bldg = {row[0]: row[1] for row in arcpy.da.SearchCursor("memory\\Bldgs_Base", ["OBJECTID", "Bldg_ID"])}
    touch_dict = {}
    with arcpy.da.SearchCursor("memory\\Bldgs_Near", ["IN_FID", "NEAR_FID"]) as cur:
        for row in cur:
            in_oid, near_oid = row[0], row[1]
            if in_oid != near_oid: 
                b1 = oid_to_bldg.get(in_oid)
                b2 = oid_to_bldg.get(near_oid)
                if b1 and b2:
                    if b1 not in touch_dict: touch_dict[b1] = set() 
                    touch_dict[b1].add(b2)

    touch_dict = {k: list(v) for k, v in touch_dict.items()}

    arcpy.management.AddFields("memory\\Bldgs_Base", [["Touch_Count", "SHORT"], ["Touched_IDs", "TEXT", "Touched_IDs", 255]])
    with arcpy.da.UpdateCursor("memory\\Bldgs_Base", ["Bldg_ID", "Touch_Count", "Touched_IDs"]) as cur:
        for row in cur:
            touches = touch_dict.get(row[0], [])
            row[1], row[2] = len(touches), str(touches)
            cur.updateRow(row)

    arcpy.AddMessage("   -> Extracting Party Walls for Semi-Detached Alignment...")
    arcpy.management.PolygonToLine("memory\\Bldgs_Base", "memory\\All_Bldg_Lines", "IDENTIFY_NEIGHBORS")
    party_wall_dict = {}
    with arcpy.da.SearchCursor("memory\\All_Bldg_Lines", ["LEFT_FID", "RIGHT_FID", "SHAPE@"]) as cur:
        for row in cur:
            if row[0] != -1 and row[1] != -1:
                l_bldg = oid_to_bldg.get(row[0])
                r_bldg = oid_to_bldg.get(row[1])
                if l_bldg and r_bldg:
                    shape = row[2]
                    p1, p2 = shape.firstPoint, shape.lastPoint
                    angle = math.degrees(math.atan2(p2.X - p1.X, p2.Y - p1.Y))
                    party_wall_dict[(min(l_bldg, r_bldg), max(l_bldg, r_bldg))] = angle % 180

    arcpy.management.CreateFeatureclass("memory", "Road_Endpoints", "POINT", spatial_reference=sr)
    with arcpy.da.InsertCursor("memory\\Road_Endpoints", ["SHAPE@XY"]) as icur:
        with arcpy.da.SearchCursor("memory\\Roads_Raw", ["SHAPE@"]) as scur:
            for row in scur:
                if row[0]:
                    icur.insertRow([(row[0].firstPoint.X, row[0].firstPoint.Y)])
                    icur.insertRow([(row[0].lastPoint.X, row[0].lastPoint.Y)])
    
    arcpy.management.DeleteIdentical("memory\\Road_Endpoints", ["Shape"])
    arcpy.analysis.SpatialJoin(
        target_features="memory\\Road_Endpoints", join_features="memory\\Roads_Raw", 
        out_feature_class="memory\\Endpoints_Joined", join_operation="JOIN_ONE_TO_ONE", 
        join_type="KEEP_ALL", match_option="WITHIN_A_DISTANCE", search_radius=f"{SNAP_TOL} Meters"
    )
    
    arcpy.analysis.Select("memory\\Endpoints_Joined", "memory\\True_CulDeSacs", "Join_Count = 1")
    dangles = [row[0] for row in arcpy.da.SearchCursor("memory\\True_CulDeSacs", ["SHAPE@XY"])]

    if save_intermediates and dangles:
        arcpy.conversion.FeatureClassToFeatureClass("memory\\True_CulDeSacs", out_gdb, "DEBUG_0_CulDeSacs")
        add_to_group(os.path.join(out_gdb, "DEBUG_0_CulDeSacs"), "DEBUG_0_CulDeSacs")

    arcpy.management.SplitLine("memory\\Roads_Raw", "memory\\Roads_Base")
    arcpy.management.AddField("memory\\Roads_Base", "Road_Bearing", "DOUBLE")
    arcpy.management.CalculateField("memory\\Roads_Base", "Road_Bearing", "get_bearing(!Shape!)", "PYTHON3", road_bearing_code)

    arcpy.management.AddField("memory\\Roads_Base", "Is_Roundabout", "SHORT")
    arcpy.management.CalculateField("memory\\Roads_Base", "Is_Roundabout", "0", "PYTHON3")
    if ROUNDABOUT_SQL:
        try:
            r_lyr = arcpy.management.MakeFeatureLayer("memory\\Roads_Base", "Roundabout_Lyr", ROUNDABOUT_SQL)
            arcpy.management.CalculateField(r_lyr, "Is_Roundabout", "1", "PYTHON3")
            arcpy.AddMessage(f"   -> Flagged roundabouts using query: {ROUNDABOUT_SQL}")
        except Exception as e:
            arcpy.AddWarning(f"   -> Roundabout SQL failed. Check syntax: {ROUNDABOUT_SQL}")

    # ==========================================
    # GLOBAL GEOMETRY DICTIONARIES FOR DYNAMIC POINTS
    # ==========================================
    arcpy.management.FeatureToPoint("memory\\Bldgs_Base", "memory\\Bldgs_Centroids", "INSIDE")
    centroid_map = {row[0]: row[1] for row in arcpy.da.SearchCursor("memory\\Bldgs_Centroids", ["OBJECTID", "Bldg_ID"])}
    
    segment_road_geoms = {row[0]: row[1] for row in arcpy.da.SearchCursor("memory\\Roads_Base", ["OBJECTID", "SHAPE@"])}
    centroid_geoms = {row[0]: row[1] for row in arcpy.da.SearchCursor("memory\\Bldgs_Centroids", ["Bldg_ID", "SHAPE@"])}

    def get_dynamic_point(bldg_id, segment_id):
        b_geom = centroid_geoms.get(bldg_id)
        r_geom = segment_road_geoms.get(segment_id)
        if b_geom and r_geom:
            res = r_geom.queryPointAndDistance(b_geom)
            return res[0].firstPoint.X, res[0].firstPoint.Y
        return None, None

    # ==========================================
    # PHASE 2: INITIAL CENTROID MATCHING 
    # ==========================================
    arcpy.AddMessage(f"Step 2: Locating closest roads and calculating Ambiguity Index (Max Dist: {ROAD_SEARCH_DIST}m)...")
    arcpy.analysis.GenerateNearTable(
        in_features="memory\\Bldgs_Centroids", near_features="memory\\Roads_Base", out_table="memory\\Bldg_Road_Near", 
        search_radius=f"{ROAD_SEARCH_DIST} Meters", location="LOCATION", angle="NO_ANGLE", closest="ALL"
    )
    
    arcpy.management.Sort("memory\\Bldg_Road_Near", "memory\\Bldg_Road_Near_Sort", [["IN_FID", "ASCENDING"], ["NEAR_DIST", "ASCENDING"]])

    road_dict = {row[0]: (row[1], row[2]) for row in arcpy.da.SearchCursor("memory\\Roads_Base", ["OBJECTID", "Road_Bearing", "Is_Roundabout"])}
    
    unified_dict = {}
    bldg_votes_primary = {}   
    bldg_votes_fallback = {}  
    cul_de_sac_rad_sq = CUL_DE_SAC_RAD ** 2 
    skipped_roundabouts = set()

    with arcpy.da.SearchCursor("memory\\Bldg_Road_Near_Sort", ["IN_FID", "NEAR_X", "NEAR_Y", "NEAR_FID", "NEAR_DIST"]) as cur:
        for row in cur:
            bldg_id = centroid_map.get(row[0])
            nx, ny, seg_id, dist = row[1], row[2], row[3], row[4]
            if not bldg_id: continue
            
            road_info = road_dict.get(seg_id)
            if not road_info: continue
            r_bearing, is_roundabout = road_info
            
            # --- Ambiguity Tracking ---
            if is_roundabout == 0:
                if bldg_id not in bldg_votes_primary:
                    bldg_votes_primary[bldg_id] = {'N_S': 0.0, 'NE_SW': 0.0, 'E_W': 0.0, 'SE_NW': 0.0}
                    bldg_votes_fallback[bldg_id] = {'N_S': 0.0, 'NE_SW': 0.0, 'E_W': 0.0, 'SE_NW': 0.0}
                    
                norm_angle = r_bearing % 180 
                if norm_angle < 22.5 or norm_angle >= 157.5: bin_name = 'N_S'
                elif norm_angle < 67.5: bin_name = 'NE_SW'
                elif norm_angle < 112.5: bin_name = 'E_W'
                else: bin_name = 'SE_NW'
                    
                safe_dist = max(dist, 0.1)
                if safe_dist <= AMBIGUITY_RADIUS: bldg_votes_primary[bldg_id][bin_name] += (1.0 / safe_dist)
                else: bldg_votes_fallback[bldg_id][bin_name] += (1.0 / safe_dist)

            # --- Initial Snapping (Captures only the closest valid row) ---
            if bldg_id not in unified_dict:
                if is_roundabout == 1:
                    skipped_roundabouts.add(bldg_id)
                    unified_dict[bldg_id] = "SKIP"
                    continue
                
                is_cul_de_sac = any(((nx - dx)**2 + (ny - dy)**2) <= cul_de_sac_rad_sq for dx, dy in dangles)
                if not is_cul_de_sac:
                    unified_dict[bldg_id] = (nx, ny, r_bearing, seg_id)
                    decay_factor = (dist / ROAD_SEARCH_DIST) ** 3
                    dist_score = max(0.0, 1.0 - decay_factor) # Normalized 0-1
                    if bldg_id in bldg_metrics: bldg_metrics[bldg_id]['dist'] = dist_score

    if skipped_roundabouts:
        arcpy.AddMessage(f"   -> Skipped {len(skipped_roundabouts)} buildings that snapped to a roundabout.")

    # Calculate final Ambiguity from tallied votes (Normalized 0-1)
    for bldg_id in oid_to_bldg.values():
        primary_votes = bldg_votes_primary.get(bldg_id, {'N_S': 0.0, 'NE_SW': 0.0, 'E_W': 0.0, 'SE_NW': 0.0})
        total_primary = sum(primary_votes.values())
        
        active_votes = bldg_votes_fallback.get(bldg_id, {'N_S': 0.0, 'NE_SW': 0.0, 'E_W': 0.0, 'SE_NW': 0.0}) if total_primary == 0 else primary_votes
        total_active_votes = sum(active_votes.values())
        
        if total_active_votes > 0:
            max_votes = max(active_votes.values())
            scaled_ambiguity = min((1.0 - (max_votes / total_active_votes)) * 2.0, 1.0)
            if bldg_id in bldg_metrics: bldg_metrics[bldg_id]['ambig'] = 1.0 - scaled_ambiguity
        else:
            if bldg_id in bldg_metrics: bldg_metrics[bldg_id]['ambig'] = 1.0

    for fc in ["memory\\Bldgs_Base", "memory\\Bldgs_Centroids"]:
        with arcpy.da.UpdateCursor(fc, ["Bldg_ID", "ROAD_X", "ROAD_Y", "BEARING", "Segment_ID"]) as cur:
            for row in cur:
                if row[0] in unified_dict and unified_dict[row[0]] != "SKIP":
                    row[1], row[2], row[3], row[4] = unified_dict[row[0]]
                    cur.updateRow(row)
                else: cur.deleteRow()

    # ==========================================
    # PHASE 2.5: TERRACE ALIGNMENT & CONSENSUS
    # ==========================================
    arcpy.AddMessage("Step 2.5: Analysing Terraced Blocks & Semi-Detached Corners...")
    current_state = {row[0]: (row[1], row[2], row[3], row[4]) for row in arcpy.da.SearchCursor("memory\\Bldgs_Base", ["Bldg_ID", "ROAD_X", "ROAD_Y", "BEARING", "Segment_ID"]) if row[3] is not None}

    visited = set()
    clusters = []
    
    for bldg in touch_dict.keys():
        if bldg not in visited:
            queue = [bldg]
            cluster = set([bldg])
            while queue:
                current = queue.pop(0)
                for neighbor in touch_dict.get(current, []):
                    if neighbor not in cluster:
                        cluster.add(neighbor)
                        queue.append(neighbor)
            visited.update(cluster)
            clusters.append(cluster)

    end_cap_updates = {}
    for cluster in clusters:
        # Rule A: The Semi-Detached Party Wall Check
        if len(cluster) == 2:
            b1, b2 = list(cluster)
            if b1 in current_state and b2 in current_state:
                b1_bearing = current_state[b1][2]
                b2_bearing = current_state[b2][2]

                diff = abs(b1_bearing - b2_bearing) % 180
                if diff > 90: diff = 180 - diff

                if diff > TERRACE_PEN_ANGLE:
                    pw_bearing = party_wall_dict.get((min(b1, b2), max(b1, b2)))
                    if pw_bearing is not None:
                        target = (pw_bearing + 90.0) % 180.0
                        
                        diff1 = abs(b1_bearing - target) % 180
                        if diff1 > 90: diff1 = 180 - diff1
                        
                        diff2 = abs(b2_bearing - target) % 180
                        if diff2 > 90: diff2 = 180 - diff2

                        if diff1 < diff2:
                            w_bear, w_seg = current_state[b1][2], current_state[b1][3]
                            nx, ny = get_dynamic_point(b2, w_seg)
                            if nx: end_cap_updates[b2] = (nx, ny, w_bear, w_seg)
                        else:
                            w_bear, w_seg = current_state[b2][2], current_state[b2][3]
                            nx, ny = get_dynamic_point(b1, w_seg)
                            if nx: end_cap_updates[b1] = (nx, ny, w_bear, w_seg)

        # Rule B: The Terrace Alignment Check (Marks t_boost for regression weighting)
        elif len(cluster) >= MIN_TERRACE: 
            bearings = [current_state[b][2] for b in cluster if b in current_state]
            if not bearings: continue
            
            base_b = bearings[0]
            is_similar = True
            for b_ang in bearings[1:]:
                diff = abs(b_ang - base_b) % 180
                if diff > 90: diff = 180 - diff
                if diff > 10.0:
                    is_similar = False
                    break
            
            if is_similar:
                for b in cluster:
                    if b in bldg_metrics: bldg_metrics[b]['t_boost'] = True
            else:
                for bldg in cluster:
                    touches = touch_dict.get(bldg, [])
                    if len(touches) == 1: 
                        neighbor = touches[0]
                        if bldg in current_state and neighbor in current_state:
                            my_bearing = current_state[bldg][2]
                            needs_fixing = True
                            for other in cluster:
                                if other != bldg and other in current_state:
                                    diff = abs(my_bearing - current_state[other][2]) % 180
                                    if diff > 90: diff = 180 - diff
                                    if diff <= TERRACE_TOL: 
                                        needs_fixing = False
                                        break
                            if needs_fixing: 
                                n_bear, n_seg = current_state[neighbor][2], current_state[neighbor][3]
                                nx, ny = get_dynamic_point(bldg, n_seg)
                                if nx: end_cap_updates[bldg] = (nx, ny, n_bear, n_seg)

    if end_cap_updates:
        arcpy.AddMessage(f"   -> Successfully aligned {len(end_cap_updates)} end-of-terrace/semi-detached buildings.")
        for fc in ["memory\\Bldgs_Base", "memory\\Bldgs_Centroids"]:
            with arcpy.da.UpdateCursor(fc, ["Bldg_ID", "ROAD_X", "ROAD_Y", "BEARING", "Segment_ID"]) as cur:
                for row in cur:
                    if row[0] in end_cap_updates:
                        row[1], row[2], row[3], row[4] = end_cap_updates[row[0]]
                        cur.updateRow(row)

    # ==========================================
    # PHASE 2.7: GLOBAL GEOMETRY PRE-PROCESSING
    # ==========================================
    arcpy.AddMessage("   -> Shattering target buildings into master walls...")
    arcpy.management.PolygonToLine("memory\\Bldgs_Base", "memory\\Walls_Master_Lines", "IDENTIFY_NEIGHBORS")
    arcpy.management.SplitLine("memory\\Walls_Master_Lines", "memory\\Walls_Master")
    arcpy.management.CalculateField("memory\\Walls_Master", "Parent_OBJ", "max(!LEFT_FID!, !RIGHT_FID!)", "PYTHON3", field_type="LONG")
    
    arcpy.management.AddField("memory\\Walls_Master", "Bldg_ID", "LONG")
    bldg_oid_map = {row[0]: row[1] for row in arcpy.da.SearchCursor("memory\\Bldgs_Base", ["OBJECTID", "Bldg_ID"])}
    with arcpy.da.UpdateCursor("memory\\Walls_Master", ["Parent_OBJ", "Bldg_ID"]) as cur:
        for row in cur:
            if row[0] in bldg_oid_map:
                row[1] = bldg_oid_map[row[0]]
                cur.updateRow(row)

    arcpy.management.AddField("memory\\Walls_Master", "Wall_ID", "LONG")
    arcpy.management.CalculateField("memory\\Walls_Master", "Wall_ID", "!OBJECTID!", "PYTHON3")
    
    arcpy.management.AddField("memory\\Walls_Master", "Wall_Bearing", "DOUBLE")
    arcpy.management.CalculateField("memory\\Walls_Master", "Wall_Bearing", "get_bearing(!Shape!)", "PYTHON3", road_bearing_code)
    
    arcpy.management.AddField("memory\\Walls_Master", "Facade_Score", "DOUBLE")

    global_bldg_wall_bearings = {}
    with arcpy.da.SearchCursor("memory\\Walls_Master", ["Bldg_ID", "Wall_Bearing"]) as c:
        for r in c:
            if r[0] is not None:
                global_bldg_wall_bearings.setdefault(r[0], []).append(r[1])
                
    # ==========================================
    # HELPER FUNCTION: RAYCAST EXECUTION & METRICS
    # ==========================================
    def run_raycast_pass(pass_name, active_bldgs=None):
        walls_filtered = f"memory\\Walls_Filtered_{pass_name}"
        pts = f"memory\\Wall_Pts_{pass_name}"
        raw_sightlines = f"memory\\Sightlines_Raw_{pass_name}"
        lyr = f"Sightlines_Lyr_{pass_name}"

        current_bearings = {row[0]: row[1] for row in arcpy.da.SearchCursor("memory\\Bldgs_Base", ["Bldg_ID", "BEARING"]) if (active_bldgs is None or row[0] in active_bldgs) and row[1] is not None}
        
        arcpy.management.MakeFeatureLayer("memory\\Walls_Master", "Walls_Lyr_Temp")
        if active_bldgs:
            query = f"Bldg_ID IN ({','.join(map(str, active_bldgs))})"
            arcpy.management.SelectLayerByAttribute("Walls_Lyr_Temp", "NEW_SELECTION", query)
            
        with arcpy.da.UpdateCursor("Walls_Lyr_Temp", ["Bldg_ID", "Wall_Bearing", "Facade_Score"]) as cur:
            for row in cur:
                b_id = row[0]
                if b_id in current_bearings:
                    w_bearing, r_bearing = row[1], current_bearings[b_id]
                    diff = abs(w_bearing - r_bearing) % 180
                    if diff > 90: diff = 180 - diff
                    row[2] = 2.0 - (diff / 90.0)
                    cur.updateRow(row)
                    
        arcpy.management.SelectLayerByAttribute("Walls_Lyr_Temp", "SUBSET_SELECTION", f"Facade_Score >= {score_threshold}")
        arcpy.management.CopyFeatures("Walls_Lyr_Temp", walls_filtered)

        selected_wall_bearings = {}
        angle_scores = {}
        wall_lengths = {}
        with arcpy.da.SearchCursor(walls_filtered, ["Wall_ID", "Wall_Bearing", "Bldg_ID", "Facade_Score", "SHAPE@LENGTH"]) as c:
            for r in c:
                if r[0] is not None:
                    selected_wall_bearings.setdefault(r[2], []).append(r[1])
                    angle_scores[r[0]] = r[3] - 1.0 
                    wall_lengths[r[0]] = r[4]

        arcpy.management.GeneratePointsAlongLines(walls_filtered, pts, "DISTANCE", Distance=f"{RAYCAST_INTERVAL} Meters")
        arcpy.management.AddGeometryAttributes(pts, "POINT_X_Y_Z_M")
        arcpy.management.CalculateField(pts, "Line_ID", "!OBJECTID!", "PYTHON3", field_type="LONG")
        
        total_pts = {}
        with arcpy.da.SearchCursor(pts, ["Wall_ID"]) as c:
            for r in c: 
                if r[0] is not None: total_pts[r[0]] = total_pts.get(r[0], 0) + 1

        arcpy.management.AddFields(pts, [["ROAD_X", "DOUBLE"], ["ROAD_Y", "DOUBLE"]])
        road_xy_dict = {row[0]: (row[1], row[2]) for row in arcpy.da.SearchCursor("memory\\Bldgs_Base", ["Bldg_ID", "ROAD_X", "ROAD_Y"]) if (active_bldgs is None or row[0] in active_bldgs)}
        with arcpy.da.UpdateCursor(pts, ["Bldg_ID", "ROAD_X", "ROAD_Y"]) as cur:
            for row in cur:
                b_id = row[0]
                if b_id in road_xy_dict:
                    row[1], row[2] = road_xy_dict[b_id]
                    cur.updateRow(row)

        pts_valid = f"memory\\Wall_Pts_Valid_{pass_name}"
        arcpy.analysis.Select(pts, pts_valid, "ROAD_X IS NOT NULL")
        arcpy.management.XYToLine(pts_valid, raw_sightlines, "ROAD_X", "ROAD_Y", "POINT_X", "POINT_Y", id_field="Line_ID")
        
        arcpy.management.AddFields(raw_sightlines, [["Bldg_ID", "LONG"], ["Wall_ID", "LONG"]])
        attr_dict = {row[0]: (row[1], row[2]) for row in arcpy.da.SearchCursor(pts_valid, ["Line_ID", "Bldg_ID", "Wall_ID"])}
        with arcpy.da.UpdateCursor(raw_sightlines, ["Line_ID", "Bldg_ID", "Wall_ID"]) as cur:
            for row in cur:
                line_id = row[0]
                if line_id in attr_dict:
                    row[1], row[2] = attr_dict[line_id]
                    cur.updateRow(row)

        if save_intermediates and pass_name == "P1":
            arcpy.management.CopyFeatures(raw_sightlines, f"memory\\Attempted_Sightlines_{pass_name}")

        arcpy.management.MakeFeatureLayer(raw_sightlines, lyr)
        arcpy.management.SelectLayerByLocation(lyr, "INTERSECT", "memory\\Bldgs_Shrunk_Master")
        arcpy.management.DeleteFeatures(lyr)
        arcpy.management.SelectLayerByAttribute(lyr, "CLEAR_SELECTION")

        surv_pts = {}
        bldg_walls = {}
        with arcpy.da.SearchCursor(raw_sightlines, ["Wall_ID", "Bldg_ID"]) as c:
            for r in c:
                w_id, b_id = r[0], r[1]
                if w_id is None or b_id is None: continue
                surv_pts[w_id] = surv_pts.get(w_id, 0) + 1
                if b_id not in bldg_walls: bldg_walls[b_id] = set()
                bldg_walls[b_id].add(w_id)
                
        metrics = {}
        for b_id, w_ids in bldg_walls.items():
            hit_walls = [w for w in w_ids if surv_pts.get(w, 0) > 0]
            if not hit_walls: continue
            
            s = sum(surv_pts.get(w, 0) for w in hit_walls)
            t = sum(total_pts.get(w, 1) for w in hit_walls) 
            r_score = s / t if t > 0 else 0.0
            
            a_score = max(angle_scores.get(w, 0.0) for w in hit_walls)
            f_len = sum(wall_lengths.get(w, 0.0) for w in hit_walls)
            
            metrics[b_id] = (a_score, r_score, f_len)

        return lyr, walls_filtered, pts_valid, metrics, selected_wall_bearings

    # ==========================================
    # PHASE 3: PASS 1 (THE TEST) & RECOVERY 
    # ==========================================
    arcpy.AddMessage("Step 3: Running Pass 1 to identify occluded and low-angle buildings...")
    sightline_lyr_p1, filtered_walls_p1, pts_valid_p1, p1_metrics, p1_sel_wall_bearings = run_raycast_pass("P1")

    survivors = set(row[0] for row in arcpy.da.SearchCursor(sightline_lyr_p1, ["Bldg_ID"]))
    all_attempted = {row[0]: (row[1], row[2], row[3], row[4]) for row in arcpy.da.SearchCursor("memory\\Bldgs_Base", ["Bldg_ID", "ROAD_X", "ROAD_Y", "BEARING", "Segment_ID"])}
    
    occluded_bldgs = set(all_attempted.keys()) - survivors
    low_angle_bldgs = set()
    
    for b_id, (ang, ray, flen) in p1_metrics.items():
        if b_id not in occluded_bldgs and ang < LOW_ANGLE_CONFIDENCE:
            low_angle_bldgs.add(b_id)
            
    recovery_candidates = occluded_bldgs.union(low_angle_bldgs)

    if recovery_candidates:
        arcpy.AddMessage(f"*** Found {len(occluded_bldgs)} Occluded and {len(low_angle_bldgs)} Low-Angle buildings. Searching for alternatives... ***")

        query = f"Bldg_ID IN ({','.join(map(str, recovery_candidates))})"
        arcpy.analysis.Select("memory\\Bldgs_Centroids", "memory\\Failed_Cen", query)
        
        search_rad = max(ROAD_SEARCH_DIST, ANGLE_RECOVERY_RADIUS)
        arcpy.analysis.GenerateNearTable(
            in_features="memory\\Failed_Cen", near_features="memory\\Roads_Base", out_table="memory\\Failed_Near", 
            search_radius=f"{search_rad} Meters", location="LOCATION", angle="NO_ANGLE", closest="ALL"
        )
        arcpy.management.Sort("memory\\Failed_Near", "memory\\Failed_Near_Sort", [["IN_FID", "ASCENDING"], ["NEAR_DIST", "ASCENDING"]])

        fail_map = {row[0]: row[1] for row in arcpy.da.SearchCursor("memory\\Failed_Cen", ["OBJECTID", "Bldg_ID"])}
        
        rule1_condA_best = {}  
        rule1_condB_best = {}  
        rule1_fallback_best = {}
        
        failed_updates = {}         
        low_angle_updates = {}      
        new_dist_scores = {}        
        fixed_set = set() 

        with arcpy.da.SearchCursor("memory\\Failed_Near_Sort", ["IN_FID", "NEAR_X", "NEAR_Y", "NEAR_FID", "NEAR_DIST"]) as cur:
            for row in cur:
                bldg_id = fail_map.get(row[0])
                if not bldg_id: continue
                
                seg_id = row[3]
                road_info = road_dict.get(seg_id)
                if not road_info: continue
                new_bearing, is_roundabout = road_info
                if is_roundabout == 1: continue
                
                nx, ny = row[1], row[2]
                is_cul_de_sac = any(((nx - dx)**2 + (ny - dy)**2) <= cul_de_sac_rad_sq for dx, dy in dangles)
                if is_cul_de_sac: continue
                
                dist = row[4]
                old_bearing = all_attempted.get(bldg_id, (0,0,None,0))[2]
                
                if old_bearing is None:
                    diff_from_old = 999.0
                else:
                    diff_from_old = abs(new_bearing - old_bearing) % 180
                    if diff_from_old > 90: diff_from_old = 180 - diff_from_old
                
                dist_score = max(0.0, 1.0 - (dist / ROAD_SEARCH_DIST)**3)

                dyn_nx, dyn_ny = get_dynamic_point(bldg_id, seg_id)
                if dyn_nx: nx, ny = dyn_nx, dyn_ny
                
                if bldg_id in occluded_bldgs:
                    walls = p1_sel_wall_bearings.get(bldg_id, [])
                    base_bearing = walls[0] if walls else old_bearing
                    
                    if base_bearing is not None:
                        target_bearing = (base_bearing + 90.0) % 180.0
                        
                        diff_from_target = abs(new_bearing - target_bearing) % 180
                        if diff_from_target > 90: diff_from_target = 180 - diff_from_target
                        target_score = (1.0 - (diff_from_target / 90.0)) * 100.0
                        
                        if dist <= 25.0 and target_score >= 90.0:
                            if bldg_id not in rule1_condA_best:
                                rule1_condA_best[bldg_id] = (nx, ny, new_bearing, seg_id, dist_score, "Rule 1: 25m/90%")
                                
                        if dist <= 50.0 and target_score >= 95.0:
                            if bldg_id not in rule1_condB_best:
                                rule1_condB_best[bldg_id] = (nx, ny, new_bearing, seg_id, dist_score, "Rule 1: 50m/95%")
                    
                    if dist <= ROAD_SEARCH_DIST and diff_from_old > RECOV_MIN_SHIFT:
                        if bldg_id not in rule1_fallback_best:
                            rule1_fallback_best[bldg_id] = (nx, ny, new_bearing, seg_id, dist_score, "Rule 1: Fallback (>25 deg)")
                            
                elif bldg_id in low_angle_bldgs and bldg_id not in fixed_set and bldg_id not in low_angle_updates:
                    if dist <= ANGLE_RECOVERY_RADIUS and diff_from_old > RECOV_MIN_SHIFT:
                        walls = global_bldg_wall_bearings.get(bldg_id, [])
                        best_score = 0.0
                        for w in walls:
                            diff = abs(w - new_bearing) % 180
                            if diff > 90: diff = 180 - diff
                            score = (1.0 - (diff / 90.0)) * 100.0
                            if score > best_score: best_score = score
                        
                        if best_score >= ANGLE_RECOVERY_TARGET:
                            low_angle_updates[bldg_id] = (nx, ny, new_bearing, seg_id, "Rule 2: Geometric Resonance")
                            new_dist_scores[bldg_id] = dist_score

    rule1_updates = {}
    for b_id in recovery_candidates:
        if b_id in rule1_condA_best: rule1_updates[b_id] = rule1_condA_best[b_id]
        elif b_id in rule1_condB_best: rule1_updates[b_id] = rule1_condB_best[b_id]

    arcpy.management.AddGeometryAttributes("memory\\Bldgs_Centroids", "POINT_X_Y_Z_M")
    arcpy.analysis.Select("memory\\Bldgs_Centroids", "memory\\Bldgs_Centroids_Valid", "ROAD_X IS NOT NULL")
    arcpy.management.XYToLine("memory\\Bldgs_Centroids_Valid", "memory\\Diag_P1", "ROAD_X", "ROAD_Y", "POINT_X", "POINT_Y", id_field="Bldg_ID")

    master_occluder_dict = {} 
    occ_candidates = [b for b in list(rule1_updates.keys()) + list(rule1_fallback_best.keys()) if b in occluded_bldgs]
    
    if occ_candidates:
        occ_query = f"Bldg_ID IN ({','.join(map(str, occ_candidates))})"
        arcpy.analysis.Select("memory\\Diag_P1", "memory\\Failed_Diag", occ_query)
        arcpy.analysis.SpatialJoin(
            target_features="memory\\Bldgs_Base", join_features="memory\\Failed_Diag", 
            out_feature_class="memory\\Occ_Check", join_operation="JOIN_ONE_TO_MANY", 
            match_option="WITHIN_A_DISTANCE", search_radius=diag_capture_radius
        )
        with arcpy.da.SearchCursor("memory\\Occ_Check", ["Bldg_ID", "Bldg_ID_1"]) as cur:
            for row in cur:
                hit_id, failed_bldg_id = row[0], row[1]
                if hit_id is not None and failed_bldg_id is not None and hit_id != failed_bldg_id:
                    touches = touch_dict.get(hit_id, [])
                    if failed_bldg_id in touches:
                        master_occluder_dict.setdefault(failed_bldg_id, []).append(hit_id)

    p2_occluder_updates = {}
    for b_id in rule1_updates:
        for occ_id in master_occluder_dict.get(b_id, []):
            f_bear, f_seg = rule1_updates[b_id][2], rule1_updates[b_id][3]
            nx, ny = get_dynamic_point(occ_id, f_seg)
            if nx: p2_occluder_updates[occ_id] = (nx, ny, f_bear, f_seg, "Infected Occluder")
            
    for b_id in rule1_fallback_best:
        for occ_id in master_occluder_dict.get(b_id, []):
            if occ_id not in p2_occluder_updates:
                f_bear, f_seg = rule1_fallback_best[b_id][2], rule1_fallback_best[b_id][3]
                nx, ny = get_dynamic_point(occ_id, f_seg)
                if nx: p2_occluder_updates[occ_id] = (nx, ny, f_bear, f_seg, "Infected Occluder (Fallback)")

    # ---------------------------------------------------------
    # NEW RULE: THE TERRACE ODD-ONE-OUT RECOVERY (WITH 360 DIRECTION)
    # ---------------------------------------------------------
    current_p2_state = {}
    for b_id, state in all_attempted.items():
        if b_id in p2_occluder_updates:
            current_p2_state[b_id] = p2_occluder_updates[b_id]
        elif b_id in failed_updates:
            current_p2_state[b_id] = failed_updates[b_id][:4] 
        else:
            if state[2] is not None:
                current_p2_state[b_id] = state 
            
    odd_one_updates = {}
    semi_detached_updates = {}
    
    for cluster in clusters:
        if len(cluster) == 2:
            b1, b2 = list(cluster)
            if b1 in recovery_candidates or b2 in recovery_candidates:
                if b1 in current_p2_state and b2 in current_p2_state:
                    b1_bearing = current_p2_state[b1][2]
                    b2_bearing = current_p2_state[b2][2]

                    diff = abs(b1_bearing - b2_bearing) % 180
                    if diff > 90: diff = 180 - diff

                    if diff > TERRACE_PEN_ANGLE:
                        pw_bearing = party_wall_dict.get((min(b1, b2), max(b1, b2)))
                        if pw_bearing is not None:
                            target = (pw_bearing + 90.0) % 180.0
                            
                            diff1 = abs(b1_bearing - target) % 180
                            if diff1 > 90: diff1 = 180 - diff1
                            
                            diff2 = abs(b2_bearing - target) % 180
                            if diff2 > 90: diff2 = 180 - diff2

                            if diff1 < diff2:
                                w_bear, w_seg = current_p2_state[b1][2], current_p2_state[b1][3]
                                nx, ny = get_dynamic_point(b2, w_seg)
                                if nx: 
                                    semi_detached_updates[b2] = (nx, ny, w_bear, w_seg)
                                    fixed_set.add(b2)
                            else:
                                w_bear, w_seg = current_p2_state[b2][2], current_p2_state[b2][3]
                                nx, ny = get_dynamic_point(b1, w_seg)
                                if nx: 
                                    semi_detached_updates[b1] = (nx, ny, w_bear, w_seg)
                                    fixed_set.add(b1)

        elif len(cluster) >= MIN_TERRACE:
            valid_members = [b for b in cluster if b in current_p2_state]
            if len(valid_members) < 3: continue
            
            # Step 1: Find 180-Degree wall consensus group (10 degree tolerance)
            consensus_group = []
            for b1 in valid_members:
                b1_ang = current_p2_state[b1][2]
                temp_group = [b1]
                for b2 in valid_members:
                    if b1 != b2:
                        b2_ang = current_p2_state[b2][2]
                        diff = abs(b1_ang - b2_ang) % 180
                        if diff > 90: diff = 180 - diff
                        if diff <= 10.0: temp_group.append(b2)
                if len(temp_group) > len(consensus_group):
                    consensus_group = temp_group
            
            # Step 2: Apply 360-Degree Vector Directional Filter
            if len(consensus_group) >= MIN_TERRACE - 1:
                true_consensus = []
                for b1 in consensus_group:
                    cen1 = centroid_geoms.get(b1)
                    if not cen1: continue
                    rx1, ry1 = current_p2_state[b1][0], current_p2_state[b1][1]
                    h1 = math.degrees(math.atan2(rx1 - cen1.firstPoint.X, ry1 - cen1.firstPoint.Y))
                    if h1 < 0: h1 += 360
                    
                    temp_dir_group = [b1]
                    for b2 in consensus_group:
                        if b1 != b2:
                            cen2 = centroid_geoms.get(b2)
                            if not cen2: continue
                            rx2, ry2 = current_p2_state[b2][0], current_p2_state[b2][1]
                            h2 = math.degrees(math.atan2(rx2 - cen2.firstPoint.X, ry2 - cen2.firstPoint.Y))
                            if h2 < 0: h2 += 360
                            
                            diff = abs(h1 - h2)
                            if diff > 180: diff = 360 - diff
                            if diff <= 45.0: # 45 Degree tolerance for 360 vectors
                                temp_dir_group.append(b2)
                    
                    if len(temp_dir_group) > len(true_consensus):
                        true_consensus = temp_dir_group
                
                # Failsafe: if vector logic fails, default to 180 logic
                if not true_consensus:
                    true_consensus = consensus_group
                    
                # Identify backwards/odd-one-out buildings
                odd_ones = [b for b in valid_members if b not in true_consensus]
                for odd_b in odd_ones:
                    odd_geom = centroid_geoms.get(odd_b)
                    best_dist = 999999
                    closest_consensus = None
                    
                    # Find geographically closest neighbor from the true front-facing consensus
                    for cb in true_consensus:
                        cb_geom = centroid_geoms.get(cb)
                        if odd_geom and cb_geom:
                            dist = odd_geom.distanceTo(cb_geom)
                            if dist < best_dist:
                                best_dist = dist
                                closest_consensus = cb
                                
                    if closest_consensus:
                        # Extract the front-facing road segment and map it
                        c_bear, c_seg = current_p2_state[closest_consensus][2], current_p2_state[closest_consensus][3]
                        nx, ny = get_dynamic_point(odd_b, c_seg)
                        if nx:
                            odd_one_updates[odd_b] = (nx, ny, c_bear, c_seg)
                            fixed_set.add(odd_b) 
    # ---------------------------------------------------------

    p2_updates = {}
    r1_p2_bldgs = set(failed_updates.keys())
    r2_p2_bldgs = set()
    
    for b_id in failed_updates: p2_updates[b_id] = failed_updates[b_id][:4]
    for b_id in p2_occluder_updates: p2_updates[b_id] = p2_occluder_updates[b_id][:4]
    for b_id in odd_one_updates: p2_updates[b_id] = odd_one_updates[b_id][:4]
    for b_id in semi_detached_updates: p2_updates[b_id] = semi_detached_updates[b_id][:4]
    
    for b_id in low_angle_updates:
        if b_id not in fixed_set: 
            p2_updates[b_id] = low_angle_updates[b_id][:4]
            r2_p2_bldgs.add(b_id)

    backup_states_p2 = {}
    with arcpy.da.SearchCursor("memory\\Bldgs_Base", ["Bldg_ID", "ROAD_X", "ROAD_Y", "BEARING", "Segment_ID"]) as c:
        for r in c:
            if r[0] in p2_updates:
                backup_states_p2[r[0]] = {
                    'x': r[1], 'y': r[2], 'bearing': r[3], 'parent': r[4],
                    'ang': p1_metrics.get(r[0], (0.0, 0.0, 0.0))[0], 
                    'ray': p1_metrics.get(r[0], (0.0, 0.0, 0.0))[1]
                }

    for fc in ["memory\\Bldgs_Base", "memory\\Bldgs_Centroids"]:
        with arcpy.da.UpdateCursor(fc, ["Bldg_ID", "ROAD_X", "ROAD_Y", "BEARING", "Segment_ID"]) as cur:
            for row in cur:
                b_id = row[0]
                if b_id in p2_updates:
                    row[1], row[2], row[3], row[4] = p2_updates[b_id][:4]
                    cur.updateRow(row)

    active_p2_bldgs = list(p2_updates.keys())
    if active_p2_bldgs:
        arcpy.AddMessage(f"   -> Executing Pass 2 (Testing {len(active_p2_bldgs)} hypotheses)...")
        sightline_lyr_p2, filtered_walls_p2, pts_valid_p2, p2_metrics, _ = run_raycast_pass("P2", active_p2_bldgs)
    else:
        sightline_lyr_p2, filtered_walls_p2, pts_valid_p2, p2_metrics = None, None, None, {}
    
    successful_p2 = set()
    reverted_p2 = set()
    r1_needs_p3 = set()
    
    for b_id in r2_p2_bldgs:
        old = backup_states_p2[b_id]
        new_ang, new_ray, _ = p2_metrics.get(b_id, (0.0, 0.0, 0.0))
        if new_ang > old['ang'] and new_ray > 0.0:
            successful_p2.add(b_id)
            bldg_metrics[b_id]['dist'] = new_dist_scores.get(b_id, bldg_metrics[b_id]['dist'])
        else:
            reverted_p2.add(b_id)
            
    other_p2_bldgs = set(p2_updates.keys()) - r2_p2_bldgs
    for b_id in other_p2_bldgs:
        _, new_ray, _ = p2_metrics.get(b_id, (0.0, 0.0, 0.0))
        if new_ray > 0.0:
            successful_p2.add(b_id)
            if b_id in failed_updates:
                bldg_metrics[b_id]['dist'] = failed_updates[b_id][4]
        else:
            reverted_p2.add(b_id)
            if b_id in r1_p2_bldgs: r1_needs_p3.add(b_id)

    for b_id in occluded_bldgs:
        if b_id not in r1_p2_bldgs:
            r1_needs_p3.add(b_id)

    bldgs_to_revert_p2 = set(reverted_p2)
    for b_id in reverted_p2:
        for occ_id in master_occluder_dict.get(b_id, []):
            if occ_id in p2_occluder_updates: bldgs_to_revert_p2.add(occ_id)
                
    if bldgs_to_revert_p2:
        for fc in ["memory\\Bldgs_Base", "memory\\Bldgs_Centroids"]:
            with arcpy.da.UpdateCursor(fc, ["Bldg_ID", "ROAD_X", "ROAD_Y", "BEARING", "Segment_ID"]) as cur:
                for row in cur:
                    if row[0] in bldgs_to_revert_p2:
                        old = backup_states_p2[row[0]]
                        row[1], row[2], row[3], row[4] = old['x'], old['y'], old['bearing'], old['parent']
                        cur.updateRow(row)

    # ---------------------------------------------------------
    # PASS 3: FALLBACK FAILSAFE & LATE SEMI-DETACHED SYNC
    # ---------------------------------------------------------
    late_semi_sync_updates = {}
    for cluster in clusters:
        if len(cluster) == 2:
            b1, b2 = list(cluster)
            
            def get_successful_state(b):
                if b in successful_p2: return p2_updates[b][:4]
                if b in survivors and b not in p2_updates: return all_attempted[b][:4]
                return None
                
            s1 = get_successful_state(b1)
            s2 = get_successful_state(b2)
            
            if s1 and b2 in r1_needs_p3:
                nx, ny = get_dynamic_point(b2, s1[3])
                if nx: late_semi_sync_updates[b2] = (nx, ny, s1[2], s1[3])
            elif s2 and b1 in r1_needs_p3:
                nx, ny = get_dynamic_point(b1, s2[3])
                if nx: late_semi_sync_updates[b1] = (nx, ny, s2[2], s2[3])

    p3_updates = {}
    for b_id in r1_needs_p3:
        if b_id in late_semi_sync_updates:
            p3_updates[b_id] = late_semi_sync_updates[b_id]
        elif b_id in rule1_fallback_best:
            p3_updates[b_id] = rule1_fallback_best[b_id][:4]

    p3_occluder_updates = {}
    for b_id in p3_updates:
        for occ_id in master_occluder_dict.get(b_id, []):
            if occ_id not in late_semi_sync_updates:
                f_bear, f_seg = p3_updates[b_id][2], p3_updates[b_id][3]
                nx, ny = get_dynamic_point(occ_id, f_seg)
                if nx: p3_occluder_updates[occ_id] = (nx, ny, f_bear, f_seg)

    successful_p3 = set()
    reverted_p3 = set()
    
    active_p3_bldgs = list(p3_updates.keys()) + list(p3_occluder_updates.keys())
    if active_p3_bldgs:
        arcpy.AddMessage(f"   -> Executing Pass 3 (Testing {len(p3_updates)} fallbacks & late syncs)...")
        backup_states_p3 = {}
        with arcpy.da.SearchCursor("memory\\Bldgs_Base", ["Bldg_ID", "ROAD_X", "ROAD_Y", "BEARING", "Segment_ID"]) as c:
            for r in c:
                if r[0] in active_p3_bldgs:
                    backup_states_p3[r[0]] = {'x': r[1], 'y': r[2], 'bearing': r[3], 'parent': r[4]}
                    
        for fc in ["memory\\Bldgs_Base", "memory\\Bldgs_Centroids"]:
            with arcpy.da.UpdateCursor(fc, ["Bldg_ID", "ROAD_X", "ROAD_Y", "BEARING", "Segment_ID"]) as cur:
                for row in cur:
                    b_id = row[0]
                    if b_id in p3_occluder_updates:
                        row[1], row[2], row[3], row[4] = p3_occluder_updates[b_id][:4]
                        cur.updateRow(row)
                    elif b_id in p3_updates:
                        row[1], row[2], row[3], row[4] = p3_updates[b_id][:4]
                        cur.updateRow(row)
                        
        sightline_lyr_p3, filtered_walls_p3, pts_valid_p3, p3_metrics, _ = run_raycast_pass("P3", active_p3_bldgs)
        
        for b_id in p3_updates:
            _, new_ray, _ = p3_metrics.get(b_id, (0.0, 0.0, 0.0))
            if new_ray > 0.0:
                successful_p3.add(b_id)
                if b_id in rule1_fallback_best and b_id not in late_semi_sync_updates:
                    bldg_metrics[b_id]['dist'] = rule1_fallback_best[b_id][4]
            else:
                reverted_p3.add(b_id)
                
        bldgs_to_revert_p3 = set(reverted_p3)
        for b_id in reverted_p3:
            for occ_id in master_occluder_dict.get(b_id, []):
                if occ_id in p3_occluder_updates: bldgs_to_revert_p3.add(occ_id)
                    
        if bldgs_to_revert_p3:
            for fc in ["memory\\Bldgs_Base", "memory\\Bldgs_Centroids"]:
                with arcpy.da.UpdateCursor(fc, ["Bldg_ID", "ROAD_X", "ROAD_Y", "BEARING", "Segment_ID"]) as cur:
                    for row in cur:
                        if row[0] in bldgs_to_revert_p3:
                            old = backup_states_p3[row[0]]
                            row[1], row[2], row[3], row[4] = old['x'], old['y'], old['bearing'], old['parent']
                            cur.updateRow(row)
    else:
        sightline_lyr_p3, filtered_walls_p3, pts_valid_p3, p3_metrics = None, None, None, {}

    p2_final_bldgs = set(successful_p2)            
    p3_final_bldgs = set(successful_p3)
    for b in successful_p3:
        p3_final_bldgs.update([occ for occ in master_occluder_dict.get(b, []) if occ in p3_occluder_updates])

    arcpy.management.CopyFeatures(filtered_walls_p1, "memory\\Walls_Final_Merged")
    arcpy.management.CopyFeatures(sightline_lyr_p1, "memory\\Sightlines_Final_Merged")
    
    all_new_bldgs = p2_final_bldgs.union(p3_final_bldgs)
    if all_new_bldgs:
        with arcpy.da.UpdateCursor("memory\\Walls_Final_Merged", ["Bldg_ID"]) as c:
            for r in c:
                if r[0] in all_new_bldgs: c.deleteRow()
        with arcpy.da.UpdateCursor("memory\\Sightlines_Final_Merged", ["Bldg_ID"]) as c:
            for r in c:
                if r[0] in all_new_bldgs: c.deleteRow()
                
    if p2_final_bldgs:
        arcpy.management.MakeFeatureLayer(filtered_walls_p2, "P2_Walls_Lyr")
        arcpy.management.MakeFeatureLayer(sightline_lyr_p2, "P2_Sight_Lyr")
        query = f"Bldg_ID IN ({','.join(map(str, p2_final_bldgs))})"
        arcpy.management.SelectLayerByAttribute("P2_Walls_Lyr", "NEW_SELECTION", query)
        arcpy.management.SelectLayerByAttribute("P2_Sight_Lyr", "NEW_SELECTION", query)
        arcpy.management.Append("P2_Walls_Lyr", "memory\\Walls_Final_Merged", "NO_TEST")
        arcpy.management.Append("P2_Sight_Lyr", "memory\\Sightlines_Final_Merged", "NO_TEST")
        
    if p3_final_bldgs:
        arcpy.management.MakeFeatureLayer(filtered_walls_p3, "P3_Walls_Lyr")
        arcpy.management.MakeFeatureLayer(sightline_lyr_p3, "P3_Sight_Lyr")
        query = f"Bldg_ID IN ({','.join(map(str, p3_final_bldgs))})"
        arcpy.management.SelectLayerByAttribute("P3_Walls_Lyr", "NEW_SELECTION", query)
        arcpy.management.SelectLayerByAttribute("P3_Sight_Lyr", "NEW_SELECTION", query)
        arcpy.management.Append("P3_Walls_Lyr", "memory\\Walls_Final_Merged", "NO_TEST")
        arcpy.management.Append("P3_Sight_Lyr", "memory\\Sightlines_Final_Merged", "NO_TEST")
        
    final_walls = "memory\\Walls_Final_Merged"
    final_sightlines = "memory\\Sightlines_Final_Merged"
    
    final_metrics = {}
    for b_id in oid_to_bldg.values():
        if b_id in p2_final_bldgs: final_metrics[b_id] = p2_metrics.get(b_id, (0.0, 0.0, 0.0))
        elif b_id in p3_final_bldgs: final_metrics[b_id] = p3_metrics.get(b_id, (0.0, 0.0, 0.0))
        else: final_metrics[b_id] = p1_metrics.get(b_id, (0.0, 0.0, 0.0))

    # ==========================================
    # EXPORT NEW RECOVERY DEBUG LAYERS
    # ==========================================
    if save_intermediates:
        if occ_candidates:
            arcpy.analysis.Select("memory\\Bldgs_Base", "memory\\Failed_Polys", f"Bldg_ID IN ({','.join(map(str, occ_candidates))})")
            add_to_occ_group(arcpy.conversion.FeatureClassToFeatureClass("memory\\Failed_Polys", out_gdb, "OCC_1_FailedBuildings"), "OCC_1_FailedBuildings")
            add_to_occ_group(arcpy.conversion.FeatureClassToFeatureClass("memory\\Failed_Diag", out_gdb, "OCC_3_FailedDiagLines"), "OCC_3_FailedDiagLines")
            add_to_occ_group(arcpy.conversion.FeatureClassToFeatureClass("memory\\Occ_Check", out_gdb, "OCC_4_OccCheckJoin"), "OCC_4_OccCheckJoin")

        recovered_main = [b for b in successful_p2.union(successful_p3) if b in occluded_bldgs]
        if recovered_main:
            arcpy.analysis.Select("memory\\Bldgs_Base", "memory\\Recovered_Main", f"Bldg_ID IN ({','.join(map(str, recovered_main))})")
            add_to_occ_group(arcpy.conversion.FeatureClassToFeatureClass("memory\\Recovered_Main", out_gdb, "OCC_5_RecoveredMainBldgs"), "OCC_5_RecoveredMainBldgs")

        recovered_occ = [occ for b in successful_p2.union(successful_p3) for occ in master_occluder_dict.get(b, [])]
        if recovered_occ:
            arcpy.analysis.Select("memory\\Bldgs_Base", "memory\\Infected_Occluders", f"Bldg_ID IN ({','.join(map(str, recovered_occ))})")
            add_to_occ_group(arcpy.conversion.FeatureClassToFeatureClass("memory\\Infected_Occluders", out_gdb, "OCC_6_InfectedOccluders"), "OCC_6_InfectedOccluders")
            
        if low_angle_updates:
            arcpy.analysis.Select("memory\\Bldgs_Base", "memory\\LowAngle_Tested", f"Bldg_ID IN ({','.join(map(str, low_angle_updates.keys()))})")
            add_to_occ_group(arcpy.conversion.FeatureClassToFeatureClass("memory\\LowAngle_Tested", out_gdb, "OCC_7_LowAngleTested"), "OCC_7_LowAngleTested")
        
        low_ang_improved = [b for b in successful_p2 if b in low_angle_bldgs]
        if low_ang_improved:
            arcpy.analysis.Select("memory\\Bldgs_Base", "memory\\LowAngle_Improved", f"Bldg_ID IN ({','.join(map(str, low_ang_improved))})")
            add_to_occ_group(arcpy.conversion.FeatureClassToFeatureClass("memory\\LowAngle_Improved", out_gdb, "OCC_8_LowAngleImproved"), "OCC_8_LowAngleImproved")

        odd_ones_improved = [b for b in successful_p2 if b in odd_one_updates]
        if odd_ones_improved:
            arcpy.analysis.Select("memory\\Bldgs_Base", "memory\\OddOneOut_Recovered", f"Bldg_ID IN ({','.join(map(str, odd_ones_improved))})")
            add_to_occ_group(arcpy.conversion.FeatureClassToFeatureClass("memory\\OddOneOut_Recovered", out_gdb, "OCC_10_OddOneOut"), "OCC_10_OddOneOut")

        semi_detached_improved = [b for b in successful_p2 if b in semi_detached_updates]
        if semi_detached_improved:
            arcpy.analysis.Select("memory\\Bldgs_Base", "memory\\SemiDetached_Recovered", f"Bldg_ID IN ({','.join(map(str, semi_detached_improved))})")
            add_to_occ_group(arcpy.conversion.FeatureClassToFeatureClass("memory\\SemiDetached_Recovered", out_gdb, "OCC_11_SemiDetached"), "OCC_11_SemiDetached")

        arcpy.management.CreateFeatureclass("memory", "Recovery_Targets", "POINT", spatial_reference=sr)
        arcpy.management.AddField("memory\\Recovery_Targets", "Bldg_ID", "LONG")
        arcpy.management.AddField("memory\\Recovery_Targets", "New_Bearing", "DOUBLE")
        with arcpy.da.InsertCursor("memory\\Recovery_Targets", ["SHAPE@XY", "Bldg_ID", "New_Bearing"]) as icur:
            for b_id, data in p2_updates.items():
                icur.insertRow([(data[0], data[1]), b_id, data[2]])
            for b_id, data in p3_updates.items():
                icur.insertRow([(data[0], data[1]), b_id, data[2]])
        add_to_occ_group(arcpy.conversion.FeatureClassToFeatureClass("memory\\Recovery_Targets", out_gdb, "OCC_9_RecoveryRoadPoints"), "OCC_9_RecoveryRoadPoints")

    # ==========================================
    # PHASE 3.5: COMPILING FINAL METRICS & CONFIDENCE ENGINE
    # ==========================================
    arcpy.AddMessage("   -> Compiling raw spatial matrices and calculating Logistic Confidence...")

    for b_id, (ang, ray, f_len) in final_metrics.items():
        if b_id in bldg_metrics:
            bldg_metrics[b_id]['angle'] = ang
            bldg_metrics[b_id]['raycast'] = ray
            peri = bldg_metrics[b_id]['perimeter']
            bldg_metrics[b_id]['facade_ratio'] = f_len / peri if peri > 0 else 0.0

    # PRE-CALCULATE PERFECT TERRACES (Consensus = Bearing + 360 Vector)
    final_states = {}
    with arcpy.da.SearchCursor("memory\\Bldgs_Base", ["Bldg_ID", "ROAD_X", "ROAD_Y", "BEARING"]) as sc:
        for r in sc:
            if r[3] is not None:
                final_states[r[0]] = {'x': r[1], 'y': r[2], 'bearing': r[3]}

    perfect_terraces = set()
    for cluster in clusters:
        if len(cluster) >= 4:
            valid_members = [b for b in cluster if b in final_states]
            
            if len(valid_members) == len(cluster):
                base_b = final_states[valid_members[0]]['bearing']
                cen_base = centroid_geoms.get(valid_members[0])
                if not cen_base: continue
                
                rx_base, ry_base = final_states[valid_members[0]]['x'], final_states[valid_members[0]]['y']
                h_base = math.degrees(math.atan2(rx_base - cen_base.firstPoint.X, ry_base - cen_base.firstPoint.Y))
                if h_base < 0: h_base += 360
                
                is_perfect = True
                for b_id in valid_members[1:]:
                    b_ang = final_states[b_id]['bearing']
                    diff_b = abs(b_ang - base_b) % 180
                    if diff_b > 90: diff_b = 180 - diff_b
                    if diff_b > 10.0:
                        is_perfect = False
                        break
                    
                    cen = centroid_geoms.get(b_id)
                    if not cen:
                        is_perfect = False
                        break
                    rx, ry = final_states[b_id]['x'], final_states[b_id]['y']
                    h = math.degrees(math.atan2(rx - cen.firstPoint.X, ry - cen.firstPoint.Y))
                    if h < 0: h += 360
                    
                    diff_v = abs(h - h_base)
                    if diff_v > 180: diff_v = 360 - diff_v
                    if diff_v > 45.0:
                        is_perfect = False
                        break
                        
                if is_perfect:
                    perfect_terraces.update(valid_members)
                    
    if perfect_terraces:
        arcpy.AddMessage(f"   -> Identified {len(perfect_terraces)} buildings in perfectly unified terraces.")

    # COMPUTE CONTINUOUS PEER CONSENSUS SCORE (0.0 to 1.0)
    for b_id, m in bldg_metrics.items():
        touches = touch_dict.get(b_id, [])
        valid_touches = [t for t in touches if t in final_states]
        
        if not valid_touches or b_id not in final_states:
            m['peer_consensus_score'] = 0.0
            continue
            
        my_state = final_states[b_id]
        my_bear = my_state['bearing']
        my_cen = centroid_geoms.get(b_id)
        
        if not my_cen:
            m['peer_consensus_score'] = 0.0
            continue
            
        my_h = math.degrees(math.atan2(my_state['x'] - my_cen.firstPoint.X, my_state['y'] - my_cen.firstPoint.Y))
        if my_h < 0: my_h += 360
        
        match_count = 0
        for t_id in valid_touches:
            t_state = final_states[t_id]
            t_bear = t_state['bearing']
            
            diff_bear = abs(my_bear - t_bear) % 180
            if diff_bear > 90: diff_bear = 180 - diff_bear
            
            t_cen = centroid_geoms.get(t_id)
            if not t_cen: continue
            
            t_h = math.degrees(math.atan2(t_state['x'] - t_cen.firstPoint.X, t_state['y'] - t_cen.firstPoint.Y))
            if t_h < 0: t_h += 360
            
            diff_h = abs(my_h - t_h)
            if diff_h > 180: diff_h = 360 - diff_h
            
            if diff_bear <= 25.0 and diff_h <= 45.0:
                match_count += 1
                
        if match_count == len(valid_touches):
            m['peer_consensus_score'] = 1.0
        elif match_count > 0:
            m['peer_consensus_score'] = 0.5
        else:
            m['peer_consensus_score'] = 0.0

    score_fields = [
        ["ANGLE_RAW", "DOUBLE"], ["RAYCAST_RAW", "DOUBLE"], 
        ["DIST_RAW", "DOUBLE"], ["AMBIG_RAW", "DOUBLE"], 
        ["FACADE_RATIO", "DOUBLE"], ["COMPLEXITY_IDX", "DOUBLE"],
        ["WAS_OCCLUDED_P1", "SHORT"], ["WAS_LOW_ANGLE_P1", "SHORT"], 
        ["IS_INFECTED_OCCLUDER", "SHORT"], ["USED_RULE1_90DEG", "SHORT"], 
        ["USED_RULE2_RESONANCE", "SHORT"], ["USED_PEER_CONSENSUS", "SHORT"], 
        ["USED_FALLBACK_P3", "SHORT"], ["PEER_CONSENSUS_SCORE", "DOUBLE"], 
        ["IS_PERFECT_TERRACE", "SHORT"], ["PRED_CONFIDENCE", "DOUBLE"] 
    ]
    arcpy.management.AddFields("memory\\Bldgs_Base", score_fields)
    
    field_names = [
        "Bldg_ID", "ANGLE_RAW", "RAYCAST_RAW", "DIST_RAW", "AMBIG_RAW", "FACADE_RATIO", "COMPLEXITY_IDX", 
        "WAS_OCCLUDED_P1", "WAS_LOW_ANGLE_P1", "IS_INFECTED_OCCLUDER", "USED_RULE1_90DEG", "USED_RULE2_RESONANCE", 
        "USED_PEER_CONSENSUS", "USED_FALLBACK_P3", "PEER_CONSENSUS_SCORE", "IS_PERFECT_TERRACE", 
        "PRED_CONFIDENCE", "Recovery_Type", "Failure_Reason"
    ]
    
    with arcpy.da.UpdateCursor("memory\\Bldgs_Base", field_names) as cur:
        for row in cur:
            b_id = row[0]
            m = bldg_metrics.get(b_id)
            
            rec_type = "None (Pass 1 Success)"
            fail_rsn = ""

            if b_id in skipped_roundabouts:
                rec_type = "N/A"
                fail_rsn = "Skipped: Snapped to Roundabout"
            elif b_id not in unified_dict:
                rec_type = "N/A"
                fail_rsn = "No Road Found within Search Distance"
            elif b_id in p3_final_bldgs:
                if b_id in p3_occluder_updates: rec_type = "Infected Occluder (Fallback)"
                elif b_id in late_semi_sync_updates: rec_type = "Late Semi-Detached Sync"
                elif b_id in rule1_fallback_best: rec_type = "Rule 1: Fallback (>25 deg)"
            elif b_id in p2_final_bldgs:
                if b_id in p2_occluder_updates: rec_type = "Infected Occluder"
                elif b_id in odd_one_updates: rec_type = "Terrace Odd-One-Out"
                elif b_id in semi_detached_updates: rec_type = "Semi-Detached Party Wall"
                elif b_id in rule1_condA_best: rec_type = "Rule 1: 25m/90%"
                elif b_id in rule1_condB_best: rec_type = "Rule 1: 50m/95%"
                elif b_id in low_angle_updates: rec_type = "Rule 2: Geometric Resonance"
            elif b_id in survivors:
                if b_id in end_cap_updates: rec_type = "End-Cap Pre-Pass Override"

            ray = final_metrics.get(b_id, (0.0, 0.0, 0.0))[1]
            if ray == 0.0 and fail_rsn == "":
                if b_id in odd_one_updates: fail_rsn = "0% Vis: Failed after Terrace Back-Facade Fix"
                elif b_id in semi_detached_updates: fail_rsn = "0% Vis: Failed after Semi-Detached Party Wall Fix"
                elif b_id in late_semi_sync_updates: fail_rsn = "0% Vis: Failed after Late Semi-Detached Sync"
                else: fail_rsn = "0% Visibility / Fully Occluded"

            if m:
                row[1] = m['angle']
                row[2] = m['raycast']
                row[3] = m['dist']
                row[4] = m['ambig']
                row[5] = m['facade_ratio']
                row[6] = m['complexity']

                row[7] = 1 if b_id in occluded_bldgs else 0
                row[8] = 1 if b_id in low_angle_bldgs else 0
                row[9] = 1 if "Occluder" in rec_type else 0
                row[10] = 1 if "Rule 1: 25m" in rec_type or "Rule 1: 50m" in rec_type else 0
                row[11] = 1 if "Rule 2" in rec_type else 0
                row[12] = 1 if "End-Cap" in rec_type or "Odd-One-Out" in rec_type or "Semi-Detached" in rec_type or "Late Semi-Detached Sync" in rec_type else 0
                row[13] = 1 if "Fallback (>25 deg)" in rec_type else 0
                row[14] = m.get('peer_consensus_score', 0.0)
                row[15] = 1 if b_id in perfect_terraces else 0

                # Base Logistic Regression Confidence
                t_count = len(touch_dict.get(b_id, []))
                base_conf = calculate_confidence(
                    angle=row[1], 
                    raycast=row[2], 
                    dist=row[3], 
                    ambig=row[4], 
                    touch_count=t_count, 
                    is_occ=row[9], 
                    used_r2=row[11], 
                    used_p3=row[13], 
                    is_terrace=row[15],                 
                    peer_score=row[14], 
                    fail_reason=fail_rsn
                )
                row[16] = base_conf
            else:
                for i in range(1, 17): row[i] = 0.0
            
            row[17] = rec_type if fail_rsn == "" else "N/A"
            row[18] = fail_rsn

            cur.updateRow(row)

    # ==========================================
    # PHASE 4: FINAL EXPORTS & DEBUG MAP
    # ==========================================
    arcpy.AddMessage("Step 5: Extracting the final true Front Facades...")
    walls_lyr = arcpy.management.MakeFeatureLayer(final_walls, "Walls_Lyr")
    arcpy.management.SelectLayerByLocation(walls_lyr, "INTERSECT", final_sightlines)
    arcpy.management.CopyFeatures(walls_lyr, output_facades)

    join_fields = ["ANGLE_RAW", "RAYCAST_RAW", "DIST_RAW", "AMBIG_RAW", "FACADE_RATIO", "COMPLEXITY_IDX", "WAS_OCCLUDED_P1", "WAS_LOW_ANGLE_P1", "IS_INFECTED_OCCLUDER", "USED_RULE1_90DEG", "USED_RULE2_RESONANCE", "USED_PEER_CONSENSUS", "USED_FALLBACK_P3", "PEER_CONSENSUS_SCORE", "IS_PERFECT_TERRACE", "PRED_CONFIDENCE", "Recovery_Type", "Failure_Reason"]
    
    # Join the original GUID back onto the final facade outputs
    if bldg_guid_field and bldg_guid_field not in ["", "#"]:
        join_fields.insert(0, "ORIG_GUID")
        
    if road_guid_field and road_guid_field not in ["", "#"]:
        arcpy.management.JoinField("memory\\Bldgs_Base", "Segment_ID", "memory\\Roads_Base", "OBJECTID", ["ROAD_ORIG_GUID"])
        join_fields.insert(1, "ROAD_ORIG_GUID")
        
    arcpy.management.JoinField(output_facades, "Bldg_ID", "memory\\Bldgs_Base", "Bldg_ID", join_fields)

    arcpy.AddMessage("Step 6: Generating Rich Centroids Output...")
    # Because Bldgs_Base contains the GUIDs, FeatureToPoint automatically pulls it into out_centroids!
    arcpy.management.FeatureToPoint("memory\\Bldgs_Base", out_centroids, "INSIDE")
    
    try:
        arcpy.management.JoinField(out_centroids, "Segment_ID", "memory\\Roads_Base", "OBJECTID")
    except Exception as e:
        arcpy.AddMessage("   -> Note: Could not dynamically join all road segment fields to the final centroids.")

    if save_intermediates:
        arcpy.AddMessage("--> Saving and grouping Intermediates...")
        
        arcpy.management.AddGeometryAttributes("memory\\Bldgs_Centroids", "POINT_X_Y_Z_M")
        arcpy.analysis.Select("memory\\Bldgs_Centroids", "memory\\Bldgs_Cen_Final", "ROAD_X IS NOT NULL")
        arcpy.management.XYToLine("memory\\Bldgs_Cen_Final", "memory\\Diag_Final", "ROAD_X", "ROAD_Y", "POINT_X", "POINT_Y", id_field="Bldg_ID")

        out_path1a = os.path.join(out_gdb, "DEBUG_1a_RoadOriginPoints")
        arcpy.management.CreateFeatureclass(out_gdb, "DEBUG_1a_RoadOriginPoints", "POINT", spatial_reference=sr)
        with arcpy.da.InsertCursor(out_path1a, ["SHAPE@XY"]) as icur:
            for row in arcpy.da.SearchCursor("memory\\Bldgs_Cen_Final", ["ROAD_X", "ROAD_Y"]):
                icur.insertRow([(row[0], row[1])])
        add_to_group(out_path1a, "DEBUG_1a_RoadOriginPoints")

        add_to_group(arcpy.conversion.FeatureClassToFeatureClass("memory\\Bldgs_Cen_Final", out_gdb, "DEBUG_1b_Centroids"), "DEBUG_1b_Centroids")
        add_to_group(arcpy.conversion.FeatureClassToFeatureClass("memory\\Diag_Final", out_gdb, "DEBUG_1c_OriginToCentroidLines"), "DEBUG_1c_OriginToCentroidLines")
        add_to_group(arcpy.conversion.FeatureClassToFeatureClass(final_walls, out_gdb, "DEBUG_2_AngleFilteredWalls"), "DEBUG_2_AngleFilteredWalls")
        
        try:
            add_to_group(arcpy.conversion.FeatureClassToFeatureClass("memory\\Attempted_Sightlines_P1", out_gdb, "DEBUG_3_AttemptedRaycasts"), "DEBUG_3_AttemptedRaycasts")
        except: pass
        
        add_to_group(arcpy.conversion.FeatureClassToFeatureClass(final_sightlines, out_gdb, "DEBUG_4_ClearSightlines"), "DEBUG_4_ClearSightlines")

    arcpy.AddMessage(f"Success! Final True Front Facade lines saved to: {output_facades}")
    arcpy.AddMessage(f"Success! Rich Centroid points saved to: {out_centroids}")

if __name__ == '__main__':
    in_buildings = arcpy.GetParameterAsText(0)
    in_roads = arcpy.GetParameterAsText(1)
    out_facades = arcpy.GetParameterAsText(2)
    out_centroids = arcpy.GetParameterAsText(3) 
    bldg_guid_field = arcpy.GetParameterAsText(4) 
    road_guid_field = arcpy.GetParameterAsText(5) 
    save_bool_str = arcpy.GetParameterAsText(6).lower() 
    save_intermediates = (save_bool_str == 'true')

    extract_facades(in_buildings, in_roads, out_facades, out_centroids, bldg_guid_field, road_guid_field, save_intermediates)