import csv
import os
import numpy as np
import pandas as pd
from calib_functions_ball_drop import *


##################################################################################################USER EDITS START HERE################################################################################################
#Enter the path to the focal follow csv file.
CSV_Path="/Users/ellag/Downloads/ball_drop_data.xlsx"
#Enter the path/name for your output csv file.
Output_calib="/Users/ellag/Library/CloudStorage/GoogleDrive-elhe2720@colorado.edu/Shared drives/Field Research Videos/Gil Lab/Curacao_2024/garden_eels/position_drop_experiment/calib_results_trial2.xlsx"
#Path for triangulated points
output_path="/Users/ellag/Library/CloudStorage/GoogleDrive-elhe2720@colorado.edu/Shared drives/Field Research Videos/Gil Lab/Curacao_2024/garden_eels/position_drop_experiment/ball_drop_data_3D_trial2.xlsx"
#Base path for frames
base_path="/Users/ellag/Library/CloudStorage/GoogleDrive-elhe2720@colorado.edu/Shared drives/Field Research Videos/Gil Lab/Curacao_2024/garden_eels/position_drop_experiment/triangulation_frames"


#Enter sheet name here
df = pd.read_excel(CSV_Path, sheet_name = 'individual_response_data_master')

#Define your 'point sets'. For example, I wanted 4 sets of xyz values for each eel. Use your column names in your spreadsheet. 
point_sets = {
    "ball_first_view": (
        ["ball_first_view_x_cam1", "ball_first_view_y_cam1"],
        ["ball_first_view_x_cam2", "ball_first_view_y_cam2"]
    ),
    "ball_hit": (
        ["ball_hit_x_cam1", "ball_hit_y_cam1"],
        ["ball_hit_x_cam2", "ball_hit_y_cam2"]
    ),
    "base": (
        ["base_x_cam1", "base_y_cam1"],
        ["base_x_cam2", "base_y_cam2"]
    ),
    "head": (
        ["head_x_cam1", "head_y_cam1"],
        ["head_x_cam2", "head_y_cam2"]
    )
}


os.chdir(base_path)

#Enter the column headings for your csv file.
#Column containing the left/first calibration video file names.
Vid_Col1="calib_1"
#Column containing the right/second calibration video file names.
Vid_Col2="calib_2"
#Column containing the trial IDs.
trial_ID_Col="trial_ID"
#Column containing drop IDs. 
drop_ID_Col="drop_ID"
#Column containing the start time of the calibration videos.
Start_Col1="calib_start_time1"
Start_Col2="calib_start_time2"
#Column containing the start time of the calibration videos.
End_Col1="calib_end_time1"
End_Col2="calib_end_time2"
#Column containing the folder names of the calibration videos.
Folder_Col1="calib_1_path"
Folder_Col2="calib_2_path"

df = df[df["trial_ID"] ==2]

current_trial_id = None


# Dictionary to cache calibration results per trial
calibration_cache = {}
calibration_metrics = []

for label in point_sets.keys():
    for axis in ["X", "Y", "Z"]:
        new_col = f"{label}_{axis}"
        if new_col not in df.columns:
            df[new_col] = np.nan

for i, row in df.iterrows():
    # Join folder + filename to get full path for each video
    video1_name = row[Vid_Col1]
    video2_name = row[Vid_Col2]
    video1_path = row[Folder_Col1]
    video2_path = row[Folder_Col2]
    start1 = row[Start_Col1]
    end1 = row[End_Col1]
    start2 = row[Start_Col2]
    end2 = row[End_Col2]
    trial_id = row[trial_ID_Col]
    drop_id = row[drop_ID_Col]

    print(f"\nProcessing Drop {drop_id} (Trial {trial_id})")
    print(f"Videos: {video1_name}, {video2_name}")

    folder_name = f"{os.path.basename(video1_path)} & {os.path.basename(video2_path)}"

    # Check if this trial's calibration is already cached
    if trial_id in calibration_cache:
        print(f"⚙️  Reusing cached calibration for Trial {trial_id}")
        mtx1, dist1, mtx2, dist2, R, T, cam_distance = calibration_cache[trial_id]

    else:
        print(f"🧮 Performing calibration for Trial {trial_id}")
        try:
            # Perform stereo calibration
            rmse, mtx1, dist1, mtx2, dist2, R, T = cam_calibration(
                base_path, video1_path, video2_path, video1_name, video2_name, start1, end1, trial_id)

            # Compute camera distance (you might need to adapt this)
            cam_distance = float(np.linalg.norm(T)) if T is not None else float('nan')

            # Store calibration results for this trial
            print(f"storing calibration results...")
            calibration_cache[trial_id] = (mtx1, dist1, mtx2, dist2, R, T, cam_distance)

            calibration_metrics.append({
                "trial_id": trial_id,
                "rmse": rmse,
                "mtx1": mtx1,
                "mtx2": mtx2,
                "dist1": dist1,
                "dist2":dist2,
                "R" : R,
                "T" : T,
                "cam_distance" : cam_distance
            })

        except Exception as e:
            print(f"❌ Error calibrating Trial {trial_id}: {e}")
            cam_distance = float('nan')
            # Store a placeholder to prevent retrying the failed calibration
            calibration_cache[trial_id] = (None, None, None, None, None, None, None)
            raise SystemExit
        
  
    # Dictionary to store triangulated 3D results for this row
    triangulated_points = {}
    for label, (cam1_cols, cam2_cols) in point_sets.items():
            try:
                # Extract 2D pixel coordinates for this point from the current row
                x1, y1 = row[cam1_cols[0]], row[cam1_cols[1]]
                x2, y2 = row[cam2_cols[0]], row[cam2_cols[1]]

                # Skip if any coordinate is missing
                if pd.isna(x1) or pd.isna(y1) or pd.isna(x2) or pd.isna(y2):
                    print(f"Skipping {label}: missing coordinate(s).")
                    triangulated_points[label] = (None, None, None)
                    continue

                # Prepare single-point arrays for triangulation
                pts_L = np.array([[x1, y1]], dtype=float)
                pts_R = np.array([[x2, y2]], dtype=float)

                # Triangulate single 3D point
                point_3D = triangulate(mtx1, mtx2, R, T, pts_L, pts_R)

                # If your triangulate() returns Nx3 array, extract the first element
                point_3D = point_3D[0] if len(point_3D.shape) > 1 else point_3D
                
                # Update dataframe values
                df.at[i, f"{label}_X"] = point_3D[0]
                df.at[i, f"{label}_Y"] = point_3D[1]
                df.at[i, f"{label}_Z"] = point_3D[2]

            except Exception as e:
                print(f"❌ Error triangulating {label} (row {i}): {e}")
                # Leave as NaN if something fails
                continue
            
    # Check if we've moved to a new trial or reached the end of the dataframe
    next_trial_id = df.iloc[i + 1][trial_ID_Col] if i + 1 < len(df) else None

    # If the next row is a new trial or we're at the last row, save the dataframe
    if next_trial_id != trial_id:
        print(f"💾 Saving progress after finishing Trial {trial_id}...")

        df.to_excel(output_path, index=False)
        print(f"✅ Saved after Trial {trial_id}")

        # --- Save calibration metrics ---
        calib_df = pd.DataFrame(calibration_metrics)
        calib_df.to_excel(Output_calib, index=False)
        print(f"📊 Calibration metrics saved to: {Output_calib}")



print(f"\n✅ All done!")
