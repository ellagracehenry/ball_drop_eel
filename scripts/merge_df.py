import pandas as pd
from pathlib import Path

# folder containing the Excel files
folder = Path("/Users/ellag/Library/CloudStorage/GoogleDrive-elhe2720@colorado.edu/Shared drives/Field Research Videos/Gil Lab/Curacao_2024/garden_eels/position_drop_experiment")

# trials to include
trials = [1,2,3,4,5,6,7,8,9,10,11,13]

dfs = []

for i, trial in enumerate(trials):
    file_path = folder / f"ball_drop_data_3D_trial{trial}.xlsx"
    
    if i == 0:
        # keep headers for first file
        df = pd.read_excel(file_path)
    else:
        # read data using existing headers, don't duplicate
        df = pd.read_excel(file_path)
    
    dfs.append(df)

# concatenate all trials
master_df = pd.concat(dfs, ignore_index=True)

# write to Excel
output_path = folder / "master_ball_drop_data_3D_0121.xlsx"
master_df.to_excel(output_path, index=False)
