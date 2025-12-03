import librosa
import numpy as np
import os
import subprocess
import tarfile
from scipy.signal import correlate
from scipy.spatial.transform import Rotation as R

import cv2 as cv
import numpy as np
import os
import subprocess
from scipy.signal import correlate
import librosa
from scipy.spatial.transform import Rotation as R
import sys
import xml.etree.ElementTree as ET
import numpy as np
import xml.etree.ElementTree as ET
import csv
import pandas as pd

from datetime import datetime, time


#########Frame Syncing and Cutting Functions

def search_csv(filename, search_column, search_value, folder_column, folder):
    found_rows = []
    with open(filename, 'r', newline='') as csvfile:
        reader = csv.DictReader(csvfile)
        for row in reader:
            if search_column in row and row[search_column] == search_value.removesuffix('.MP4') and folder_column in row and row[folder_column] == folder:
                print(row)
                found_rows.append(row)
    return found_rows

def search_xlsx(filename, search_column, search_value, folder_column, folder):
    # Load the Excel file into a pandas DataFrame
    df = pd.read_excel(filename)

    # Remove .MP4 suffix from search value to match your original logic
    search_value = search_value.removesuffix('.MP4')

    # Filter rows that match both conditions
    mask = (df[search_column] == search_value) & (df[folder_column] == folder)
    matching_rows = df.loc[mask]

    # Print and return as a list of dictionaries (to stay consistent with your old function)
    found_rows = matching_rows.to_dict(orient='records')
    for row in found_rows:
        print(row)
    return found_rows

def cam_calibration(base_path, left_cal_vid, right_cal_vid, left_cal, right_cal, start_time, end_time, trial_id):

    out_fps="3"

    # Build full paths for both videos
    cal_video_list = [
    os.path.join(left_cal_vid, left_cal +".MP4"),
    os.path.join(right_cal_vid, right_cal +".MP4")
    ]

    video_name_list = [left_cal, right_cal]

    # Create left/right directories calibration frames
    cal_dir_L=str(left_cal) + "_trial" + str(trial_id) + "_" + "cam1_synced"
    cal_dir_R=str(right_cal) + "_trial" + str(trial_id) + "_" + "cam2_synced"

    cal_dir_list = [cal_dir_L, cal_dir_R]

    if not os.path.exists(cal_dir_L):
        os.makedirs(cal_dir_L)
        print("Directory for left calibration video created successfully!")
    else:
        print("Directory for left calibration video already exists!")

    if not os.path.exists(cal_dir_R):
        os.makedirs(cal_dir_R)
        print("Directory for right calibration video created successfully!")
    else:
        print("Directory for right calibration video already exists!")


    # Extract frames rate from calibration videos
    left_cal_mp4=cal_video_list[0]
    right_cal_mp4=cal_video_list[1]
    fps = get_frame_rate(left_cal_mp4)
    print(fps)
    fps_r=get_frame_rate(right_cal_mp4)
    print(fps_r)

    # Check frame rates are consistent
    if not fps == fps_r:
        print("Videos recorded with different fps, quitting")
        sys.exit()
    else:
        print("Original video fps is ",fps, ". Extracting at ", out_fps, "fps.")

    # Extract frame delay
    frame_delay=extract_audio_features(cal_video_list[0], cal_video_list[1],fps)

    #Adds / to end of base path
    #base_path+="/"

    # Extract synced frames
    for x in range(len(cal_video_list)):
        print(f"here")
        extract_frames_new(cal_dir_list[x], cal_video_list[x], video_name_list[x], frame_delay[x], fps, out_fps, start_time,end_time)
        print("Calibration frames extracted") 

    #Cut off the extra bits at the end so the number of images is the same
    delete_unmatched_files(cal_dir_L, cal_dir_R)

    start_time = parse_time(start_time)
    end_time = parse_time(end_time) 
 
    # #Cut off the beginning of the video if it is above water
    # cut_frames=(float(start_time)*float(out_fps))
    # print(cut_frames)
    # if int(cut_frames)>0:
    #     remove_first_n_files(cal_dir_L,int(cut_frames))
    #     remove_first_n_files(cal_dir_R,int(cut_frames))
    #     print("Unnecessary frames deleted.")
    # else:
    #     print("Calibration starts immediately. No files to delete")

    ######### Run calibration and triangulation ########## 
    # Calibrate left camera:
    print(f"calibrating cam1")
    mtx1, dist1 = calibrate_camera(base_path, images_folder=cal_dir_L)

    print(f"calibrating cam2")
    # Calibrate right camera:
    mtx2, dist2 = calibrate_camera(base_path, images_folder=cal_dir_R)

    print(f"stereocalibrating...")
    # Calibrate cameras together:
    rmse, R, T = stereo_calibrate(base_path, mtx1, dist1, mtx2, dist2, cal_dir_L, cal_dir_R)

    # The baseline (distance between cameras in real-world units)
    return rmse, mtx1, dist1, mtx2, dist2, R, T

#Get Frame Rate
def get_frame_rate(video_path):
    # Run ffprobe to get the frame rate in the format "numerator/denominator"
    result = subprocess.run(
        ['ffprobe', '-v', 'error', '-select_streams', 'v:0', '-show_entries', 'stream=r_frame_rate', '-of', 'default=nk=1:nw=1', video_path],
        capture_output=True,
        text=True
    )
    
    # Extract the frame rate string
    frame_rate_str = result.stdout.strip()
    
    # Split the frame rate string by '/' and calculate the rate
    numerator, denominator = map(int, frame_rate_str.split('/'))
    frame_rate = numerator / denominator
    
    return frame_rate


#function to extract audio features and find the delay between cameras. Frame rate must be the same for
#the cameras in the stereo pair
def extract_audio_features(filename1, filename2,fps):
    video1=str(filename1)
    video2=str(filename2)
    # Load audio using librosa
    y1, sr1 = librosa.load(video1, sr=None)
    y2, sr2 = librosa.load(video2, sr=None)
    corr = correlate(y1, y2, mode='full')
    norm_corr=corr / (np.linalg.norm(corr) * np.linalg.norm(corr))
    offset = np.argmax(norm_corr) - (len(y2) - 1)
    # ***Change this if not 24fps:
    offset_frames= round((offset/sr1)*fps)
    #frame offset: if positive, camera 1 started first. If negative, camera 2 started first
    if offset_frames >0:
        frame_delay=[offset_frames,0]
    else:
        frame_delay=[0,-offset_frames]
    print(frame_delay)
    return frame_delay


#call ffmpeg
#3 frames/second. Start at frame number based on delay
#when this function is called, video and delay inputs need to be indexed from lists e.g. delay=frame_delay[1]


def extract_frames_new(cal_dir, video, video_name, delay, fps, out_fps, start_time, end_time):
    frames_folder = str(cal_dir)
    print(frames_folder)
    output=str(frames_folder) + '/img_%04d.jpg'
    video_file=str(video)

    print(start_time)

    start_sec = parse_time(start_time)
    end_sec = parse_time(end_time) 

    print(f"start_sec: {start_sec}, end_sec: {end_sec}")


    print(f"parse time complete")

    # Calculate duration
    duration = end_sec - start_sec
    if duration <= 0:
        raise ValueError("end_frame must be greater than start_frame")

    start_sec_adjusted = start_sec + (delay / fps)

    subprocess.run(["ffmpeg", "-ss", str(start_sec_adjusted), "-i", video_file, "-t", str(duration), "-r", out_fps, "-q:v", "5", "-f", "image2", output])

def extract_frames(video, video_name, delay, fps, out_fps):
    frames_folder = str(video_name)+ '_synced'
    print(frames_folder)
    output=str(frames_folder) + '/img_%04d.jpg'
    video_file=str(video)
    subprocess.run(["ffmpeg", "-ss", f"{delay/fps}", "-i", video_file, "-r", out_fps, "-q:v", "5", "-f", "image2", output])

def parse_time(t):
        print(f"Input type: {type(t)}")
        if isinstance(t, str):
            print(f"its a string")
            try:
                dt = datetime.strptime(t.strip(), "%H:%M:%S")
                return dt.hour * 3600 + dt.minute * 60 + dt.second
            except ValueError:
                print(f"⚠️ Invalid time format '{t}', assuming 0 sec.")
                return 0
        # Case 2: If t is a datetime.time object, we convert it to total seconds
        elif isinstance(t, datetime):
            print(f"its a datetime")
            return t.hour * 3600 + t.minute * 60 + t.second
            # Case 3: If t is a datetime.time object, extract the time in seconds
        elif isinstance(t, time):
            print(f"it's a datetime.time object")
            return t.hour * 3600 + t.minute * 60 + t.second
        else:
            print(f"⚠️ Invalid input type for time: {t}, assuming 0 sec.")
            return 0
        

def delete_unmatched_files(folder1, folder2):
    # Get the set of filenames from each folder
    files1 = set(os.listdir(folder1))
    files2 = set(os.listdir(folder2))
    
    # Files that are in folder1 but not in folder2
    unmatched_files1 = files1 - files2
    
    # Files that are in folder2 but not in folder1
    unmatched_files2 = files2 - files1
    
    # Full paths for the files to be deleted
    unmatched_files1_paths = [os.path.join(folder1, f) for f in unmatched_files1]
    unmatched_files2_paths = [os.path.join(folder2, f) for f in unmatched_files2]

    # Delete files that are in folder1 but not in folder2
    for file_path in unmatched_files1_paths:
        try:
            os.remove(file_path)
            print(f"Deleted {file_path} from {folder1}")
        except Exception as e:
            print(f"Error deleting {file_path}: {e}")
    
    # Delete files that are in folder2 but not in folder1
    for file_path in unmatched_files2_paths:
        try:
            os.remove(file_path)
            print(f"Deleted {file_path} from {folder2}")
        except Exception as e:
            print(f"Error deleting {file_path}: {e}")
            

def remove_first_n_files(folder_path, n):
    try:
        # Get a list of files in the folder
        files = sorted([f for f in os.listdir(folder_path) if os.path.isfile(os.path.join(folder_path, f))])
        
        # Ensure there are files to remove
        if not files:
            print(f"No files found in {folder_path}.")
            return
        
        # Determine the number of files to remove
        files_to_remove = files[:n]
        
        # Remove the files
        for file in files_to_remove:
            file_path = os.path.join(folder_path, file)
            os.remove(file_path)
            #print(f"Removed: {file_path}")
            
        #print(f"Removed {len(files_to_remove)} files from {folder_path}.")
        
    except Exception as e:
        print(f"An error occurred: {e}")

######Calibration functions 

#DS_Store skip is a Mac-specific problem. Otherwise, it is irrelevant
#@profile #gets a line-by-line profiling of memory usage to identify intensive processes

def calibrate_camera(base_path, images_folder):
    print("Image folder:", images_folder)
    print("Files:", sorted(os.listdir(images_folder)))

    valid_exts = ('.jpg', '.jpeg', '.png', '.bmp', '.tiff')

    images_path=os.path.join(base_path, images_folder)
    images_names = sorted(os.listdir(images_path))

    images = []
    for imname in sorted(os.listdir(images_path)):
        # skip hidden or non-image files like .DS_Store or desktop.ini
        if not imname.lower().endswith(valid_exts):
            print(f"⚠️ Skipping non-image file: {imname}")
            continue

        full_path = os.path.join(images_path, imname)
        im = cv.imread(full_path)
        if im is None:
            print(f"⚠️ Could not read {full_path}")
            continue

        images.append(im)

        if not images:
            raise RuntimeError(f"No valid images found in {images_path}")
    
    # images_path=os.path.join(base_path, images_folder)
    # images_names = os.listdir(images_path)
    # images = []
    # for imname in images_names:
    #     if imname== '.DS_Store':
    #         continue
    #     full_path = os.path.join(images_path, imname)
    #     im = cv.imread(str(full_path), 1)
    #     images.append(im)

    #many of the following annotations are from TemugeB's blog post: 
    #https://temugeb.github.io/opencv/python/2021/02/02/stereo-camera-calibration-and-triangulation.html
    #I will precede all of his notes with TB for TemugeB. I cannot necessarily explain some of his notes
    #since I did not write them (e.g. I don't know in what direction to adjust the checkerboard criteria
    #if the computer can't find it, although it has had no problem doing so thus far)
        
    #print(f"getting criteria")
    #TB: criteria used by checkerboard pattern detector.
    #TB: Change this if the code can't find the checkerboard.
    criteria = (cv.TERM_CRITERIA_EPS + cv.TERM_CRITERIA_MAX_ITER, 30, 0.001)

    
 
    rows = 8 #TB: number of checkerboard rows. change back to 8x11
    columns = 11 #TB: number of checkerboard columns.
    world_scaling = 0.03 #30mm
 
    #TB: coordinates of squares in the checkerboard world space
    objp = np.zeros((rows*columns,3), np.float32) #make matrix of 0's for each square
    objp[:,:2] = np.mgrid[0:columns,0:rows].T.reshape(-1,2)
    objp = world_scaling* objp #Calculate x,y coordinate of each square in meters, leaving column with 0's 
 
    #TB: frame dimensions. Frames should be the same size.
    width = images[0].shape[1]
    height = images[0].shape[0]
 
    #TB: Pixel coordinates of checkerboards
    imgpoints = [] #TB: 2d points in image plane.
 
    #TB: coordinates of the checkerboard in checkerboard world space.
    objpoints = [] #TB: 3d point in real world space
 
    imgno = 0

    #print(f"frame by frame")
    for frame in images:
        gray = cv.cvtColor(frame, cv.COLOR_BGR2GRAY)
 
        #TB: find the checkerboard
        ret, corners = cv.findChessboardCornersSB(gray, (columns, rows), None)
        imgno = imgno + 1
        #print(f"finding checker")
 
        if ret == True:
 
            #TB: Convolution size used to improve corner detection. Don't make this too large.
            conv_size = (11, 11)
 
            #TB: opencv can attempt to improve the checkerboard coordinates
            corners = cv.cornerSubPix(gray, corners, conv_size, (-1, -1), criteria)
            cv.drawChessboardCorners(frame, (columns, rows), corners, ret)
            print(f"Number of corners; {len(corners)}")
            #cv.imshow('img', frame)
            #cv.waitKey(500)

            filename = str(imgno) + ".jpg"
            foldername = "checker_cam"
            filepath = os.path.join(images_path, foldername)
            completeName = os.path.join(images_path, foldername, filename)

            if not os.path.exists(filepath):
               os.makedirs(filepath)

            cv.imwrite(completeName,frame)
 
            objpoints.append(objp)
            imgpoints.append(corners)
 
 
 
    ret, mtx, dist, rvecs, tvecs = cv.calibrateCamera(objpoints, imgpoints, (width, height), None, None)
    print('rmse:', ret)
    print('camera matrix:\n', mtx)
    print('distortion coeffs:', dist)
    print('Rs:\n', rvecs)
    print('Ts:\n', tvecs)
 
    return mtx, dist 


#@profile #gets a line-by-line profiling of memory usage to identify intensive processes
def stereo_calibrate(base_path, mtx1, dist1, mtx2, dist2, frames_folder1, frames_folder2):
    #first do single camera calibration
    #TB: read the synched frames

    print("Image folder:", frames_folder1)
    print("Image folder:", frames_folder2)
    print("Files:", os.listdir(frames_folder1))

    images_path1 = os.path.join(base_path, frames_folder1)
    images_path2 = os.path.join(base_path, frames_folder2)

    # sort file names
    c1_images_names = sorted(os.listdir(images_path1))
    c2_images_names = sorted(os.listdir(images_path2))

    # valid image extensions
    valid_exts = ('.jpg', '.jpeg', '.png', '.bmp', '.tiff')

    c1_images = []
    c2_images = []

    for im in c1_images_names:
        # skip system files and non-image files
        if im.lower() in ('.ds_store', 'desktop.ini') or not im.lower().endswith(valid_exts):
            print(f"⚠️ Skipping non-image/system file in cam1: {im}")
            continue

        full_path1 = os.path.join(images_path1, im)
        im1 = cv.imread(full_path1, 1)
        if im1 is None:
            print(f"⚠️ Could not read image in cam1: {full_path1}")
            continue

        c1_images.append(im1)

    for im in c2_images_names:
        # skip system files and non-image files
        if im.lower() in ('.ds_store', 'desktop.ini') or not im.lower().endswith(valid_exts):
            print(f"⚠️ Skipping non-image/system file in cam2: {im}")
            continue

        full_path2 = os.path.join(images_path2, im)
        im2 = cv.imread(full_path2, 1)
        if im2 is None:
            print(f"⚠️ Could not read image in cam2: {full_path2}")
            continue

        c2_images.append(im2)


    #TB: change this if stereo calibration not good.
    criteria = (cv.TERM_CRITERIA_EPS + cv.TERM_CRITERIA_MAX_ITER, 30, 0.001)
 
    rows = 8 #TB: number of checkerboard rows.
    columns = 11 #TB: number of checkerboard columns.
    world_scaling = 0.03 #30 mm
 
    #TB: coordinates of squares in the checkerboard world space
    objp = np.zeros((rows*columns,3), np.float32)
    objp[:,:2] = np.mgrid[0:rows,0:columns].T.reshape(-1,2)
    objp = world_scaling* objp
 
    #TB: frame dimensions. Frames should be the same size.
    width = c1_images[0].shape[1]
    height = c1_images[0].shape[0]
 
    #TB: Pixel coordinates of checkerboards
    imgpoints_left = [] # 2d points in image plane.
    imgpoints_right = []
 
    #TB: coordinates of the checkerboard in checkerboard world space.
    objpoints = [] # 3d point in real world space
    count=0
    #stereo calibration
    for frame1, frame2 in zip(c1_images, c2_images):
        gray1 = cv.cvtColor(frame1, cv.COLOR_BGR2GRAY)
        gray2 = cv.cvtColor(frame2, cv.COLOR_BGR2GRAY)
        c_ret1, corners1 = cv.findChessboardCornersSB(gray1, (8, 11), None)
        c_ret2, corners2 = cv.findChessboardCornersSB(gray2, (8, 11), None)
        #find paired frames that both have the checkerboard
        if c_ret1 == True and c_ret2 == True:
            count+=1
            corners1 = cv.cornerSubPix(gray1, corners1, (11, 11), (-1, -1), criteria)
            corners2 = cv.cornerSubPix(gray2, corners2, (11, 11), (-1, -1), criteria)
            
            cv.drawChessboardCorners(frame1, (8,11), corners1, c_ret1)
            #cv.imshow('img', frame1)
 
            cv.drawChessboardCorners(frame2, (8,11), corners2, c_ret2)
            #cv.imshow('img2', frame2)

            filename1 = str(count) + ".jpg"
            foldername1 = "checker_stereocam"
            folderpath1 = os.path.join(images_path1, foldername1)
            completeName1 = os.path.join(images_path1, foldername1, filename1)

            if not os.path.exists(folderpath1):
               os.makedirs(folderpath1)

            cv.imwrite(completeName1,frame1)

            filename2 = str(count) + ".jpg"
            foldername2 = "checker_stereocam"
            folderpath2 = os.path.join(images_path2, foldername2)
            completeName2 = os.path.join(images_path2, foldername2, filename2)

            if not os.path.exists(folderpath2):
               os.makedirs(folderpath2)

            cv.imwrite(completeName2,frame2)
 
            objpoints.append(objp)
            imgpoints_left.append(corners1)
            imgpoints_right.append(corners2)
 
    stereocalibration_flags = cv.CALIB_FIX_INTRINSIC
    


    ret, CM1, dist1, CM2, dist2, R, T, E, F  = cv.stereoCalibrate(objpoints, imgpoints_left, imgpoints_right, mtx1, dist1,
                                                                 mtx2, dist2, (width, height), criteria = criteria, flags = stereocalibration_flags)
 
    print("cameras stereo-calibrated, Ideal RMSE is <0.3. Overall RMSE:", ret, ret/count)

    print("Final paired frames used for calibration:", len(objpoints))

    return ret, R, T


def triangulate(mtx1, mtx2, R, T, fishpoints1, fishpoints2):
 
    uvs1 = fishpoints1
    uvs2 = fishpoints2
 
 
    #TB: RT matrix for C1 is identity.
    RT1 = np.concatenate([np.eye(3), [[0],[0],[0]]], axis = -1)
    P1 = mtx1 @ RT1 #projection matrix for C1
 
    #TB: RT matrix for C2 is the R and T obtained from stereo calibration.
    RT2 = np.concatenate([R, T], axis = -1)
    P2 = mtx2 @ RT2 #TB: projection matrix for C2
 
    def DLT(P1, P2, point1, point2):

        A = [point1[1]*P1[2,:] - P1[1,:],
             P1[0,:] - point1[0]*P1[2,:],
             point2[1]*P2[2,:] - P2[1,:],
             P2[0,:] - point2[0]*P2[2,:]
            ]
        A = np.array(A).reshape((4,4))
 
        B = A.transpose() @ A
        from scipy import linalg
        U, s, Vh = linalg.svd(B, full_matrices = False)
        
        return Vh[3,0:3]/Vh[3,3]
 
    p3ds = []
    for uv1, uv2 in zip(uvs1, uvs2):
        if uv1 is None or uv2 is None:
            p3ds.append(None)
        else:
            _p3d = DLT(P1, P2, uv1, uv2)
            _p3d=np.ndarray.tolist(_p3d)
            p3ds.append(_p3d)

    #these are the 3d points:
    print(p3ds)
    return np.array(p3ds)

