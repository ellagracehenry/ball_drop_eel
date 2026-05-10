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
import pickle

from PyQt6 import QtWidgets, QtGui, QtCore
import sys, os, cv2
import numpy as np

from pyocamcalib.modelling.calibration import CalibrationEngine

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
    df = pd.read_excel(filename)
    search_value = search_value.removesuffix('.MP4')
    mask = (df[search_column] == search_value) & (df[folder_column] == folder)
    matching_rows = df.loc[mask]
    found_rows = matching_rows.to_dict(orient='records')
    for row in found_rows:
        print(row)
    return found_rows

def cam_calibration(base_path, left_cal_vid, right_cal_vid, left_cal, right_cal, start_time, end_time, trial_id):

    out_fps="3"

    cal_video_list = [
    os.path.join(left_cal_vid, left_cal +".MP4"),
    os.path.join(right_cal_vid, right_cal +".MP4")
    ]

    video_name_list = [left_cal, right_cal]

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

    left_cal_mp4=cal_video_list[0]
    right_cal_mp4=cal_video_list[1]
    fps = get_frame_rate(left_cal_mp4)
    fps_r=get_frame_rate(right_cal_mp4)

    print(fps)
    print(fps_r)

    if not fps == fps_r:
        print("Videos recorded with different fps, quitting")
        sys.exit()
    else:
        print("Original video fps is ",fps, ". Extracting at ", out_fps, "fps.")

    frame_delay=extract_audio_features(cal_video_list[0], cal_video_list[1],fps)

    for x in range(len(cal_video_list)):
        if has_images(cal_dir_list[x]):
            print(f"Frames already exist, skipping extraction...")
        else:
            extract_frames_new(cal_dir_list[x], cal_video_list[x], video_name_list[x], frame_delay[x], fps, out_fps, start_time,end_time)
            print("Calibration frames extracted") 

    delete_unmatched_files(cal_dir_L, cal_dir_R)

    start_time = parse_time(start_time)
    end_time = parse_time(end_time) 

    print(f"calibrating cam1")
    #mtx1, dist1 = calibrate_camera(base_path, images_folder=cal_dir_L)

    print(f"calibrating cam2")
    #mtx2, dist2 = calibrate_camera(base_path, images_folder=cal_dir_R)

    print(f"stereocalibrating...")

    # To use fixed intrinsics from a known-good trial, uncomment below and comment out stereo_calibrate:
    mtx1_good = np.array([[1.28756315e+03, 0.00000000e+00, 9.18200332e+02],
                       [0.00000000e+00, 1.28484443e+03, 5.45072820e+02],
                       [0.00000000e+00, 0.00000000e+00, 1.00000000e+00]])
    dist1_good = np.array([[ 0.49839047,  0.17340749,  0.00957047, -0.02368860, -0.09713041]])
    mtx2_good = np.array([[1.35928116e+03, 0.00000000e+00, 9.06979147e+02],
                       [0.00000000e+00, 1.36738729e+03, 6.17501783e+02],
                       [0.00000000e+00, 0.00000000e+00, 1.00000000e+00]])
    dist2_good = np.array([[ 0.38976805,  0.54305831,  0.05857733, -0.01435611, -0.01380362]])
    rmse, R, T, mtx1, dist1, mtx2, dist2 = stereo_calibrate_fixed_intrinsics(base_path, mtx1_good, dist1_good, mtx2_good, dist2_good, cal_dir_L, cal_dir_R)

    #rmse, R, T, mtx1, mtx2, dist1, dist2 = stereo_calibrate(base_path, mtx1, dist1, mtx2, dist2, cal_dir_L, cal_dir_R)

    return rmse, mtx1, dist1, mtx2, dist2, R, T


def get_frame_rate(video_path):
    result = subprocess.run(
        ['ffprobe', '-v', 'error', '-select_streams', 'v:0', '-show_entries', 'stream=r_frame_rate', '-of', 'default=nk=1:nw=1', video_path],
        capture_output=True,
        text=True
    )
    frame_rate_str = result.stdout.strip()
    numerator, denominator = map(int, frame_rate_str.split('/'))
    frame_rate = numerator / denominator
    return frame_rate


def extract_audio_features(filename1, filename2,fps):
    video1=str(filename1)
    video2=str(filename2)
    y1, sr1 = librosa.load(video1, sr=None)
    y2, sr2 = librosa.load(video2, sr=None)
    corr = correlate(y1, y2, mode='full')
    norm_corr=corr / (np.linalg.norm(y1) * np.linalg.norm(y2))
    offset = np.argmax(norm_corr) - (len(y2) - 1)
    offset_frames= round((offset/sr1)*fps)
    if offset_frames >0:
        frame_delay=[offset_frames,0]
    else:
        frame_delay=[0,-offset_frames]
    print(frame_delay)
    return frame_delay


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
        elif isinstance(t, datetime):
            print(f"its a datetime")
            return t.hour * 3600 + t.minute * 60 + t.second
        elif isinstance(t, time):
            print(f"it's a datetime.time object")
            return t.hour * 3600 + t.minute * 60 + t.second
        else:
            print(f"⚠️ Invalid input type for time: {t}, assuming 0 sec.")
            return 0
        

def delete_unmatched_files(folder1, folder2):
    files1 = set(os.listdir(folder1))
    files2 = set(os.listdir(folder2))
    unmatched_files1 = files1 - files2
    unmatched_files2 = files2 - files1
    unmatched_files1_paths = [os.path.join(folder1, f) for f in unmatched_files1]
    unmatched_files2_paths = [os.path.join(folder2, f) for f in unmatched_files2]

    for file_path in unmatched_files1_paths:
        try:
            os.remove(file_path)
            print(f"Deleted {file_path} from {folder1}")
        except Exception as e:
            print(f"Error deleting {file_path}: {e}")
    
    for file_path in unmatched_files2_paths:
        try:
            os.remove(file_path)
            print(f"Deleted {file_path} from {folder2}")
        except Exception as e:
            print(f"Error deleting {file_path}: {e}")
            

def remove_first_n_files(folder_path, n):
    try:
        files = sorted([f for f in os.listdir(folder_path) if os.path.isfile(os.path.join(folder_path, f))])
        if not files:
            print(f"No files found in {folder_path}.")
            return
        files_to_remove = files[:n]
        for file in files_to_remove:
            file_path = os.path.join(folder_path, file)
            os.remove(file_path)
    except Exception as e:
        print(f"An error occurred: {e}")

def has_images(folder, valid_exts=('.jpg', '.jpeg', '.png', '.bmp', '.tiff')):
    if not os.path.exists(folder):
        return False
    return any(f.lower().endswith(valid_exts) for f in os.listdir(folder))


######Calibration functions

def per_frame_reprojection_errors(objpoints, imgpoints, mtx, dist):
    """
    Compute per-frame mean reprojection error using solvePnP.
    Returns array of per-frame errors in pixels.
    """
    errors = []
    for obj, img in zip(objpoints, imgpoints):
        ok, rvec, tvec = cv.solvePnP(obj, img, mtx, dist)
        if not ok:
            errors.append(np.inf)
            continue
        proj, _ = cv.projectPoints(obj, rvec, tvec, mtx, dist)
        err = np.sqrt(((proj.reshape(-1, 2) - img.reshape(-1, 2)) ** 2).sum(axis=1)).mean()
        errors.append(err)
    return np.array(errors)


def filter_stereo_pairs(objpoints, imgpoints_left, imgpoints_right, mtx1, dist1, mtx2, dist2, threshold=1.5):
    """
    Filter stereo pairs by per-frame reprojection error in both cameras.
    Prints a per-pair diagnostic so you can see which frames are outliers.
    Returns filtered (objpoints, imgpoints_left, imgpoints_right).
    """
    errors_l = per_frame_reprojection_errors(objpoints, imgpoints_left, mtx1, dist1)
    errors_r = per_frame_reprojection_errors(objpoints, imgpoints_right, mtx2, dist2)

    print(f"\n--- Per-frame reprojection errors (threshold={threshold}px) ---")
    print(f"{'Pair':>4}  {'cam1':>8}  {'cam2':>8}  {'keep':>6}")
    for i, (el, er) in enumerate(zip(errors_l, errors_r)):
        keep = (el < threshold) and (er < threshold)
        flag = "✓" if keep else "✗ OUTLIER"
        print(f"{i:>4}  {el:>8.2f}  {er:>8.2f}  {flag}")

    mask = (errors_l < threshold) & (errors_r < threshold)
    n_kept = int(mask.sum())
    n_total = len(mask)
    print(f"\nKept {n_kept} / {n_total} pairs after filtering at {threshold}px threshold\n")

    if n_kept < 6:
        print(f"⚠️  Only {n_kept} pairs survive at {threshold}px — consider raising the threshold.")

    obj_f   = [o for o, m in zip(objpoints, mask) if m]
    left_f  = [p for p, m in zip(imgpoints_left, mask) if m]
    right_f = [p for p, m in zip(imgpoints_right, mask) if m]

    return obj_f, left_f, right_f

def filter_stereo_pairs_by_size(objpoints, imgpoints_left, imgpoints_right,
                                 mtx1, dist1, mtx2, dist2,
                                 reproj_threshold=1.5, size_ratio_max=1):
    errors_l = per_frame_reprojection_errors(objpoints, imgpoints_left, mtx1, dist1)
    errors_r = per_frame_reprojection_errors(objpoints, imgpoints_right, mtx2, dist2)

    print(f"\n--- Per-frame filtering (reproj<{reproj_threshold}px, size_ratio<{size_ratio_max}) ---")
    print(f"{'Pair':>4}  {'cam1':>8}  {'cam2':>8}  {'ratio':>6}  {'keep'}")

    keep = []
    for i, (el, er, cl, cr) in enumerate(zip(errors_l, errors_r, imgpoints_left, imgpoints_right)):
        area_l = cl[:,0,0].ptp() * cl[:,0,1].ptp()
        area_r = cr[:,0,0].ptp() * cr[:,0,1].ptp()
        ratio = max(area_l, area_r) / (min(area_l, area_r) + 1e-6)
        ok = (el < reproj_threshold) and (er < reproj_threshold) and (ratio < size_ratio_max)
        print(f"{i:>4}  {el:>8.2f}  {er:>8.2f}  {ratio:>6.2f}  {'✓' if ok else '✗'}")
        keep.append(ok)

    mask = np.array(keep)
    print(f"\nKept {mask.sum()} / {len(mask)} pairs\n")

    return ([o for o,m in zip(objpoints,mask) if m],
            [p for p,m in zip(imgpoints_left,mask) if m],
            [p for p,m in zip(imgpoints_right,mask) if m])

def calibrate_camera(base_path, images_folder):
    print("Image folder:", images_folder)
    print("Files:", sorted(os.listdir(images_folder)))

    valid_exts = ('.jpg', '.jpeg', '.png', '.bmp', '.tiff')

    images_path=os.path.join(base_path, images_folder)

    print(images_path)

    images = []
    images_names = []
    for imname in sorted(os.listdir(images_path)):
        if not imname.lower().endswith(valid_exts):
            print(f"⚠️ Skipping non-image file: {imname}")
            continue

        full_path = os.path.join(images_path, imname)
        im = cv.imread(full_path)
        if im is None:
            print(f"⚠️ Could not read {full_path}")
            continue

        images.append(im)
        images_names.append(imname)

        if not images:
            raise RuntimeError(f"No valid images found in {images_path}")
        
    criteria = (cv.TERM_CRITERIA_EPS + cv.TERM_CRITERIA_MAX_ITER, 30, 0.001)

    rows = 8
    columns = 11
    world_scaling = 0.03
 
    objp = np.zeros((rows*columns,3), np.float32)
    objp[:,:2] = np.mgrid[0:columns,0:rows].T.reshape(-1,2)
    objp = world_scaling* objp
 
    width = images[0].shape[1]
    height = images[0].shape[0]
 
    imgpoints = []
    objpoints = []

    for i, frame in enumerate(images):
        gray = cv.cvtColor(frame, cv.COLOR_BGR2GRAY)
        ret, corners = cv.findChessboardCornersSB(gray, (columns, rows),cv.CALIB_CB_ADAPTIVE_THRESH | cv.CALIB_CB_NORMALIZE_IMAGE | cv.CALIB_CB_EXHAUSTIVE)
        
        if ret == True:
            conv_size = (5, 5)
            corners = cv.cornerSubPix(gray, corners, conv_size, (-1, -1), criteria)

            board_width_px  = corners[:, 0, 0].ptp()
            board_height_px = corners[:, 0, 1].ptp()

            cv.drawChessboardCorners(frame, (columns,rows), corners, ret)
            print(f"Number of corners; {len(corners)}")

            filename = str(i) + ".jpg"
            foldername = "checker_cam"
            filepath = os.path.join(images_path, foldername)
            completeName = os.path.join(images_path, foldername, filename)

            if not os.path.exists(filepath):
               os.makedirs(filepath)

            cv.imwrite(completeName,frame)
 
            objpoints.append(objp.copy())
            imgpoints.append(corners)

    ret, mtx, dist, rvecs, tvecs = cv.calibrateCamera(objpoints, imgpoints, (width, height), None, None)
    print('rmse:', ret)
    print('camera matrix:\n', mtx)
    print('distortion coeffs:', dist)
    print('Rs:\n', rvecs)
    print('Ts:\n', tvecs)
 
    return mtx, dist 


def calibrate_camera_interactive(base_path, images_folder):
    print("Image folder:", images_folder)
    images_path = os.path.join(base_path, images_folder)

    valid_exts = ('.jpg', '.jpeg', '.png', '.bmp', '.tiff')

    images = []
    images_names = []
    for imname in sorted(os.listdir(images_path)):
        if not imname.lower().endswith(valid_exts):
            continue
        full_path = os.path.join(images_path, imname)
        im = cv.imread(full_path)
        if im is None:
            continue
        images.append(im)
        images_names.append(imname)

    if not images:
        raise RuntimeError(f"No valid images found in {images_path}")

    rows = 8
    columns = 11
    world_scaling = 0.03
    objp = np.zeros((rows*columns, 3), np.float32)
    objp[:, :2] = np.mgrid[0:columns, 0:rows].T.reshape(-1, 2)
    objp *= world_scaling

    width, height = images[0].shape[1], images[0].shape[0]

    criteria = (cv.TERM_CRITERIA_EPS + cv.TERM_CRITERIA_MAX_ITER, 30, 0.001)
    conv_size = (1, 1)

    objpoints = []
    imgpoints = []
    all_corners = {}

    def adjust_corners(event, x, y, flags, param):
        data = param
        if 'corners' not in data or 'display_params' not in data:
            return

        corners = data['corners']
        dp = data['display_params']

        ox = dp['pan_x'] + x * dp['view_w'] / dp['disp_w']
        oy = dp['pan_y'] + y * dp['view_h'] / dp['disp_h']

        if event == cv.EVENT_LBUTTONDOWN:
            dists = np.linalg.norm(corners.reshape(-1, 2) - np.array([ox, oy]), axis=1)
            idx = np.argmin(dists)
            data['drag_idx'] = idx
        elif event == cv.EVENT_MOUSEMOVE and 'drag_idx' in data:
            idx = data['drag_idx']
            corners[idx, 0, 0] = ox
            corners[idx, 0, 1] = oy
        elif event == cv.EVENT_LBUTTONUP and 'drag_idx' in data:
            data.pop('drag_idx')

    for i, frame in enumerate(images):
        gray = cv.cvtColor(frame, cv.COLOR_BGR2GRAY)
        ret, corners = cv.findChessboardCornersSB(gray, (columns, rows),cv.CALIB_CB_ADAPTIVE_THRESH | cv.CALIB_CB_NORMALIZE_IMAGE | cv.CALIB_CB_EXHAUSTIVE)
        if not ret:
            print(f"Checkerboard not found in {images_names[i]}, skipping.")
            continue

        corners = cv.cornerSubPix(gray, corners, conv_size, (-1, -1), criteria)

        data = {'corners': corners, 'display_params': {}}
        cv.namedWindow("Adjust corners")
        cv.setMouseCallback("Adjust corners", adjust_corners, data)

        zoom = 1.0
        pan_x = 0
        pan_y = 0
        step = 20
        zoom_step = 1.2

        while True:
            view_w = int(width / zoom)
            view_h = int(height / zoom)
            x1 = int(np.clip(pan_x, 0, width - view_w))
            y1 = int(np.clip(pan_y, 0, height - view_h))
            x2 = x1 + view_w
            y2 = y1 + view_h

            display = frame[y1:y2, x1:x2]
            display = cv.resize(display, (width, height))

            data['display_params'] = {
                'zoom': zoom, 'pan_x': x1, 'pan_y': y1,
                'view_w': view_w, 'view_h': view_h,
                'disp_w': width, 'disp_h': height
            }

            for idx, c in enumerate(corners):
                cx = int((c[0,0] - x1) * width / (x2 - x1))
                cy = int((c[0,1] - y1) * height / (y2 - y1))
                cv.line(display, (cx-5, cy), (cx+5, cy), (0,0,255), 1)
                cv.line(display, (cx, cy-5), (cx, cy+5), (0,0,255), 1)
                cv.putText(display, str(idx), (cx+5, cy-5), cv.FONT_HERSHEY_SIMPLEX, 0.5, (0,255,0), 1)

            cv.imshow("Adjust corners", display)
            key = cv.waitKey(30) & 0xFF

            if key == 13:
                break
            elif key in (ord('+'), ord('=')):
                zoom *= zoom_step
            elif key in (ord('-'), ord('_')):
                zoom /= zoom_step
            elif key == ord('w'):
                pan_y -= step
            elif key == ord('s'):
                pan_y += step
            elif key == ord('a'):
                pan_x -= step
            elif key == ord('d'):
                pan_x += step

            pan_x = np.clip(pan_x, 0, width - int(width / zoom))
            pan_y = np.clip(pan_y, 0, height - int(height / zoom))

        cv.destroyWindow("Adjust corners")

        foldername = "checker_cam"
        filepath = os.path.join(images_path, foldername)
        os.makedirs(filepath, exist_ok=True)
        completeName = os.path.join(filepath, f"{i}.jpg")
        cv.drawChessboardCorners(frame, (columns, rows), corners, True)
        cv.imwrite(completeName, frame)

        objpoints.append(objp)
        imgpoints.append(corners)
        all_corners[images_names[i]] = corners.reshape(-1, 2)

    corners_file = os.path.join(images_path, "annotated_corners.npz")
    np.savez(corners_file, objpoints=objpoints, imgpoints=imgpoints, all_corners=all_corners)
    print(f"Saved annotated corners to {corners_file}")

    ret, mtx, dist, rvecs, tvecs = cv.calibrateCamera(objpoints, imgpoints, (width, height), None, None, flags=cv.CALIB_RATIONAL_MODEL)
    print('rmse:', ret)
    print('camera matrix:\n', mtx)
    print('distortion coeffs:', dist)

    return objpoints, imgpoints, mtx, dist, all_corners


def stereo_calibrate(base_path, mtx1, dist1, mtx2, dist2, frames_folder1, frames_folder2,
                     filter_threshold=1.5):
    """
    Stereo calibration with per-frame outlier filtering before the solve.
    filter_threshold: max reprojection error in pixels to keep a pair.
                      Set to None to disable filtering entirely.
    """
    print("Image folder:", frames_folder1)
    print("Image folder:", frames_folder2)
    print("Files:", os.listdir(frames_folder1))

    images_path1 = os.path.join(base_path, frames_folder1)
    images_path2 = os.path.join(base_path, frames_folder2)

    c1_images_names = sorted(os.listdir(images_path1))
    c2_images_names = sorted(os.listdir(images_path2))

    print(c1_images_names[:5])
    print(c2_images_names[:5])

    valid_exts = ('.jpg', '.jpeg', '.png', '.bmp', '.tiff')

    c1_images = []
    c2_images = []

    for im in c1_images_names:
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
        if im.lower() in ('.ds_store', 'desktop.ini') or not im.lower().endswith(valid_exts):
            print(f"⚠️ Skipping non-image/system file in cam2: {im}")
            continue
        full_path2 = os.path.join(images_path2, im)
        im2 = cv.imread(full_path2, 1)
        if im2 is None:
            print(f"⚠️ Could not read image in cam2: {full_path2}")
            continue
        c2_images.append(im2)

    criteria = (cv.TERM_CRITERIA_EPS + cv.TERM_CRITERIA_MAX_ITER, 30, 0.001)
 
    rows = 8
    columns = 11
    world_scaling = 0.03
 
    objp = np.zeros((rows*columns,3), np.float32)
    objp[:,:2] = np.mgrid[0:columns,0:rows].T.reshape(-1,2)
    objp = world_scaling* objp
 
    width = c1_images[0].shape[1]
    height = c1_images[0].shape[0]
 
    imgpoints_left = []
    imgpoints_right = []
    objpoints = []
    count = 0

    for frame1, frame2 in zip(c1_images, c2_images):
        gray1 = cv.cvtColor(frame1, cv.COLOR_BGR2GRAY)
        gray2 = cv.cvtColor(frame2, cv.COLOR_BGR2GRAY)
        c_ret1, corners1 = cv.findChessboardCornersSB(gray1, (columns, rows), cv.CALIB_CB_ADAPTIVE_THRESH | cv.CALIB_CB_NORMALIZE_IMAGE | cv.CALIB_CB_EXHAUSTIVE)
        c_ret2, corners2 = cv.findChessboardCornersSB(gray2, (columns, rows), cv.CALIB_CB_ADAPTIVE_THRESH | cv.CALIB_CB_NORMALIZE_IMAGE | cv.CALIB_CB_EXHAUSTIVE)

        if c_ret1 == True and c_ret2 == True:
            count += 1
            corners1 = cv.cornerSubPix(gray1, corners1, (5,5), (-1, -1), criteria)
            corners2 = cv.cornerSubPix(gray2, corners2, (5,5), (-1, -1), criteria)

            bw1 = corners1[:,0,0].ptp()
            bh1 = corners1[:,0,1].ptp()
            bw2 = corners2[:,0,0].ptp()
            bh2 = corners2[:,0,1].ptp()
            print(f"Pair {count}: cam1 board {bw1:.0f}x{bh1:.0f}px, cam2 board {bw2:.0f}x{bh2:.0f}px")
            
            cv.drawChessboardCorners(frame1, (columns,rows), corners1, c_ret1)
            cv.drawChessboardCorners(frame2, (columns,rows), corners2, c_ret2)

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
 
            objpoints.append(objp.copy())
            imgpoints_left.append(corners1)
            imgpoints_right.append(corners2)

    print(f"\nTotal stereo pairs detected: {count}")

    # # ↓ ADD THIS BLOCK HERE
    # print("\n--- Ratio threshold sweep ---")
    # for ratio_max in [1.6, 1.4, 1.2, 1.1]:
    #     obj_f, left_f, right_f = filter_stereo_pairs_by_size(
    #         objpoints, imgpoints_left, imgpoints_right,
    #         mtx1, dist1, mtx2, dist2,
    #         reproj_threshold=1.5, size_ratio_max=ratio_max
    #     )
    #     if len(obj_f) < 6:
    #         print(f"ratio<{ratio_max}: only {len(obj_f)} pairs, skipping")
    #         continue
    #     ret_test, *_ = cv.stereoCalibrate(
    #         obj_f, left_f, right_f,
    #         mtx1, dist1, mtx2, dist2,
    #         (width, height), criteria=criteria, flags=cv.CALIB_FIX_INTRINSIC
    #     )
    #     print(f"ratio<{ratio_max}: {len(obj_f)} pairs, RMSE={ret_test:.4f}")
    # # ↑ END OF BLOCK

    # --- Per-frame outlier filtering ---
    # if filter_threshold is not None and len(objpoints) > 0:
    #     objpoints, imgpoints_left, imgpoints_right = filter_stereo_pairs_by_size(
    #         objpoints, imgpoints_left, imgpoints_right,
    #         mtx1, dist1, mtx2, dist2,
    #         reproj_threshold=1.3, size_ratio_max= 1.5
    #     )

    if len(objpoints) == 0:
        raise RuntimeError("No stereo pairs remain after filtering — try raising filter_threshold.")

    #stereocalibration_flags = cv.CALIB_FIX_INTRINSIC
    stereocalibration_flags = 0

    ret, CM1, dist1, CM2, dist2, R, T, E, F = cv.stereoCalibrate(
        objpoints, imgpoints_left, imgpoints_right,
        mtx1, dist1, mtx2, dist2,
        (width, height),
        criteria=criteria,
        flags=stereocalibration_flags
    )
 
    print(f"Stereo RMSE: {ret:.4f}  (per-pair: {ret/len(objpoints):.4f})")
    print(f"Camera distance: {np.linalg.norm(T):.3f}m")
    print(f"Final pairs used: {len(objpoints)}")

    return ret, R, T, CM1, CM2, dist1, dist2


def stereo_calibrate_fixed_intrinsics(base_path, mtx1, dist1, mtx2, dist2, frames_folder1, frames_folder2,
                                       filter_threshold=1.5):
    """
    Use pre-supplied intrinsics (from a good trial) and solve only R and T
    from the current trial's stereo frames.

    filter_threshold: max per-frame reprojection error in pixels to keep a pair.
                      Uses the supplied intrinsics to score each frame before calibrating.
                      Set to None to disable filtering.
    """
    images_path1 = os.path.join(base_path, frames_folder1)
    images_path2 = os.path.join(base_path, frames_folder2)

    valid_exts = ('.jpg', '.jpeg', '.png', '.bmp', '.tiff')

    c1_images = []
    c2_images = []

    for im in sorted(os.listdir(images_path1)):
        if not im.lower().endswith(valid_exts):
            continue
        img = cv.imread(os.path.join(images_path1, im), 1)
        if img is not None:
            c1_images.append(img)

    for im in sorted(os.listdir(images_path2)):
        if not im.lower().endswith(valid_exts):
            continue
        img = cv.imread(os.path.join(images_path2, im), 1)
        if img is not None:
            c2_images.append(img)

    assert len(c1_images) > 0, "No images loaded for cam1"
    assert len(c2_images) > 0, "No images loaded for cam2"
    assert len(c1_images) == len(c2_images), \
        f"Frame count mismatch: cam1={len(c1_images)}, cam2={len(c2_images)}"

    criteria = (cv.TERM_CRITERIA_EPS + cv.TERM_CRITERIA_MAX_ITER, 30, 0.001)
    rows, columns, world_scaling = 8, 11, 0.03

    objp = np.zeros((rows*columns, 3), np.float32)
    objp[:,:2] = np.mgrid[0:columns, 0:rows].T.reshape(-1, 2)
    objp *= world_scaling

    width = c1_images[0].shape[1]
    height = c1_images[0].shape[0]

    imgpoints_left = []
    imgpoints_right = []
    objpoints = []
    count = 0

    for frame1, frame2 in zip(c1_images, c2_images):
        gray1 = cv.cvtColor(frame1, cv.COLOR_BGR2GRAY)
        gray2 = cv.cvtColor(frame2, cv.COLOR_BGR2GRAY)
        c_ret1, corners1 = cv.findChessboardCornersSB(gray1, (columns, rows),
            cv.CALIB_CB_ADAPTIVE_THRESH | cv.CALIB_CB_NORMALIZE_IMAGE | cv.CALIB_CB_EXHAUSTIVE)
        c_ret2, corners2 = cv.findChessboardCornersSB(gray2, (columns, rows),
            cv.CALIB_CB_ADAPTIVE_THRESH | cv.CALIB_CB_NORMALIZE_IMAGE | cv.CALIB_CB_EXHAUSTIVE)

        if c_ret1 and c_ret2:
            count += 1
            corners1 = cv.cornerSubPix(gray1, corners1, (5,5), (-1,-1), criteria)
            corners2 = cv.cornerSubPix(gray2, corners2, (5,5), (-1,-1), criteria)

            bw1 = corners1[:,0,0].ptp()
            bh1 = corners1[:,0,1].ptp()
            bw2 = corners2[:,0,0].ptp()
            bh2 = corners2[:,0,1].ptp()
            print(f"Pair {count}: cam1 board {bw1:.0f}x{bh1:.0f}px, cam2 board {bw2:.0f}x{bh2:.0f}px")

            objpoints.append(objp.copy())
            imgpoints_left.append(corners1)
            imgpoints_right.append(corners2)

    print(f"\nTotal stereo pairs detected: {count}")
    assert count > 0, "No valid stereo pairs found — check that the checkerboard is visible in both cameras."

    # # --- Per-frame outlier filtering using the supplied intrinsics ---
    # if filter_threshold is not None:
    #     objpoints, imgpoints_left, imgpoints_right = filter_stereo_pairs_by_size(
    #         objpoints, imgpoints_left, imgpoints_right,
    #         mtx1, dist1, mtx2, dist2,
    #         reproj_threshold=1.5, size_ratio_max=1
    #     )

    # if len(objpoints) == 0:
    #     raise RuntimeError("No stereo pairs remain after filtering — try raising filter_threshold.")

    ret, CM1, dist1_out, CM2, dist2_out, R, T, E, F = cv.stereoCalibrate(
        objpoints, imgpoints_left, imgpoints_right,
        mtx1, dist1, mtx2, dist2,
        (width, height),
        criteria=criteria,
        flags=cv.CALIB_FIX_INTRINSIC
    )

    print(f"\nStereo RMSE with borrowed intrinsics: {ret:.4f}")
    print(f"Camera distance: {np.linalg.norm(T):.3f}m")
    print(f"Final pairs used: {len(objpoints)}")
    print(f"R:\n{R}")
    print(f"T:\n{T}")

    return ret, R, T, CM1, dist1_out, CM2, dist2_out


def stereo_calibrate_accept_interactive(
    base_path,
    frames_folder1,
    frames_folder2,
    objpoints_cam1,
    imgpoints_cam1,
    objpoints_cam2,
    imgpoints_cam2,
    mtx1, dist1,
    mtx2, dist2
):
    """
    Stereo calibration using manually adjusted checkerboard corners.
    Image size is inferred from the first valid image.
    """

    images_path1 = os.path.join(base_path, frames_folder1)
    images_path2 = os.path.join(base_path, frames_folder2)

    valid_exts = ('.jpg', '.jpeg', '.png', '.bmp', '.tiff')

    img_sample = None
    for fname in sorted(os.listdir(images_path1)):
        if fname.lower().endswith(valid_exts):
            img_sample = cv.imread(os.path.join(images_path1, fname))
            if img_sample is not None:
                break

    if img_sample is None:
        raise RuntimeError("Could not read any images to determine image size")

    height, width = img_sample.shape[:2]
    image_size = (width, height)

    print("Stereo image size:", image_size)

    n_pairs = min(
        len(imgpoints_cam1),
        len(imgpoints_cam2),
        len(objpoints_cam1),
        len(objpoints_cam2)
    )

    if n_pairs == 0:
        raise RuntimeError("No matched interactive corner sets found")

    print(f"Using {n_pairs} matched frame pairs for stereo calibration")

    objpoints = []
    imgpoints_left = []
    imgpoints_right = []

    for i in range(n_pairs):
        objpoints.append(objpoints_cam1[i])
        imgpoints_left.append(imgpoints_cam1[i])
        imgpoints_right.append(imgpoints_cam2[i])

    criteria = (
        cv.TERM_CRITERIA_EPS + cv.TERM_CRITERIA_MAX_ITER,
        30,
        0.001
    )

    flags = cv.CALIB_FIX_INTRINSIC

    ret, CM1, dist1_out, CM2, dist2_out, R, T, E, F = cv.stereoCalibrate(
        objpoints,
        imgpoints_left,
        imgpoints_right,
        mtx1, dist1,
        mtx2, dist2,
        image_size,
        criteria=criteria,
        flags=flags
    )

    print("Stereo calibration complete")
    print("Overall RMSE:", ret)
    print("Baseline (meters):", np.linalg.norm(T))
    print("Rotation matrix:\n", R)
    print("Translation vector:\n", T)

    return ret, R, T


def triangulate(mtx1, mtx2, dist1, dist2, R, T, fishpoints1, fishpoints2):
 
    uvs1 = fishpoints1
    uvs2 = fishpoints2
 
    RT1 = np.concatenate([np.eye(3), [[0],[0],[0]]], axis = -1)
    P1 = mtx1 @ RT1
 
    RT2 = np.concatenate([R, T], axis = -1)
    P2 = mtx2 @ RT2
 
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
 
    def DLT_with_reproj(P1, P2, point1, point2):
        A = [point1[1]*P1[2,:] - P1[1,:],
            P1[0,:] - point1[0]*P1[2,:],
            point2[1]*P2[2,:] - P2[1,:],
            P2[0,:] - point2[0]*P2[2,:]]
        A = np.array(A).reshape((4,4))
        B = A.transpose() @ A
        from scipy import linalg
        U, s, Vh = linalg.svd(B, full_matrices=False)
        p3d = Vh[3,0:3] / Vh[3,3]
    
        p3d_h = np.append(p3d, 1.0)
        proj1 = P1 @ p3d_h
        proj2 = P2 @ p3d_h
        proj1 = proj1[:2] / proj1[2]
        proj2 = proj2[:2] / proj2[2]
    
        err1 = np.linalg.norm(proj1 - np.array(point1))
        err2 = np.linalg.norm(proj2 - np.array(point2))
        rmse = np.sqrt((err1**2 + err2**2) / 2)
    
        return p3d, rmse
    
    p3ds = []
    reproj_errors = []
    for uv1, uv2 in zip(uvs1, uvs2):
        if uv1 is None or uv2 is None:
            p3ds.append(None)
            reproj_errors.append(None)
        else:
            _p3d, _err = DLT_with_reproj(P1, P2, uv1, uv2)
            _p3d=np.ndarray.tolist(_p3d)
            p3ds.append(_p3d)
            reproj_errors.append(_err)

    return np.array(p3ds), reproj_errors

def detect_corners_pyocamcalib(working_dir, chessboard_size=(8, 11), camera_name="MyCamera", square_size=0.03):
    engine = CalibrationEngine(working_dir, chessboard_size, camera_name, square_size)
    engine.detect_corners(check=False)
    now = datetime.now()
    dt_string = now.strftime("%d%m%Y_%H%M%S")
    pickle_path = os.path.join(working_dir, f'detections_{camera_name}_{dt_string}.pickle')
    pickle_path = engine.save_detection()
    return pickle_path

def load_corners_for_calibration(corners_file):
    with open(corners_file, "rb") as f:
        data = pickle.load(f)

    corners_dict = {}

    for key in data:
        filename = os.path.basename(key) 
        image_points = np.asarray(data[key]['image_points'], dtype = np.float32)
        if image_points.ndim == 2:
           image_points = image_points.reshape(-1,1,2)
        corners_dict[filename] = image_points
        
    return corners_dict

def convert_pyocamcalib_corners(corners_list):
    converted = []
    for corners in corners_list:
        corners = np.array(corners, dtype=np.float32)
        corners = corners.reshape(-1, 1, 2)
        converted.append(corners)
    return converted

def stereo_calibrate_pyocamcalib(
    base_path,
    mtx1, dist1,
    mtx2, dist2,
    frames_folder1,
    frames_folder2,
    corners_file_left,
    corners_file_right,
    chessboard_size=(8, 11),
    square_size=0.03
):
    corners_dict_left = load_corners_for_calibration(corners_file_left)
    corners_dict_right = load_corners_for_calibration(corners_file_right)

    rows, columns = chessboard_size

    objp = np.zeros((rows * columns, 3), np.float32)
    objp[:, :2] = np.mgrid[0:columns, 0:rows].T.reshape(-1, 2)
    objp *= square_size

    objpoints = []
    imgpoints_left = []
    imgpoints_right = []

    common_filenames = sorted(
        set(corners_dict_left.keys()) & set(corners_dict_right.keys())
    )

    if len(common_filenames) == 0:
        raise RuntimeError("❌ No matching frames between left and right corners")

    for fname in common_filenames:
        cl = corners_dict_left[fname]
        cr = corners_dict_right[fname]

        cl = cl.reshape(columns, rows, 2)
        cl = cl.transpose(1, 0, 2)
        cl = cl.reshape(-1, 2)

        cr = cr.reshape(columns, rows, 2)
        cr = cr.transpose(1, 0, 2)
        cr = cr.reshape(-1, 2)
        
        if cl.shape != cr.shape:
            print(f"⚠️ Skipping {fname}: corner count mismatch")
            continue

        objpoints.append(objp)
        imgpoints_left.append(cl)
        imgpoints_right.append(cr)

    print(f"✅ Stereo pairs used: {len(objpoints)}")

    img_sample = cv.imread(os.path.join(base_path, frames_folder1, common_filenames[0]))
    height, width = img_sample.shape[:2]

    criteria = (
        cv.TERM_CRITERIA_EPS + cv.TERM_CRITERIA_MAX_ITER,
        30,
        0.001
    )

    flags = cv.CALIB_FIX_INTRINSIC

    ret, CM1, dist1, CM2, dist2, R, T, E, F = cv.stereoCalibrate(
        objpoints,
        imgpoints_left,
        imgpoints_right,
        mtx1,
        dist1,
        mtx2,
        dist2,
        (width, height),
        criteria=criteria,
        flags=flags
    )

    print(f"📐 Stereo RMSE: {ret}")
    print(f"📏 Baseline (m): {np.linalg.norm(T)}")

    return ret, R, T

def calibrate_camera_pyocamcalib(base_path, images_folder, corners_file, chessboard_size=(8,11), square_size=0.03):
    print("Image folder:", images_folder)
    print("Files:", sorted(os.listdir(images_folder)))

    valid_exts = ('.jpg', '.jpeg', '.png', '.bmp', '.tiff')

    images_path=os.path.join(base_path, images_folder)

    images = []
    images_names = []
    for imname in sorted(os.listdir(images_path)):
        if not imname.lower().endswith(valid_exts):
            print(f"⚠️ Skipping non-image file: {imname}")
            continue

        full_path = os.path.join(images_path, imname)
        im = cv.imread(full_path)
        if im is None:
            print(f"⚠️ Could not read {full_path}")
            continue

        images.append(im)
        images_names.append(imname)

    if not images:
        raise RuntimeError(f"No valid images found in {images_path}")
    
    print(corners_file)
        
    height, width = images[0].shape[:2]
        
    corners_dict = load_corners_for_calibration(corners_file)
    print(f"corners loaded in")

    rows, columns = chessboard_size
 
    objp = np.zeros((rows*columns,3), np.float32)
    objp[:,:2] = np.mgrid[0:columns,0:rows].T.reshape(-1,2)
    objp*= square_size
 
    imgpoints = []
    objpoints = []

    criteria = (
        cv.TERM_CRITERIA_EPS + cv.TERM_CRITERIA_MAX_ITER,
        30,
        0.001
    )

    for image_name in images_names:
        if image_name not in corners_dict:
            print(f"⚠️ No corners found for {image_name}")
            continue
    
        corners = corners_dict[image_name]
        corners = corners.reshape(columns, rows, 2)
        corners = corners.transpose(1, 0, 2)
        corners = corners.reshape(-1, 2)
        expected = rows * columns

        if corners.shape[0] != expected:
            print(f"⚠️ {image_name}: expected {expected} corners, got {corners.shape[0]}")
            continue

        print(f"{image_name}: Number of corners; {len(corners)}")

        objpoints.append(objp.copy())
        imgpoints.append(corners)
 
    ret, mtx, dist, rvecs, tvecs = cv.calibrateCamera(objpoints, imgpoints, (width, height), None, None, criteria = criteria)
    print('rmse:', ret)
    print('camera matrix:\n', mtx)
    print('distortion coeffs:', dist)
    print('Rs:\n', rvecs)
    print('Ts:\n', tvecs)
 
    return mtx, dist 

def annotate_checkerboard_zoom(folder_path, save_path="corners.npy"):
    """
    Simple checkerboard annotation tool (PyQt6).

    - Each frame starts EMPTY
    - Left-click: add thin red cross
    - Right-click: delete nearest cross
    - Zoom + pan supported
    - EXACTLY 88 points required to advance
    - Skipped frames are NOT saved
    """

    REQUIRED_POINTS = 88

    class GraphicsView(QtWidgets.QGraphicsView):
        def __init__(self, scene):
            super().__init__(scene)
            self.setDragMode(QtWidgets.QGraphicsView.DragMode.ScrollHandDrag)
            self.zoom_factor = 1.1

        def wheelEvent(self, event):
            factor = self.zoom_factor if event.angleDelta().y() > 0 else 1 / self.zoom_factor
            self.scale(factor, factor)

    class CrossItem(QtWidgets.QGraphicsItem):
        """Thin red cross (not draggable)."""
        def __init__(self, x, y, size=8):
            super().__init__()
            self.setPos(x, y)
            self.size = size

        def boundingRect(self):
            s = self.size
            return QtCore.QRectF(-s/2, -s/2, s, s)

        def paint(self, painter, option, widget):
            painter.setPen(QtGui.QPen(QtCore.Qt.GlobalColor.red, 1))
            s = self.size / 2
            painter.drawLine(QtCore.QLineF(-s, 0, s, 0))
            painter.drawLine(QtCore.QLineF(0, -s, 0, s))

    class Annotator(QtWidgets.QWidget):
        def __init__(self, images):
            super().__init__()
            self.setWindowTitle("Checkerboard Annotator")

            self.images = images
            self.index = 0
            self.corners = {}
            self.cross_items = []

            self.scene = QtWidgets.QGraphicsScene()
            self.view = GraphicsView(self.scene)

            layout = QtWidgets.QVBoxLayout(self)
            layout.addWidget(self.view)

            btns = QtWidgets.QHBoxLayout()
            self.prev_btn = QtWidgets.QPushButton("Previous")
            self.next_btn = QtWidgets.QPushButton("Next")
            self.skip_btn = QtWidgets.QPushButton("Corners not all visible")
            self.save_btn = QtWidgets.QPushButton("Save & Quit")

            for b in (self.prev_btn, self.next_btn, self.skip_btn, self.save_btn):
                btns.addWidget(b)

            layout.addLayout(btns)

            self.prev_btn.clicked.connect(self.prev_image)
            self.next_btn.clicked.connect(self.next_image)
            self.skip_btn.clicked.connect(self.skip_frame)
            self.save_btn.clicked.connect(self.save_and_quit)

            self.view.viewport().installEventFilter(self)
            self.load_image()

        def load_image(self):
            self.scene.clear()
            self.cross_items = []

            img = cv2.imread(self.images[self.index])
            img = cv2.cvtColor(img, cv2.COLOR_BGR2RGB)

            h, w, ch = img.shape
            qimg = QtGui.QImage(img.data, w, h, ch * w,
                                QtGui.QImage.Format.Format_RGB888)
            self.scene.addPixmap(QtGui.QPixmap.fromImage(qimg))

            self.setWindowTitle(
                f"Checkerboard Annotator — {os.path.basename(self.images[self.index])}"
            )

        def eventFilter(self, source, event):
            if event.type() == QtCore.QEvent.Type.MouseButtonPress:
                pos = self.view.mapToScene(event.pos())
                x, y = pos.x(), pos.y()

                if event.button() == QtCore.Qt.MouseButton.LeftButton:
                    c = CrossItem(x, y)
                    self.scene.addItem(c)
                    self.cross_items.append(c)

                elif event.button() == QtCore.Qt.MouseButton.RightButton and self.cross_items:
                    dists = [np.hypot(x - c.pos().x(), y - c.pos().y())
                             for c in self.cross_items]
                    idx = int(np.argmin(dists))
                    if dists[idx] < 20:
                        self.scene.removeItem(self.cross_items[idx])
                        self.cross_items.pop(idx)

                return True

            return False

        def save_current(self):
            if len(self.cross_items) == REQUIRED_POINTS:
                fname = os.path.basename(self.images[self.index])
                pts = [(c.pos().x(), c.pos().y()) for c in self.cross_items]
                self.corners[fname] = np.array(pts, dtype=np.float32)

        def next_image(self):
            if len(self.cross_items) != REQUIRED_POINTS:
                QtWidgets.QMessageBox.warning(
                    self,
                    "Incomplete",
                    f"{len(self.cross_items)} / {REQUIRED_POINTS} points"
                )
                return

            self.save_current()
            if self.index < len(self.images) - 1:
                self.index += 1
                self.load_image()

        def prev_image(self):
            if self.index > 0:
                self.index -= 1
                self.load_image()

        def skip_frame(self):
            if self.index < len(self.images) - 1:
                self.index += 1
                self.load_image()

        def save_and_quit(self):
            with open(save_path, "wb") as f:
                pickle.dump(self.corners, f)
            QtWidgets.QApplication.quit()

    images = sorted(
        os.path.join(folder_path, f)
        for f in os.listdir(folder_path)
        if f.lower().endswith((".png", ".jpg", ".jpeg"))
    )

    if not images:
        print("No images found")
        return

    app = QtWidgets.QApplication(sys.argv)
    w = Annotator(images)
    w.resize(1200, 900)
    w.show()
    app.exec()


import numpy as np
import cv2 as cv
import os

def load_annotated_corners_cam1(npz_path, base_path, cal_dir_L):
    """
    Load annotated corners and re-run calibration for camera 1.
    """
    images_path=os.path.join(base_path, cal_dir_L)

    valid_exts = ('.jpg', '.jpeg', '.png', '.bmp', '.tiff')

    if not os.path.exists(npz_path):
        raise FileNotFoundError(f"NPZ file not found: {npz_path}")

    data = np.load(npz_path, allow_pickle=True)

    objpoints_cam1 = list(data["objpoints"])
    imgpoints_cam1 = list(data["imgpoints"])
    all_corners = data["all_corners"].item()

    if not objpoints_cam1 or not imgpoints_cam1:
        raise RuntimeError("No calibration data found in NPZ file.")
    
    images = []
    images_names = []
    for imname in sorted(os.listdir(images_path)):
        if not imname.lower().endswith(valid_exts):
            print(f"⚠️ Skipping non-image file: {imname}")
            continue

        full_path = os.path.join(images_path, imname)
        im = cv.imread(full_path)
        if im is None:
            print(f"⚠️ Could not read {full_path}")
            continue

        images.append(im)
        images_names.append(imname)

    if not images:
        raise RuntimeError(f"No valid images found in {images_path}")

    height, width = images[0].shape[:2]

    rms, mtx1, dist1, rvecs, tvecs = cv.calibrateCamera(
        objpoints_cam1,
        imgpoints_cam1,
        (width,height),
        None,
        None
    )

    print("Camera 1 re-calibration complete")
    print("RMSE:", rms)
    print("Camera matrix:\n", mtx1)
    print("Distortion coefficients:\n", dist1)

    return objpoints_cam1, imgpoints_cam1, mtx1, dist1