import os, shutil
import ntpath
import sys
import subprocess
from shutil import copyfile
import SimpleITK as sitk
import time
import nibabel as nib
import numpy as np

def ensure_file(file_path):
	exists = os.path.isfile(file_path)
	if exists:
	    return True;
	else:
		return False;

def ensure_dir(file_path):
    #directory = os.path.dirname(file_path)
    directory = file_path
    #print(directory)
    if os.path.exists(directory):
       return True;
       #os.makedirs(directory)
    else:
    	return False;

def convert_dcm_nii(dcm_folder,output_fold,output_name):
	reader = sitk.ImageSeriesReader()
	dicom_names = reader.GetGDCMSeriesFileNames(dcm_folder)
	reader.SetFileNames(dicom_names)
	image = reader.Execute()
	size = image.GetSize()
	print( "NII Image size:", size[0], size[1], size[2])
	##ensure_dir(y+z)
    
	sitk.WriteImage(image, output_fold+output_name+".nii")
	#dcm_txt_file.write(output_name+".nii\n")
	print(output_name+".nii")


def raw_to_nii(filename,output_name):
	output_fold = "D:\\Google Drive\\PREDICTpy\\SPOTLIGHT_ROI\\mhd\\"
	image = sitk.ReadImage(filename)
	sitk.WriteImage(image, output_fold+output_name+".nii")

#Followup
#Baseline
# PREDICT STITCHING
study_n="13-008"

convert_dcm_nii("D:/ICH Files/PREDICT/PREDICT_"+str(study_n)+"/Baseline/series","D:/ICH Files/PREDICT_ADD/","PREDICT_"+str(study_n)+"-Baseline")
convert_dcm_nii("D:/ICH Files/PREDICT/PREDICT_"+str(study_n)+"/Followup/series","D:/ICH Files/PREDICT_ADD/","PREDICT_"+str(study_n)+"-Followup")


study_n="05-075"
time="Baseline"

convert_dcm_nii("D:/ICH Files/PREDICT/PREDICT_"+str(study_n)+"/"+str(time)+"/series","D:/ICH Files/PREDICT_JOIN/","img1")
convert_dcm_nii("D:/ICH Files/PREDICT/PREDICT_"+str(study_n)+"/"+str(time)+"/series1","D:/ICH Files/PREDICT_JOIN/","img2")


sl1_img = nib.load("D:/ICH Files/PREDICT_JOIN/img1.nii")
sl2_img = nib.load("D:/ICH Files/PREDICT_JOIN/img2.nii")

sl1_img_np = sl1_img.get_fdata()
sl2_img_np = sl2_img.get_fdata()

sl1_img_np.shape

sl3 = np.concatenate([sl1_img_np, sl2_img_np], -1)

sl3.shape

ni_img = nib.Nifti1Image(sl3, np.eye(4))

nib.save(ni_img, "D:/ICH Files/PREDICT_JOIN/PREDICT_"+str(study_n)+"-"+str(time)+".nii")



#3 series
convert_dcm_nii("D:/ICH Files/PREDICT/PREDICT_05-062/Followup/series2","D:/ICH Files/PREDICT_JOIN/NII/","img1")
convert_dcm_nii("D:/ICH Files/PREDICT/PREDICT_05-062/Followup/series","D:/ICH Files/PREDICT_JOIN/NII/","img2")
convert_dcm_nii("D:/ICH Files/PREDICT/PREDICT_05-062/Followup/series1","D:/ICH Files/PREDICT_JOIN/NII/","img3")


sl1_img = nib.load("D:/ICH Files/PREDICT_JOIN/NII/img1.nii")
sl2_img = nib.load("D:/ICH Files/PREDICT_JOIN/NII/img2.nii")
sl3_img = nib.load("D:/ICH Files/PREDICT_JOIN/NII/img3.nii")

sl1_img_np = sl1_img.get_fdata()
sl2_img_np = sl2_img.get_fdata()
sl3_img_np = sl3_img.get_fdata()

sl1_img_np.shape

sl4 = np.concatenate([sl1_img_np, sl2_img_np], -1)

sl4.shape

sl5 = np.concatenate([sl4, sl3_img_np], -1)

ni_img = nib.Nifti1Image(sl5, np.eye(4))

nib.save(ni_img, 'D:/ICH Files/PREDICT_JOIN/PREDICT_05-062-Followup.nii')





# SPOTLIGHT STITCHING

study_n="09-01-001-Baseline"
#subfolder="unknown3"
#n="7"

convert_dcm_nii("D:/ICH Files/STOP-IT/STOPIT_DCM/"+str(study_n)+"/", "D:/ICH Files/STOP-IT/STOP-IT_ADD/", str(study_n))



convert_dcm_nii("D:/ICH Files/PREDICT/PREDICT_"+str(study_n)+"/Followup/series","D:/ICH Files/PREDICT_ADD/","PREDICT_"+str(study_n)+"-Followup")