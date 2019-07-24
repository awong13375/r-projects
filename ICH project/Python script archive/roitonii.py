
import os, shutil
import ntpath
import sys
import subprocess
from shutil import copyfile
import SimpleITK as sitk
import time

#loop through roi folders F:\PREDICT\PREDICT_Volume_Measurements

#for each folder, run Thienv3.exe dcm qs_folder output folder


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

def convert_dcm_nii(dcm_folder,output_name):

	output_fold = "F:\\PREDICT\\PREDICT_DCM_NII\\"
	reader = sitk.ImageSeriesReader()
	dicom_names = reader.GetGDCMSeriesFileNames(dcm_folder)
	reader.SetFileNames(dicom_names)

	image = reader.Execute()

	size = image.GetSize()
	#print( "NII Image size:", size[0], size[1], size[2])
	##ensure_dir(y+z)
	sitk.WriteImage(image, output_fold+output_name+".nii")
	dcm_txt_file.write(output_name+".nii\n")
	print(output_name+".nii")

    
def run_prog(dcm_fold, qs_fold, qs_name, pred_name,b_or_f):

	qs_name1 = qs_name + "_TotalMask_Uncropped.nii"
	if 'ich' in qs_fold.lower():
		
		if ensure_dir(dcm_fold):
			#print(dcm_fold)
			#print(qs_fold)	
			#print(qs_name1)
			#print(pred_name)
			temp_fold = "F:\\PREDICT\\temp"
			roi_fold = "F:\\PREDICT\\PREDICT_ROI_NII"
			
			subprocess.check_call(['Thienv3.exe', dcm_fold, qs_fold, temp_fold])
			if ensure_file(temp_fold + "\\" + qs_name1):
				copyfile(temp_fold + "\\" + qs_name1, roi_fold + "\\" + pred_name + "-" + qs_name + ".nii")
			roi_txt_file.write(pred_name + "-" + qs_name + ".nii\n")
			print(pred_name + "-" + qs_name + ".nii")
			for the_file in os.listdir(temp_fold):
				file_path = os.path.join(temp_fold, the_file)
				try:
					if os.path.isfile(file_path):
						os.unlink(file_path)
				except Exception as e:
					print(e)

			convert_dcm_nii(dcm_fold,pred_name + "_" + b_or_f)

		else:
			print("DOES NOT EXIST: " + dcm_fold)

#ROI Location
#path = "F:\\PREDICT\\PREDICT_Volume_Measurements"
path = "F:\\PREDICT\\PREDICTVOLUMES"

#DICOM Folders
dcm_path = "F:\\PREDICT\\PREDICTDCM\\"

dcm_txt_file = open("dcm_files.txt","w")
roi_txt_file = open("roi_files.txt","w")

files = []
# r=root, d=directories, f = files
for r, d, f in os.walk(path):
    for dir1 in d:
        if 'qs' in dir1: #If the word 'series' is in dir1, then it goes through the following.
         	direct1=r+"\\"+dir1
         	#print(direct1)
         	qs_file = ntpath.basename(direct1)
         	qs_file1 = qs_file[:len(qs_file)-3]
         	direct2=direct1[26:40]
         	#print(direct2)
         	dcm_p = direct2.replace(" ","_")

         	#print(dcm_p)
         	direct3=direct1[41:]
         	#print(direct3)
         	dcm_p = direct2.replace(" ","_")
         	if 'line' in direct3.lower() or 'basleine' in direct3.lower():
         		#print('Baseline')
         		#run_prog
         		run_prog(dcm_path + dcm_p + "\\Baseline\\series", direct1, qs_file1, dcm_p,'Baseline')
         	elif 'ow' in direct3.lower() or '24' in direct3.lower():
         		#print('Follow-up')
         		#run_prog
         		run_prog(dcm_path + dcm_p + "\\Followup\\series", direct1, qs_file1, dcm_p,'Followup')         		


dcm_txt_file.close()
roi_txt_file.close()
