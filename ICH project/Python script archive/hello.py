#goal = convert dicom to nii

#1. get list of dicom folders PREDICT-01-001/baseline/series

from __future__ import print_function

import os

import SimpleITK as sitk
import sys

def test1():
	print('test');

def ensure_dir(file_path):
    #directory = os.path.dirname(file_path)
    directory = file_path
    print(directory)
    if not os.path.exists(directory):
        os.makedirs(directory)

def convert_dcm_nii(dcm_folder,output_name):

	output_fold = "F:\\PREDICT\\PREDICT_SCAN_NII\\"
	reader = sitk.ImageSeriesReader()
	dicom_names = reader.GetGDCMSeriesFileNames(dcm_folder)
	reader.SetFileNames(dicom_names)

	image = reader.Execute()

	size = image.GetSize()
	print( "Image size:", size[0], size[1], size[2])
	##ensure_dir(y+z)
	sitk.WriteImage(image, output_fold+output_name)




path = "F:\\PREDICT\\PREDICT_DCM"


files = []
# r=root, d=directories, f = files
for r, d, f in os.walk(path):
    for dir1 in d:
        if 'series' in dir1: #If the word 'series' is in dir1, then it goes through the following.
         	direct1=r+"\\"+dir1
        	print(direct1)
        	direct2 = direct1[23:len(direct1)-7]+".nii"
        	direct3 = direct2.replace("\\","-")
        	print(direct3)
        	convert_dcm_nii(direct1,direct3)

            #files.append(os.path.join(r, file))

#for f in files:
    #print(f)



#convert to dicom - nii





#rename nii in single folder using the folder name - PREDICT-01-001-baseline.nii

#importing os module
"""
import os
"""
#Function to rename multiple files
"""
def main():
	i = 0

	for filename in os.listdir("F:\\PREDICT\\PythonCode\\test"):
		dst = "Hostel" + str(i) + ".nii"
		src = 'xyz' + filename
		dst = 'xyz' + dst

		#rename() function will rename all the files.
		os.rename(src,dst)
		i += 1
"""


