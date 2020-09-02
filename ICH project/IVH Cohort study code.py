import os, shutil
from pandas import DataFrame
from shutil import copyfile
import nibabel as nib
import numpy as np
import csv

#Set working directory
os.chdir(r"C:\Users\alexw\Desktop")
directory=r"C:\Users\alexw\Desktop"

#Compare files to gsheet to identify missing files
f= open("PREDICT ROIs.txt","r")
lines = f.read().split('\n')

file_list=os.listdir(r"D:\ICH Files\PREDICT\PREDICT\ROI_NII")

list=[]
for file in file_list:
    list.append(file[0:23])

missing=[]
for line in lines:
    if line not in list:
        missing.append(line)

print(missing)
    
#Copy files from IVH folder to DCM_NII folder
filepath=r"D:\ICH Files\SPOTLIGHT\SPOTLIGHT_IVH"
for r, d, f in os.walk(filepath):
    if len(f)>0:
        for file in f:
            copyfile(r + "\\" + file, r"D:\ICH Files\SPOTLIGHT\SPOTLIGHT\DCM_NII" + "\\" + file)
    print(r)
    print("-------")
    print(d)
    print("*******")
    print(f)

#Generate text file
os.chdir(r"D:\ICH Files\PREDICT\PREDICT")
filepath=r"D:\ICH Files\PREDICT\PREDICT"
txt_file=open("dcm_nii.txt","w+")
for r, d, f in os.walk(filepath+"\\"+"DCM_NII"):
    for file in f:
        txt_file.write(file)
        txt_file.write("\n")
    print(r)
    print("-------")
    print(d)
    print("*******")
    print(f)
txt_file.close()

#Compare dcm and roi text files
dcm_nii_txt=open("dcm_nii.txt","r")
roi_nii_txt=open("roi_nii.txt","r")
for (dcm, roi) in zip(dcm_nii_txt, roi_nii_txt):
    if dcm[0:23] == roi[0:23]:
        print ("match!")
    if dcm[0:23] != roi[0:23]:
        print (dcm[0:23] + "|" + roi[0:23])

#nii volume calculator
def compute_nii_label_volume(nii_filepath):
  nii_img = nib.load(nii_filepath)
  print('affine:', nii_img.affine)
  voxel_spacing = [nii_img.affine[i][i] for i in range(3)]
  voxel_volume = np.prod(voxel_spacing)
  print('voxel_spacing:', voxel_spacing)
  print('voxel_volume:', voxel_volume, 'mm3')
  label_data = np.array(np.rint(nii_img.get_fdata()), dtype=np.int8)
  
  label_values = np.unique(label_data)
  print(label_values)  # this is used to compute the volume of all labels [0,1,2 etc.] that are present
  volumes = [0 for n in label_values]
  for i,k in enumerate(label_values):
    volumes[i] = voxel_volume * np.sum(np.where(label_data == k, 1, 0))

  return volumes

filepath=r"D:\ICH Files\SPOTLIGHT\SPOTLIGHT\ROI_NII"
volumes=[]
for r, d, f in os.walk(filepath):
    for file in f:
        volumes.append(compute_nii_label_volume(filepath + "\\" + file))
    print(r)
    print("-------")
    print(d)
    print("*******")
    print(f)

os.chdir(r"C:\Users\alexw\Desktop")
with open("volumes.csv", "w", newline="") as f:
    writer = csv.writer(f)
    writer.writerows(volumes)

