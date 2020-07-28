import os, shutil
import ntpath
import sys
import subprocess
from shutil import copyfile
import SimpleITK as sitk
import time

#PREDICT

nii_directory=r"D:\ICH Files\PREDICT\PREDICT_TOTAL\PREDICT_IVH"

file=open(r"D:\ICH Files\PREDICT\PREDICT_TOTAL\PREDICT_missing_rois.txt")
content=file.read()
content=content.split("\n")
file.close()

for r, d, f in os.walk(nii_directory):
    for file in f:
        for study in content:
            if study in file:
                copyfile(r + "\\" + file, r"D:\ICH Files\PREDICT\PREDICT_TOTAL\PREDICT_missing_rois" + "\\" + file)


#SPOTLIGHT

