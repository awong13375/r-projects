import os, shutil
import ntpath
import sys
import subprocess
from shutil import copyfile
import SimpleITK as sitk
import time
import nibabel as nib

mhdpath = r"D:\Virtual Machine\Shared Folder\DAL_ICH_MHD_MAC"
outputpath = r"D:\Virtual Machine\Shared Folder\DAL_ICH_MHD_NII"

dirfiles = os.listdir(mhdpath)
i=0;
for x in dirfiles:
    print(x)
    if x.endswith('.mhd'):
        print(x[:len(x)-4])
        i += 1
        img = sitk.ReadImage(mhdpath + "\\" + x)
        outname = outputpath + "\\" + x[:len(x)-4] +".nii"
        print(outname)
        sitk.WriteImage(img, outname)
print(i)
