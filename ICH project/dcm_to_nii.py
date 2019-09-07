import os, shutil
import ntpath
import sys
import subprocess
from shutil import copyfile
import SimpleITK as sitk
import time


dcmpath = r"D:\Virtual Machine\Shared Folder\DAL_ICH_DCM"
dcmoutputpath = r"D:\Virtual Machine\Shared Folder\DAL_ICH_DCM_NII"

reader = sitk.ImageSeriesReader()
reader2 = sitk.ImageFileReader()


for r, d, f in os.walk(dcmpath):
    for dir1 in d:
        direct1=r+"\\"+dir1
        print(direct1)
        direct1b = direct1.replace('\\','\\\\')
        onlyfiles = next(os.walk(direct1b))[2] #dir is your directory path as string
        totalfiles = len(onlyfiles)
        if totalfiles > 5:
            thepath = ntpath.basename(direct1b)
            #print(thepath)
            dirfiles = os.listdir(direct1b)
            #print(dirfiles)
            print(direct1b)
            dicom_names = reader.GetGDCMSeriesFileNames(direct1b)
            reader.SetFileNames(dicom_names)
            image = reader.Execute()
            size = image.GetSize()
            #print(size)
            xarr = [];
            #get slice details
            for x in dirfiles:
                #print((direct1b+"\\\\"+x))
                reader2.SetFileName(direct1b+"\\\\"+x)
                reader2.LoadPrivateTagsOn();
                reader2.ReadImageInformation();
            #print(xarr)
            sitk.WriteImage(image, dcmoutputpath+"\\\\"+thepath+".nii")

            while True:
                direct1b = os.path.dirname(direct1b)
                #print(direct1b)
                pardir = ntpath.basename(direct1b)
                thepath = pardir + "-" + thepath


