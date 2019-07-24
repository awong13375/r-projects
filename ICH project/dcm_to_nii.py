import os, shutil
import ntpath
import sys
import subprocess
from shutil import copyfile
import SimpleITK as sitk
import time


dcmpath = r"D:\Alex Files\SPOTLIGHT\SPOTLIGHT"
mhdpath = r"D:\Alex Files\SPOTLIGHT\SPOTLIGHT_MHD_MAC"
dcmoutputpath = r"D:\Alex Files\SPOTLIGHT\SPOTLIGHT_DCM_NII"
mhdoutputpath = r"D:\Alex Files\SPOTLIGHT\SPOTLIGHT_ROI_NII"

reader = sitk.ImageSeriesReader()
reader2 = sitk.ImageFileReader()

i=0
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
            xarr = []
            #get slice details
            for x in dirfiles:
                #print((direct1b+"\\\\"+x))
                reader2.SetFileName(direct1b+"\\\\"+x)
                reader2.LoadPrivateTagsOn()
                reader2.ReadImageInformation()
                xarr.append([{"slicesnum":reader2.GetMetaData("0020|0013"),"slicethick":reader2.GetMetaData("0018|0050")}])
            #print(xarr)
            #get name
            while True:
                direct1b = os.path.dirname(direct1b)
                #print(direct1b)
                pardir = ntpath.basename(direct1b)
                thepath = pardir + "-" + thepath
                if pardir == "SPOTLIGHT":
                    print(thepath)
                    break
            
            sitk.WriteImage(image, dcmoutputpath+"\\\\"+thepath+".nii")
            i = i+1
            
print(i)




