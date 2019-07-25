"""
1) Obtain supplemental list of nii files from google sheet
2) Copy supplemental list of nii files in google sheet that match files from old, to new directory
3) Create text file listing all files in new directory 
"""
#pip install gspread
#pip install pandas
#pip install oauth2client

import os, shutil
import gspread
from oauth2client.service_account import ServiceAccountCredentials
from pandas import DataFrame

# ==============================================================================
# FOR PREDICT FILES
# ==============================================================================

#Set working directory
os.chdir(r"C:\R workspace\r-projects\ICH project")

#Connect to Google sheets
scope = ['https://spreadsheets.google.com/feeds','https://www.googleapis.com/auth/drive']
creds = ServiceAccountCredentials.from_json_keyfile_name('client_secret.json', scope)
client = gspread.authorize(creds)

sheet = client.open("PREDICT ROI File Issues").worksheet("Sean version PREDICT Missing V4")

data={'file_names':sheet.col_values(1),'include':sheet.col_values(3)}
df=DataFrame(data)
file_list=[]
for index, row in df.iterrows():
    if row['include']=='1':
        file_list.append(row['file_names'])


#DCM
old_directory=r"D:\Alex Files\PREDICT_TOTAL_V4\PREDICT_DCM_NII"
new_directory=r"D:\Alex Files\PREDICT_TOTAL_V4\PREDICT_DCM_NII"

#Copy supplemental PREDICT DCM nii's from old directory to new directory
oldlist=os.listdir(old_directory)
for oldfile in oldlist:
    for newfile in file_list:
        if newfile[8:10] == oldfile[8:10] and newfile[11:14]==oldfile[11:14]:
            if "Ba" in oldfile and ("Ba" or "ba") in newfile:
                src = old_directory + "//" + oldfile
                dst = new_directory + "//" + oldfile
                shutil.copyfile(src, dst)
            if "Fol" in oldfile and ("24h" or "Fo") in newfile:
                src = old_directory + "//" + oldfile
                dst = new_directory + "//" + oldfile
                shutil.copyfile(src, dst)

#Create text file of DCM nii files
newdir_list=os.listdir(new_directory)
if "dcm_files.txt" in newdir_list: 
    newdir_list.remove("dcm_files.txt")
if "dcm_files.txt" in os.listdir(os.path.dirname(new_directory)):
    os.remove(os.path.dirname(new_directory)+"//"+"dcm_files.txt")
f= open(os.path.dirname(new_directory)+"//"+"dcm_files.txt","w+")
for file in newdir_list:
    f.write(file)
    f.write("\n")
    print(file)
f.close()

#Create text file of ROI nii files
new_roi_directory = "D:\Alex Files\PREDICT_TOTAL_V4\PREDICT_ROI_NII"
newdir_list=os.listdir(new_roi_directory)
if "roi_files.txt" in newdir_list: 
    newdir_list.remove("roi_files.txt")
if "roi_files.txt" in os.listdir(os.path.dirname(new_roi_directory)):
    os.remove(os.path.dirname(new_roi_directory)+"//"+"roi_files.txt")
f= open(os.path.dirname(new_roi_directory)+"//"+"roi_files.txt","w+")
for file in newdir_list:
    f.write(file)
    f.write("\n")
    print(file)
f.close()

#Compare DCM and ROI nii txt files

error=0
dcmlist = [dcm.rstrip('\n') for dcm in open(os.path.dirname(new_directory)+"//"+"dcm_files.txt","r")]
roilist = [roi.rstrip('\n') for roi in open(os.path.dirname(new_directory)+"//"+"roi_files.txt","r")]
for dcmfile, roifile in zip(dcmlist, roilist):
    if not(dcmfile[8:10] == roifile[8:10] and dcmfile[11:14]==roifile[11:14]):
        print("Scan number error:",dcmfile,roifile)
        error+=1
    elif "Ba" in dcmfile:
        if not(any(x in roifile for x in ["Ba","ba"])):
            print("Baseline/Followup Error:",dcmfile,roifile)
            error+=1
    elif "Fol" in dcmfile:
        if not(any(x in roifile for x in ["Fo","fo"])):
            print("Baseline/Followup Error:",dcmfile,roifile)
            error+=1
print ("Total number of errors:",error)


# ==============================================================================
# FOR SPOTLIGHT FILES
# ==============================================================================
#Set working directory
os.chdir(r"C:\R workspace\r-projects\ICH project")

#Connect to Google sheets
scope = ['https://spreadsheets.google.com/feeds','https://www.googleapis.com/auth/drive']
creds = ServiceAccountCredentials.from_json_keyfile_name('client_secret.json', scope)
client = gspread.authorize(creds)

sheet = client.open("PREDICT ROI File Issues").worksheet("Sean version SPOTLIGHT missing v1")

data={'file_names':sheet.col_values(1),'include':sheet.col_values(3)}
df=DataFrame(data)
file_list=[]
for index, row in df.iterrows():
    if row['include']=='1':
        file_list.append(row['file_names'])


#DCM
old_directory=r"D:\Alex Files\SPOTLIGHT_V2\SPOTLIGHT_V2\SPOTLIGHT_NII_V2"
new_directory=r"D:\Alex Files\SPOTLIGHT_V2\SPOTLIGHT_V2\SPOTLIGHT_NII_V2"

#Copy supplemental SPOTLIGHT DCM nii's from old directory to new directory
oldlist=os.listdir(old_directory)
for oldfile in oldlist:
    for newfile in file_list:
        if newfile[0:3] == oldfile[8:10] and newfile[4:9]==oldfile[11:14]:
            if "Ba" in oldfile and {"BL","PARE"} in newfile:
                src = old_directory + "//" + oldfile
                dst = new_directory + "//" + oldfile
                shutil.copyfile(src, dst)
            if "Fol" in oldfile and ("24h" or "Fo") in newfile:
                src = old_directory + "//" + oldfile
                dst = new_directory + "//" + oldfile
                shutil.copyfile(src, dst)

#Create text file of DCM nii files
newdir_list=os.listdir(new_directory)
if "dcm_files.txt" in newdir_list: 
    newdir_list.remove("dcm_files.txt")
if "dcm_files.txt" in os.listdir(os.path.dirname(new_directory)):
    os.remove(os.path.dirname(new_directory)+"//"+"dcm_files.txt")
f= open(os.path.dirname(new_directory)+"//"+"dcm_files.txt","w+")
for file in newdir_list:
    f.write(file)
    f.write("\n")
    print(file)
f.close()

#Create text file of ROI nii files
new_roi_directory = "D:\Alex Files\SPOTLIGHT_V2\SPOTLIGHT_V2\SPOTLIGHT_ROI_NII_V2"
newdir_list=os.listdir(new_roi_directory)
if "roi_files.txt" in newdir_list: 
    newdir_list.remove("roi_files.txt")
if "roi_files.txt" in os.listdir(os.path.dirname(new_roi_directory)):
    os.remove(os.path.dirname(new_roi_directory)+"//"+"roi_files.txt")
f= open(os.path.dirname(new_roi_directory)+"//"+"roi_files.txt","w+")
for file in newdir_list:
    f.write(file)
    f.write("\n")
    print(file)
f.close()

#Compare DCM and ROI nii txt files

error=0
dcmlist = [dcm.rstrip('\n') for dcm in open(os.path.dirname(new_directory)+"//"+"dcm_files.txt","r")]
roilist = [roi.rstrip('\n') for roi in open(os.path.dirname(new_directory)+"//"+"roi_files.txt","r")]
for dcmfile, roifile in zip(dcmlist, roilist):
    if not(dcmfile[8:10] == roifile[8:10] and dcmfile[11:14]==roifile[11:14]):
        print("Error:",dcmfile,roifile)
        error+=1
    elif "Bas" in dcmfile:
        if not(("Bas" or "bas") in roifile):
            print("Error:",dcmfile,roifile)
            error+=1
    elif "Fol" in dcmfile:
        if not(("fol" or "Fol") in roifile):
            print("Error:",dcmfile,roifile)
            error+=1
print ("Total number of errors:",error)




