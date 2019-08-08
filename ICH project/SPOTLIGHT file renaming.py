import os, shutil
import gspread
from oauth2client.service_account import ServiceAccountCredentials
from pandas import DataFrame

#Set working directory
os.chdir(r"C:\R workspace\r-projects\ICH project")

#Connect to Google sheets
scope = ['https://spreadsheets.google.com/feeds','https://www.googleapis.com/auth/drive']
creds = ServiceAccountCredentials.from_json_keyfile_name('client_secret.json', scope)
client = gspread.authorize(creds)

sheet = client.open("PREDICT ROI File Issues").worksheet("SPOTLIGHT v2")

    data={'original_DCM_filename':sheet.col_values(1)[1:],
    'DCM_change_to':sheet.col_values(2)[1:],
    'original_ROI_filename':sheet.col_values(3)[1:],
    'ROI_change_to':sheet.col_values(4)[1:]}
    df=DataFrame(data)


dcm_directory=r"D:\Alex Files\SPOTLIGHT_V3\SPOTLIGHT_NII_V3"
roi_directory=r"D:\Alex Files\SPOTLIGHT_V3\SPOTLIGHT_ROI_NII_V3"

for original_filename, change_to_filename in zip(data['original_DCM_filename'], data['DCM_change_to']):
    for dcmfile in os.listdir(dcm_directory):
        if dcmfile==original_filename:
            os.rename(dcm_directory + '//' + dcmfile , dcm_directory + '//' + change_to_filename)

for original_filename, change_to_filename in zip(data['original_ROI_filename'], data['ROI_change_to']):
    for roifile in os.listdir(roi_directory):
        if roifile==original_filename:
            os.rename(roi_directory+'//'+roifile, roi_directory+'//'+change_to_filename)

#Create text file of DCM nii files
newdir_list=os.listdir(dcm_directory)
if "dcm_files.txt" in newdir_list: 
    newdir_list.remove("dcm_files.txt")
if "dcm_files.txt" in os.listdir(os.path.dirname(dcm_directory)):
    os.remove(os.path.dirname(dcm_directory)+"//"+"dcm_files.txt")
f= open(os.path.dirname(dcm_directory)+"//"+"dcm_files.txt","w+")
for file in newdir_list:
    f.write(file)
    f.write("\n")
    print(file)
f.close()

#Create text file of ROI nii files
newdir_list=os.listdir(roi_directory)
if "roi_files.txt" in newdir_list: 
    newdir_list.remove("roi_files.txt")
if "roi_files.txt" in os.listdir(os.path.dirname(roi_directory)):
    os.remove(os.path.dirname(roi_directory)+"//"+"roi_files.txt")
f= open(os.path.dirname(roi_directory)+"//"+"roi_files.txt","w+")
for file in newdir_list:
    f.write(file)
    f.write("\n")
    print(file)
f.close()

#Compare DCM and ROI nii txt files

error=0
dcmlist = [dcm.rstrip('\n') for dcm in open(os.path.dirname(dcm_directory)+"//"+"dcm_files.txt","r")]
roilist = [roi.rstrip('\n') for roi in open(os.path.dirname(roi_directory)+"//"+"roi_files.txt","r")]
for dcmfile, roifile in zip(dcmlist, roilist):
    if not(dcmfile[10:13] == roifile[0:3] and dcmfile[14:18]==roifile[4:8]):
        print("Error:",dcmfile,roifile)
        error+=1
    elif not(dcmfile[19]==roifile[9]):
        print("Error:",dcmfile,roifile)
        error+=1
print ("Total number of errors:",error)








