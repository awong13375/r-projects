"""
1) Obtain supplemental list of nii files from google sheet
2) Copy supplemental list of nii files in google sheet that match files from old, to new directory
3) Create text file listing all files in new directory 
"""

import os, shutil
import gspread
from oauth2client.service_account import ServiceAccountCredentials
from pandas import DataFrame


#Set working directory
os.chdir(r"C:\R workspace\r-projects\ICH project")

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
old_directory=r"C:\Users\alexw\Desktop\Alex Files\PREDICTv1\PREDICT_DCM_NII"
new_directory=r"C:\Users\alexw\Desktop\Alex Files\PREDICTv4\PREDICT_DCM_NII"

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

#Create text file of nii files
newdir_list=os.listdir(new_directory)
if "dcm_files.txt" in newdir_list: 
    newdir_list.remove("dcm_files.txt")
if "dcm_files.txt" in os.listdir(new_directory):
    os.remove(new_directory+"//"+"dcm_files.txt")
f= open(new_directory+"//"+"dcm_files.txt","w+")
for file in newdir_list:
    f.write(file)
    f.write("\n")
f.close()



