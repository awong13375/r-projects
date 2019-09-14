import os, shutil
import gspread
from oauth2client.service_account import ServiceAccountCredentials
from pandas import DataFrame

#Set working directory
os.chdir(r"C:\Users\alexw\Desktop")
directory=r"C:\Users\alexw\Desktop"

#Connect to Google sheets
scope = ['https://spreadsheets.google.com/feeds','https://www.googleapis.com/auth/drive']
creds = ServiceAccountCredentials.from_json_keyfile_name('client_secret.json', scope)
client = gspread.authorize(creds)

sheet = client.open("PREDICT ROI File Issues").worksheet("Dal ICH Cases 1 master sheet")

data={'pt_name':sheet.col_values(4)[18:],
'Dal_ID':sheet.col_values(5)[18:]}
df=DataFrame(data)

f= open("dal_cases.txt","w+")

for name, pt_id in zip(data['pt_name'],data['Dal_ID']):
    result = [x.strip() for x in name.split(',')]
    print(result)
    f.write(result[0])
    f.write(",")
    f.write(result[1])
    f.write(",")
    f.write(pt_id)
    f.write("\n")
f.close()



#Generate text file directly from file directory
for r,d,f in os.walk(directory):
    if r==r"C:\Users\alexw\Desktop\STOPIT Followup Cases\STOPIT_DCM_NII":
        txt=open("stopit_dcm.txt","w+")
        for file in f:
            print(file)
            txt.write(file)
            txt.write("\n")
        txt.close()
    if r==r"C:\Users\alexw\Desktop\STOPIT Followup Cases\STOPIT_MHD_NII":
        txt=open("stopit_mhd.txt","w+")
        for file in f:
            txt.write(file)
            txt.write("\n")
        txt.close()
    

