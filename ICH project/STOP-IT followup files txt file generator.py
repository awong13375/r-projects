import os, shutil
import gspread
from oauth2client.service_account import ServiceAccountCredentials
from pandas import DataFrame

#Set working directory
os.chdir(r"C:\Users\alexw\Desktop\STOP-IT_24hour_FollowUp-v2")
filepath=r"C:\Users\alexw\Desktop\STOP-IT_24hour_FollowUp-v2"

for r, d, f in os.walk(filepath):
    if r==r"C:\Users\alexw\Desktop\STOP-IT_24hour_FollowUp-v2":
        print(d)
        f=open(os.path.dirname(filepath)+"\\"+"STOP_IT.txt","w+")
        for filename in d:
            f.write(filename)
            f.write("-Followup")
            f.write("\n")
            print(filename)
        f.close()
