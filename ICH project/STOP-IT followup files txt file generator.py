import os, shutil
import gspread
from pandas import DataFrame

#Set working directory
os.chdir(r"D:\ICH Files\STOP-IT\STOP-IT\DCM_NII")
filepath=r"D:\ICH Files\STOP-IT\STOP-IT\DCM_NII"

for r, d, f in os.walk(filepath):
    if r==r"D:\ICH Files\STOP-IT\STOP-IT\DCM_NII":
        print(d)
        f=open(os.path.dirname(filepath)+"\\"+"STOP_IT.txt","w+")
        for filename in d:
            f.write(filename)
            f.write("-Followup")
            f.write("\n")
            print(filename)
        f.close()
