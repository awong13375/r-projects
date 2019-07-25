import os, shutil
import gspread
from oauth2client.service_account import ServiceAccountCredentials
from pandas import DataFrame

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




old_directory=r"C:\Users\alexw\Desktop\Alex Files\PREDICT"
new_directory=r"C:\Users\alexw\Desktop\Alex Files\test"


for ro, do, fo in os.walk(old_directory):
    for r, d, f in os.walk(new_directory):

        print (ro)
        print (do)
        print (fo)
        print (";")
        print (r)
        print(d)
        print(f)
        print("-----------------")

