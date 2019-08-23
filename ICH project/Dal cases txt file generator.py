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





