dcm_directory=r"D:\SPOTLIGHT-CLEANED-ORIG\SPOTLIGHT\002-0003\2-Post-dose"

for dcmfile in os.listdir(dcm_directory):
    os.rename(dcm_directory+'//'+dcmfile, dcm_directory+'//'+dcmfile+".dcm")
