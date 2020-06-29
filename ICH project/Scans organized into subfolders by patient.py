import os

dcm_directory=r"D:\ICH Files\STOP-IT\STOP-IT_ALL_INCLUDED_SCANS"


dcm_file_list=os.listdir(dcm_directory)
i=0
for filename in dcm_file_list:
    dcm_file_list[i]=filename[:9]
    i=i+1

dcm_file_list=list(dict.fromkeys(dcm_file_list))
for folder_name in dcm_file_list:
    if not os.path.exists(dcm_directory + "\\" + folder_name):
        os.makedirs(dcm_directory + "\\" + folder_name)

dcm_nii_list=list()
for filename in os.listdir(dcm_directory):
    if "nii" in filename:
        dcm_nii_list.append(filename)


for filename in  dcm_nii_list:
    os.rename(dcm_directory + "\\" + filename, dcm_directory + "\\" + filename[:9] + "\\" + filename)

