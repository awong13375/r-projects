dcm_directory=r"D:\ICH Files\SPOTLIGHT\SPOTLIGHT_ADD"
newdir_list=os.listdir(dcm_directory)
if "dcm_files.txt" in newdir_list: 
    newdir_list.remove("dcm_files.txt")
if "dcm_files.txt" in os.listdir(os.path.dirname(dcm_directory)):
    os.remove(os.path.dirname(dcm_directory)+"//"+"dcm_files.txt")
f= open(os.path.dirname(dcm_directory)+"//"+"dcm_add_files.txt","w+")
for file in newdir_list:
    f.write(file)
    f.write(",")
    print(file)
f.close()

