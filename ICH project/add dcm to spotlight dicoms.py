dcm_directory=r"D:\Alex Files\SPOTLIGHT_V3\SPOTLIGHT_NII_V3"


for original_filename, change_to_filename in zip(data['original_DCM_filename'], data['DCM_change_to']):
    for dcmfile in os.listdir(dcm_directory):
        if dcmfile==original_filename:
            os.rename(dcm_directory + '//' + dcmfile , dcm_directory + '//' + change_to_filename)
