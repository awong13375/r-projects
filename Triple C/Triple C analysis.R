data=read.csv("C:/Users/alexw/Google Drive/Desktop files/Dal Med/Med3/TripleC/Data/Triple C raw data.csv")
data[data==""]<-NA

#subset data into pts with post infection syndrome vs. not
pis_group=subset(data, data$physicalpostcovid=="Yes" | data$physicalpostcovid_2=="Yes" | data$physicalpostcovid_2_3=="Yes" | data$physicalpostcovid_2_3_4 =="Yes" | 
                       data$mhpostcovid=="Yes" | data$mhpostcovid_2=="Yes" | data$mhpostcovid_2_3=="Yes" | data$mhpostcovid_2_3_4=="Yes")
no_pis_group=subset(data, data$physicalpostcovid=="No" & (data$physicalpostcovid_2=="No" | is.na(data$physicalpostcovid_2))  & (data$physicalpostcovid_2_3=="No" | is.na(data$physicalpostcovid_2_3)) 
                    & (data$physicalpostcovid_2_3_4 =="No" | is.na(data$physicalpostcovid_2_3_4)) & (data$mhpostcovid=="No" | is.na(data$mhpostcovid)) & 
                      (data$mhpostcovid_2=="No" | is.na(data$mhpostcovid_2)) & (data$mhpostcovid_2_3=="No" | is.na(data$mhpostcovid_2_3)) & 
                      (data$mhpostcovid_2_3_4=="No" | is.na(data$mhpostcovid_2_3_4)))
missing_pis_fu=subset(data, is.na(data$physicalpostcovid) & is.na(data$mhpostcovid))





