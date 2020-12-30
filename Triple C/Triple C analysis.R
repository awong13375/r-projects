data=read.csv("C:/Users/alexw/Google Drive/Desktop files/Dal Med/Med3/TripleC/Data/Triple C raw data.csv")
data[data==""]<-NA

#subset data into pts with post infection syndrome vs. not
pis_group=subset(data, data$physicalpostcovid=="Yes" | data$physicalpostcovid_2=="Yes" | data$physicalpostcovid_2_3=="Yes" | data$physicalpostcovid_2_3_4 =="Yes" | 
                       data$mhpostcovid=="Yes" | data$mhpostcovid_2=="Yes" | data$mhpostcovid_2_3=="Yes" | data$mhpostcovid_2_3_4=="Yes")
pis_group$pis_status="Yes"
no_pis_group=subset(data, data$physicalpostcovid=="No" & (data$physicalpostcovid_2=="No" | is.na(data$physicalpostcovid_2))  & (data$physicalpostcovid_2_3=="No" | is.na(data$physicalpostcovid_2_3)) 
                    & (data$physicalpostcovid_2_3_4 =="No" | is.na(data$physicalpostcovid_2_3_4)) & (data$mhpostcovid=="No" | is.na(data$mhpostcovid)) & 
                      (data$mhpostcovid_2=="No" | is.na(data$mhpostcovid_2)) & (data$mhpostcovid_2_3=="No" | is.na(data$mhpostcovid_2_3)) & 
                      (data$mhpostcovid_2_3_4=="No" | is.na(data$mhpostcovid_2_3_4)))
no_pis_group$pis_status="No"
missing_pis_fu=subset(data, is.na(data$physicalpostcovid) & is.na(data$mhpostcovid))
missing_pis_fu$pis_status=NA
data=rbind(pis_group, no_pis_group, missing_pis_fu)
data$pis_status=factor(data$pis_status, levels=c("Yes","No"))

data$covid_positive <- as.Date(as.character(data$covid_positive),"%d-%m-%Y")
data$date_recovered <- as.Date(as.character(data$date_recovered),"%d-%m-%Y")
data$duration_of_illness=difftime(data$date_recovered ,data$covid_positive , units = c("days"))


#Table 1 (1 mo follow up only so far)
for (status in levels(as.factor(data$pis_status))){
  pis_data=subset(data, data$pis_status==status)
  result=c()
  
  result=append(result, nrow(pis_data))
  
  result=append(result, mean(pis_data$age_years))
  result=append(result, sd(pis_data$age_years))
  
  result=append(result, nrow(subset(pis_data, pis_data$sex=="Female")))
  result=append(result, nrow(subset(pis_data, pis_data$sex=="Female"))/nrow(!is.na(pis_data))*100)
  
  result=append(result, nrow(subset(pis_data, pis_data$ethnic=="White/Caucasian")))
  result=append(result, nrow(subset(pis_data, pis_data$ethnic=="White/Caucasian"))/nrow(!is.na(pis_data))*100)
  
  result=append(result, nrow(subset(pis_data, pis_data$ethnic=="East Asian")))
  result=append(result, nrow(subset(pis_data, pis_data$ethnic=="East Asian"))/nrow(!is.na(pis_data))*100)
  
  result=append(result, nrow(subset(pis_data, pis_data$ethnic=="Black")))
  result=append(result, nrow(subset(pis_data, pis_data$ethnic=="Black"))/nrow(!is.na(pis_data))*100)
  
  result=append(result, nrow(subset(pis_data, pis_data$ethnic=="Other")))
  result=append(result, nrow(subset(pis_data, pis_data$ethnic=="Other"))/nrow(!is.na(pis_data))*100)
  
  result=append(result, nrow(subset(pis_data, pis_data$housing=="Rooming house")))
  result=append(result, nrow(subset(pis_data, pis_data$housing=="Rooming house"))/nrow(!is.na(pis_data))*100)
  
  result=append(result, nrow(subset(pis_data, pis_data$housing=="Apartment")))
  result=append(result, nrow(subset(pis_data, pis_data$housing=="Apartment"))/nrow(!is.na(pis_data))*100)
  
  result=append(result, nrow(subset(pis_data, pis_data$housing=="Townhome/Semi-detatched")))
  result=append(result, nrow(subset(pis_data, pis_data$housing=="Townhome/Semi-detatched"))/nrow(!is.na(pis_data))*100)
  
  result=append(result, nrow(subset(pis_data, pis_data$housing=="Detatched house")))
  result=append(result, nrow(subset(pis_data, pis_data$housing=="Detatched house"))/nrow(!is.na(pis_data))*100)
  
  result=append(result, nrow(subset(pis_data, pis_data$income=="Low Income (<= $35,999)")))
  result=append(result, nrow(subset(pis_data, pis_data$income=="Low Income (<= $35,999)"))/nrow(!is.na(pis_data))*100)
  
  result=append(result, nrow(subset(pis_data, pis_data$income=="Middle Income ($36-60,000)")))
  result=append(result, nrow(subset(pis_data, pis_data$income=="Middle Income ($36-60,000)"))/nrow(!is.na(pis_data))*100)
  
  result=append(result, nrow(subset(pis_data, pis_data$income=="High Income (> $60,000)")))
  result=append(result, nrow(subset(pis_data, pis_data$income=="High Income (> $60,000)"))/nrow(!is.na(pis_data))*100)
  
  result=append(result, nrow(subset(pis_data, pis_data$income=="Unknown")))
  result=append(result, nrow(subset(pis_data, pis_data$income=="Unknown"))/nrow(!is.na(pis_data))*100)
  
  result=append(result, nrow(subset(pis_data, pis_data$cardiac=="Yes")))
  result=append(result, nrow(subset(pis_data, pis_data$cardiac=="Yes"))/nrow(!is.na(pis_data))*100)
  
  result=append(result, nrow(subset(pis_data, pis_data$pulmonary=="Yes")))
  result=append(result, nrow(subset(pis_data, pis_data$pulmonary=="Yes"))/nrow(!is.na(pis_data))*100)
  
  result=append(result, nrow(subset(pis_data, pis_data$hypertension=="Yes")))
  result=append(result, nrow(subset(pis_data, pis_data$hypertension=="Yes"))/nrow(!is.na(pis_data))*100)
  
  result=append(result, nrow(subset(pis_data, pis_data$asthma=="Yes")))
  result=append(result, nrow(subset(pis_data, pis_data$asthma=="Yes"))/nrow(!is.na(pis_data))*100)
  
  result=append(result, nrow(subset(pis_data, pis_data$diabetes1=="Yes" | pis_data$diabetes2=="Yes")))
  result=append(result, nrow(subset(pis_data, pis_data$diabetes1=="Yes" | pis_data$diabetes2=="Yes"))/nrow(!is.na(pis_data))*100)
  
  result=append(result, nrow(subset(pis_data, pis_data$liver=="Yes")))
  result=append(result, nrow(subset(pis_data, pis_data$liver=="Yes"))/nrow(!is.na(pis_data))*100)
  
  result=append(result, nrow(subset(pis_data, pis_data$rhuem=="Yes")))
  result=append(result, nrow(subset(pis_data, pis_data$rhuem=="Yes"))/nrow(!is.na(pis_data))*100)
  
  result=append(result, nrow(subset(pis_data, pis_data$neuro=="Yes")))
  result=append(result, nrow(subset(pis_data, pis_data$neuro=="Yes"))/nrow(!is.na(pis_data))*100)
  
  result=append(result, nrow(subset(pis_data, pis_data$dementia=="Yes")))
  result=append(result, nrow(subset(pis_data, pis_data$dementia=="Yes"))/nrow(!is.na(pis_data))*100)
  
  result=append(result, nrow(subset(pis_data, pis_data$hematologic=="Yes")))
  result=append(result, nrow(subset(pis_data, pis_data$hematologic=="Yes"))/nrow(!is.na(pis_data))*100)
  
  result=append(result, nrow(subset(pis_data, pis_data$malnutrition=="Yes")))
  result=append(result, nrow(subset(pis_data, pis_data$malnutrition=="Yes"))/nrow(!is.na(pis_data))*100)
  result=append(result, nrow(subset(pis_data, pis_data$malnutrition=="Unknown")))
  result=append(result, nrow(subset(pis_data, pis_data$malnutrition=="Unknown"))/nrow(!is.na(pis_data))*100)
  
  result=append(result, nrow(subset(pis_data, pis_data$malignant=="Yes")))
  result=append(result, nrow(subset(pis_data, pis_data$malignant=="Yes"))/nrow(!is.na(pis_data))*100)
  result=append(result, nrow(subset(pis_data, pis_data$malignant=="Unknown")))
  result=append(result, nrow(subset(pis_data, pis_data$malignant=="Unknown"))/nrow(!is.na(pis_data))*100)
  
  
  result=append(result, nrow(subset(pis_data, pis_data$hiv=="Yes")))
  result=append(result, nrow(subset(pis_data, pis_data$hiv=="Yes"))/nrow(!is.na(pis_data))*100)
  result=append(result, nrow(subset(pis_data, pis_data$hiv=="Unknown")))
  result=append(result, nrow(subset(pis_data, pis_data$hiv=="Unknown"))/nrow(!is.na(pis_data))*100)
  
  
  result=append(result, nrow(subset(pis_data, pis_data$smoker=="Yes")))
  result=append(result, nrow(subset(pis_data, pis_data$smoker=="Yes"))/nrow(!is.na(pis_data))*100)
  
  result=append(result, nrow(subset(pis_data, pis_data$obesity=="Yes")))
  result=append(result, nrow(subset(pis_data, pis_data$obesity=="Yes"))/nrow(!is.na(pis_data))*100)
  result=append(result, nrow(subset(pis_data, pis_data$obesity=="Unknown")))
  result=append(result, nrow(subset(pis_data, pis_data$obesity=="Unknown"))/nrow(!is.na(pis_data))*100)
  
  
  result=append(result, nrow(subset(pis_data, pis_data$snore=="Yes")))
  result=append(result, nrow(subset(pis_data, pis_data$snore=="Yes"))/nrow(!is.na(pis_data))*100)
  result=append(result, nrow(subset(pis_data, pis_data$snore=="Unknown")))
  result=append(result, nrow(subset(pis_data, pis_data$snore=="Unknown"))/nrow(!is.na(pis_data))*100)
  
  result=append(result, nrow(subset(pis_data, pis_data$cpap=="Yes")))
  result=append(result, nrow(subset(pis_data, pis_data$cpap=="Yes"))/nrow(!is.na(pis_data))*100)
  
  result=append(result, nrow(subset(pis_data, pis_data$acei!="No" | pis_data$arbs!="No")))
  result=append(result, nrow(subset(pis_data, pis_data$acei!="No" | pis_data$arbs!="No"))/nrow(!is.na(pis_data))*100)
  
  result=append(result, nrow(subset(pis_data, pis_data$nsaids!="No")))
  result=append(result, nrow(subset(pis_data, pis_data$nsaids!="No"))/nrow(!is.na(pis_data))*100)
  
  result=append(result, nrow(subset(pis_data, pis_data$steroid=="Yes")))
  result=append(result, nrow(subset(pis_data, pis_data$steroid=="Yes"))/nrow(!is.na(pis_data))*100)
  result=append(result, nrow(subset(pis_data, pis_data$steroid=="Unknown")))
  result=append(result, nrow(subset(pis_data, pis_data$steroid=="Unknown"))/nrow(!is.na(pis_data))*100)
  
  result=append(result, nrow(subset(pis_data, pis_data$statin!="No")))
  result=append(result, nrow(subset(pis_data, pis_data$statin!="No"))/nrow(!is.na(pis_data))*100)
  
  result=append(result, nrow(subset(pis_data, pis_data$metformin=="Yes")))
  result=append(result, nrow(subset(pis_data, pis_data$metformin=="Yes"))/nrow(!is.na(pis_data))*100)
  
  result=append(result, nrow(subset(pis_data, pis_data$acetamin=="Yes")))
  result=append(result, nrow(subset(pis_data, pis_data$acetamin=="Yes"))/nrow(!is.na(pis_data))*100)
  
  result=append(result, nrow(subset(pis_data, pis_data$verapamil=="Yes")))
  result=append(result, nrow(subset(pis_data, pis_data$verapamil=="Yes"))/nrow(!is.na(pis_data))*100)
  
  if (status=="Yes"){
    table_1=as.data.frame(result)
  } else {
    table_1=cbind(table_1, result)
  }
}


colnames(table_1)=c("PIS","No PIS")
rownames(table_1)=c("n","age","age_sd","gender","gender_%","white","white_%","asian","asian_%","black","black_%","other","other_%",
                    "rooming_house","rooming_house_%","apartment","apartment_%","townhome","townhome_%","detatched house","detatched house_%",
                    "low$","low$_%","middle$","middle$_%","high$","high$_%","unknown$","unknown_%","cardiac","cardiac_%","pulm","pulm_%","htn","htn_%","asthma","asthma_%",
                    "DM","DM_%","liver","liver_%","rhuem","rhuem_%","neuro","neuro_%","dementia","dementia_%","hematologic","hematologic_%","malnutrition","malnutrition_%",
                    "unknown_malnutrition","unknown_malnutrition_%","malignant","malignant_%","unknown_malignant","unknown_malignant_%",
                    "hiv","hiv_%","unknown_hiv","unknown_hiv_%","smoker","smoker_%","obesity","obesity_%","unknown_obesity","unknown_obesity_%","snore","snore_%",
                    "unknown_snore","unknown_snore_%","cpap","cpap_%","acei/arb","acei/arb_%","nsaids","nsaids_%","steroid","steroid_%","unknown_steroid","unknown_steroid_%",
                    "statin","statin_%","metformin","metformin_%","acetamin","acetamin_%","verapamil","verapamil_%")

#write.csv(table_1, "C:/Users/alexw/Google Drive/Desktop files/Dal Med/Med3/TripleC/table_1.csv")










