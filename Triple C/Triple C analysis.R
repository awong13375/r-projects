library(WebPower)
library(rqdatatable)
#data merging
old_data=read.csv("C:/Users/alexw/Google Drive/Desktop files/Dal Med/Med3/TripleC/Data/Triple C raw data.csv")
new_data=read.csv("C:/Users/alexw/Google Drive/Desktop files/Dal Med/Med3/TripleC/Data/Triple C Data - 01Jun2021_rawdata.csv")
merged_data <- natural_join(old_data, new_data, by="ï..Record.Id", jointype="FULL")

col_order=as.vector(colnames(new_data))
merged_data=merged_data[,col_order]
#write.csv(merged_data, "C:/Users/alexw/Google Drive/Desktop files/Dal Med/Med3/TripleC/Data/merged_data.csv")

#Power analysis
wp.logistic(n = NULL, p0 = 0.53, p1 = 0.47, alpha = 0.05,
            power = 0.80, family = c("normal"), parameter = c(0,1))

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


# Table 1 ----
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
  
  result=append(result, mean(pis_data$duration_of_illness, na.rm=TRUE))
  result=append(result, sd(pis_data$age_years, na.rm=TRUE))
  
  result=append(result, nrow(subset(pis_data, pis_data$fever_documented=="Yes")))
  result=append(result, nrow(subset(pis_data, pis_data$fever_documented=="Yes"))/nrow(!is.na(pis_data))*100)
  result=append(result, nrow(subset(pis_data, pis_data$fever_documented=="Unknown")))
  result=append(result, nrow(subset(pis_data, pis_data$fever_documented=="Unknown"))/nrow(!is.na(pis_data))*100)
  
  result=append(result, nrow(subset(pis_data, pis_data$jointpain=="Yes")))
  result=append(result, nrow(subset(pis_data, pis_data$jointpain=="Yes"))/nrow(!is.na(pis_data))*100)
  result=append(result, nrow(subset(pis_data, pis_data$jointpain=="Unknown")))
  result=append(result, nrow(subset(pis_data, pis_data$jointpain=="Unknown"))/nrow(!is.na(pis_data))*100)
  
  result=append(result, nrow(subset(pis_data, pis_data$cough=="Yes")))
  result=append(result, nrow(subset(pis_data, pis_data$cough=="Yes"))/nrow(!is.na(pis_data))*100)
  result=append(result, nrow(subset(pis_data, pis_data$cough=="Unknown")))
  result=append(result, nrow(subset(pis_data, pis_data$cough=="Unknown"))/nrow(!is.na(pis_data))*100)
  
  result=append(result, nrow(subset(pis_data, pis_data$fatigue=="Yes")))
  result=append(result, nrow(subset(pis_data, pis_data$fatigue=="Yes"))/nrow(!is.na(pis_data))*100)
  result=append(result, nrow(subset(pis_data, pis_data$fatigue=="Unknown")))
  result=append(result, nrow(subset(pis_data, pis_data$fatigue=="Unknown"))/nrow(!is.na(pis_data))*100)
  
  result=append(result, nrow(subset(pis_data, pis_data$sorethroat=="Yes")))
  result=append(result, nrow(subset(pis_data, pis_data$sorethroat=="Yes"))/nrow(!is.na(pis_data))*100)
  result=append(result, nrow(subset(pis_data, pis_data$sorethroat=="Unknown")))
  result=append(result, nrow(subset(pis_data, pis_data$sorethroat=="Unknown"))/nrow(!is.na(pis_data))*100)
  
  result=append(result, nrow(subset(pis_data, pis_data$runnynose=="Yes")))
  result=append(result, nrow(subset(pis_data, pis_data$runnynose=="Yes"))/nrow(!is.na(pis_data))*100)
  result=append(result, nrow(subset(pis_data, pis_data$runnynose=="Unknown")))
  result=append(result, nrow(subset(pis_data, pis_data$runnynose=="Unknown"))/nrow(!is.na(pis_data))*100)
  
  result=append(result, nrow(subset(pis_data, pis_data$sob=="Yes")))
  result=append(result, nrow(subset(pis_data, pis_data$sob=="Yes"))/nrow(!is.na(pis_data))*100)
  result=append(result, nrow(subset(pis_data, pis_data$sob=="Unknown")))
  result=append(result, nrow(subset(pis_data, pis_data$sob=="Unknown"))/nrow(!is.na(pis_data))*100)
  
  result=append(result, nrow(subset(pis_data, pis_data$earpain=="Yes")))
  result=append(result, nrow(subset(pis_data, pis_data$earpain=="Yes"))/nrow(!is.na(pis_data))*100)
  result=append(result, nrow(subset(pis_data, pis_data$earpain=="Unknown")))
  result=append(result, nrow(subset(pis_data, pis_data$earpain=="Unknown"))/nrow(!is.na(pis_data))*100)
  
  result=append(result, nrow(subset(pis_data, pis_data$wheeze=="Yes")))
  result=append(result, nrow(subset(pis_data, pis_data$wheeze=="Yes"))/nrow(!is.na(pis_data))*100)
  result=append(result, nrow(subset(pis_data, pis_data$wheeze=="Unknown")))
  result=append(result, nrow(subset(pis_data, pis_data$wheeze=="Unknown"))/nrow(!is.na(pis_data))*100)
  
  result=append(result, nrow(subset(pis_data, pis_data$chestpain=="Yes")))
  result=append(result, nrow(subset(pis_data, pis_data$chestpain=="Yes"))/nrow(!is.na(pis_data))*100)
  result=append(result, nrow(subset(pis_data, pis_data$chestpain=="Unknown")))
  result=append(result, nrow(subset(pis_data, pis_data$chestpain=="Unknown"))/nrow(!is.na(pis_data))*100)
  
  result=append(result, nrow(subset(pis_data, pis_data$muscleaches=="Yes")))
  result=append(result, nrow(subset(pis_data, pis_data$muscleaches=="Yes"))/nrow(!is.na(pis_data))*100)
  result=append(result, nrow(subset(pis_data, pis_data$muscleaches=="Unknown")))
  result=append(result, nrow(subset(pis_data, pis_data$muscleaches=="Unknown"))/nrow(!is.na(pis_data))*100)
  
  result=append(result, nrow(subset(pis_data, pis_data$headache=="Yes")))
  result=append(result, nrow(subset(pis_data, pis_data$headache=="Yes"))/nrow(!is.na(pis_data))*100)
  result=append(result, nrow(subset(pis_data, pis_data$headache=="Unknown")))
  result=append(result, nrow(subset(pis_data, pis_data$headache=="Unknown"))/nrow(!is.na(pis_data))*100)
  
  result=append(result, nrow(subset(pis_data, pis_data$aloc=="Yes")))
  result=append(result, nrow(subset(pis_data, pis_data$aloc=="Yes"))/nrow(!is.na(pis_data))*100)
  result=append(result, nrow(subset(pis_data, pis_data$aloc=="Unknown")))
  result=append(result, nrow(subset(pis_data, pis_data$aloc=="Unknown"))/nrow(!is.na(pis_data))*100)
  
  result=append(result, nrow(subset(pis_data, pis_data$losstaste=="Yes")))
  result=append(result, nrow(subset(pis_data, pis_data$losstaste=="Yes"))/nrow(!is.na(pis_data))*100)
  result=append(result, nrow(subset(pis_data, pis_data$losstaste=="Unknown")))
  result=append(result, nrow(subset(pis_data, pis_data$losstaste=="Unknown"))/nrow(!is.na(pis_data))*100)
  
  result=append(result, nrow(subset(pis_data, pis_data$abdominalpain=="Yes")))
  result=append(result, nrow(subset(pis_data, pis_data$abdominalpain=="Yes"))/nrow(!is.na(pis_data))*100)
  result=append(result, nrow(subset(pis_data, pis_data$abdominalpain=="Unknown")))
  result=append(result, nrow(subset(pis_data, pis_data$abdominalpain=="Unknown"))/nrow(!is.na(pis_data))*100)
  
  result=append(result, nrow(subset(pis_data, pis_data$nausea=="Yes")))
  result=append(result, nrow(subset(pis_data, pis_data$nausea=="Yes"))/nrow(!is.na(pis_data))*100)
  result=append(result, nrow(subset(pis_data, pis_data$nausea=="Unknown")))
  result=append(result, nrow(subset(pis_data, pis_data$nausea=="Unknown"))/nrow(!is.na(pis_data))*100)
  
  result=append(result, nrow(subset(pis_data, pis_data$diarrhea=="Yes")))
  result=append(result, nrow(subset(pis_data, pis_data$diarrhea=="Yes"))/nrow(!is.na(pis_data))*100)
  result=append(result, nrow(subset(pis_data, pis_data$diarrhea=="Unknown")))
  result=append(result, nrow(subset(pis_data, pis_data$diarrhea=="Unknown"))/nrow(!is.na(pis_data))*100)
  
  result=append(result, nrow(subset(pis_data, pis_data$skinrash=="Yes")))
  result=append(result, nrow(subset(pis_data, pis_data$skinrash=="Yes"))/nrow(!is.na(pis_data))*100)
  result=append(result, nrow(subset(pis_data, pis_data$skinrash=="Unknown")))
  result=append(result, nrow(subset(pis_data, pis_data$skinrash=="Unknown"))/nrow(!is.na(pis_data))*100)
  
  result=append(result, nrow(subset(pis_data, pis_data$hemoptysis=="Yes")))
  result=append(result, nrow(subset(pis_data, pis_data$hemoptysis=="Yes"))/nrow(!is.na(pis_data))*100)
  result=append(result, nrow(subset(pis_data, pis_data$hemoptysis=="Unknown")))
  result=append(result, nrow(subset(pis_data, pis_data$hemoptysis=="Unknown"))/nrow(!is.na(pis_data))*100)
  
  result=append(result, nrow(subset(pis_data, pis_data$travel_canada=="Yes")))
  result=append(result, nrow(subset(pis_data, pis_data$travel_canada=="Yes"))/nrow(!is.na(pis_data))*100)
  result=append(result, nrow(subset(pis_data, pis_data$travel_canada=="Unknown")))
  result=append(result, nrow(subset(pis_data, pis_data$travel_canada=="Unknown"))/nrow(!is.na(pis_data))*100)
  
  result=append(result, nrow(subset(pis_data, pis_data$travel_intl=="Yes")))
  result=append(result, nrow(subset(pis_data, pis_data$travel_intl=="Yes"))/nrow(!is.na(pis_data))*100)
  result=append(result, nrow(subset(pis_data, pis_data$travel_intl=="Unknown")))
  result=append(result, nrow(subset(pis_data, pis_data$travel_intl=="Unknown"))/nrow(!is.na(pis_data))*100)
  
  result=append(result, nrow(subset(pis_data, !is.na(pis_data$hospital_admit))))
  result=append(result, nrow(subset(pis_data, !is.na(pis_data$hospital_admit)))/nrow(!is.na(pis_data))*100)
  result=append(result, nrow(subset(pis_data, is.na(pis_data$hospital_admit))))
  result=append(result, nrow(subset(pis_data, is.na(pis_data$hospital_admit)))/nrow(!is.na(pis_data))*100)
                
  result=append(result, nrow(subset(pis_data, pis_data$where_admit=="Intensive Care Unit")))
  result=append(result, nrow(subset(pis_data, pis_data$where_admit=="Intensive Care Unit"))/nrow(!is.na(pis_data))*100)
  result=append(result, nrow(subset(pis_data, is.na(pis_data$hospital_admit))))
  result=append(result, nrow(subset(pis_data, is.na(pis_data$hospital_admit)))/nrow(!is.na(pis_data))*100)
  
  
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
                    "statin","statin_%","metformin","metformin_%","acetamin","acetamin_%","verapamil","verapamil_%","dur_of_illness(d)","dur_of_illness_sd","fever","fever_%",
                    "unknown_fever","unknown_fever_%","joint_pain","joint_pain_%","unknown_joint_pain","unknown_joint_pain_%","cough","cough_%","unknown_cough","unknown_cough_%",
                    "fatigue","fatigue_%","unknown_fatigue","unknown_fatigue_%","sorethroat","sorethroat_%","unknown_sorethroat","unknown_sorethroat_%","runnynose","runnynose_%",
                    "unknown_runnynose","unknown_runnynose_%","sob","sob_%","unknown_sob","unknown_sob_%","earpain","earpain_%","unknown_earpain","unknown_earpain_%",
                    "wheeze","wheeze_%","unknown_wheeze","unknown_wheeze_%","chestpain","chestpain_%","unknown_chestpain","unknown_chestpain_%",
                    "muscleaches","muscleaches_%","unknown_muscleaches","unknown_muscleaches_%","h/a","h/a_%","unknown_h/a","unknown_h/a_%",
                    "aloc","aloc_%","unknown_aloc","unknown_aloc_%","losstaste","losstaste_%","unknown_losstaste","unknown_losstaste_%",
                    "abpain","abpain_%","unknown_abpain","unknown_abpain_%","nausea","nausea_%","unknown_nausea","unknown_nausea_%",
                    "diarrhea","diarrhea_%","unknown_diarrhea","unknown_diarrhea_%","skinrash","skinrash_%","unknown_skinrash","unknown_skinrash_%",
                    "hemoptysis","hemoptysis_%","unknown_hemoptysis","unknown_hemoptysis_%","travel_canada","travel_canada_%","unknown_travel_canada","unknown_travel_canada_%",
                    "travel_intl","travel_intl_%","unknown_travel_intl","unknown_travel_intl_%","admission","admission_%","NA_admission","NA_admission_%","ICU","ICU_%",
                    "unknown_ICU","unknown_ICU_%"
                    )
# Table 1 export ----
#write.csv(table_1, "C:/Users/alexw/Google Drive/Desktop files/Dal Med/Med3/TripleC/table_1.csv")

# Table 1 stats ----
table1stats=c()

pis_group=subset(data, data$pis_status=="Yes")
no_pis_group=subset(data, data$pis_status=="No")

table1stats=append(table1stats, t.test(age_years ~ pis_status, data=data)$p.value)

M=as.table(cbind(c(nrow(subset(pis_group, pis_group$sex=="Male")),nrow(subset(pis_group, pis_group$sex=="Female"))),
                 c(nrow(subset(no_pis_group, no_pis_group$sex=="M")),nrow(subset(no_pis_group, no_pis_group$sex=="Female")))
                 ))
table1stats=append(table1stats,chisq.test(M)$p.value)

M=as.table(cbind(c(nrow(subset(pis_group, pis_group$ethnic=="White/Caucasian")),nrow(subset(pis_group, pis_group$ethnic=="East Asian")),nrow(subset(pis_group, pis_group$ethnic=="Black")),nrow(subset(pis_group, pis_group$ethnic=="Other"))),
                 c(nrow(subset(no_pis_group, no_pis_group$ethnic=="White/Caucasian")),nrow(subset(no_pis_group, no_pis_group$ethnic=="East Asian")),nrow(subset(pis_group, pis_group$ethnic=="Black")),nrow(subset(pis_group, pis_group$ethnic=="Other")))
))
table1stats=append(table1stats,fisher.test(M)$p.value)

M=as.table(cbind(c(nrow(subset(pis_group, pis_group$housing=="Rooming house")),nrow(subset(pis_group, pis_group$housing=="Apartment")),nrow(subset(pis_group, pis_group$housing=="Townhome/Semi-detatched")),nrow(subset(pis_group, pis_group$housing=="Detatched house"))),
                 c(nrow(subset(no_pis_group, no_pis_group$housing=="Rooming house")),nrow(subset(no_pis_group, no_pis_group$housing=="Apartment")),nrow(subset(pis_group, pis_group$housing=="Townhome/Semi-detatched")),nrow(subset(pis_group, pis_group$housing=="Detatched house")))
))
table1stats=append(table1stats,fisher.test(M)$p.value)

M=as.table(cbind(c(nrow(subset(pis_group, pis_group$income=="Low Income (<= $35,999)")),nrow(subset(pis_group, pis_group$income=="Middle Income ($36-60,000)")),nrow(subset(pis_group, pis_group$income=="High Income (> $60,000)"))),
                 c(nrow(subset(no_pis_group, no_pis_group$income=="Low Income (<= $35,999)")),nrow(subset(no_pis_group, no_pis_group$income=="Middle Income ($36-60,000)")),nrow(subset(pis_group, pis_group$income=="High Income (> $60,000)")))
))
table1stats=append(table1stats,fisher.test(M)$p.value)

M=as.table(cbind(c(nrow(subset(pis_group, pis_group$cardiac=="Yes")),nrow(subset(pis_group, pis_group$cardiac=="No"))),
                 c(nrow(subset(no_pis_group, no_pis_group$cardiac=="Yes")),nrow(subset(no_pis_group, no_pis_group$cardiac=="No")))
))
table1stats=append(table1stats,fisher.test(M)$p.value)

M=as.table(cbind(c(nrow(subset(pis_group, pis_group$pulmonary=="Yes")),nrow(subset(pis_group, pis_group$pulmonary=="No"))),
                 c(nrow(subset(no_pis_group, no_pis_group$pulmonary=="Yes")),nrow(subset(no_pis_group, no_pis_group$pulmonary=="No")))
))
table1stats=append(table1stats,fisher.test(M)$p.value)

M=as.table(cbind(c(nrow(subset(pis_group, pis_group$hypertension=="Yes")),nrow(subset(pis_group, pis_group$hypertension=="No"))),
                 c(nrow(subset(no_pis_group, no_pis_group$hypertension=="Yes")),nrow(subset(no_pis_group, no_pis_group$hypertension=="No")))
))
table1stats=append(table1stats,chisq.test(M)$p.value)

M=as.table(cbind(c(nrow(subset(pis_group, pis_group$asthma=="Yes")),nrow(subset(pis_group, pis_group$asthma=="No"))),
                 c(nrow(subset(no_pis_group, no_pis_group$asthma=="Yes")),nrow(subset(no_pis_group, no_pis_group$asthma=="No")))
))
table1stats=append(table1stats,fisher.test(M)$p.value)

M=as.table(cbind(c(nrow(subset(pis_group, pis_group$diabetes1=="Yes" | pis_group$diabetes2=="Yes")),nrow(subset(pis_group, pis_group$diabetes1=="No" | pis_group$diabetes2=="No"))),
                 c(nrow(subset(no_pis_group, no_pis_group$diabetes1=="Yes" | no_pis_group$diabetes2=="Yes")),nrow(subset(no_pis_group, no_pis_group$diabetes1=="No"| no_pis_group$diabetes2=="No")))
))
table1stats=append(table1stats,fisher.test(M)$p.value)

M=as.table(cbind(c(nrow(subset(pis_group, pis_group$liver=="Yes")),nrow(subset(pis_group, pis_group$liver=="No"))),
                 c(nrow(subset(no_pis_group, no_pis_group$liver=="Yes")),nrow(subset(no_pis_group, no_pis_group$liver=="No")))
))
table1stats=append(table1stats,fisher.test(M)$p.value)

M=as.table(cbind(c(nrow(subset(pis_group, pis_group$rheum=="Yes")),nrow(subset(pis_group, pis_group$rheum=="No"))),
                 c(nrow(subset(no_pis_group, no_pis_group$rheum=="Yes")),nrow(subset(no_pis_group, no_pis_group$rheum=="No")))
))
table1stats=append(table1stats,chisq.test(M)$p.value)

M=as.table(cbind(c(nrow(subset(pis_group, pis_group$neuro=="Yes")),nrow(subset(pis_group, pis_group$neuro=="No"))),
                 c(nrow(subset(no_pis_group, no_pis_group$neuro=="Yes")),nrow(subset(no_pis_group, no_pis_group$neuro=="No")))
))
table1stats=append(table1stats,fisher.test(M)$p.value)

M=as.table(cbind(c(nrow(subset(pis_group, pis_group$dementia=="Yes")),nrow(subset(pis_group, pis_group$dementia=="No"))),
                 c(nrow(subset(no_pis_group, no_pis_group$dementia=="Yes")),nrow(subset(no_pis_group, no_pis_group$dementia=="No")))
))
table1stats=append(table1stats,fisher.test(M)$p.value)

M=as.table(cbind(c(nrow(subset(pis_group, pis_group$hematologic=="Yes")),nrow(subset(pis_group, pis_group$hematologic=="No"))),
                 c(nrow(subset(no_pis_group, no_pis_group$hematologic=="Yes")),nrow(subset(no_pis_group, no_pis_group$hematologic=="No")))
))
table1stats=append(table1stats,fisher.test(M)$p.value)

M=as.table(cbind(c(nrow(subset(pis_group, pis_group$malnutrition=="Yes")),nrow(subset(pis_group, pis_group$malnutrition=="No"))),
                 c(nrow(subset(no_pis_group, no_pis_group$malnutrition=="Yes")),nrow(subset(no_pis_group, no_pis_group$malnutrition=="No")))
))
table1stats=append(table1stats,fisher.test(M)$p.value)

M=as.table(cbind(c(nrow(subset(pis_group, pis_group$malignant=="Yes")),nrow(subset(pis_group, pis_group$malignant=="No"))),
                 c(nrow(subset(no_pis_group, no_pis_group$malignant=="Yes")),nrow(subset(no_pis_group, no_pis_group$malignant=="No")))
))
table1stats=append(table1stats,fisher.test(M)$p.value)

M=as.table(cbind(c(nrow(subset(pis_group, pis_group$hiv=="Yes")),nrow(subset(pis_group, pis_group$hiv=="No"))),
                 c(nrow(subset(no_pis_group, no_pis_group$hiv=="Yes")),nrow(subset(no_pis_group, no_pis_group$hiv=="No")))
))
table1stats=append(table1stats,fisher.test(M)$p.value)

M=as.table(cbind(c(nrow(subset(pis_group, pis_group$smoker=="Yes")),nrow(subset(pis_group, pis_group$smoker=="No"))),
                 c(nrow(subset(no_pis_group, no_pis_group$smoker=="Yes")),nrow(subset(no_pis_group, no_pis_group$smoker=="No")))
))
table1stats=append(table1stats,fisher.test(M)$p.value)

M=as.table(cbind(c(nrow(subset(pis_group, pis_group$obesity=="Yes")),nrow(subset(pis_group, pis_group$obesity=="No"))),
                 c(nrow(subset(no_pis_group, no_pis_group$obesity=="Yes")),nrow(subset(no_pis_group, no_pis_group$obesity=="No")))
))
table1stats=append(table1stats,chisq.test(M)$p.value)

M=as.table(cbind(c(nrow(subset(pis_group, pis_group$snore=="Yes")),nrow(subset(pis_group, pis_group$snore=="No"))),
                 c(nrow(subset(no_pis_group, no_pis_group$snore=="Yes")),nrow(subset(no_pis_group, no_pis_group$snore=="No")))
))
table1stats=append(table1stats,chisq.test(M)$p.value)

M=as.table(cbind(c(nrow(subset(pis_group, pis_group$cpap=="Yes")),nrow(subset(pis_group, pis_group$cpap=="No"))),
                 c(nrow(subset(no_pis_group, no_pis_group$cpap=="Yes")),nrow(subset(no_pis_group, no_pis_group$cpap=="No")))
))
table1stats=append(table1stats,fisher.test(M)$p.value)

M=as.table(cbind(c(nrow(subset(pis_group, pis_group$acei!="No" | pis_group$arbs!="No")),nrow(subset(pis_group, pis_group$acei=="No" | pis_group$arbs=="No"))),
                 c(nrow(subset(no_pis_group, no_pis_group$acei!="No" | no_pis_group$arbs!="No")),nrow(subset(no_pis_group, no_pis_group$acei=="No" | no_pis_group$arbs=="No")))
))
table1stats=append(table1stats,chisq.test(M)$p.value)

M=as.table(cbind(c(nrow(subset(pis_group, pis_group$nsaids=="Yes")),nrow(subset(pis_group, pis_group$nsaids=="No"))),
                 c(nrow(subset(no_pis_group, no_pis_group$nsaids=="Yes")),nrow(subset(no_pis_group, no_pis_group$nsaids=="No")))
))
table1stats=append(table1stats,fisher.test(M)$p.value)

M=as.table(cbind(c(nrow(subset(pis_group, pis_group$steroid=="Yes")),nrow(subset(pis_group, pis_group$steroid=="No"))),
                 c(nrow(subset(no_pis_group, no_pis_group$steroid=="Yes")),nrow(subset(no_pis_group, no_pis_group$steroid=="No")))
))
table1stats=append(table1stats,fisher.test(M)$p.value)

M=as.table(cbind(c(nrow(subset(pis_group, pis_group$statin!="No")),nrow(subset(pis_group, pis_group$statin=="No"))),
                 c(nrow(subset(no_pis_group, no_pis_group$statin!="No")),nrow(subset(no_pis_group, no_pis_group$statin=="No")))
))
table1stats=append(table1stats,fisher.test(M)$p.value)

M=as.table(cbind(c(nrow(subset(pis_group, pis_group$metformin=="Yes")),nrow(subset(pis_group, pis_group$metformin=="No"))),
                 c(nrow(subset(no_pis_group, no_pis_group$metformin=="Yes")),nrow(subset(no_pis_group, no_pis_group$metformin=="No")))
))
table1stats=append(table1stats,fisher.test(M)$p.value)

M=as.table(cbind(c(nrow(subset(pis_group, pis_group$acetamin=="Yes")),nrow(subset(pis_group, pis_group$acetamin=="No"))),
                 c(nrow(subset(no_pis_group, no_pis_group$acetamin=="Yes")),nrow(subset(no_pis_group, no_pis_group$acetamin=="No")))
))
table1stats=append(table1stats,chisq.test(M)$p.value)

M=as.table(cbind(c(nrow(subset(pis_group, pis_group$verapamil=="Yes")),nrow(subset(pis_group, pis_group$verapamil=="No"))),
                 c(nrow(subset(no_pis_group, no_pis_group$verapamil=="Yes")),nrow(subset(no_pis_group, no_pis_group$verapamil=="No")))
))
table1stats=append(table1stats,fisher.test(M)$p.value)

table1stats=append(table1stats, t.test(duration_of_illness ~ pis_status, data=data)$p.value)

M=as.table(cbind(c(nrow(subset(pis_group, pis_group$fever_documented=="Yes")),nrow(subset(pis_group, pis_group$fever_documented=="No"))),
                 c(nrow(subset(no_pis_group, no_pis_group$fever_documented=="Yes")),nrow(subset(no_pis_group, no_pis_group$fever_documented=="No")))
))
table1stats=append(table1stats,chisq.test(M)$p.value)

M=as.table(cbind(c(nrow(subset(pis_group, pis_group$jointpain=="Yes")),nrow(subset(pis_group, pis_group$jointpain=="No"))),
                 c(nrow(subset(no_pis_group, no_pis_group$jointpain=="Yes")),nrow(subset(no_pis_group, no_pis_group$jointpain=="No")))
))
table1stats=append(table1stats,chisq.test(M)$p.value)

M=as.table(cbind(c(nrow(subset(pis_group, pis_group$cough=="Yes")),nrow(subset(pis_group, pis_group$cough=="No"))),
                 c(nrow(subset(no_pis_group, no_pis_group$cough=="Yes")),nrow(subset(no_pis_group, no_pis_group$cough=="No")))
))
table1stats=append(table1stats,chisq.test(M)$p.value)

M=as.table(cbind(c(nrow(subset(pis_group, pis_group$fatigue=="Yes")),nrow(subset(pis_group, pis_group$fatigue=="No"))),
                 c(nrow(subset(no_pis_group, no_pis_group$fatigue=="Yes")),nrow(subset(no_pis_group, no_pis_group$fatigue=="No")))
))
table1stats=append(table1stats,chisq.test(M)$p.value)

M=as.table(cbind(c(nrow(subset(pis_group, pis_group$sorethroat=="Yes")),nrow(subset(pis_group, pis_group$sorethroat=="No"))),
                 c(nrow(subset(no_pis_group, no_pis_group$sorethroat=="Yes")),nrow(subset(no_pis_group, no_pis_group$sorethroat=="No")))
))
table1stats=append(table1stats,chisq.test(M)$p.value)

M=as.table(cbind(c(nrow(subset(pis_group, pis_group$runnynose=="Yes")),nrow(subset(pis_group, pis_group$runnynose=="No"))),
                 c(nrow(subset(no_pis_group, no_pis_group$runnynose=="Yes")),nrow(subset(no_pis_group, no_pis_group$runnynose=="No")))
))
table1stats=append(table1stats,chisq.test(M)$p.value)

M=as.table(cbind(c(nrow(subset(pis_group, pis_group$sob=="Yes")),nrow(subset(pis_group, pis_group$sob=="No"))),
                 c(nrow(subset(no_pis_group, no_pis_group$sob=="Yes")),nrow(subset(no_pis_group, no_pis_group$sob=="No")))
))
table1stats=append(table1stats,chisq.test(M)$p.value)

M=as.table(cbind(c(nrow(subset(pis_group, pis_group$earpain=="Yes")),nrow(subset(pis_group, pis_group$earpain=="No"))),
                 c(nrow(subset(no_pis_group, no_pis_group$earpain=="Yes")),nrow(subset(no_pis_group, no_pis_group$earpain=="No")))
))
table1stats=append(table1stats,fisher.test(M)$p.value)

M=as.table(cbind(c(nrow(subset(pis_group, pis_group$wheezing=="Yes")),nrow(subset(pis_group, pis_group$wheezing=="No"))),
                 c(nrow(subset(no_pis_group, no_pis_group$wheezing=="Yes")),nrow(subset(no_pis_group, no_pis_group$wheezing=="No")))
))
table1stats=append(table1stats,fisher.test(M)$p.value)

M=as.table(cbind(c(nrow(subset(pis_group, pis_group$chestpain=="Yes")),nrow(subset(pis_group, pis_group$chestpain=="No"))),
                 c(nrow(subset(no_pis_group, no_pis_group$chestpain=="Yes")),nrow(subset(no_pis_group, no_pis_group$chestpain=="No")))
))
table1stats=append(table1stats,chisq.test(M)$p.value)

M=as.table(cbind(c(nrow(subset(pis_group, pis_group$muscleaches=="Yes")),nrow(subset(pis_group, pis_group$muscleaches=="No"))),
                 c(nrow(subset(no_pis_group, no_pis_group$muscleaches=="Yes")),nrow(subset(no_pis_group, no_pis_group$muscleaches=="No")))
))
table1stats=append(table1stats,chisq.test(M)$p.value)

M=as.table(cbind(c(nrow(subset(pis_group, pis_group$headache=="Yes")),nrow(subset(pis_group, pis_group$headache=="No"))),
                 c(nrow(subset(no_pis_group, no_pis_group$headache=="Yes")),nrow(subset(no_pis_group, no_pis_group$headache=="No")))
))
table1stats=append(table1stats,chisq.test(M)$p.value)

M=as.table(cbind(c(nrow(subset(pis_group, pis_group$aloc=="Yes")),nrow(subset(pis_group, pis_group$aloc=="No"))),
                 c(nrow(subset(no_pis_group, no_pis_group$aloc=="Yes")),nrow(subset(no_pis_group, no_pis_group$aloc=="No")))
))
table1stats=append(table1stats,fisher.test(M)$p.value)

M=as.table(cbind(c(nrow(subset(pis_group, pis_group$losstaste=="Yes")),nrow(subset(pis_group, pis_group$losstaste=="No"))),
                 c(nrow(subset(no_pis_group, no_pis_group$losstaste=="Yes")),nrow(subset(no_pis_group, no_pis_group$losstaste=="No")))
))
table1stats=append(table1stats,chisq.test(M)$p.value)

M=as.table(cbind(c(nrow(subset(pis_group, pis_group$abdominalpain=="Yes")),nrow(subset(pis_group, pis_group$abdominalpain=="No"))),
                 c(nrow(subset(no_pis_group, no_pis_group$abdominalpain=="Yes")),nrow(subset(no_pis_group, no_pis_group$abdominalpain=="No")))
))
table1stats=append(table1stats,fisher.test(M)$p.value)

M=as.table(cbind(c(nrow(subset(pis_group, pis_group$nausea=="Yes")),nrow(subset(pis_group, pis_group$nausea=="No"))),
                 c(nrow(subset(no_pis_group, no_pis_group$nausea=="Yes")),nrow(subset(no_pis_group, no_pis_group$nausea=="No")))
))
table1stats=append(table1stats,chisq.test(M)$p.value)

M=as.table(cbind(c(nrow(subset(pis_group, pis_group$diarrhea=="Yes")),nrow(subset(pis_group, pis_group$diarrhea=="No"))),
                 c(nrow(subset(no_pis_group, no_pis_group$diarrhea=="Yes")),nrow(subset(no_pis_group, no_pis_group$diarrhea=="No")))
))
table1stats=append(table1stats,chisq.test(M)$p.value)

M=as.table(cbind(c(nrow(subset(pis_group, pis_group$skinrash=="Yes")),nrow(subset(pis_group, pis_group$skinrash=="No"))),
                 c(nrow(subset(no_pis_group, no_pis_group$skinrash=="Yes")),nrow(subset(no_pis_group, no_pis_group$skinrash=="No")))
))
table1stats=append(table1stats,fisher.test(M)$p.value)

M=as.table(cbind(c(nrow(subset(pis_group, pis_group$hemoptysis=="Yes")),nrow(subset(pis_group, pis_group$hemoptysis=="No"))),
                 c(nrow(subset(no_pis_group, no_pis_group$hemoptysis=="Yes")),nrow(subset(no_pis_group, no_pis_group$hemoptysis=="No")))
))
table1stats=append(table1stats,fisher.test(M)$p.value)

M=as.table(cbind(c(nrow(subset(pis_group, pis_group$travel_canada=="Yes")),nrow(subset(pis_group, pis_group$travel_canada=="No"))),
                 c(nrow(subset(no_pis_group, no_pis_group$travel_canada=="Yes")),nrow(subset(no_pis_group, no_pis_group$travel_canada=="No")))
))
table1stats=append(table1stats,chisq.test(M)$p.value)

M=as.table(cbind(c(nrow(subset(pis_group, pis_group$travel_intl=="Yes")),nrow(subset(pis_group, pis_group$travel_intl=="No"))),
                 c(nrow(subset(no_pis_group, no_pis_group$travel_intl=="Yes")),nrow(subset(no_pis_group, no_pis_group$travel_intl=="No")))
))
table1stats=append(table1stats,chisq.test(M)$p.value)

M=as.table(cbind(c(nrow(subset(pis_group, !is.na(pis_group$admit_date))),nrow(subset(pis_group, is.na(pis_group$admit_date)))),
                 c(nrow(subset(no_pis_group, !is.na(no_pis_group$admit_date))),nrow(subset(no_pis_group, is.na(no_pis_group$admit_date))))
))
table1stats=append(table1stats,fisher.test(M)$p.value)

M=as.table(cbind(c(nrow(subset(pis_group, pis_group$where_admit=="Intensive Care Unit")),nrow(subset(pis_group, pis_group$where_admit!="Intensive Care Unit"))),
                 c(nrow(subset(no_pis_group, no_pis_group$where_admit=="Intensive Care Unit")),nrow(subset(no_pis_group, no_pis_group$where_admit!="Intensive Care Unit")))
))
table1stats=append(table1stats,fisher.test(M)$p.value)


table1stats=as.data.frame(table1stats)
colnames(table1stats)=c("p-value")

rownames(table1stats)=c("age","sex","ethnicity (fe)","housing (fe)","income (fe)", "cardiac (Fe)","pulmonary (fe)","hypertension","asthma (fe)","diabetes (fe)",
                        "liver(fe)","rheum","neuro (fe)","dementia (fe)","hematologic (fe)","malnutrition (fe)", "malignancy (fe)","hiv (fe)","smoker (fe)","obesity",
                        "snore","cpap (fe)","acei/arb","nsaids (fe)","steroid (fe)","statin (fe)","metformin (fe)","acetaminophen","verapamil (Fe)",
                        "duration_of_illness","fever","joint pain","cough","fatigue","sorethroat","runny nose","sob","earpain (fe)","wheeze (fe)","c/p","muscle ache",
                        "h/a","aloc (fe)","losstaste","abpain (fe)","nausea","diarrhea","skinrash (fe)","hemoptysis (fe)","travel canada","travel int",
                        "admission (fe)","icu (fe)")
# Table 1 stats export----
#write.csv(table1stats, "C:/Users/alexw/Google Drive/Desktop files/Dal Med/Med3/TripleC/table_1_stats.csv")

# Table 2 ----

table_2=c()

table_2=append(table_2, nrow(subset(data, data$physicalpostcovid=="Yes" | data$mhpostcovid=="Yes")))

table_2=append(table_2, nrow(subset(data, data$post_recovery_sob_1==1)))
table_2=append(table_2, nrow(subset(data, data$post_recovery_sob_1==1))/nrow(subset(data, data$physicalpostcovid=="Yes" | data$mhpostcovid=="Yes"))*100)

table_2=append(table_2, nrow(subset(data, data$post_recovery_fatigue_1==1)))
table_2=append(table_2, nrow(subset(data, data$post_recovery_fatigue_1==1))/nrow(subset(data, data$physicalpostcovid=="Yes" | data$mhpostcovid=="Yes"))*100)

table_2=append(table_2, nrow(subset(data, data$post_recovery_anosmia_ageusia_1==1)))
table_2=append(table_2, nrow(subset(data, data$post_recovery_anosmia_ageusia_1==1))/nrow(subset(data, data$physicalpostcovid=="Yes" | data$mhpostcovid=="Yes"))*100)

table_2=append(table_2, nrow(subset(data, data$post_recovery_hair_loss_1==1)))
table_2=append(table_2, nrow(subset(data, data$post_recovery_hair_loss_1==1))/nrow(subset(data, data$physicalpostcovid=="Yes" | data$mhpostcovid=="Yes"))*100)

table_2=append(table_2, nrow(subset(data, data$post_recovery_brain_fog_1==1)))
table_2=append(table_2, nrow(subset(data, data$post_recovery_brain_fog_1==1))/nrow(subset(data, data$physicalpostcovid=="Yes" | data$mhpostcovid=="Yes"))*100)

table_2=append(table_2, nrow(subset(data, data$post_recovery_weight_loss_1==1)))
table_2=append(table_2, nrow(subset(data, data$post_recovery_weight_loss_1==1))/nrow(subset(data, data$physicalpostcovid=="Yes" | data$mhpostcovid=="Yes"))*100)

table_2=append(table_2, nrow(subset(data, data$post_recovery_arthralgia_myalgia_1==1)))
table_2=append(table_2, nrow(subset(data, data$post_recovery_arthralgia_myalgia_1==1))/nrow(subset(data, data$physicalpostcovid=="Yes" | data$mhpostcovid=="Yes"))*100)

table_2=append(table_2, nrow(subset(data, data$post_recovery_headache_1==1)))
table_2=append(table_2, nrow(subset(data, data$post_recovery_headache_1==1))/nrow(subset(data, data$physicalpostcovid=="Yes" | data$mhpostcovid=="Yes"))*100)

table_2=append(table_2, nrow(subset(data, data$post_recovery_diarrhea_1==1)))
table_2=append(table_2, nrow(subset(data, data$post_recovery_diarrhea_1==1))/nrow(subset(data, data$physicalpostcovid=="Yes" | data$mhpostcovid=="Yes"))*100)

table_2=append(table_2, nrow(subset(data, data$post_recovery_cough_1==1)))
table_2=append(table_2, nrow(subset(data, data$post_recovery_cough_1==1))/nrow(subset(data, data$physicalpostcovid=="Yes" | data$mhpostcovid=="Yes"))*100)

table_2=append(table_2, nrow(subset(data, data$post_recovery_fever_1==1)))
table_2=append(table_2, nrow(subset(data, data$post_recovery_fever_1==1))/nrow(subset(data, data$physicalpostcovid=="Yes" | data$mhpostcovid=="Yes"))*100)

table_2=append(table_2, nrow(subset(data, data$post_recovery_dizziness_1==1)))
table_2=append(table_2, nrow(subset(data, data$post_recovery_dizziness_1==1))/nrow(subset(data, data$physicalpostcovid=="Yes" | data$mhpostcovid=="Yes"))*100)

table_2=append(table_2, nrow(subset(data, data$post_recovery_anxiety_1==1)))
table_2=append(table_2, nrow(subset(data, data$post_recovery_anxiety_1==1))/nrow(subset(data, data$physicalpostcovid=="Yes" | data$mhpostcovid=="Yes"))*100)

table_2=append(table_2, nrow(subset(data, data$post_recovery_other_1==1)))
table_2=append(table_2, nrow(subset(data, data$post_recovery_other_1==1))/nrow(subset(data, data$physicalpostcovid=="Yes" | data$mhpostcovid=="Yes"))*100)


table_2=as.data.frame(table_2)
colnames(table_2)=c("post recovery symptoms")
rownames(table_2)=c("n at 1 month","sob", "sob_%","fatigue","fatigue_%","anosmia","anosmia_%","hair loss","hair loss_%","brain fog","brain fog_%","w/l","w/l_%",
                    "arth/myalgia","arth/myalgia_%","h/a","h/a_%","diarrhea","diarrhea_%","cough","cough_%","fever","fever_%","dizziness","dizziness_%",
                    "anxiety","anxiety_%","other","other_%")

# Table 2 export ----
#write.csv(table_2, "C:/Users/alexw/Google Drive/Desktop files/Dal Med/Med3/TripleC/table_2.csv")




























