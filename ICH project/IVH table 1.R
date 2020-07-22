library(googlesheets4)
library(googledrive)
library(irr)
library(stringr)
library(blandr)
library(ggplot2)
library(cowplot)
library(data.table)
library(stringr)
library(tidyverse)
library(chron)
# Data Consolidation ----
# PREDICT
predict_enrolled=readxl::read_xls("D:/Google Drive/Desktop files/Dal Med/ICH/PREDICTprimaryV3_TH.xls", sheet="enrolled")
predict_enrolled=as.data.frame(predict_enrolled)

predict_enrolled$patient_site=str_pad(predict_enrolled$patient_site, 2, pad="0")
predict_enrolled$patient_study=str_pad(predict_enrolled$patient_study, 3, pad="0")
predict_enrolled$PID=paste(predict_enrolled$patient_site, predict_enrolled$patient_study, sep="")

for_gs <- read_sheet("1c44iNPzrDiZ0DzBb1eAggm3VBd5ImMEnsFcWgzcZWC0", sheet="PREDICT V7 TOTAL")
for_gs=as.data.frame(for_gs)
predict_enrolled=subset(predict_enrolled, predict_enrolled$PID %in% subset(for_gs, for_gs$`IVH Present`==1)$PID)
predict_enrolled$study=c("Predict")

for_gs=for_gs[duplicated(for_gs[,c("PID")]),]
predict_enrolled$bl_fu=c(0)
predict_enrolled$bl_fu[predict_enrolled$PID %in% for_gs$PID] = 1

# SPOTLIGHT/STOP-IT

SL_SI_enrolled=readxl::read_xlsx("D:/Google Drive/Desktop files/Dal Med/ICH/Archive/GraebCombined_withmRS (Recovered).xlsx", sheet="ALL")
SL_SI_enrolled=as.data.frame(SL_SI_enrolled)
colnames(SL_SI_enrolled)[colnames(SL_SI_enrolled) == "Subject"]="PID"
for_gs <- read_sheet("1c44iNPzrDiZ0DzBb1eAggm3VBd5ImMEnsFcWgzcZWC0", sheet="SPOTLIGHT/STOP-IT")
for_gs=as.data.frame(for_gs)
for_gs$PID=str_pad(for_gs$PID, 7, pad="0")
SL_SI_enrolled=subset(SL_SI_enrolled, SL_SI_enrolled$PID %in% subset(for_gs, for_gs$`IVH Present`==1)$PID)

for_gs_study=for_gs[c("PID","Study")]
for_gs_study=for_gs_study[!duplicated(for_gs_study[,c("PID")]),]
SL_SI_enrolled=merge(SL_SI_enrolled, for_gs_study, by="PID", all.x=TRUE)

for_gs=for_gs[duplicated(for_gs[,c("PID")]),]
SL_SI_enrolled$bl_fu=c(0)
SL_SI_enrolled$bl_fu[SL_SI_enrolled$PID %in% for_gs$PID] = 1

# Rename cols ----

# PREDICT

colnames(predict_enrolled)[colnames(predict_enrolled) == "ageatevent"] = "age"

#gender

colnames(predict_enrolled)[colnames(predict_enrolled) == "hypertension"] = "htn"

colnames(predict_enrolled)[colnames(predict_enrolled) == "diabetes"] = "dm"

colnames(predict_enrolled)[colnames(predict_enrolled) == "high_cholesterol"] = "hpl"

colnames(predict_enrolled)[colnames(predict_enrolled) == "priorICH"] = "prev_ich"

colnames(predict_enrolled)[colnames(predict_enrolled) == "on_warfarin"] = "warfarin"

colnames(predict_enrolled)[colnames(predict_enrolled) == "factorVIIa"] = "rfVIIa"

#predict_enrolled$`Event date/time` <- as.POSIXct(predict_enrolled$`Event date/time`, format = "%Y-%m-%d %H:%M:%S")
#colnames(predict_enrolled)[colnames(predict_enrolled) == "Event date/time"] = "dt_event"

hour=as.numeric(substr(predict_enrolled$`event-baselineCT`, 1, 1))
min=as.numeric(substr(predict_enrolled$`event-baselineCT`, 4, 5))
hr_min=as.data.frame(hour + min/60)
colnames(hr_min)=c("t_from_onset_to_ct")
predict_enrolled=cbind(predict_enrolled, hr_min)

# SPOTLIGHT/STOP-IT
colnames(SL_SI_enrolled)[colnames(SL_SI_enrolled) == "Study"] = "study"

colnames(SL_SI_enrolled)[colnames(SL_SI_enrolled) == "AGE_AT_ARRIVAL"] = "age"

colnames(SL_SI_enrolled)[colnames(SL_SI_enrolled) == "GENDER"] = "gender"

colnames(SL_SI_enrolled)[colnames(SL_SI_enrolled) == "MED_HIST_HYPERTENSION"] = "htn"
SL_SI_enrolled$htn[SL_SI_enrolled$htn==3]=NA
SL_SI_enrolled$htn[SL_SI_enrolled$htn==2]=0

colnames(SL_SI_enrolled)[colnames(SL_SI_enrolled) == "MED_HIST_DIABETES"] = "dm"
SL_SI_enrolled$dm[SL_SI_enrolled$dm==3]=NA
SL_SI_enrolled$dm[SL_SI_enrolled$dm==2]=0

colnames(SL_SI_enrolled)[colnames(SL_SI_enrolled) == "MED_HIST_HYPERLIPID"] = "hpl"
SL_SI_enrolled$hpl[SL_SI_enrolled$hpl==3]=NA
SL_SI_enrolled$hpl[SL_SI_enrolled$hpl==2]=0

colnames(SL_SI_enrolled)[colnames(SL_SI_enrolled) == "MED_HIST_INTRACEREB_HEM"] = "prev_ich"
SL_SI_enrolled$prev_ich[SL_SI_enrolled$prev_ich==3]=NA
SL_SI_enrolled$prev_ich[SL_SI_enrolled$prev_ich==2]=0

SL_SI_enrolled$warfarin=c(0)

colnames(SL_SI_enrolled)[colnames(SL_SI_enrolled) == "Drug received"] = "rfVIIa"
SL_SI_enrolled$rfVIIa[is.na(SL_SI_enrolled$rfVIIa)]=0

#t_from_onset_to_ct

keep_cols=c("PID","age","gender","htn","dm","hpl","prev_ich","warfarin","rfVIIa","t_from_onset_to_ct","bl_fu","study")
predict_enrolled=predict_enrolled[keep_cols]
SL_SI_enrolled=SL_SI_enrolled[keep_cols]

data=rbind(predict_enrolled, SL_SI_enrolled)



# Table 1 ----

studys=c("Predict","SPOTLIGHT")
for (study in studys){
  if (study=="Predict"){
    subdata=subset(data, data$study=="Predict")
  } else if (study=="SPOTLIGHT") {
      subdata=subset(data, data$study=="SPOTLIGHT" | data$study== "STOP-IT")
  }
  
  column=c()
  
  column=append(column, nrow(subset(subdata)))
  
  column=append(column, mean(subdata$age, na.rm=TRUE))
  column=append(column, sd(subdata$age, na.rm=TRUE))
  column=append(column, nrow(subset(subdata, is.na(subdata$age))))
  
  column=append(column, nrow(subset(subdata, subdata$gender==2)))
  column=append(column, nrow(subset(subdata, subdata$gender==2))/nrow(subset(subdata, !is.na(subdata$gender)))*100)
  column=append(column, nrow(subset(subdata, is.na(subdata$gender))))
  
  column=append(column, nrow(subset(subdata, subdata$htn==1)))
  column=append(column, nrow(subset(subdata, subdata$htn==1))/nrow(subset(subdata, !is.na(subdata$htn)))*100)
  column=append(column, nrow(subset(subdata, is.na(subdata$htn))))
  
  column=append(column, nrow(subset(subdata, subdata$dm==1)))
  column=append(column, nrow(subset(subdata, subdata$dm==1))/nrow(subset(subdata, !is.na(subdata$dm)))*100)
  column=append(column, nrow(subset(subdata, is.na(subdata$dm))))
  
  column=append(column, nrow(subset(subdata, subdata$hpl==1)))
  column=append(column, nrow(subset(subdata, subdata$hpl==1))/nrow(subset(subdata, !is.na(subdata$hpl)))*100)
  column=append(column, nrow(subset(subdata, is.na(subdata$hpl))))
  
  column=append(column, nrow(subset(subdata, subdata$prev_ich==1)))
  column=append(column, nrow(subset(subdata, subdata$prev_ich==1))/nrow(subset(subdata, !is.na(subdata$prev_ich)))*100)
  column=append(column, nrow(subset(subdata, is.na(subdata$prev_ich))))
  
  column=append(column, nrow(subset(subdata, subdata$warfarin==1)))
  column=append(column, nrow(subset(subdata, subdata$warfarin==1))/nrow(subset(subdata, !is.na(subdata$warfarin)))*100)
  column=append(column, nrow(subset(subdata, is.na(subdata$warfarin))))
  
  column=append(column, nrow(subset(subdata, subdata$rfVIIa==1)))
  column=append(column, nrow(subset(subdata, subdata$rfVIIa==1))/nrow(subset(subdata, !is.na(subdata$rfVIIa)))*100)
  column=append(column, nrow(subset(subdata, is.na(subdata$rfVIIa))))
  
  column=append(column, median(subdata$t_from_onset_to_ct, na.rm=TRUE))
  column=append(column, IQR(subdata$t_from_onset_to_ct, na.rm=TRUE))
  column=append(column, as.numeric(quantile(subdata$t_from_onset_to_ct, na.rm=TRUE))[2])
  column=append(column, as.numeric(quantile(subdata$t_from_onset_to_ct, na.rm=TRUE))[4])
  column=append(column, nrow(subset(subdata, is.na(subdata$t_from_onset_to_ct))))
  
  column=append(column, nrow(subset(subdata, subdata$bl_fu==1)))
  column=append(column, nrow(subset(subdata, subdata$bl_fu==1))/nrow(subset(subdata, !is.na(subdata$bl_fu)))*100)
  column=append(column, nrow(subset(subdata, is.na(subdata$bl_fu))))
  
  
  if (study=="Predict"){
    result=as.data.frame(column)
  } else {
    result=cbind(result, column)
  }
  
}

rownames(result)=c("n",
                   "mean age","sd age","# age NA",
                   "# F","% F","# F NA",
                   "# HTN","% HTN","# HTN NA",
                   "# DM","% DM","# DM NA",
                   "# HPL","% HPL","# HPL NA",
                   "# prev-ich","% prev-ich","# prev-ich NA",
                   "# warfarin","% warfarin","# warfarin NA",
                   "# rfVIIa","% rfVIIa","# rfVIIa NA",
                   "median t onset to ct","IQR t onset to ct","Q1 t onset to ct","Q3 t onset to ct","# t onset to ct NA",
                   "# pts with bl+fu","% pts with bl+fu","# pts with bl+fu NA"
                   
)

colnames(result)=c("PREDICT","SPOTLIGHT/STOP-IT")

#write.csv(result, "D:/Google Drive/Desktop files/Dal Med/ICH/IVH table 1.csv")


# Table 1 stats ----

table1stats=c()

data_predict=subset(data, data$study=="Predict")
data_SL_SI=subset(data, data$study=="SPOTLIGHT" | data$study=="STOP-IT")


table1stats=append(table1stats, t.test(data_predict$age, data_SL_SI$age)$p.value)


M=as.table(cbind(c(nrow(subset(data_predict, data_predict$gender==1)),nrow(subset(data_predict, data_predict$gender==2))),
                 c(nrow(subset(data_SL_SI, data_SL_SI$gender==1)),nrow(subset(data_SL_SI, data_SL_SI$gender==2)))
))
table1stats=append(table1stats,chisq.test(M)$p.value)


M=as.table(cbind(c(nrow(subset(data_predict, data_predict$htn==0)),nrow(subset(data_predict, data_predict$htn==1))),
                 c(nrow(subset(data_SL_SI, data_SL_SI$htn==0)),nrow(subset(data_SL_SI, data_SL_SI$htn==1)))
))
table1stats=append(table1stats,chisq.test(M)$p.value)


M=as.table(cbind(c(nrow(subset(data_predict, data_predict$dm==0)),nrow(subset(data_predict, data_predict$dm==1))),
                 c(nrow(subset(data_SL_SI, data_SL_SI$dm==0)),nrow(subset(data_SL_SI, data_SL_SI$dm==1)))
))
table1stats=append(table1stats,chisq.test(M)$p.value)


M=as.table(cbind(c(nrow(subset(data_predict, data_predict$hpl==0)),nrow(subset(data_predict, data_predict$hpl==1))),
                 c(nrow(subset(data_SL_SI, data_SL_SI$hpl==0)),nrow(subset(data_SL_SI, data_SL_SI$hpl==1)))
))
table1stats=append(table1stats,chisq.test(M)$p.value)


M=as.table(cbind(c(nrow(subset(data_predict, data_predict$prev_ich==0)),nrow(subset(data_predict, data_predict$prev_ich==1))),
                 c(nrow(subset(data_SL_SI, data_SL_SI$prev_ich==0)),nrow(subset(data_SL_SI, data_SL_SI$prev_ich==1)))
))
table1stats=append(table1stats,fisher.test(M)$p.value)


M=as.table(cbind(c(nrow(subset(data_predict, data_predict$warfarin==0)),nrow(subset(data_predict, data_predict$warfarin==1))),
                 c(nrow(subset(data_SL_SI, data_SL_SI$warfarin==0)),nrow(subset(data_SL_SI, data_SL_SI$warfarin==1)))
))
table1stats=append(table1stats,fisher.test(M)$p.value)


M=as.table(cbind(c(nrow(subset(data_predict, data_predict$rfVIIa==0)),nrow(subset(data_predict, data_predict$rfVIIa==1))),
                 c(nrow(subset(data_SL_SI, data_SL_SI$rfVIIa==0)),nrow(subset(data_SL_SI, data_SL_SI$rfVIIa==1)))
))
table1stats=append(table1stats,fisher.test(M)$p.value)


table1stats=append(table1stats, t.test(data_predict$t_from_onset_to_ct, data_SL_SI$t_from_onset_to_ct)$p.value)


M=as.table(cbind(c(nrow(subset(data_predict, data_predict$bl_fu==0)),nrow(subset(data_predict, data_predict$bl_fu==1))),
                 c(nrow(subset(data_SL_SI, data_SL_SI$bl_fu==0)),nrow(subset(data_SL_SI, data_SL_SI$bl_fu==1)))
))
table1stats=append(table1stats,chisq.test(M)$p.value)

table1stats=as.data.frame(table1stats)

rownames(table1stats)=c("age","gender","htn","dm","hpl","prev_ich","warfarin","rfVIIa","t_from_onset_to_ct","bl_fu")
colnames(table1stats)=c("p-value")

test_types=as.data.frame(c("t-test","chisq","chisq","chisq","chisq","fisher","fisher","fisher","t-test","chisq"))
colnames(test_types)=c("test type")
table1stats=cbind(table1stats, test_types)

#write.csv(table1stats, "D:/Google Drive/Desktop files/Dal Med/ICH/IVH table 1 stats.csv")
