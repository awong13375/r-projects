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
library(DescTools)
library(caret)
library(pROC)
# Data Consolidation ----
# PREDICT
predict_enrolled=readxl::read_xls("C:/Users/alexw/Google Drive/Desktop files/Dal Med/ICH/PREDICTprimaryV3_TH.xls", sheet="enrolled")
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

SL_SI_enrolled=readxl::read_xlsx("C:/Users/alexw/Google Drive/Desktop files/Dal Med/ICH/Archive/GraebCombined_withmRS (Recovered).xlsx", sheet="ALL")
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

keep_cols=c("PID","age","gender","htn","dm","hpl","prev_ich","warfarin","rfVIIa","t_from_onset_to_ct","bl_fu","study","train")
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

#write.csv(result, "C:/Users/alexw/Google Drive/Desktop files/Dal Med/ICH/IVH table 1.csv")


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

#write.csv(table1stats, "C:/Users/alexw/Google Drive/Desktop files/Dal Med/ICH/IVH table 1 stats.csv")


# Table 2 ----
predict_gs <- read_sheet("1c44iNPzrDiZ0DzBb1eAggm3VBd5ImMEnsFcWgzcZWC0", sheet="PREDICT V7 TOTAL")
predict_gs=as.data.frame(predict_gs)
predict_gs=subset(predict_gs, predict_gs$`IVH Present`==1)
predict_gs$IVHS_manual_diff=predict_gs$`IVHS Estimated Volume (mL)` - predict_gs$`Manual Segmentation IVH Volume (mL)`

sl_si_gs <- read_sheet("1c44iNPzrDiZ0DzBb1eAggm3VBd5ImMEnsFcWgzcZWC0", sheet="SPOTLIGHT/STOP-IT")
sl_si_gs=as.data.frame(sl_si_gs)
sl_si_gs=subset(sl_si_gs, sl_si_gs$`IVH Present`==1)
sl_si_gs$IVHS_manual_diff=sl_si_gs$`IVHS Estimated Volume (mL)` - sl_si_gs$`Manual Segmentation IVH Volume (mL)`

keep_cols=c("Study","PID","study_time","Manual Segmentation ICH Volume (mL)","Manual Segmentation IVH Volume (mL)","IVHS_Reader1","IVHS Estimated Volume (mL)",
            "oGS_Reader1 (12)","mGS_Reader1 (32)","dice_ich","dice_ivh","IVHS_manual_diff","train_category")
predict_gs=predict_gs[keep_cols]
sl_si_gs=sl_si_gs[keep_cols]

combined_gs=rbind(predict_gs, sl_si_gs)
training_gs=subset(combined_gs, combined_gs$train_category=="training")
validation_gs=subset(combined_gs, combined_gs$train_category=="validation")

#TRAINING
training_tab_2=c()

training_tab_2=append(training_tab_2, nrow(training_gs))

#IVHS estimated volume 
training_tab_2=append(training_tab_2, median(training_gs$`IVHS Estimated Volume (mL)`))
training_tab_2=append(training_tab_2, as.numeric(quantile(training_gs$`IVHS Estimated Volume (mL)`, na.rm=TRUE))[2])
training_tab_2=append(training_tab_2, as.numeric(quantile(training_gs$`IVHS Estimated Volume (mL)`, na.rm=TRUE))[4])

#CNN estimated volume

#Manual segmentation volume
training_tab_2=append(training_tab_2, median(training_gs$`Manual Segmentation IVH Volume (mL)`))
training_tab_2=append(training_tab_2, as.numeric(quantile(training_gs$`Manual Segmentation IVH Volume (mL)`, na.rm=TRUE))[2])
training_tab_2=append(training_tab_2, as.numeric(quantile(training_gs$`Manual Segmentation IVH Volume (mL)`, na.rm=TRUE))[4])

#IVHS score
training_tab_2=append(training_tab_2, median(training_gs$IVHS_Reader1))
training_tab_2=append(training_tab_2, as.numeric(quantile(training_gs$`IVHS Estimated Volume (mL)`, na.rm=TRUE))[2])
training_tab_2=append(training_tab_2, as.numeric(quantile(training_gs$`IVHS Estimated Volume (mL)`, na.rm=TRUE))[4])

#oGS score
training_tab_2=append(training_tab_2, median(training_gs$`oGS_Reader1 (12)`))
training_tab_2=append(training_tab_2, as.numeric(quantile(training_gs$`oGS_Reader1 (12)`, na.rm=TRUE))[2])
training_tab_2=append(training_tab_2, as.numeric(quantile(training_gs$`oGS_Reader1 (12)`, na.rm=TRUE))[4])

#mGS score
training_tab_2=append(training_tab_2, median(training_gs$`mGS_Reader1 (32)`))
training_tab_2=append(training_tab_2, as.numeric(quantile(training_gs$`mGS_Reader1 (32)`, na.rm=TRUE))[2])
training_tab_2=append(training_tab_2, as.numeric(quantile(training_gs$`mGS_Reader1 (32)`, na.rm=TRUE))[4])

#IVHS manual segmentation diff
training_tab_2=append(training_tab_2, median(training_gs$IVHS_manual_diff))
training_tab_2=append(training_tab_2, as.numeric(quantile(training_gs$IVHS_manual_diff, na.rm=TRUE))[2])
training_tab_2=append(training_tab_2, as.numeric(quantile(training_gs$IVHS_manual_diff, na.rm=TRUE))[4])

#CNN manual segmentation diff


#VALIDATION
validation_tab_2=c()

validation_tab_2=append(validation_tab_2, nrow(validation_gs))

#IVHS estimated volume 
validation_tab_2=append(validation_tab_2, median(validation_gs$`IVHS Estimated Volume (mL)`))
validation_tab_2=append(validation_tab_2, as.numeric(quantile(validation_gs$`IVHS Estimated Volume (mL)`, na.rm=TRUE))[2])
validation_tab_2=append(validation_tab_2, as.numeric(quantile(validation_gs$`IVHS Estimated Volume (mL)`, na.rm=TRUE))[4])

#CNN estimated volume

#Manual segmentation volume
validation_tab_2=append(validation_tab_2, median(validation_gs$`Manual Segmentation IVH Volume (mL)`))
validation_tab_2=append(validation_tab_2, as.numeric(quantile(validation_gs$`Manual Segmentation IVH Volume (mL)`, na.rm=TRUE))[2])
validation_tab_2=append(validation_tab_2, as.numeric(quantile(validation_gs$`Manual Segmentation IVH Volume (mL)`, na.rm=TRUE))[4])

#IVHS score
validation_tab_2=append(validation_tab_2, median(validation_gs$IVHS_Reader1))
validation_tab_2=append(validation_tab_2, as.numeric(quantile(validation_gs$`IVHS Estimated Volume (mL)`, na.rm=TRUE))[2])
validation_tab_2=append(validation_tab_2, as.numeric(quantile(validation_gs$`IVHS Estimated Volume (mL)`, na.rm=TRUE))[4])

#oGS score
validation_tab_2=append(validation_tab_2, median(validation_gs$`oGS_Reader1 (12)`))
validation_tab_2=append(validation_tab_2, as.numeric(quantile(validation_gs$`oGS_Reader1 (12)`, na.rm=TRUE))[2])
validation_tab_2=append(validation_tab_2, as.numeric(quantile(validation_gs$`oGS_Reader1 (12)`, na.rm=TRUE))[4])

#mGS score
validation_tab_2=append(validation_tab_2, median(validation_gs$`mGS_Reader1 (32)`))
validation_tab_2=append(validation_tab_2, as.numeric(quantile(validation_gs$`mGS_Reader1 (32)`, na.rm=TRUE))[2])
validation_tab_2=append(validation_tab_2, as.numeric(quantile(validation_gs$`mGS_Reader1 (32)`, na.rm=TRUE))[4])

#IVHS manual segmentation diff
validation_tab_2=append(validation_tab_2, median(validation_gs$IVHS_manual_diff))
validation_tab_2=append(validation_tab_2, as.numeric(quantile(validation_gs$IVHS_manual_diff, na.rm=TRUE))[2])
validation_tab_2=append(validation_tab_2, as.numeric(quantile(validation_gs$IVHS_manual_diff, na.rm=TRUE))[4])

#CNN manual segmentation diff

training_tab_2=as.data.frame(training_tab_2)
validation_tab_2=as.data.frame(validation_tab_2)
tab_2=cbind(training_tab_2, validation_tab_2)
rownames(tab_2)=c("n","IVHS est vol","IVHS vol Q2","IVHS vol Q3",
                  "Manual vol","manual Q2","manual Q3",
                  "IVHS score","IVHS score Q2","IVHS score Q3",
                  "oGS score","oGS Q2","oGS Q3",
                  "mGS score","mGS Q2","mGS Q3",
                  "IVHS manual diff","IVHS man diff Q2","IVHS man diff Q3")
colnames(tab_2)=c("Training","Validation")
#write.csv(tab_2, "C:/Users/alexw/Google Drive/Desktop files/Dal Med/ICH/IVH Table 2.csv")


# Table 2 stats ----

#Data consolidation
data_stats=rbind(training_gs, validation_gs)
results_stats=c()
results_stats=append(results_stats, kruskal.test(`IVHS Estimated Volume (mL)` ~ train_category, data=data_stats)$p.value)
results_stats=append(results_stats, kruskal.test(`Manual Segmentation IVH Volume (mL)` ~ train_category, data=data_stats)$p.value)
results_stats=append(results_stats, kruskal.test(IVHS_Reader1 ~ train_category, data=data_stats)$p.value)
results_stats=append(results_stats, kruskal.test(`oGS_Reader1 (12)` ~ train_category, data=data_stats)$p.value)
results_stats=append(results_stats, kruskal.test(`mGS_Reader1 (32)` ~ train_category, data=data_stats)$p.value)
results_stats=append(results_stats, kruskal.test(IVHS_manual_diff ~ train_category, data=data_stats)$p.value)

results_stats=as.data.frame(results_stats)
colnames(results_stats)=c("p-value")
rownames(results_stats)=c("IVHS est vol","Man seg vol","ivhs score","ogs","mgs","ivhs man diff")

#write.csv(results_stats, "C:/Users/alexw/Google Drive/Desktop files/Dal Med/ICH/IVH Table 2 stats.csv")

# Table 3 ----
# ICC Calculations

#TRAINING

#IVHS vs manual segmentation
icc(data.frame(training_gs$`IVHS Estimated Volume (mL)`,
               training_gs$`Manual Segmentation IVH Volume (mL)`),
               model="twoway",type="agreement")

#VALIDATION

#IVHS vs manual segmentation
icc(data.frame(validation_gs$`IVHS Estimated Volume (mL)`,
               validation_gs$`Manual Segmentation IVH Volume (mL)`),
               model="twoway",type="agreement")

# Table 4 ----
#Training
tab_4_results=c()
tab_4_results=append(tab_4_results, mean(training_gs$dice_ivh))
tab_4_results=append(tab_4_results, t.test(training_gs$dice_ivh)$conf.int[1])
tab_4_results=append(tab_4_results, t.test(training_gs$dice_ivh)$conf.int[2])

tab_4_results=append(tab_4_results, mean(validation_gs$dice_ivh))
tab_4_results=append(tab_4_results, t.test(validation_gs$dice_ivh)$conf.int[1])
tab_4_results=append(tab_4_results, t.test(validation_gs$dice_ivh)$conf.int[2])

tab_4_results=as.data.frame(tab_4_results)
rownames(tab_4_results)=c("train mean","train l_95_ci","train u_95_ci",
                          "validate mean","validate l_95_ci","validate u_95_ci")
#write.csv(tab_4_results,"C:/Users/alexw/Google Drive/Desktop files/Dal Med/ICH/IVH Table 4.csv")


# Table 5 ----
#Pearson R correlations 

#TRAINING
#IVHS score
cor.test(subset(data_stats, data_stats$train_category=="training")$IVHS_Reader1, 
         subset(data_stats, data_stats$train_category=="training")$`Manual Segmentation IVH Volume (mL)`
         , method = "pearson")
FisherZ(cor.test(subset(data_stats, data_stats$train_category=="training")$IVHS_Reader1, 
                 subset(data_stats, data_stats$train_category=="training")$`Manual Segmentation IVH Volume (mL)`
                 , method = "pearson")$estimate)
FisherZ(cor.test(subset(data_stats, data_stats$train_category=="training")$IVHS_Reader1, 
               subset(data_stats, data_stats$train_category=="training")$`Manual Segmentation IVH Volume (mL)`
               , method = "pearson")$conf.int[1])
FisherZ(cor.test(subset(data_stats, data_stats$train_category=="training")$IVHS_Reader1, 
                 subset(data_stats, data_stats$train_category=="training")$`Manual Segmentation IVH Volume (mL)`
                 , method = "pearson")$conf.int[2])


#oGS
cor.test(subset(data_stats, data_stats$train_category=="training")$`oGS_Reader1 (12)`, 
         subset(data_stats, data_stats$train_category=="training")$`Manual Segmentation IVH Volume (mL)`
         , method = "pearson")
FisherZ(cor.test(subset(data_stats, data_stats$train_category=="training")$`oGS_Reader1 (12)`, 
                 subset(data_stats, data_stats$train_category=="training")$`Manual Segmentation IVH Volume (mL)`
                 , method = "pearson")$estimate)
FisherZ(cor.test(subset(data_stats, data_stats$train_category=="training")$`oGS_Reader1 (12)`, 
                 subset(data_stats, data_stats$train_category=="training")$`Manual Segmentation IVH Volume (mL)`
                 , method = "pearson")$conf.int[1])
FisherZ(cor.test(subset(data_stats, data_stats$train_category=="training")$`oGS_Reader1 (12)`, 
                 subset(data_stats, data_stats$train_category=="training")$`Manual Segmentation IVH Volume (mL)`
                 , method = "pearson")$conf.int[2])

#mGS
cor.test(subset(data_stats, data_stats$train_category=="training")$`mGS_Reader1 (32)`, 
         subset(data_stats, data_stats$train_category=="training")$`Manual Segmentation IVH Volume (mL)`
         , method = "pearson")
FisherZ(cor.test(subset(data_stats, data_stats$train_category=="training")$`mGS_Reader1 (32)`, 
                 subset(data_stats, data_stats$train_category=="training")$`Manual Segmentation IVH Volume (mL)`
                 , method = "pearson")$estimate)
FisherZ(cor.test(subset(data_stats, data_stats$train_category=="training")$`mGS_Reader1 (32)`, 
                 subset(data_stats, data_stats$train_category=="training")$`Manual Segmentation IVH Volume (mL)`
                 , method = "pearson")$conf.int[1])
FisherZ(cor.test(subset(data_stats, data_stats$train_category=="training")$`mGS_Reader1 (32)`, 
                 subset(data_stats, data_stats$train_category=="training")$`Manual Segmentation IVH Volume (mL)`
                 , method = "pearson")$conf.int[2])


#VALIDATION
#IVHS score
cor.test(subset(data_stats, data_stats$train_category=="validation")$IVHS_Reader1, 
         subset(data_stats, data_stats$train_category=="validation")$`Manual Segmentation IVH Volume (mL)`
         , method = "pearson")
FisherZ(cor.test(subset(data_stats, data_stats$train_category=="validation")$IVHS_Reader1, 
                 subset(data_stats, data_stats$train_category=="validation")$`Manual Segmentation IVH Volume (mL)`
                 , method = "pearson")$estimate)
FisherZ(cor.test(subset(data_stats, data_stats$train_category=="validation")$IVHS_Reader1, 
                 subset(data_stats, data_stats$train_category=="validation")$`Manual Segmentation IVH Volume (mL)`
                 , method = "pearson")$conf.int[1])
FisherZ(cor.test(subset(data_stats, data_stats$train_category=="validation")$IVHS_Reader1, 
                 subset(data_stats, data_stats$train_category=="validation")$`Manual Segmentation IVH Volume (mL)`
                 , method = "pearson")$conf.int[2])


#oGS
cor.test(subset(data_stats, data_stats$train_category=="validation")$`oGS_Reader1 (12)`, 
         subset(data_stats, data_stats$train_category=="validation")$`Manual Segmentation IVH Volume (mL)`
         , method = "pearson")
FisherZ(cor.test(subset(data_stats, data_stats$train_category=="validation")$`oGS_Reader1 (12)`, 
                 subset(data_stats, data_stats$train_category=="validation")$`Manual Segmentation IVH Volume (mL)`
                 , method = "pearson")$estimate)
FisherZ(cor.test(subset(data_stats, data_stats$train_category=="validation")$`oGS_Reader1 (12)`, 
                 subset(data_stats, data_stats$train_category=="validation")$`Manual Segmentation IVH Volume (mL)`
                 , method = "pearson")$conf.int[1])
FisherZ(cor.test(subset(data_stats, data_stats$train_category=="validation")$`oGS_Reader1 (12)`, 
                 subset(data_stats, data_stats$train_category=="validation")$`Manual Segmentation IVH Volume (mL)`
                 , method = "pearson")$conf.int[2])

#mGS
cor.test(subset(data_stats, data_stats$train_category=="validation")$`mGS_Reader1 (32)`, 
         subset(data_stats, data_stats$train_category=="validation")$`Manual Segmentation IVH Volume (mL)`
         , method = "pearson")
FisherZ(cor.test(subset(data_stats, data_stats$train_category=="validation")$`mGS_Reader1 (32)`, 
                 subset(data_stats, data_stats$train_category=="validation")$`Manual Segmentation IVH Volume (mL)`
                 , method = "pearson")$estimate)
FisherZ(cor.test(subset(data_stats, data_stats$train_category=="validation")$`mGS_Reader1 (32)`, 
                 subset(data_stats, data_stats$train_category=="validation")$`Manual Segmentation IVH Volume (mL)`
                 , method = "pearson")$conf.int[1])
FisherZ(cor.test(subset(data_stats, data_stats$train_category=="validation")$`mGS_Reader1 (32)`, 
                 subset(data_stats, data_stats$train_category=="validation")$`Manual Segmentation IVH Volume (mL)`
                 , method = "pearson")$conf.int[2])


# Bland Altman Plots ----
#PREDICT
#IVHS estimate vs manual segmentation
IVHS_est_man_seg_training=(blandr.draw(subset(data_stats, data_stats$train_category=="training")$`IVHS Estimated Volume (mL)`, 
                     subset(data_stats, data_stats$train_category=="training")$`Manual Segmentation IVH Volume (mL)`, 
                     sig.level = 0.95, LoA.mode = 1, ciDisplay = FALSE,
                     ciShading = FALSE,
                     lowest_y_axis = ymin, highest_y_axis = ymax, point_size = 0.8,
                     overlapping = FALSE, plotter = "ggplot", x.plot.mode = "means",
                     y.plot.mode = "difference", plotProportionalBias = FALSE,
                     plotProportionalBias.se = TRUE, assume.differences.are.normal = TRUE) 
                 + xlab("Mean IVH volume (mL)")
                 + ylab("IVHS Estimated Volume - Manual Segmentation Volume (mL)")
                 + theme(
                   panel.border = element_blank(),  
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(),
                   axis.line = element_line(colour = "black", size=1),
                   axis.text=element_text(size=12),
                   axis.title=element_text(size=12)
                 )
                 + expand_limits(x = 85, y = 0)
                 + scale_x_continuous(expand = c(0, 1))
                 
                 + labs(title = element_blank())
)

#CNN vs manual segmentation

#SPOTLIGHT
#IVHS estimate vs manual segmentation
IVHS_est_man_seg_validation=(blandr.draw(subset(data_stats, data_stats$train_category=="validation")$`IVHS Estimated Volume (mL)`, 
                              subset(data_stats, data_stats$train_category=="validation")$`Manual Segmentation IVH Volume (mL)`, 
                              sig.level = 0.95, LoA.mode = 1, ciDisplay = FALSE,
                              ciShading = FALSE,
                              lowest_y_axis = ymin, highest_y_axis = ymax, point_size = 0.8,
                              overlapping = FALSE, plotter = "ggplot", x.plot.mode = "means",
                              y.plot.mode = "difference", plotProportionalBias = FALSE,
                              plotProportionalBias.se = TRUE, assume.differences.are.normal = TRUE) 
                  + xlab("Mean IVH volume (mL)")
                  + ylab("IVHS Estimated Volume - Manual Segmentation Volume (mL)")
                  + theme(
                    panel.border = element_blank(),  
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.background = element_blank(),
                    axis.line = element_line(colour = "black", size=1),
                    axis.text=element_text(size=12),
                    axis.title=element_text(size=12)
                  )
                  + expand_limits(x = 90, y = 0)
                  + scale_x_continuous(expand = c(0, 1))
                  
                  + labs(title = element_blank())
)

#CNN vs manual segmentation


# Linear Regression ----
#PREDICT
#IVHS estimate vs manual segmentation
data_stats$IVHS_man_seg_avg=(data_stats$`IVHS Estimated Volume (mL)`+data_stats$`Manual Segmentation IVH Volume (mL)`)/2

lml=lm(IVHS_manual_diff ~ IVHS_man_seg_avg, data=subset(data_stats, data_stats$train_category=="training"))
summary(lml)

#CNN vs manual segmentation



#SPOTLIGHT
#IVHS estimate vs manual segmentation
data_stats$IVHS_man_seg_avg=(data_stats$`IVHS Estimated Volume (mL)`+data_stats$`Manual Segmentation IVH Volume (mL)`)/2

lml=lm(IVHS_manual_diff ~ IVHS_man_seg_avg, data=subset(data_stats, data_stats$train_category=="validation"))
summary(lml)

#CNN vs manual segmentation


# Hematoma expansion analysis ----

#Filter out studies without followup scans
#PREDICT
predict_gs_expansion <- predict_gs
for (study in levels(as.factor(predict_gs$PID))){
  if (nrow(subset(predict_gs, predict_gs$PID==study))<2){
    predict_gs_expansion=subset(predict_gs_expansion, predict_gs_expansion$PID!=study)
  }
}

#SPOTLIGHT/STOP-IT
sl_si_gs_expansion <- sl_si_gs
for (study in levels(as.factor(sl_si_gs$PID))){
  if (nrow(subset(sl_si_gs, sl_si_gs$PID==study))<2){
    sl_si_gs_expansion=subset(sl_si_gs_expansion, sl_si_gs_expansion$PID!=study)
  }
}

#Expansion analysis
#PREDICT
n=0
for (study in levels(as.factor(predict_gs_expansion$PID))){
  subtab_study=subset(predict_gs_expansion, predict_gs_expansion$PID==study)
  row=c()
  row=append(row, study)
  
  #Manual segmentation
  if ((subtab_study[2,"Manual Segmentation IVH Volume (mL)"]-subtab_study[1,"Manual Segmentation IVH Volume (mL)"])>=1 |
      ((subtab_study[2,"Manual Segmentation IVH Volume (mL)"]-subtab_study[1,"Manual Segmentation IVH Volume (mL)"])/subtab_study[1,"Manual Segmentation IVH Volume (mL)"])>=0.33){
    row=append(row, 1)
  } else {
    row=append(row, 0)
  }
  
  #IVHS est vol
  if ((subtab_study[2,"IVHS Estimated Volume (mL)"]-subtab_study[1,"IVHS Estimated Volume (mL)"])>=1 |
      ((subtab_study[2,"IVHS Estimated Volume (mL)"]-subtab_study[1,"IVHS Estimated Volume (mL)"])/subtab_study[1,"IVHS Estimated Volume (mL)"]>=0.33)){
    row=append(row, 1)
  } else {
    row=append(row, 0)
  }
  
  #oGS
  if ((subtab_study[2,"oGS_Reader1 (12)"]-subtab_study[1,"oGS_Reader1 (12)"])>0){
    row=append(row, 1)
  } else {
    row=append(row, 0)
  }
  
  #mGS
  if ((subtab_study[2,"mGS_Reader1 (32)"]-subtab_study[1,"mGS_Reader1 (32)"])>0){
    row=append(row, 1)
  } else {
    row=append(row, 0)
  }
  row=as.numeric(row)
  if (n==0){
    expansion_predict=rbind(row)
    colnames(expansion_predict)=c("study","Manual_segmentation","IVHS","oGS","mGS")
  } else {
    expansion_predict=rbind(expansion_predict, row)
  }
  n=n+1
}
expansion_predict=as.data.frame(expansion_predict)

#IVHS
conf_matrix_predict_IVHS=table(expansion_predict$IVHS, expansion_predict$Manual_segmentation)
sensitivity(conf_matrix_predict_IVHS)
specificity(conf_matrix_predict_IVHS)

logit<- glm(Manual_segmentation ~ IVHS, family=binomial,data=expansion_predict)
predicted_prob<-predict(logit,type="response")
roccurve <- roc(expansion_predict$Manual_segmentation, predicted_prob)
auc(roccurve)
ci.auc(roccurve)

#oGS
conf_matrix_predict_oGS=table(expansion_predict$oGS, expansion_predict$Manual_segmentation)
sensitivity(conf_matrix_predict_oGS)
specificity(conf_matrix_predict_oGS)

logit<- glm(Manual_segmentation ~ oGS, family=binomial,data=expansion_predict)
predicted_prob<-predict(logit,type="response")
roccurve <- roc(expansion_predict$Manual_segmentation, predicted_prob)
auc(roccurve)
ci.auc(roccurve)

#mGS
conf_matrix_predict_mGS=table(expansion_predict$mGS, expansion_predict$Manual_segmentation)
sensitivity(conf_matrix_predict_mGS)
specificity(conf_matrix_predict_mGS)

logit<- glm(Manual_segmentation ~ mGS, family=binomial,data=expansion_predict)
predicted_prob<-predict(logit,type="response")
roccurve <- roc(expansion_predict$Manual_segmentation, predicted_prob)
auc(roccurve)
ci.auc(roccurve)

#CNN

#SPOTLIGHt/STOP-IT
n=0
for (study in levels(as.factor(sl_si_gs_expansion$PID))){
  subtab_study=subset(sl_si_gs_expansion, sl_si_gs_expansion$PID==study)
  
  if (nrow(subtab_study)<=2){
    i_range=c(2)
  } else if (nrow(subtab_study>2)){
    i_range=c(2:nrow(subtab_study))
  }
  
  for (i in i_range){
    row=c()
    row=append(row, study)
    
    #Manual segmentation
    if ((subtab_study[i,"Manual Segmentation IVH Volume (mL)"]-subtab_study[1,"Manual Segmentation IVH Volume (mL)"])>=1 |
        ((subtab_study[i,"Manual Segmentation IVH Volume (mL)"]-subtab_study[1,"Manual Segmentation IVH Volume (mL)"])/subtab_study[1,"Manual Segmentation IVH Volume (mL)"])>=0.33){
      row=append(row, 1)
    } else {
      row=append(row, 0)
    }
    
    #IVHS est vol
    if ((subtab_study[i,"IVHS Estimated Volume (mL)"]-subtab_study[1,"IVHS Estimated Volume (mL)"])>=1 |
        ((subtab_study[i,"IVHS Estimated Volume (mL)"]-subtab_study[1,"IVHS Estimated Volume (mL)"])/subtab_study[1,"IVHS Estimated Volume (mL)"])>=0.33){
      row=append(row, 1)
    } else {
      row=append(row, 0)
    }
    
    #oGS
    if (subtab_study[i,"oGS_Reader1 (12)"]-subtab_study[1,"oGS_Reader1 (12)"]>0){
      row=append(row, 1)
    } else {
      row=append(row, 0)
    }
    
    #mGS
    if (subtab_study[i,"mGS_Reader1 (32)"]-subtab_study[1,"mGS_Reader1 (32)"]>0){
      row=append(row, 1)
    } else {
      row=append(row, 0)
    }
    
    row=as.numeric(row)
    
    if (n==0){
      expansion_sl_si=rbind(row)
      colnames(expansion_sl_si)=c("study","Manual_segmentation","IVHS","oGS","mGS")
    } else {
      expansion_sl_si=rbind(expansion_sl_si, row)
    }
    n=n+1
  }
}
expansion_sl_si=as.data.frame(expansion_sl_si)

#IVHS
conf_matrix_sl_si_IVHS=table(expansion_sl_si$IVHS, expansion_sl_si$Manual_segmentation)
sensitivity(conf_matrix_sl_si_IVHS)
specificity(conf_matrix_sl_si_IVHS)

logit<- glm(Manual_segmentation ~ IVHS, family=binomial,data=expansion_sl_si)
predicted_prob<-predict(logit,type="response")
roccurve <- roc(expansion_sl_si$Manual_segmentation, predicted_prob)
auc(roccurve)
ci.auc(roccurve)

#oGS
conf_matrix_sl_si_oGS=table(expansion_sl_si$oGS, expansion_sl_si$Manual_segmentation)
sensitivity(conf_matrix_sl_si_oGS)
specificity(conf_matrix_sl_si_oGS)

logit<- glm(Manual_segmentation ~ oGS, family=binomial,data=expansion_sl_si)
predicted_prob<-predict(logit,type="response")
roccurve <- roc(expansion_sl_si$Manual_segmentation, predicted_prob)
auc(roccurve)
ci.auc(roccurve)

#mGS
conf_matrix_sl_si_mGS=table(expansion_sl_si$mGS, expansion_sl_si$Manual_segmentation)
sensitivity(conf_matrix_sl_si_mGS)
specificity(conf_matrix_sl_si_mGS)

logit<- glm(Manual_segmentation ~ mGS, family=binomial,data=expansion_sl_si)
predicted_prob<-predict(logit,type="response")
roccurve <- roc(expansion_sl_si$Manual_segmentation, predicted_prob)
auc(roccurve)
ci.auc(roccurve)




