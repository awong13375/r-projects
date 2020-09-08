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


#PREDICT
predict_tab_2=c()

predict_tab_2=append(predict_tab_2, nrow(predict_gs))

#IVHS estimated volume 
predict_tab_2=append(predict_tab_2, median(predict_gs$`IVHS Estimated Volume (mL)`))
predict_tab_2=append(predict_tab_2, as.numeric(quantile(predict_gs$`IVHS Estimated Volume (mL)`, na.rm=TRUE))[2])
predict_tab_2=append(predict_tab_2, as.numeric(quantile(predict_gs$`IVHS Estimated Volume (mL)`, na.rm=TRUE))[4])

#CNN estimated volume

#Manual segmentation volume
predict_tab_2=append(predict_tab_2, median(predict_gs$`Manual Segmentation IVH Volume (mL)`))
predict_tab_2=append(predict_tab_2, as.numeric(quantile(predict_gs$`Manual Segmentation IVH Volume (mL)`, na.rm=TRUE))[2])
predict_tab_2=append(predict_tab_2, as.numeric(quantile(predict_gs$`Manual Segmentation IVH Volume (mL)`, na.rm=TRUE))[4])

#IVHS score
predict_tab_2=append(predict_tab_2, median(predict_gs$IVHS_Reader1))
predict_tab_2=append(predict_tab_2, as.numeric(quantile(predict_gs$`IVHS Estimated Volume (mL)`, na.rm=TRUE))[2])
predict_tab_2=append(predict_tab_2, as.numeric(quantile(predict_gs$`IVHS Estimated Volume (mL)`, na.rm=TRUE))[4])

#oGS score
predict_tab_2=append(predict_tab_2, median(predict_gs$`oGS_Reader1 (12)`))
predict_tab_2=append(predict_tab_2, as.numeric(quantile(predict_gs$`oGS_Reader1 (12)`, na.rm=TRUE))[2])
predict_tab_2=append(predict_tab_2, as.numeric(quantile(predict_gs$`oGS_Reader1 (12)`, na.rm=TRUE))[4])

#mGS score
predict_tab_2=append(predict_tab_2, median(predict_gs$`mGS_Reader1 (32)`))
predict_tab_2=append(predict_tab_2, as.numeric(quantile(predict_gs$`mGS_Reader1 (32)`, na.rm=TRUE))[2])
predict_tab_2=append(predict_tab_2, as.numeric(quantile(predict_gs$`mGS_Reader1 (32)`, na.rm=TRUE))[4])

#IVHS manual segmentation diff
predict_tab_2=append(predict_tab_2, median(predict_gs$IVHS_manual_diff))
predict_tab_2=append(predict_tab_2, as.numeric(quantile(predict_gs$IVHS_manual_diff, na.rm=TRUE))[2])
predict_tab_2=append(predict_tab_2, as.numeric(quantile(predict_gs$IVHS_manual_diff, na.rm=TRUE))[4])

#CNN manual segmentation diff


#SPOTLIGHT
sl_si_tab_2=c()

sl_si_tab_2=append(sl_si_tab_2, nrow(sl_si_gs))

#IVHS estimated volume 
sl_si_tab_2=append(sl_si_tab_2, median(sl_si_gs$`IVHS Estimated Volume (mL)`))
sl_si_tab_2=append(sl_si_tab_2, as.numeric(quantile(sl_si_gs$`IVHS Estimated Volume (mL)`, na.rm=TRUE))[2])
sl_si_tab_2=append(sl_si_tab_2, as.numeric(quantile(sl_si_gs$`IVHS Estimated Volume (mL)`, na.rm=TRUE))[4])

#CNN estimated volume

#Manual segmentation volume
sl_si_tab_2=append(sl_si_tab_2, median(sl_si_gs$`Manual Segmentation IVH Volume (mL)`))
sl_si_tab_2=append(sl_si_tab_2, as.numeric(quantile(sl_si_gs$`Manual Segmentation IVH Volume (mL)`, na.rm=TRUE))[2])
sl_si_tab_2=append(sl_si_tab_2, as.numeric(quantile(sl_si_gs$`Manual Segmentation IVH Volume (mL)`, na.rm=TRUE))[4])

#IVHS score
sl_si_tab_2=append(sl_si_tab_2, median(sl_si_gs$IVHS_Reader1))
sl_si_tab_2=append(sl_si_tab_2, as.numeric(quantile(sl_si_gs$`IVHS Estimated Volume (mL)`, na.rm=TRUE))[2])
sl_si_tab_2=append(sl_si_tab_2, as.numeric(quantile(sl_si_gs$`IVHS Estimated Volume (mL)`, na.rm=TRUE))[4])

#oGS score
sl_si_tab_2=append(sl_si_tab_2, median(sl_si_gs$`oGS_Reader1 (12)`))
sl_si_tab_2=append(sl_si_tab_2, as.numeric(quantile(sl_si_gs$`oGS_Reader1 (12)`, na.rm=TRUE))[2])
sl_si_tab_2=append(sl_si_tab_2, as.numeric(quantile(sl_si_gs$`oGS_Reader1 (12)`, na.rm=TRUE))[4])

#mGS score
sl_si_tab_2=append(sl_si_tab_2, median(sl_si_gs$`mGS_Reader1 (32)`))
sl_si_tab_2=append(sl_si_tab_2, as.numeric(quantile(sl_si_gs$`mGS_Reader1 (32)`, na.rm=TRUE))[2])
sl_si_tab_2=append(sl_si_tab_2, as.numeric(quantile(sl_si_gs$`mGS_Reader1 (32)`, na.rm=TRUE))[4])

#IVHS manual segmentation diff
sl_si_tab_2=append(sl_si_tab_2, median(sl_si_gs$IVHS_manual_diff))
sl_si_tab_2=append(sl_si_tab_2, as.numeric(quantile(sl_si_gs$IVHS_manual_diff, na.rm=TRUE))[2])
sl_si_tab_2=append(sl_si_tab_2, as.numeric(quantile(sl_si_gs$IVHS_manual_diff, na.rm=TRUE))[4])

#CNN manual segmentation diff

predict_tab_2=as.data.frame(predict_tab_2)
sl_si_tab_2=as.data.frame(sl_si_tab_2)
tab_2=cbind(predict_tab_2, sl_si_tab_2)
rownames(tab_2)=c("n","IVHS est vol","IVHS vol Q2","IVHS vol Q3",
                  "Manual vol","manual Q2","manual Q3",
                  "IVHS score","IVHS score Q2","IVHS score Q3",
                  "oGS score","oGS Q2","oGS Q3",
                  "mGS score","mGS Q2","mGS Q3",
                  "IVHS manual diff","IVHS man diff Q2","IVHS man diff Q3")
colnames(tab_2)=c("PREDICT","SPOTLIGHT/STOP-IT")
#write.csv(tab_2, "C:/Users/alexw/Google Drive/Desktop files/Dal Med/ICH/IVH Table 2.csv")


# Table 2 stats ----

#Data consolidation
keeps=c("Study","IVHS Estimated Volume (mL)","Manual Segmentation IVH Volume (mL)","IVHS_Reader1","oGS_Reader1 (12)","mGS_Reader1 (32)","IVHS_manual_diff")
predict_gs_stats=predict_gs[,keeps]
sl_si_gs_stats=sl_si_gs[,keeps]
sl_si_gs_stats$Study[sl_si_gs_stats$Study=="STOP-IT" | sl_si_gs_stats$Study=="SPOTLIGHT"]="SPOTLIGHT/STOP-IT"
data_stats=rbind(predict_gs_stats, sl_si_gs_stats)
results_stats=c()
results_stats=append(results_stats, kruskal.test(`IVHS Estimated Volume (mL)` ~ Study, data=data_stats)$p.value)
results_stats=append(results_stats, kruskal.test(`Manual Segmentation IVH Volume (mL)` ~ Study, data=data_stats)$p.value)
results_stats=append(results_stats, kruskal.test(IVHS_Reader1 ~ Study, data=data_stats)$p.value)
results_stats=append(results_stats, kruskal.test(`oGS_Reader1 (12)` ~ Study, data=data_stats)$p.value)
results_stats=append(results_stats, kruskal.test(`mGS_Reader1 (32)` ~ Study, data=data_stats)$p.value)
results_stats=append(results_stats, kruskal.test(IVHS_manual_diff ~ Study, data=data_stats)$p.value)

results_stats=as.data.frame(results_stats)
colnames(results_stats)=c("p-value")
rownames(results_stats)=c("IVHS est vol","Man seg vol","ivhs score","ogs","mgs","ivhs man diff")

#write.csv(results_stats, "C:/Users/alexw/Google Drive/Desktop files/Dal Med/ICH/IVH Table 2 stats.csv")

# Table 3 ----
# ICC Calculations

#PREDICT

#IVHS vs manual segmentation
icc(data.frame(predict_gs$`IVHS Estimated Volume (mL)`,
               predict_gs$`Manual Segmentation IVH Volume (mL)`),
               model="twoway",type="agreement")

#SPOTLIGHT
icc(data.frame(sl_si_gs$`IVHS Estimated Volume (mL)`,
               sl_si_gs$`Manual Segmentation IVH Volume (mL)`),
               model="twoway",type="agreement")

# Table 4 ----
#Table 5 ----
#Pearson R correlations 

#PREDICT
#IVHS score
cor.test(subset(data_stats, data_stats$Study=="PREDICT")$IVHS_Reader1, 
         subset(data_stats, data_stats$Study=="PREDICT")$`Manual Segmentation IVH Volume (mL)`
         , method = "pearson")
FisherZ(cor.test(subset(data_stats, data_stats$Study=="PREDICT")$IVHS_Reader1, 
                 subset(data_stats, data_stats$Study=="PREDICT")$`Manual Segmentation IVH Volume (mL)`
                 , method = "pearson")$estimate)
FisherZ(cor.test(subset(data_stats, data_stats$Study=="PREDICT")$IVHS_Reader1, 
               subset(data_stats, data_stats$Study=="PREDICT")$`Manual Segmentation IVH Volume (mL)`
               , method = "pearson")$conf.int[1])
FisherZ(cor.test(subset(data_stats, data_stats$Study=="PREDICT")$IVHS_Reader1, 
                 subset(data_stats, data_stats$Study=="PREDICT")$`Manual Segmentation IVH Volume (mL)`
                 , method = "pearson")$conf.int[2])


#oGS
cor.test(subset(data_stats, data_stats$Study=="PREDICT")$`oGS_Reader1 (12)`, 
         subset(data_stats, data_stats$Study=="PREDICT")$`Manual Segmentation IVH Volume (mL)`
         , method = "pearson")
FisherZ(cor.test(subset(data_stats, data_stats$Study=="PREDICT")$`oGS_Reader1 (12)`, 
                 subset(data_stats, data_stats$Study=="PREDICT")$`Manual Segmentation IVH Volume (mL)`
                 , method = "pearson")$estimate)
FisherZ(cor.test(subset(data_stats, data_stats$Study=="PREDICT")$`oGS_Reader1 (12)`, 
                 subset(data_stats, data_stats$Study=="PREDICT")$`Manual Segmentation IVH Volume (mL)`
                 , method = "pearson")$conf.int[1])
FisherZ(cor.test(subset(data_stats, data_stats$Study=="PREDICT")$`oGS_Reader1 (12)`, 
                 subset(data_stats, data_stats$Study=="PREDICT")$`Manual Segmentation IVH Volume (mL)`
                 , method = "pearson")$conf.int[2])

#mGS
cor.test(subset(data_stats, data_stats$Study=="PREDICT")$`mGS_Reader1 (32)`, 
         subset(data_stats, data_stats$Study=="PREDICT")$`Manual Segmentation IVH Volume (mL)`
         , method = "pearson")
FisherZ(cor.test(subset(data_stats, data_stats$Study=="PREDICT")$`mGS_Reader1 (32)`, 
                 subset(data_stats, data_stats$Study=="PREDICT")$`Manual Segmentation IVH Volume (mL)`
                 , method = "pearson")$estimate)
FisherZ(cor.test(subset(data_stats, data_stats$Study=="PREDICT")$`mGS_Reader1 (32)`, 
                 subset(data_stats, data_stats$Study=="PREDICT")$`Manual Segmentation IVH Volume (mL)`
                 , method = "pearson")$conf.int[1])
FisherZ(cor.test(subset(data_stats, data_stats$Study=="PREDICT")$`mGS_Reader1 (32)`, 
                 subset(data_stats, data_stats$Study=="PREDICT")$`Manual Segmentation IVH Volume (mL)`
                 , method = "pearson")$conf.int[2])


#SPOTLIGHT
#IVHS score
cor.test(subset(data_stats, data_stats$Study=="SPOTLIGHT/STOP-IT")$IVHS_Reader1, 
         subset(data_stats, data_stats$Study=="SPOTLIGHT/STOP-IT")$`Manual Segmentation IVH Volume (mL)`
         , method = "pearson")
FisherZ(cor.test(subset(data_stats, data_stats$Study=="SPOTLIGHT/STOP-IT")$IVHS_Reader1, 
                 subset(data_stats, data_stats$Study=="SPOTLIGHT/STOP-IT")$`Manual Segmentation IVH Volume (mL)`
                 , method = "pearson")$estimate)
FisherZ(cor.test(subset(data_stats, data_stats$Study=="SPOTLIGHT/STOP-IT")$IVHS_Reader1, 
                 subset(data_stats, data_stats$Study=="SPOTLIGHT/STOP-IT")$`Manual Segmentation IVH Volume (mL)`
                 , method = "pearson")$conf.int[1])
FisherZ(cor.test(subset(data_stats, data_stats$Study=="SPOTLIGHT/STOP-IT")$IVHS_Reader1, 
                 subset(data_stats, data_stats$Study=="SPOTLIGHT/STOP-IT")$`Manual Segmentation IVH Volume (mL)`
                 , method = "pearson")$conf.int[2])


#oGS
cor.test(subset(data_stats, data_stats$Study=="SPOTLIGHT/STOP-IT")$`oGS_Reader1 (12)`, 
         subset(data_stats, data_stats$Study=="SPOTLIGHT/STOP-IT")$`Manual Segmentation IVH Volume (mL)`
         , method = "pearson")
FisherZ(cor.test(subset(data_stats, data_stats$Study=="SPOTLIGHT/STOP-IT")$`oGS_Reader1 (12)`, 
                 subset(data_stats, data_stats$Study=="SPOTLIGHT/STOP-IT")$`Manual Segmentation IVH Volume (mL)`
                 , method = "pearson")$estimate)
FisherZ(cor.test(subset(data_stats, data_stats$Study=="SPOTLIGHT/STOP-IT")$`oGS_Reader1 (12)`, 
                 subset(data_stats, data_stats$Study=="SPOTLIGHT/STOP-IT")$`Manual Segmentation IVH Volume (mL)`
                 , method = "pearson")$conf.int[1])
FisherZ(cor.test(subset(data_stats, data_stats$Study=="SPOTLIGHT/STOP-IT")$`oGS_Reader1 (12)`, 
                 subset(data_stats, data_stats$Study=="SPOTLIGHT/STOP-IT")$`Manual Segmentation IVH Volume (mL)`
                 , method = "pearson")$conf.int[2])

#mGS
cor.test(subset(data_stats, data_stats$Study=="SPOTLIGHT/STOP-IT")$`mGS_Reader1 (32)`, 
         subset(data_stats, data_stats$Study=="SPOTLIGHT/STOP-IT")$`Manual Segmentation IVH Volume (mL)`
         , method = "pearson")
FisherZ(cor.test(subset(data_stats, data_stats$Study=="SPOTLIGHT/STOP-IT")$`mGS_Reader1 (32)`, 
                 subset(data_stats, data_stats$Study=="SPOTLIGHT/STOP-IT")$`Manual Segmentation IVH Volume (mL)`
                 , method = "pearson")$estimate)
FisherZ(cor.test(subset(data_stats, data_stats$Study=="SPOTLIGHT/STOP-IT")$`mGS_Reader1 (32)`, 
                 subset(data_stats, data_stats$Study=="SPOTLIGHT/STOP-IT")$`Manual Segmentation IVH Volume (mL)`
                 , method = "pearson")$conf.int[1])
FisherZ(cor.test(subset(data_stats, data_stats$Study=="SPOTLIGHT/STOP-IT")$`mGS_Reader1 (32)`, 
                 subset(data_stats, data_stats$Study=="SPOTLIGHT/STOP-IT")$`Manual Segmentation IVH Volume (mL)`
                 , method = "pearson")$conf.int[2])


# Bland Altman Plots ----
#PREDICT
#IVHS estimate vs manual segmentation
IVHS_est_man_seg_PREDICT=(blandr.draw(subset(data_stats, data_stats$Study=="PREDICT")$`IVHS Estimated Volume (mL)`, 
                     subset(data_stats, data_stats$Study=="PREDICT")$`Manual Segmentation IVH Volume (mL)`, 
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
IVHS_est_man_seg_SPOTLIGHT_STOPIT=(blandr.draw(subset(data_stats, data_stats$Study=="SPOTLIGHT/STOP-IT")$`IVHS Estimated Volume (mL)`, 
                              subset(data_stats, data_stats$Study=="SPOTLIGHT/STOP-IT")$`Manual Segmentation IVH Volume (mL)`, 
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
                  + expand_limits(x = 60, y = 0)
                  + scale_x_continuous(expand = c(0, 1))
                  
                  + labs(title = element_blank())
)

#CNN vs manual segmentation


# Linear Regression ----
#PREDICT
#IVHS estimate vs manual segmentation
data_stats$IVHS_man_seg_avg=(data_stats$`IVHS Estimated Volume (mL)`+data_stats$`Manual Segmentation IVH Volume (mL)`)/2

lml=lm(IVHS_manual_diff ~ IVHS_man_seg_avg, data=subset(data_stats, data_stats$Study=="PREDICT"))
summary(lml)

#CNN vs manual segmentation



#SPOTLIGHT
#IVHS estimate vs manual segmentation
data_stats$IVHS_man_seg_avg=(data_stats$`IVHS Estimated Volume (mL)`+data_stats$`Manual Segmentation IVH Volume (mL)`)/2

lml=lm(IVHS_manual_diff ~ IVHS_man_seg_avg, data=subset(data_stats, data_stats$Study=="SPOTLIGHT/STOP-IT"))
summary(lml)

#CNN vs manual segmentation


# Hematoma expansion analysis ----






