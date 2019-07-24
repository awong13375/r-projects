library(googlesheets)
library(googledrive)
library(irr)
library(stringr)
library(blandr)
library(ggplot2)
library(cowplot)

for_gs <- gs_title("PREDICT ROI File Issues")


# PREDICT Data merging ------------------------------------------------------------
setwd("C:/Users/alexw/OneDrive/Dal Med/ICH")
pt_char=read.csv("PREDICT_baseline_data.csv")
alex_predict_sheet <- gs_read(for_gs, ws="Alex version of PREDICTv3")
X2=as.vector(alex_predict_sheet$X2)

baseline=c()
for (i in X2){
  if (grepl("24",i)){
    if (grepl("b",i)|grepl("B",i)){
      baseline=append(baseline, 0)
    }else{
      baseline=append(baseline, 1)
    }
    next
  }
  if ((grepl("B",i)|grepl("b",i))&!(grepl("f",i)|grepl("F",i))){
    baseline=append(baseline, 0)
  }else{
    baseline=append(baseline, 1)
  }
}
alex_predict_sheet=cbind(alex_predict_sheet, baseline)
sean_predict_sheet=cbind(sean_predict_sheet, baseline)

psite=as.character(as.vector(alex_predict_sheet$Psite))
pnum=as.character(as.vector(alex_predict_sheet$Pnum))
PID=paste(psite, pnum, sep="", collapse=NULL)

alex_predict_sheet=cbind(alex_predict_sheet, PID)
sean_predict_sheet=cbind(sean_predict_sheet, PID)

baseline_predict_AW=subset(alex_predict_sheet, alex_predict_sheet$baseline==0)
followup_predict_AW=subset(alex_predict_sheet, alex_predict_sheet$baseline==1)
baseline_predict_SN=subset(sean_predict_sheet, sean_predict_sheet$baseline==0)
followup_predict_SN=subset(sean_predict_sheet, sean_predict_sheet$baseline==1)

keeps=c("PID","Irregular?","ABC-A","ABC-B","ABC-C","ABC/2","QuantomoVol")
keep=c("PID","Irregular?","ABC-A","ABC-B","ABC-C","ABC/2")
baseline_predict_AW=baseline_predict_AW[keeps]
baseline_predict_SN=baseline_predict_SN[keep]
colnames(baseline_predict_AW)=c("PID","b_Irregular?_AW","b_ABC-A_AW","b_ABC-B_AW","b_ABC-C_AW","b_ABC/2_AW","b_Quantomovol")
colnames(baseline_predict_SN)=c("PID","b_Irregular?_SN","b_ABC-A_SN","b_ABC-B_SN","b_ABC-C_SN","b_ABC/2_SN")
followup_predict_AW=followup_predict_AW[keeps]
followup_predict_SN=followup_predict_SN[keep]
colnames(followup_predict_AW)=c("PID","fu_Irregular?_AW","fu_ABC-A_AW","fu_ABC-B_AW","fu_ABC-C_AW","fu_ABC/2_AW","fu_Quantomovol")
colnames(followup_predict_SN)=c("PID","fu_Irregular?_SN","fu_ABC-A_SN","fu_ABC-B_SN","fu_ABC-C_SN","fu_ABC/2_SN")

baseline_predict=merge(baseline_predict_AW, baseline_predict_SN, by="PID")
followup_predict=merge(followup_predict_AW, followup_predict_SN, by="PID")
combined=merge(baseline_predict, followup_predict, by="PID", all.x=TRUE)

pt_char$?..patient_site=str_pad(pt_char$?..patient_site, 2, pad="0")
pt_char$patient_study=str_pad(pt_char$patient_study, 3, pad="0")
pt_char$PID=paste(pt_char$?..patient_site, pt_char$patient_study, sep="", collapse=NULL)

predict_pt_char=merge(pt_char, combined, by="PID", all.y=TRUE)

#write.csv(predict_pt_char, "PREDICTv3 merged database.csv")


# SPOTLIGHT Data merging --------------------------------------------------
setwd("C:/Users/alexw/OneDrive/Dal Med/ICH")
pt_char=read.csv("SPOTLIGHT_Baseline_data.csv")
pt_char$?..Subject=str_pad(pt_char$?..Subject, 6, pad="0")
colnames(pt_char)[1]<-"PID"


alex_spotlight_sheet <- gs_read(for_gs, ws="Alex version SPOTLIGHT v1")
sean_spotlight_sheet <- gs_read(for_gs, ws="Sean version SPOTLIGHT v1")

CASE=as.vector(alex_spotlight_sheet$CASE)

baseline=c()
for (i in CASE){
  if (grepl("1hr",i)|grepl("1HR",i)|grepl("Post Dose",i)|grepl("2HR",i)){
    baseline=append(baseline, 1)
    next
  }
  if (grepl("24 HR CT 1703H",i)){
    baseline=append(baseline, 3)
    next
  }
  if (grepl("24HR",i)|grepl("24hr",i)|(grepl("24 hour",i)|grepl("24 HR",i)|grepl("24 hr",i))){
    baseline=append(baseline, 2)
    next
  }
  if (grepl("Base",i)|grepl("base",i)|grepl("BL",i)){
    baseline=append(baseline, 0)
  }else{
    baseline=append(baseline, 2)
  }
}

alex_spotlight_sheet=cbind(alex_spotlight_sheet, baseline)
sean_spotlight_sheet=cbind(sean_spotlight_sheet, baseline)

psite=as.character(as.vector(alex_spotlight_sheet$Psite))
pnum=as.character(as.vector(alex_spotlight_sheet$Pnum))
PID=paste(psite, pnum, sep="", collapse=NULL)

alex_spotlight_sheet=cbind(alex_spotlight_sheet, PID)
sean_spotlight_sheet=cbind(sean_spotlight_sheet, PID)

baseline_spotlight_AW=subset(alex_spotlight_sheet, alex_spotlight_sheet$baseline==0)
onehr_spotlight_AW=subset(alex_spotlight_sheet, alex_spotlight_sheet$baseline==1)
followup_spotlight_AW=subset(alex_spotlight_sheet, alex_spotlight_sheet$baseline==2)
followup2_spotlight_AW=subset(alex_spotlight_sheet, alex_spotlight_sheet$baseline==3)
baseline_spotlight_SN=subset(sean_spotlight_sheet, sean_spotlight_sheet$baseline==0)
onehr_spotlight_SN=subset(sean_spotlight_sheet, sean_spotlight_sheet$baseline==1)
followup_spotlight_SN=subset(sean_spotlight_sheet, sean_spotlight_sheet$baseline==2)
followup2_spotlight_SN=subset(sean_spotlight_sheet, sean_spotlight_sheet$baseline==3)


keeps=c("PID","Irregular","ABC-A","ABC-B","ABC-C","ABC/2","Quantomo")
keep=c("PID","Irregular","ABC-A","ABC-B","ABC-C","ABC/2")

baseline_spotlight_AW=baseline_spotlight_AW[keeps]
baseline_spotlight_SN=baseline_spotlight_SN[keep]
colnames(baseline_spotlight_AW)=c("PID","b_Irregular_AW","b_ABC-A_AW","b_ABC-B_AW","b_ABC-C_AW","b_ABC/2_AW","b_Quantomo")
colnames(baseline_spotlight_SN)=c("PID","b_Irregular_SN","b_ABC-A_SN","b_ABC-B_SN","b_ABC-C_SN","b_ABC/2_SN")

onehr_spotlight_AW=onehr_spotlight_AW[keeps]
onehr_spotlight_SN=onehr_spotlight_SN[keep]
colnames(onehr_spotlight_AW)=c("PID","1hr_Irregular_AW","1hr_ABC-A_AW","1hr_ABC-B_AW","1hr_ABC-C_AW","1hr_ABC/2_AW","1hr_Quantomo")
colnames(onehr_spotlight_SN)=c("PID","1hr_Irregular_SN","1hr_ABC-A_SN","1hr_ABC-B_SN","1hr_ABC-C_SN","1hr_ABC/2_SN")


followup_spotlight_AW=followup_spotlight_AW[keeps]
followup_spotlight_SN=followup_spotlight_SN[keep]
colnames(followup_spotlight_AW)=c("PID","fu_Irregular?_AW","fu_ABC-A_AW","fu_ABC-B_AW","fu_ABC-C_AW","fu_ABC/2_AW","fu_Quantomo")
colnames(followup_spotlight_SN)=c("PID","fu_Irregular?_SN","fu_ABC-A_SN","fu_ABC-B_SN","fu_ABC-C_SN","fu_ABC/2_SN")

followup2_spotlight_AW=followup2_spotlight_AW[keeps]
followup2_spotlight_SN=followup2_spotlight_SN[keep]
colnames(followup2_spotlight_AW)=c("PID","fu2_Irregular?_AW","fu2_ABC-A_AW","fu2_ABC-B_AW","fu2_ABC-C_AW","fu2_ABC/2_AW","fu2_Quantomo")
colnames(followup2_spotlight_SN)=c("PID","fu2_Irregular?_SN","fu2_ABC-A_SN","fu2_ABC-B_SN","fu2_ABC-C_SN","fu2_ABC/2_SN")


baseline_spotlight=merge(baseline_spotlight_AW, baseline_spotlight_SN, by="PID")
onehr_spotlight=merge(onehr_spotlight_AW, onehr_spotlight_SN, by="PID")
followup_spotlight=merge(followup_spotlight_AW, followup_spotlight_SN, by="PID")
followup2_spotlight=merge(followup2_spotlight_AW, followup2_spotlight_SN, by="PID")

combined2=merge(baseline_spotlight, onehr_spotlight, by="PID", all=TRUE)
combined1=merge(combined2, followup_spotlight, by="PID", all=TRUE)
combined=merge(combined1, followup2_spotlight, by="PID", all=TRUE)

spotlight_pt_char=merge(pt_char, combined, by="PID", all.y=TRUE)

#write.csv(spotlight_pt_char, "SPOTLIGHT database merged.csv")


# Table 1 -----------------------------------------------------------------
predict=read.csv("PREDICTv3 merged database.csv")
spotlight=read.csv("SPOTLIGHT database merged.csv")
predict[predict==""]<-NA
spotlight[spotlight==""]<-NA

predict$study=c("Predict")
spotlight$study=c("Spotlight")

keep=c("PID","study","ageatevent","gender","hypertension","diabetes","on_antiplatelet_therapy","on_warfarin","ICH_lobar.base",
       "ICH_deep.base","ICH_post.base","IVH.base","Time.from.STROKE_ONSET.to.INITIAL_CT_SCAN")

predict=predict[keep]
spotlight=spotlight[keep]
combined=rbind(predict, spotlight)

studytype=c("Predict","Spotlight")

for(studys in studytype){
  column=c()
  datastudy=subset(combined, combined[,"study"]==studys)
  
  column=append(column, nrow(datastudy))
  
  column=append(column, mean(datastudy$ageatevent, na.rm=TRUE))
  column=append(column, sd(datastudy$ageatevent, na.rm=TRUE))
  
  column=append(column, nrow(subset(datastudy, datastudy$gender=="M")))
  column=append(column, nrow(subset(datastudy, datastudy$gender=="M"))/nrow(datastudy)*100)
  column=append(column, nrow(subset(datastudy, datastudy$gender=="F")))
  column=append(column, nrow(subset(datastudy, datastudy$gender=="F"))/nrow(datastudy)*100)
  column=append(column, nrow(subset(datastudy, is.na(datastudy$gender))))
  column=append(column, nrow(subset(datastudy, is.na(datastudy$gender)))/nrow(datastudy)*100)
  
  
  column=append(column, nrow(subset(datastudy, datastudy$hypertension=="1")))
  column=append(column, nrow(subset(datastudy, datastudy$hypertension=="1"))/nrow(datastudy)*100)
  column=append(column, nrow(subset(datastudy, datastudy$hypertension=="0")))
  column=append(column, nrow(subset(datastudy, datastudy$hypertension=="0"))/nrow(datastudy)*100)
  column=append(column, nrow(subset(datastudy, is.na(datastudy$hypertension))))
  column=append(column, nrow(subset(datastudy, is.na(datastudy$hypertension)))/nrow(datastudy)*100)
  
  
  column=append(column, nrow(subset(datastudy, datastudy$diabetes=="1")))
  column=append(column, nrow(subset(datastudy, datastudy$diabetes=="1"))/nrow(datastudy)*100)
  column=append(column, nrow(subset(datastudy, datastudy$diabetes=="0")))
  column=append(column, nrow(subset(datastudy, datastudy$diabetes=="0"))/nrow(datastudy)*100)
  column=append(column, nrow(subset(datastudy, is.na(datastudy$diabetes))))
  column=append(column, nrow(subset(datastudy, is.na(datastudy$diabetes)))/nrow(datastudy)*100)
  
  column=append(column, nrow(subset(datastudy, datastudy$on_antiplatelet_therapy=="1")))
  column=append(column, nrow(subset(datastudy, datastudy$on_antiplatelet_therapy=="1"))/nrow(datastudy)*100)
  column=append(column, nrow(subset(datastudy, datastudy$on_antiplatelet_therapy=="0")))
  column=append(column, nrow(subset(datastudy, datastudy$on_antiplatelet_therapy=="0"))/nrow(datastudy)*100)
  column=append(column, nrow(subset(datastudy, is.na(datastudy$on_antiplatelet_therapy))))
  column=append(column, nrow(subset(datastudy, is.na(datastudy$on_antiplatelet_therapy)))/nrow(datastudy)*100)
  
  column=append(column, nrow(subset(datastudy, datastudy$on_warfarin=="1")))
  column=append(column, nrow(subset(datastudy, datastudy$on_warfarin=="1"))/nrow(datastudy)*100)
  column=append(column, nrow(subset(datastudy, datastudy$on_warfarin=="0")))
  column=append(column, nrow(subset(datastudy, datastudy$on_warfarin=="0"))/nrow(datastudy)*100)
  column=append(column, nrow(subset(datastudy, is.na(datastudy$on_warfarin))))
  column=append(column, nrow(subset(datastudy, is.na(datastudy$on_warfarin)))/nrow(datastudy)*100)
  
  column=append(column, nrow(subset(datastudy, datastudy$ICH_lobar.base=="1")))
  column=append(column, nrow(subset(datastudy, datastudy$ICH_lobar.base=="1"))/nrow(datastudy)*100)
  column=append(column, nrow(subset(datastudy, datastudy$ICH_lobar.base=="0")))
  column=append(column, nrow(subset(datastudy, datastudy$ICH_lobar.base=="0"))/nrow(datastudy)*100)
  column=append(column, nrow(subset(datastudy, is.na(datastudy$ICH_lobar.base))))
  column=append(column, nrow(subset(datastudy, is.na(datastudy$ICH_lobar.base)))/nrow(datastudy)*100)
  
  column=append(column, nrow(subset(datastudy, datastudy$ICH_deep.base=="1")))
  column=append(column, nrow(subset(datastudy, datastudy$ICH_deep.base=="1"))/nrow(datastudy)*100)
  column=append(column, nrow(subset(datastudy, datastudy$ICH_deep.base=="0")))
  column=append(column, nrow(subset(datastudy, datastudy$ICH_deep.base=="0"))/nrow(datastudy)*100)
  column=append(column, nrow(subset(datastudy, is.na(datastudy$ICH_deep.base))))
  column=append(column, nrow(subset(datastudy, is.na(datastudy$ICH_deep.base)))/nrow(datastudy)*100)
  
  column=append(column, nrow(subset(datastudy, datastudy$ICH_post.base=="1")))
  column=append(column, nrow(subset(datastudy, datastudy$ICH_post.base=="1"))/nrow(datastudy)*100)
  column=append(column, nrow(subset(datastudy, datastudy$ICH_post.base=="0")))
  column=append(column, nrow(subset(datastudy, datastudy$ICH_post.base=="0"))/nrow(datastudy)*100)
  column=append(column, nrow(subset(datastudy, is.na(datastudy$ICH_post.base))))
  column=append(column, nrow(subset(datastudy, is.na(datastudy$ICH_post.base)))/nrow(datastudy)*100)
  
  column=append(column, nrow(subset(datastudy, datastudy$IVH.base=="1")))
  column=append(column, nrow(subset(datastudy, datastudy$IVH.base=="1"))/nrow(datastudy)*100)
  column=append(column, nrow(subset(datastudy, datastudy$IVH.base=="0")))
  column=append(column, nrow(subset(datastudy, datastudy$IVH.base=="0"))/nrow(datastudy)*100)
  column=append(column, nrow(subset(datastudy, is.na(datastudy$IVH.base))))
  column=append(column, nrow(subset(datastudy, is.na(datastudy$IVH.base)))/nrow(datastudy)*100)
  
  column=append(column, mean(datastudy$Time.from.STROKE_ONSET.to.INITIAL_CT_SCAN, na.rm=TRUE))
  column=append(column, sd(datastudy$Time.from.STROKE_ONSET.to.INITIAL_CT_SCAN, na.rm=TRUE))
  
  if (studys=="Predict"){
    outcomes=as.data.frame(column)
  }else{
    outcomes=cbind(outcomes, column)
  }
  
  if (studys=="Spotlight"){
    column=c()
    datastudy=combined
    
    column=append(column, nrow(datastudy))
    
    column=append(column, mean(datastudy$ageatevent, na.rm=TRUE))
    column=append(column, sd(datastudy$ageatevent, na.rm=TRUE))
    
    column=append(column, nrow(subset(datastudy, datastudy$gender=="M")))
    column=append(column, nrow(subset(datastudy, datastudy$gender=="M"))/nrow(datastudy)*100)
    column=append(column, nrow(subset(datastudy, datastudy$gender=="F")))
    column=append(column, nrow(subset(datastudy, datastudy$gender=="F"))/nrow(datastudy)*100)
    column=append(column, nrow(subset(datastudy, is.na(datastudy$gender))))
    column=append(column, nrow(subset(datastudy, is.na(datastudy$gender)))/nrow(datastudy)*100)
    
    
    column=append(column, nrow(subset(datastudy, datastudy$hypertension=="1")))
    column=append(column, nrow(subset(datastudy, datastudy$hypertension=="1"))/nrow(datastudy)*100)
    column=append(column, nrow(subset(datastudy, datastudy$hypertension=="0")))
    column=append(column, nrow(subset(datastudy, datastudy$hypertension=="0"))/nrow(datastudy)*100)
    column=append(column, nrow(subset(datastudy, is.na(datastudy$hypertension))))
    column=append(column, nrow(subset(datastudy, is.na(datastudy$hypertension)))/nrow(datastudy)*100)
    
    
    column=append(column, nrow(subset(datastudy, datastudy$diabetes=="1")))
    column=append(column, nrow(subset(datastudy, datastudy$diabetes=="1"))/nrow(datastudy)*100)
    column=append(column, nrow(subset(datastudy, datastudy$diabetes=="0")))
    column=append(column, nrow(subset(datastudy, datastudy$diabetes=="0"))/nrow(datastudy)*100)
    column=append(column, nrow(subset(datastudy, is.na(datastudy$diabetes))))
    column=append(column, nrow(subset(datastudy, is.na(datastudy$diabetes)))/nrow(datastudy)*100)
    
    column=append(column, nrow(subset(datastudy, datastudy$on_antiplatelet_therapy=="1")))
    column=append(column, nrow(subset(datastudy, datastudy$on_antiplatelet_therapy=="1"))/nrow(datastudy)*100)
    column=append(column, nrow(subset(datastudy, datastudy$on_antiplatelet_therapy=="0")))
    column=append(column, nrow(subset(datastudy, datastudy$on_antiplatelet_therapy=="0"))/nrow(datastudy)*100)
    column=append(column, nrow(subset(datastudy, is.na(datastudy$on_antiplatelet_therapy))))
    column=append(column, nrow(subset(datastudy, is.na(datastudy$on_antiplatelet_therapy)))/nrow(datastudy)*100)
    
    column=append(column, nrow(subset(datastudy, datastudy$on_warfarin=="1")))
    column=append(column, nrow(subset(datastudy, datastudy$on_warfarin=="1"))/nrow(datastudy)*100)
    column=append(column, nrow(subset(datastudy, datastudy$on_warfarin=="0")))
    column=append(column, nrow(subset(datastudy, datastudy$on_warfarin=="0"))/nrow(datastudy)*100)
    column=append(column, nrow(subset(datastudy, is.na(datastudy$on_warfarin))))
    column=append(column, nrow(subset(datastudy, is.na(datastudy$on_warfarin)))/nrow(datastudy)*100)
    
    column=append(column, nrow(subset(datastudy, datastudy$ICH_lobar.base=="1")))
    column=append(column, nrow(subset(datastudy, datastudy$ICH_lobar.base=="1"))/nrow(datastudy)*100)
    column=append(column, nrow(subset(datastudy, datastudy$ICH_lobar.base=="0")))
    column=append(column, nrow(subset(datastudy, datastudy$ICH_lobar.base=="0"))/nrow(datastudy)*100)
    column=append(column, nrow(subset(datastudy, is.na(datastudy$ICH_lobar.base))))
    column=append(column, nrow(subset(datastudy, is.na(datastudy$ICH_lobar.base)))/nrow(datastudy)*100)
    
    column=append(column, nrow(subset(datastudy, datastudy$ICH_deep.base=="1")))
    column=append(column, nrow(subset(datastudy, datastudy$ICH_deep.base=="1"))/nrow(datastudy)*100)
    column=append(column, nrow(subset(datastudy, datastudy$ICH_deep.base=="0")))
    column=append(column, nrow(subset(datastudy, datastudy$ICH_deep.base=="0"))/nrow(datastudy)*100)
    column=append(column, nrow(subset(datastudy, is.na(datastudy$ICH_deep.base))))
    column=append(column, nrow(subset(datastudy, is.na(datastudy$ICH_deep.base)))/nrow(datastudy)*100)
    
    column=append(column, nrow(subset(datastudy, datastudy$ICH_post.base=="1")))
    column=append(column, nrow(subset(datastudy, datastudy$ICH_post.base=="1"))/nrow(datastudy)*100)
    column=append(column, nrow(subset(datastudy, datastudy$ICH_post.base=="0")))
    column=append(column, nrow(subset(datastudy, datastudy$ICH_post.base=="0"))/nrow(datastudy)*100)
    column=append(column, nrow(subset(datastudy, is.na(datastudy$ICH_post.base))))
    column=append(column, nrow(subset(datastudy, is.na(datastudy$ICH_post.base)))/nrow(datastudy)*100)
    
    column=append(column, nrow(subset(datastudy, datastudy$IVH.base=="1")))
    column=append(column, nrow(subset(datastudy, datastudy$IVH.base=="1"))/nrow(datastudy)*100)
    column=append(column, nrow(subset(datastudy, datastudy$IVH.base=="0")))
    column=append(column, nrow(subset(datastudy, datastudy$IVH.base=="0"))/nrow(datastudy)*100)
    column=append(column, nrow(subset(datastudy, is.na(datastudy$IVH.base))))
    column=append(column, nrow(subset(datastudy, is.na(datastudy$IVH.base)))/nrow(datastudy)*100)
    
    column=append(column, mean(datastudy$Time.from.STROKE_ONSET.to.INITIAL_CT_SCAN, na.rm=TRUE))
    column=append(column, sd(datastudy$Time.from.STROKE_ONSET.to.INITIAL_CT_SCAN, na.rm=TRUE))
    
    outcomes=cbind(outcomes, column)
  }
}

colnames(outcomes)=c("Predict","Spotlight","Combined")
rownames(outcomes)=c("n","mean age","sd age",
                     "M","M%","F","F%","NA gender","NA gender%",
                     "Y HTN","Y HTN%","N HTN","N HTN%", "NA HTN","NA HTN%",
                     "Y DM","Y DM%","N DM","N DM%", "NA DM","NA DM%",
                     "Y antiplate","Y antiplate %","N antiplate","N antiplate%","NA antiplate","NA antiplate%",
                     "Y warfarin","Y warfarin %","N warfarin","N warfarin%","NA warfarin","NA warfarin%",
                     "Y ICHlobe","Y ICHlobe%","N ICHlobe","N ICHlobe%","NA ICHlobe","NA ICHlobe%",
                     "Y ICHdeep","Y ICHdeep%","N ICHdeep","N ICHdeep%","NA ICHdeep","NA ICHdeep%",
                     "Y ICHpost","Y ICHpost%","N ICHpost","N ICHpost%","NA ICHpost","NA ICHpost%",
                     "Y IVH","Y IVH%","N IVH","N IVH%","NA IVH","NA IVH%",
                     "mean Time to CT","sd time to CT")

#write.csv(outcomes, "C:/Users/alexw/OneDrive/Dal Med/ICH/Results/Table 1 values.csv")

table1stats=c()
predict=subset(combined, combined$study=="Predict")
spotlight=subset(combined, combined$study=="Spotlight")
table1stats=append(table1stats, t.test(ageatevent ~ study, data=combined, na.rm=TRUE)$p.value)

M=as.table(cbind(c(nrow(subset(predict, predict$gender=="M")),nrow(subset(predict, predict$gender=="F"))),
                 c(nrow(subset(spotlight, spotlight$gender=="M")),nrow(subset(spotlight,spotlight$gender=="F")))
))
table1stats=append(table1stats,chisq.test(M)$p.value)

M=as.table(cbind(c(nrow(subset(predict, predict$hypertension==1)),nrow(subset(predict, predict$hypertension==0))),
                 c(nrow(subset(spotlight, spotlight$hypertension==1)),nrow(subset(spotlight,spotlight$hypertension==0)))
))
table1stats=append(table1stats,chisq.test(M)$p.value)

M=as.table(cbind(c(nrow(subset(predict, predict$diabetes==1)),nrow(subset(predict, predict$diabetes==0))),
                 c(nrow(subset(spotlight, spotlight$diabetes==1)),nrow(subset(spotlight,spotlight$diabetes==0)))
))
table1stats=append(table1stats,chisq.test(M)$p.value)

M=as.table(cbind(c(nrow(subset(predict, predict$on_antiplatelet_therapy==1)),nrow(subset(predict, predict$on_antiplatelet_therapy==0))),
                 c(nrow(subset(spotlight, spotlight$on_antiplatelet_therapy==1)),nrow(subset(spotlight,spotlight$on_antiplatelet_therapy==0)))
))
table1stats=append(table1stats,fisher.test(M)$p.value)

M=as.table(cbind(c(nrow(subset(predict, predict$on_warfarin==1)),nrow(subset(predict, predict$on_warfarin==0))),
                 c(nrow(subset(spotlight, spotlight$on_warfarin==1)),nrow(subset(spotlight,spotlight$on_warfarin==0)))
))
table1stats=append(table1stats,fisher.test(M)$p.value)

M=as.table(cbind(c(nrow(subset(predict, predict$ICH_lobar.base==1)),nrow(subset(predict, predict$ICH_lobar.base==0))),
                 c(nrow(subset(spotlight, spotlight$ICH_lobar.base==1)),nrow(subset(spotlight,spotlight$ICH_lobar.base==0)))
))
table1stats=append(table1stats,chisq.test(M)$p.value)

M=as.table(cbind(c(nrow(subset(predict, predict$ICH_deep.base==1)),nrow(subset(predict, predict$ICH_deep.base==0))),
                 c(nrow(subset(spotlight, spotlight$ICH_deep.base==1)),nrow(subset(spotlight,spotlight$ICH_deep.base==0)))
))
table1stats=append(table1stats,chisq.test(M)$p.value)

M=as.table(cbind(c(nrow(subset(predict, predict$ICH_post.base==1)),nrow(subset(predict, predict$ICH_post.base==0))),
                 c(nrow(subset(spotlight, spotlight$ICH_post.base==1)),nrow(subset(spotlight,spotlight$ICH_post.base==0)))
))
table1stats=append(table1stats,fisher.test(M)$p.value)

M=as.table(cbind(c(nrow(subset(predict, predict$IVH.base==1)),nrow(subset(predict, predict$IVH.base==0))),
                 c(nrow(subset(spotlight, spotlight$IVH.base==1)),nrow(subset(spotlight,spotlight$IVH.base==0)))
))
table1stats=append(table1stats,chisq.test(M)$p.value)

table1stats=append(table1stats, t.test(Time.from.STROKE_ONSET.to.INITIAL_CT_SCAN ~ study, data=combined, na.rm=TRUE)$p.value)


table1stats=as.data.frame(table1stats)
rownames(table1stats)=c("ageatevent","gender","hypertension","diabetes","on_antiplatelet_therapy","on_warfarin","ICH_lobar.base",
                        "ICH_deep.base","ICH_post.base","IVH.base","Time.from.STROKE_ONSET.to.INITIAL_CT_SCAN")
colnames(table1stats)=c("p-value")

#write.csv(table1stats, "C:/Users/alexw/OneDrive/Dal Med/ICH/Results/Table 1 stats.csv")


# Table 2 -----------------------------------------------------------------

alex_predict_sheet <- gs_read(for_gs, ws="Alex version of PREDICTv3")
sean_predict_sheet <- gs_read(for_gs, ws="Sean version of PREDICTv3")

alexpredict=as.vector(alex_predict_sheet$`ABC/2`)
seanpredict=as.vector(sean_predict_sheet$`ABC/2`)
quantomopredict=as.vector(sean_predict_sheet$QuantomoVol)

volumespredict=data.frame(alexpredict, seanpredict)
volumespredict$alexpredict=as.numeric(as.character(volumespredict$alexpredict))
volumespredict$seanpredict=as.numeric(as.character(volumespredict$seanpredict))
volumespredict$average=rowMeans(volumespredict)
volumespredict=cbind(volumespredict, quantomopredict)
volumespredict$quantomopredict=as.numeric(as.character(volumespredict$quantomopredict))
volumespredict$diff=volumespredict$quantomopredict-volumespredict$average
volumespredict$study="Predict"
colnames(volumespredict)=c("alex","sean","avg","quantomo","diff","study")

alex_spotlight_sheet <- gs_read(for_gs, ws="Alex version SPOTLIGHT v1")
sean_spotlight_sheet <- gs_read(for_gs, ws="Sean version SPOTLIGHT v1")

alexspotlight=as.vector(alex_spotlight_sheet$`ABC/2`)
seanspotlight=as.vector(sean_spotlight_sheet$`ABC/2`)
quantomospotlight=as.vector(sean_spotlight_sheet$Quantomo)

volumesspotlight=data.frame(alexspotlight, seanspotlight)
volumesspotlight$alexspotlight=as.numeric(as.character(volumesspotlight$alexspotlight))
volumesspotlight$seanspotlight=as.numeric(as.character(volumesspotlight$seanspotlight))
volumesspotlight$average=rowMeans(volumesspotlight)
volumesspotlight=cbind(volumesspotlight, quantomospotlight)
volumesspotlight$quantomospotlight=as.numeric(as.character(volumesspotlight$quantomospotlight))
volumesspotlight$diff=volumesspotlight$quantomospotlight-volumesspotlight$average
volumesspotlight$study="Spotlight"
colnames(volumesspotlight)=c("alex","sean","avg","quantomo","diff","study")

volumes=rbind(volumespredict, volumesspotlight)


studytype=c("Predict","Spotlight")

for(studys in studytype){
  column=c()
  volumesstudy=subset(volumes, volumes[,"study"]==studys)
  
  column=append(column, nrow(volumesstudy))
  
  column=append(column, range(volumesstudy$avg))
  column=append(column, range(volumesstudy$quantomo))
  column=append(column, range(volumesstudy$diff))
  
  column=append(column, mean(volumesstudy$avg))
  column=append(column, sd(volumesstudy$avg))
  column=append(column, mean(volumesstudy$quantomo))
  column=append(column, sd(volumesstudy$quantomo))
  column=append(column, mean(volumesstudy$diff))
  column=append(column, sd(volumesstudy$diff))
  
  column=append(column, median(volumesstudy$avg))
  column=append(column, median(volumesstudy$quantomo))
  column=append(column, median(volumesstudy$diff))
  
  column=append(column, quantile(volumesstudy$avg, 1/4))
  column=append(column, quantile(volumesstudy$avg, 3/4))
  column=append(column, quantile(volumesstudy$quantomo, 1/4))
  column=append(column, quantile(volumesstudy$quantomo, 3/4))
  column=append(column, quantile(volumesstudy$diff, 1/4))
  column=append(column, quantile(volumesstudy$diff, 3/4))
  
  
  if (studys=="Predict"){
    table2=as.data.frame(column)
  }else{
    table2=cbind(table2, column)
  }
  
  if (studys=="Spotlight"){
    column=c()
    volumesstudy=volumes
    
    column=append(column, nrow(volumesstudy))
    
    column=append(column, range(volumesstudy$avg))
    column=append(column, range(volumesstudy$quantomo))
    column=append(column, range(volumesstudy$diff))
    
    column=append(column, mean(volumesstudy$avg))
    column=append(column, sd(volumesstudy$avg))
    column=append(column, mean(volumesstudy$quantomo))
    column=append(column, sd(volumesstudy$quantomo))
    column=append(column, mean(volumesstudy$diff))
    column=append(column, sd(volumesstudy$diff))
    
    column=append(column, median(volumesstudy$avg))
    column=append(column, median(volumesstudy$quantomo))
    column=append(column, median(volumesstudy$diff))
    
    column=append(column, quantile(volumesstudy$avg, 1/4))
    column=append(column, quantile(volumesstudy$avg, 3/4))
    column=append(column, quantile(volumesstudy$quantomo, 1/4))
    column=append(column, quantile(volumesstudy$quantomo, 3/4))
    column=append(column, quantile(volumesstudy$diff, 1/4))
    column=append(column, quantile(volumesstudy$diff, 3/4))
    
    table2=cbind(table2, column)
  }
}

colnames(table2)=c("Predict","Spotlight","Combined")
rownames(table2)=c("n","min avg","max avg","min quantomo","max quantomo","min diff","max diff",
                     "mean avg","sd avg","mean quantomo","sd quantomo","mean diff","sd diff",
                     "median avg","median quantomo","median diff",
                     "Q1 avg","Q3 avg","Q1 quantomo","Q3 quantomo","Q1 diff","Q3 diff")

#write.csv(table2,"C:/Users/alexw/OneDrive/Dal Med/ICH/Results/Table 2 values.csv")

# ICC Calculations --------------------------------------------------------

icc(data.frame(subset(volumes, volumes$study=="Predict")$alex,
               subset(volumes, volumes$study=="Predict")$sean),
    model="twoway",type="agreement")

icc(data.frame(subset(volumes, volumes$study=="Predict")$alex,
               subset(volumes, volumes$study=="Predict")$quantomo),
    model="twoway",type="agreement")

icc(data.frame(subset(volumes, volumes$study=="Predict")$sean,
               subset(volumes, volumes$study=="Predict")$quantomo),
    model="twoway",type="agreement")

icc(data.frame(subset(volumes, volumes$study=="Predict")$avg,
               subset(volumes, volumes$study=="Predict")$quantomo),
    model="twoway",type="agreement")

icc(data.frame(subset(volumes, volumes$study=="Spotlight")$alex,
               subset(volumes, volumes$study=="Spotlight")$sean),
    model="twoway",type="agreement")

icc(data.frame(subset(volumes, volumes$study=="Spotlight")$alex,
               subset(volumes, volumes$study=="Spotlight")$quantomo),
    model="twoway",type="agreement")

icc(data.frame(subset(volumes, volumes$study=="Spotlight")$sean,
               subset(volumes, volumes$study=="Spotlight")$quantomo),
    model="twoway",type="agreement")

icc(data.frame(subset(volumes, volumes$study=="Spotlight")$avg,
               subset(volumes, volumes$study=="Spotlight")$quantomo),
    model="twoway",type="agreement")

icc(data.frame(volumes$alex, volumes$sean), model="twoway",type="agreement")
icc(data.frame(volumes$alex, volumes$quantomo), model="twoway",type="agreement")
icc(data.frame(volumes$sean, volumes$quantomo), model="twoway",type="agreement")
icc(data.frame(volumes$avg, volumes$quantomo), model="twoway",type="agreement")



# Bland Altman graphs -----------------------------------------------------
ymin=-50
ymax=150

Predict_AW_SN=(blandr.draw(subset(volumes, volumes$study=="Predict")$alex, 
                           subset(volumes, volumes$study=="Predict")$sean, 
                           sig.level = 0.95, LoA.mode = 1, ciDisplay = FALSE,
                           ciShading = FALSE,
                           lowest_y_axis = ymin, highest_y_axis = ymax, point_size = 0.8,
                           overlapping = FALSE, plotter = "ggplot", x.plot.mode = "means",
                           y.plot.mode = "difference", plotProportionalBias = FALSE,
                           plotProportionalBias.se = TRUE, assume.differences.are.normal = TRUE) 
               + xlab("Mean ICH volume (mL)")
               + ylab("ABC/2 - ABC/2 (mL)")
               + theme(
                 panel.border = element_blank(),  
                 panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank(),
                 panel.background = element_blank(),
                 axis.line = element_line(colour = "grey")
               )
               + expand_limits(x = 200, y = 0)
               + scale_x_continuous(expand = c(0, 1))
               + ylim(ymin, ymax)
               + labs(title = element_blank())
)

Predict_AW_Quant=(blandr.draw(subset(volumes, volumes$study=="Predict")$alex, 
                              subset(volumes, volumes$study=="Predict")$quantomo, 
                              sig.level = 0.95, LoA.mode = 1, annotate = FALSE, ciDisplay = FALSE,
                              ciShading = FALSE, normalLow = FALSE, normalHigh = FALSE,
                              lowest_y_axis = FALSE, highest_y_axis = FALSE, point_size = 0.8,
                              overlapping = FALSE, plotter = "ggplot", x.plot.mode = "means",
                              y.plot.mode = "difference", plotProportionalBias = FALSE,
                              plotProportionalBias.se = TRUE, assume.differences.are.normal = TRUE) 
                  + xlab("Mean ICH volume (mL)")
                  + ylab("ABC/2 - Quantomo (mL)")
                  + theme(
                    panel.border = element_blank(),  
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.background = element_blank(),
                    axis.line = element_line(colour = "grey")
                  )
                  + expand_limits(x = 200, y = 0)
                  + scale_x_continuous(expand = c(0, 1))
                  + ylim(ymin, ymax)
                  + labs(title = element_blank())
)
Predict_SN_Quant=(blandr.draw(subset(volumes, volumes$study=="Predict")$sean, 
                              subset(volumes, volumes$study=="Predict")$quantomo, 
                              sig.level = 0.95, LoA.mode = 1, annotate = FALSE, ciDisplay = FALSE,
                              ciShading = FALSE, normalLow = FALSE, normalHigh = FALSE,
                              lowest_y_axis = FALSE, highest_y_axis = FALSE, point_size = 0.8,
                              overlapping = FALSE, plotter = "ggplot", x.plot.mode = "means",
                              y.plot.mode = "difference", plotProportionalBias = FALSE,
                              plotProportionalBias.se = TRUE, assume.differences.are.normal = TRUE) 
                  + xlab("Mean ICH volume (mL)")
                  + ylab("ABC/2 - Quantomo (mL)")
                  + theme(
                    panel.border = element_blank(),  
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.background = element_blank(),
                    axis.line = element_line(colour = "grey")
                  )
                  + expand_limits(x = 200, y = 0)
                  + scale_x_continuous(expand = c(0, 1))
                  + ylim(ymin, ymax)
                  + labs(title = element_blank())
)

Predict_Avg_Quant=(blandr.draw(subset(volumes, volumes$study=="Predict")$avg, 
                               subset(volumes, volumes$study=="Predict")$quantomo, 
                               sig.level = 0.95, LoA.mode = 1, annotate = FALSE, ciDisplay = FALSE,
                               ciShading = FALSE, normalLow = FALSE, normalHigh = FALSE,
                               lowest_y_axis = FALSE, highest_y_axis = FALSE, point_size = 0.8,
                               overlapping = FALSE, plotter = "ggplot", x.plot.mode = "means",
                               y.plot.mode = "difference", plotProportionalBias = FALSE,
                               plotProportionalBias.se = TRUE, assume.differences.are.normal = TRUE) 
                   + xlab("Mean ICH volume (mL)")
                   + ylab("ABC/2 - Quantomo (mL)")
                   + theme(
                     panel.border = element_blank(),  
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(),
                     panel.background = element_blank(),
                     axis.line = element_line(colour = "grey")
                   )
                   + expand_limits(x = 200, y = 0)
                   + scale_x_continuous(expand = c(0, 1))
                   + ylim(ymin, ymax)
                   + labs(title = element_blank())
)


Spotlight_AW_SN=(blandr.draw(subset(volumes, volumes$study=="Spotlight")$alex, 
                             subset(volumes, volumes$study=="Spotlight")$sean, 
                             sig.level = 0.95, LoA.mode = 1, annotate = FALSE, ciDisplay = FALSE,
                             ciShading = FALSE, normalLow = FALSE, normalHigh = FALSE,
                             lowest_y_axis = FALSE, highest_y_axis = FALSE, point_size = 0.8,
                             overlapping = FALSE, plotter = "ggplot", x.plot.mode = "means",
                             y.plot.mode = "difference", plotProportionalBias = FALSE,
                             plotProportionalBias.se = TRUE, assume.differences.are.normal = TRUE) 
                 + xlab("Mean ICH volume (mL)")
                 + ylab("ABC/2 - ABC/2 (mL)")
                 + theme(
                   panel.border = element_blank(),  
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(),
                   axis.line = element_line(colour = "grey")
                 )
                 + expand_limits(x = 200, y = 0)
                 + scale_x_continuous(expand = c(0, 1))
                 + ylim(ymin, ymax)
                 + labs(title = element_blank())
)
Spotlight_AW_Quant=(blandr.draw(subset(volumes, volumes$study=="Spotlight")$alex, 
                                subset(volumes, volumes$study=="Spotlight")$quantomo, 
                                sig.level = 0.95, LoA.mode = 1, annotate = FALSE, ciDisplay = FALSE,
                                ciShading = FALSE, normalLow = FALSE, normalHigh = FALSE,
                                lowest_y_axis = FALSE, highest_y_axis = FALSE, point_size = 0.8,
                                overlapping = FALSE, plotter = "ggplot", x.plot.mode = "means",
                                y.plot.mode = "difference", plotProportionalBias = FALSE,
                                plotProportionalBias.se = TRUE, assume.differences.are.normal = TRUE) 
                    + xlab("Mean ICH volume (mL)")
                    + ylab("ABC/2 - Quantomo (mL)")
                    + theme(
                      panel.border = element_blank(),  
                      panel.grid.major = element_blank(),
                      panel.grid.minor = element_blank(),
                      panel.background = element_blank(),
                      axis.line = element_line(colour = "grey")
                    )
                    + expand_limits(x = 200, y = 0)
                    + scale_x_continuous(expand = c(0, 1))
                    + ylim(ymin, ymax)
                    + labs(title = element_blank())
)
Spotlight_SN_Quant=(blandr.draw(subset(volumes, volumes$study=="Spotlight")$sean, 
                                subset(volumes, volumes$study=="Spotlight")$quantomo, 
                                sig.level = 0.95, LoA.mode = 1, annotate = FALSE, ciDisplay = FALSE,
                                ciShading = FALSE, normalLow = FALSE, normalHigh = FALSE,
                                lowest_y_axis = FALSE, highest_y_axis = FALSE, point_size = 0.8,
                                overlapping = FALSE, plotter = "ggplot", x.plot.mode = "means",
                                y.plot.mode = "difference", plotProportionalBias = FALSE,
                                plotProportionalBias.se = TRUE, assume.differences.are.normal = TRUE) 
                    + xlab("Mean ICH volume (mL)")
                    + ylab("ABC/2 - Quantomo (mL)")
                    + theme(
                      panel.border = element_blank(),  
                      panel.grid.major = element_blank(),
                      panel.grid.minor = element_blank(),
                      panel.background = element_blank(),
                      axis.line = element_line(colour = "grey")
                    )
                    + expand_limits(x = 200, y = 0)
                    + scale_x_continuous(expand = c(0, 1))
                    + ylim(ymin, ymax)
                    + labs(title = element_blank())
)

Spotlight_Avg_Quant=(blandr.draw(subset(volumes, volumes$study=="Spotlight")$avg, 
                                 subset(volumes, volumes$study=="Spotlight")$quantomo, 
                                 sig.level = 0.95, LoA.mode = 1, annotate = FALSE, ciDisplay = FALSE,
                                 ciShading = FALSE, normalLow = FALSE, normalHigh = FALSE,
                                 lowest_y_axis = FALSE, highest_y_axis = FALSE, point_size = 0.8,
                                 overlapping = FALSE, plotter = "ggplot", x.plot.mode = "means",
                                 y.plot.mode = "difference", plotProportionalBias = FALSE,
                                 plotProportionalBias.se = TRUE, assume.differences.are.normal = TRUE) 
                     + xlab("Mean ICH volume (mL)")
                     + ylab("ABC/2 - Quantomo (mL)")
                     + theme(
                       panel.border = element_blank(),  
                       panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(),
                       panel.background = element_blank(),
                       axis.line = element_line(colour = "grey")
                     )
                     + expand_limits(x = 200, y = 0)
                     + scale_x_continuous(expand = c(0, 1))
                     + ylim(ymin, ymax)
                     + labs(title = element_blank())
)

Combined_AW_SN=(blandr.draw(volumes$alex, 
                            volumes$sean, 
                            sig.level = 0.95, LoA.mode = 1, annotate = FALSE, ciDisplay = FALSE,
                            ciShading = FALSE, normalLow = FALSE, normalHigh = FALSE,
                            lowest_y_axis = FALSE, highest_y_axis = FALSE, point_size = 0.8,
                            overlapping = FALSE, plotter = "ggplot", x.plot.mode = "means",
                            y.plot.mode = "difference", plotProportionalBias = FALSE,
                            plotProportionalBias.se = TRUE, assume.differences.are.normal = TRUE) 
                + xlab("Mean ICH volume (mL)")
                + ylab("ABC/2 - ABC/2 (mL)")
                + theme(
                  panel.border = element_blank(),  
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(),
                  axis.line = element_line(colour = "grey")
                )
                + expand_limits(x = 200, y = 0)
                + scale_x_continuous(expand = c(0, 1))
                + ylim(ymin, ymax)
                + labs(title = element_blank())
)
Combined_AW_Quant=(blandr.draw(volumes$alex, 
                               volumes$quantomo, 
                               sig.level = 0.95, LoA.mode = 1, annotate = FALSE, ciDisplay = FALSE,
                               ciShading = FALSE, normalLow = FALSE, normalHigh = FALSE,
                               lowest_y_axis = FALSE, highest_y_axis = FALSE, point_size = 0.8,
                               overlapping = FALSE, plotter = "ggplot", x.plot.mode = "means",
                               y.plot.mode = "difference", plotProportionalBias = FALSE,
                               plotProportionalBias.se = TRUE, assume.differences.are.normal = TRUE) 
                   + xlab("Mean ICH volume (mL)")
                   + ylab("ABC/2 - Quantomo (mL)")
                   + theme(
                     panel.border = element_blank(),  
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(),
                     panel.background = element_blank(),
                     axis.line = element_line(colour = "grey")
                   )
                   + expand_limits(x = 200, y = 0)
                   + scale_x_continuous(expand = c(0, 1))
                   + ylim(ymin, ymax)
                   + labs(title = element_blank())
)
Combined_SN_Quant=(blandr.draw(volumes$sean, 
                               volumes$quantomo, 
                               sig.level = 0.95, LoA.mode = 1, annotate = FALSE, ciDisplay = FALSE,
                               ciShading = FALSE, normalLow = FALSE, normalHigh = FALSE,
                               lowest_y_axis = FALSE, highest_y_axis = FALSE, point_size = 0.8,
                               overlapping = FALSE, plotter = "ggplot", x.plot.mode = "means",
                               y.plot.mode = "difference", plotProportionalBias = FALSE,
                               plotProportionalBias.se = TRUE, assume.differences.are.normal = TRUE) 
                   + xlab("Mean ICH volume (mL)")
                   + ylab("ABC/2 - Quantomo (mL)")
                   + theme(
                     panel.border = element_blank(),  
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(),
                     panel.background = element_blank(),
                     axis.line = element_line(colour = "grey")
                   )
                   + expand_limits(x = 200, y = 0)
                   + scale_x_continuous(expand = c(0, 1))
                   + ylim(ymin, ymax)
                   + labs(title = element_blank())
)

Combined_Avg_Quant=(blandr.draw(volumes$avg, 
                                volumes$quantomo, 
                                sig.level = 0.95, LoA.mode = 1, annotate = FALSE, ciDisplay = FALSE,
                                ciShading = FALSE, normalLow = FALSE, normalHigh = FALSE,
                                lowest_y_axis = FALSE, highest_y_axis = FALSE, point_size = 0.8,
                                overlapping = FALSE, plotter = "ggplot", x.plot.mode = "means",
                                y.plot.mode = "difference", plotProportionalBias = FALSE,
                                plotProportionalBias.se = TRUE, assume.differences.are.normal = TRUE) 
                    + xlab("Mean ICH volume (mL)")
                    + ylab("ABC/2 - Quantomo (mL)")
                    + theme(
                      panel.border = element_blank(),  
                      panel.grid.major = element_blank(),
                      panel.grid.minor = element_blank(),
                      panel.background = element_blank(),
                      axis.line = element_line(colour = "grey")
                    )
                    + expand_limits(x = 200, y = 0)
                    + scale_x_continuous(expand = c(0, 1))
                    + ylim(ymin, ymax)
                    + labs(title = element_blank())
)

# Plot --------------------------------------------------------------------
plot_grid(Predict_AW_SN, Predict_AW_Quant, Predict_SN_Quant, Predict_Avg_Quant,
          Spotlight_AW_SN, Spotlight_AW_Quant, Spotlight_SN_Quant, Spotlight_Avg_Quant,
          Combined_AW_SN, Combined_AW_Quant, Combined_SN_Quant, Combined_Avg_Quant,
          labels = c("A","B","C","D","E","F","G","H","I","J","K","L"),
          ncol = 4, nrow = 3)


