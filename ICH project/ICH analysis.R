library(googlesheets)
library(googledrive)
library(irr)
library(stringr)
library(blandr)
library(ggplot2)
library(cowplot)

for_gs <- gs_title("PREDICT ROI File Issues")
setwd("C:/Users/alexw/Google Drive/Desktop files/Dal Med/ICH")


# PREDICT Data merging ------------------------------------------------------------
pt_char=read.csv("PREDICT_baseline_data.csv")
predict_sheet <- as.data.frame(gs_read(for_gs, ws="PREDICT V6 TOTAL"))
X2=as.vector(predict_sheet$`Abbrev filename`)

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
predict_sheet=cbind(predict_sheet, baseline)

psite=as.character(as.vector(predict_sheet$Psite))
pnum=as.character(as.vector(predict_sheet$Pnum))
PID=paste(psite, pnum, sep="", collapse=NULL)

predict_sheet=cbind(predict_sheet, PID)

baseline_predict=subset(predict_sheet, predict_sheet$baseline==0)
followup_predict=subset(predict_sheet, predict_sheet$baseline==1)

keep=c("PID","baseline","Irregular_SN","ABC-A_SN","ABC-B_SN","ABC-C_SN","ABC/2_SN","Irregular_AW","ABC-A_AW",
       "ABC-B_AW","ABC-C_AW","ABC/2_AW","ABC/2-Diff","ABC/2AVG-Quantomo","NewQuantomo","DeepMedicVol")
baseline_predict=baseline_predict[keep]
followup_predict=followup_predict[keep]

colnames(baseline_predict)=c("PID","baseline","b_Irregular_SN","b_ABC-A_SN","b_ABC-B_SN","b_ABC-C_SN","b_ABC/2_SN",
                             "b_Irregular_AW","b_ABC-A_AW","b_ABC-B_AW","b_ABC-C_AW","b_ABC/2_AW","b_ABC/2-Diff",
                             "b_ABC/2AVG-Quantomo","b_NewQuantomo","b_DeepMedicVol")
colnames(followup_predict)=c("PID","baseline","fu_Irregular_SN","fu_ABC-A_SN","fu_ABC-B_SN","fu_ABC-C_SN",
                             "fu_ABC/2_SN","fu_Irregular_AW","fu_ABC-A_AW","fu_ABC-B_AW","fu_ABC-C_AW","fu_ABC/2_AW",
                             "fu_ABC/2-Diff","fu_ABC/2AVG-Quantomo","fu_NewQuantomo","fu_DeepMedicVol")

combined=merge(baseline_predict, followup_predict, by="PID", all.x=TRUE, all.y=TRUE)

pt_char$?..patient_site=str_pad(pt_char$?..patient_site, 2, pad="0")
pt_char$patient_study=str_pad(pt_char$patient_study, 3, pad="0")
pt_char$PID=paste(pt_char$?..patient_site, pt_char$patient_study, sep="", collapse=NULL)

predict_pt_char=merge(pt_char, combined, by="PID", all.y=TRUE)

#write.csv(predict_pt_char, "PREDICTv6 merged database.csv")


# SPOTLIGHT Data merging --------------------------------------------------
setwd("C:/Users/alexw/Google Drive/Desktop files/Dal Med/ICH")
pt_char=read.csv("SPOTLIGHT_Baseline_data.csv")
pt_char$?..Subject=str_pad(pt_char$?..Subject, 6, pad="0")
colnames(pt_char)[1]<-"PID"


spotlight_sheet <- gs_read(for_gs, ws="SPOTLIGHT v5")

CASE=as.vector(spotlight_sheet$'RENAME FOR ORDER')

baseline=c()
for (i in CASE){
  baseline=append(baseline, as.numeric(substr(i, 20, 20)))
}

spotlight_sheet=cbind(spotlight_sheet, baseline)

psite=as.character(as.vector(spotlight_sheet$Psite))
pnum=as.character(as.vector(spotlight_sheet$Pnum))
PID=paste(psite, pnum, sep="", collapse=NULL)

spotlight_sheet=cbind(spotlight_sheet, PID)

one_spotlight=subset(spotlight_sheet, spotlight_sheet$baseline==1)
two_spotlight=subset(spotlight_sheet, spotlight_sheet$baseline==2)
three_spotlight=subset(spotlight_sheet, spotlight_sheet$baseline==3)
four_spotlight=subset(spotlight_sheet, spotlight_sheet$baseline==4)
five_spotlight=subset(spotlight_sheet, spotlight_sheet$baseline==5)

keeps=c("PID","S-Irregular","S-ABC-A","S-ABC-B","S-ABC-C","S-ABC/2","NewQuantomo","A-Irregular","A-ABC-A","A-ABC-B","A-ABC-C","A-ABC/2",
        "DM-Volume")

one_spotlight=one_spotlight[keeps]
colnames(one_spotlight)=c("PID","S-Irregular_1","S-ABC-A_1","S-ABC-B_1","S-ABC-C_1","S-ABC/2_1","NewQuantomo_1","A-Irregular_1","A-ABC-A_1",
                          "A-ABC-B_1","A-ABC-C_1","A-ABC/2_1","DM-Volume_1")

two_spotlight=two_spotlight[keeps]
colnames(two_spotlight)=c("PID","S-Irregular_2","S-ABC-A_2","S-ABC-B_2","S-ABC-C_2","S-ABC/2_2","NewQuantomo_2","A-Irregular_2","A-ABC-A_2",
                          "A-ABC-B_2","A-ABC-C_2","A-ABC/2_2","DM-Volume_2")

three_spotlight=three_spotlight[keeps]
colnames(three_spotlight)=c("PID","S-Irregular_3","S-ABC-A_3","S-ABC-B_3","S-ABC-C_3","S-ABC/2_3","NewQuantomo_3","A-Irregular_3","A-ABC-A_3",
                            "A-ABC-B_3","A-ABC-C_3","A-ABC/2_3","DM-Volume_3")

four_spotlight=four_spotlight[keeps]
colnames(four_spotlight)=c("PID","S-Irregular_4","S-ABC-A_4","S-ABC-B_4","S-ABC-C_4","S-ABC/2_4","NewQuantomo_4","A-Irregular_4","A-ABC-A_4",
                           "A-ABC-B_4","A-ABC-C_4","A-ABC/2_4","DM-Volume_4")

five_spotlight=five_spotlight[keeps]
colnames(five_spotlight)=c("PID","S-Irregular_5","S-ABC-A_5","S-ABC-B_5","S-ABC-C_5","S-ABC/2_5","NewQuantomo_5","A-Irregular_5","A-ABC-A_5",
                           "A-ABC-B_5","A-ABC-C_5","A-ABC/2_5","DM-Volume_5")

combined4=merge(one_spotlight, two_spotlight, by="PID", all=TRUE)
combined3=merge(combined4, three_spotlight, by="PID", all=TRUE)
combined2=merge(combined3, four_spotlight, by="PID", all=TRUE)
combined=merge(combined2, five_spotlight, by="PID", all=TRUE)

spotlight_pt_char=merge(pt_char, combined, by="PID", all.y=TRUE)

#write.csv(spotlight_pt_char, "SPOTLIGHT database merged.csv")

# Dal Data Extraction from GS ---------------------------------------------

dal_sheet <- as.data.frame(gs_read(for_gs, ws="DalV3"))
dal_sheet[dal_sheet==""]<-NA

keep=c("Dal_ID","Age","Gender", "Anticoagulation", "HTN", "Warfarin",
       "Deep=1/Lobar=2/PF=3", "IVH", "Time of onset to CT (hrs)","Quantomo","S-ABC/2","DeepMedic"
       )
dal_sheet=dal_sheet[keep]

# Table 1 -----------------------------------------------------------------
predict=read.csv("PREDICTv6 merged database.csv")
spotlight=read.csv("SPOTLIGHT database merged.csv")
predict[predict==""]<-NA
spotlight[spotlight==""]<-NA

predict$study=c("Predict")
spotlight$study=c("Spotlight")

keep=c("PID","study","ageatevent","gender","hypertension","diabetes","on_antiplatelet_therapy","on_warfarin","ICH_lobar.base",
       "ICH_deep.base","ICH_post.base","IVH.base","Time.from.STROKE_ONSET.to.INITIAL_CT_SCAN")

spotlight$Time.from.STROKE_ONSET.to.INITIAL_CT_SCAN=as.factor(spotlight$Time.from.STROKE_ONSET.to.INITIAL_CT_SCAN)

predict=predict[keep]
spotlight=spotlight[keep]
combined=rbind(predict, spotlight)
combined$Time.from.STROKE_ONSET.to.INITIAL_CT_SCAN=as.numeric(as.character(combined$Time.from.STROKE_ONSET.to.INITIAL_CT_SCAN))

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

#write.csv(outcomes, "C:/Users/alexw/Google Drive/Desktop files/Dal Med/ICH/Results/Table 1 values.csv")

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
table1stats=append(table1stats,fisher.test(M)$p.value)

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
table1stats=cbind(table1stats, c("t-test","chisq","chisq","fisher","fisher","fisher","chisq","chisq","fisher","chisq","t-test"))
rownames(table1stats)=c("ageatevent","gender","hypertension","diabetes","on_antiplatelet_therapy","on_warfarin","ICH_lobar.base",
                        "ICH_deep.base","ICH_post.base","IVH.base","Time.from.STROKE_ONSET.to.INITIAL_CT_SCAN")
colnames(table1stats)=c("p-value","stat test")

#write.csv(table1stats, "C:/Users/alexw/Google Drive/Desktop files/Dal Med/ICH/Results/Table 1 stats.csv")


# Table 2 -----------------------------------------------------------------

predict_sheet <- as.data.frame(gs_read(for_gs, ws="PREDICT V6 TOTAL"))

psite=as.character(as.vector(predict_sheet$Psite))
pnum=as.character(as.vector(predict_sheet$Pnum))
PID=paste(psite, pnum, sep="", collapse=NULL)
predict_sheet=cbind(predict_sheet, PID)
predict_sheet$study=c("Predict")
X2=as.vector(predict_sheet$`Abbrev filename`)

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
predict_sheet=cbind(predict_sheet, baseline)

predictkeep=c("PID","study","baseline","Irregular_SN","ABC/2_SN","ABC/2_AW","NewQuantomo","DeepMedicVol")

predict_sheet=predict_sheet[predictkeep]


spotlight_sheet <- as.data.frame(gs_read(for_gs, ws="SPOTLIGHT v5"))

psite=as.character(as.vector(spotlight_sheet$Psite))
pnum=as.character(as.vector(spotlight_sheet$Pnum))
PID=paste(psite, pnum, sep="", collapse=NULL)
spotlight_sheet=cbind(spotlight_sheet, PID)
spotlight_sheet$study=c("Spotlight")

CASE=as.vector(spotlight_sheet$'RENAME FOR ORDER')
baseline=c()
for (i in CASE){
  baseline=append(baseline, as.numeric(substr(i, 20, 20)))
}
spotlight_sheet=cbind(spotlight_sheet, baseline)

spotlightkeep=c("PID","study","baseline","S-Irregular","S-ABC/2","A-ABC/2","QT-Vol","DM-Volume")
spotlight_sheet=spotlight_sheet[spotlightkeep]
colnames(spotlight_sheet)=c("PID","study","baseline","Irregular_SN","ABC/2_SN","ABC/2_AW","NewQuantomo","DeepMedicVol")
data=rbind(predict_sheet, spotlight_sheet)

data$'ABC/2avg'=rowMeans(data[,5:6])
data$'QT-ABC/2'=data$NewQuantomo-data$'ABC/2_SN'
data$'DM-QT'=data$DeepMedicVol-data$NewQuantomo


studytype=c("Predict","Spotlight")

for(studys in studytype){
  column=c()
  volumesstudy=subset(data, data[,"study"]==studys)
  
  column=append(column, nrow(volumesstudy))
  
  column=append(column, range(volumesstudy$'ABC/2_SN'))
  column=append(column, range(volumesstudy$'ABC/2_AW'))
  column=append(column, range(volumesstudy$NewQuantomo))
  column=append(column, range(volumesstudy$DeepMedicVol))
  column=append(column, range(volumesstudy$'QT-ABC/2'))
  column=append(column, range(volumesstudy$'DM-QT'))
  
  column=append(column, mean(volumesstudy$'ABC/2_SN'))
  column=append(column, sd(volumesstudy$'ABC/2_SN'))
  column=append(column, mean(volumesstudy$'ABC/2_AW'))
  column=append(column, sd(volumesstudy$'ABC/2_AW'))
  column=append(column, mean(volumesstudy$NewQuantomo))
  column=append(column, sd(volumesstudy$NewQuantomo))
  column=append(column, mean(volumesstudy$DeepMedicVol))
  column=append(column, sd(volumesstudy$DeepMedicVol))
  column=append(column, mean(volumesstudy$'QT-ABC/2'))
  column=append(column, sd(volumesstudy$'QT-ABC/2'))
  column=append(column, mean(volumesstudy$'DM-QT'))
  column=append(column, sd(volumesstudy$'DM-QT'))
  
  column=append(column, median(volumesstudy$'ABC/2_SN'))
  column=append(column, median(volumesstudy$'ABC/2_AW'))
  column=append(column, median(volumesstudy$NewQuantomo))
  column=append(column, median(volumesstudy$DeepMedicVol))
  column=append(column, median(volumesstudy$'QT-ABC/2'))
  column=append(column, median(volumesstudy$'DM-QT'))
  
  column=append(column, quantile(volumesstudy$'ABC/2_SN', 1/4))
  column=append(column, quantile(volumesstudy$'ABC/2_SN', 3/4))
  column=append(column, quantile(volumesstudy$'ABC/2_AW', 1/4))
  column=append(column, quantile(volumesstudy$'ABC/2_AW', 3/4))
  column=append(column, quantile(volumesstudy$NewQuantomo, 1/4))
  column=append(column, quantile(volumesstudy$NewQuantomo, 3/4))
  column=append(column, quantile(volumesstudy$DeepMedicVol, 1/4))
  column=append(column, quantile(volumesstudy$DeepMedicVol, 3/4))
  column=append(column, quantile(volumesstudy$'QT-ABC/2', 1/4))
  column=append(column, quantile(volumesstudy$'QT-ABC/2', 3/4))
  column=append(column, quantile(volumesstudy$'DM-QT', 1/4))
  column=append(column, quantile(volumesstudy$'DM-QT', 3/4))
  
  if (studys=="Predict"){
    table2=as.data.frame(column)
  }else{
    table2=cbind(table2, column)
  }
  
  if (studys=="Spotlight"){
    column=c()
    volumesstudy=data
    
    column=append(column, nrow(volumesstudy))
    
    column=append(column, range(volumesstudy$'ABC/2_SN'))
    column=append(column, range(volumesstudy$'ABC/2_AW'))
    column=append(column, range(volumesstudy$NewQuantomo))
    column=append(column, range(volumesstudy$DeepMedicVol))
    column=append(column, range(volumesstudy$'QT-ABC/2'))
    column=append(column, range(volumesstudy$'DM-QT'))
    
    column=append(column, mean(volumesstudy$'ABC/2_SN'))
    column=append(column, sd(volumesstudy$'ABC/2_SN'))
    column=append(column, mean(volumesstudy$'ABC/2_AW'))
    column=append(column, sd(volumesstudy$'ABC/2_AW'))
    column=append(column, mean(volumesstudy$NewQuantomo))
    column=append(column, sd(volumesstudy$NewQuantomo))
    column=append(column, mean(volumesstudy$DeepMedicVol))
    column=append(column, sd(volumesstudy$DeepMedicVol))
    column=append(column, mean(volumesstudy$'QT-ABC/2'))
    column=append(column, sd(volumesstudy$'QT-ABC/2'))
    column=append(column, mean(volumesstudy$'DM-QT'))
    column=append(column, sd(volumesstudy$'DM-QT'))
    
    column=append(column, median(volumesstudy$'ABC/2_SN'))
    column=append(column, median(volumesstudy$'ABC/2_AW'))
    column=append(column, median(volumesstudy$NewQuantomo))
    column=append(column, median(volumesstudy$DeepMedicVol))
    column=append(column, median(volumesstudy$'QT-ABC/2'))
    column=append(column, median(volumesstudy$'DM-QT'))
    
    column=append(column, quantile(volumesstudy$'ABC/2_SN', 1/4))
    column=append(column, quantile(volumesstudy$'ABC/2_SN', 3/4))
    column=append(column, quantile(volumesstudy$'ABC/2_AW', 1/4))
    column=append(column, quantile(volumesstudy$'ABC/2_AW', 3/4))
    column=append(column, quantile(volumesstudy$NewQuantomo, 1/4))
    column=append(column, quantile(volumesstudy$NewQuantomo, 3/4))
    column=append(column, quantile(volumesstudy$DeepMedicVol, 1/4))
    column=append(column, quantile(volumesstudy$DeepMedicVol, 3/4))
    column=append(column, quantile(volumesstudy$'QT-ABC/2', 1/4))
    column=append(column, quantile(volumesstudy$'QT-ABC/2', 3/4))
    column=append(column, quantile(volumesstudy$'DM-QT', 1/4))
    column=append(column, quantile(volumesstudy$'DM-QT', 3/4))
    
    table2=cbind(table2, column)
  }
}

colnames(table2)=c("Predict","Spotlight","Combined")
rownames(table2)=c("n","min ABC/2_SN","max ABC/2_SN","min ABC/2_AW","max ABC/2_AW","min QT","max QT","min DM","max DM","min QT-ABC/2_SN","max QT-ABC/2_SN",
                   "min DM-QT","max DM-QT","mean ABC/2_SN","sd ABC/2_SN","mean ABC/2_AW","sd ABC/2_AW","mean QT","sd QT","mean DM","sd DM","mean QT-ABC/2_SN",
                   "sd QT-ABC/2_SN","mean DM-QT","sd DM-QT","median ABC/2_SN","median ABC/2_AW","median QT","median DM","median QT-ABC/2_SN","median DM-QT",
                   "Q1 ABC/2_SN","Q3 ABC/2_SN","Q1 ABC/2_AW","Q3 ABC/2_AW","Q1 QT","Q3 QT","Q1 DM","Q3 DM","Q1 QT-ABC/2_SN","Q3 QT-ABC/2_SN","Q1 DM-QT","Q3 DM-QT")

#write.csv(table2,"C:/Users/alexw/Google Drive/Desktop files/Dal Med/ICH/Results/Table 2 values.csv")
#write.csv(data, "C:/Users/alexw/Google Drive/Desktop files/Dal Med/ICH/Results/volumes_data.csv")
# ICC Calculations --------------------------------------------------------

icc(data.frame(subset(data, data$study=="Predict")$'ABC/2_SN',
               subset(data, data$study=="Predict")$NewQuantomo),
    model="twoway",type="agreement")

icc(data.frame(subset(data, data$study=="Predict")$NewQuantomo,
               subset(data, data$study=="Predict")$DeepMedicVol),
    model="twoway",type="agreement")

icc(data.frame(subset(data, data$study=="Spotlight")$'ABC/2_SN',
               subset(data, data$study=="Spotlight")$NewQuantomo),
    model="twoway",type="agreement")

icc(data.frame(subset(data, data$study=="Spotlight")$NewQuantomo,
               subset(data, data$study=="Spotlight")$DeepMedicVol),
    model="twoway",type="agreement")


icc(data.frame(data$'ABC/2_SN', data$NewQuantomo), model="twoway",type="agreement")
icc(data.frame(data$NewQuantomo, data$DeepMedicVol), model="twoway",type="agreement")


# Bland Altman graphs -----------------------------------------------------
ymin=-50
ymax=100

Predict_QT_ABC2=(blandr.draw(subset(data, data$study=="Predict")$'ABC/2_SN', 
                           subset(data, data$study=="Predict")$NewQuantomo, 
                           sig.level = 0.95, LoA.mode = 1, ciDisplay = FALSE,
                           ciShading = FALSE,
                           lowest_y_axis = ymin, highest_y_axis = ymax, point_size = 0.8,
                           overlapping = FALSE, plotter = "ggplot", x.plot.mode = "means",
                           y.plot.mode = "difference", plotProportionalBias = FALSE,
                           plotProportionalBias.se = TRUE, assume.differences.are.normal = TRUE) 
               + xlab("Mean ICH volume (mL)")
               + ylab("Quantomo - ABC/2 (mL)")
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

Predict_QT_DM=(blandr.draw(subset(data, data$study=="Predict")$NewQuantomo, 
                              subset(data, data$study=="Predict")$DeepMedicVol, 
                              sig.level = 0.95, LoA.mode = 1, annotate = FALSE, ciDisplay = FALSE,
                              ciShading = FALSE, normalLow = FALSE, normalHigh = FALSE,
                              lowest_y_axis = FALSE, highest_y_axis = FALSE, point_size = 0.8,
                              overlapping = FALSE, plotter = "ggplot", x.plot.mode = "means",
                              y.plot.mode = "difference", plotProportionalBias = FALSE,
                              plotProportionalBias.se = TRUE, assume.differences.are.normal = TRUE) 
                  + xlab("Mean ICH volume (mL)")
                  + ylab("Deep Medic - Quantomo (mL)")
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

Spotlight_QT_ABC2=(blandr.draw(subset(data, data$study=="Spotlight")$'ABC/2_SN', 
                             subset(data, data$study=="Spotlight")$NewQuantomo, 
                             sig.level = 0.95, LoA.mode = 1, ciDisplay = FALSE,
                             ciShading = FALSE,
                             lowest_y_axis = ymin, highest_y_axis = ymax, point_size = 0.8,
                             overlapping = FALSE, plotter = "ggplot", x.plot.mode = "means",
                             y.plot.mode = "difference", plotProportionalBias = FALSE,
                             plotProportionalBias.se = TRUE, assume.differences.are.normal = TRUE) 
                 + xlab("Mean ICH volume (mL)")
                 + ylab("Quantomo - ABC/2 (mL)")
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

Spotlight_QT_DM=(blandr.draw(subset(data, data$study=="Spotlight")$NewQuantomo, 
                           subset(data, data$study=="Spotlight")$DeepMedicVol, 
                           sig.level = 0.95, LoA.mode = 1, annotate = FALSE, ciDisplay = FALSE,
                           ciShading = FALSE, normalLow = FALSE, normalHigh = FALSE,
                           lowest_y_axis = FALSE, highest_y_axis = FALSE, point_size = 0.8,
                           overlapping = FALSE, plotter = "ggplot", x.plot.mode = "means",
                           y.plot.mode = "difference", plotProportionalBias = FALSE,
                           plotProportionalBias.se = TRUE, assume.differences.are.normal = TRUE) 
               + xlab("Mean ICH volume (mL)")
               + ylab("Deep Medic - Quantomo (mL)")
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



Combined_QT_ABC2=(blandr.draw(data$'ABC/2_SN', 
                            data$NewQuantomo, 
                            sig.level = 0.95, LoA.mode = 1, annotate = FALSE, ciDisplay = FALSE,
                            ciShading = FALSE, normalLow = FALSE, normalHigh = FALSE,
                            lowest_y_axis = FALSE, highest_y_axis = FALSE, point_size = 0.8,
                            overlapping = FALSE, plotter = "ggplot", x.plot.mode = "means",
                            y.plot.mode = "difference", plotProportionalBias = FALSE,
                            plotProportionalBias.se = TRUE, assume.differences.are.normal = TRUE) 
                + xlab("Mean ICH volume (mL)")
                + ylab("Quantomo - ABC/2 (mL)")
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
Combined_QT_DM=(blandr.draw(data$NewQuantomo, 
                            data$DeepMedicVol, 
                               sig.level = 0.95, LoA.mode = 1, annotate = FALSE, ciDisplay = FALSE,
                               ciShading = FALSE, normalLow = FALSE, normalHigh = FALSE,
                               lowest_y_axis = FALSE, highest_y_axis = FALSE, point_size = 0.8,
                               overlapping = FALSE, plotter = "ggplot", x.plot.mode = "means",
                               y.plot.mode = "difference", plotProportionalBias = FALSE,
                               plotProportionalBias.se = TRUE, assume.differences.are.normal = TRUE) 
                   + xlab("Mean ICH volume (mL)")
                   + ylab("Deep Medic - Quantomo (mL)")
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
plot_grid(Predict_QT_ABC2, Predict_QT_DM, Spotlight_QT_ABC2, Spotlight_QT_DM, Combined_QT_ABC2, Combined_QT_DM,
          labels = c("A","B","C","D","E","F"),
          ncol = 2, nrow = 3)


# Hematoma expansion analysis -------------------------------------------------------

rm(expansion)
data$expansion_ABC=c(0)
data$expansion_QT=c(0)
data$expansion_DM=c(0)
for (pt_id in levels(data$PID)){
  pt_data=subset(data, data$PID==pt_id)
  if (nrow(pt_data)>1){
    for (i in c(2:nrow(pt_data))){
      ABC2=pt_data[i,"ABC/2_SN"]-pt_data[1,"ABC/2_SN"]
      if (ABC2>6 | ABC2/pt_data[1,"ABC/2_SN"]>0.33){
        if (exists("expansion")==FALSE){
          expansion=pt_data[1,]
          pt_data[i,"expansion_ABC"]=1
          expansion=rbind(expansion, pt_data[i,])
        } else {
          expansion=rbind(expansion, pt_data[1,])
          pt_data[i,"expansion_ABC"]=1
          expansion=rbind(expansion, pt_data[i,])
        }
      }
      
      QT=pt_data[i,"NewQuantomo"]-pt_data[1,"NewQuantomo"]
      if (QT>6 | QT/pt_data[1,"NewQuantomo"]>0.33){
        if (exists("expansion")==FALSE){
          expansion=pt_data[1,]
          pt_data[i,"expansion_QT"]=1
          expansion=rbind(expansion, pt_data[i,])
        }
        else if (expansion[nrow(expansion),"PID"]==as.factor(pt_id)){
          expansion[nrow(expansion),"expansion_QT"]=1
        } 
      } 
      
      DM=pt_data[i,"DeepMedicVol"]-pt_data[1,"DeepMedicVol"]
      if (DM>6 | DM/pt_data[1,"DeepMedicVol"]>0.33){
        if (exists("expansion")==FALSE){
          expansion=pt_data[1,]
          pt_data[i,"expansion_DM"]=1
          expansion=rbind(expansion, pt_data[i,])
        }
        else if (expansion[nrow(expansion),"PID"]==as.factor(pt_id)){
          expansion[nrow(expansion),"expansion_DM"]=1
        }
      }
    }
  }
}
expansion$PID <- factor(expansion$PID)
b_fu_data=subset(data, duplicated(data$PID)==TRUE)
b_fu_data$PID <- factor(b_fu_data$PID)


cat("Proportion of patients with hematoma expansion = ",length(levels(expansion$PID)),"/",length(levels(b_fu_data$PID)),",",
        length(levels(expansion$PID))/length(levels(b_fu_data$PID))*100,"%")

ABC2_expansion=subset(expansion, expansion$expansion_ABC==1)
ABC2_expansion$PID <- factor(ABC2_expansion$PID)
cat("Proportion of patients with hematoma expansion by ABC2 measure = ",length(levels(ABC2_expansion$PID)),"/",length(levels(b_fu_data$PID)),",",
    length(levels(ABC2_expansion$PID))/length(levels(b_fu_data$PID))*100,"%")

QT_expansion=subset(expansion, expansion$expansion_QT==1)
QT_expansion$PID <- factor(QT_expansion$PID)
cat("Proportion of patients with hematoma expansion by QT measure = ",length(levels(QT_expansion$PID)),"/",length(levels(b_fu_data$PID)),",",
    length(levels(QT_expansion$PID))/length(levels(b_fu_data$PID))*100,"%")

DM_expansion=subset(expansion, expansion$expansion_DM==1)
DM_expansion$PID <- factor(DM_expansion$PID)
cat("Proportion of patients with hematoma expansion by DM measure = ",length(levels(DM_expansion$PID)),"/",length(levels(b_fu_data$PID)),",",
    length(levels(DM_expansion$PID))/length(levels(b_fu_data$PID))*100,"%")


#write.csv(expansion, "C:/Users/alexw/Google Drive/Desktop files/Dal Med/ICH/Results/Hematoma expansion.csv")





