# Load data ---------------------------------------------------------------
setwd("C:/Users/alexw/Google Drive/Desktop files/Dal Med/Aneurysm Repair Project/Data Analysis")
data=read.csv("Data table FINAL.csv")
library(MASS)
library(Hmisc)
library(brant)
library(generalhoslem)
library(DescTools)
library(aod)
library(pROC)
library("verification")
summary=summary(data)

# Remove missing data -----------------------------------------------------
data=subset(data, data$SAH.Etiology=="aSAH")
data$SAH.Etiology<-factor(data$SAH.Etiology)


show("Number of patients that died before treatment")
show(nrow(subset(data,data$Death.before.Treatment==1)))
data$Death.before.Treatment=as.factor(data$Death.before.Treatment)
data=subset(data, data$Death.before.Treatment!=1)

show("Number of patients treated with coil and clip")
show(nrow(subset(data,data$Treatment=="Coil/Clip")))
data=subset(data, data$Treatment!="Coil/Clip")
data$Treatment <- factor(data$Treatment)
data$Treatment <- factor(data$Treatment, levels=c("Clip","Coil"), ordered=FALSE)

data=subset(data, data$DOB!="NA")
data=subset(data, data$Gender!="NA")
data=subset(data, data$Admission.Date!="NA")
data=subset(data, data$Discharge.Date!="NA")
data=subset(data, data$Current.smoke!="NA")
data=subset(data, data$HTN!="NA")
data=subset(data, data$DM!="NA")
data=subset(data, data$Past.SAH!="NA")
data=subset(data, data$Past.stroke!="NA")
data=subset(data, data$Op.Date!="NA")
data=subset(data, data$Time.to.Treatment..days.!="NA")

data=subset(data, data$X..Aneurysms!="NA")
data=subset(data, data$A1.size!="NA")

#data=subset(data, data$A2.size!="NA")
#data=subset(data, data$A3.size!="NA")
#data=subset(data, data$A4.size!="NA")
#data=subset(data, data$A5.size!="NA")

data=subset(data, data$A1.location!="NA")
data$A1.location <- factor(data$A1.location)

#data=subset(data, data$A2.location!="NA")
#data=subset(data, data$A3.location!="NA")
#data=subset(data, data$A4.location!="NA")
#data=subset(data, data$A5.location!="NA")
#data=subset(data, data$GCS!="NA")
data=subset(data, data$WFNS!="NA")
data=subset(data, data$Fisher!="NA")

#data=subset(data, data$mRS!="NA")
#data=subset(data, data$HH!="NA")
data=subset(data, data$Length.of.hospital.stay..day.!="NA")
data=subset(data, data$Mortality!="NA")
data=subset(data, data$Rebleed!="NA")

show(nrow(subset(data,data$Treatment=="Coil")))
show(nrow(subset(data,data$Treatment=="Clip")))

#Recategorize GOS to dead, dependent, and independent
data$GOS.at.discharge=as.character(data$GOS.at.discharge)
data$GOS.at.discharge[data$GOS.at.discharge=="1"]=0
data$GOS.at.discharge[data$GOS.at.discharge=="2"|data$GOS.at.discharge=="3"]=1
data$GOS.at.discharge[data$GOS.at.discharge=="4"|data$GOS.at.discharge=="5"]=2
data$GOS.at.discharge=as.factor(data$GOS.at.discharge)
data$GOS.at.discharge <- factor(data$GOS.at.discharge, levels=c(0,1,2), ordered=FALSE)

data$GOS.at.6....3.months=as.character(data$GOS.at.6....3.months)
data$GOS.at.6....3.months[data$GOS.at.6....3.months=="1"]=0
data$GOS.at.6....3.months[data$GOS.at.6....3.months=="2"|data$GOS.at.6....3.months=="3"]=1
data$GOS.at.6....3.months[data$GOS.at.6....3.months=="4"|data$GOS.at.6....3.months=="5"]=2
data$GOS.at.6....3.months=as.factor(data$GOS.at.6....3.months)
data$GOS.at.6....3.months <- factor(data$GOS.at.6....3.months, levels=c(0,1,2), ordered=FALSE)

data$GOS.at.12....3.months=as.character(data$GOS.at.12....3.months)
data$GOS.at.12....3.months[data$GOS.at.12....3.months=="1"]=0
data$GOS.at.12....3.months[data$GOS.at.12....3.months=="2"|data$GOS.at.12....3.months=="3"]=1
data$GOS.at.12....3.months[data$GOS.at.12....3.months=="4"|data$GOS.at.12....3.months=="5"]=2
data$GOS.at.12....3.months=as.factor(data$GOS.at.12....3.months)
data$GOS.at.12....3.months <- factor(data$GOS.at.12....3.months, levels=c(0,1,2), ordered=FALSE)

#Recategorize date of admission
data$Admission.Date=as.Date(data$Admission.Date)
data$date=c(0)
data$date[data$Admission.Date <= as.Date("2007-12-31")] = "date_1"
data$date[data$Admission.Date > as.Date("2007-12-31") & data$Admission.Date <=as.Date("2012-12-31")] = "date_2"
data$date[data$Admission.Date > as.Date("2012-12-31")] = "date_3"
data$date=as.character(data$date)

#loss to followup table
treattype=c("Coil","Clip")
yeartype=c("date_1","date_2","date_3")
for (year in yeartype){
  for (treat in treattype){
    column=c()
    datayear=subset(data, data[,"date"]==year)
    datatreat=subset(datayear, datayear[,"Treatment"]==treat)
    
    column=append(column, nrow(datatreat))
    datatreat=subset(datatreat, datatreat$GOS.at.6....3.months!="NA")
    column=append(column, nrow(datatreat))
    datatreat=subset(datatreat, datatreat$GOS.at.12....3.months!="NA")
    column=append(column, nrow(datatreat))

    if (treat=="Coil" & year=="date_1"){
      lof_table=as.data.frame(column)
    } else {
      lof_table=cbind(lof_table, column)
    }
  }
}


#Remove missing GOS data
data=subset(data, data$GOS.at.discharge!="NA")
data=subset(data, data$GOS.at.6....3.months!="NA")
data=subset(data, data$GOS.at.12....3.months!="NA")

#### ANEURYSM LOCATION TABLE ####

for(treattype in c("Coil","Clip")){
  altab=c()
  datatreat=subset(data, data$Treatment==treattype)
  altab=append(altab, nrow(subset(datatreat, datatreat$A1.location=="Acomm")))
  altab=append(altab, nrow(subset(datatreat, datatreat$A1.location=="Acomm"))/nrow(datatreat)*100)
  altab=append(altab, nrow(subset(datatreat, datatreat$A1.location=="ACA")))
  altab=append(altab, nrow(subset(datatreat, datatreat$A1.location=="ACA"))/nrow(datatreat)*100)
  altab=append(altab, nrow(subset(datatreat, datatreat$A1.location=="ICA")))
  altab=append(altab, nrow(subset(datatreat, datatreat$A1.location=="ICA"))/nrow(datatreat)*100)
  altab=append(altab, nrow(subset(datatreat, datatreat$A1.location=="PC")))
  altab=append(altab, nrow(subset(datatreat, datatreat$A1.location=="PC"))/nrow(datatreat)*100)
  altab=append(altab, nrow(subset(datatreat, datatreat$A1.location=="MCA")))
  altab=append(altab, nrow(subset(datatreat, datatreat$A1.location=="MCA"))/nrow(datatreat)*100)
  altab=append(altab, nrow(subset(datatreat, datatreat$A1.location=="Pcomm")))
  altab=append(altab, nrow(subset(datatreat, datatreat$A1.location=="Pcomm"))/nrow(datatreat)*100)
  altab=append(altab, nrow(subset(datatreat, datatreat$A1.location=="PCA")))
  altab=append(altab, nrow(subset(datatreat, datatreat$A1.location=="PCA"))/nrow(datatreat)*100)
  altab=append(altab, nrow(subset(datatreat, datatreat$A1.location=="SCA")))
  altab=append(altab, nrow(subset(datatreat, datatreat$A1.location=="SCA"))/nrow(datatreat)*100)
  altab=append(altab, nrow(subset(datatreat, datatreat$A1.location=="BA")))
  altab=append(altab, nrow(subset(datatreat, datatreat$A1.location=="BA"))/nrow(datatreat)*100)
  altab=append(altab, nrow(subset(datatreat, datatreat$A1.location=="AICA")))
  altab=append(altab, nrow(subset(datatreat, datatreat$A1.location=="AICA"))/nrow(datatreat)*100)
  altab=append(altab, nrow(subset(datatreat, datatreat$A1.location=="VBJ")))
  altab=append(altab, nrow(subset(datatreat, datatreat$A1.location=="VBJ"))/nrow(datatreat)*100)
  altab=append(altab, nrow(subset(datatreat, datatreat$A1.location=="VA")))
  altab=append(altab, nrow(subset(datatreat, datatreat$A1.location=="VA"))/nrow(datatreat)*100)
  altab=append(altab, nrow(subset(datatreat, datatreat$A1.location=="PICA")))
  altab=append(altab, nrow(subset(datatreat, datatreat$A1.location=="PICA"))/nrow(datatreat)*100)
  
  if (treattype=="Coil"){
    result=as.data.frame(altab)
  } else {
    result=cbind(result, altab)
  }
  
  if (treattype=="Clip"){
    altab=c()
    altab=append(altab, nrow(subset(data, data$A1.location=="Acomm")))
    altab=append(altab, nrow(subset(data, data$A1.location=="Acomm"))/nrow(data)*100)
    altab=append(altab, nrow(subset(data, data$A1.location=="ACA")))
    altab=append(altab, nrow(subset(data, data$A1.location=="ACA"))/nrow(data)*100)
    altab=append(altab, nrow(subset(data, data$A1.location=="ICA")))
    altab=append(altab, nrow(subset(data, data$A1.location=="ICA"))/nrow(data)*100)
    altab=append(altab, nrow(subset(data, data$A1.location=="PC")))
    altab=append(altab, nrow(subset(data, data$A1.location=="PC"))/nrow(data)*100)
    altab=append(altab, nrow(subset(data, data$A1.location=="MCA")))
    altab=append(altab, nrow(subset(data, data$A1.location=="MCA"))/nrow(data)*100)
    altab=append(altab, nrow(subset(data, data$A1.location=="Pcomm")))
    altab=append(altab, nrow(subset(data, data$A1.location=="Pcomm"))/nrow(data)*100)
    altab=append(altab, nrow(subset(data, data$A1.location=="PCA")))
    altab=append(altab, nrow(subset(data, data$A1.location=="PCA"))/nrow(data)*100)
    altab=append(altab, nrow(subset(data, data$A1.location=="SCA")))
    altab=append(altab, nrow(subset(data, data$A1.location=="SCA"))/nrow(data)*100)
    altab=append(altab, nrow(subset(data, data$A1.location=="BA")))
    altab=append(altab, nrow(subset(data, data$A1.location=="BA"))/nrow(data)*100)
    altab=append(altab, nrow(subset(data, data$A1.location=="AICA")))
    altab=append(altab, nrow(subset(data, data$A1.location=="AICA"))/nrow(data)*100)
    altab=append(altab, nrow(subset(data, data$A1.location=="VBJ")))
    altab=append(altab, nrow(subset(data, data$A1.location=="VBJ"))/nrow(data)*100)
    altab=append(altab, nrow(subset(data, data$A1.location=="VA")))
    altab=append(altab, nrow(subset(data, data$A1.location=="VA"))/nrow(data)*100)
    altab=append(altab, nrow(subset(data, data$A1.location=="PICA")))
    altab=append(altab, nrow(subset(data, data$A1.location=="PICA"))/nrow(data)*100)
    result=cbind(result, altab)
  }
  
}
colnames(result)=c("Coil","Clip","Total")
rownames(result)=c("Acomm","1%","ACA","2%","ICA","3%","PC","4%","MCA","5%","Pcomm","6%","PCA","7%",
                   "SCA","8%","BA","9%","AICA","10%","VBJ","11%","VA","12%","PICA","13%")
#write.csv(result, "C:/Users/alexw/Google Drive/Desktop files/Dal Med/Aneurysm Repair Project/Data Analysis/Results/aneurysm location proportions.csv")
####

#Recategorize aneurysm location into anterior and posterior circulation
data$A1.location=as.character(data$A1.location)
data$A1.location[data$A1.location=="ACA" | 
                 data$A1.location=="Acomm"|
                 data$A1.location=="ICA"|
                 data$A1.location=="MCA"|
                 data$A1.location=="Pcomm"|
                 data$A1.location=="PC"
                 ]="Anterior"
data$A1.location[data$A1.location=="AICA" | 
                 data$A1.location=="BA"|
                 data$A1.location=="PCA"|
                 data$A1.location=="PICA"|
                 data$A1.location=="SCA"|
                 data$A1.location=="VA"|
                 data$A1.location=="VBJ"
                 ]="Posterior"
data$A1.location=as.factor(data$A1.location)


# Subgroup analyses by 5-year ---------------------------------------------

##Create age table

datetype=c("date_1","date_2","date_3")

for(year in datetype){
  column=c()
  datadate=subset(data, data[,"date"]==year)
  
  column=nrow(datadate)
  
  column=append(column, round(mean(subset(datadate, datadate$Treatment=="Coil")$Age.at.admission), digits=1))
  column=append(column, round(sd(subset(datadate, datadate$Treatment=="Coil")$Age.at.admission), digits=1))
  column=append(column, round(mean(subset(datadate, datadate$Treatment=="Clip")$Age.at.admission), digits=1))
  column=append(column, round(sd(subset(datadate, datadate$Treatment=="Clip")$Age.at.admission), digits=1))
  
  if (year=="date_1"){
    agetable=as.data.frame(column)
  }else{
    agetable=cbind(agetable, column)
  }
}
colnames(agetable)=c("2002-2007","2008-2012","2013-2017")
rownames(agetable)=c("n", "Coil mean age","Coil sd age", "Clip mean age","Clip sd age")

#Age table stats
age_table_stats=c()

age_table_stats=append(age_table_stats,
                       kruskal.test(Age.at.admission ~ date, 
                       data=subset(data,data$Treatment=="Coil"))$p.value)
  
age_table_stats=append(age_table_stats,
                       kruskal.test(Age.at.admission ~ date, 
                       data=subset(data,data$Treatment=="Clip"))$p.value)

age_table_stats=as.data.frame(age_table_stats)
rownames(age_table_stats)=c("Coil mean age","Clip mean age")
colnames(age_table_stats)=c("p-value")

data1=subset(data, data$date=="date_1")
data2=subset(data, data$date=="date_2")
data3=subset(data, data$date=="date_3")

age_table_stats_2 = c()

age_table_stats_2 = append(age_table_stats_2,
                           t.test(Age.at.admission ~ Treatment, data=data1)$p.value)

age_table_stats_2 = append(age_table_stats_2,
                           t.test(Age.at.admission ~ Treatment, data=data2)$p.value)

age_table_stats_2 = append(age_table_stats_2,
                           wilcox.test(Age.at.admission ~ Treatment, data=data3)$p.value)


# Table 1 values ----------------------------------------------------------
treattype=c("Coil","Clip")
for(treat in treattype){
  column=c()
  datatreat=subset(data, data[,"Treatment"]==treat)
  
  column=nrow(datatreat)
  column=append(column, round(mean(datatreat$Age.at.admission),digits=1))
  column=append(column, round(sd(datatreat$Age.at.admission),digits=1))
  
  column=append(column, nrow(subset(datatreat, datatreat$Gender=="M")))
  column=append(column, nrow(subset(datatreat, datatreat$Gender=="M"))/nrow(datatreat)*100)
  column=append(column, nrow(subset(datatreat, datatreat$Gender=="F")))
  column=append(column, nrow(subset(datatreat, datatreat$Gender=="F"))/nrow(datatreat)*100)
  
  column=append(column, nrow(subset(datatreat, datatreat$Current.smoke==1)))
  column=append(column, nrow(subset(datatreat, datatreat$Current.smoke==1))/nrow(datatreat)*100)
  column=append(column, nrow(subset(datatreat, datatreat$Current.smoke==0)))
  column=append(column, nrow(subset(datatreat, datatreat$Current.smoke==0))/nrow(datatreat)*100)
  
  column=append(column, nrow(subset(datatreat, datatreat$HTN==1)))
  column=append(column, nrow(subset(datatreat, datatreat$HTN==1))/nrow(datatreat)*100)
  column=append(column, nrow(subset(datatreat, datatreat$HTN==0)))
  column=append(column, nrow(subset(datatreat, datatreat$HTN==0))/nrow(datatreat)*100)
  
  column=append(column, nrow(subset(datatreat, datatreat$DM==1)))
  column=append(column, nrow(subset(datatreat, datatreat$DM==1))/nrow(datatreat)*100)
  column=append(column, nrow(subset(datatreat, datatreat$DM==0)))
  column=append(column, nrow(subset(datatreat, datatreat$DM==0))/nrow(datatreat)*100)
  
  column=append(column, nrow(subset(datatreat, datatreat$Past.SAH==1)))
  column=append(column, nrow(subset(datatreat, datatreat$Past.SAH==1))/nrow(datatreat)*100)
  column=append(column, nrow(subset(datatreat, datatreat$Past.SAH==0)))
  column=append(column, nrow(subset(datatreat, datatreat$Past.SAH==0))/nrow(datatreat)*100)
  
  column=append(column, nrow(subset(datatreat, datatreat$Past.stroke==1)))
  column=append(column, nrow(subset(datatreat, datatreat$Past.stroke==1))/nrow(datatreat)*100)
  column=append(column, nrow(subset(datatreat, datatreat$Past.stroke==0)))
  column=append(column, nrow(subset(datatreat, datatreat$Past.stroke==0))/nrow(datatreat)*100)
  
  column=append(column, round(mean(datatreat$Time.to.Treatment..days.),digits=1))
  column=append(column, round(sd(datatreat$Time.to.Treatment..days.),digits=1))
  
  column=append(column, nrow(subset(datatreat, datatreat$X..Aneurysms==1)))
  column=append(column, nrow(subset(datatreat, datatreat$X..Aneurysms==1))/nrow(datatreat)*100)
  column=append(column, nrow(subset(datatreat, datatreat$X..Aneurysms>1)))
  column=append(column, nrow(subset(datatreat, datatreat$X..Aneurysms>1))/nrow(datatreat)*100)
  
  column=append(column, round(mean(datatreat$A1.size),digits=1))
  column=append(column, round(sd(datatreat$A1.size),digits=1))
  
  column=append(column, nrow(subset(datatreat, datatreat$A1.location=="Anterior")))
  column=append(column, nrow(subset(datatreat, datatreat$A1.location=="Anterior"))/nrow(datatreat)*100)
  column=append(column, nrow(subset(datatreat, datatreat$A1.location=="Posterior")))
  column=append(column, nrow(subset(datatreat, datatreat$A1.location=="Posterior"))/nrow(datatreat)*100)
  
  column=append(column, nrow(subset(datatreat, datatreat$WFNS<4)))
  column=append(column, nrow(subset(datatreat, datatreat$WFNS<4))/nrow(datatreat)*100)
  column=append(column, nrow(subset(datatreat, datatreat$WFNS>=4)))
  column=append(column, nrow(subset(datatreat, datatreat$WFNS>=4))/nrow(datatreat)*100)
  
  column=append(column, nrow(subset(datatreat, datatreat$Fisher<3)))
  column=append(column, nrow(subset(datatreat, datatreat$Fisher<3))/nrow(datatreat)*100)
  column=append(column, nrow(subset(datatreat, datatreat$Fisher>=3)))
  column=append(column, nrow(subset(datatreat, datatreat$Fisher>=3))/nrow(datatreat)*100)
  
  column=append(column, round(mean(datatreat$Length.of.hospital.stay..day.),digits=1))
  column=append(column, round(sd(datatreat$Length.of.hospital.stay..day.),digits=1))
  
  if (treat=="Coil"){
    table1=as.data.frame(column)
  }else{
    table1=cbind(table1, column)
  }
}
colnames(table1)=c("Coil","Clip")
rownames(table1)=c("n",
                   "Mean Age at Admission","SD Age at Admission",
                   "M","M %",
                   "F","F %",
                   "Y Smoke","Y Smoke %",
                   "N Smoke","N Smoke %",
                   "Y HTN","Y HTN %",
                   "N HTN","N HTN %",
                   "Y DM","Y DM %",
                   "N DM","N DM %",
                   "Y PastSAH","Y PastSAH %",
                   "N PastSAH","N PastSAH %",
                   "Y stroke","Y stroke %",
                   "N stroke","N stroke %",
                   "Mean Time to treat (d)","SD Time to Treat",
                   "Single aneurysm", "% single aneurysm",
                   "Multiple aneurysms","% multiple aneurysms",
                   "Mean Culprit Aneurysm Size (mm)","SD Culprit Aneurysm Size",
                   "Anterior location","Anterior location %",
                   "Posterior location","Posterior location %",
                   "WFNS 1-3","WFNS 1-3 %",
                   "WFNS 4-5","WFNS 4-5 %",
                   "Fisher 1-2","Fisher 1-2 %",
                   "Fisher 3-4","Fisher 3-4 %",
                   "Mean Length of hospital stay (d)","SD Length of hospital stay (d)"
                   )
#write.csv(table1,"C:/Users/alexw/OneDrive/Dal Med/Aneurysm Repair Project/Data Analysis/Results/Table 1.csv")

# Table 1 statistics ------------------------------------------------------
data$Treatment <- factor(data$Treatment)
table1stats=c()

##T-test for age
table1stats=append(table1stats, t.test(Age.at.admission ~ Treatment, data=data)$p.value)

##Chi-sq test for gender
datacoil=subset(data, data$Treatment=="Coil")
dataclip=subset(data, data$Treatment=="Clip")

M=as.table(cbind(c(nrow(subset(datacoil, datacoil$Gender=="M")),nrow(subset(datacoil, datacoil$Gender=="F"))),
                 c(nrow(subset(dataclip, dataclip$Gender=="M")),nrow(subset(dataclip, dataclip$Gender=="F")))
                 ))
table1stats=append(table1stats,chisq.test(M)$p.value)

##Chi-sq test for smoke, HTN, DM, PastSAH, stroke
M=as.table(cbind(c(nrow(subset(datacoil, datacoil$Current.smoke==1)),nrow(subset(datacoil, datacoil$Current.smoke==0))),
                 c(nrow(subset(dataclip, dataclip$Current.smoke==1)),nrow(subset(dataclip, dataclip$Current.smoke==0)))
                 ))
table1stats=append(table1stats,chisq.test(M)$p.value)


M=as.table(cbind(c(nrow(subset(datacoil, datacoil$HTN==1)),nrow(subset(datacoil, datacoil$HTN==0))),
                 c(nrow(subset(dataclip, dataclip$HTN==1)),nrow(subset(dataclip, dataclip$HTN==0)))
))
table1stats=append(table1stats,chisq.test(M)$p.value)


M=as.table(cbind(c(nrow(subset(datacoil, datacoil$DM==1)),nrow(subset(datacoil, datacoil$DM==0))),
                 c(nrow(subset(dataclip, dataclip$DM==1)),nrow(subset(dataclip, dataclip$DM==0)))
))
table1stats=append(table1stats,chisq.test(M)$p.value)


M=as.table(cbind(c(nrow(subset(datacoil, datacoil$Past.SAH==1)),nrow(subset(datacoil, datacoil$Past.SAH==0))),
                 c(nrow(subset(dataclip, dataclip$Past.SAH==1)),nrow(subset(dataclip, dataclip$Past.SAH==0)))
))
table1stats=append(table1stats,fisher.test(M)$p.value)


M=as.table(cbind(c(nrow(subset(datacoil, datacoil$Past.stroke==1)),nrow(subset(datacoil, datacoil$Past.stroke==0))),
                 c(nrow(subset(dataclip, dataclip$Past.stroke==1)),nrow(subset(dataclip, dataclip$Past.stroke==0)))
))
table1stats=append(table1stats,fisher.test(M)$p.value)

##Mann_whitney test for Time to treatment
table1stats=append(table1stats, wilcox.test(Time.to.Treatment..days. ~ Treatment, data=data)$p.value)

##Chi-sq test for single vs multiple aneurysms
M=as.table(cbind(c(nrow(subset(datacoil, datacoil$X..Aneurysms==1)),nrow(subset(datacoil, datacoil$X..Aneurysms>1))),
                 c(nrow(subset(dataclip, dataclip$X..Aneurysms==1)),nrow(subset(dataclip, dataclip$X..Aneurysms>1)))
))
table1stats=append(table1stats,chisq.test(M)$p.value)

##Mann-Whitney test for culprit aneurysm size
table1stats=append(table1stats, wilcox.test(A1.size ~ Treatment, data=data)$p.value)

##Chi-sq test for aneurysm location
M=as.table(cbind(c(nrow(subset(datacoil, datacoil$A1.location=="Anterior")),nrow(subset(datacoil, datacoil$A1.location=="Posterior"))),
                 c(nrow(subset(dataclip, dataclip$A1.location=="Anterior")),nrow(subset(dataclip, dataclip$A1.location=="Posterior")))
))
table1stats=append(table1stats,chisq.test(M)$p.value)

##Chi-sq test for WFNS
M=as.table(cbind(c(nrow(subset(datacoil, datacoil$WFNS<4)),nrow(subset(datacoil, datacoil$WFNS>=4))),
                 c(nrow(subset(dataclip, dataclip$WFNS<4)),nrow(subset(dataclip, dataclip$WFNS>=4)))
))
table1stats=append(table1stats,chisq.test(M)$p.value)

##Chi-sq test for Fisher
M=as.table(cbind(c(nrow(subset(datacoil, datacoil$Fisher<3)),nrow(subset(datacoil, datacoil$Fisher>=3))),
                 c(nrow(subset(dataclip, dataclip$Fisher<3)),nrow(subset(dataclip, dataclip$Fisher>=3)))
))
table1stats=append(table1stats,chisq.test(M)$p.value)

##Mann Whitney test for length of hospital stay
table1stats=append(table1stats, wilcox.test(Length.of.hospital.stay..day. ~ Treatment, data=data)$p.value)

table1stats=as.data.frame(table1stats)
rownames(table1stats)=c("Age at admission",
                        "Gender",
                        "Smoke",
                        "HTN","DM","PastSAH","stroke","Time to treat","Single vs multiple aneurysms",
                        "Culprit aneurysm size","aneurysm location","WFNS","Fisher","Length of hospital stay"
                        )
#write.csv(table1stats, "C:/Users/alexw/OneDrive/Dal Med/Aneurysm Repair Project/Data Analysis/Results/Table 1 Stats.csv")


# Outcomes table ----------------------------------------------------------
for(treat in treattype){
  column=c()
  datatreat=subset(data, data[,"Treatment"]==treat)
  
  column=append(column, nrow(subset(datatreat, datatreat$GOS.at.discharge==0)))
  column=append(column, nrow(subset(datatreat, datatreat$GOS.at.discharge==0))/nrow(datatreat)*100)
  column=append(column, nrow(subset(datatreat, datatreat$GOS.at.discharge==1)))
  column=append(column, nrow(subset(datatreat, datatreat$GOS.at.discharge==1))/nrow(datatreat)*100)
  column=append(column, nrow(subset(datatreat, datatreat$GOS.at.discharge==2)))
  column=append(column, nrow(subset(datatreat, datatreat$GOS.at.discharge==2))/nrow(datatreat)*100)
  
  column=append(column, nrow(subset(datatreat, datatreat$GOS.at.6....3.months==0)))
  column=append(column, nrow(subset(datatreat, datatreat$GOS.at.6....3.months==0))/nrow(datatreat)*100)
  column=append(column, nrow(subset(datatreat, datatreat$GOS.at.6....3.months==1)))
  column=append(column, nrow(subset(datatreat, datatreat$GOS.at.6....3.months==1))/nrow(datatreat)*100)
  column=append(column, nrow(subset(datatreat, datatreat$GOS.at.6....3.months==2)))
  column=append(column, nrow(subset(datatreat, datatreat$GOS.at.6....3.months==2))/nrow(datatreat)*100)
  
  column=append(column, nrow(subset(datatreat, datatreat$GOS.at.12....3.months==0)))
  column=append(column, nrow(subset(datatreat, datatreat$GOS.at.12....3.months==0))/nrow(datatreat)*100)
  column=append(column, nrow(subset(datatreat, datatreat$GOS.at.12....3.months==1)))
  column=append(column, nrow(subset(datatreat, datatreat$GOS.at.12....3.months==1))/nrow(datatreat)*100)
  column=append(column, nrow(subset(datatreat, datatreat$GOS.at.12....3.months==2)))
  column=append(column, nrow(subset(datatreat, datatreat$GOS.at.12....3.months==2))/nrow(datatreat)*100)
  
  
  column=append(column, nrow(subset(datatreat, datatreat$Mortality==1)))
  column=append(column, nrow(subset(datatreat, datatreat$Mortality==1))/nrow(datatreat)*100)
  column=append(column, nrow(subset(datatreat, datatreat$Mortality==0)))
  column=append(column, nrow(subset(datatreat, datatreat$Mortality==0))/nrow(datatreat)*100)
  
  column=append(column, nrow(subset(datatreat, datatreat$Rebleed==1)))
  column=append(column, nrow(subset(datatreat, datatreat$Rebleed==1))/nrow(datatreat)*100)
  column=append(column, nrow(subset(datatreat, datatreat$Rebleed==0)))
  column=append(column, nrow(subset(datatreat, datatreat$Rebleed==0))/nrow(datatreat)*100)
  
  column=append(column, nrow(subset(datatreat, datatreat$Vasospasm==1)))
  column=append(column, nrow(subset(datatreat, datatreat$Vasospasm==1))/nrow(datatreat)*100)
  column=append(column, nrow(subset(datatreat, datatreat$Vasospasm==0)))
  column=append(column, nrow(subset(datatreat, datatreat$Vasospasm==0))/nrow(datatreat)*100)
  
  column=append(column, nrow(subset(datatreat, datatreat$Infarct==1)))
  column=append(column, nrow(subset(datatreat, datatreat$Infarct==1))/nrow(datatreat)*100)
  column=append(column, nrow(subset(datatreat, datatreat$Infarct==0)))
  column=append(column, nrow(subset(datatreat, datatreat$Infarct==0))/nrow(datatreat)*100)
  
  column=append(column, nrow(subset(datatreat, datatreat$Hydrocephalus==1)))
  column=append(column, nrow(subset(datatreat, datatreat$Hydrocephalus==1))/nrow(datatreat)*100)
  column=append(column, nrow(subset(datatreat, datatreat$Hydrocephalus==0)))
  column=append(column, nrow(subset(datatreat, datatreat$Hydrocephalus==0))/nrow(datatreat)*100)
  
  column=append(column, nrow(subset(datatreat, datatreat$Seizure==1)))
  column=append(column, nrow(subset(datatreat, datatreat$Seizure==1))/nrow(datatreat)*100)
  column=append(column, nrow(subset(datatreat, datatreat$Seizure==0)))
  column=append(column, nrow(subset(datatreat, datatreat$Seizure==0))/nrow(datatreat)*100)
  
  column=append(column, nrow(subset(datatreat, datatreat$Infection==1)))
  column=append(column, nrow(subset(datatreat, datatreat$Infection==1))/nrow(datatreat)*100)
  column=append(column, nrow(subset(datatreat, datatreat$Infection==0)))
  column=append(column, nrow(subset(datatreat, datatreat$Infection==0))/nrow(datatreat)*100)
  
  column=append(column, nrow(subset(datatreat, datatreat$Pneumonia==1)))
  column=append(column, nrow(subset(datatreat, datatreat$Pneumonia==1))/nrow(datatreat)*100)
  column=append(column, nrow(subset(datatreat, datatreat$Pneumonia==0)))
  column=append(column, nrow(subset(datatreat, datatreat$Pneumonia==0))/nrow(datatreat)*100)
  
  if (treat=="Coil"){
    outcomes=as.data.frame(column)
  }else{
    outcomes=cbind(outcomes, column)
  }
}

colnames(outcomes)=c("Coil","Clip")
rownames(outcomes)=c("GOS discharge 1","GOS discharge 1 %","GOS discharge 2-3","GOS discharge 2-3 %",
                     "GOS discharge 4-5","GOS discharge 4-5 %",
                     "GOS 6m 1","GOS 6m 1%","GOS 6m 2-3","GOS 6m 2-3%","GOS 6m 4-5","GOS 6m 4-5%",
                     "GOS 12m 1","GOS 12m 1%","GOS 12m 2-3","GOS 12m 2-3%","GOS 12m 4-5","GOS 12m 4-5%",
                     "Y Mortality","Y Mortality %",
                     "N Mortality","N Mortality %",
                     "Y Rebleed","Y Rebleed %",
                     "N Rebleed","N Rebleed %",
                     "Y Vasospasm","Y Vasospasm %",
                     "N Vasospasm","N Vasospasm %",
                     "Y Infarct","Y Infarct %",
                     "N Infarct","N Infarct %",
                     "Y Hydrocephalus","Y Hydrocephalus %",
                     "N Hydrocephalus","N Hydrocephalus %",
                     "Y Seizure","Y Seizure %",
                     "N Seizure","N Seizure %",
                     "Y Infection","Y Infection %",
                     "N Infection","N Infection %",
                     "Y Pneumonia","Y Pneumonia %",
                     "N Pneumonia","N Pneumonia %")

#write.csv(outcomes, "C:/Users/alexw/OneDrive/Dal Med/Aneurysm Repair Project/Data Analysis/Results/Outcomes table.csv")


# Outcomes statistics -----------------------------------------------------
outcomesstats=c()

M=as.table(cbind(c(nrow(subset(datacoil, datacoil$GOS.at.discharge==0)),
                   nrow(subset(datacoil,(datacoil$GOS.at.discharge==1))),
                   nrow(subset(datacoil, datacoil$GOS.at.discharge==2))),
                 c(nrow(subset(dataclip, dataclip$GOS.at.discharge==0)),
                   nrow(subset(dataclip,(dataclip$GOS.at.discharge==1))),
                   nrow(subset(dataclip, dataclip$GOS.at.discharge==2)))
))
outcomesstats=append(outcomesstats,chisq.test(M)$p.value)

M=as.table(cbind(c(nrow(subset(datacoil, datacoil$GOS.at.6....3.months==0)),
                   nrow(subset(datacoil,(datacoil$GOS.at.6....3.months==1))),
                   nrow(subset(datacoil, datacoil$GOS.at.6....3.months==2))),
                 c(nrow(subset(dataclip, dataclip$GOS.at.6....3.months==0)),
                   nrow(subset(dataclip,(dataclip$GOS.at.6....3.months==1))),
                   nrow(subset(dataclip, dataclip$GOS.at.6....3.months==2)))
))
outcomesstats=append(outcomesstats,chisq.test(M)$p.value)

M=as.table(cbind(c(nrow(subset(datacoil, datacoil$GOS.at.12....3.months==0)),
                   nrow(subset(datacoil,(datacoil$GOS.at.12....3.months==1))),
                   nrow(subset(datacoil, datacoil$GOS.at.12....3.months==2))),
                 c(nrow(subset(dataclip, dataclip$GOS.at.12....3.months==0)),
                   nrow(subset(dataclip,(dataclip$GOS.at.12....3.months==1))),
                   nrow(subset(dataclip, dataclip$GOS.at.12....3.months==2)))
))
outcomesstats=append(outcomesstats,chisq.test(M)$p.value)


M=as.table(cbind(c(nrow(subset(datacoil, datacoil$Mortality=="1")),nrow(subset(datacoil, datacoil$Mortality=="0"))),
                 c(nrow(subset(dataclip, dataclip$Mortality=="1")),nrow(subset(dataclip, dataclip$Mortality=="0")))
))
outcomesstats=append(outcomesstats,chisq.test(M)$p.value)

M=as.table(cbind(c(nrow(subset(datacoil, datacoil$Rebleed=="1")),nrow(subset(datacoil, datacoil$Rebleed=="0"))),
                 c(nrow(subset(dataclip, dataclip$Rebleed=="1")),nrow(subset(dataclip, dataclip$Rebleed=="0")))
))
outcomesstats=append(outcomesstats,chisq.test(M)$p.value)

M=as.table(cbind(c(nrow(subset(datacoil, datacoil$Vasospasm=="1")),nrow(subset(datacoil, datacoil$Vasospasm=="0"))),
                 c(nrow(subset(dataclip, dataclip$Vasospasm=="1")),nrow(subset(dataclip, dataclip$Vasospasm=="0")))
))
outcomesstats=append(outcomesstats,chisq.test(M)$p.value)

M=as.table(cbind(c(nrow(subset(datacoil, datacoil$Infarct=="1")),nrow(subset(datacoil, datacoil$Infarct=="0"))),
                 c(nrow(subset(dataclip, dataclip$Infarct=="1")),nrow(subset(dataclip, dataclip$Infarct=="0")))
))
outcomesstats=append(outcomesstats,chisq.test(M)$p.value)

M=as.table(cbind(c(nrow(subset(datacoil, datacoil$Hydrocephalus=="1")),nrow(subset(datacoil, datacoil$Hydrocephalus=="0"))),
                 c(nrow(subset(dataclip, dataclip$Hydrocephalus=="1")),nrow(subset(dataclip, dataclip$Hydrocephalus=="0")))
))
outcomesstats=append(outcomesstats,chisq.test(M)$p.value)

M=as.table(cbind(c(nrow(subset(datacoil, datacoil$Seizure=="1")),nrow(subset(datacoil, datacoil$Seizure=="0"))),
                 c(nrow(subset(dataclip, dataclip$Seizure=="1")),nrow(subset(dataclip, dataclip$Seizure=="0")))
))
outcomesstats=append(outcomesstats,fisher.test(M)$p.value)

M=as.table(cbind(c(nrow(subset(datacoil, datacoil$Infection=="1")),nrow(subset(datacoil, datacoil$Infection=="0"))),
                 c(nrow(subset(dataclip, dataclip$Infection=="1")),nrow(subset(dataclip, dataclip$Infection=="0")))
))
outcomesstats=append(outcomesstats,fisher.test(M)$p.value)

M=as.table(cbind(c(nrow(subset(datacoil, datacoil$Pneumonia=="1")),nrow(subset(datacoil, datacoil$Pneumonia=="0"))),
                 c(nrow(subset(dataclip, dataclip$Pneumonia=="1")),nrow(subset(dataclip, dataclip$Pneumonia=="0")))
))
outcomesstats=append(outcomesstats,fisher.test(M)$p.value)

outcomesstats=as.data.frame(outcomesstats)
rownames(outcomesstats)=c("GOS at discharge","GOS at 6m","GOS at 12m",
                          "Mortality","Rebleed","Vasospasm","Infarct",
                          "Hydrocephalus","Seizure","Infection","Pneumonia")

#write.csv(outcomesstats, "C:/Users/alexw/OneDrive/Dal Med/Aneurysm Repair Project/Data Analysis/Results/Outcomes stats.csv")


# GOS Regression analyses -----------------------------------------------------


##Recategorize age
summary(data$Age.at.admission)
data$Age.at.admission0=c(0)
data$Age.at.admission1=c(0)
data$Age.at.admission0[data$Age.at.admission<50]=1
data$Age.at.admission1[data$Age.at.admission>=50]=1
data$Age.at.admission1 <- factor(data$Age.at.admission1, levels=c(0,1), ordered=FALSE)

##Recategorize aneurysm location
data$A1.location=as.character(data$A1.location)
data$A1.location[data$A1.location=="Anterior"]=0
data$A1.location[data$A1.location=="Posterior"]=1
data$A1.location <- factor(data$A1.location, levels=c(0,1), ordered=FALSE)


##Set factor level order for HTN
data$HTN <- factor(data$HTN, levels=c(0,1), ordered=FALSE)

#Crude regression analyses

###GOS at discharge
modeltreatment <- polr(GOS.at.discharge ~ Treatment, data = data, Hess=TRUE)
ctable <- coef(summary(modeltreatment))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(modeltreatment)
exp(coef(modeltreatment))
exp(rbind(OR = coef(modeltreatment), ci))

modelhtn <- polr(GOS.at.discharge ~ HTN, data = data, Hess=TRUE)
ctable <- coef(summary(modelhtn))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(modelhtn)
exp(coef(modelhtn))
exp(rbind(OR = coef(modelhtn), ci))

modelage <- polr(GOS.at.discharge ~ Age.at.admission1, data = data, Hess=TRUE)
ctable <- coef(summary(modelage))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(modelage)
exp(coef(modelage))
exp(rbind(OR = coef(modelage), ci))

modelaloc <- polr(GOS.at.discharge ~ A1.location, data = data, Hess=TRUE)
ctable <- coef(summary(modelaloc))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(modelaloc)
exp(coef(modelaloc))
exp(rbind(OR = coef(modelaloc), ci))

###GOS at 6 months
modeltreatment <- polr(GOS.at.6....3.months ~ Treatment, data = data, Hess=TRUE)
ctable <- coef(summary(modeltreatment))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(modeltreatment)
exp(coef(modeltreatment))
exp(rbind(OR = coef(modeltreatment), ci))

modelhtn <- polr(GOS.at.6....3.months ~ HTN, data = data, Hess=TRUE)
ctable <- coef(summary(modelhtn))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(modelhtn)
exp(coef(modelhtn))
exp(rbind(OR = coef(modelhtn), ci))

modelage <- polr(GOS.at.6....3.months ~ Age.at.admission1, data = data, Hess=TRUE)
ctable <- coef(summary(modelage))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(modelage)
exp(coef(modelage))
exp(rbind(OR = coef(modelage), ci))

modelaloc <- polr(GOS.at.6....3.months ~ A1.location, data = data, Hess=TRUE)
ctable <- coef(summary(modelaloc))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(modelaloc)
exp(coef(modelaloc))
exp(rbind(OR = coef(modelaloc), ci))


###GOS at 12 months
modeltreatment <- polr(GOS.at.12....3.months ~ Treatment, data = data, Hess=TRUE)
ctable <- coef(summary(modeltreatment))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(modeltreatment)
exp(coef(modeltreatment))
exp(rbind(OR = coef(modeltreatment), ci))

modelhtn <- polr(GOS.at.12....3.months ~ HTN, data = data, Hess=TRUE)
ctable <- coef(summary(modelhtn))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(modelhtn)
exp(coef(modelhtn))
exp(rbind(OR = coef(modelhtn), ci))

modelage <- polr(GOS.at.12....3.months ~ Age.at.admission1, data = data, Hess=TRUE)
ctable <- coef(summary(modelage))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(modelage)
exp(coef(modelage))
exp(rbind(OR = coef(modelage), ci))

modelaloc <- polr(GOS.at.12....3.months ~ A1.location, data = data, Hess=TRUE)
ctable <- coef(summary(modelaloc))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(modelaloc)
exp(coef(modelaloc))
exp(rbind(OR = coef(modelaloc), ci))


#Adjusted regression analysis

##GOS at discharge
modeladjusted <- polr(GOS.at.discharge ~ Treatment + HTN + Age.at.admission1 + A1.location, data = data, Hess=TRUE)
ctable <- coef(summary(modeladjusted))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(modeladjusted)
exp(coef(modeladjusted))
exp(cbind(OR = coef(modeladjusted), ci))
PseudoR2(modeladjusted, which="all")

modeladjustedinteract <- polr(GOS.at.discharge ~ Treatment + HTN + Age.at.admission1 
                              + A1.location + HTN*Age.at.admission1, data = data, Hess=TRUE)
ctable <- coef(summary(modeladjustedinteract))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(modeladjustedinteract)
exp(coef(modeladjustedinteract))
exp(cbind(OR = coef(modeladjustedinteract), ci))
PseudoR2(modeladjustedinteract, which="all")

###Confusion matrix to calculate misclassification rate
predictGOS = predict(modeladjusted,data)
table(data$GOS.at.discharge, predictGOS)
mean(as.character(data$GOS.at.discharge) != as.character(predictGOS))

###Assess proportional odds assumption
brant(modeladjusted)

###Goodness of fit tests
logitgof(data$GOS.at.discharge, fitted(modeladjusted), g = 10, ord = TRUE)
lipsitz.test(modeladjusted)
pulkrob.chisq(modeladjusted, c("Treatment"))
pulkrob.deviance(modeladjusted, c("Treatment"))

##GOS at 6 months
modeladjusted <- polr(GOS.at.6....3.months ~ Treatment + HTN + Age.at.admission1 + A1.location, data = data, Hess=TRUE)
ctable <- coef(summary(modeladjusted))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(modeladjusted)
exp(coef(modeladjusted))
exp(cbind(OR = coef(modeladjusted), ci))
PseudoR2(modeladjusted, which="all")

modeladjustedinteract <- polr(GOS.at.6....3.months ~ Treatment + HTN + Age.at.admission1 
                      + A1.location + HTN*Age.at.admission1, data = data, Hess=TRUE)
ctable <- coef(summary(modeladjustedinteract))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(modeladjustedinteract)
exp(coef(modeladjustedinteract))
exp(cbind(OR = coef(modeladjustedinteract), ci))
PseudoR2(modeladjustedinteract, which="all")

###Confusion matrix to calculate misclassification rate
predictGOS = predict(modeladjusted,data)
table(data$GOS.at.6....3.months, predictGOS)
mean(as.character(data$GOS.at.6....3.months) != as.character(predictGOS))

###Assess proportional odds assumption
brant(modeladjusted)

###Goodness of fit tests
logitgof(data$GOS.at.6....3.months, fitted(modeladjusted), g = 10, ord = TRUE)
lipsitz.test(modeladjusted)
pulkrob.chisq(modeladjusted, c("Treatment"))
pulkrob.deviance(modeladjusted, c("Treatment"))

##GOS at 12 months
modeladjusted <- polr(GOS.at.12....3.months ~ Treatment + HTN + Age.at.admission1 + A1.location, data = data, Hess=TRUE)
ctable <- coef(summary(modeladjusted))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(modeladjusted)
exp(coef(modeladjusted))
exp(cbind(OR = coef(modeladjusted), ci))
PseudoR2(modeladjusted, which="all")


modeladjustedinteract <- polr(GOS.at.12....3.months ~ Treatment + HTN + Age.at.admission1 
                      + A1.location + HTN*Age.at.admission1, data = data, Hess=TRUE)
ctable <- coef(summary(modeladjustedinteract))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(modeladjustedinteract)
exp(coef(modeladjustedinteract))
exp(cbind(OR = coef(modeladjustedinteract), ci))
PseudoR2(modeladjustedinteract, which="all")

###Confusion matrix to calculate misclassification rate
predictGOS = predict(modeladjusted,data)
table(data$GOS.at.12....3.months, predictGOS)
mean(as.character(data$GOS.at.12....3.months) != as.character(predictGOS))

###Assess proportional odds assumption
brant(modeladjusted)

###Goodness of fit tests
logitgof(data$GOS.at.12....3.months, fitted(modeladjusted), g = 10, ord = TRUE)
lipsitz.test(modeladjusted)
pulkrob.chisq(modeladjusted, c("Treatment"))
pulkrob.deviance(modeladjusted, c("Treatment"))

##Assessing proportional odds assumption
#sf <- function(y) {
#  c('Y>=0' = qlogis(mean(y >= 0)),
#    'Y>=1' = qlogis(mean(y >= 1)),
#    'Y>=2' = qlogis(mean(y >= 2)))
#}
#
#(s <- with(data, summary(as.numeric(GOS.at.discharge) ~ Treatment + HTN + Age.at.admission1, fun=sf)))
#p<-plot(s, which=1:3, pch=1:3, xlab='logit', main=' ', xlim=range(s[,3:4]))


# GOS regression subgroup analysis by 5 year periods ----------------------

data1=subset(data, data$date=="date_1")
data2=subset(data, data$date=="date_2")
data3=subset(data, data$date=="date_3")

# 2002-2007
### GOS at discharge
modelcrude <- polr(GOS.at.discharge ~ Treatment, data = data1, Hess=TRUE)
ctable <- coef(summary(modelcrude))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(modelcrude)
modelcrude
exp(coef(modelcrude))
exp(cbind(OR = coef(modelcrude), ci))
PseudoR2(modelcrude, which="all")
ctable


modeladjusted <- polr(GOS.at.discharge ~ Treatment + HTN + Age.at.admission1 + A1.location, data = data1, Hess=TRUE)
ctable <- coef(summary(modeladjusted))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(modeladjusted)
modeladjusted
exp(coef(modeladjusted))
exp(cbind(OR = coef(modeladjusted), ci))
PseudoR2(modeladjusted, which="all")
ctable

### GOS at 6 months
modelcrude <- polr(GOS.at.6....3.months ~ Treatment, data = data1, Hess=TRUE)
ctable <- coef(summary(modelcrude))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(modelcrude)
modelcrude
exp(coef(modelcrude))
exp(cbind(OR = coef(modelcrude), ci))
PseudoR2(modelcrude, which="all")
ctable


modeladjusted <- polr(GOS.at.6....3.months ~ Treatment + HTN + Age.at.admission1 + A1.location, data = data1, Hess=TRUE)
ctable <- coef(summary(modeladjusted))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(modeladjusted)
modeladjusted
exp(coef(modeladjusted))
exp(cbind(OR = coef(modeladjusted), ci))
PseudoR2(modeladjusted, which="all")
ctable

### GOS at 12 months

modelcrude <- polr(GOS.at.12....3.months ~ Treatment, data = data1, Hess=TRUE)
ctable <- coef(summary(modelcrude))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(modelcrude)
modelcrude
exp(coef(modelcrude))
exp(cbind(OR = coef(modelcrude), ci))
PseudoR2(modelcrude, which="all")
ctable


modeladjusted <- polr(GOS.at.12....3.months ~ Treatment + HTN + Age.at.admission1 + A1.location, data = data1, Hess=TRUE)
ctable <- coef(summary(modeladjusted))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(modeladjusted)
modeladjusted
exp(coef(modeladjusted))
exp(cbind(OR = coef(modeladjusted), ci))
PseudoR2(modeladjusted, which="all")
ctable

# 2008-2012
### GOS at discharge
modelcrude <- polr(GOS.at.discharge ~ Treatment, data = data2, Hess=TRUE)
ctable <- coef(summary(modelcrude))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(modelcrude)
modelcrude
exp(coef(modelcrude))
exp(cbind(OR = coef(modelcrude), ci))
PseudoR2(modelcrude, which="all")
ctable


modeladjusted <- polr(GOS.at.discharge ~ Treatment + HTN + Age.at.admission1 + A1.location, data = data2, Hess=TRUE)
ctable <- coef(summary(modeladjusted))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(modeladjusted)
modeladjusted
exp(coef(modeladjusted))
exp(cbind(OR = coef(modeladjusted), ci))
PseudoR2(modeladjusted, which="all")
ctable

### GOS at 6 months
modelcrude <- polr(GOS.at.6....3.months ~ Treatment, data = data2, Hess=TRUE)
ctable <- coef(summary(modelcrude))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(modelcrude)
modelcrude
exp(coef(modelcrude))
exp(cbind(OR = coef(modelcrude), ci))
PseudoR2(modelcrude, which="all")
ctable


modeladjusted <- polr(GOS.at.6....3.months ~ Treatment + HTN + Age.at.admission1 + A1.location, data = data2, Hess=TRUE)
ctable <- coef(summary(modeladjusted))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(modeladjusted)
modeladjusted
exp(coef(modeladjusted))
exp(cbind(OR = coef(modeladjusted), ci))
PseudoR2(modeladjusted, which="all")
ctable

### GOS at 12 months

modelcrude <- polr(GOS.at.12....3.months ~ Treatment, data = data2, Hess=TRUE)
ctable <- coef(summary(modelcrude))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(modelcrude)
modelcrude
exp(coef(modelcrude))
exp(cbind(OR = coef(modelcrude), ci))
PseudoR2(modelcrude, which="all")
ctable


modeladjusted <- polr(GOS.at.12....3.months ~ Treatment + HTN + Age.at.admission1 + A1.location, data = data2, Hess=TRUE)
ctable <- coef(summary(modeladjusted))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(modeladjusted)
modeladjusted
exp(coef(modeladjusted))
exp(cbind(OR = coef(modeladjusted), ci))
PseudoR2(modeladjusted, which="all")
ctable

#2013-2017

### GOS at discharge
modelcrude <- polr(GOS.at.discharge ~ Treatment, data = data3, Hess=TRUE)
ctable <- coef(summary(modelcrude))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(modelcrude)
modelcrude
exp(coef(modelcrude))
exp(cbind(OR = coef(modelcrude), ci))
PseudoR2(modelcrude, which="all")
ctable


modeladjusted <- polr(GOS.at.discharge ~ Treatment + HTN + Age.at.admission1 + A1.location, data = data3, Hess=TRUE)
ctable <- coef(summary(modeladjusted))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(modeladjusted)
modeladjusted
exp(coef(modeladjusted))
exp(cbind(OR = coef(modeladjusted), ci))
PseudoR2(modeladjusted, which="all")
ctable

### GOS at 6 months
modelcrude <- polr(GOS.at.6....3.months ~ Treatment, data = data3, Hess=TRUE)
ctable <- coef(summary(modelcrude))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(modelcrude)
modelcrude
exp(coef(modelcrude))
exp(cbind(OR = coef(modelcrude), ci))
PseudoR2(modelcrude, which="all")
ctable


modeladjusted <- polr(GOS.at.6....3.months ~ Treatment + HTN + Age.at.admission1 + A1.location, data = data3, Hess=TRUE)
ctable <- coef(summary(modeladjusted))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(modeladjusted)
modeladjusted
exp(coef(modeladjusted))
exp(cbind(OR = coef(modeladjusted), ci))
PseudoR2(modeladjusted, which="all")
ctable

### GOS at 12 months

modelcrude <- polr(GOS.at.12....3.months ~ Treatment, data = data3, Hess=TRUE)
ctable <- coef(summary(modelcrude))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(modelcrude)
modelcrude
exp(coef(modelcrude))
exp(cbind(OR = coef(modelcrude), ci))
PseudoR2(modelcrude, which="all")
ctable


modeladjusted <- polr(GOS.at.12....3.months ~ Treatment + HTN + Age.at.admission1 + A1.location, data = data3, Hess=TRUE)
ctable <- coef(summary(modeladjusted))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(modeladjusted)
modeladjusted
exp(coef(modeladjusted))
exp(cbind(OR = coef(modeladjusted), ci))
PseudoR2(modeladjusted, which="all")
ctable


# Secondary Outcomes Regression Analyses ----------------------------------

#Crude models

data$Mortality <- factor(data$Mortality, levels=c(0,1), ordered=FALSE)
modelmort=glm(Mortality ~ Treatment, data=data, family="binomial")
summary(modelmort)
#wald.test(b = coef(modelmort), Sigma = vcov(modelmort), Terms = 4:6)
exp(coef(modelmort))
exp(cbind(OR = coef(modelmort), confint(modelmort)))

data$Rebleed <- factor(data$Rebleed, levels=c(0,1), ordered=FALSE)
modelrebleed=glm(Rebleed ~ Treatment, data=data, family="binomial")
summary(modelrebleed)
exp(coef(modelrebleed))
exp(cbind(OR = coef(modelrebleed), confint(modelrebleed)))

data$Vasospasm <- factor(data$Vasospasm, levels=c(0,1), ordered=FALSE)
modelvasospasm=glm(Vasospasm ~ Treatment, data=data, family="binomial")
summary(modelvasospasm)
exp(coef(modelvasospasm))
exp(cbind(OR = coef(modelvasospasm), confint(modelvasospasm)))

data$Infarct <- factor(data$Infarct, levels=c(0,1), ordered=FALSE)
modelinfarct=glm(Infarct ~ Treatment, data=data, family="binomial")
summary(modelinfarct)
exp(coef(modelinfarct))
exp(cbind(OR = coef(modelinfarct), confint(modelinfarct)))

data$Hydrocephalus <- factor(data$Hydrocephalus, levels=c(0,1), ordered=FALSE)
modelhydro=glm(Hydrocephalus ~ Treatment, data=data, family="binomial")
summary(modelhydro)
exp(coef(modelhydro))
exp(cbind(OR = coef(modelhydro), confint(modelhydro)))

data$Seizure <- factor(data$Seizure, levels=c(0,1), ordered=FALSE)
modelseize=glm(Seizure ~ Treatment, data=data, family="binomial")
summary(modelseize)
exp(coef(modelseize))
exp(cbind(OR = coef(modelseize), confint(modelseize)))

data$Infection <- factor(data$Infection, levels=c(0,1), ordered=FALSE)
modelinfect=glm(Infection ~ Treatment, data=data, family="binomial")
summary(modelinfect)
exp(coef(modelinfect))
exp(cbind(OR = coef(modelinfect), confint(modelinfect)))

data$Pneumonia <- factor(data$Pneumonia, levels=c(0,1), ordered=FALSE)
modelpneum=glm(Pneumonia ~ Treatment, data=data, family="binomial")
summary(modelpneum)
exp(coef(modelpneum))
exp(cbind(OR = coef(modelpneum), confint(modelpneum)))

#Adjusted models

modelmort=glm(Mortality ~ Treatment + HTN + Age.at.admission1 + A1.location, data=data, family="binomial")
summary(modelmort)
exp(coef(modelmort))
exp(cbind(OR = coef(modelmort), confint(modelmort)))
logitgof(data$Mortality, fitted(modelmort))
rocmort=roc(data$Mortality, predict(modelmort,type=c("response")), ci=TRUE, auc=TRUE)
plot.roc(rocmort, print.auc=TRUE, xlim=c(1,0), xlab="1-Specificity", asp=NA)
roc.area(data$Mortality, data$Treatment)


modelrebleed=glm(Rebleed ~ Treatment + HTN + Age.at.admission1 + A1.location, data=data, family="binomial")
summary(modelrebleed)
exp(coef(modelrebleed))
exp(cbind(OR = coef(modelrebleed), confint(modelrebleed)))
logitgof(data$Rebleed, fitted(modelrebleed))
rocrebleed=roc(data$Rebleed, predict(modelrebleed,type=c("response")), ci=TRUE, auc=TRUE)
plot.roc(rocrebleed, print.auc=TRUE, xlim=c(1,0), xlab="1-Specificity", asp=NA)


modelvasospasm=glm(Vasospasm ~ Treatment + HTN + Age.at.admission1 + A1.location, data=data, family="binomial")
summary(modelvasospasm)
exp(coef(modelvasospasm))
exp(cbind(OR = coef(modelvasospasm), confint(modelvasospasm)))
logitgof(data$Vasospasm, fitted(modelvasospasm))
rocvasospasm=roc(data$Vasospasm, predict(modelvasospasm,type=c("response")), ci=TRUE, auc=TRUE)
plot.roc(rocvasospasm, print.auc=TRUE, xlim=c(1,0), xlab="1-Specificity", asp=NA)


modelinfarct=glm(Infarct ~ Treatment + HTN + Age.at.admission1 + A1.location, data=data, family="binomial")
summary(modelinfarct)
exp(coef(modelinfarct))
exp(cbind(OR = coef(modelinfarct), confint(modelinfarct)))
logitgof(data$Infarct, fitted(modelinfarct))
rocinfarct=roc(data$Infarct, predict(modelinfarct,type=c("response")), ci=TRUE, auc=TRUE)
plot.roc(rocinfarct, print.auc=TRUE, xlim=c(1,0), xlab="1-Specificity", asp=NA)


modelhydro=glm(Hydrocephalus ~ Treatment + HTN + Age.at.admission1 + A1.location, data=data, family="binomial")
summary(modelhydro)
exp(coef(modelhydro))
exp(cbind(OR = coef(modelhydro), confint(modelhydro)))
logitgof(data$Hydrocephalus, fitted(modelhydro))
rochydro=roc(data$Hydrocephalus, predict(modelhydro,type=c("response")), ci=TRUE, auc=TRUE)
plot.roc(rochydro, print.auc=TRUE, xlim=c(1,0), xlab="1-Specificity", asp=NA)


modelseize=glm(Seizure ~ Treatment + HTN + Age.at.admission1 + A1.location, data=data, family="binomial")
summary(modelseize)
exp(coef(modelseize))
exp(cbind(OR = coef(modelseize), confint(modelseize)))
logitgof(data$Seizure, fitted(modelseize))
rocseize=roc(data$Seizure, predict(modelseize,type=c("response")), ci=TRUE, auc=TRUE)
plot.roc(rocseize, print.auc=TRUE, xlim=c(1,0), xlab="1-Specificity", asp=NA)

 
modelinfect=glm(Infection ~ Treatment + HTN + Age.at.admission1 + A1.location, data=data, family="binomial")
summary(modelinfect)
exp(coef(modelinfect))
exp(cbind(OR = coef(modelinfect), confint(modelinfect)))
logitgof(data$Infection, fitted(modelinfect))
rocinfect=roc(data$Infection, predict(modelinfect,type=c("response")), ci=TRUE, auc=TRUE)
plot.roc(rocinfect, print.auc=TRUE, xlim=c(1,0), xlab="1-Specificity", asp=NA)


modelpneum=glm(Pneumonia ~ Treatment + HTN + Age.at.admission1 + A1.location, data=data, family="binomial")
summary(modelpneum)
exp(coef(modelpneum))
exp(cbind(OR = coef(modelpneum), confint(modelpneum)))
logitgof(data$Pneumonia, fitted(modelpneum))
rocpneum=roc(data$Pneumonia, predict(modelpneum,type=c("response")), ci=TRUE, auc=TRUE)
plot.roc(rocpneum, print.auc=TRUE, xlim=c(1,0), xlab="1-Specificity", asp=NA)


