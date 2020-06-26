library(googlesheets4)
library(googledrive)
library(irr)
library(stringr)
library(blandr)
library(ggplot2)
library(cowplot)
library(data.table)
library(stringr)

# Open data files ----

fulldb=readxl::read_xls("D:/Google Drive/Desktop files/Dal Med/ICH/PREDICTprimaryV3_TH.xls", sheet="FullDB")
fulldb=as.data.frame(fulldb)

primary_anal=readxl::read_xls("D:/Google Drive/Desktop files/Dal Med/ICH/PREDICTprimaryV3_TH.xls", sheet="PrimaryAnalysis")
primary_anal=as.data.frame(primary_anal)

enrolled=readxl::read_xls("D:/Google Drive/Desktop files/Dal Med/ICH/PREDICTprimaryV3_TH.xls", sheet="enrolled")
enrolled=as.data.frame(enrolled)

dcm_join_files=transpose(as.data.frame(read.csv("D:/ICH Files/PREDICT/dcm_join_files.csv", header=FALSE)))
dcm_add_files=transpose(as.data.frame(read.csv("D:/ICH Files/PREDICT/dcm_add_files.csv", header=FALSE)))
dcm_previous_files=transpose(as.data.frame(read.csv("D:/ICH Files/PREDICT/dcm_previous_files.csv", header=FALSE)))

dcm_combined=rbind(dcm_join_files, dcm_add_files, dcm_previous_files)

fulldb$patient_site=str_pad(fulldb$patient_site, 2, pad="0")
fulldb$patient_study=str_pad(fulldb$patient_study, 3, pad="0")
fulldb$PID=paste(fulldb$patient_site, fulldb$patient_study, sep="")

primary_anal$patient_site=str_pad(primary_anal$patient_site, 2, pad="0")
primary_anal$patient_study=str_pad(primary_anal$patient_study, 3, pad="0")
primary_anal$PID=paste(primary_anal$patient_site, primary_anal$patient_study, sep="")

enrolled$patient_site=str_pad(enrolled$patient_site, 2, pad="0")
enrolled$patient_study=str_pad(enrolled$patient_study, 3, pad="0")
enrolled$PID=paste(enrolled$patient_site, enrolled$patient_study, sep="")

# Identify IVH cases in PREDICT ADD and JOIN ----

keep_cols=c("PID","IVH base","IVH 24")
ivh_data=fulldb[keep_cols]
ivh_data_base=ivh_data[c("PID","IVH base")]
ivh_data_24h=ivh_data[c("PID","IVH 24")]

dcm_combined$patient_site=substr(dcm_combined$V1, 9, 10)
dcm_combined$patient_study=substr(dcm_combined$V1, 12, 14)
dcm_combined$PID=paste(dcm_combined$patient_site, dcm_combined$patient_study, sep="")
dcm_combined$time=c(0)
dcm_combined[grepl("Base",dcm_combined$V1), "time"]=0
dcm_combined[grepl("Follow",dcm_combined$V1), "time"]=1

dcm_combined_base=subset(dcm_combined, dcm_combined$time==0)
dcm_combined_24h=subset(dcm_combined, dcm_combined$time==1)

dcm_combined_base_ivh=merge(x=dcm_combined_base, y=ivh_data_base, by="PID", all.x=TRUE)
dcm_combined_24h_ivh=merge(x=dcm_combined_24h, y=ivh_data_24h, by="PID", all.x=TRUE)

names(dcm_combined_base_ivh)[names(dcm_combined_base_ivh)=="IVH base"]="IVH"
names(dcm_combined_24h_ivh)[names(dcm_combined_24h_ivh)=="IVH 24"]="IVH"

dcm_combined_ivh=rbind(dcm_combined_base_ivh, dcm_combined_24h_ivh)
dcm_combined_ivh$'Abbrev filename'=str_sub(dcm_combined_ivh$V1, 9, -5)
substr(dcm_combined_ivh$`Abbrev filename`, 7, 7)="-"
#write.csv(dcm_combined_ivh, "D:/Google Drive/Desktop files/Dal Med/ICH/dcm_combined_ivh.csv")

# Access Gdrive ----
#gs4_auth()
for_gs <- read_sheet("1c44iNPzrDiZ0DzBb1eAggm3VBd5ImMEnsFcWgzcZWC0", sheet="Copy of PREDICT V6 TOTAL")
for_gs=as.data.frame(for_gs)
dcm_list=for_gs$PID

# Included/Excluded studies ----
fulldb$inc_exc=c(99)
fulldb$inc_exc[!((fulldb$PID %in% primary_anal$PID) | (fulldb$PID %in% enrolled$PID)) & !(fulldb$PID %in% dcm_list)]=0
fulldb$inc_exc[((fulldb$PID %in% primary_anal$PID) | (fulldb$PID %in% enrolled$PID)) & fulldb$PID %in% dcm_list]=1
fulldb$inc_exc[((fulldb$PID %in% primary_anal$PID) | (fulldb$PID %in% enrolled$PID)) & !(fulldb$PID %in% dcm_list)]=2
fulldb$inc_exc[!((fulldb$PID %in% primary_anal$PID) | (fulldb$PID %in% enrolled$PID)) & fulldb$PID %in% dcm_list]=3

inc_exc_0=subset(fulldb, fulldb$inc_exc==0)
inc_exc_1=subset(fulldb, fulldb$inc_exc==1)
inc_exc_2=subset(fulldb, fulldb$inc_exc==2)
inc_exc_3=subset(fulldb, fulldb$inc_exc==3)

# Patients not in PREDICT demographics xls at all, but included in our study
dcm_list=as.data.frame(dcm_list)
missing_dcm=subset(dcm_list, !(dcm_list %in% fulldb$PID))

# Patients with IVH in our study
ivh_dcm=subset(for_gs, for_gs$`IVH Present`==1)
ivh_pt_list=ivh_dcm$PID






