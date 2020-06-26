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

fulldb=readxl::read_xlsx("D:/Google Drive/Desktop files/Dal Med/ICH/Archive/GraebCombined_withmRS (Recovered).xlsx", sheet="ALL")
fulldb=as.data.frame(fulldb)
keep_cols=c("V1", "PID", "time","study")
# SPOTLIGHT

dcm_add_files=transpose(as.data.frame(read.csv("D:/ICH Files/SPOTLIGHT/dcm_add_files.csv", fileEncoding="UTF-8-BOM", header=FALSE)))
dcm_previous_files=transpose(as.data.frame(read.csv("D:/ICH Files/SPOTLIGHT/dcm_previous_files.csv", fileEncoding="UTF-8-BOM", header=FALSE)))

SL_dcm_combined_files=rbind(dcm_add_files, dcm_previous_files)
SL_dcm_combined_files$id_1=substr(SL_dcm_combined_files$V1, 11, 13)
SL_dcm_combined_files$id_2=substr(SL_dcm_combined_files$V1, 15, 18)
SL_dcm_combined_files$time=substr(SL_dcm_combined_files$V1, 20, 20)

SL_dcm_combined_files$PID=paste(SL_dcm_combined_files$id_1, SL_dcm_combined_files$id_2, sep="")
SL_dcm_combined_files$study=c("SPOTLIGHT")
SL_dcm_combined_files=SL_dcm_combined_files[keep_cols]

# STOP-IT

dcm_add_files=transpose(as.data.frame(read.csv("D:/ICH Files/STOP-IT/dcm_add_files.csv", fileEncoding="UTF-8-BOM", header=FALSE)))
dcm_previous_files=transpose(as.data.frame(read.csv("D:/ICH Files/STOP-IT/dcm_previous_files.csv", fileEncoding="UTF-8-BOM", header=FALSE)))

SI_dcm_combined_files=rbind(dcm_add_files, dcm_previous_files)
SI_dcm_combined_files$id_1=substr(SI_dcm_combined_files$V1, 1, 2)
SI_dcm_combined_files$id_1=str_pad(SI_dcm_combined_files$id_1, 3, pad="0")

SI_dcm_combined_files$id_2=substr(SI_dcm_combined_files$V1, 4, 5)

SI_dcm_combined_files$id_3=substr(SI_dcm_combined_files$V1, 7, 9)
SI_dcm_combined_files$id_3=substr(SI_dcm_combined_files$id_3, 2, 3)

SI_dcm_combined_files$time=c(99)
SI_dcm_combined_files$time[grepl("Base",SI_dcm_combined_files$V1)]=0
SI_dcm_combined_files$time[grepl("Follow",SI_dcm_combined_files$V1)]=1

SI_dcm_combined_files$PID=paste(SI_dcm_combined_files$id_2, SI_dcm_combined_files$id_1, SI_dcm_combined_files$id_3, sep="")
SI_dcm_combined_files$study=c("STOP-IT")
SI_dcm_combined_files=SI_dcm_combined_files[keep_cols]

# SPOTLIGHT STOP IT Merge
SL_SI_dcm_files=rbind(SL_dcm_combined_files, SI_dcm_combined_files)

# Included/Excluded Studies ----
dcm_list=SL_SI_dcm_files$PID

fulldb$inc_exc=c(99)
fulldb$inc_exc[(fulldb$SPOT_Present==2)& !(fulldb$PID %in% dcm_list)]=0
fulldb$inc_exc[(fulldb$SPOT_Present==1)& (fulldb$PID %in% dcm_list)]=1
fulldb$inc_exc[(fulldb$SPOT_Present==1)& !(fulldb$PID %in% dcm_list)]=2
fulldb$inc_exc[(fulldb$SPOT_Present==2)& (fulldb$PID %in% dcm_list)]=3

# Patients not in SL/SI demographics xls at all, but included in our study
dcm_list=SL_SI_dcm_files[,c("PID","study")]
missing_dcm=subset(dcm_list, !(dcm_list$PID %in% fulldb$PID))
missing_pid_list=as.factor(as.character(missing_dcm$PID))




