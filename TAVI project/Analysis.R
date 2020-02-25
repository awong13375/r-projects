# TAVI EVOLUTION, PART 2
# UPDATED: 2020-03-08

# LIBRARIES ----

library(gmodels)

data=read.csv("C:/Users/alexw/Google Drive/Desktop files/Dal Med/Med2/TAVI Project/TAVI.csv")
summary(data)
data$dt_tavi <- as.Date(as.character(data$dt_tavi),"%d/%m/%Y")

# Data cleaning/recategorize ----

## Subset data into 3 eras by TAVI date
data$tavi_era=c(0)
data$tavi_era[data$dt_tavi>=as.Date("2017-01-01")]=2
data$tavi_era[data$dt_tavi<as.Date("2017-01-01")&data$dt_tavi>=as.Date("2015-01-01")]=1
data$tavi_era[data$dt_tavi<as.Date("2015-01-01")]=0
data$tavi_era[is.na(data$dt_tavi)]=NA

# Table 1 values ----
eras=c(0, 1, 2)
for (era in eras){
  subdata=subset(data, data$tavi_era==era)
  
  column=c()
  
  column=append(column, nrow(subset(subdata)))
  
  column=append(column, mean(subdata$pt_age, na.rm=TRUE))
  column=append(column, sd(subdata$pt_age, na.rm=TRUE))
  column=append(column, nrow(subset(subdata, is.na(subdata$pt_age))))
  
  column=append(column, nrow(subset(subdata, subdata$pt_sex=="F")))
  column=append(column, nrow(subset(subdata, subdata$pt_sex=="F"))/nrow(subdata)*100)
  column=append(column, nrow(subset(subdata, is.na(subdata$pt_sex))))
  
  column=append(column, nrow(subset(subdata, subdata$rf_diabetes==1)))
  column=append(column, nrow(subset(subdata, subdata$rf_diabetes==1))/nrow(subdata)*100)
  column=append(column, nrow(subset(subdata, is.na(subdata$rf_diabetes))))
  
  column=append(column, nrow(subset(subdata, subdata$rf_cva==1|subdata$rf_dementia==1|subdata$rf_tia==1)))
  column=append(column, nrow(subset(subdata, subdata$rf_cva==1|subdata$rf_dementia==1|subdata$rf_tia==1))/nrow(subdata)*100)
  column=append(column, nrow(subset(subdata, is.na(subdata$rf_cva&subdata$rf_dementia&subdata$rf_tia))))
  
  column=append(column, nrow(subset(subdata, subdata$rf_carotid==1)))
  column=append(column, nrow(subset(subdata, subdata$rf_carotid==1))/nrow(subdata)*100)
  column=append(column, nrow(subset(subdata, is.na(subdata$rf_carotid))))
  
  column=append(column, nrow(subset(subdata, subdata$rf_renal_failure==1)))
  column=append(column, nrow(subset(subdata, subdata$rf_renal_failure==1))/nrow(subdata)*100)
  column=append(column, nrow(subset(subdata, is.na(subdata$rf_renal_failure))))
  
  column=append(column, nrow(subset(subdata, subdata$rf_pulm==1)))
  column=append(column, nrow(subset(subdata, subdata$rf_pulm==1))/nrow(subdata)*100)
  column=append(column, nrow(subset(subdata, is.na(subdata$rf_pulm))))
  
  column=append(column, nrow(subset(subdata, subdata$prev_pci==1)))
  column=append(column, nrow(subset(subdata, subdata$prev_pci==1))/nrow(subdata)*100)
  column=append(column, nrow(subset(subdata, is.na(subdata$prev_pci))))
  
  column=append(column, nrow(subset(subdata, subdata$prev_cabg==1)))
  column=append(column, nrow(subset(subdata, subdata$prev_cabg==1))/nrow(subdata)*100)
  column=append(column, nrow(subset(subdata, is.na(subdata$prev_cabg))))
  
  column=append(column, nrow(subset(subdata, subdata$rf_nyha=="III"|subdata$rf_nyha=="IV")))
  column=append(column, nrow(subset(subdata, subdata$rf_nyha=="III"|subdata$rf_nyha=="IV"))/nrow(subdata)*100)
  column=append(column, nrow(subset(subdata, is.na(subdata$rf_nyha))))
  
  column=append(column, median(subdata$rf_lvef, na.rm=TRUE))
  column=append(column, IQR(subdata$rf_lvef, na.rm=TRUE))
  column=append(column, nrow(subset(subdata, is.na(subdata$rf_lvef))))
  
  column=append(column, nrow(subset(subdata, subdata$test_moca<26)))
  column=append(column, nrow(subset(subdata, subdata$test_moca<26))/nrow(subdata)*100)
  column=append(column, nrow(subset(subdata, is.na(subdata$test_moca))))
  
  column=append(column, nrow(subset(subdata, subdata$test_katz<6)))
  column=append(column, nrow(subset(subdata, subdata$test_katz<6))/nrow(subdata)*100)
  column=append(column, nrow(subset(subdata, is.na(subdata$test_katz))))
  
  column=append(column, median(subdata$rf_euroscore_log, na.rm=TRUE))
  column=append(column, IQR(subdata$rf_euroscore_log, na.rm=TRUE))
  column=append(column, nrow(subset(subdata, is.na(subdata$rf_euroscore_log))))
  
  column=append(column, median(subdata$rf_sts, na.rm=TRUE))
  column=append(column, IQR(subdata$rf_sts, na.rm=TRUE))
  column=append(column, nrow(subset(subdata, is.na(subdata$rf_sts))))
  
  
  if (era==0){
    result=as.data.frame(column)
  } else {
    result=cbind(result, column)
  }
  
}

rownames(result)=c("n",
                   "mean age","sd age","# age NA",
                   "# F","% F","# F NA",
                   "# DM","% DM","# DM NA",
                   "# neuro","% neuro","# neuro NA",
                   "# carotid","% carotid","# carotid NA",
                   "# renal failure","% renal failure","# renal failure NA",
                   "# pulm","% pulm","# pulm NA",
                   "# pci","% pci","# pci NA",
                   "# cabg","% cabg","# cabg NA",
                   "# nyha3-4","% nyha3-4","# nyha NA",
                   "median LVEF","IQR LVEF","# LVEF NA",
                   "# moca<26","% moca<26","# moca<26 NA",
                   "# katz<6","% katz<6","# katz<6 NA",
                   "median euro","IQR euro","# euro NA",
                   "median sts","IQR sts","# sts NA"
                   
                   )
colnames(result)=c("2010-2014","2015-2016","2017-2019")


#write.csv(result, "C:/Users/alexw/Google Drive/Desktop files/Dal Med/Med2/TAVI Project/table1.csv")

# Table 1 stats ----

table1stats=c()

era1=subset(data, data$tavi_era==0)
era2=subset(data, data$tavi_era==1)
era3=subset(data, data$tavi_era==2)


table1stats=append(table1stats, oneway.test(pt_age ~ tavi_era, data=data)$p.value)


M=as.table(cbind(c(nrow(subset(era1, era1$pt_sex=="M")),nrow(subset(era1, era1$pt_sex=="F"))),
                 c(nrow(subset(era2, era2$pt_sex=="M")),nrow(subset(era2, era2$pt_sex=="F"))),
                 c(nrow(subset(era3, era3$pt_sex=="M")),nrow(subset(era3, era3$pt_sex=="F")))
))
table1stats=append(table1stats,chisq.test(M)$p.value)


M=as.table(cbind(c(nrow(subset(era1, era1$rf_diabetes==0)),nrow(subset(era1, era1$rf_diabetes==1))),
                 c(nrow(subset(era2, era2$rf_diabetes==0)),nrow(subset(era2, era2$rf_diabetes==1))),
                 c(nrow(subset(era3, era3$rf_diabetes==0)),nrow(subset(era3, era3$rf_diabetes==1)))
))
table1stats=append(table1stats,chisq.test(M)$p.value)

M=as.table(cbind(c(nrow(subset(era1, era1$rf_cva==1|era1$rf_dementia==1|era1$rf_tia==1)),nrow(subset(era1, era1$rf_cva==0|era1$rf_dementia==0|era1$rf_tia==0))),
                 c(nrow(subset(era2, era2$rf_cva==1|era2$rf_dementia==1|era2$rf_tia==1)),nrow(subset(era2, era2$rf_cva==0|era2$rf_dementia==0|era2$rf_tia==0))),
                 c(nrow(subset(era3, era3$rf_cva==1|era3$rf_dementia==1|era3$rf_tia==1)),nrow(subset(era3, era3$rf_cva==0|era3$rf_dementia==0|era3$rf_tia==0)))
))
table1stats=append(table1stats,chisq.test(M)$p.value)


M=as.table(cbind(c(nrow(subset(era1, era1$rf_carotid==0)),nrow(subset(era1, era1$rf_carotid==1))),
                 c(nrow(subset(era2, era2$rf_carotid==0)),nrow(subset(era2, era2$rf_carotid==1))),
                 c(nrow(subset(era3, era3$rf_carotid==0)),nrow(subset(era3, era3$rf_carotid==1)))
))
table1stats=append(table1stats,chisq.test(M)$p.value)


M=as.table(cbind(c(nrow(subset(era1, era1$rf_renal_failure==0)),nrow(subset(era1, era1$rf_renal_failure==1))),
                 c(nrow(subset(era2, era2$rf_renal_failure==0)),nrow(subset(era2, era2$rf_renal_failure==1))),
                 c(nrow(subset(era3, era3$rf_renal_failure==0)),nrow(subset(era3, era3$rf_renal_failure==1)))
))
table1stats=append(table1stats,chisq.test(M)$p.value)


M=as.table(cbind(c(nrow(subset(era1, era1$rf_pulm==0)),nrow(subset(era1, era1$rf_pulm==1))),
                 c(nrow(subset(era2, era2$rf_pulm==0)),nrow(subset(era2, era2$rf_pulm==1))),
                 c(nrow(subset(era3, era3$rf_pulm==0)),nrow(subset(era3, era3$rf_pulm==1)))
))
table1stats=append(table1stats,chisq.test(M)$p.value)


M=as.table(cbind(c(nrow(subset(era1, era1$prev_pci==0)),nrow(subset(era1, era1$prev_pci==1))),
                 c(nrow(subset(era2, era2$prev_pci==0)),nrow(subset(era2, era2$prev_pci==1))),
                 c(nrow(subset(era3, era3$prev_pci==0)),nrow(subset(era3, era3$prev_pci==1)))
))
table1stats=append(table1stats,chisq.test(M)$p.value)


M=as.table(cbind(c(nrow(subset(era1, era1$prev_cabg==0)),nrow(subset(era1, era1$prev_cabg==1))),
                 c(nrow(subset(era2, era2$prev_cabg==0)),nrow(subset(era2, era2$prev_cabg==1))),
                 c(nrow(subset(era3, era3$prev_cabg==0)),nrow(subset(era3, era3$prev_cabg==1)))
))
table1stats=append(table1stats,chisq.test(M)$p.value)


M=as.table(cbind(c(nrow(subset(era1, era1$rf_nyha=="III"|era1$rf_nyha=="IV")),nrow(subset(era1, era1$rf_nyha=="I"|era1$rf_nyha=="II"))),
                 c(nrow(subset(era2, era2$rf_nyha=="III"|era2$rf_nyha=="IV")),nrow(subset(era2, era2$rf_nyha=="I"|era2$rf_nyha=="II"))),
                 c(nrow(subset(era3, era3$rf_nyha=="III"|era3$rf_nyha=="IV")),nrow(subset(era3, era3$rf_nyha=="I"|era3$rf_nyha=="II")))
))
table1stats=append(table1stats,chisq.test(M)$p.value)


table1stats=append(table1stats, oneway.test(rf_lvef ~ tavi_era, data=data)$p.value)


M=as.table(cbind(c(nrow(subset(era1, era1$test_moca<26)),nrow(subset(era1, era1$test_moca>=26))),
                 c(nrow(subset(era2, era2$test_moca<26)),nrow(subset(era2, era2$test_moca>=26))),
                 c(nrow(subset(era3, era3$test_moca<26)),nrow(subset(era3, era3$test_moca>=26)))
))
table1stats=append(table1stats,chisq.test(M)$p.value)


M=as.table(cbind(c(nrow(subset(era1, era1$test_katz<6)),nrow(subset(era1, era1$test_katz>=6))),
                 c(nrow(subset(era2, era2$test_katz<6)),nrow(subset(era2, era2$test_katz>=6))),
                 c(nrow(subset(era3, era3$test_katz<6)),nrow(subset(era3, era3$test_katz>=6)))
))
table1stats=append(table1stats,chisq.test(M)$p.value)


table1stats=append(table1stats, oneway.test(rf_euroscore_log ~ tavi_era, data=data)$p.value)

table1stats=append(table1stats, oneway.test(rf_sts ~ tavi_era, data=data)$p.value)



table1stats=as.data.frame(table1stats)
rownames(table1stats)=c("age","gender","DM","neuro","carotid","renal","pulm","pci","cabg","nyha","lvef","moca","katz","euro","sts")
colnames(table1stats)=c("p-value")

#write.csv(table1stats, "C:/Users/alexw/Google Drive/Desktop files/Dal Med/Med2/TAVI Project/table1stats.csv")


# DATABASE

tve <- tav[
  c(
    "id_tavi", "id_pprn", "dt_birth", "dt_tavi", "pt_age", "pt_sex", "pt_height", "pt_weight",
    "rf_diabetes", "rf_pvd", "rf_cva", "rf_cva_type", "rf_dementia", "rf_tia", "rf_carotid",
    "rf_renal_failure", "rf_pulm", "rf_nyha", "rf_lvef",
    "prev_pci", "prev_cabg", "prev_a_bio_vs", "prev_afib_correct", "prev_pmicd", "prev_septal_rup", "prev_ventric_repr", "prev_valve_plas_m", "prev_valve_repl_m", "prev_valve_repr_m"
    "prev_valve_plas_t", "prev_valve_repl_t", "prev_valve_repr_t", "prev_asd", 
    "test_moca", "test_moca_blind", "rf_sts", "rf_euroscore_log", "test_dasi", "test_vas",
    "test_katz"
  )
  ]

tve$dt_tavi_day <- as.numeric(tve$dt_tavi) - 14710


# PLOTS

plot(
  tve$dt_tavi_day, tve$rf_sts,
  main = NULL,
  ylab = "STS",
  xlab = "Days"
)
abline(lm(tve$rf_sts ~ tve$dt_tavi_day))	# regression line
abline(v = 265, col = "blue", lty = 2)	# 2011
abline(v = 631, col = "blue", lty = 2)	# 2012
abline(v = 996, col = "blue", lty = 2)	# 2013
abline(v = 1361, col = "blue", lty = 2)	# 2014
abline(v = 1726, col = "blue", lty = 2)	# 2015
abline(v = 2092, col = "blue", lty = 2)	# 2016
abline(v = 2457, col = "blue", lty = 2)	# 2017
abline(v = 2822, col = "blue", lty = 2)	# 2018
abline(v = 3187, col = "blue", lty = 2)	# 2019

plot(
  tve$dt_tavi_day, tve$rf_euroscore_log,
  main = NULL,
  ylab = "Log EuroSCORE",
  xlab = "Days"
)
abline(lm(tve$rf_euroscore_log ~ tve$dt_tavi_day))	# regression line
abline(v = 265, col = "blue", lty = 2)				# 2011
abline(v = 631, col = "blue", lty = 2)				# 2012
abline(v = 996, col = "blue", lty = 2)				# 2013
abline(v = 1361, col = "blue", lty = 2)				# 2014
abline(v = 1726, col = "blue", lty = 2)				# 2015
abline(v = 2092, col = "blue", lty = 2)				# 2016
abline(v = 2457, col = "blue", lty = 2)				# 2017
abline(v = 2822, col = "blue", lty = 2)				# 2018
abline(v = 3187, col = "blue", lty = 2)				# 2019