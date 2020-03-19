# TAVI EVOLUTION, PART 2
# UPDATED: 2020-03-08

# LIBRARIES ----

library(gmodels)
library(ggplot2)
library(ggpubr)

data=read.csv("C:/Users/alexw/Google Drive/Desktop files/Dal Med/Med2/TAVI Project/TAVI.csv")
additional_data=read.csv("C:/Users/alexw/Google Drive/Desktop files/Dal Med/Med2/TAVI Project/TAVI Additional Data.csv")
data=merge(data, additional_data, by="id_tavi")
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
  
  column=append(column, nrow(subset(subdata, subdata$rf_pvd==1)))
  column=append(column, nrow(subset(subdata, subdata$rf_pvd==1))/nrow(subdata)*100)
  column=append(column, nrow(subset(subdata, is.na(subdata$rf_pvd))))
  
  column=append(column, nrow(subset(subdata, subdata$rf_cva==1|subdata$rf_dementia==1|subdata$rf_tia==1)))
  column=append(column, nrow(subset(subdata, subdata$rf_cva==1|subdata$rf_dementia==1|subdata$rf_tia==1))/nrow(subdata)*100)
  column=append(column, nrow(subset(subdata, is.na(subdata$rf_cva)&is.na(subdata$rf_dementia)&is.na(subdata$rf_tia))))
  
  column=append(column, nrow(subset(subdata, subdata$rf_carotid==1)))
  column=append(column, nrow(subset(subdata, subdata$rf_carotid==1))/nrow(subdata)*100)
  column=append(column, nrow(subset(subdata, is.na(subdata$rf_carotid))))
  
  column=append(column, nrow(subset(subdata, subdata$rf_renal_failure==1)))
  column=append(column, nrow(subset(subdata, subdata$rf_renal_failure==1))/nrow(subdata)*100)
  column=append(column, nrow(subset(subdata, is.na(subdata$rf_renal_failure))))
  
  column=append(column, nrow(subset(subdata, subdata$rf_pulm==1)))
  column=append(column, nrow(subset(subdata, subdata$rf_pulm==1))/nrow(subdata)*100)
  column=append(column, nrow(subset(subdata, is.na(subdata$rf_pulm))))
  
  column=append(column, nrow(subset(subdata, subdata$prev_cabg==1|subdata$prev_pci==1)))
  column=append(column, nrow(subset(subdata, subdata$prev_cabg==1|subdata$prev_pci==1))/nrow(subdata)*100)
  column=append(column, nrow(subset(subdata, is.na(subdata$prev_cabg)&is.na(subdata$prev_pci))))
  
  column=append(column, nrow(subset(subdata, subdata$prev_pci==1)))
  column=append(column, nrow(subset(subdata, subdata$prev_pci==1))/nrow(subdata)*100)
  column=append(column, nrow(subset(subdata, is.na(subdata$prev_pci))))
  
  column=append(column, nrow(subset(subdata, subdata$prev_cabg==1|subdata$prev_a_bio_vs==1|subdata$prev_septal_rup==1|subdata$prev_ventric_repr==1|
                                      subdata$prev_valve_plas_m==1|subdata$prev_valve_repl_m==1|subdata$prev_valve_repr_m==1|subdata$prev_valve_plas_t==1|
                                      subdata$prev_valve_repl_t==1|subdata$prev_valve_repr_t==1|subdata$prev_asd==1)))
  column=append(column, nrow(subset(subdata, subdata$prev_cabg==1|subdata$prev_a_bio_vs==1|subdata$prev_septal_rup==1|subdata$prev_ventric_repr==1|
                                      subdata$prev_valve_plas_m==1|subdata$prev_valve_repl_m==1|subdata$prev_valve_repr_m==1|subdata$prev_valve_plas_t==1|
                                      subdata$prev_valve_repl_t==1|subdata$prev_valve_repr_t==1|subdata$prev_asd==1))/nrow(subdata)*100)
  column=append(column, nrow(subset(subdata, is.na(subdata$prev_cabg==1)&is.na(subdata$prev_a_bio_vs==1)&is.na(subdata$prev_septal_rup==1)&
                                      is.na(subdata$prev_ventric_repr==1)&is.na(subdata$prev_valve_plas_m==1)&is.na(subdata$prev_valve_repl_m==1)&
                                      is.na(subdata$prev_valve_repr_m==1)&is.na(subdata$prev_valve_plas_t==1)&is.na(subdata$prev_valve_repl_t==1)&
                                      is.na(subdata$prev_valve_repr_t==1)&is.na(subdata$prev_asd==1))))
  
  column=append(column, nrow(subset(subdata, subdata$prev_cabg==1)))
  column=append(column, nrow(subset(subdata, subdata$prev_cabg==1))/nrow(subdata)*100)
  column=append(column, nrow(subset(subdata, is.na(subdata$prev_cabg))))
  
  column=append(column, nrow(subset(subdata, subdata$rf_nyha=="III"|subdata$rf_nyha=="IV")))
  column=append(column, nrow(subset(subdata, subdata$rf_nyha=="III"|subdata$rf_nyha=="IV"))/nrow(subdata)*100)
  column=append(column, nrow(subset(subdata, is.na(subdata$rf_nyha))))
  
  column=append(column, median(subdata$rf_lvef, na.rm=TRUE))
  column=append(column, IQR(subdata$rf_lvef, na.rm=TRUE))
  column=append(column, nrow(subset(subdata, is.na(subdata$rf_lvef))))
  
  column=append(column, mean(subdata$echo_a_grad_mean, na.rm=TRUE))
  column=append(column, sd(subdata$echo_a_grad_mean, na.rm=TRUE))
  column=append(column, nrow(subset(subdata, is.na(subdata$echo_a_grad_mean))))
  
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
                   "# pvd","% pvd","# pvd NA",
                   "# neuro","% neuro","# neuro NA",
                   "# carotid","% carotid","# carotid NA",
                   "# renal failure","% renal failure","# renal failure NA",
                   "# pulm","% pulm","# pulm NA",
                   "# coronary","% coronary","# coronary NA",
                   "# pci","% pci","# pci NA",
                   "# cardiac sx","% cardiac sx","# cardiac sx NA",
                   "# cabg","% cabg","# cabg NA",
                   "# nyha3-4","% nyha3-4","# nyha NA",
                   "median LVEF","IQR LVEF","# LVEF NA",
                   "mean MVG","sd MVG","# MVG NA",
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


M=as.table(cbind(c(nrow(subset(era1, era1$rf_pvd==0)),nrow(subset(era1, era1$rf_pvd==1))),
                 c(nrow(subset(era2, era2$rf_pvd==0)),nrow(subset(era2, era2$rf_pvd==1))),
                 c(nrow(subset(era3, era3$rf_pvd==0)),nrow(subset(era3, era3$rf_pvd==1)))
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


M=as.table(cbind(c(nrow(subset(era1, era1$prev_cabg==0 & era1$prev_cabg==0)),nrow(subset(era1, era1$prev_cabg==1 | era1$prev_cabg==1))),
                 c(nrow(subset(era2, era2$prev_cabg==0 & era2$prev_cabg==0)),nrow(subset(era2, era2$prev_cabg==1 | era2$prev_cabg==1))),
                 c(nrow(subset(era3, era3$prev_cabg==0 & era3$prev_cabg==0)),nrow(subset(era3, era3$prev_cabg==1 | era3$prev_cabg==1)))
))
table1stats=append(table1stats,chisq.test(M)$p.value)


M=as.table(cbind(c(nrow(subset(era1, era1$prev_pci==0)),nrow(subset(era1, era1$prev_pci==1))),
                 c(nrow(subset(era2, era2$prev_pci==0)),nrow(subset(era2, era2$prev_pci==1))),
                 c(nrow(subset(era3, era3$prev_pci==0)),nrow(subset(era3, era3$prev_pci==1)))
))
table1stats=append(table1stats,chisq.test(M)$p.value)



M=as.table(cbind(c(nrow(subset(era1, era1$prev_cabg==0 & era1$prev_a_bio_vs==0 & era1$prev_septal_rup==0 & era1$prev_ventric_repr==0 & 
                               era1$prev_valve_plas_m==0 & era1$prev_valve_repl_m==0 & era1$prev_valve_repr_m==0 & era1$prev_valve_plas_t==0 & 
                               era1$prev_valve_repl_t==0 & era1$prev_valve_repr_t==0 & era1$prev_asd==0)),
                   nrow(subset(era1, era1$prev_cabg==1|era1$prev_a_bio_vs==1|era1$prev_septal_rup==1|era1$prev_ventric_repr==1|
                               era1$prev_valve_plas_m==1|era1$prev_valve_repl_m==1|era1$prev_valve_repr_m==1|era1$prev_valve_plas_t==1|
                               era1$prev_valve_repl_t==1|era1$prev_valve_repr_t==1|era1$prev_asd==1))),
                 c(nrow(subset(era2, era2$prev_cabg==0 & era2$prev_a_bio_vs==0 & era2$prev_septal_rup==0 & era2$prev_ventric_repr==0 & 
                               era2$prev_valve_plas_m==0 & era2$prev_valve_repl_m==0 & era2$prev_valve_repr_m==0 & era2$prev_valve_plas_t==0 & 
                               era2$prev_valve_repl_t==0 & era2$prev_valve_repr_t==0 & era2$prev_asd==0)),
                   nrow(subset(era2, era2$prev_cabg==1|era2$prev_a_bio_vs==1|era2$prev_septal_rup==1|era2$prev_ventric_repr==1|
                               era2$prev_valve_plas_m==1|era2$prev_valve_repl_m==1|era2$prev_valve_repr_m==1|era2$prev_valve_plas_t==1|
                               era2$prev_valve_repl_t==1|era2$prev_valve_repr_t==1|era2$prev_asd==1))),
                 c(nrow(subset(era3, era3$prev_cabg==0 & era3$prev_a_bio_vs==0 & era3$prev_septal_rup==0 & era3$prev_ventric_repr==0 & 
                                 era3$prev_valve_plas_m==0 & era3$prev_valve_repl_m==0 & era3$prev_valve_repr_m==0 & era3$prev_valve_plas_t==0 & 
                                 era3$prev_valve_repl_t==0 & era3$prev_valve_repr_t==0 & era3$prev_asd==0)),
                   nrow(subset(era3, era3$prev_cabg==1|era3$prev_a_bio_vs==1|era3$prev_septal_rup==1|era3$prev_ventric_repr==1|
                                 era3$prev_valve_plas_m==1|era3$prev_valve_repl_m==1|era3$prev_valve_repr_m==1|era3$prev_valve_plas_t==1|
                                 era3$prev_valve_repl_t==1|era3$prev_valve_repr_t==1|era3$prev_asd==1)))
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

table1stats=append(table1stats, oneway.test(echo_a_grad_mean ~ tavi_era, data=data)$p.value)

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
rownames(table1stats)=c("age","gender","DM","pvd","neuro","carotid","renal","pulm","coronary","pci","heart sx","cabg","nyha","lvef","mvg","moca","katz","euro","sts")
colnames(table1stats)=c("p-value")

#write.csv(table1stats, "C:/Users/alexw/Google Drive/Desktop files/Dal Med/Med2/TAVI Project/table1stats.csv")

# Scater Plots ----
## Euroscore ----

eurolm <- lm(rf_euroscore_log ~ dt_tavi, data=data)
summary(eurolm)

ggplot(data, aes(dt_tavi, rf_euroscore_log)) +
  geom_point() +
  geom_smooth(method = "lm", se=FALSE, color="black")+
  xlab("Year") +
  ylab("Euroscore") + 
  scale_x_date(date_breaks = "1 year", date_labels ="%Y")+
  geom_vline(xintercept=as.Date("2015-01-01"), linetype="dotted")+
  geom_vline(xintercept=as.Date("2017-01-01"), linetype="dotted")+
  theme_classic()

## STS score ----

stslm <- lm(rf_sts ~ dt_tavi, data=data)
summary(stslm)

ggplot(data, aes(dt_tavi, rf_sts)) +
  geom_point() +
  geom_smooth(method = "lm", se=FALSE, color="black")+
  xlab("Year") +
  ylab("STS score") + 
  scale_x_date(date_breaks = "1 year", date_labels ="%Y")+
  geom_vline(xintercept=as.Date("2015-01-01"), linetype="dotted")+
  geom_vline(xintercept=as.Date("2017-01-01"), linetype="dotted")+
  theme_classic()

# Box Plots ----
data$tavi_era[data$tavi_era==0]="2010-2014"
data$tavi_era[data$tavi_era==1]="2015-2016"
data$tavi_era[data$tavi_era==2]="2017-2019"

data$tavi_era=as.factor(data$tavi_era)
## Euroscore ----

ggplot(data, aes(tavi_era, rf_euroscore_log)) +
  geom_boxplot() +
  xlab("Year") +
  ylab("Euroscore") + 
  theme_classic()

## STS Score ----

ggplot(data, aes(tavi_era, rf_sts)) +
  geom_boxplot() +
  xlab("Year") +
  ylab("STS Score") + 
  theme_classic()

