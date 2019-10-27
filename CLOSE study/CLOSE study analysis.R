library(plyr)
library(irr)
library(psych)
library(rel)
setwd("C:/Users/alexw/Google Drive/Desktop files/Dal Med/RIM")


# Match CJC and JK data ---------------------------------------------------


data=read.csv("Data Collection Form - compiled.csv")
data$keros=c(0)
data$asym=c(0)
data$bonydehisc=c(0)
data$lamina=c(0)
data$orbitaldehisc=c(0)
data$ucinate=c(0)
data$onodi=c(0)
data$carotiddehisc=c(0)
data$ondehisc=c(0)
data$sinussep=c(0)
data$supraorbpneum=c(0)

data$keros[data$Keros.Classification.CJC!=data$Keros.Classification.JK]=1
data$asym[data$Asymmetry.CJC!=data$Asymmetry.JK]=1
data$bonydehisc[data$Bony.dehiscence.CJC!=data$Bony.dehiscence.JK]=1
data$lamina[data$Intact.lamina.CJC!=data$Intact.lamina.JK]=1
data$orbitaldehisc[data$Orbital.dehiscence.into.ethmoid.sinus.CJC!=data$Orbital.dehiscence.into.ethmoid.sinus.JK]=1
data$ucinate[data$Ucinate.process.contacting.orbital.wall.CJC!=data$Ucinate.process.contacting.orbital.wall.JK]=1
data$onodi[data$Presence.CJC!=data$Presence.JK]=1
data$carotiddehisc[data$Carotid.dehiscence.CJC!=data$Carotid.dehiscence.JK]=1
data$ondehisc[data$Optic.nerve.dehiscence.CJC!=data$Optic.nerve.dehiscence.JK]=1
data$sinussep[data$Sinus.septation.inserting.on.carotid.canal.CJC!=data$Sinus.septation.inserting.on.carotid.canal.JK]=1
data$supraorbpneum[data$Supraortibal.pneumatization.CJC!=data$Supraortibal.pneumatization.JK]=1

dataconflict=subset(data, data$keros==1|
                          data$asym==1|
                          data$bonydehisc==1|
                          data$lamina==1|
                          data$orbitaldehisc==1|
                          data$ucinate==1|
                          data$onodi==1|
                          data$carotiddehisc==1|
                          data$ondehisc==1|
                          data$sinussep==1|
                          data$supraorbpneum==1
                          )


print("# scans with conflicting data")
cat(nrow(dataconflict),"/",nrow(data),",",nrow(dataconflict)/nrow(data)*100,"%")

keep=c("keros","asym","bonydehisc","lamina","orbitaldehisc","ucinate","onodi","carotiddehisc","ondehisc","sinussep","supraorbpneum")
dataconflict=dataconflict[keep]
total_conflicts=nrow(subset(dataconflict, dataconflict$keros==1))+
  nrow(subset(dataconflict, dataconflict$asym==1))+
  nrow(subset(dataconflict, dataconflict$bonydehisc==1))+
  nrow(subset(dataconflict, dataconflict$lamina==1))+
  nrow(subset(dataconflict, dataconflict$orbitaldehisc==1))+
  nrow(subset(dataconflict, dataconflict$ucinate==1))+
  nrow(subset(dataconflict, dataconflict$onodi==1))+
  nrow(subset(dataconflict, dataconflict$carotiddehisc==1))+
  nrow(subset(dataconflict, dataconflict$ondehisc==1))+
  nrow(subset(dataconflict, dataconflict$sinussep==1))+
  nrow(subset(dataconflict, dataconflict$supraorbpneum==1))
  
print("# values with conflicts")
cat(total_conflicts, "/", nrow(dataconflict)*ncol(dataconflict),",",total_conflicts/(nrow(dataconflict)*ncol(dataconflict))*100,"%")






# Calculate CLOSE rates --------------------------------------------------
data1=read.csv("Data Collection Form - Final.csv")
data2=read.csv("Data Collection Form_Wong.csv")
data1$version=c("Gold standard")
data2$version=c("MT")

data1$Intact.lamina=factor(data1$Intact.lamina, levels=c("0","1"))
data1$Orbital.dehiscence.into.ethmoid.sinus=factor(data1$Orbital.dehiscence.into.ethmoid.sinus, levels=c("0","1"))
data2$Intact.lamina=factor(data2$Intact.lamina, levels=c("0","1"))
data2$Orbital.dehiscence.into.ethmoid.sinus=factor(data2$Orbital.dehiscence.into.ethmoid.sinus, levels=c("0","1"))

data=rbind(data2, data1)

version=c("Gold standard","MT")

for (ver in version){
  column=c()
  
  dataver=subset(data, data$version==ver)
  
  column=append(column, nrow(dataver))
  
  column=append(column, nrow(subset(dataver, dataver$Gender=="M")))
  column=append(column, nrow(subset(dataver, dataver$Gender=="M"))/nrow(dataver)*100)
  column=append(column, nrow(subset(dataver, dataver$Gender=="F")))
  column=append(column, nrow(subset(dataver, dataver$Gender=="F"))/nrow(dataver)*100)
  
  column=append(column, nrow(subset(dataver, dataver$Keros.Classification..Type.I.III.==1)))
  column=append(column, nrow(subset(dataver, dataver$Keros.Classification..Type.I.III.==1))/nrow(dataver)*100)
  column=append(column, nrow(subset(dataver, dataver$Keros.Classification..Type.I.III.==2)))
  column=append(column, nrow(subset(dataver, dataver$Keros.Classification..Type.I.III.==2))/nrow(dataver)*100)
  column=append(column, nrow(subset(dataver, dataver$Keros.Classification..Type.I.III.==3)))
  column=append(column, nrow(subset(dataver, dataver$Keros.Classification..Type.I.III.==3))/nrow(dataver)*100)
  
  column=append(column, nrow(subset(dataver, dataver$Asymmetry..height.difference.1mm.==1)))
  column=append(column, nrow(subset(dataver, dataver$Asymmetry..height.difference.1mm.==1))/nrow(dataver)*100)
  column=append(column, nrow(subset(dataver, dataver$Asymmetry..height.difference.1mm.==0)))
  column=append(column, nrow(subset(dataver, dataver$Asymmetry..height.difference.1mm.==0))/nrow(dataver)*100)
  
  column=append(column, nrow(subset(dataver, dataver$Bony.dehiscence==1)))
  column=append(column, nrow(subset(dataver, dataver$Bony.dehiscence==1))/nrow(dataver)*100)
  column=append(column, nrow(subset(dataver, dataver$Bony.dehiscence==0)))
  column=append(column, nrow(subset(dataver, dataver$Bony.dehiscence==0))/nrow(dataver)*100)
  
  column=append(column, nrow(subset(dataver, dataver$Intact.lamina==0)))
  column=append(column, nrow(subset(dataver, dataver$Intact.lamina==0))/nrow(dataver)*100)
  column=append(column, nrow(subset(dataver, dataver$Intact.lamina==1)))
  column=append(column, nrow(subset(dataver, dataver$Intact.lamina==1))/nrow(dataver)*100)
  
  column=append(column, nrow(subset(dataver, dataver$Orbital.dehiscence.into.ethmoid.sinus==1)))
  column=append(column, nrow(subset(dataver, dataver$Orbital.dehiscence.into.ethmoid.sinus==1))/nrow(dataver)*100)
  column=append(column, nrow(subset(dataver, dataver$Orbital.dehiscence.into.ethmoid.sinus==0)))
  column=append(column, nrow(subset(dataver, dataver$Orbital.dehiscence.into.ethmoid.sinus==0))/nrow(dataver)*100)
  
  column=append(column, nrow(subset(dataver, dataver$Ucinate.process.contacting.orbital.wall==1)))
  column=append(column, nrow(subset(dataver, dataver$Ucinate.process.contacting.orbital.wall==1))/nrow(dataver)*100)
  column=append(column, nrow(subset(dataver, dataver$Ucinate.process.contacting.orbital.wall==0)))
  column=append(column, nrow(subset(dataver, dataver$Ucinate.process.contacting.orbital.wall==0))/nrow(dataver)*100)
  
  column=append(column, nrow(subset(dataver, dataver$Presence==1)))
  column=append(column, nrow(subset(dataver, dataver$Presence==1))/nrow(dataver)*100)
  column=append(column, nrow(subset(dataver, dataver$Presence==0)))
  column=append(column, nrow(subset(dataver, dataver$Presence==0))/nrow(dataver)*100)
  
  column=append(column, nrow(subset(dataver, dataver$Carotid.dehiscence==1)))
  column=append(column, nrow(subset(dataver, dataver$Carotid.dehiscence==1))/nrow(dataver)*100)
  column=append(column, nrow(subset(dataver, dataver$Carotid.dehiscence==0)))
  column=append(column, nrow(subset(dataver, dataver$Carotid.dehiscence==0))/nrow(dataver)*100)
  
  column=append(column, nrow(subset(dataver, dataver$Optic.nerve.dehiscence==1)))
  column=append(column, nrow(subset(dataver, dataver$Optic.nerve.dehiscence==1))/nrow(dataver)*100)
  column=append(column, nrow(subset(dataver, dataver$Optic.nerve.dehiscence==0)))
  column=append(column, nrow(subset(dataver, dataver$Optic.nerve.dehiscence==0))/nrow(dataver)*100)
  
  column=append(column, nrow(subset(dataver, dataver$Sinus.septation.inserting.on.carotid.canal==1)))
  column=append(column, nrow(subset(dataver, dataver$Sinus.septation.inserting.on.carotid.canal==1))/nrow(dataver)*100)
  column=append(column, nrow(subset(dataver, dataver$Sinus.septation.inserting.on.carotid.canal==0)))
  column=append(column, nrow(subset(dataver, dataver$Sinus.septation.inserting.on.carotid.canal==0))/nrow(dataver)*100)
  
  column=append(column, nrow(subset(dataver, dataver$Supraortibal.pneumatization==1)))
  column=append(column, nrow(subset(dataver, dataver$Supraortibal.pneumatization==1))/nrow(dataver)*100)
  column=append(column, nrow(subset(dataver, dataver$Supraortibal.pneumatization==0)))
  column=append(column, nrow(subset(dataver, dataver$Supraortibal.pneumatization==0))/nrow(dataver)*100)
  
  if (ver=="Gold standard"){
    table1=as.data.frame(column)
  } else{
    table1=cbind(table1, column)
  }
}


colnames(table1)=c("Gold standard","MT")
rownames(table1)=c("n","M","M%","F","F%",
                   "Keros1","Keros1%","Keros2","Keros2%","Keros3","Keros3%",
                   "Y Asym","Y Asym%","N Asym","N Asym%",
                   "Y Bony dehisc","Y Bony dehisc%","N Bony dehisc","N Bony dehisc%",
                   "Y intact lamina","Y Intact lamina%","N intact lamina","N intact lamina%",
                   "Y orbit dehisc","Y orbit dehisc%","N orbit dehisc","N orbit dehisc%",
                   "Y ucinate contact","Y ucinate contact%","N ucinate contact","N ucinate contact%",
                   "Y onodi","Y onodi%","N onodi","N onodi%",
                   "Y carotid dehisc","Y carotid dehisc%","N carotid dehisc","N carotid dehisc%",
                   "Y ON dehisc","Y ON dehisc%","N ON dehisc","N ON dehisc%",
                   "Y sinus sept","Y sinus sept%","N sinus sept","N sinus sept%",
                   "Y supraorb pneum","Y supraorb pneum%","N supraorb pneum","N supraorb pneum%"
                   )

#write.csv(table1, "table1.csv")

# Kappa statistics --------------------------------------------------------

pctagree=c()
kappacoef=c()
kappaCI=c()

pctagree=append(pctagree, agree(cbind(as.factor(data1$Keros.Classification..Type.I.III.), 
                                as.factor(data2$Keros.Classification..Type.I.III.)))$value)
pctagree=append(pctagree, agree(cbind(data1$Asymmetry..height.difference.1mm., 
                                         data2$Asymmetry..height.difference.1mm.))$value)
pctagree=append(pctagree, agree(cbind(data1$Bony.dehiscence, 
                                         data2$Bony.dehiscence))$value)
pctagree=append(pctagree, agree(cbind(data1$Intact.lamina, 
                                         data2$Intact.lamina))$value)
pctagree=append(pctagree, agree(cbind(data1$Orbital.dehiscence.into.ethmoid.sinus, 
                                         data2$Orbital.dehiscence.into.ethmoid.sinus))$value)
pctagree=append(pctagree, agree(cbind(data1$Ucinate.process.contacting.orbital.wall, 
                                         data2$Ucinate.process.contacting.orbital.wall))$value)
pctagree=append(pctagree, agree(cbind(data1$Presence, 
                                         data2$Presence))$value)
pctagree=append(pctagree, agree(cbind(data1$Carotid.dehiscence, 
                                         data2$Carotid.dehiscence))$value)
pctagree=append(pctagree, agree(cbind(data1$Optic.nerve.dehiscence, 
                                         data2$Optic.nerve.dehiscence))$value)
pctagree=append(pctagree, agree(cbind(data1$Sinus.septation.inserting.on.carotid.canal, 
                                         data2$Sinus.septation.inserting.on.carotid.canal))$value)
pctagree=append(pctagree, agree(cbind(data1$Supraortibal.pneumatization, 
                                         data2$Supraortibal.pneumatization))$value)


kappacoef=append(kappacoef, kappa2(cbind(as.factor(data1$Keros.Classification..Type.I.III.), 
                                             as.factor(data2$Keros.Classification..Type.I.III.)), "equal")$p.value)
kappacoef=append(kappacoef, kappa2(cbind(data1$Asymmetry..height.difference.1mm., 
                                             data2$Asymmetry..height.difference.1mm.), "unweighted")$p.value)
kappacoef=append(kappacoef, kappa2(cbind(data1$Bony.dehiscence, 
                                             data2$Bony.dehiscence), "unweighted")$p.value)
kappacoef=append(kappacoef, kappa2(cbind(data1$Intact.lamina, 
                                             data2$Intact.lamina), "unweighted")$p.value)
kappacoef=append(kappacoef, kappa2(cbind(data1$Orbital.dehiscence.into.ethmoid.sinus, 
                                             data2$Orbital.dehiscence.into.ethmoid.sinus), "unweighted")$p.value)
kappacoef=append(kappacoef, kappa2(cbind(data1$Ucinate.process.contacting.orbital.wall, 
                                             data2$Ucinate.process.contacting.orbital.wall), "unweighted")$p.value)
kappacoef=append(kappacoef, kappa2(cbind(data1$Presence, 
                                             data2$Presence), "unweighted")$p.value)
kappacoef=append(kappacoef, kappa2(cbind(data1$Carotid.dehiscence, 
                                             data2$Carotid.dehiscence), "unweighted")$p.value)
kappacoef=append(kappacoef, kappa2(cbind(data1$Optic.nerve.dehiscence, 
                                             data2$Optic.nerve.dehiscence), "unweighted")$p.value)
kappacoef=append(kappacoef, kappa2(cbind(data1$Sinus.septation.inserting.on.carotid.canal, 
                                             data2$Sinus.septation.inserting.on.carotid.canal), "unweighted")$p.value)
kappacoef=append(kappacoef, kappa2(cbind(data1$Supraortibal.pneumatization, 
                                             data2$Supraortibal.pneumatization), "unweighted")$p.value)
kappacoef=append(kappacoef, kappa2(cbind(as.factor(data1$Keros.Classification..Type.I.III.), 
                                         as.factor(data2$Keros.Classification..Type.I.III.)), "equal")$p.value)
kappacoef=append(kappacoef, kappa2(cbind(data1$Asymmetry..height.difference.1mm., 
                                         data2$Asymmetry..height.difference.1mm.), "unweighted")$p.value)
kappacoef=append(kappacoef, kappa2(cbind(data1$Bony.dehiscence, 
                                         data2$Bony.dehiscence), "unweighted")$p.value)
kappacoef=append(kappacoef, kappa2(cbind(data1$Intact.lamina, 
                                         data2$Intact.lamina), "unweighted")$p.value)
kappacoef=append(kappacoef, kappa2(cbind(data1$Orbital.dehiscence.into.ethmoid.sinus, 
                                         data2$Orbital.dehiscence.into.ethmoid.sinus), "unweighted")$p.value)
kappacoef=append(kappacoef, kappa2(cbind(data1$Ucinate.process.contacting.orbital.wall, 
                                         data2$Ucinate.process.contacting.orbital.wall), "unweighted")$p.value)
kappacoef=append(kappacoef, kappa2(cbind(data1$Presence, 
                                         data2$Presence), "unweighted")$p.value)
kappacoef=append(kappacoef, kappa2(cbind(data1$Carotid.dehiscence, 
                                         data2$Carotid.dehiscence), "unweighted")$p.value)
kappacoef=append(kappacoef, kappa2(cbind(data1$Optic.nerve.dehiscence, 
                                         data2$Optic.nerve.dehiscence), "unweighted")$p.value)
kappacoef=append(kappacoef, kappa2(cbind(data1$Sinus.septation.inserting.on.carotid.canal, 
                                         data2$Sinus.septation.inserting.on.carotid.canal), "unweighted")$p.value)
kappacoef=append(kappacoef, kappa2(cbind(data1$Supraortibal.pneumatization, 
                                         data2$Supraortibal.pneumatization), "unweighted")$p.value)


kappaCI=append(kappaCI, cohen.kappa(cbind(as.factor(data1$Keros.Classification..Type.I.III.), 
                                         as.factor(data2$Keros.Classification..Type.I.III.)), "equal")$p.value)
kappaCI=append(kappaCI, kappa2(cbind(data1$Asymmetry..height.difference.1mm., 
                                         data2$Asymmetry..height.difference.1mm.), "unweighted")$p.value)
kappaCI=append(kappaCI, kappa2(cbind(data1$Bony.dehiscence, 
                                         data2$Bony.dehiscence), "unweighted")$p.value)
kappaCI=append(kappaCI, kappa2(cbind(data1$Intact.lamina, 
                                         data2$Intact.lamina), "unweighted")$p.value)
kappaCI=append(kappaCI, kappa2(cbind(data1$Orbital.dehiscence.into.ethmoid.sinus, 
                                         data2$Orbital.dehiscence.into.ethmoid.sinus), "unweighted")$p.value)
kappaCI=append(kappaCI, kappa2(cbind(data1$Ucinate.process.contacting.orbital.wall, 
                                         data2$Ucinate.process.contacting.orbital.wall), "unweighted")$p.value)
kappaCI=append(kappaCI, kappa2(cbind(data1$Presence, 
                                         data2$Presence), "unweighted")$p.value)
kappaCI=append(kappaCI, kappa2(cbind(data1$Carotid.dehiscence, 
                                         data2$Carotid.dehiscence), "unweighted")$p.value)
kappaCI=append(kappaCI, kappa2(cbind(data1$Optic.nerve.dehiscence, 
                                         data2$Optic.nerve.dehiscence), "unweighted")$p.value)
kappaCI=append(kappaCI, kappa2(cbind(data1$Sinus.septation.inserting.on.carotid.canal, 
                                         data2$Sinus.septation.inserting.on.carotid.canal), "unweighted")$p.value)
kappaCI=append(kappaCI, kappa2(cbind(data1$Supraortibal.pneumatization, 
                                         data2$Supraortibal.pneumatization), "unweighted")$p.value)
kappaCI=append(kappaCI, kappa2(cbind(as.factor(data1$Keros.Classification..Type.I.III.), 
                                         as.factor(data2$Keros.Classification..Type.I.III.)), "equal")$p.value)
kappaCI=append(kappaCI, kappa2(cbind(data1$Asymmetry..height.difference.1mm., 
                                         data2$Asymmetry..height.difference.1mm.), "unweighted")$p.value)
kappaCI=append(kappaCI, kappa2(cbind(data1$Bony.dehiscence, 
                                         data2$Bony.dehiscence), "unweighted")$p.value)
kappaCI=append(kappaCI, kappa2(cbind(data1$Intact.lamina, 
                                         data2$Intact.lamina), "unweighted")$p.value)
kappaCI=append(kappaCI, kappa2(cbind(data1$Orbital.dehiscence.into.ethmoid.sinus, 
                                         data2$Orbital.dehiscence.into.ethmoid.sinus), "unweighted")$p.value)
kappaCI=append(kappaCI, kappa2(cbind(data1$Ucinate.process.contacting.orbital.wall, 
                                         data2$Ucinate.process.contacting.orbital.wall), "unweighted")$p.value)
kappaCI=append(kappaCI, kappa2(cbind(data1$Presence, 
                                         data2$Presence), "unweighted")$p.value)
kappaCI=append(kappaCI, kappa2(cbind(data1$Carotid.dehiscence, 
                                         data2$Carotid.dehiscence), "unweighted")$p.value)
kappaCI=append(kappaCI, kappa2(cbind(data1$Optic.nerve.dehiscence, 
                                         data2$Optic.nerve.dehiscence), "unweighted")$p.value)
kappaCI=append(kappaCI, kappa2(cbind(data1$Sinus.septation.inserting.on.carotid.canal, 
                                         data2$Sinus.septation.inserting.on.carotid.canal), "unweighted")$p.value)
kappaCI=append(kappaCI, kappa2(cbind(data1$Supraortibal.pneumatization, 
                                         data2$Supraortibal.pneumatization), "unweighted")$p.value)


kappacoef=as.data.frame(kappacoef)
rownames(table1stats)=c("keros","asym","bony dehisc","intact lamina","orbital dehisc","ucinate process",
                        "onodi","carotid dehisc","ON dehisc","sinus sept","supraorb pneum")
