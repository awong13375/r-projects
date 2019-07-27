library(plyr)
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
data=read.csv("Data Collection Form - Final.csv")

table1=c()

table1=append(table1, nrow(data))

table1=append(table1, nrow(subset(data, data$Gender=="M")))
table1=append(table1, nrow(subset(data, data$Gender=="M"))/nrow(data)*100)
table1=append(table1, nrow(subset(data, data$Gender=="F")))
table1=append(table1, nrow(subset(data, data$Gender=="F"))/nrow(data)*100)

table1=append(table1, nrow(subset(data, data$Keros.Classification..Type.I.III.==1)))
table1=append(table1, nrow(subset(data, data$Keros.Classification..Type.I.III.==1))/nrow(data)*100)
table1=append(table1, nrow(subset(data, data$Keros.Classification..Type.I.III.==2)))
table1=append(table1, nrow(subset(data, data$Keros.Classification..Type.I.III.==2))/nrow(data)*100)
table1=append(table1, nrow(subset(data, data$Keros.Classification..Type.I.III.==3)))
table1=append(table1, nrow(subset(data, data$Keros.Classification..Type.I.III.==3))/nrow(data)*100)

table1=append(table1, nrow(subset(data, data$Asymmetry..height.difference.1mm.==1)))
table1=append(table1, nrow(subset(data, data$Asymmetry..height.difference.1mm.==1))/nrow(data)*100)
table1=append(table1, nrow(subset(data, data$Asymmetry..height.difference.1mm.==0)))
table1=append(table1, nrow(subset(data, data$Asymmetry..height.difference.1mm.==0))/nrow(data)*100)

table1=append(table1, nrow(subset(data, data$Bony.dehiscence==1)))
table1=append(table1, nrow(subset(data, data$Bony.dehiscence==1))/nrow(data)*100)
table1=append(table1, nrow(subset(data, data$Bony.dehiscence==0)))
table1=append(table1, nrow(subset(data, data$Bony.dehiscence==0))/nrow(data)*100)

table1=append(table1, nrow(subset(data, data$Intact.lamina==0)))
table1=append(table1, nrow(subset(data, data$Intact.lamina==0))/nrow(data)*100)
table1=append(table1, nrow(subset(data, data$Intact.lamina==1)))
table1=append(table1, nrow(subset(data, data$Intact.lamina==1))/nrow(data)*100)

table1=append(table1, nrow(subset(data, data$Orbital.dehiscence.into.ethmoid.sinus==1)))
table1=append(table1, nrow(subset(data, data$Orbital.dehiscence.into.ethmoid.sinus==1))/nrow(data)*100)
table1=append(table1, nrow(subset(data, data$Orbital.dehiscence.into.ethmoid.sinus==0)))
table1=append(table1, nrow(subset(data, data$Orbital.dehiscence.into.ethmoid.sinus==0))/nrow(data)*100)

table1=append(table1, nrow(subset(data, data$Ucinate.process.contacting.orbital.wall==1)))
table1=append(table1, nrow(subset(data, data$Ucinate.process.contacting.orbital.wall==1))/nrow(data)*100)
table1=append(table1, nrow(subset(data, data$Ucinate.process.contacting.orbital.wall==0)))
table1=append(table1, nrow(subset(data, data$Ucinate.process.contacting.orbital.wall==0))/nrow(data)*100)

table1=append(table1, nrow(subset(data, data$Presence==1)))
table1=append(table1, nrow(subset(data, data$Presence==1))/nrow(data)*100)
table1=append(table1, nrow(subset(data, data$Presence==0)))
table1=append(table1, nrow(subset(data, data$Presence==0))/nrow(data)*100)

table1=append(table1, nrow(subset(data, data$Carotid.dehiscence==1)))
table1=append(table1, nrow(subset(data, data$Carotid.dehiscence==1))/nrow(data)*100)
table1=append(table1, nrow(subset(data, data$Carotid.dehiscence==0)))
table1=append(table1, nrow(subset(data, data$Carotid.dehiscence==0))/nrow(data)*100)

table1=append(table1, nrow(subset(data, data$Optic.nerve.dehiscence==1)))
table1=append(table1, nrow(subset(data, data$Optic.nerve.dehiscence==1))/nrow(data)*100)
table1=append(table1, nrow(subset(data, data$Optic.nerve.dehiscence==0)))
table1=append(table1, nrow(subset(data, data$Optic.nerve.dehiscence==0))/nrow(data)*100)

table1=append(table1, nrow(subset(data, data$Sinus.septation.inserting.on.carotid.canal==1)))
table1=append(table1, nrow(subset(data, data$Sinus.septation.inserting.on.carotid.canal==1))/nrow(data)*100)
table1=append(table1, nrow(subset(data, data$Sinus.septation.inserting.on.carotid.canal==0)))
table1=append(table1, nrow(subset(data, data$Sinus.septation.inserting.on.carotid.canal==0))/nrow(data)*100)

table1=append(table1, nrow(subset(data, data$Supraortibal.pneumatization==1)))
table1=append(table1, nrow(subset(data, data$Supraortibal.pneumatization==1))/nrow(data)*100)
table1=append(table1, nrow(subset(data, data$Supraortibal.pneumatization==0)))
table1=append(table1, nrow(subset(data, data$Supraortibal.pneumatization==0))/nrow(data)*100)



table1=as.data.frame(table1)
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

write.csv(table1, "table1.csv")
