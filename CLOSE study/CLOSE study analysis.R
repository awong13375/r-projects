library(plyr)
setwd("C:/Users/alexw/OneDrive/Dal Med/RIM")

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



