library(httr)
library(jsonlite)
repos <- GET(url = paste0("https://data.smartdublin.ie/dataset/19a0948f-9505-49db-b399-226a8bcc1493/resource/e3083443-bd76-4e9a-acb6-7dc971c0e1dc/download/2013-2015-dfb-ambulance.csv"))
library(httr)
library(dplyr)
library(magrittr)
repo_content <- content(repos)
names(dublinlecpaffluenceand_deprivationammended_1_)[2] <- "Population"
dublinlecpaffluenceand_deprivationammended_1_ = dublinlecpaffluenceand_deprivationammended_1_[-1,]
repo_content[repo_content == "Donnybrook"] <- "Dublin City"
i=1
Sum=0
DublinPop=dublinlecpaffluenceand_deprivationammended_1_
for (var in DublinPop$`Administrative Region`){
  if(var=="Dublin City"){
    Sum=(as.numeric(DublinPop$Population[i])+as.numeric(Sum))
    i=i+1
  }
}

SumSouth=0
for (var in DublinPop$`Administrative Region`){
  if(var=="South Dublin"){
    SumSouth=(as.numeric(DublinPop$Population[i])+as.numeric(SumSouth))
    i=i+1
  }
}

SumFingal=0
for (var in DublinPop$`Administrative Region`){
  if(var=="Fingal"){
    SumFingal=(as.numeric(DublinPop$Population[i])+as.numeric(SumFingal))
    i=i+1
  }
}
SumDun=0
for (var in DublinPop$`Administrative Region`){
  if(var=="Dún Laoghaire-Rathdown"){
    SumDun=(as.numeric(DublinPop$Population[i])+as.numeric(SumDun))
    i=i+1
  }
}
install.packages("data.table")
library("data.table")
TotalPopOfRegions = data.table(
  AdministrativeRegion = c("Dublin City","South Dublin","Fingal","Dún Laoghaire-Rathdown"),
  Population = c(Sum,SumSouth,SumFingal,SumDun)
)
Backup=IncidentsPerPop
AmbulanceCalls[AmbulanceCalls == "Dun Laoghaire"] <- "Dún Laoghaire-Rathdown"
Backup2=AmbulanceCalls
DublinCityIncidents=0
for (var in AmbulanceCalls$`Station Area`){
  if(var=="Dublin City"){

    DublinCityIncidents=DublinCityIncidents+1
  }
}
SouthDublinIncidents=0
for (var in AmbulanceCalls$`Station Area`){
  if(var=="South Dublin"){
    
    SouthDublinIncidents=SouthDublinIncidents+1
  }
}

FingalIncidents=0
for (var in AmbulanceCalls$`Station Area`){
  if(var=="Fingal"){
    
    FingalIncidents=FingalIncidents+1
  }
}

DúnLaoghaireIncidents=0
for (var in AmbulanceCalls$`Station Area`){
  if(var=="Dún Laoghaire-Rathdown"){
    
    DúnLaoghaireIncidents=DúnLaoghaireIncidents+1
  }
}

IncidentsPerPop = data.table(
  AdministrativeRegion = c("Dublin City","South Dublin","Fingal","Dún Laoghaire-Rathdown"),
  Population = c(Sum,SumSouth,SumFingal,SumDun),
  AmbulanceCalls=c(DublinCityIncidents,SouthDublinIncidents,FingalIncidents,DúnLaoghaireIncidents),
  CallsPer1000=c((DublinCityIncidents/Sum)*1000,(SouthDublinIncidents/SumSouth)*1000,(FingalIncidents/SumFingal)*1000,(DúnLaoghaireIncidents/SumDun)*1000)
)

library(ggplot2)
ggplot(IncidentsPerPop, aes(x=IncidentsPerPop$AdministrativeRegion, y=IncidentsPerPop$CallsPer1000)) + 
  geom_bar(stat = "identity")

DunLifeThreatening =0
DunSerious=0
DunMinor=0

for (var in Fill$`Clinical Status`){
  if(var=="Delta"||var=="Echo"||var=="E"||var=="D"){
    DunLifeThreatening=DunLifeThreatening+1
  }
  if(var=="Bravo"||var=="Charlie"||var=="B"||var=="C"){
    DunSerious=DunSerious+1
  }
  if(var=="Alpha"||var=="Omega"||var=="A"||var=="O"){
    DunMinor=DunMinor+1
  }
  
}
dplyr::filter(AmbulanceCalls, !grepl("Dublin City|Fingal|South Dublin",AmbulanceCalls$`Station Area`))
dplyr::filter(AmbulanceCalls, !grepl("Fingal",AmbulanceCalls$`Station Area`))
dplyr::filter(AmbulanceCalls, !grepl("South Dublin",AmbulanceCalls$`Station Area`))
Fill=AmbulanceCalls[!grepl("Dublin City|Fingal|Dún Laoghaire-Rathdown", AmbulanceCalls$`Station Area`),]

FinLifeThreatening =0
FinSerious=0
FinMinor=0

for (var in Fill$`Clinical Status`){
  if(var=="Delta"||var=="Echo"||var=="E"||var=="D"){
    FinLifeThreatening=FinLifeThreatening+1
  }
  if(var=="Bravo"||var=="Charlie"||var=="B"||var=="C"){
    FinSerious=FinSerious+1
  }
  if(var=="Alpha"||var=="Omega"||var=="A"||var=="O"){
    FinMinor=FinMinor+1
  }
  
}

SouthLifeThreatening =0
SouthSerious=0
SouthMinor=0

for (var in Fill$`Clinical Status`){
  if(var=="Delta"||var=="Echo"||var=="E"||var=="D"){
    SouthLifeThreatening=SouthLifeThreatening+1
  }
  if(var=="Bravo"||var=="Charlie"||var=="B"||var=="C"){
    SouthSerious=SouthSerious+1
  }
  if(var=="Alpha"||var=="Omega"||var=="A"||var=="O"){
    SouthMinor=SouthMinor+1
  }
  
}

DubLifeThreatening =0
DubSerious=0
DubMinor=0

for (var in Fill$`Clinical Status`){
  if(var=="Delta"||var=="Echo"||var=="E"||var=="D"){
    DubLifeThreatening=DubLifeThreatening+1
  }
  if(var=="Bravo"||var=="Charlie"||var=="B"||var=="C"){
    DubSerious=DubSerious+1
  }
  if(var=="Alpha"||var=="Omega"||var=="A"||var=="O"){
    DubMinor=DubMinor+1
  }
  
}
IncidentsPerPop = data.table(
  AdministrativeRegion = c("Dublin City","South Dublin","Fingal","Dún Laoghaire-Rathdown"),
  Population = c(Sum,SumSouth,SumFingal,SumDun),
  LifeThreateningCalls=c(DubLifeThreatening,SouthLifeThreatening,FinLifeThreatening,DunLifeThreatening),
  SeriousCalls=c(DubSerious,SouthSerious,FinSerious,DunSerious),
  MinorCalls=c(DubMinor,SouthMinor,FinMinor,DunMinor),
  TotalAmbulanceCalls=c(DublinCityIncidents,SouthDublinIncidents,FingalIncidents,DúnLaoghaireIncidents),
  CallsPer1000=c((DublinCityIncidents/Sum)*1000,(SouthDublinIncidents/SumSouth)*1000,(FingalIncidents/SumFingal)*1000,(DúnLaoghaireIncidents/SumDun)*1000)
)

Serious=c((DubLifeThreatening/Sum)*1000,(DubSerious/Sum)*1000,(DubMinor/Sum)*1000)

LifeThreatening=c((DubLifeThreatening/DublinCityIncidents)*1000,(SouthLifeThreatening/SouthDublinIncidents)*1000,(FinLifeThreatening/FingalIncidents)*1000,(DunLifeThreatening/DúnLaoghaireIncidents)*1000)
Serious=c((DubSerious/DublinCityIncidents)*1000,(SouthSerious/SouthDublinIncidents)*1000,(FinSerious/FingalIncidents)*1000,(DunSerious/DúnLaoghaireIncidents)*1000)
Minor=c((DubMinor/DublinCityIncidents)*1000,(SouthMinor/SouthDublinIncidents)*1000,(FinMinor/FingalIncidents)*1000,(DunMinor/DúnLaoghaireIncidents)*1000)
ggplot(IncidentsPerPop, aes(x=IncidentsPerPop$AdministrativeRegion, y=Minor)) + 
  geom_bar(stat = "identity")