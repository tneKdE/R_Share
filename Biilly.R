library(data.table)
library(readr)
library(readxl)
library(lubridate)
library(dplyr)
library(reshape2)

rm(list = ls())
MyDir<-choose.dir("Y:/Shared/TWAI11/TEAMDATA/_Central/NCC/Operational Capability/23 Risk Models and Analysis/2018/TobySpaces", caption = "Location of the Scenario Data")
setwd(MyDir)

dir.create(paste(MyDir, "/NAP_Maint", sep = ""))

TheScens<-c("CECCDEM","CRCCDEM", "SPCCDEM", "TDCCDEM")#c("CECCDEM","CRCCDEM", "SPCCDEM", "TDCCDEM")#, "SPCCDEM", "TDCCDEM","TDCCDEM")#,"CRCCDEM", "SPCCDEM", "TDCCDEM")
for (MyScen in TheScens) {
  
  TheYears<-c(2019)
  for (MyYear in TheYears) {
    
    
    MiDir<-(paste(MyDir, "/", MyScen, "/Supply", sep = ""))
    setwd(MiDir)
    
    YearSS<-substr(MyYear,1,4)
    S_Code<-substr(MyScen,1,2)
    
    HiLo<-c("HiC", "HiL")
    MyP<-paste(S_Code,"_",YearSS,"_*",sep = "" )
    
    files<-list.files(pattern = MyP)
    i<-0
    for(MyFile in files){
      Hvar<-substr(MyFile,9,11)
      if(Hvar=="HiC"){
        MyX<-fread(MyFile,header=T,sep=",")# Tell R which ccdem file to convert should be a 980 converted ccdem file
        MyX$BA<-rowSums(MyX[,c(3:7)])
        MyX$EA<-MyX[,c(9)]
        MyX$IOG<-MyX[,c(10)]
        MyX$MH<-rowSums(MyX[,c(11:12)])
        MyX$SF<-rowSums(MyX[,c(13:15)])
        MyX$FES<-MyScen
        MyX$HiLo<-Hvar
        
        NewTab<-MyX[,c(1,2,46:47,41:45,40)]
        names(NewTab)[10]<-'T_Demand'
        
        if(exists("NAP_Supply")){
          NAP_Supply<-rbind(NAP_Supply, NewTab)
        }else{
          NAP_Supply<-NewTab
        }
        
      }else{
        MyX<-fread(MyFile,header=T,sep=",")# Tell R which ccdem file to convert should be a 980 converted ccdem file
        MyX$BA<-rowSums(MyX[,c(3:7)])
        MyX$EA<-MyX[,c(9)]
        MyX$IOG<-MyX[,c(10)]
        MyX$MH<-rowSums(MyX[,c(11:12)])
        MyX$SF<-rowSums(MyX[,c(13:15)])
        MyX$FES<-MyScen
        MyX$HiLo<-Hvar
        
        NewTab<-MyX[,c(1,2,46:47,41:45,40)]
        names(NewTab)[10]<-'T_Demand'
        
        if(exists("NAP_Supply")){
          NAP_Supply<-rbind(NAP_Supply, NewTab)
        }else{
          NAP_Supply<-NewTab
        }
        
      }#HiLo
    }#Files
    
  }#Years
  
}#Scenario  

NAP_Supply$GasDay<-ymd(NAP_Supply$GasDay)
NAP_Supply$MyMon<-month(NAP_Supply$GasDay)

NAP_Apr<-subset(NAP_Supply, NAP_Supply$MyMon==4)
NAP_Apr$BAC<-90 - NAP_Apr$BA
NAP_Apr$IOGC<-43.5 - NAP_Apr$IOG
NAP_Apr$SFC<-105 - NAP_Apr$SF

NAP_May<-subset(NAP_Supply, NAP_Supply$MyMon==5)
NAP_May$BAC<-65- NAP_May$BA
NAP_May$IOGC<-41.2 - NAP_May$IOG
NAP_May$MHC<-45 - NAP_May$MH
NAP_May$SFC<-102.77 - NAP_May$SF

NAP_Jun<-subset(NAP_Supply, NAP_Supply$MyMon==6)
NAP_Jun$BAC<-70 - NAP_Jun$BA
NAP_Jun$IOGC<-39.1 - NAP_Jun$IOG
NAP_Jun$MHC<-45 - NAP_Jun$MH
NAP_Jun$SFC<-93.05 - NAP_Jun$SF

NAP_Jul<-subset(NAP_Supply, NAP_Supply$MyMon==7)
NAP_Jul$BAC<-65 - NAP_Jul$BA
NAP_Jul$EAC<-110 - NAP_Jul$EA
NAP_Jul$IOGC<-39.3 - NAP_Jul$IOG
NAP_Jul$MHC<-60 - NAP_Jul$MH
NAP_Jul$SFC<-78.03 - NAP_Jul$SF

NAP_Aug<-subset(NAP_Supply, NAP_Supply$MyMon==8)
NAP_Aug$BAC<-80 - NAP_Aug$BA
NAP_Aug$IOGC<-39.3 - NAP_Aug$IOG
NAP_Aug$MHC<-60 - NAP_Aug$MH
NAP_Aug$SFC<-82.21 - NAP_Aug$SF

NAP_Sep<-subset(NAP_Supply, NAP_Supply$MyMon==9)
NAP_Sep$BAC<-80 - NAP_Sep$BA
NAP_Sep$IOGC<-39.1 - NAP_Sep$IOG
NAP_Sep$SFC<-92.74 - NAP_Sep$SF

setwd(paste(MyDir, "/NAP_Maint", sep = ""))

MyBA<-subset(NAP_Apr, NAP_Apr$BAC<0)
MyBA<-rbind(MyBA,subset(NAP_May, NAP_May$BAC<0), fill = TRUE)
MyBA<-rbind(MyBA,subset(NAP_Jun, NAP_Jun$BAC<0), fill = TRUE)
MyBA<-rbind(MyBA,subset(NAP_Jul, NAP_Jul$BAC<0), fill = TRUE)
MyBA<-rbind(MyBA,subset(NAP_Aug, NAP_Aug$BAC<0), fill = TRUE)
MyBA<-rbind(MyBA,subset(NAP_Sep, NAP_Sep$BAC<0), fill = TRUE)
fwrite(MyBA, 'Bacton.csv',sep = ',')

MyEA<-subset(NAP_Apr, NAP_Apr$EAC<0)
MyEA<-rbind(MyEA,subset(NAP_May, NAP_May$EAC<0), fill = TRUE)
MyEA<-rbind(MyEA,subset(NAP_Jun, NAP_Jun$EAC<0), fill = TRUE)
MyEA<-rbind(MyEA,subset(NAP_Jul, NAP_Jul$EAC<0), fill = TRUE)
MyEA<-rbind(MyEA,subset(NAP_Aug, NAP_Aug$EAC<0), fill = TRUE)
MyEA<-rbind(MyEA,subset(NAP_Sep, NAP_Sep$EAC<0), fill = TRUE)
fwrite(MyEA, 'Easington.csv',sep = ',')

MyIOG<-subset(NAP_Apr, NAP_Apr$IOGC<0)
MyIOG<-rbind(MyIOG,subset(NAP_May, NAP_May$IOGC<0), fill = TRUE)
MyIOG<-rbind(MyIOG,subset(NAP_Jun, NAP_Jun$IOGC<0), fill = TRUE)
MyIOG<-rbind(MyIOG,subset(NAP_Jul, NAP_Jul$IOGC<0), fill = TRUE)
MyIOG<-rbind(MyIOG,subset(NAP_Aug, NAP_Aug$IOGC<0), fill = TRUE)
MyIOG<-rbind(MyIOG,subset(NAP_Sep, NAP_Sep$IOGC<0), fill = TRUE)
fwrite(MyIOG, 'IOG.csv',sep = ',')

MyMH<-subset(NAP_Apr, NAP_Apr$MHC<0)
MyMH<-rbind(MyMH,subset(NAP_May, NAP_May$MHC<0), fill = TRUE)
MyMH<-rbind(MyMH,subset(NAP_Jun, NAP_Jun$MHC<0), fill = TRUE)
MyMH<-rbind(MyMH,subset(NAP_Jul, NAP_Jul$MHC<0), fill = TRUE)
MyMH<-rbind(MyMH,subset(NAP_Aug, NAP_Aug$MHC<0), fill = TRUE)
MyMH<-rbind(MyMH,subset(NAP_Sep, NAP_Sep$MHC<0), fill = TRUE)
fwrite(MyMH, 'Milford.csv',sep = ',')

MySF<-subset(NAP_Apr, NAP_Apr$SFC<0)
MySF<-rbind(MySF,subset(NAP_May, NAP_May$SFC<0), fill = TRUE)
MySF<-rbind(MySF,subset(NAP_Jun, NAP_Jun$SFC<0), fill = TRUE)
MySF<-rbind(MySF,subset(NAP_Jul, NAP_Jul$SFC<0), fill = TRUE)
MySF<-rbind(MySF,subset(NAP_Aug, NAP_Aug$SFC<0), fill = TRUE)
MySF<-rbind(MySF,subset(NAP_Sep, NAP_Sep$SFC<0), fill = TRUE)
fwrite(MySF, 'St_Fergus.csv',sep = ',')

library(ggplot2)
# Basic histogram
ggplot(NAP_Sep, aes(x=SF)) + geom_histogram()
# Change the width of bins
ggplot(NAP_Sep, aes(x=SF)) + 
  geom_histogram(binwidth=1)
# Change colors
p<-ggplot(NAP_Sep, aes(x=SF)) + 
  geom_histogram(color="green", fill="light green")+
  labs(y = "Count", x = "St Fergus Supply (mscm/d)") +
  ggtitle("Forecast Data September 2020") 

jpeg("SF_Sep.jpg", units = "cm", width = 16, height = 9, res = 2000)
p
dev.off()

MyBnd<-Boundaries[8]
MyFES<-FES[1]
XX<-fread('C:/Neu_TS_2018/TobySpaces/SPCCDEM/Supply/SP_2020_HiCont.csv',header = T)
YY<-fread('C:/Neu_TS_2018/TobySpaces/SPCCDEM/Supply/SP_2020_HiLNG.csv',header = T)
XX$GasDay<-YY$GasDay
XX$SimNum<-YY$SimNum
XX$GasDay<-ymd(XX$GasDay)
fwrite(XX,'C:/Neu_TS_2018/TobySpaces/SPCCDEM/Supply/SP_2020_HiCont.csv',sep = ",")

for (j in Upd) {
  print(grep(j,names(CFile)))
  
}

library(data.table)
library(readxl)
library(lubridate)
library(dplyr)
library(reshape2)
library(fitdistrplus)

rm(list = ls())


MyDir<-"C:/Neu_TS_2018/TobySpaces"
setwd(MyDir)

FES<-c('CE', 'CR', 'SP', 'TD')

for (MyFES in FES) {
  MiDir<-paste(MyDir,"/", MyFES, "CCDEM/SC_Exit_Boundary",sep = "")
  setwd(MiDir)
  SC_Exit<-fread("SC_Exit.csv")
  if(exists("MyTab")==FALSE){
    MyTab<-SC_Exit
  }else{
    MyTab<-rbind(MyTab,SC_Exit)
  }
}

