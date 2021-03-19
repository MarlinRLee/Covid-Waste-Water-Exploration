#---
#title: "Data.Proccess"
#author: "Marlin"
#date: "3/13/2021"
#output: html_document
#---

library(tidyverse)
library(readxl)
library(broom)
library(zoo)
library(RcppRoll)
library(ggformula)
library(ggpubr)


#Creates WastewaterData

tmpfn <- function(N1, N2) {
  AVG <- sqrt(N1 * N2)
  AVG <- ifelse(is.na(N1), N2, AVG)
  AVG <- ifelse(is.na(N2), N1, AVG)
}
WasteWater = function(filename){
  
  tmpfn <- function(N1, N2) {
    AVG <- sqrt(N1 * N2)
    AVG <- ifelse(is.na(N1), N2, AVG)
    AVG <- ifelse(is.na(N2), N1, AVG)
  }
  
  missing_codes <- c("","NA","0","Undetected","Not Detected",
                     "Field Parameters to be filled in", 
                     "Inhibited-to be re-ran", "#DIV/0!")
  sheets <- excel_sheets(Waterfilename)
  water_MMSD <- read_excel(Waterfilename,
                           na = missing_codes,
                           col_types = c("text", "date", rep("numeric", 14)),
                           sheet = 1)%>%
    select(-c(13:16))
  water_Interceptors <- read_excel(Waterfilename,
                                   na = missing_codes,
                                   col_types = c("text", "date", rep("numeric", 8), "text"),
                                   sheet = 2) %>%
    select(-"...11") %>%
    mutate(TSS = NA,
           Site = ifelse(Site == "MMSD P02", "MMSD P2", Site),
           Site = ifelse(Site == "MMSD P07", "MMSD P7", Site),
           Site = ifelse(Site == "MMSD P08", "MMSD P8", Site)) %>%
    select(1:5, TSS, everything())
  water_UW_Dorms <- read_excel(Waterfilename,
                               na = missing_codes,
                               col_types = c("text", "date", rep("numeric", 9), rep("text", 1)),
                               sheet = 3) %>%
    select(-"...12")
  water_UW_sellery <- read_excel(Waterfilename,
                                 na = missing_codes,
                                 col_types = c("text", "date", rep("numeric", 9), rep("text", 1)),
                                 sheet = 4) %>%
    select(-"...12")
  water <- 
    bind_rows(water_MMSD,
              water_Interceptors,
              water_UW_Dorms,
              water_UW_sellery) %>%
    rename(Date = "Collection Date",
           pH = "pH (SU)",
           Total_Flow = "Total Flow (MGD)",
           Conductivity = "Conductivity (uS/CM@25C)",
           N1 = "N1 (GC/L)",
           N2 = "N2 (GC/L)",
           PMMoV = "PMMoV (GC/L)",
           Pct_BCoV = "% Recovery (BCoV)") %>%
    filter(!is.na(Site)) %>%
    mutate(AVG = tmpfn(N1, N2),
           wt = 2 - is.na(N1) - is.na(N2),
           Site = ifelse(Site == "UW-D", "UW-LakeShore", Site),
           Site = ifelse(Site == "UW-S", "UW-Sellery", Site),
           TSS = ifelse(is.na(TSS), `TSS (mg/L)`, TSS),
           Date=as.Date(Date))%>%
    select(-`TSS (mg/L)`)
}

#Creates  CovidNumberData

CovidData = function(CovidFileName){
  lag=7
  covidData = read.csv(CovidFileName)%>%
    mutate(ServiceID = ifelse(ServiceID=="MMSD","Madison",paste("MMSD P",ServiceID,sep="")),Date = as.Date(Date))%>%
    rename(Site=ServiceID)%>%
    group_by(Site)%>%
    mutate(Cases=Cases - lag(Cases, n=lag,default = NA),Tests=Tests- lag(Tests, n=lag,default = NA))%>%
    mutate(roll=Cases/Tests)
  return(covidData)
}
N2Fixer = function(data){
  N2Mod = lm(log(N2)~log(N1), data=data)
  data$temp=10^predict.lm(N2Mod,data,interval="none")
  data=data%>%
    mutate(N2=ifelse(is.na(N2),temp,N2))
  data=data%>%
    select(-temp)
  return(data)
}
N1Fixer = function(data){
  N1Mod = lm(N1~N2, data=data)
  data$temp=predict.lm(N1Mod,data,interval="none")
  data=data%>%
    mutate(N2=ifelse(is.na(N2),temp,N2))
  data=data%>%
    select(-temp)
  return(data)
}