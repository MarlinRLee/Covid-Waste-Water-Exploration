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
  
  missing_codes <- c("","NA","0","Undetected","Not Detected",
                     "Field Parameters to be filled in", 
                     "Inhibited-to be re-ran", "#DIV/0!")
  sheets <- excel_sheets(filename)
  water_MMSD <- read_excel(filename,
                      na = missing_codes,
                      col_types = c("text", "date", rep("numeric", 9), "text"),
                      sheet = 1) %>%
    rename(Comment = "...12")
  water_Interceptors <- read_excel(filename,
                      na = missing_codes,
                      col_types = c("text", "date", rep("numeric", 8), "text"),
                      sheet = 2) %>%
    rename(Comment = "...11") %>%
    mutate(TSS = NA,
           Site = ifelse(Site == "MMSD P02", "MMSD P2", Site),
           Site = ifelse(Site == "MMSD P07", "MMSD P7", Site),
           Site = ifelse(Site == "MMSD P08", "MMSD P8", Site)) %>%
    select(1:5, TSS, everything())
  water_UW_Dorms <- read_excel(filename,
                      na = missing_codes,
                      col_types = c("text", "date", rep("numeric", 9), rep("text", 1)),
                      sheet = 3) %>%
    rename(Comment = "...12")
  water <- 
    bind_rows(water_MMSD,
              water_Interceptors,
              water_UW_Dorms) %>%
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
           Site = ifelse(Site == "UW-S", "UW-Sellery", Site))
  return(water)
}
#Creates  CovidNumberData

CovidData = function(CovidFileName){
  lag=7
  covidData = read.csv(CovidFileName)%>%
    mutate(ServiceID = ifelse(ServiceID=="MMSD","Madison",paste("MMSD P",ServiceID,sep="")),Date = as.Date(Date , format = "%m/%d/%y"))%>%
    rename(Site=ServiceID)%>%
    group_by(Site)%>%
    mutate(Cases=Cases - 0*lag(Cases, n=lag,default = NA),Tests=Tests- 0*lag(Tests, n=lag,default = NA))%>%
    mutate(roll=Cases/Tests)
  return(covidData)
}

