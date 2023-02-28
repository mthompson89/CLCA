library(tidyverse)
library(dbplyr)
library(RSQLite)
library(stringr)
library(foreach)

files <- list.files(path = "Z:/FVS_OutputFiles/RunOutputDb Files ", pattern = '*.db', full.names = TRUE)

treesp <- read_csv("Sp_data.csv") %>% 
  mutate(SpeciesFIA = as.character(splist),
         SpeciesFIA = ifelse(nchar(SpeciesFIA)<3,paste("0",SpeciesFIA,sep = ""),SpeciesFIA)) 

files_short <- files[1:2]
summaries_w_deg <- list()

dbdeg <- function(x){
  
  filename <- basename(files[x])
  print(paste("basename created:",filename))
  
  con <- DBI::dbConnect(RSQLite::SQLite(),dbname = filename)
  print(paste("connection made to",filename))
  
  
  fortyp_rx <- sub("\\..*", "", filename)
  fortypName <- sub("\\_.*", "", filename)
  rx <- str_extract(filename, "CTRL|CTRH|TSIH|TSIL|HvyH|ParH|ShwD|LtiG")
  
  print(paste("savename and forest type created:",fortyp_rx,fortypName))
  
  Treelist <- tbl(con,"FVS_TreeList_East")
  summary <- tbl(con, "FVS_Summary2_East")
  
  print("Treelist and summaries connected")
 
  # read in and join tree list with sp info
  Treelist_edited <-Treelist %>% 
    filter(Year >= 2020) %>% 
    left_join(treesp, by = "SpeciesFIA", copy = TRUE)%>% 
    mutate(RD = (TPA/0.404686)*(0.00015+0.00218*sg)*((DBH*2.54)/25)^1.6,
           prime_AGS = desirability == 1  & TreeVal != 3,
           sec_AGS = desirability == 2 & TreeVal != 3,
           tert_AGS = desirability == 3 & TreeVal != 3,
           UGS = TreeVal == 3) 
  
  print("treelist and sp list joined")
  #summarise treelist to get the prime,sec,tert, and UGS Relative density
  standsummaries <- Treelist_edited %>% 
    group_by(StandID,Year) %>% 
    summarise(PrimeRD = sum(ifelse(prime_AGS == TRUE,RD,0),na.rm = T),
              SecRD = sum(ifelse(sec_AGS == TRUE,RD,0),na.rm = T),
              TertRD = sum(ifelse(tert_AGS == TRUE,RD,0),na.rm = T),
              UGSRD = sum(ifelse(UGS == TRUE,RD,0),na.rm = T))
  print("treelist summarised with RD")
  
  #calculate degradation by stand and year 
  standsummaries <- standsummaries %>% 
    group_by(StandID,Year) %>% 
    mutate(Deg = case_when(PrimeRD >= 0.4 ~ 1,
                           PrimeRD + SecRD >= 0.4~ 2,
                           PrimeRD + SecRD + TertRD >= 0.4 ~ 3,
                           PrimeRD + SecRD + TertRD + UGSRD >= 0.4 ~4,
                           TRUE ~ 5)) %>% 
    select(StandID,Year,Deg)
  print("Deg Calculated for stands")
  
  
  # filter the FVS_summary table to only 2020
  Summarydf<- summary %>% 
    filter(Year>= 2020)
  
  #Join the Treelist and the summary table excluding stands that have NA deg (because of no trees)
  sumFinal <- Summarydf %>% 
    left_join(standsummaries, by = c("StandID","Year"))%>% 
    filter(is.na(Deg) == FALSE) 
  
  print("treelist joined with summaries")
  
  Sum_w_Deg <- sumFinal %>% 
    mutate(fortyp = fortypName) %>% 
    collect()
  print("summary collected")
  
  Sum_w_Deg <- as.data.frame(Sum_w_Deg) %>% 
    mutate(fortyp = fortypName,
           Rx = rx) %>% 
    select(-CaseID)
  
  return(Sum_w_Deg)
  
}

 sum_w_deg <- foreach(i = 1:length(files), .combine = 'rbind')%do% {
   dbdeg(i)
    }
