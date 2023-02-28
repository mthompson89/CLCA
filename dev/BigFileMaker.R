library(tidyverse)
library(data.table)

#get file names
cabon_filenames <- list.files(pattern = "CarbonOutput.csv", recursive = TRUE)
summary_filenames <- list.files(pattern = "Summary", recursive = TRUE)
Condnames <- c("StandID","Year","Cond_ForType","Cond_BA","Cond_StdAge")



#load data
#loadCarbonData
# CTRH_Carbon <- read_csv("CarbonOutputs/CTRHigh_CarbonOutput.csv") %>%  mutate(MgmtID = "CTRH") 
# SHTW_Carbon <- read_csv("CarbonOutputs/Shelterwood_CarbonOutput.csv") %>% mutate(MgmtID = "SHTW") 

CarbonOutputs <- rbind(read_csv("CarbonOutputs/CTRLow_CarbonOutput.csv") %>% mutate(MgmtID = "CTRL"), 
                       read_csv("CarbonOutputs/HeavyHarvest_CarbonOutput.csv") %>% mutate(MgmtID = "HVYH"), 
                       read_csv("CarbonOutputs/PartialHarvest_CarbonOutput.csv") %>% mutate(MgmtID = "PARH"),
                       read_csv("CarbonOutputs/TSIHigh_CarbonOutput.csv") %>% mutate(MgmtID = "TSIH"), 
                       read_csv("CarbonOutputs/TSILow_CarbonOutput.csv") %>% mutate(MgmtID = "TSIL") 
                       )%>%
  rename("Year" = years) %>%
  mutate(Year = case_when(Year == -1 ~2021,
                          Year == 0 ~ 2022,
                          Year == 1 ~ 2023,
                          Year == 2 ~ 2024,
                          Year == 3 ~ 2025,
                          Year == 4 ~ 2026,
                          Year == 5 ~ 2027,
                          Year == 6 ~ 2028,
                          Year == 7 ~ 2029,
                          Year == 8 ~  2030,
                          Year == 9 ~  2031,
                          Year == 10 ~ 2032,
                          Year == 11 ~ 2033,
                          Year == 12 ~ 2034,
                          Year == 13 ~ 2035,
                          Year == 14 ~ 2036,
                          Year == 15 ~ 2037,
                          Year == 16 ~ 2038,
                          Year == 17 ~ 2039,
                          Year == 18 ~ 2040,
                          Year == 19 ~ 2041,
                          Year == 20 ~ 2042,
                          Year == 21 ~ 2043,
                          Year == 22 ~ 2044,
                          Year == 23 ~ 2045,
                          Year == 24 ~ 2046,
                          Year == 25 ~ 2047,
                          Year == 26 ~ 2048,
                          Year == 27 ~ 2049,
                          Year == 28 ~ 2050,
                          Year == 29 ~ 2051,
                          Year == 30 ~ 2052,
                          Year == 31 ~ 2053,
                          Year == 32 ~ 2054,
                          Year == 33 ~ 2055,
                          Year == 34 ~ 2056,
                          Year == 35 ~ 2057,
                          Year == 36 ~ 2058,
                          Year == 37 ~ 2059,
                          Year == 38 ~ 2060,
                          Year == 39 ~ 2061,
                          Year == 40 ~ 2062,
                          Year == 41 ~ 2063,
                          Year == 42 ~ 2064,
                          Year == 43 ~ 2065,
                          Year == 44 ~ 2066,
                          Year == 45 ~ 2067,
                          Year == 46 ~ 2068,
                          Year == 47 ~ 2069,
                          Year == 48 ~ 2070,
                          Year == 49 ~ 2071,
                          Year == 50 ~ 2072))


SummaryOutputs <- rbind(read_csv("NHCTRLow_Summary5Yr_w_deg.csv"),
                        read_csv("NHHeavyHarvest_Summary5Yr_w_deg.csv"),
                        read_csv("PartialHarvest_Summary5Yr_w_deg.csv"),
                        read_csv("NHTSIHigh_Summary5Yr_w_deg.csv"),
                        read_csv("NHTSILow_Summary5Yr_w_deg.csv"))%>% 
  filter(Year <= 2069)


## LoadCondition Data
##

COND <-  read_csv("data/FIANH_COND.csv",col_types = cols(CN = "c",PLT_CN = "c")) %>% 
  select(CN,INVYR,FORTYPCD,BALIVE,STDAGE) %>% 
  rename("StandID"=CN,"ForType" = FORTYPCD,"StartingBA" = BALIVE,"StndAge" = STDAGE) 
  
  
## join all tables ##

totaltable <- SummaryOutputs %>% 
  inner_join(CarbonOutputs, by = c('StandID','MgmtID','Year')) %>% 
  left_join(COND, by = "StandID")


#check
CTindx <- apply(CarbonOutputs, 2, function(x) any(is.na(x) | is.infinite(x)))
CTindx
STindx <- apply(SummaryOutputs, 2, function(x) any(is.na(x) | is.infinite(x)))
STindx
TTindx <- apply(totaltable, 2, function(x) any(is.na(x) | is.infinite(x)))
TTindx

write_csv(totaltable,"TotalSummaryTable.csv")


