
source("functions/load_functions.R")
non_appCLCA <- function(Cutlist,Carbonsummary, CarbE = TRUE, Treelist = FALSE,per_energy = 0.5, per_ef=0.5, Region1 = "Northeast" ){
  
  # # #test Inputs
  library(shiny)
  library(tidyverse)
  library(readxl)
  library(data.table)
  library(gridExtra)
  library(plotrix)
  library(plotly)
  library(Rcpp)
  library(vroom)

  

  fvs_carbon <- Carbonsummary%>%
    filter(Year > 2022) %>% 
    mutate(years = Year-min(Year))%>%
    select(-Year) %>% 
    dplyr::filter(years <=46)
  
  
  numStands <- length(unique(Cutlist$StandID))
  
  if(CarbE == TRUE){
    Cutlist$t1 <- Cutlist$MCuFt
    Cutlist$t2 <- Cutlist$SCuFt
  }else{
    Cutlist$t1 <- Cutlist$TCuFt
    Cutlist$t2 <- Cutlist$MCuFt
  }
  if(Treelist == FALSE){
    data <- Cutlist%>%
      mutate(pulpCuFt = (t1-t2)*TPA,
             sawCuFt = t2*TPA,
             years = Year-min(Year))%>%
      left_join(sp_lookup,by = c("SpeciesFIA"="Species"))%>%
      select(-Year)
    
    
    ### DATA GROUPING by stand AND year ####
    DataByStandandYr <- data%>% 
      group_by(StandID,SW_HW,years)%>%
      summarise(SWSL_MgC = ifelse(SW_HW == "SW",((sum(sawCuFt)*0.0283168)*0.43)*0.48,0),
                SWPW_MgC = ifelse(SW_HW == "SW",((sum(pulpCuFt)*0.0283168)*0.43)*0.48,0),
                HWSL_MgC = ifelse(SW_HW == "HW",((sum(sawCuFt)*0.0283168)*0.43)*0.48,0),
                HWPW_MgC = ifelse(SW_HW == "HW",((sum(pulpCuFt)*0.0283168)*0.43)*0.48,0),
                TCuM = sum(t1)*0.0283168)%>%
      unique()%>%
      group_by(StandID,years)%>%
      summarise(SWSL_MgC = sum(SWSL_MgC),
                SWPW_MgC = sum(SWPW_MgC),
                HWSL_MgC = sum(HWSL_MgC),
                HWPW_MgC = sum(HWPW_MgC),
                TCuM = sum(TCuM))
    
    
    DataByStandandYr <- DataByStandandYr%>%
      group_by(StandID,years)%>%
      mutate(MgC_Eng = SWPW_MgC + HWPW_MgC * per_energy,               
             MgC_Pap = SWPW_MgC + HWPW_MgC * (1- per_energy),
             H_emiss_MgCO2e = TCuM * 0.00925,
             T_emiss_MgCO2e = sum(SWSL_MgC, SWPW_MgC, HWSL_MgC, HWPW_MgC) * 0.0458,
             SW_Man_emiss_MgCO2e = SWSL_MgC* 0.461,
             HW_Man_emiss_MgCO2e = HWSL_MgC* 0.484,
             Paper_Man_emiss_MgCO2e = MgC_Pap* 0.384)%>%
      mutate(M_Emiss_MgCO2e = sum(SW_Man_emiss_MgCO2e, HW_Man_emiss_MgCO2e, Paper_Man_emiss_MgCO2e),
             A_emiss_MgCO2e = MgC_Eng * (per_ef * 0.652)) 
    
    
    ## rate table calculations#####
    
    loop_rate_table <- data.frame(year = c(1:47),
                                  SWSL_inuse = integer(47),
                                  SWPW_inuse = integer(47),
                                  HWSL_inuse = integer(47),
                                  HWPW_inuse = integer(47),
                                  SWSL_Landfill = integer(47),
                                  SWPW_Landfill = integer(47),
                                  HWSL_Landfill = integer(47),
                                  HWPW_Landfill = integer(47)
    )  
    
    #creating loop_rate Table for loop
    for (i in 1:46){
      loop_rate_table$SWSL_inuse[i] <- inuse_rate_func(i-1,"Softwood","Saw log", Region1)
      loop_rate_table$SWPW_inuse[i] <- inuse_rate_func(i-1,"Softwood","Pulpwood", Region1)
      loop_rate_table$HWSL_inuse[i] <- inuse_rate_func(i-1,"Hardwood","Saw log", Region1)
      loop_rate_table$HWPW_inuse[i] <- inuse_rate_func(i-1,"Hardwood","Pulpwood", Region1)
      
      
      loop_rate_table$SWSL_Landfill[i] <- landfill_rate_func(i-1,"Softwood","Saw log", Region1)
      loop_rate_table$SWPW_Landfill[i] <- landfill_rate_func(i-1,"Softwood","Pulpwood", Region1)
      loop_rate_table$HWSL_Landfill[i] <- landfill_rate_func(i-1,"Hardwood","Saw log", Region1)
      loop_rate_table$HWPW_Landfill[i] <- landfill_rate_func(i-1,"Hardwood","Pulpwood", Region1)
      
    }
    
    
    #shift rate table to allow for year -1
    loop_rate_table <- loop_rate_table%>%
      mutate(year = lag(year,1,default = 0),
             SWSL_inuse = lag(SWSL_inuse,1,default = 0),
             SWPW_inuse = lag(SWPW_inuse,1,default = 0),
             HWSL_inuse = lag(HWSL_inuse,1,default = 0),
             HWPW_inuse = lag(HWPW_inuse,1,default = 0),
             SWSL_Landfill = lag(SWSL_Landfill,1,default = 0),
             SWPW_Landfill = lag(SWPW_Landfill,1,default = 0),
             HWSL_Landfill = lag(HWSL_Landfill,1,default = 0),
             HWPW_Landfill = lag(HWPW_Landfill,1,default = 0))
    
    CO2_OT_bystand_yr <- data.frame(years = rep(0:46, times = nrow(DataByStandandYr)),
                                    year = rep(DataByStandandYr$years,each = 47),
                                    StandID = rep(DataByStandandYr$StandID,each = 47),
                                    in_use = numeric(nrow(DataByStandandYr)*47),
                                    landfill = numeric(nrow(DataByStandandYr)*47))
    
    
    CO2_OT_bystand_yr <- rcppForLoop(CO2_OT_bystand_yr, DataByStandandYr, loop_rate_table, numStands)
    
    if(sum(CO2_OT_bystand_yr$year) > 0){
      
      CO2_wide <- CO2_OT_bystand_yr %>% 
        pivot_wider(id_cols = c(StandID,years), names_from = year,values_from = c(in_use,landfill))
      cnames <- colnames(CO2_wide)
      CO2_wide2 <- CO2_wide%>% 
        group_by(StandID) %>%
        mutate(in_use_5    = ifelse("in_use_5"  %in% cnames,lag(in_use_5, n   = 5L , default = 0),0),
               in_use_10   = ifelse("in_use_10" %in% cnames,lag(in_use_10,n   = 10L, default = 0),0),
               in_use_15   = ifelse("in_use_15" %in% cnames,lag(in_use_15,n   = 15L, default = 0),0),
               in_use_20   = ifelse("in_use_20" %in% cnames,lag(in_use_20,n   = 20L, default = 0),0),
               in_use_25   = ifelse("in_use_25" %in% cnames,lag(in_use_25,n   = 25L, default = 0),0),
               in_use_30   = ifelse("in_use_30" %in% cnames,lag(in_use_30,n   = 30L, default = 0),0),
               in_use_35   = ifelse("in_use_35" %in% cnames,lag(in_use_35,n   = 35L, default = 0),0),
               in_use_40   = ifelse("in_use_40" %in% cnames,lag(in_use_40,n   = 40L, default = 0),0),
               in_use_45   = ifelse("in_use_45" %in% cnames,lag(in_use_45,n   = 45L, default = 0),0),
               landfill_5  = ifelse("landfill_5" %in%  cnames, lag(landfill_5 ,n = 5L,  default = 0),0),
               landfill_10 = ifelse("landfill_10" %in% cnames,lag(landfill_10,n = 10L, default = 0),0),
               landfill_15 = ifelse("landfill_15" %in% cnames,lag(landfill_15,n = 15L, default = 0),0),
               landfill_20 = ifelse("landfill_20" %in% cnames,lag(landfill_20,n = 20L, default = 0),0),
               landfill_25 = ifelse("landfill_25" %in% cnames,lag(landfill_25,n = 25L, default = 0),0),
               landfill_30 = ifelse("landfill_30" %in% cnames,lag(landfill_30,n = 30L, default = 0),0),
               landfill_35 = ifelse("landfill_35" %in% cnames,lag(landfill_35,n = 35L, default = 0),0),
               landfill_40 = ifelse("landfill_40" %in% cnames,lag(landfill_40,n = 40L, default = 0),0),
               landfill_45 = ifelse("landfill_45" %in% cnames,lag(landfill_45,n = 45L, default = 0),0)) %>% 
        select(StandID,years,
               in_use_0,in_use_5,in_use_10,in_use_15,in_use_20,in_use_25,
               in_use_30,in_use_35,in_use_40,in_use_45,landfill_0,landfill_5,
               landfill_10,landfill_15,landfill_20,landfill_25,landfill_30,
               landfill_35, landfill_40,landfill_45) %>% 
        replace(is.na(.),0)
      
      INLF_bsy <-  CO2_wide2%>%  
        rowwise() %>% 
        mutate(in_use = sum(c(in_use_0,in_use_5,in_use_10,in_use_15,in_use_20,in_use_25,
                              in_use_30,in_use_35,in_use_40,in_use_45)),
               landfill = sum(c(landfill_0,landfill_5,landfill_10,landfill_15,landfill_20,
                                landfill_25,landfill_30,landfill_35, landfill_40,landfill_45))) %>% 
        select(StandID,years,in_use,landfill)
    }else{
      INLF_bsy <- CO2_OT_bystand_yr %>% 
        select(StandID,years,in_use,landfill)
    }
    
    #build full database by stand
    DataByStand_full <- DataByStandandYr %>% 
      select(-c(3:7)) %>% 
      mutate(years = years+1) %>% 
      full_join(INLF_bsy, by = c("StandID","years")) %>% 
      replace(is.na(.),0) %>% 
      rename(Avoided_MgCO2e   = A_emiss_MgCO2e,
             Harvest_MgCO2e   = H_emiss_MgCO2e,
             Transport_MgCO2e = T_emiss_MgCO2e,
             Manu_MgCO2e      = M_Emiss_MgCO2e)
    
    #####
    
    
    # this needs to work with what ever the start year is and how ever long  the analyis is
    # years since treatment = year - start year
    # grouped by just Year to get total summary----------------
    CO2e_SY.1 <- fvs_carbon[,c("StandID","years","Total_Stand_Carbon")]%>%
      mutate(years = years+1) %>% 
      group_by(StandID,years)%>%
      summarise(mean_carbonstocks = 0-mean(Total_Stand_Carbon*3.667)) %>% 
      full_join(DataByStand_full, by = c("StandID","years")) %>% 
      replace(is.na(.),0) %>% 
      mutate(inuse_MgCO2e = 0-(in_use*3.667),
             landfill_MgCO2e = 0-(landfill*3.667),
             Avoided_MgCO2e = 0-Avoided_MgCO2e) %>% 
      select(StandID,years,inuse_MgCO2e,landfill_MgCO2e,Avoided_MgCO2e,
             Harvest_MgCO2e,Transport_MgCO2e,Manu_MgCO2e,mean_carbonstocks
      )
    
    
    
    CO2e_SY.2 <- as.data.frame(CO2e_SY.1) %>%
      group_by(StandID) %>% 
      arrange(StandID,years) %>% 
      #finds diff between change in carbon after 5 years
      # mutate(CarbonStocksChange = mean_carbonStocks - lag(mean_carbonStocks, n = 5, default = 0))
      mutate(CarbonStocksChange = mean_carbonstocks - lag(mean_carbonstocks, n = 5)) %>%
      mutate(CarbonStocksChange = replace(CarbonStocksChange,2,mean_carbonstocks[2])) %>% 
      replace(is.na(.),0) %>% 
      arrange(StandID,years)
    

    CO2e_SY.3 <- CO2e_SY.2%>%
      group_by(StandID) %>% 
      mutate(CSC_cont = rep((CarbonStocksChange[seq(from = 8, to = length(CarbonStocksChange), by = 5)]/ 5),
                            each = 5, length.out = length(years)))  %>%     #divides diff by 5 and repeats
      mutate(CSC_cont = replace(lag(CSC_cont,2,default = 0),2,CarbonStocksChange[2]))          #lags by one row to offset from year 0
    
    CO2e_SY.3[c(2),11] <- CO2e_SY.3[c(2),9] # fill in the first row with carbonstockschange year 0
    
    CO2e_SY.4 <- CO2e_SY.3%>%
      group_by(StandID) %>% 
      mutate(ForestStocks  = cumsum(CSC_cont),
             ForestStocks = replace(ForestStocks,1,ForestStocks[2]))%>%
      rowwise() %>% 
      mutate(storesum = sum(c(inuse_MgCO2e,landfill_MgCO2e,Avoided_MgCO2e)),
             emishsum = sum(c(Harvest_MgCO2e,Transport_MgCO2e,Manu_MgCO2e)))%>%
      mutate(NetCO2e  = sum(c(storesum,emishsum,ForestStocks)))
    # CO2e_Year[1,11] <- CO2e_Year[1,9]
    
    CO2e_SY.5 <- CO2e_SY.4%>%
      group_by(StandID) %>% 
      mutate(years = c(-1:(length(years)-2))) %>% 
      select(StandID,years,inuse_MgCO2e,landfill_MgCO2e,
             Avoided_MgCO2e,Harvest_MgCO2e,Transport_MgCO2e,
             Manu_MgCO2e,ForestStocks,NetCO2e)
    
    
    
  }else{
    
    CO2e_SY.1 <- fvs_carbon[,c("StandID","years","Total_Stand_Carbon")]%>%
      mutate(years = years+1) %>% 
      group_by(StandID,years)%>%
      summarise(mean_carbonstocks = 0-mean(Total_Stand_Carbon*3.667))
    
    CO2e_SY.1.5 <- CO2e_SY.1 %>% 
      group_by(StandID) %>% 
      complete(years = min(0):max(years))
    
    
    CO2e_SY.2 <- as.data.frame(CO2e_SY.1.5)%>%
      group_by(StandID) %>% 
      arrange(StandID,years) %>% 
      #finds diff between change in carbon after 5 years
      #mutate(CarbonStocksChange = mean_carbonStocks - lag(mean_carbonStocks, n = 5, default = 0))
      mutate(CarbonStocksChange = mean_carbonstocks - lag(mean_carbonstocks, n = 5)) %>%
      mutate(CarbonStocksChange = replace(CarbonStocksChange,2,mean_carbonstocks[2])) %>% 
      replace(is.na(.),0) %>% 
      arrange(StandID,years)
    
    
    CO2e_SY.3 <- CO2e_SY.2%>%
      group_by(StandID) %>% 
      mutate(CSC_cont = rep((CarbonStocksChange[seq(from = 7, to = length(CarbonStocksChange), 5)]/ 5),
                            each = 5, length.out = length(years)))  %>%     #divides diff by 5 and repeats
      mutate(CSC_cont = replace(lag(CSC_cont,2,default = 0),2,CarbonStocksChange[2]))          #lags by one row to offset from year 0
    
    CO2e_SY.3[c(1),4] <- CO2e_SY.3[c(2),3] # fill in the first row with carbonstockschange year 0
    
    CO2e_SY.4 <- CO2e_SY.3%>%
      group_by(StandID) %>% 
      mutate(ForestStocks  = cumsum(CSC_cont),
             ForestStocks = replace(ForestStocks,1,ForestStocks[2]))

    
    CO2e_SY.5 <- CO2e_SY.4%>%
      group_by(StandID) %>% 
      mutate(Year = c(-1:(length(years)-2)),
             MgmtID = "LItG") %>% 
      select(StandID,MgmtID,Year,ForestStocks)

    
  }
  
  
  
}




