CLCA_bystand <- function(Cutlist, CarbE = TRUE, Carbonsummary, per_energy, per_ef, Region1 ){
  
  # # #test Inputs
   # Cutlist <- read_csv("data/CLRC_Cutlist.csv", col_types = c("ff")) # THIS IS THE CUTLIST
   # per_energy <- 0.5
   # per_ef <- 0.5
   # Region1 <- "Northeast"
   # CarbE <- TRUE
   # Carbonsummary <- read_csv("data/CLRC_Carbon.csv", col_types = c("ff")) #   THIS SHOULD BE THE CARBON SUMMARY INPUT
  
   fvs_carbon <- Carbonsummary%>%
     mutate(years = Year-min(Year))%>%
     select(-Year)
   
   if(CarbE == TRUE){
     Cutlist$t1 <- Cutlist$MCuFt
     Cutlist$t2 <- Cutlist$SCuFt
   }else{
     Cutlist$t1 <- Cutlist$TCuFt
     Cutlist$t2 <- Cutlist$MCuFt
   }
   data <- Cutlist%>%
     mutate(pulpCuFt = (t1-t2)*TPA,
            sawCuFt = t2*TPA)%>%
     left_join(sp_lookup,by = c("SpeciesFIA"="Species"))
   
   
 
  
  ### DATA GROUPING ####
   DataByStand <- data%>%
     group_by(StandID,SW_HW)%>%
     summarise(SWSL_MgC = ifelse(SW_HW == "SW",((sum(sawCuFt)*0.0283168)*0.43)*0.48,0),
               SWPW_MgC = ifelse(SW_HW == "SW",((sum(pulpCuFt)*0.0283168)*0.43)*0.48,0),
               HWSL_MgC = ifelse(SW_HW == "HW",((sum(sawCuFt)*0.0283168)*0.43)*0.48,0),
               HWPW_MgC = ifelse(SW_HW == "HW",((sum(pulpCuFt)*0.0283168)*0.43)*0.48,0),
               TCuM = sum(t1)*0.0283168)%>%
     unique()%>%
     group_by(StandID)%>%
     summarise(SWSL_MgC = sum(SWSL_MgC),
               SWPW_MgC = sum(SWPW_MgC),
               HWSL_MgC = sum(HWSL_MgC),
               HWPW_MgC = sum(HWPW_MgC),
               TCuM = sum(TCuM))
  
  
   DataByStand <- DataByStand%>%
     group_by(StandID)%>%
     mutate(MgC_Eng = SWPW_MgC + HWPW_MgC * per_energy,               ###THIS WILL BE AN INPUT###
            MgC_Pap = SWPW_MgC + HWPW_MgC * (1- per_energy),
            H_emiss_MgCO2e = TCuM * 0.00925,
            T_emiss_MgCO2e = sum(SWSL_MgC, SWPW_MgC, HWSL_MgC, HWPW_MgC) * 0.0458,
            SW_Man_emiss_MgCO2e = SWSL_MgC* 0.461,
            HW_Man_emiss_MgCO2e = HWSL_MgC* 0.484,
            Paper_Man_emiss_MgCO2e = MgC_Pap* 0.384)%>%
     mutate(M_Emiss_MgCO2e = sum(SW_Man_emiss_MgCO2e, HW_Man_emiss_MgCO2e, Paper_Man_emiss_MgCO2e),
            A_emiss_MgCO2e = MgC_Eng * (per_ef * 0.652)) ###THIS WILL BE AN INPUT###
  
  
  # FOR LOOP FOR ADDING INUSE BY STAND ID FOR 40 YEARS
  CO2_OT_bystand <- data.frame(year = rep(0:35, times = nrow(DataByStand)))
  CO2_OT_bystand$StandID <- rep(DataByStand$StandID, times = 36)
  CO2_OT_bystand$in_use <- numeric(nrow(DataByStand)* 36)
  CO2_OT_bystand$landfill <- numeric(nrow(DataByStand)* 36)
  
  
  
  rate_table <- rate_calculator%>%
    select(-"Frac_energy")%>%
    filter("year_post" <= 40, Region == Region1)
  
  loop_rate_table <- data.frame(year = c(1:42),
                                SWSL_inuse = integer(42),
                                SWPW_inuse = integer(42),
                                HWSL_inuse = integer(42),
                                HWPW_inuse = integer(42),
                                SWSL_Landfill = integer(42),
                                SWPW_Landfill = integer(42),
                                HWSL_Landfill = integer(42),
                                HWPW_Landfill = integer(42)
  )  
  
  #creating loop_rate Table for loop
  for (i in 1:42){
    loop_rate_table$SWSL_inuse[i] <- inuse_rate_func(i-1,"Softwood","Saw log", Region1)
    loop_rate_table$SWPW_inuse[i] <- inuse_rate_func(i-1,"Softwood","Pulpwood", Region1)
    loop_rate_table$HWSL_inuse[i] <- inuse_rate_func(i-1,"Hardwood","Saw log", Region1)
    loop_rate_table$HWPW_inuse[i] <- inuse_rate_func(i-1,"Hardwood","Pulpwood", Region1)
    
    
    loop_rate_table$SWSL_Landfill[i] <- landfill_rate_func(i-1,"Softwood","Saw log", Region1)
    loop_rate_table$SWPW_Landfill[i] <- landfill_rate_func(i-1,"Softwood","Pulpwood", Region1)
    loop_rate_table$HWSL_Landfill[i] <- landfill_rate_func(i-1,"Hardwood","Saw log", Region1)
    loop_rate_table$HWPW_Landfill[i] <- landfill_rate_func(i-1,"Hardwood","Pulpwood", Region1)
    
  }
  
  #turn any potential NA's  into 0's
  # loop_rate_table[is.na(loop_rate_table)] <- 0
  
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
  
  CO2_OT_bystand <- data.frame(year = rep(0:41, times = 15),
                               StandID = rep(DataByStand$StandID, each = 42),
                               in_use = numeric(42*nrow(DataByStand)),
                               landfill = numeric(42*nrow(DataByStand)))
  
  CO2_OT_bystand <- rcppForLoop(CO2_OT_bystand, DataByStand, loop_rate_table)
  
  
  #build full database by stand
  DataByStand_full <- full_join(DataByStand,CO2_OT_bystand, by = "StandID")
  DataByStand_full <- DataByStand_full%>%
    mutate(Avoided_E_MgCO2e    = case_when(year == 0 ~ 0,
                                           year == 1 ~ A_emiss_MgCO2e,
                                           TRUE ~   0),
           Harvest_E_MgCO2e    = case_when(year == 0 ~ 0,
                                           year == 1 ~   H_emiss_MgCO2e,
                                           TRUE ~   0),
           Transport_E_MgCO2e  = case_when(year == 0 ~ 0,
                                           year == 1 ~  T_emiss_MgCO2e, 
                                           TRUE ~   0),
           Manu_E_MgCO2e       = case_when(year == 0 ~ 0,
                                           year == 1 ~  M_Emiss_MgCO2e,
                                           TRUE ~   0),
           in_use              = case_when(year ==  0 ~ 0,
                                           TRUE ~ in_use),
           landfill            = case_when(year ==   0 ~ 0,
                                           TRUE ~ landfill))
  
  
  #this needs to work with what ever the start year is and how ever long  the analyis is
  # years since treatment = year - startyea
  mean_TSC <- fvs_carbon[,c("StandID","years","Total_Stand_Carbon")]%>%
    group_by(years)%>%
    summarise(mean_carbonstocks = 0-mean(Total_Stand_Carbon*3.667, na.rm = TRUE))
  
  CO2e_Year <- DataByStand_full%>%
    group_by(year)%>%
    summarise(mean_inuse = 0-mean(in_use * 3.667, na.rm = TRUE),
              mean_landfill = 0-mean(landfill * 3.667,na.rm = TRUE),
              mean_harvest_E = mean(Harvest_E_MgCO2e,na.rm = TRUE), 
              mean_transport_E = mean(Transport_E_MgCO2e,na.rm = TRUE),
              mean_Manufacturing_E = mean(Manu_E_MgCO2e,na.rm = TRUE),
              mean_avoided_E = 0-mean(Avoided_E_MgCO2e ,na.rm = TRUE),
              mean_carbonStocks = case_when(year ==  0 ~ mean_TSC$mean_carbonstocks[which(mean_TSC$years ==  0)],
                                            year ==  1 ~ mean_TSC$mean_carbonstocks[which(mean_TSC$years ==  1)],
                                            year ==  6 ~ mean_TSC$mean_carbonstocks[which(mean_TSC$years ==  6)],
                                            year == 11 ~ mean_TSC$mean_carbonstocks[which(mean_TSC$years == 11)],
                                            year == 16 ~ mean_TSC$mean_carbonstocks[which(mean_TSC$years == 16)],
                                            year == 21 ~ mean_TSC$mean_carbonstocks[which(mean_TSC$years == 21)],
                                            year == 26 ~ mean_TSC$mean_carbonstocks[which(mean_TSC$years == 26)],
                                            year == 31 ~ mean_TSC$mean_carbonstocks[which(mean_TSC$years == 31)],
                                            year == 36 ~ mean_TSC$mean_carbonstocks[which(mean_TSC$years == 36)],
                                            year == 41 ~ mean_TSC$mean_carbonstocks[which(mean_TSC$years == 41)],
                                            TRUE ~ 0))%>%
    unique()%>%
    ungroup()
  
  
  CO2e_Year <- as.data.frame(CO2e_Year)%>%
    #finds diff between change in carbon after 5 years
    mutate(CarbonStocksChange = mean_carbonStocks - lag(mean_carbonStocks, n = 5, default = 0)) 
  
  
  CO2e_Year <- CO2e_Year%>%
    mutate(CSC_cont = rep((CarbonStocksChange[seq(from = 7, to = length(CarbonStocksChange), 5)]/ 5), each = 5, length.out = 42))%>% #divides diff by 5 and repeats
    mutate(CSC_cont = lag(CSC_cont,2))%>% #lags by one row to offset from year 0
    mutate(CSC_cont = coalesce(CSC_cont,0)) # fills row 1 with 0
  
  CO2e_Year[c(2),10] <- CO2e_Year[c(2),9] # fill in the first row with carbonstockschange year 0
  
  CO2e_Year <- CO2e_Year%>%
    mutate(CSC_sum  = cumsum(CSC_cont))%>%
    mutate(storesum = rowSums(.[c(2,3,11)]),
           emishsum = cumsum(rowSums(.[4:7])))%>%
    mutate(NetCO2e  = rowSums(.[12:13]))
  CO2e_Year[1,11] <- CO2e_Year[1,9]
  
  
  print(CO2e_Year)
}
