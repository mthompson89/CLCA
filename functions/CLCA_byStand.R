CLCA_bystand <- function(Cutlist, Silvi_M, per_energy, per_ef, Region1 ){
  
  data <- mutate(Cutlist, pulpCuFt = (T..1-T..2)*TPA)
  data <- mutate(data, sawCuFt = T..2*TPA)
  data <- data%>%
    left_join(sp_lookup, by ="Species")
  
  data$MgmtID <-  dplyr::recode(data$MgmtID,"clrc" = "clearcut", "clrp" = "clearcut_plant", "CTR" = "CTR")
  
  data <- data%>%
    filter(MgmtID == Silvi_M)
  
  carbon <- fvs_carbon%>%
    select(MgmtID, StandID, Total_Stand_Carbon,years.since.treatment, ForTyp, start.deg)%>%
    group_by(StandID)%>%
    filter(MgmtID == Silvi_M )                    ###THIS WILL BE AN INPUT###
  
  ### DATA GROUPING ####
  #calculate metric tons of carbon of Softwood Sawlogs by standID 
  SWSL_byStandID <- data%>% 
    filter(SW_HW =="SW")%>%               #filter softwood
    group_by(StandID)%>%                            #group by stand ID
    summarise(saw_CuFt = sum(sawCuFt), n = n())%>%  #sum the cubic feet of sawlogs by stand ID
    mutate(SWSL_CuM = saw_CuFt * 0.0283168,
           SWSL_MgC = (SWSL_CuM*0.43)*0.48)%>%        #add new column for cubic meters of saw logs
    select(StandID, SWSL_MgC)
  
  
  #calculate metric tons of carbon of Softwood Pulpwood by standID 
  
  SWPW_byStandID <- data%>% 
    filter(SW_HW =="SW")%>%                  #filter softwood
    group_by(StandID)%>%                               #group by stand ID
    summarise(pulp_CuFt = sum(pulpCuFt), n = n())%>%   #sum the cubic feet of sawlogs by stand ID
    mutate(SWPW_CuM = pulp_CuFt * 0.0283168)%>%        #add new column for cubic meters of saw logs
    mutate(SWPW_MgC = (SWPW_CuM*0.43)*0.48)%>%                #convert cubic meters of sawlogs to metric tons of carbon
    select(StandID, SWPW_MgC)
  
  #calculate metric tons of carbon of Hardwood Sawlogs by standID 
  
  HWSL_byStandID <- data%>% 
    filter(SW_HW =="HW")%>%                 #filter softwood
    group_by(StandID)%>%                              #group by stand ID
    summarise(saw_CuFt = sum(sawCuFt), n = n())%>%    #sum the cubic feet of sawlogs by stand ID
    mutate(HWSL_CuM = saw_CuFt * 0.0283168)%>%        #add new column for cubic meters of saw logs
    mutate(HWSL_MgC = (HWSL_CuM*0.43)*0.48)%>%        #convert cubic meters of sawlogs to metric tons of carbon
    select(StandID, HWSL_MgC)
  
  #calculate metric tons of carbon of Hardwood Pulpwood by standID 
  
  HWPW_byStandID <- data%>% 
    filter(SW_HW =="HW")%>%                  #filter softwood
    group_by(StandID)%>%                               #group by stand ID
    summarise(pulp_CuFt = sum(pulpCuFt), n = n())%>%   #sum the cubic feet of sawlogs by stand ID
    mutate(HWPW_CuM = pulp_CuFt * 0.0283168)%>%        #add new column for cubic meters of saw logs
    mutate(HWPW_MgC = (HWPW_CuM*0.43)*0.48)%>%         #convert cubic meters of sawlogs to metric tons of carbon
    select(StandID, HWPW_MgC)
  
  
  #calculate total cubic meters of timber by stand ID
  
  TCM_byStandID <- data%>% 
    group_by(StandID)%>%                            #group by stand ID
    summarise(T..1 = sum(T..1), n = n())%>%         #sum the cubic feet of sawlogs by stand ID
    mutate(TCuM_byStand = T..1 * 0.0283168)%>%      #add new column for cubic meters of saw logs
    select(StandID, TCuM_byStand)
  
  DataByStand <- full_join(SWSL_byStandID, SWPW_byStandID,  by = "StandID")%>%
    full_join(HWSL_byStandID, by = "StandID")%>%
    full_join(HWPW_byStandID, by = "StandID")%>%
    full_join(TCM_byStandID, by = "StandID")
  DataByStand <- DataByStand%>%
    group_by(StandID)%>%
    mutate(MgC_Eng = SWPW_MgC + HWPW_MgC * per_energy,             ###THIS WILL BE AN INPUT###
           MgC_Pap = SWPW_MgC + HWPW_MgC * (1- per_energy),
           H_emiss_MgCO2e = TCuM_byStand * 0.00925,
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
    filter("year_post" <= 35, Region == Region1)
  
  loop_rate_table <- data.frame(year = c(0:35),
                                SWSL_inuse = integer(36),
                                SWPW_inuse = integer(36),
                                HWSL_inuse = integer(36),
                                HWPW_inuse = integer(36),
                                SWSL_Landfill = integer(36),
                                SWPW_Landfill = integer(36),
                                HWSL_Landfill = integer(36),
                                HWPW_Landfill = integer(36)
  )  
  
  #creating loop_rate Table for loop
  for (i in 0:36){
    loop_rate_table$SWSL_inuse[i] <- inuse_rate_func(i,"Softwood","Saw log", Region1)
    loop_rate_table$SWPW_inuse[i] <- inuse_rate_func(i,"Softwood","Pulpwood", Region1)
    loop_rate_table$HWSL_inuse[i] <- inuse_rate_func(i,"Hardwood","Saw log", Region1)
    loop_rate_table$HWPW_inuse[i] <- inuse_rate_func(i,"Hardwood","Pulpwood", Region1)
    
    
    loop_rate_table$SWSL_Landfill[i] <- landfill_rate_func(i,"Softwood","Saw log", Region1)
    loop_rate_table$SWPW_Landfill[i] <- landfill_rate_func(i,"Softwood","Pulpwood", Region1)
    loop_rate_table$HWSL_Landfill[i] <- landfill_rate_func(i,"Hardwood","Saw log", Region1)
    loop_rate_table$HWPW_Landfill[i] <- landfill_rate_func(i,"Hardwood","Pulpwood", Region1)
    
  }
  
  loop_rate_table[is.na(loop_rate_table)] <- 0
  
  CO2_OT_bystand <- rcppForLoop(CO2_OT_bystand, DataByStand, loop_rate_table)
  
  
  
  #build full database by stand
  DataByStand_full <- full_join(DataByStand,CO2_OT_bystand, by = "StandID")
  DataByStand_full <- DataByStand_full%>%
    mutate(Avoided_E_MgCO2e = case_when(year > 0  ~ 0,
                                        TRUE      ~ A_emiss_MgCO2e),
           Harvest_E_MgCO2e = case_when(year > 0  ~ 0,
                                        TRUE      ~ H_emiss_MgCO2e),
           Transport_E_MgCO2e = case_when(year > 0  ~ 0,
                                          TRUE      ~ T_emiss_MgCO2e),
           Manu_E_MgCO2e = case_when(year > 0  ~ 0,
                                     TRUE      ~ M_Emiss_MgCO2e))
  
  Stand_data <- carbon%>%
    left_join(DataByStand_full, by = "StandID")%>%
    group_by(StandID, year)%>%
    mutate(TotalStandCarbon = case_when(year == 0 &  years.since.treatment == 0 ~Total_Stand_Carbon,
                                        year == 5 &  years.since.treatment == 5 ~Total_Stand_Carbon,
                                        year == 10 &  years.since.treatment == 10 ~Total_Stand_Carbon,
                                        year == 15 &  years.since.treatment == 15 ~Total_Stand_Carbon,
                                        year == 20 &  years.since.treatment == 20 ~Total_Stand_Carbon,
                                        year == 25 &  years.since.treatment == 25 ~Total_Stand_Carbon,
                                        year == 30 &  years.since.treatment == 30 ~Total_Stand_Carbon,
                                        year == 35 &  years.since.treatment == 35 ~Total_Stand_Carbon,
                                        TRUE ~ 0))
  
  CO2e_Year <- Stand_data%>%
    group_by(year)%>%
    summarise(mean_inuse = 0-mean(in_use, na.rm = TRUE),
              mean_landfill = 0-mean(landfill,na.rm = TRUE),
              mean_harvest_E = mean(Harvest_E_MgCO2e,na.rm = TRUE), 
              mean_transport_E = mean(Transport_E_MgCO2e,na.rm = TRUE),
              mean_Manufacturing_E = mean(Manu_E_MgCO2e,na.rm = TRUE),
              mean_avoided_E = 0-mean(Avoided_E_MgCO2e ,na.rm = TRUE),
              mean_carbonStocks = 0-mean(TotalStandCarbon * 3.667, na.rm = TRUE)) #converted to CO2e
  
  CO2e_Year <- CO2e_Year%>%
    mutate(CarbonStocksChange = mean_carbonStocks - lag(mean_carbonStocks, n = 5, default = 0))
  
  CO2e_Year <- CO2e_Year%>%
    mutate(CSC_cont = rep((CarbonStocksChange[seq(6, length(CarbonStocksChange), 5)]/ 5), each = 5, length.out = 37))%>%
    mutate(CSC_cont = lag(CSC_cont))%>%
    mutate(CSC_cont = coalesce(CSC_cont,0))
  
  CO2e_Year[1,10] <- CO2e_Year[1,9]
  
  CO2e_Year <- CO2e_Year%>%
    mutate(CSC_sum = cumsum(CSC_cont))%>%
    select(-CSC_cont, -CarbonStocksChange, -mean_carbonStocks)
  
  
  CO2e_Year <-  CO2e_Year %>%
    rowwise() %>%
    mutate(rowsum = sum(c_across(c(mean_inuse, mean_landfill, CSC_sum)))) %>%
    ungroup() %>%
    mutate(rowdiff = rowsum - lag(rowsum),
           NetCO2e = cumsum(c(sum(CO2e_Year[1,2:8]), rowdiff[-1])))
  
  print(CO2e_Year)
}