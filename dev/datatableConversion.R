library(data.table)

tsc.s.dt <- data.table(mean_TSC)
dbs.f.dt <- data.table(DataByStand_full)


tsc.s.dt[years == 5,mean_carbonstocks]
co2.dt <- dbs.f.dt[, .(mean_inuse = 0 - mean(in_use *3.667),
                       mean_landfill = 0 - mean(landfill*3.667),
                       mean_harvest_E = mean(Harvest_E_MgCO2e),
                       mean_transport_E = mean(Transport_E_MgCO2e),
                       mean_manu_E = mean(Manu_E_MgCO2e),
                       mean_avoid_E = 0 - mean(Avoided_E_MgCO2e),
                       mean_carbonstocks = fcase(years == 1, tsc.s.dt[years == 0,  mean_carbonstocks],
                                                 years == 6 , tsc.s.dt[years == 5,  mean_carbonstocks],
                                                 years == 11, tsc.s.dt[years == 10, mean_carbonstocks],
                                                 years == 16, tsc.s.dt[years == 15, mean_carbonstocks],
                                                 years == 21, tsc.s.dt[years == 20, mean_carbonstocks],
                                                 years == 26, tsc.s.dt[years == 25, mean_carbonstocks],
                                                 years == 31, tsc.s.dt[years == 30, mean_carbonstocks],
                                                 years == 36, tsc.s.dt[years == 35, mean_carbonstocks],
                                                 years == 41, tsc.s.dt[years == 40, mean_carbonstocks],
                                                 years == 46, tsc.s.dt[years == 45, mean_carbonstocks],
                                                 default = 0)), by = years]



system.time({
mean_TSC_stand <- fvs_carbon[,c("StandID","years","Total_Stand_Carbon")]%>%
  group_by(StandID,years)%>%
  summarise(mean_carbonstocks = 0-mean(Total_Stand_Carbon*3.667, na.rm = TRUE))
})

fvsc.DT <- data.table(fvs_carbon[,c("StandID","years","Total_Stand_Carbon")])

system.time({
tsc.sy.dt <- fvsc.DT[,mcs := 0-mean(Total_Stand_Carbon*3.667), by = .(StandID,years)]
})

system.time({
co2.dt <- dbs.f.dt[, .(mean_inuse = 0 - mean(in_use *3.667),
                       mean_landfill = 0 - mean(landfill*3.667),
                       mean_harvest_E = mean(Harvest_E_MgCO2e),
                       mean_transport_E = mean(Transport_E_MgCO2e),
                       mean_manu_E = mean(Manu_E_MgCO2e),
                       mean_avoid_E = 0 - mean(Avoided_E_MgCO2e),
                       mean_carbonstocks = fcase(years == 1 ,  tsc.sy.dt[years == 0,  mcs, by = .(StandID)],
                                                 years == 6 , tsc.sy.dt[years == 5 ,  mcs, by = .(StandID)],
                                                 years == 11, tsc.sy.dt[years == 10, mcs, by = .(StandID)],
                                                 years == 16, tsc.sy.dt[years == 15, mcs, by = .(StandID)],
                                                 years == 21, tsc.sy.dt[years == 20, mcs, by = .(StandID)],
                                                 years == 26, tsc.sy.dt[years == 25, mcs, by = .(StandID)],
                                                 years == 31, tsc.sy.dt[years == 30, mcs, by = .(StandID)],
                                                 years == 36, tsc.sy.dt[years == 35, mcs, by = .(StandID)],
                                                 years == 41, tsc.sy.dt[years == 40, mcs, by = .(StandID)],
                                                 years == 46, tsc.sy.dt[years == 45, mcs, by = .(StandID)]
                                                 )), by = .(StandID,years)]
})

dbs.f.dt[,mean_carbonstocks = fcase(years == 1L , tsc.s.dt[years == 0L,  mean_carbonstocks],
                                    years == 6L , tsc.s.dt[years == 5L,  mean_carbonstocks],
                                    years == 11L, tsc.s.dt[years == 10L, mean_carbonstocks],
                                    years == 16L, tsc.s.dt[years == 15L, mean_carbonstocks],
                                    years == 21L, tsc.s.dt[years == 20L, mean_carbonstocks],
                                    years == 26L, tsc.s.dt[years == 25L, mean_carbonstocks],
                                    years == 31L, tsc.s.dt[years == 30L, mean_carbonstocks],
                                    years == 36L, tsc.s.dt[years == 35L, mean_carbonstocks],
                                    years == 41L, tsc.s.dt[years == 40L, mean_carbonstocks],
                                    years == 46L, tsc.s.dt[years == 45L, mean_carbonstocks]), by = .(years)]



dt.year=data.table(data.frame(ID = rep(LETTERS[1:3],each = 20),
                      years = rep(1:20,times = 3)       ))

dt.value=data.table(data.frame(ID = rep(LETTERS[1:3],each = 20),
                      years = rep(0:19,times = 3),
                      value = sample(c(150:250),size = 60)))

dt.year[,value = fcase(years == 1L , tsc.s.dt[years == 0L,  mean_carbonstocks]
                       years == 6L , tsc.s.dt[years == 5L,  mean_carbonstocks]
                       years == 11L, tsc.s.dt[years == 10L, mean_carbonstocks]
                       years == 16L, tsc.s.dt[years == 15L, mean_carbonstocks]
                       years == 21L, tsc.s.dt[years == 20L, mean_carbonstocks]), by = .(ID,years)]


CO2e_sy <- DataByStand_full%>%
  group_by(StandID,years)%>%
  summarise(mean_inuse = 0-mean(in_use * 3.667, na.rm = TRUE),
            mean_landfill = 0-mean(landfill * 3.667,na.rm = TRUE),
            mean_harvest_E = mean(Harvest_E_MgCO2e,na.rm = TRUE),
            mean_transport_E = mean(Transport_E_MgCO2e,na.rm = TRUE),
            mean_Manufacturing_E = mean(Manu_E_MgCO2e,na.rm = TRUE),
            mean_avoided_E = 0-mean(Avoided_E_MgCO2e ,na.rm = TRUE),
            mean_carbonStocks = case_when(years ==  1  ~ mean_TSC_stand$mean_carbonstocks[which(mean_TSC_stand$years == 0 )],
                                          years ==  6  ~ mean_TSC_stand$mean_carbonstocks[which(mean_TSC_stand$years == 5 )],
                                          years ==  11 ~ mean_TSC_stand$mean_carbonstocks[which(mean_TSC_stand$years == 10)],
                                          years ==  16 ~ mean_TSC_stand$mean_carbonstocks[which(mean_TSC_stand$years == 15)],
                                          years ==  21 ~ mean_TSC_stand$mean_carbonstocks[which(mean_TSC_stand$years == 20)],
                                          years ==  26 ~ mean_TSC_stand$mean_carbonstocks[which(mean_TSC_stand$years == 25)],
                                          years ==  31 ~ mean_TSC_stand$mean_carbonstocks[which(mean_TSC_stand$years == 30)],
                                          years ==  36 ~ mean_TSC_stand$mean_carbonstocks[which(mean_TSC_stand$years == 35)],
                                          years ==  41 ~ mean_TSC_stand$mean_carbonstocks[which(mean_TSC_stand$years == 40)],
                                          years ==  46 ~ mean_TSC_stand$mean_carbonstocks[which(mean_TSC_stand$years == 45)],
                                          TRUE ~ 0))%>%
  unique()%>%
  ungroup()


#10 year increment method
# CO2e_Year <- DataByStand_full%>%
#   group_by(year)%>%
#   summarise(mean_inuse = 0-mean(in_use * 3.667, na.rm = TRUE),
#             mean_landfill = 0-mean(landfill * 3.667,na.rm = TRUE),
#             mean_harvest_E = mean(Harvest_E_MgCO2e,na.rm = TRUE), 
#             mean_transport_E = mean(Transport_E_MgCO2e,na.rm = TRUE),
#             mean_Manufacturing_E = mean(Manu_E_MgCO2e,na.rm = TRUE),
#             mean_avoided_E = 0-mean(Avoided_E_MgCO2e ,na.rm = TRUE),
#             mean_carbonStocks = case_when(year == 0  ~ mean_TSC$mean_carbonstocks[which(mean_TSC$years ==  0 )],
#                                           year == 10 ~ mean_TSC$mean_carbonstocks[which(mean_TSC$years ==  10)],
#                                           year == 20 ~ mean_TSC$mean_carbonstocks[which(mean_TSC$years ==  20)],
#                                           year == 30 ~ mean_TSC$mean_carbonstocks[which(mean_TSC$years ==  30)],
#                                           year == 40 ~ mean_TSC$mean_carbonstocks[which(mean_TSC$years ==  40)],
#                                           year == 50 ~ mean_TSC$mean_carbonstocks[which(mean_TSC$years ==  50)],
#                                           TRUE ~ 0))%>%
# unique()%>%
# ungroup()


CO2e_sy <- as.data.frame(CO2e_sy)%>%
  #finds diff between change in carbon after 5 years
  # mutate(CarbonStocksChange = mean_carbonStocks - lag(mean_carbonStocks, n = 5, default = 0))
  mutate(CarbonStocksChange = mean_carbonStocks - lag(mean_carbonStocks, n = 5, default = 0)) 


CO2e_sy <- CO2e_sy%>%
  mutate(CSC_cont = rep((CarbonStocksChange[seq(from = 7, to = length(CarbonStocksChange), 5)]/ 5),
                        each = 5, length.out = 47))%>% #divides diff by 5 and repeats
  mutate(CSC_cont = lag(CSC_cont,1))%>% #lags by one row to offset from year 0
  mutate(CSC_cont = coalesce(CSC_cont,0)) # fills row 1 with 0

CO2e_sy[c(1),10] <- CO2e_sy[c(2),9] # fill in the first row with carbonstockschange year 0

CO2e_sy <- CO2e_sy%>%
  mutate(ForestStocks  = cumsum(CSC_cont))%>%
  mutate(storesum = rowSums(.[c(2,3,11)]),
         emishsum = cumsum(rowSums(.[4:7])))%>%
  mutate(NetCO2e  = rowSums(.[12:13]))
# CO2e_Year[1,11] <- CO2e_Year[1,9]

CO2e_sy <- CO2e_sy%>%
  mutate(year = c(-1:(nrow(CO2e_sy)-2)))