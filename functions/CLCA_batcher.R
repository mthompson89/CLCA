source('functions/non-AppCLCA.R')
library(dbplyr)


fortyp_vec  <- c('Pine','SpFr','HrWd')

Rx_vec <- c('CTRH','CTRL','TSIH','TSIL','ShWD','ParH','HvyH','LitG')


for(i in fortyp_vec){
  for(j in Rx_vec){
    #create path to file
    path <- paste("Z:/FVS_OutputFiles/RunOutputDb Files/",i,"_",j,".db",sep = "")
    #establish connection
    con <- DBI::dbConnect(RSQLite::SQLite(),dbname = path)
    #pull carbon file from .db
    Carbon <- as.data.frame(tbl(con,"FVS_Carbon") %>% collect()) %>% mutate(StandID = as.factor(StandID))
    #pull cutlist file from .db
    CList <- as.data.frame(tbl(con, "FVS_Cutlist_East") %>% collect()) %>%  mutate(StandID = as.factor(StandID))
    #create output object name
    obj <- paste(i,'_',j,'_','CLCATable.csv',sep = "")
    #creat my output object path
    outputpath <- paste("Z:/FVS_OutputFiles/CLCAoutputs/",obj,sep = "")
    #run my CLCA calcluator
    if(j == "LitG"){
      CLCATable <- non_appCLCA(Cutlist = CList,
                             Carbonsummary = Carbon,
                             Treelist = TRUE) %>% 
        mutate(fortyp = i,
               MgmntID = j)
    }else{
      CLCATable <- non_appCLCA(Cutlist = CList,
                               Carbonsummary = Carbon)%>% 
        mutate(fortyp = i,
               MgmntID = j)
    }
    #write the clcatable to a new folder
    write.csv(CLCATable,outputpath)
    print(paste("finished",i,j))
    #disconnect from db to establish a new one on next loop
    DBI::dbDisconnect(con)
    
 }
}


files <- c("HrWd_CTRH_CLCATable.csv", "HrWd_CTRL_CLCATable.csv", "HrWd_HvyH_CLCATable.csv", "HrWd_TSIL_CLCATable.csv",
"HrWd_ParH_CLCATable.csv", "HrWd_ShWD_CLCATable.csv", "HrWd_TSIH_CLCATable.csv", "Pine_TSIL_CLCATable.csv",
"Pine_CTRH_CLCATable.csv", "Pine_CTRL_CLCATable.csv", "Pine_HvyH_CLCATable.csv", "SpFr_TSIL_CLCATable.csv",
"Pine_ParH_CLCATable.csv", "Pine_ShWD_CLCATable.csv", "Pine_TSIH_CLCATable.csv", "SpFr_TSIH_CLCATable.csv",
"SpFr_CTRH_CLCATable.csv", "SpFr_CTRL_CLCATable.csv", "SpFr_HvyH_CLCATable.csv", "SpFr_ShWD_CLCATable.csv",  
"SpFr_ParH_CLCATable.csv")

files2 <- c("Pine_LitG_CLCATable.csv","SpFr_LitG_CLCATable.csv","HrWd_LitG_CLCATable.csv")

carbontable <- foreach(i = 1:21, .combine = 'rbind') %do% {
  path = paste("Z:/FVS_OutputFiles/CLCAoutputs/",files[i],sep = "")
  read_csv(path) %>% select(-1)
}

carbontable2 <- foreach(i = 1:3, .combine = 'rbind') %do% {
  path = paste("Z:/FVS_OutputFiles/CLCAoutputs/",files2[i],sep = "")
  read_csv(path) %>% select(-1)
}


carbontable2 <- carbontable2 %>% 
  mutate(inuse_MgCO2e = 0,
         years = Year,
         landfill_MgCO2e = 0,
         Avoided_MgCO2e = 0,
         Transport_MgCO2e = 0,
         Manu_MgCO2e = 0,
         Harvest_MgCO2e = 0,
         NetCO2e = ForestStocks) %>% 
  select(StandID, years,inuse_MgCO2e,landfill_MgCO2e,Avoided_MgCO2e,Harvest_MgCO2e,Transport_MgCO2e,Manu_MgCO2e,ForestStocks,NetCO2e,fortyp,MgmntID)

CLCATable_Full <- rbind(carbontable,carbontable2)
write.csv(CLCATable_Full,'data/CLCATable_full.csv')
