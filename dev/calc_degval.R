library(tidyverse)
library(data.table)
library(tools)

#load data
treelist_filenames <- list.files(path = "Z:/EDrive/FVS/CLCA/data/UpdatedFVSOutputs", pattern = "TreeList", recursive = TRUE)
summarytable_filenames <- list.files(path = "Z:/EDrive/FVS/CLCA/data/UpdatedFVSOutputs", pattern = "Summary", recursive = TRUE)
Treesp <- fread("data/Sp_data.csv")
pb <- txtProgressBar(min = 0,      # Minimum value of the progress bar
                     max = length(treelist_filenames)*8, # Maximum value of the progress bar
                     style = 3,    # Progress bar style (also available style = 1 and style = 2)
                     width = 50,   # Progress bar width. Defaults to getOption("width")
                     char = "=")   # Character used to create the bar
# fwrite(Treelist, file = fname)

# m_files <- c("CTRHigh/NHCTRHigh_Summary5Yr.csv","LetItGrow/LetItGrow_Summary5Yr.csv","Shelterwood/NHShelterwood_Summary5Yr.csv")
# 
# summarytable_filenames <- summarytable_filenames[! summarytable_filenames %in% m_files]
for( i in 1:length(treelist_filenames)){
  Treelist <- fread(paste("Z:/EDrive/FVS/CLCA/data/UpdatedFVSOutputs/",treelist_filenames[i],sep = ""))
  summary <- fread(paste("Z:/EDrive/FVS/CLCA/data/UpdatedFVSOutputs/",summarytable_filenames[i],sep = ""))
  setTxtProgressBar(pb,value = (i-1)*8+1)

  Treelist <- merge(Treelist,Treesp[,c("splist","sg","desirability")], by.x = "SpeciesFIA", by.y = "splist" , all.x=T)
  setTxtProgressBar(pb,value = (i-1)*8+2)

  Treelist <- Treelist[,':='(RD = (TPA/0.404686)*(0.00015+0.00218*sg)*((DBH*2.54)/25)^1.6,
                             prime_AGS = desirability == 1  & TreeVal != 3,
                             sec_AGS = desirability == 2 & TreeVal != 3,
                             tert_AGS = desirability == 3 & TreeVal != 3,
                             UGS = TreeVal == 3)]
  setTxtProgressBar(pb,value = (i-1)*8+3)

  standSummaries <- Treelist[,.(PrimeRD = sum(ifelse(prime_AGS == TRUE,RD,0),na.rm = T),
                                SecRD = sum(ifelse(sec_AGS == TRUE,RD,0),na.rm = T),
                                TertRD = sum(ifelse(tert_AGS == TRUE,RD,0),na.rm = T),
                                UGSRD = sum(ifelse(UGS == TRUE,RD,0),na.rm = T)), by = .(StandID,Year)]
  setTxtProgressBar(pb,value = (i-1)*8+4)

  standSummaries <- standSummaries[, `:=` (Deg = case_when(PrimeRD >= 0.4 ~ 1,
                                                           PrimeRD + SecRD >= 0.4~ 2,
                                                           PrimeRD + SecRD + TertRD >= 0.4 ~ 3,
                                                           PrimeRD + SecRD + TertRD + UGSRD >= 0.4 ~4,
                                                           TRUE ~ 5)), by = .(StandID,Year)]
  setTxtProgressBar(pb,value = (i-1)*8+5)

  Treelist_summary <- merge(summary,standSummaries[,c('StandID','Year','Deg')], by = c('StandID','Year'), all.x = T)
  setTxtProgressBar(pb,value = (i-1)*8+6)

  fname <- paste(file_path_sans_ext(basename(summarytable_filenames[i])),"_w_deg.csv",sep = "")
  setTxtProgressBar(pb,value = (i-1)*8+7)

  fwrite(Treelist_summary, file = fname)
  setTxtProgressBar(pb,value = (i-1)*8+8)

  rm(Treelist,standSummaries,Treelist_summary,summary)

}

