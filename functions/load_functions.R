rate_calculator <- read_csv("data/Table_6.csv", col_types = "fffnnnnn")
subset_carbon <- read_csv("data/subset_carbon.csv", col_types = "fff")
colnames(rate_calculator)<- c("Region","Soft_Hard","saw_pulp_all",
                              "year_post","Frac_in_use","Frac_landfil",
                              "Frac_energy","Frac_emit")
sp_lookup <- read_csv("data/species_lookup.csv", col_types = c("Species" = "c"))

landfill_rate_func <- function(year,wood,product, R){
  rate_calculator%>%
    mutate(saw_pulp = case_when(saw_pulp_all == "All"~ "Saw log",
                                saw_pulp_all == "Saw log" ~ "Saw log",
                                saw_pulp_all == "Pulpwood" ~ "Pulpwood"))%>%
    filter(Region == R, Soft_Hard == wood, saw_pulp == product, year_post == (year))%>%
    select( Frac_landfil)%>%
    as.numeric()
  # | saw_pulp_all == "All"
}

inuse_rate_func <- function(year,wood,product, R){
  rate_calculator%>%
    mutate(saw_pulp = case_when(saw_pulp_all == "All"~ "Saw log",
                                saw_pulp_all == "Saw log" ~ "Saw log",
                                saw_pulp_all == "Pulpwood" ~ "Pulpwood"))%>%
    filter(Region == R, Soft_Hard == wood, saw_pulp == product , year_post == (year))%>%
    select(Frac_in_use)%>%
    as.numeric()
  # | saw_pulp_all == "All"
}

## c++ for loop for rate for stand+year calculator ----------------
cppFunction(
  'DataFrame rcppForLoop(DataFrame CO2, DataFrame Data, DataFrame Loop, int numStands) {
    
  //creating vectors for co2dataframe
NumericVector CO2U = CO2["in_use"];
NumericVector CO2L = CO2["landfill"];
   
   //creating vectors for input data. 
   //DataFrame is telling C++ the type of object Data will be
   //Below, this is saying Dataa is going to be a numeric vector
   //and it is going to be the column labled "SWSL_MgC" from Data.
   
NumericVector DBS_SWSL_MgC = Data["SWSL_MgC"];
NumericVector DBS_SWPW_MgC = Data["SWPW_MgC"];
NumericVector DBS_HWSL_MgC = Data["HWSL_MgC"];
NumericVector DBS_HWPW_MgC = Data["HWPW_MgC"];  
  
  //creating vectors for inuse Loop table
NumericVector LRT_SWSL_inuse = Loop["SWSL_inuse"];
NumericVector LRT_SWPW_inuse = Loop["SWPW_inuse"];
NumericVector LRT_HWSL_inuse = Loop["HWSL_inuse"];
NumericVector LRT_HWPW_inuse = Loop["HWPW_inuse"];



  //creating vectors for landfill Loop table
NumericVector LRT_SWSL_Landfill = Loop["SWSL_Landfill"];
NumericVector LRT_SWPW_Landfill = Loop["SWPW_Landfill"];
NumericVector LRT_HWSL_Landfill = Loop["HWSL_Landfill"];
NumericVector LRT_HWPW_Landfill = Loop["HWPW_Landfill"];


int x = numStands;
int y = 47;
  //instead of calling it outer loop object i I am calling it stand but it is the number of rows
  //in Dataa. so start stand at 0, and go until you hit x (n rows in Data)
  //and increase stand by 1 (stand++)
 
 

  for (int i = 0; i < x; i++){

      for (int j = 0; j < y; j++){


        CO2U[i*y+j] = (DBS_SWSL_MgC[i] * LRT_SWSL_inuse[j])    + (DBS_SWPW_MgC[i] * LRT_SWPW_inuse[j]) +
                      (DBS_HWSL_MgC[i] * LRT_HWSL_inuse[j])    + (DBS_HWPW_MgC[i] * LRT_HWPW_inuse[j]);

        CO2L[i*y+j] = (DBS_SWSL_MgC[i] * LRT_SWSL_Landfill[j]) + (DBS_SWPW_MgC[i] * LRT_SWPW_Landfill[j]) +
                      (DBS_HWSL_MgC[i] * LRT_HWSL_Landfill[j]) + (DBS_HWPW_MgC[i] * LRT_HWPW_Landfill[j]);
      }
  }

  return CO2;
}')

## c++ for loop for rate for onlystand calculator ----------------
cppFunction(
  'DataFrame rcppForLoopStand(DataFrame CO2, DataFrame Data, DataFrame Loop,int numStands) {
    
  //creating vectors for co2dataframe
NumericVector CO2U = CO2["in_use"];
NumericVector CO2L = CO2["landfill"];
   
   //creating vectors for input data. 
   //DataFrame is telling C++ the type of object Data will be
   //Below, this is saying Dataa is going to be a numeric vector
   //and it is going to be the column labled "SWSL_MgC" from Data.
   
NumericVector DBS_SWSL_MgC = Data["SWSL_MgC"];
NumericVector DBS_SWPW_MgC = Data["SWPW_MgC"];
NumericVector DBS_HWSL_MgC = Data["HWSL_MgC"];
NumericVector DBS_HWPW_MgC = Data["HWPW_MgC"];  
  
  //creating vectors for inuse Loop table
NumericVector LRT_SWSL_inuse = Loop["SWSL_inuse"];
NumericVector LRT_SWPW_inuse = Loop["SWPW_inuse"];
NumericVector LRT_HWSL_inuse = Loop["HWSL_inuse"];
NumericVector LRT_HWPW_inuse = Loop["HWPW_inuse"];



  //creating vectors for landfill Loop table
NumericVector LRT_SWSL_Landfill = Loop["SWSL_Landfill"];
NumericVector LRT_SWPW_Landfill = Loop["SWPW_Landfill"];
NumericVector LRT_HWSL_Landfill = Loop["HWSL_Landfill"];
NumericVector LRT_HWPW_Landfill = Loop["HWPW_Landfill"];


int x = numStands;
  //instead of calling it outer loop object i I am calling it stand but it is the number of rows
  //in Dataa. so start stand at 0, and go until you hit x (n rows in Data)
  //and increase stand by 1 (stand++)
 
 

  for (int i = 0; i < x; i++){

      for (int j = 0; j < 41; j++){


        CO2U[i*46+j] = (DBS_SWSL_MgC[i] * LRT_SWSL_inuse[j])    + (DBS_SWPW_MgC[i] * LRT_SWPW_inuse[j]) +
                      (DBS_HWSL_MgC[i] * LRT_HWSL_inuse[j])    + (DBS_HWPW_MgC[i] * LRT_HWPW_inuse[j]);

        CO2L[i*46+j] = (DBS_SWSL_MgC[i] * LRT_SWSL_Landfill[j]) + (DBS_SWPW_MgC[i] * LRT_SWPW_Landfill[j]) +
                      (DBS_HWSL_MgC[i] * LRT_HWSL_Landfill[j]) + (DBS_HWPW_MgC[i] * LRT_HWPW_Landfill[j]);
      }
  }

  return CO2;
}')
