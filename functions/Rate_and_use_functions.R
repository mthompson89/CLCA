landfill_rate_func <- function(year,wood,product, R){
  rate_calculator%>%
    filter(Region == R, Soft_Hard == wood, saw_pulp_all == product | saw_pulp_all == "All", year_post == (year))%>%
    select( Frac_landfil)%>%
    as.numeric()
}

inuse_rate_func <- function(year,wood,product, R){
  rate_calculator%>%
    filter(Region == R, Soft_Hard == wood, saw_pulp_all == product | saw_pulp_all == "All", year_post == (year))%>%
    select(Frac_in_use)%>%
    as.numeric()
}

cppFunction(
  'DataFrame rcppForLoop(DataFrame CO2, DataFrame Data, DataFrame Loop) {
  //creating vectors for co2dataframe
NumericVector CO2a = CO2["year"];
NumericVector CO2b = CO2["in_use"];
NumericVector CO2c = CO2["landfill"];
   
   //creating vectors for input data
NumericVector Dataa = Data["SWSL_MgC"];
NumericVector Datab = Data["SWPW_MgC"];
NumericVector Datac = Data["HWSL_MgC"];
NumericVector Datad = Data["HWPW_MgC"];  
  
  //creating vectors for inuse Loop table
NumericVector LoopUa = Loop["SWSL_inuse"];
NumericVector LoopUb = Loop["SWPW_inuse"];
NumericVector LoopUc = Loop["HWSL_inuse"];
NumericVector LoopUd = Loop["HWPW_inuse"];


  //creating vectors for landfill Loop table
NumericVector LoopLa = Loop["SWSL_Landfill"];
NumericVector LoopLb = Loop["SWPW_Landfill"];
NumericVector LoopLc = Loop["HWSL_Landfill"];
NumericVector LoopLd = Loop["HWPW_Landfill"];


int x = Data.nrows();

 
  for (int stand = 0; stand < x; stand++){
      
      for (int year = 0; year < 36; year++){
      
        CO2a[stand*36+year] = year;
        
        CO2b[stand*36+year] = (Dataa[stand] * LoopUa[year]) + (Datab[stand] * LoopUb[year]) + 
                              (Datac[stand] * LoopUc[year]) + (Datad[stand] * LoopUd[year]);
                     
        CO2c[stand*36+year] = (Dataa[stand] * LoopLa[year]) + (Datab[stand] * LoopLb[year]) +
                              (Datac[stand] * LoopLc[year]) + (Datad[stand] * LoopLd[year]);
      }
  }

  return CO2;
}')