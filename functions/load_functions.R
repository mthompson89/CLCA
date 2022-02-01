rate_calculator <- read_csv("data/Table_6.csv", col_types = "fffnnnnn")
subset_carbon <- read_csv("data/subset_carbon.csv", col_types = "fff")
colnames(rate_calculator)<- c("Region","Soft_Hard","saw_pulp_all",
                              "year_post","Frac_in_use","Frac_landfil",
                              "Frac_energy","Frac_emit")
sp_lookup <- read_csv("data/species_lookup.csv", col_types = c("Species" = "c"))