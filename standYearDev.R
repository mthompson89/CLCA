
df1 <- data.frame(ID = c("154","154", "155", "155", "156", "156", "157",
                        "157", "158", "158", "159", "159", "160", "160",
                        "162", "163", "163", "163", "164", "164"),
                 year = c(0, 5,  0, 5, 0,
                          5, 0,  5, 0, 5,
                          0, 5,  0, 5, 5,
                          0, 5, 45, 0, 5),
                 value = c(0.25, 3.43, 0.11, 3.37,
                           0.00, 0.27, 1.25, 3.91,
                           0.42, 2.14, 0.02, 0.80,
                           0.00, 0.19, 2.77, 0.58,
                           1.77, 0.07, 0.35, 4.17))


df2 <- DateByStand_yr %>% 
  as_tibble() %>% 
  complete(StandID,years = 0:45, fill = list(value = 0)) %>% 
  replace(is.na(.),0) %>% 
  distinct()

View(df2 %>% filter(StandID == "0033198304000200300163"))


df3 <- data.frame(ID = rep(unique(df1$ID),each = 51),
                 year = rep(0:50,times = length(unique(df1$ID))))%>% 
  mutate(newvalue = case_when(year == df1$year & ID == df1$ID ~ df1$value,
                              TRUE ~ 0))

  
desireddf <- data.frame(ID = rep("154",times = 51),
                        year = c(0:50),
                        value = c(0.25,0,0,0,0,3.43,rep(0,time = 45)))


df <- tibble(
  group = c(1:2, 1),
  item_id = c(1:2, 2),
  item_name = c("a", "b", "b"),
  value1 = 1:3,
  value2 = 4:6
)
df %>% complete(group, nesting(item_id, item_name))

# You can also choose to fill in missing values
df %>% complete(group, nesting(item_id, item_name), fill = list(value1 = 0))


plot_ly(CO2e_Year, x = ~year, y = ~mean_inuse, type = "bar", name = "In Use")%>%
  add_trace(y = ~mean_landfill, name = "landfill")%>%
  add_trace(y = ~mean_harvest_E, name = "Harvest Emissions")%>%
  add_trace(y = ~mean_transport_E, name = "Transport Emissions")%>%
  add_trace(y = ~mean_Manufacturing_E, name = "Manufacturing Emissions")%>%
  add_trace(y = ~mean_avoided_E, name = "Avoided Emissions")%>%
  add_trace(y = ~ForestStocks, name = "Forest Stocks")%>%
  add_trace(y = ~NetCO2e, type = "scatter", mode= "lines",
            line = list(color = "red"),
            name = "MeanNetCO2")%>%
  layout(barmode = "relative",  paper_bgcolor='rgb(255,255,255)', plot_bgcolor='rgb(229,229,229)',
         xaxis = list(title = "Years Since Cut",
                      gridcolor = 'rgb(255,255,255)',
                      showgrid = TRUE,
                      showline = FALSE,
                      showticklabels = TRUE,
                      tickcolor = 'rgb(127,127,127)',
                      ticks = 'outside',
                      zeroline = FALSE),
         yaxis = list(title = "Mean CO2e (Mg)",
                      gridcolor = 'rgb(255,255,255)',
                      showgrid = TRUE,
                      showline = FALSE,
                      showticklabels = TRUE,
                      tickcolor = 'rgb(127,127,127)',
                      ticks = 'outside',
                      zeroline = FALSE), barmode = "group")
