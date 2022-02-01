library(shiny)
library(tidyverse)
library(readxl)
library(data.table)
library(gridExtra)
library(plotrix)
library(plotly)
library(Rcpp)
library(vroom)

#Load functions and dataframes
file.paths <- list.files("functions",pattern = "*.R$",full.names = TRUE)
sapply(file.paths,source,.GlobalEnv)


ui <- fluidPage(
    
    # Application title
    title = "Carbon Life Cycle Analysis",
    conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                     tags$div("Please Wait While App is Working", id = "UpdateAnimate", class = "loading dots")),
    plotlyOutput("MainPlot"),
    plotlyOutput("LinePlot"),
    hr(),
    tags$head(tags$style(type="text/css", "
                               .loading {
                                    display: inline-block;
                                    overflow: hidden;
                                    height: 1.3em;
                                    margin-top: -0.3em;
                                    line-height: 1.5em;
                                    vertical-align: text-bottom;
                                    box-sizing: border-box;
                                    }
                               .loading.dots::after {
                                  text-rendering: geometricPrecision;
                                   content: '.\\A..\\A...\\A....';
                                  animation: spin10 2s steps(10) infinite;
                                  animation-duration: 2s;
                                  animation-timing-function: steps(10);
                                  animation-delay: 0s;
                                  animation-iteration-count: infinite;
                                  animation-direction: normal;
                                  animation-fill-mode: none;
                                  animation-play-state: running;
                                  animation-name: spin10;
                              }
                              .loading::after {
                                  display: inline-table;
                                  white-space: pre;
                                  text-align: left;
                              }
                              @keyframes spin10 { to { transform: translateY(-15.0em); } }
                              '
                            ")),
    fluidRow(
        column(3,
               h4("Carbon Life Cycle Analysis"),
               fileInput("cutlist",
                         "Cutlist to Use",
                         accept = c("text/csv", "text/comma-separated-values,text/plain",".csv"),
               ),
               checkboxInput("header",
                             "Header",
                             TRUE
               ),
               downloadButton("downloadData", "Download")),
        column(4,
               selectInput("region",
                           "Region",
                           choices = c("Northeast","North Central","Pacific Northwest; Eastside","Pacific Northwest; Westside",
                                       "Pacific Southwest","Rocky Mountain","South Central","Southeast","West")
               ),
               
               selectInput("SilviM",
                           "Silvicultural Method",
                           choices = c("clearcut","clearcut_plant", "CTR")
               ),
        ),
        column(4,
               sliderInput("percentE",
                           "Percent of pulp used for Energy",
                           min = 0,
                           max = 1,
                           step = .05,
                           value = .5
               ),
               sliderInput("per_eff",
                           "wood burning efficancy",
                           min = 0,
                           max = 1,
                           step = .05,
                           value = .5)
               
        )
    )
)

server <- function(input, output) {
    #Change max import size
    options(shiny.maxRequestSize=1000*1024^2)
    #bring in data that was uploaded and store it as reactive object (this prevents the app from running too slowly)  
    CLCATable <- reactive({
        file <- input$cutlist
        
        ext <- tools::file_ext(file$datapath)
        validate(need(ext == "csv", "please upload a csv file"))
        
        read_csv(file$datapath, col_types = c("StandID" = "f","Species" = "c"))  
    })
    
    
    output$MainPlot <- renderPlotly({
        
        plotCLCA  <- CLCA_bystand(CLCATable(), Silvi_M = input$SilviM, per_energy = input$percentE , per_ef = input$per_eff, Region1 = input$region)
        
        # plotCLCA <- plotCLCA%>%
        #     filter(year <= 35)
        
        barfig3 <- plot_ly(plotCLCA, x = ~year, y = ~mean_inuse, type = "bar", name = "In Use")
        barfig3 <- barfig3%>%
          add_trace(y = ~mean_landfill, name = "landfill")%>%
          add_trace(y = ~mean_harvest_E, name = "Harvest Emissions")%>%
          add_trace(y = ~mean_transport_E, name = "Transport Emissions")%>%
          add_trace(y = ~mean_Manufacturing_E, name = "Manufacturing Emissions")%>%
          add_trace(y = ~mean_avoided_E, name = "Avoided Emissions")%>%
          add_trace(y = ~CSC_sum, name = "Forest Stocks")%>%
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
        
        
    })
    
    output$downloadData <- downloadHandler(
      
      filename = function() {
        
        paste("dataTable","csv", sep=".")
      },
      content = function(file) {
        dataTable <- CLCA_StandOutput(CLCATable(), 
                                               Silvi_M = input$SilviM, per_energy = input$percentE , 
                                               per_ef = input$per_eff, Region1 = input$region)
        write.table(dataTable, file, sep = ",", row.names = FALSE)
      }
    )
    # output$LinePlot <- renderPlotly({
    #     clearcut_CO2e <-  CLCA_bystand_clrc(Cutlist = CLCATable(), per_energy = input$percentE, per_ef = input$per_eff, Region1 = input$region)
    #     clrp_CO2e <- CLCA_bystand_clrp(Cutlist = CLCATable(), per_energy = input$percentE, per_ef = input$per_eff, Region1 = input$region)
    #     CTR_CO2e <- CLCA_bystand_CTR(Cutlist = CLCATable(), per_energy = input$percentE, per_ef = input$per_eff, Region1 = input$region)
    #     
    #     clearcut_CO2e <- clearcut_CO2e %>%
    #         filter(year <= 35)
    #     
    #     clrp_CO2e <- clrp_CO2e %>%
    #         filter(year <= 35)
    #     
    #     CTR_CO2e <- CTR_CO2e %>%
    #         filter(year <= 35)
    #     
    #     barfig2 <- plot_ly(data = clearcut_CO2e, x = ~year, y = ~mean_NetCO2e, type = "scatter", mode = "lines", name = "Clearcut")%>%
    #         add_trace(data = clrp_CO2e,  x = ~year, y = ~mean_NetCO2e, type = "scatter", mode = "lines", name = "Clearcut + Plant")%>%
    #         add_trace(data = CTR_CO2e, x = ~year, y = ~mean_NetCO2e, type = "scatter", mode = "lines", name = "Crop Tree Release")%>%
    #         layout(xaxis = list(title = "Years Since Cut",
    #                             gridcolor = 'rgb(255,255,255)',
    #                             showgrid = TRUE,
    #                             showline = FALSE,
    #                             showticklabels = TRUE,
    #                             tickcolor = 'rgb(127,127,127)',
    #                             ticks = 'outside',
    #                             zeroline = FALSE),
    #                yaxis = list(title = "Mean CO2e (Mg)",
    #                             gridcolor = 'rgb(255,255,255)',
    #                             showgrid = TRUE,
    #                             showline = FALSE,
    #                             showticklabels = TRUE,
    #                             tickcolor = 'rgb(127,127,127)',
    #                             ticks = 'outside',
    #                             zeroline = FALSE), barmode = "group")
    #     
    #     
    #     
    # })
}
# Run the application 
shinyApp(ui = ui, server = server)