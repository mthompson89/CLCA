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
    sidebarLayout(
      
      sidebarPanel(
        fileInput("cutlist",
                  "FVS Cutlist Output",
                  accept = c("text/csv", "text/comma-separated-values,text/plain",".csv"),
        ),
        
        fileInput("carbonsummary",
                  "FVS Carbon Output",
                  accept = c("text/csv", "text/comma-separated-values,text/plain",".csv"),
        ),
        checkboxInput("carbonE",
                      "FVS_Carbon_East?",
                      TRUE
        ),
        selectInput("region",
                    "Region",
                    choices = c("Northeast","North Central","Pacific Northwest; Eastside","Pacific Northwest; Westside",
                                "Pacific Southwest","Rocky Mountain","South Central","Southeast","West")
        ),
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
                    value = .5),
        
        downloadButton("downloadData", "Download")
    ),
    
    mainPanel(
      # Application title
      title = "Carbon Life Cycle Analysis",
      conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                       tags$div("Please Wait While App is Working", id = "UpdateAnimate", class = "loading dots")),
      plotlyOutput("MainPlot"),
      div(dataTableOutput("Table"),style = "font-size:80%"),
      hr(),
      # css code for the moving elipses while it loads
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
                              "))

               
        )
    )
)

server <- function(input, output) {
    #Change max import size
    options(shiny.maxRequestSize=1000*1024^2)
  
    #bring in data that was uploaded and store it as reactive object (this prevents the app from running too slowly)  
          CutlistTable <- reactive({
              file <- input$cutlist
              
              ext <- tools::file_ext(file$datapath)
              validate(need(ext == "csv", "please upload a csv file"))
              
              read_csv(file$datapath, col_types = c("StandID" = "f","Species" = "c"))  
          })
          
          CSTable <- reactive({
            file <- input$carbonsummary
            
            ext <- tools::file_ext(file$datapath)
            validate(need(ext == "csv", "please upload a csv file"))
            
            read_csv(file$datapath, col_types = c("StandID" = "f"))  
          })
          
          clcatable <- reactive({
            
            CLCA_bystand(CutlistTable(), 
                         CarbE = input$carbonE,
                         Carbonsummary = CSTable(),
                         per_energy = input$percentE,
                         per_ef = input$per_eff,
                         Region1 = input$region)
          })
    
  # use inputs to render carbon table
   output$Table <- renderDataTable(
     
     CLCA_bystand(CutlistTable(), 
                 CarbE = input$carbonE,
                 Carbonsummary = CSTable(),
                 per_energy = input$percentE,
                 per_ef = input$per_eff,
                 Region1 = input$region),
     
     options = list(scrollY = 300)
     
     
   )
   
  # use inputs to render bar graph of carbon
    output$MainPlot <- renderPlotly({
        
        plotCLCA  <- CLCA_bystand(CutlistTable(), 
                                  CarbE = input$carbonE,
                                  Carbonsummary = CSTable(),
                                  per_energy = input$percentE,
                                  per_ef = input$per_eff,
                                  Region1 = input$region)
        
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
        
        paste("clca_data","csv", sep=".")
      },
      content = function(file) {

        write.table(clcatable(), file, sep = ",", row.names = FALSE)
      }
    )
    
}
# Run the application 
shinyApp(ui = ui, server = server)