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
        selectInput("Treelist",
                    "Baseline Model",
                    choices = c("No"=FALSE,"Yes"=TRUE)
        ),
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
                    "Percent of pulp used for energy",
                    min = 0,
                    max = 1,
                    step = .05,
                    value = .5
        ),
        sliderInput("per_eff",
                    "Wood burning effciency",
                    min = 0,
                    max = 1,
                    step = .05,
                    value = .5),
        
        downloadButton("downloadData", "Download")
    ),
    
    mainPanel(
      # Application title
        title = "Carbon Life Cycle Analysis",
      # add a waiting message while app loads
        conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                         tags$div("Please Wait While App is Working", id = "UpdateAnimate", class = "loading dots")),
      
      # main barplot
        uiOutput('dropdown'),
        plotlyOutput("MainPlot"),
      
      # clca table
        div(dataTableOutput("Table"),style = "font-size:80%"),
      
      # css code for the moving elipses of the wait message
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
  # Change max import size
    options(shiny.maxRequestSize=1000*1024^2)

  # create reactive objects to be used in the functions below 
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
            
            CLCA_bySY(CutlistTable(), 
                         CarbE = input$carbonE,
                         Treelist = input$Treelist,
                         Carbonsummary = CSTable(),
                         per_energy = input$percentE,
                         per_ef = input$per_eff,
                         Region1 = input$region)
          })
    

  # use inputs to render bar graph of carbon
    output$dropdown <- renderUI({
      req(CutlistTable())
      selectInput("StandID","Choose Stand",unique(clcatable()$StandID))
    }) 

    output$MainPlot <- renderPlotly({
      if(input$Treelist == FALSE){

        plotData <- clcatable() %>% 
          filter(StandID == input$StandID)
        
        barfig <- plot_ly(plotData, x = ~years, y = ~inuse_MgCO2e, type = "bar", name = "In Use")%>%
          add_trace(y = ~landfill_MgCO2e, name = "landfill")%>%
          add_trace(y = ~Harvest_MgCO2e, name = "Harvest Emissions")%>%
          add_trace(y = ~Transport_MgCO2e, name = "Transport Emissions")%>%
          add_trace(y = ~Manu_MgCO2e, name = "Manufacturing Emissions")%>%
          add_trace(y = ~Avoided_MgCO2e, name = "Avoided Emissions")%>%
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
        
        
        
      
      }
        
    })
    
  # use inputs to render carbon table
    output$Table <- renderDataTable(
      
      clcatable(),
      
      options = list(scrollY = 300)
      
      
    )
    
  
  # use clcatable ractive from above and allow user to download as .csv
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