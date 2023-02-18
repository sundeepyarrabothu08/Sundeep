
#Shiny Web application of EDA data visualization tool.
#Author:D.siva Kumari
# Start of the shiny app


# Load all libraries     
    #setwd()
   # library(shiny)
    library(tools)
    library(shinydashboard)
    library(shinyWidgets)
    library(shinyjs)
    library(bs4Dash)
    library(dplyr)
    library(ggplot2)

# Source files of modules
    source("R/upload_dataset_module.R")
    source("R/plots_module.R")

#UI Starts
    
    ui <- fluidPage(
        tags$div(class = "header_class",
                 tags$h1("EDA Data Visualization Tool")),
        tags$hr(),
        includeCSS("www/main.css"),  # Include the css file
        
        useBs4Dash(), # use bs4 to have a single page layout
        
        useShinyjs(), # js to enable and disable the buttons
        
    # UI of the upload dataset fileinput widget
        
        fluidPage(
            upload_dataset_UI("upload_dataset_label")
        ), # End of the fluid row
        
    # Start of the plots UI
       fluidPage(
           plots_module_UI("plot_module_label")
       )
        
        
    ) ## End of the UI 
    
    # Start of the server
    
    server <- function(input, output, session) {
        
     # calling Upload dataset module server function  
      
    inputdataset<- upload_dataset_Server("upload_dataset_label")
    
      #  calling plots module server function
      
     plots_module_Server("plot_module_label",dataset=inputdataset$inpu_dataset,input_value=inputdataset$radio_btn)
   
    }  # End of the server function
    
shinyApp(ui, server)
    
 # End of the shiny application
