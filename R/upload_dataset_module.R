# Upload dataset UI function starts

upload_dataset_UI<-function(id){
  ns<-NS(id)
  fluidRow(
  column(
    width = 3,
    fluidRow(
      bs4Dash::box(
        title = tags$strong("Please Upload Dataset:"),
        fileInput(inputId = ns("upload_dataset"), label = "csv format only"),
        width = 12,
        collapsible = FALSE
      )
    ),
    fluidRow(
      tooltip(
      actionButton(inputId = ns("accept_data_button"), "Data Upload", class = "button_class"),
      title = "Click on the button to see the plot selection",placement = "top"
      )
    ),
    tags$br(),
  uiOutput(ns("plot_type_ui"))  
    
    
  ),  # End of the upload dataset box
  
  # Start of the tab box of dataset view and other details
  column(
    width = 9,
    bs4Dash::tabBox(
      width = 12,
      id = "dataset_box",
      title = "Details of the Uploaded Data Set",
      selected = "DataSet View",
      status = "purple",
      type = "tabs",
      tabPanel(
        title = "DataSet View",
        bs4Dash::box(
          title = tags$strong("DataSet View"),
          width = 12,
          verbatimTextOutput(ns("upload_text")),
          
          DT::dataTableOutput(ns("inputdataset")),
          style = 'overflow-x: scroll;height:400px;overflow-y: scroll;'
        )
      ),
      tabPanel(
        title = "Structure",
        bs4Dash::box(
          title = tags$strong("Structure of the DataSet"),
          width = 12,
          verbatimTextOutput(ns("strucure_dataset")),
          style = 'overflow-x: scroll;height:300px;overflow-y: scroll;'
        )
      ),
      tabPanel(
        title = "Summary",
        bs4Dash::box(
          title = tags$strong("Summary of the DataSet"),
          width = 12,
          verbatimTextOutput(ns("summary_dataset")),
          style = 'overflow-x: scroll;height:300px;overflow-y: scroll;'
        )
      ),
      tabPanel(
        title = "Classes",
        bs4Dash::box(
          title = tags$strong("Classes of the DataSet"),
          width = 12,
          verbatimTextOutput(ns("class_dataset")),
          style = 'overflow-x: scroll;height:300px;overflow-y: scroll;'
        )
      )
    )
    
  ) 
  ) #End of the tab box
} # End of the module UI function

# Start of the upload dataset server function

upload_dataset_Server<-function(id){
  moduleServer(id, function(input, output, session) {
    vals <- reactiveValues()
    
    disable("accept_data_button")
    ns<-NS(id)
    
    observeEvent(input$accept_data_button,{
      
      output$plot_type_ui<-renderUI({
      fluidRow(
        bs4Dash::box(
          width = 12,
          title = tags$strong("Select a Type:"),
          radioButtons(
            ns("plot_option"),
            "",
            selected = FALSE,
            
            c(
              "Univariate distribution" = "uni",
              "Bi-variate distribution" = "bivari",
              "Both" = "both"
            )
          )
        )
      )
      })
    })
    output$upload_text <- renderPrint({
      if (is.null(input$upload_dataset))
      {
        cat("[Please upload a dataset to view]")
        
      }
      else
      {
        output$inputdataset <- DT::renderDataTable({
          vals$file <- input$upload_dataset
          vals$file_format <-
            tools::file_ext(vals$file$datapath)
          req(vals$file)
          validate(need(
            vals$file_format == "csv",
            "Please Upload a csv file"
          ))
          enable("accept_data_button")
          vals$input_csv <-
            read.csv(vals$file$datapath, header = TRUE)
        })
        
      }
    })
    
    # Render output to the strucure of the dataset
    output$strucure_dataset <- renderPrint({
      if (is.null(input$upload_dataset))
      {
        cat("[Please upload a dataset to view]")
      }
      else
      {
        req(vals$file)
        validate(need(
          vals$file_format == "csv",
          "Please Upload a csv file"
        ))
        str(vals$input_csv)
      }
    })  # End of the outpt
    
   # Render output of the summary 
     output$summary_dataset <- renderPrint({
      if (is.null(input$upload_dataset))
      {
        cat("[Please upload a dataset to view]")
      }
      else
      {
        req(vals$file)
        validate(need(
          vals$file_format == "csv",
          "Please Upload a csv file"
        ))
        summary(vals$input_csv)
      }
    })
    # Render output of the class 
    output$class_dataset <- renderPrint({
      if (is.null(input$upload_dataset))
      {
        cat("[Please upload a dataset to view]")
      }
      else
      {
        req(vals$file)
        validate(need(
          vals$file_format == "csv",
          "Please Upload a csv file"
        ))
        sapply(vals$input_csv, class)
      }
    })
   
    
    # Return the list of values from the Server upload data set module
    
    return(
      list(
        inpu_dataset = reactive({vals$input_csv}),  # Return the upload dataset reactive
        radio_btn=reactive({input$plot_option}) # Return the radio button option reactive
      )
    ) # End of the return
    
    
  })  # End of the module server function
}

# End of the upload dataset server function