
#Start of the module UI with id

plots_module_UI<-function(id){
  ns<-NS(id)
  fluidRow(column(width = 6,
                  uiOutput(
                    ns("plot_optionUI")  # UI for the selected option of the plot
                  )),
           column(width = 6,
                  uiOutput(ns("plot_results")))  # UI for the plot result
  )  # End of the Plot UI
  
}  # End of the module UI

#Start of the Module server with 3 arguments

plots_module_Server<-function(id,dataset,input_value){
 
   moduleServer(id, function(input, output, session) {
   
     vals <- reactiveValues()
    
      ns<-NS(id)
     
    # Render output for the plot type  
     output$plot_optionUI <- renderUI({
    
       
       req(input_value())
  
      if (input_value() == "uni") {
        
        bs4Dash::box(
          width = 12,
          title = tags$strong("Univariate Distribution Plots"),
          fluidRow(
            column(
              width = 4,
              tooltip(
                selectInput(
                  inputId = ns("select_column_uni_bar"),
                  label = "Select column for Barplot:",
                  choices = c("", names(dataset())) ,
                  selected = NULL
                ),title = "Make sure to select categorical column",placement = "top"
              )
            ),
            column(
              width = 5,
              tooltip(
                selectInput(
                  inputId = ns("select_column_uni_density"),
                  label = "Select column for Density Plot:",
                  choices = c("", names(dataset())) ,
                  selected = NULL
                ),title = "Make sure to select continuous  column",placement = "top"
              )
            ),
            column(
              width = 3,
              tooltip(
                actionButton(ns("plot_uni"), "Plot", class =
                               "plot_class"),
                title = "Make sure to select columns before click",placement = "top"
              )
            )
          )
          
        )
      } else if (input_value() == "bivari") {
        bs4Dash::box(
          width = 12,
          title = tags$strong("Bi-variate Distribution Scatter Plot"),
          fluidRow(
            column(
              width = 5,
              tooltip(
                selectInput(
                  inputId = ns("select_column_biv_scatter_x"),
                  label = "Select (X) column:",
                  choices = c("", names(dataset())) ,
                  selected = NULL
                ),
                title = "Make sure to select continuous  columns",placement = "top")
            ),
            
            column(
              width = 5,
              tooltip(
                selectInput(
                  inputId = ns("select_column_biv_scatter_y"),
                  label = "Select (Y) column:",
                  choices = c("", names(dataset())) ,
                  selected = NULL
                ),
                title = "Make sure to select continuous  columns",placement = "top")
            ),
            
            column(
              width = 2,
              tooltip(
                actionButton(ns("plot_biv"), "Plot", class = "plot_class"),
                title = "Make sure to select columns before click",placement = "top")
            )
          )
        )
        
        
      } else{
        bs4Dash::box(
          width = 12,
          title = tags$strong("Univariate & Bi-variate Distribution Plots"),
          fluidRow(
            column(width = 4,
                   fluidRow(
                     tooltip(
                       selectInput(
                         inputId = ns("select_column_uni_bar_both"),
                         label = "Select column for Barplot:",
                         choices = c("", names(dataset())) ,
                         selected = NULL
                       ),
                       title = "Make sure to select categorical column",placement = "top"),
                     tooltip(
                       selectInput(
                         inputId = ns("select_column_uni_density_both"),
                         label = "Select column for Density Plot:",
                         choices = c("", names(dataset())) ,
                         selected = NULL
                       ),
                       title = "Make sure to select continuous column",placement = "top")
                     
                     
                     
                   )),
            
            
            column(
              width = 6,
              bs4Dash::box(
                width = 12,
                title = tags$strong("Scatter Plot:"),
                fluidRow(
                  column(
                    width = 6,
                    tooltip(
                      selectInput(
                        inputId = ns("select_column_biv_scatter_x_both"),
                        label = "Select (X) column:",
                        choices = c("", names(dataset())) ,
                        selected = NULL
                      ),
                      title = "Make sure to select continuous column",placement = "top")
                  ),
                  column(
                    width = 6,
                    tooltip(
                      selectInput(
                        inputId = ns("select_column_biv_scatter_y_both"),
                        label = "Select (Y) column:",
                        choices = c("", names(dataset())) ,
                        selected = NULL
                      ),
                      title = "Make sure to select continuous Column",placement = "top"
                    )
                  )
                )
              )
            ),
            
            column(
              width = 2,
              tooltip(
                actionButton(ns("plot_both"), "Plot", class = "plot_class"),
                title = "Make sure to select columns before click",placement = "top"
              )
            )
          )
          
        )
      }
    }) 
     # End of the render
    
     # Observe event of the univariate plot button
    observeEvent(input$plot_uni, {
      output$plot_results <- renderUI({
        if (input$select_column_uni_density != "" &
            input$select_column_uni_bar != "") {
          tabBox(
            width = 12,
            id = "plot_tabbox",
            title = "Univariate Plots",
            selected = "Barplot",
            status = "teal",
            solidHeader = TRUE,
            type = "tabs",
            tabPanel(
              title = "Barplot",
              bs4Dash::box(
                width = 12,
                title =  paste0(
                  "Bar Plot with ","(",
                  input$select_column_uni_bar,")",
                  " ",
                  "Column"
                ),
                plotOutput(ns("barplot_result"))
                
              )
              
            ),
            tabPanel(
              title = "Densityplot",
              bs4Dash::box(
                width = 12,
                title = paste0(
                  "Density Plot with ","(",
                  input$select_column_uni_density,")",
                  " ",
                  "Column"
                ),
                plotOutput(ns("density_plot_result"))
              )
            )
          )
        }else{
          tags$div(class="info_class",
                   return("Please select the column for each plot before click on the Plot button"))
          
        }
      })
      
      
      output$density_plot_result <- renderPlot({
        table <- dataset()
        column <- input$select_column_uni_density
        
        table %>%
          ggplot() +
          aes_string(x = column) +
          geom_density() +
          geom_density(
            fill = "orchid",
            size = 2,
            position = "identity",
            alpha = 0.4,
            linetype = "dashed"
          )
      })
      
      output$barplot_result <- renderPlot({
        ggplot(dataset(),
               aes_string(x = input$select_column_uni_bar)) +
          geom_bar(fill = "cornflowerblue",
                   color = "black") +
          labs(
            x = input$select_column_uni_bar,
            y = "Frequency",
            title = "Bar Plot using ggplot2"
          )
        
        
        
      })
    })
    # End of the univariate plot button
    
    # Observe event of the bivariate plot button
    observeEvent(input$plot_biv, {
      output$plot_results <- renderUI({
        if (input$select_column_biv_scatter_x != "" &
            input$select_column_biv_scatter_y != "") {
          tabBox(
            width = 12,
            id = "plot_tabbox_bi",
            title = "Bivariate Plots...",
            selected = "Scatterplot",
            status = "purple",
            solidHeader = TRUE,
            type = "tabs",
            
            tabPanel(
              title = "Scatterplot",
              bs4Dash::box(
                width = 12,
                title = paste0(
                  "Scatter Plot with ",
                  "(",
                  input$select_column_biv_scatter_x,
                  ",",
                  input$select_column_biv_scatter_y,
                  ")",
                  "Columns"
                ),
                plotOutput(ns("scatter_plot_result"))
              )
            )
          )
        }else{
          tags$div(class="info_class",
                   return("Please select the column for each plot before click on the Plot button"))
          
        }
      })
      
      
      
      
      output$scatter_plot_result <- renderPlot({
        # enhanced scatter plot
        ggplot(
          dataset(),
          aes_string(
            x = input$select_column_biv_scatter_x,
            y = input$select_column_biv_scatter_y
          )
        ) +
          geom_point(color = "cornflowerblue",
                     size = 2,
                     alpha = .8) +
          
          labs(
            x = input$select_column_biv_scatter_x,
            y = input$select_column_biv_scatter_y,
            title = "Scatter plot using Ggplot2"
          )+ theme_bw()
        
      })
      
    })
    # End of the bivariate plot button
    
    # Observe event of the both type plot button
    
    observeEvent(input$plot_both,{
      
      output$plot_results <- renderUI({
        if (input$select_column_uni_density_both != "" &
            input$select_column_uni_bar_both != "" & input$select_column_biv_scatter_x_both != "" & input$select_column_biv_scatter_y_both != "") {
          tabBox(
            width = 12,
            id = "plotboth_tabbox",
            title = "Univariate & Bivariate Plots",
            selected = "Barplot",
            status = "teal",
            solidHeader = TRUE,
            type = "tabs",
            tabPanel(
              title = "Barplot",
              bs4Dash::box(
                width = 12,
                title =  paste0(
                  "Bar Plot with ","(",
                  input$select_column_uni_bar_both,")",
                  " ",
                  "Column"
                ),
                plotOutput(ns("barplot_result_both"))
                
              )
              
            ),
            tabPanel(
              title = "Densityplot",
              bs4Dash::box(
                width = 12,
                title = paste0(
                  "Density Plot with ","(",
                  input$select_column_uni_density_both,")",
                  " ",
                  "Column"
                ),
                plotOutput(ns("density_plot_result_both"))
              )
            ),
            tabPanel(
              title = "Scatterplot",
              bs4Dash::box(
                width = 12,
                title = paste0(
                  "Scatter Plot with ",
                  "(",
                  input$select_column_biv_scatter_x_both,
                  ",",
                  input$select_column_biv_scatter_y_both,
                  ")",
                  "Columns"
                ),
                plotOutput(ns("scatter_plot_result_both"))
              )
            )
          )
        }else{
          tags$div(class="info_class",
                   return("Please select the column for each plot before click on the Plot button"))
          
        }
        
      })
      
      output$scatter_plot_result_both <- renderPlot({
        # enhanced scatter plot
        ggplot(
          dataset(),
          aes_string(
            x = input$select_column_biv_scatter_x_both,
            y = input$select_column_biv_scatter_y_both
          )
        ) +
          geom_point(color = "cornflowerblue",
                     size = 2,
                     alpha = .8) +
          
          labs(
            x = input$select_column_biv_scatter_x_both,
            y = input$select_column_biv_scatter_y_both,
            title = "Scatter plot using Ggplot2"
          )+ theme_bw()
        
      })
      
      
      output$density_plot_result_both <- renderPlot({
        table <- dataset()
        column <- input$select_column_uni_density_both
        
        table %>%
          ggplot() +
          aes_string(x = column) +
          geom_density() +
          geom_density(
            fill = "orchid",
            size = 2,
            position = "identity",
            alpha = 0.4,
            linetype = "dashed"
          )
      })
      
      output$barplot_result_both <- renderPlot({
        ggplot(dataset(),
               aes_string(x = input$select_column_uni_bar_both)) +
          geom_bar(fill = "cornflowerblue",
                   color = "black") +
          labs(
            x = input$select_column_uni_bar_both,
            y = "Frequency",
            title = "Bar Plot using ggplot2"
          )
        
        
        
      })
      
    })
    # End of the evenet
    
  })  # End of the module server function
}  # End of the plos module server function
