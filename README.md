# Quantzig_EDA_Task_modules
e EDA data visualization ‘ggplot2’ tool using R and RShiny using modules

## Author: Dadi Siva Kumari
## Mail-id: sivakumari1996d@gmail.com
## Input Data:
*  Upload any csv format dataset

### Libraries:
 * library(shiny)
 * library(tools)
  *  library(shinydashboard)
   * library(shinyWidgets)
   * library(shinyjs)
   * library(bs4Dash)
   * library(dplyr)
   * library(ggplot2)

### Technologies: 
1. RStudio
Version 1.4.1106
© 2009-2021 RStudio, PBC
2. R version 4.1.1 (2021-08-10) -- "Kick Things"
3. CSS
### code type: 
Modularization

### Deployment:
Deployed in the Shinyapps.io Server
### Launch:
https://sivakumari.shinyapps.io/EDA_Task/


## R Shiny Application Set-Up: 
#### /app.R
#### /R/plots_module.R
#### /R/upload_dataset_module.R
#### /www/main.css

### The application consists of two modules...

* The first R Shiny module shows upload  input dataset with one tab box to show "Summary,strucutre and classes" of the uploaded dataset and  returns the uploaded dataset and radio button selection value  in the server module.
* The second R Shiny module shows a box to select type of the plot you want (univariate,bivariate and both) once you select any option it shows another box with plot button to select columns to get the plot once we select the columns from the select input widget then click on the plot button we get tab box with plots on left side use can see the plots.
* The final app.R file will add these source files and run the shiny app.

### WWW Folder
>  main.css file is to add CSS elements to add style to the shiny application.

