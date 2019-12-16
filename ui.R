library(shiny)
library(shinydashboard)
library(shinythemes)
library(dplyr)
library(shinyjs)
library(readr)
library(shinyWidgets)
library(readxl)
library(plotly)
library(shinyanimate)
packageVersion('plotly')


# Load data
meat_data=read_csv("data/carcass_calculator_data.csv")
mur=read_csv("data/beef.csv")
hvm = read_csv("data/HVM_Transaction_data_9_30_18__9_30_19.csv")
calorie <- read_excel("data/Calorie.xlsx")
protein <- read_excel("data/upload.xlsx")

my_choices <- c(meat_data$cut)
agr<-c(protein$Food_type)


  title <- dashboardHeader(titleWidth = 400,
                           dropdownMenu())
  title$children[[2]]$children <- tags$a(href = 'http://www.happyvalleymeat.com',
                    "Happy Valley Meat Company")
      
  dbside <- dashboardSidebar(
        sidebarMenu(
                     menuItem("Overview",tabName = "Details",icon = icon("bar-chart-o")),
                     menuItem("Environmental Impact",tabName = "Visulization",icon = icon("database")),
                     menuItem("Comparison",tabName = "Comparison",icon = icon("rank"))
                    ))
  

  dbbody <- dashboardBody(
    includeCSS("stylesheet.css"),
    fluidRow(
    tabItems(
      tabItem(tabName = "Details",
              box(
                  title = "Type of Cuts",
                  width = 12,
                  id = "P.cut",
                  height = '230px',
                  selectInput(inputId = "type", label = strong("TYPE"),
                              choices= my_choices,
                              multiple = TRUE,
                             ),
                  checkboxInput("All", "Select all",value = TRUE)
              ),
              br(),
              br(),
              
              box(h2("Overview"),
                  br(),
                  br(),
                  plotlyOutput("details"),
                  width=12)),

      ## information for Environmental Score
      tabItem(tabName = "Visulization",
              
              fluidRow(
                tabBox(
                  width = 7,
                  height = "1360px",
                  title = "Environmental Impact Score(EI)",
                  tabPanel("Data",tableOutput("Table")),
                  tabPanel("Visualization", plotlyOutput("distplot"))),
                box(
                 title = "Type of Cuts",
                  width = 5,
                  height = "330px",
                  id = "P.cut",
                selectInput(inputId = "type1", label = strong("TYPE"),
                              choices= my_choices,
                              multiple = TRUE,
                            ),
                  checkboxInput("ALL", "Select all",value = TRUE)
                ),

                box(
                  id = "P.cut",
                  width = 5,
                  height = "90px",
                  solidHeader = TRUE,
                  numericInput("weight_meat", label = "Pounds of Each Cut",value = 100),
                  
                ),
                box(
                  title = strong("Select weights for environment impact"),
                  id = "P.weight",
                  width = 5,
                  height = "330px",
                  numericInput("water_weight",label = h4("Weight of Water Usage"),
                               value = 2),
                  numericInput("land_weight",label = h4("Weight of Land Usage"),
                               value = 3),
                  numericInput("CO2_weight",label = h4("Weight of CO2 Emission"),
                               value = 2))
              )),
      

      ## information for comparison with other poducts
      tabItem(tabName = "Comparison",
              fluidRow(
                box(
                title = "Type of Food",
                width = 2,
                height = "1000px",
                id = "P.cut",
                selectInput(inputId = "agProduct", label = strong("Food types"),
                            choices= agr,
                            multiple = TRUE,
                ),
                checkboxInput("all", "Select all",value = TRUE)
              ),
                tabBox(
                  width = 10,
                  height = "1000px",
                  title = strong("Comparison among different food types"),
                  tabPanel("Per Tonne Protein Consumed",
                           br(),
                           br(),
                           fluidRow(
                             box(
                               width = 6,
                               plotlyOutput("prepre")
                             ),
                             box(
                               width = 6,
                               plotlyOutput("pro_CO2")
                             )
                           ),
                           fluidRow(
                             box(
                               width = 6,
                               plotlyOutput("pro_Water")
                             ),
                            
                              
                             box(width = 6,
                                 plotlyOutput("pro_Land")
                                 )
                           )),
                  
                  tabPanel("Per Million Kilocalories Consumed", 
                           br(),
                           br(),
                           fluidRow(
                             box(width = 6,
                                 plotlyOutput("calcal")),
                             box(width = 6,
                               plotlyOutput("cal_CO2"))
                             ),
                           fluidRow(
                             box(width = 6,
                                 plotlyOutput("cal_Water")
                                 ),
                             box(width = 6,
                                 plotlyOutput("cal_Land")
                                 )
                             
                             ))
                  )))
      )))
         
  
 # Define UI
  ui <-dashboardPage(
    skin = "green",
    title,
    dbside,
    dbbody)
