library(shiny)
library(shinythemes)

fluidPage(theme = shinytheme("cyborg"),
                navbarPage(
                  "Freight Operation Efficiency Metric (FOEM)",
                  tabPanel("Calculator",
                           # Page header
                           headerPanel('Impact of technologies on various metric by segment?'),
                           # side panel
                           sidebarPanel(
                             HTML("<h3>Input parameters</h3>"),
                             
                             # Select Metric
                             selectInput("metric", label = "Metric:", 
                                         choices = list("Unit Energy Consumption (BTU/mile)" = "Unit Energy Consumption", 
                                                        "Unit Cost ($/mile)" = "Unit Cost"),
                                         selected = "Unit Energy Consumption (BTU/mile)"),
                             # Select technology 
                             selectInput("tech", label ="Technology:",
                                         choices = list("Powertrain" = "powertrain", 
                                                        "Platooning" = 'platoon'), 
                                         selected = "Powertrain"),
                             # Select technology level
                             sliderInput("t_lv", label ="Technology level:",
                                         min = 0, max = 5,
                                         value = 0),
                             # Change in the technology level
                             numericInput("t_delta", label = "Technology change (e.g. 5.1 %):", 
                                          value = 5.1),
                             
                             actionButton("submitbutton", "Submit", class = "btn btn-primary")
                           ), # sidebarPanel
                           mainPanel(
                             tags$label(h3('Status/Output')), # Status/Output Text Box
                             verbatimTextOutput('contents'),
                             uiOutput("button"),
                             tableOutput('tabledata') # results table,
                           ) # mainPanel
                  ), # Navbar 1, tabPanel
                  tabPanel("Technology Definition", titlePanel("Technology Definition"),div(includeMarkdown("TechD.md"), align="justify")),
                  tabPanel("About", titlePanel("About"), div(includeMarkdown("About.md"), align="justify"))
                  
                ) # navbarPage
) # fluidPage
