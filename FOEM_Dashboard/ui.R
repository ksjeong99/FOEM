library(shiny)
library(shinythemes)

fluidPage(theme = shinytheme("cyborg"),
          navbarPage(
            "Freight Operation Efficiency Metric (FOEM)",
            tabPanel("Calculator",
                     # Page header
                     headerPanel('Impact of technology scenarios'),
                     # side panel
                     sidebarPanel(
                       HTML("<h3>Select an analysis level and technology scenarios</h3>"),
                       # Select group_by
                       selectInput("group", label = "Analysis Level:", 
                                   choices = list("Segment-level" = "segment",
                                                  "System-level" = "system"),
                                   selected = "Segment-level"),                             
                       
                       # Select Frieght Projection
                       selectInput("s_dmd", label ="Freight Demand Projection:",
                                   choices = list("BAU-freightProj" ="BAU-freightProj",
                                                  "ECommerce-freightProj" = "ECommerce-freightProj"),
                                   selected = "BAU-freightProj"),
                       
                       # select Payload factor scenario
                       selectInput("s_pload", label ="Payload:",
                                   choices = list("BAU-payload"="BAU-payload",
                                                  "+10%-payload"="+10%-payload"),
                                   selected = "BAU-payload"),
                       
                       # Select powertrain
                       selectInput("s_ptrain", label ="Powertrain Adoption:",
                                   choices = list("AEO21-powertrainAdopt" = "AEO21-powertrainAdopt",
                                                  "MidZEV-powertrainAdopt" = "MidZEV-powertrainAdopt",
                                                  "HighZEV-powertrainAdopt" = "HighZEV-powertrainAdopt"), 
                                   selected = "AEO21-powertrainAdopt"),
                       # select Connectivity scenario
                       selectInput("s_conn", label ="Connectivity:",
                                   choices = list("BAU-connectivity"="BAU-connectivity",
                                                  "HC-connectivity"="HC-connectivity"),
                                   selected = "BAU-connectivity"),                               
                       
                       # select carbon emissions scenario
                       selectInput("s_energy", label ="Carbon Emissions:",
                                   choices = list("Base-wtwEmissions"= "Base-wtwEmissions",               
                                                  "HighRenewable-wtwEmissions"="HighRenewable-wtwEmissions"),
                                   selected = "Base-wtwEmissions"),
                       
                       actionButton("submitbutton", "Submit", class = "btn btn-primary")
                     ), # sidebarPanel
                     mainPanel(
                       tags$label(h3('Status/Output')), # Status/Output Text Box
                       htmlOutput("testHTML"),
                       tabsetPanel(
                         tabPanel("Energy", plotOutput("plot1")),
                         tabPanel("Cost", plotOutput("plot2")), 
                         tabPanel("Emission", plotOutput("plot3")), 
                         tabPanel("Table", downloadButton("downloadData", label ="Download"), tableOutput("table")))
                     )), # Navbar 1, tabPanel
            tabPanel("Technology Definition", titlePanel("Technology Definition"),div(includeMarkdown("TechD.md"), align="justify")),
            tabPanel("About", titlePanel("About"), div(includeMarkdown("About.md"), align="justify"))
            
          ) # navbarPage
) # fluidPage
