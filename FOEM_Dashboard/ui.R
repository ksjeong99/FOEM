library(shiny)
library(shinythemes)
library(shinyauthr)     # shiny authentication modules
library(sodium) 

fluidPage(theme = shinytheme("cyborg"),
          shinyauthr::loginUI(id = "login"),
          uiOutput(outputId = "display_content_authr")
         ) # fluidPage
