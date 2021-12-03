library(shiny)
library(shinythemes)


source("module_login.R")
fluidPage(theme = shinytheme("cyborg"),
          login_ui(id = "module_login", title = "Please login"),
          uiOutput(outputId = "display_content_authr")
         ) # fluidPage
