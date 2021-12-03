library(shiny)
library(shinythemes)

login_ui <- function(id, title) {
  
  ns <- NS(id) # namespaced id
  
  # define ui part
  div(
    id = ns("login"),
    style = "width: 500px; max-width: 100%; margin: 0 auto;",
    
    div(
      class = "well",
      
      h4(class = "text-center", title),
      p(class = "text-center", 
        tags$small("Second approach login form")),
      
      textInput(
        inputId     = ns("ti_user_name_module"),
        label       = tagList(icon("user"), 
                              "User Name"),
        placeholder = "Enter user name"
      ),
      
      passwordInput(
        inputId     = ns("ti_password_module"), 
        label       = tagList(
          icon("unlock-alt"), 
          "Password"
        ), 
        placeholder = "Enter password"
      ), 
      
      div(
        class = "text-center",
        actionButton(
          inputId = ns("ab_login_button_module"), 
          label   = "Log in",
          class   = "btn-primary"
        )
      )
    )
  )
}

fluidPage(theme = shinytheme("cyborg"),
          login_ui(id = "module_login", title = "Please login"),
          uiOutput(outputId = "display_content_authr")
         ) # fluidPage
