library(shiny)
library(shinyAce)
library(shinyjs)

source("mods/mod-commitq1.R")
source("mods/mod-commitq2.R")
ui <- fluidPage(
  includeCSS("www/style.css"),
  shinyjs::useShinyjs(),
  navbarPage("Git in theory",
             tabPanel("Commiting",
                      tabsetPanel(type = "tabs",
                                  tabPanel("Q1",  
                                           commitq1_ui("q1")),
                                  tabPanel("Q2",
                                           commitq2_ui("q2"))
                                           )
                      
                     ),
             tabPanel("Making Branches"
             ),
             tabPanel("Merging Branches")
  )
)


server <- function(input, output, session) {
  commitq1_server("q1")
  commitq2_server("q2")
}

shinyApp(ui = ui, server = server)
