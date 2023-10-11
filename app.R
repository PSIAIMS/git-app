library(shiny)
library(shinyAce)
library(shinyjs)
library(bslib)
library(purrr)
library(stringr)

source("data/code_versions.R")
source("global.R")

  
ui <-  fluidPage(
  includeCSS("www/style.css"),
  shinyjs::useShinyjs(),
  page_navbar(
    title = "Git in theory",
    theme = bs_theme(bootswatch ="minty"),
    nav_panel("Commiting",
              navset_card_tab(
                nav_panel("Q1",
                          commitq2_ui("q2")),
                nav_panel("Q2",  
                          commitq1_ui("q1")),
                nav_panel("Q3",  
                          commitq3_ui("q3"))
                
              )
              
    ),
    nav_panel("Making Branches",
              navset_card_tab(
                nav_panel("Q1",
                          branchq1_ui("bq1")),
                nav_panel("Q2",
                          branchq2_ui("bq2"))
              )
              ),
    nav_panel("Merging Branches", 
              navset_card_tab(
                nav_panel("Q1",
                          mergeq1_ui("mq1")),
                nav_panel("Q2",
                          mergeq2_ui("mq2")
                          )
              )
              ),
    nav_panel("Remotes", 
              navset_card_tab(
                nav_panel("Q1",
                          remotesq1_ui("rq1"))
              )
    )
  )
)


server <- function(input, output, session) {
  commitq1_server("q1")
  commitq2_server("q2")
  commitq3_server("q3")
  
  branchq1_server("bq1")
  branchq2_server("bq2")
  
  mergeq1_server("mq1")
  mergeq2_server("mq2")
  
  remotesq1_server("rq1")
  
}

shinyApp(ui = ui, server = server)
