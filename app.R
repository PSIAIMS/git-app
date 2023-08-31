library(shiny)
library(shinyAce)
library(shinyjs)
library(purrr)

list.files(path = "mods/", full.names = TRUE) |> 
  map(source)
  
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
                          commitq1_ui("q1"))
              )
              
    ),
    nav_panel("Making Branches"),
    nav_panel("Remotes"),
    nav_panel("Merging Branches")
  )
)


server <- function(input, output, session) {
  commitq1_server("q1")
  commitq2_server("q2")
}

shinyApp(ui = ui, server = server)
