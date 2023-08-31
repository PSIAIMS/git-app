commitq1_ui <- function(id){
  ns <- NS(id)
  tagList(
    h3("Make a commit history that matches the following diagram:"),
    div(
      div(
        class = "dot",
        message = "My first commit"
      ),
      div(
        class = "dot",
        message = "Next commit"
      ),
      div(
        class = "dot",
        message = "Final commit"
      ),
      class = "branch"
    ),
    textInput(ns("input_text"), "Enter Commit Message:"),
    fluidRow(
      column(6,
    actionButton(ns("commit_btn"), "Commit")),
    column(6, actionButton(ns("undo_btn"), "Undo"))),
    div(id = ns("answers"),
        div(id = ns("commit_ls"), class = "branch")
        )
  )
}

commitq1_server <- function(id){
  moduleServer(
    id,
    function(input, output, session){
      ns <- session$ns
      
      observeEvent(input$commit_btn, {
        if (input$input_text != "") {
          insertUI(
            selector = paste0("#",  ns("commit_ls")),
            where = "beforeEnd",
            ui = div(
              class = "dot",
              message = input$input_text
            )
          )
          updateTextInput(session, "input_text", value = "")
        }
      })
      
      observeEvent(input$undo_btn, {
        removeUI(selector = paste0("#",  ns("commit_ls")))
        insertUI(
          selector = paste0("#",  ns("answers")),
          where = "beforeEnd",
          ui = div(id = ns("commit_ls"), class = "branch")
        )
        
      })
      
    }
  )
}
