commitq1_ui <- function(id){
  ns <- NS(id)
  tagList(
    textInput(ns("input_text"), "Enter Commit Message:"),
    actionButton(ns("commit_btn"), "Commit"),
    actionButton(ns("undo_btn"), "Undo"),
    div(id = ns("commit_ls"), class = "branch")
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
        removeUI(selector = ".dot")
      })
      
    }
  )
}
