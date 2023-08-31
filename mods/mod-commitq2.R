init <- 'print("hello [My Name]")'

commitq2_ui <- function(id){
  ns <- NS(id)
  splitLayout(
    cellWidths = c("50%", "50%"),
    aceEditor(ns("code_box"), "Code Box", value = init),
    column(
      width = 12,
      div(style = "margin-left: 0.4rem;",
        textInput(ns("commit_msg"), "Commit Message", "")),
      actionButton(ns("commit_btn"), "Commit", width = "100%"),
      div(id = ns("commit_ls"), class = "branch",
          div(
            class = "dot",
            message = "My first commit"
          )
          
      )
    )
  )
}

commitq2_server <- function(id){
  moduleServer(
    id,
    function(input, output, session){
      ns <- session$ns

      observeEvent(input$code_box, {
        if(input$code_box != init){
          shinyjs::enable("commit_msg")
        } else {
          shinyjs::disable("commit_msg")
          shinyjs::disable("commit_btn")
        }
      })
      

      observeEvent(input$commit_msg, {
        if (input$commit_msg != "") {
        shinyjs::enable("commit_btn")
        } else {
          shinyjs::disable("commit_btn")
        }
      })

      observeEvent(input$commit_btn, {
        insertUI(
          selector = paste0("#",  ns("commit_ls")),
          where = "beforeEnd",
          ui = div(
            class = "dot",
            message = input$commit_msg
          )
        )

        updateTextInput(session, "commit_msg", value = "")
      })


    }
  )
}
