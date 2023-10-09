commitq1_ui <- function(id){
  ns <- NS(id)
  tagList(
    h3("Practice making commits:"),
    p('Try to copy the commit network graph below, 
      by checking each commit message and creating a similar commit below.'),
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
      
      num_dots <- reactiveVal(0)
      messages <- reactiveVal(NULL)
      
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
          disable("commit_btn")
          enable("undo_btn")
          num_dots(num_dots()+1)
          messages(c(messages(), input$input_text))
        }
        
      })
      
      observeEvent(input$input_text, {
        if (input$input_text == "") {
          disable("commit_btn")
        } else if(num_dots() < 3) {
          enable("commit_btn")
        }
        
      })
      
      observeEvent(input$undo_btn, {
        runjs(str_glue(
          "$('#{ns('commit_ls')}').find('.dot:last').remove();"
        ))
        
        messages(messages()[-length(messages())])
        
        num_dots(num_dots()-1)
      })
      
      observe({
        if(num_dots() < 1){
          disable("undo_btn")
        } else if(num_dots() == 3){
          disable("commit_btn")
          test <- setequal(messages(), c("My first commit",
                                "Next commit",
                                "Final commit"))
          if(test){
            showModal(
              modalDialog(
                title = "Congratulations",
                p("You have matched the network graph right down to the commit messages! Time for the next challenge!"),
                footer=tagList(
                  actionButton(ns('close'), 'close')
                )
              )
            )
          } else {
            showModal(
              modalDialog(
                title = "Oops",
                p("You matched the general shape of the network, but your commit messages are different. 
                  Use the 'undo' button to back and hover over each commit above to check the messages."),
                footer=tagList(
                  actionButton(ns('close'), 'close')
                )
              )
            )
          }
        }
        
      })
      
      observeEvent(input$close,{
        removeModal()
      })
      
    }
  )
}
