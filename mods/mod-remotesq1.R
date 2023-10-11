
remotesq1_ui <- function(id){
  ns <- NS(id)
  tagList(
    h3("Finding branches"),
    textOutput(ns("question")),
    div(class = "box", id = ns("remotes"),
        "Remote",
        div(class = "graph", id = ns("graph-remote"),
            div(class = "slice",
                div(class = "branch"),
                div(class = c("branch", "main", "left"), id = ns("b0r"),
                    "Main",
                    div(
                      class = "dot",
                      message = "My first commit"
                    ),
                    div(
                      class = "dot",
                      message = "Next commit"
                    ),
                    
                    style = "z-index: 3;"
                )
            ),
            div(class = "slice", style = "z-index: 2;",
                div(
                  class = c("branch", "topbranch", "analysis2"), id = ns("b1r"),
                  div(
                    class = "dot",
                    message = "Update name of variable"
                  ),
                  div(
                    class = c("dot", "head"),
                    message = "Fixed bug"
                  )
                ),
                div(
                  class = c("branch", "samebranch", "main", "right"), id = ns("b2r"),
                  div(
                    class = "dot",
                    message = "New version"
                  )
                )
            )
        )
    ),
    div(class = "box", id = ns("local"), 
        "Local ",
        div(class = "graph", id = ns("graph-remote"),
            div(class = "slice",
                div(class = "branch"),
                div(class = c("branch", "main", "left"), id = ns("b0l"),
                    "Main",
                    div(
                      class = "dot",
                      message = "My first commit"
                    ),
                    div(
                      class = "dot",
                      message = "Next commit"
                    ),
                    
                    style = "z-index: 3;"
                )
            ),
            div(class = "slice", style = "z-index: 2;",
                div(
                  class = c("branch", "topbranch", "analysis2"), id = ns("b1l"),
                  div(
                    class = "dot",
                    message = "Update name of variable"
                  ),
                  div(
                    class = c("dot", "head"),
                    message = "Fixed bug"
                  )
                ),
                div(
                  class = c("branch", "samebranch", "main", "right"), id = ns("b2l"),
                  div(
                    class = "dot",
                    message = "New version"
                  )
                )
            )
        )
    )
    
    
  )
}

remotesq1_server <- function(id){
  moduleServer(
    id,
    function(input, output, session){
      ns <- session$ns
      quest_n <- reactiveVal(1)
      questions <- c(
        "Find the 'remotes/main' branch",
        "Find the 'local/analysis2' branch",
        "Find the 'local/main' branch"
        )
      
      output$question <- renderText(questions[quest_n()])
      
      observeEvent(input$close,{
        removeModal()
      })

      onclick("b0r", {
        if(quest_n() == 1){
          showModal(
            modalDialog(
              title = "Correct!",
              p("Let's find another branch"),
              footer=tagList(
                actionButton(ns('close'), 'close')
              )
            )
          )
          quest_n(2)
        } else {
          showModal(
            modalDialog(
              title = "Oops",
              p("You haven't found the right branch, keep looking"),
              footer=tagList(
                actionButton(ns('close'), 'close')
              )
            )
          )
        }
        
      })
      
      onclick("b2r", {
        if(quest_n() == 1){
          showModal(
            modalDialog(
              title = "Correct!",
              p("Let's find another branch"),
              footer=tagList(
                actionButton(ns('close'), 'close')
              )
            )
          )
          quest_n(2)
        } else {
          showModal(
            modalDialog(
              title = "Oops",
              p("You haven't found the right branch, keep looking"),
              footer=tagList(
                actionButton(ns('close'), 'close')
              )
            )
          )
        }
        
      })
      
      onclick("b1r", {
          showModal(
            modalDialog(
              title = "Oops",
              p("You haven't found the right branch, keep looking"),
              footer=tagList(
                actionButton(ns('close'), 'close')
              )
            )
          )
      })
      
      onclick("b0l", {
        if(quest_n() == 3){
          showModal(
            modalDialog(
              title = "Correct!",
              p("Let's move on to the next question"),
              footer=tagList(
                actionButton(ns('close'), 'close')
              )
            )
          )
        } else {
          showModal(
            modalDialog(
              title = "Oops",
              p("You haven't found the right branch, keep looking"),
              footer=tagList(
                actionButton(ns('close'), 'close')
              )
            )
          )
        }
        
      })
      
      onclick("b2l", {
        if(quest_n() == 3){
          showModal(
            modalDialog(
              title = "Correct!",
              p("Let's move on to the next question"),
              footer=tagList(
                actionButton(ns('close'), 'close')
              )
            )
          )
        } else {
          showModal(
            modalDialog(
              title = "Oops",
              p("You haven't found the right branch, keep looking"),
              footer=tagList(
                actionButton(ns('close'), 'close')
              )
            )
          )
        }
        
      })
      
      onclick("b1l", {
        if(quest_n() == 2){
          showModal(
            modalDialog(
              title = "Correct!",
              p("Let's move on to the next question"),
              footer=tagList(
                actionButton(ns('close'), 'close')
              )
            )
          )
          quest_n(3)
        } else {
          showModal(
            modalDialog(
              title = "Oops",
              p("You haven't found the right branch, keep looking"),
              footer=tagList(
                actionButton(ns('close'), 'close')
              )
            )
          )
        }
        
      })
      
      
      
    }
  )
}
