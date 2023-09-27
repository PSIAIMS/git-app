analysis2 <- "[Contents of Analysis2 Branch]"
main <- "[Contents of Main Branch]"




mergeq1_ui <- function(id){
  ns <- NS(id)
  tagList(
    h3("Mering branches together"),
    h5("We are going to combine our branch into main"),
    fluidRow(
      column(width = 6,
             selectInput(ns("slct_brn"),  "Branch",
                         c("main", "Analysis2"))
      )
    ),
    div(class = "graph", id = ns("graph"), style = "height:40vh",
        div(
          div(
            class = "dot",
            message = "My first commit"
          ),
          div(
            class = "dot",
            message = "Next commit"
          ),
          class = "branch", id = ns("b0"),
          style = "z-index: 3;"
        ),
        div(class = "slice", id = ns("commit_ls"), style = "z-index: 2;",
            div(
              div(
                class = "dot",
                message = "Update name of variable"
              ),
              div(
                class = c("dot", "head"),
                message = "Fixed bug"
              ),
              class = c("branch", "topbranch"), id = ns("b1")
            ),
            div(
              div(
                class = "dot",
                message = "New version"
              ),
              class = c("branch", "lowbranch"), id = ns("b2")
            )
        )
    ),
    fluidRow(
      column(width = 11, 
             aceEditor(ns("code_box"), "Code Box", value = main)
      )
    ),
    actionButton(ns("merge"), "Merge")
    
    
  )
}

mergeq1_server <- function(id){
  moduleServer(
    id,
    function(input, output, session){
      ns <- session$ns
      
      observeEvent(input$slct_brn, {
        if(input$slct_brn == "main"){
          shinyjs::runjs(
            str_glue(
              "$('#{ns('graph')}').find('.head').first().removeClass('head');
              $('#{ns('b2')}').find('.dot').last().addClass('head');
              "
            )
          )
          
          updateAceEditor(session, "code_box", 
                          value = main)
          
        } else {
          shinyjs::runjs(
            str_glue(
              "$('#{ns('graph')}').find('.head').first().removeClass('head');
              $('#{ns('b1')}').find('.dot').last().addClass('head');
              "
            )
          )
          
          updateAceEditor(session, "code_box", 
                          value = analysis2)
        }
      })
      
      
      
      observeEvent(input$merge, {
        # Add New Dot 
        insertUI(
          selector = paste0("#",  ns("graph")),
          where = "beforeEnd",
          ui = div(class = c("branch", "comboUp"),
                   div(class = c("branch", "comboLow"),
                       id = ns("b3"),
                       div(
                         class = "dot",
                         message = "Merged"
                       )
                   )
          )
        )
        # Move Head 
        shinyjs::runjs(str_glue(
          "$('#{ns('graph')}').find('.head').first().removeClass('head');
              $('#{ns('b3')}').find('.dot').last().addClass('head');
              "
        ))
        
        # Update code 
        updateAceEditor(session, "code_box", 
                        value = paste(main, "\n", analysis2))
        
      })
      
      
      
      
      
    }
  )
}
