analysis2 <- "[Contents of Analysis2 Branch]"
main <- "[Contents of Main Branch]"




mergeq2_ui <- function(id){
  ns <- NS(id)
  tagList(
    h3("Mering branches together"),
    h5("We are going to combine our branch into main"),
    fluidRow(
      column(width = 6,
             selectInput(ns("slct_brn"),  "Branch",
                         c("main", "Analysis2"), width  ='80%')
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
        div(class = "slice", id = ns("commit_ls"), style = "z-index: 2; height:539px",
            div(
              div(
                class = "dot",
                message = "Update name of variable"
              ),
              div(
                class = c("dot", "head"),
                message = "Fixed bug"
              ),
              class = c("branch", "topbranch2"), id = ns("b1")
            ),
            div(
              div(
                class = "dot",
                message = "New version"
              ),
              class = c("branch", "straight"), id = ns("b2")
            )
        )
    ),
    fluidRow(
      column(width = 11, 
             aceEditor(ns("code_box"), "Code Box", value = main, readOnly = TRUE)
      )
    ),
    actionButton(ns("merge"), "Merge into main")
    
    
  )
}

mergeq2_server <- function(id){
  moduleServer(
    id,
    function(input, output, session){
      ns <- session$ns
      
      main_cont <- reactiveVal(main)
      
      observeEvent(input$slct_brn, {
        if(input$slct_brn == "main" & input$merge == 0){
          shinyjs::runjs(
            str_glue(
              "$('#{ns('graph')}').find('.head').first().removeClass('head');
              $('#{ns('b2')}').find('.dot').last().addClass('head');
              "
            )
          )
          
          updateAceEditor(session, "code_box", 
                          value = main_cont())
          
        } else if(input$slct_brn == "main" & input$merge > 0){
          shinyjs::runjs(
            str_glue(
              "$('#{ns('graph')}').find('.head').first().removeClass('head');
              $('#{ns('b3')}').find('.dot').last().addClass('head');
              "
            )
          )
          
          updateAceEditor(session, "code_box", 
                          value = main_cont())
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
      
      observeEvent(input$merge_box, {
        
        test <- str_detect(input$merge_box, "(<<<<<<< HEAD|=======|>>>>>>>)",
                           negate = TRUE)
        
        if(test){
          main_cont(input$merge_box)
          shinyjs::enable("commit")
        } else {
          shinyjs::disable("commit")
        }
        
      })
      
      observeEvent(input$commit, {
        removeModal()
      })
      
      
      
      observeEvent(input$merge, {
        # Merge Conflict box
        showModal(
          modalDialog(
            title = "Merge Conflict",
            h5("There is a merge conflict with the file. Please Correct and then commit"),
            aceEditor(ns("merge_box"), "Merge Box", 
                      value = paste0(
                        "<<<<<<< HEAD\n",
                        main,
                        "\n=======\n",
                        analysis2,
                        "\n>>>>>>> Analysis2"
                        )),
            footer=tagList(
              actionButton(ns('commit'), 'Commit')
            )
          
          )
        )
        
        # Add New Dot 
        insertUI(
          selector = paste0("#",  ns("graph")),
          where = "beforeEnd",
          ui = div(class = c("branch", "comboUp2"),
                   div(class = c("branch", "comboStraight2"),
                       id = ns("b3"),
                       div(
                         class = c("dot", 'head'),
                         message = "Merged"
                       )
                   )
          )
        )
        # Move Head 
        shinyjs::runjs(str_glue(
          "$('#{ns('graph')}').find('.head').first().removeClass('head');
              "
        ))
        
        # Update code 
        updateAceEditor(session, "code_box", 
                        value = paste(main, "\n", analysis2))
        
      })
      
      
      
      
      
    }
  )
}
