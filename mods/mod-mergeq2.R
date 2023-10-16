analysis2 <- "[Contents of Analysis2 Branch]"
main <- "[Contents of Main Branch]"


mergeq2_ui <- function(id){
  ns <- NS(id)
  tagList(
    h3("Merging branches together"),
    p("Again, we want to pull the updates we have made on the analysis branch into main.
      But this time there is a commit on main already.
      Change to the branch to main and merge to see what happens. 
      "),

    fluidRow(
      column(width = 6,
             selectInput(ns("slct_brn"),  "Branch",
                         c("main", "Analysis2"), selected = "Analysis2", width  ='80%')
      )
    ),
    div(class = "graph", id = ns("graph"),
        div(class = "slice",
            div(class = "branch"),
            div(class = c("branch", "main", "left"), id = ns("b0"),
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
        div(class = "slice", id = ns("commit_ls"), style = "z-index: 2;",
            div(
              class = c("branch", "topbranch", "analysis2"), id = ns("b1"),
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
              class = c("branch", "samebranch", "main", "right"), id = ns("b2"),
              div(
                class = "dot",
                message = "New version"
              )
            )
        ),
        div(class = "slice", id = ns("merge_loc"),
            div(class = "branch")
        )
    ),
    fluidRow(
      column(width = 3, 
             disabled(actionButton(ns("merge"), "Merge")))
    ),
    fluidRow(
      column(width = 11, 
             aceEditor(ns("code_box"), "Code Box", value = main, readOnly = TRUE)
      )
    ),
    
    
    
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
          enable("merge")
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
          
          disable("merge")
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
        # Update code 
        updateAceEditor(session, "code_box", 
                        value = main_cont())
      })
      
      
      
      observeEvent(input$merge, {
        # Merge Conflict box
        showModal(
          modalDialog(
            title = "Merge Conflict",
            h5("There is a merge conflict with the file. Please Correct and then commit"),
            p("Merge conflict can happen when multiple people change the same file. 
              To resolve this you just need to decide what to keep and delete the <<<, ==, and >>> rows."),
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
          selector = paste0("#",  ns("merge_loc")),
          where = "beforeEnd",
          ui = div(class = c("branch", "topbottom"),
                   div(class = c("branch", "main", "right", "samebranch2"),
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
          $('#{ns('b2')}').css('width', '100%');
          $('#{ns('b2')}').css('border-radius', '0px');
              "
        ))
        
        
        disable("merge")
        
      })
      
      
      
      
      
    }
  )
}
