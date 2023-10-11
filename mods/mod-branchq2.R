
branchq2_ui <- function(id){
  ns <- NS(id)
  tagList(
    h3("Can you branch off Camille's tagged commit to make the updates journal A asked for?"),
    p('Using your mouse click to checkout the tagged commit. 
      Then create a new branch and click the commit button to reveal her updates.'),
    div(class = "graph", id = ns("graph"),
        div(class = "slice",
            div(class = "branch"),
            div(class = c("branch", "main", "left"), id = ns("b0"),
                "Main",
                div(
                  class = "dot",
                  message = "Code V1",
                  id = ns("dot1")
                ),
                div(
                  class = c("dot", "tagged"),
                  message = "Code V2",
                  id = ns("dot2")
                ),
                style = "z-index: 3;"
            )
        ),
        div(class = "slice", id = ns("commit_ls"), style = "z-index: 2;",
            div(
              class = c("branch"), id = ns("b1")
            ),
            div(
              div(
                class = "dot",
                message = "Code V3", 
                id = ns("dot3")
              ),
              div(
                class = c("dot", "head"),
                message = "Code V4",
                id = ns("dot4")
              ),
              class = c("branch", "samebranch", "main", "right"), id = ns("b2")
            )
        )
    ),
    
    
    fluidRow( 
      column(4, actionButton(ns("branch_btn"), "Make Branch")),
      column(6,
             actionButton(ns("commit_btn"), "Commit")
      ),
      
      fluidRow(
        column(width = 11, 
               aceEditor(ns("code_box"), "Code Box", value = code_versions["dots1"], 
                         readOnly = TRUE)
        )
      )
      
    )
  )
}

branchq2_server <- function(id){
  moduleServer(
    id,
    function(input, output, session){
      ns <- session$ns
      hide("commit_btn")
      
      # current_dot <- reactiveVal("dot4")
      
      observeEvent(input$branch_btn, {
        if(!is.null(input$head) && input$head == 'dot2'){
          disable("branch_btn")
          shinyjs::runjs(
            str_glue(
              "$('#{ns('b1')}').addClass('topbranch');
              "
            )
          )
          show("commit_btn")
        } else {
          showModal(
            modalDialog(
              title = "Opps",
              p("You haven't found the right commit, keep looking"),
              footer=tagList(
                actionButton(ns('close'), 'close')
              )
            )
          )
        }
      })
      
      
      observeEvent(input$close,{
        removeModal()
      })
      
      
      observeEvent(input$commit_btn, {
        shinyjs::runjs(
          str_glue(
            "$('#{ns('graph')}').find('.head').first().removeClass('head');"
          )
          
        )
        loc <- paste0("dotJA", input$commit_btn)
        insertUI(
          selector = paste0("#", ns("b1")),
          where = "beforeEnd",
          ui = div(
            id = ns(loc),
            class = c("dot", 'head'),
            message = ifelse(input$commit_btn == 1, "Journal Update",
                             "Final Journal Update")
          )
        )
        shinyjs::runjs(
          str_glue(
            "$('#{ns('b1')}').addClass('analysis2');"
          )
        )
        
        
        updateAceEditor(session, "code_box", 
                        value = code_versions[loc])
        
        if(input$commit_btn > 1){
          disable("commit_btn")
        }
      })
      
      
      dot_update("dot1", ns, session)
      dot_update("dot2", ns, session)
      dot_update("dot3", ns, session)
      dot_update("dot4", ns, session) 
      dot_update("dotJA1", ns, session) 
      dot_update("dotJA2", ns, session) 
    }
  )
}



