commitq3_ui <- function(id){
  ns <- NS(id)
  tagList(
    h3("Can you find the version of code Camille needs for journal A?"),
    p('Using your mouse click to checkout each version Camille saved. 
      Once you find the right one make sure to tag it so we can find it in the future.'),
    
    div(id = ns("graph"),
        div(
          class = "dot",
          message = "Code V1",
          id = ns("dot1")
        ),
        div(
          class = "dot",
          message = "Code V2",
          id = ns("dot2")
        ),
        div(
          class = "dot",
          message = "Code V3", 
          id = ns("dot3")
        ),
        div(
          class = c("dot", "head"),
          message = "Code V4",
          id = ns("dots1")
        ),
        class = "branch"
    ),
    
    
    fluidRow( 
      column(4, actionButton(ns("tag_btn"), "Tag"))),
    
    fluidRow(
      column(width = 11, 
             aceEditor(ns("code_box"), "Code Box", value = code_versions["dots1"], 
                       readOnly = TRUE)
      )
    )
    
  )
}

commitq3_server <- function(id){
  moduleServer(
    id,
    function(input, output, session){
      ns <- session$ns
      
      
      observeEvent(input$tag_btn, {
        if(!is.null(input$head) && input$head == 'dot2'){
          showModal(
            modalDialog(
              title = "Tag Commit",
              p("Add a tag message to this commit"),
              textInput(ns("input_tag"), "Tag Name:"),
              footer=tagList(
                actionButton(ns('submit'), 'Submit')
              )
            )
          )
        } else {
          showModal(
            modalDialog(
              title = "Oops",
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
      
      observeEvent(input$submit, { 
        removeModal()
        if (input$input_tag != "") {
          shinyjs::runjs(
            str_glue(
              "$('#{ns('graph')}').find('.head').first().addClass('tagged');
              "
            )
          )
          disable("tag_btn")
        }
        })
      
      
      dot_update("dot1", ns, session)
      dot_update("dot2", ns, session)
      dot_update("dot3", ns, session)
      dot_update("dots1", ns, session)
      
    }
  )
}


