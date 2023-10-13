
mergeq1_ui <- function(id){
  ns <- NS(id)
  tagList(
    h3("Merging branches together"),
    p("We want to pull the updates we have made on the analysis branch into main.
      To do this change the branch to main and then merge. 
      "),
    fluidRow(
      column(width = 6,
             selectInput(ns("slct_brn"),  "Branch",
                         c("main", "Analysis2"), selected = "Analysis2")
      )
    ),
    div(class = "graph", id = ns("graph"),
        div(class = "slice",
            div(class = "branch"),
            div(class = c("branch", "main"), id = ns("b0"),
                "Main",
                div(
                  class = "dot",
                  message = "My first commit"
                ),
                div(
                  class = c("dot"),
                  message = "Next commit"
                ),
                
                style = "z-index: 3;"
            )
        ),
        div(class = "slice", style = "z-index: 2;",
            div(
              div(
                class = "dot",
                message = "Update name of variable"
              ),
              div(
                class = c("dot"),
                message = "Fixed bug"
              ),
              class = c("branch", "topbranch", "analysis2"), id = ns("b1")
            ),
            div(
              class = c("branch", "main"), id = ns("b2") 
            )
        ), 
        div(class = "slice", id = ns("merge_loc"),
        )
    ),
    fluidRow(
      column(width = 3, 
             actionButton(ns("merge"), "Merge")
             
      ), 
      column(width = 3, 
             conditionalPanel(
               condition = "input.merge > 0",
               actionButton(ns("undo"), "Undo"),
               ns = ns
             )
      )
    )
  )
}

mergeq1_server <- function(id){
  moduleServer(
    id,
    function(input, output, session){
      ns <- session$ns
      merge_loc <- reactiveVal("")
      
      observeEvent(input$close,{
        removeModal()
      })
      
      observeEvent(input$slct_brn, {
        if(input$slct_brn == "main" & merge_loc() %in% c("", "Analysis2")){
          shinyjs::runjs(
            str_glue(
              "$('#{ns('graph')}').find('.head').first().removeClass('head');
              $('#{ns('b0')}').find('.dot').last().addClass('head');
              "
            )
          )
          
        } else if(input$slct_brn == "main" & merge_loc() == "main"){
          shinyjs::runjs(
            str_glue(
              "$('#{ns('graph')}').find('.head').first().removeClass('head');
              $('#{ns('b3')}').find('.dot').last().addClass('head');
              "
            )
          )
          
        } else {
          shinyjs::runjs(
            str_glue(
              "$('#{ns('graph')}').find('.head').first().removeClass('head');
              $('#{ns('b1')}').find('.dot').last().addClass('head');
              "
            )
          )
        } 
      })
      
      
      
      observeEvent(input$merge, {
        # Add New Dot 
        if(input$slct_brn == "main"){
          insertUI(
            selector = paste0("#",  ns("merge_loc")),
            where = "beforeEnd",
            ui = tagList(
              div(class = "branch"),
              div(
                class = c("branch", "topbottom"),
                  div(class = c("branch", "samebranch2", "main", "right"),
                      id = ns("b3"),
                      div(
                        class = c("dot", "head"),
                        message = "Merged"
                      )
                      
                  )
              )
            )
          )
          
          # Move Head 
          shinyjs::runjs(str_glue(
            "$('#{ns('graph')}').find('.head').first().removeClass('head');
            $('#{ns('b0')}').addClass('left');
           $('#{ns('b2')}').css('width', '100%');
          $('#{ns('b2')}').css('border-radius', '0px');
              "
          ))
          
        } else {
          shinyjs::runjs(str_glue(
            "$('#{ns('graph')}').find('.head').first().removeClass('head');
              "
          ))
          
          showModal(
            modalDialog(
              title = "Oops",
              p("You merged main into the Analysis2 branch rather than bringing 
                the Analysis branch into main. You will need to undo the merge and change your branch."),
              footer=tagList(
                actionButton(ns('close'), 'close')
              )
            )
          )

          insertUI(
            selector = paste0("#",  ns("b1")),
            where = "beforeEnd",
            ui = div(
              id = ns("mergedot"),
              class = c("bottomtop"),
              div(
                id = ns("dotm"),
                class = c("dot", "head"),
                message = "Merged"
              )
            )
          )
          
        }
        
        merge_loc(input$slct_brn)
        print(merge_loc())
        disable("merge")
        
      })
      
      
      observeEvent(input$undo,{
        enable("merge")
        if(merge_loc() == "main"){
          removeUI(
            selector = paste0("#",  ns("merge_loc"))
          )
          
          insertUI(
            selector = paste0("#",  ns("graph")),
            where = "beforeEnd",
            ui = div(class = "slice", id = ns("merge_loc"))
          )
          
          shinyjs::runjs(str_glue(
            "$('#{ns('b0')}').removeClass('left');
           $('#{ns('b2')}').css('width', 'unset');
          $('#{ns('b2')}').css('border-radius', 'unset');
              "
          ))
          
          
        } else if(merge_loc() == "Analysis2"){
          removeUI(
            selector = paste0("#",  ns("mergedot"))
          )
          
          shinyjs::runjs(
            str_glue(
              "$('#{ns('b1')}').find('.dot').last().addClass('head');"
            )
          )
          
        }
      })
      
      
      
      
      
      
      
    }
  )
}
