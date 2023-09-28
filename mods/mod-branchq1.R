branchq1_ui <- function(id){
  ns <- NS(id)
  tagList(
    h3("Add a branch new branch"),
    fluidRow(
      column(6, actionButton(ns("branch_btn"), "Make Branch")),
      column(6, 
             conditionalPanel(
               condition = "input.branch_btn > 0",
               selectInput(ns("slct_brn"),  "Branch", 
                           c("main", "New Branch"), selected = "New Branch"),
               # div(actionButton(ns("reset"), "Reset"),
               #     style = "margin-right: 0; margin-left: auto; "),
               ns = ns
             )
      )
    ),
    div(class = "graph", id = ns("graph"),
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
            class = c("dot", "head"),
            message = "Final commit"
          ),
          class = "branch", id = ns("b0"),
          style = "z-index: 3;"
        ),
        div(class = "slice", id = ns("commit_ls")),
    ),
    conditionalPanel(
      condition = "input.branch_btn > 0 & input.slct_brn == 'New Branch'",
      h3("Lets also add a commit to our branch"),
      fluidRow(
        column(width = 6, textInput(ns("input_text"), "Enter Commit Message:")),
        column(width = 6, br(), actionButton(ns("commit_btn"), "Commit"))
      ),
      ns = ns
    )
  )
}

branchq1_server <- function(id){
  moduleServer(
    id,
    function(input, output, session){
      ns <- session$ns
      observeEvent(input$branch_btn, {
        disable("branch_btn")
        
        insertUI(
          selector = paste0("#",  ns("commit_ls")),
          where = "beforeEnd",
          ui = div(
            class = c("branch", "topbranch"),
            id = paste0(ns("b"), input$branch_btn)
          )
        )
        
        
      })
      
      observeEvent(input$commit_btn, {
        if (input$input_text != "") {
          shinyjs::runjs(
            str_glue(
              "$('#{ns('graph')}').find('.head').first().removeClass('head');"
            )
            
          )
          branch_loc <- ifelse(input$slct_brn == "main", ns('b0'), ns('b1'))
          insertUI(
            selector = paste0("#", branch_loc),
            where = "beforeEnd",
            ui = div(
              class = c("dot", 'head'),
              message = input$input_text
            )
          )
          updateTextInput(session, "input_text", value = "")
        }
      })
      
      observeEvent(input$slct_brn, {
        if(input$slct_brn == "main"){
          shinyjs::runjs(
            str_glue(
              "$('#{ns('graph')}').find('.head').first().removeClass('head');
              $('#{ns('b0')}').find('.dot').last().addClass('head');
              "
            )
          )
        } else if(input$commit_btn > 0) {
          shinyjs::runjs(
            str_glue(
              "$('#{ns('graph')}').find('.head').first().removeClass('head');
              $('#{ns('b1')}').find('.dot').last().addClass('head');
              "
            )
          )
        }
      })

      
      
    }
  )
}
