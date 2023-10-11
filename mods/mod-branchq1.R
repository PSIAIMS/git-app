branchq1_ui <- function(id){
  ns <- NS(id)
  tagList(
    h3("Add a branch new branch"),
    fluidRow(
       column(4, 
              br(),
             actionButton(ns("branch_btn"), "Make Branch")),
      column(4,
             conditionalPanel(
               condition = "input.branch_btn > 0",
               selectInput(ns("slct_brn"),  "Branch", 
                           c("main", "New Branch"), selected = "New Branch"),
               ns = ns
             )
             )
    ),
    div(class = "graph", id = ns("graph"),
        div(class = "slice", 
            div(class = c("branch")),
            div(
              class = c("branch", "main"), id = ns("b0"),
              "Main",
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
              
              style = "z-index: 3;"
            )),
        div(class = "slice", 
            id = ns("commit_ls"),
            div(class = "branch", id = ns("b1")),
            div(class = "branch", id = ns("b2"))
            ),
    ),
    conditionalPanel(
      condition = "input.branch_btn > 0",
      h3("Add Commits"),
      p("Now let's add two additional commits to each branch. 
        The commit will always be added to the branch you are currently on"),
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
      # indicates where the head of main is 
      main <- reactiveVal(ns("b0"))
      # Make a new branch 
      observeEvent(input$branch_btn, {
        disable("branch_btn")
        shinyjs::runjs(
          str_glue(
            "$('#{ns('b1')}').addClass('topbranch');
            "
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
          branch_loc <- ifelse(input$slct_brn == "main", ns('b2'), ns('b1'))
          
          insertUI(
            selector = paste0("#", branch_loc),
            where = "beforeEnd",
            ui = div(
              class = c("dot", 'head'),
              message = input$input_text
            )
          )
          
          if(input$slct_brn == "main"){
            main(ns('b2'))
            shinyjs::runjs(
              str_glue(
                "$('#{ns('b2')}').addClass('samebranch').addClass('main').addClass('right');
                $('#{ns('b0')}').addClass('left');"
              )
            )
          } else {
            shinyjs::runjs(
              str_glue(
                "$('#{ns('b1')}').addClass('newbranch');"
              )
            )
          }

          updateTextInput(session, "input_text", value = "")
        }
      })
      
      observeEvent(input$slct_brn, {
        if(input$slct_brn == "main"){
          shinyjs::runjs(
            str_glue(
              "$('#{ns('graph')}').find('.head').first().removeClass('head');
              $('#{main()}').find('.dot').last().addClass('head');
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
        } else {
          shinyjs::runjs(
            str_glue(
              "$('#{ns('graph')}').find('.head').first().removeClass('head');
              $('#{ns('b0')}').find('.dot').last().addClass('head');
              "
            )
          )
        }
      })
      
      
      
    }
  )
}
