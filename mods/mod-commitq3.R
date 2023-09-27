commitq3_ui <- function(id){
  ns <- NS(id)
  tagList(
    h3("How could have Camille organised her work better?"),
    p('1 - Committing all important changes'),
    p('2 - Tagging each release: here her submission to the journal'),
    p('3 - Collaborating with Sylvie using git'),
    
  
    div(
      div(
        class = "dot",
        message = "Code V1"
      ),
      div(
        class = "dot",
        message = "Code V2, Tag:Submission",
        style="background-color:#FF4F00"      ),
      div(
        class = "dot",
        message = "Code V3"
      ),
      div(
        class = "dot",
        message = "Integration of Sylvie work"
      ),
      class = "branch"
    ),
    
    
    fluidRow(
      column(4,
             textInput(ns("input_text"), "Commit Message:")),
      column(4,
             textInput(ns("input_tag"), "Tag Name:"))),
    
    
    fluidRow(
      column(4,
             actionButton(ns("commit_btn"), "Commit")),
      column(4,
             actionButton(ns("tag_btn"), "Commit and tag")),
      column(4, actionButton(ns("undo_btn"), "Undo"))),
    div(id = ns("answers"),
        div(id = ns("commit_ls"), class = "branch")
    )
  )
}

commitq3_server <- function(id){
  moduleServer(
    id,
    function(input, output, session){
      ns <- session$ns
      
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
        }
      })
      
      observeEvent(input$tag_btn, {
        
        if (input$input_tag != "" & input$input_text != "" ) {
          insertUI(
            selector = paste0("#",  ns("commit_ls")),
            where = "beforeEnd",
            ui = div(
              class = "dot",
              message = paste0(input$input_text, ', Tag: ',  input$input_tag), 
              style="background-color:#FF4F00"
              
            )
          )
          updateTextInput(session, "input_text", value = "")
          updateTextInput(session, "input_tag", value = "")
          
        }
      })

      
      
      observeEvent(input$undo_btn, {
        removeUI(selector = paste0("#",  ns("commit_ls")))
        insertUI(
          selector = paste0("#",  ns("answers")),
          where = "beforeEnd",
          ui = div(id = ns("commit_ls"), class = "branch")
        )
        
      })
      
    }
  )
}
