
remotesq2_ui <- function(id){
  ns <- NS(id)
  tagList(
    h3("Working with Remotes"),
    p("Let's walk through Sylvie's workflow to make the updates for Camille. 
      First we are going to clone the repository. Then, need to 'checkout' the tagged commit by clicking on it. 
      After, we can make a new branch, add a commit and finally push back to the remote so Camille can see the updates."),
    div(class = "box", id = ns("remotes"),
        div("Remote", style = "margin-bottom: 3px; width : 90%;" ),
        div(class = "graph", id = ns("graphr"),
            div(class = "slice", id = ns("slice1r"),
                div(class = c("branch", "main", "left"), 
                    "Main",
                    div(
                      class = "dot",
                      message = "Code V1"
                    ),
                    div(
                      class = c("dot", "tagged"),
                      message = "Code V2"
                    ),
                    style = "z-index: 3;"
                )
            ),
            div(class = "slice", id = ns("slice2r"), style = "z-index: 2;",
                div(
                  div(
                    class = "dot",
                    message = "Code V3"
                  ),
                  div(
                    class = c("dot"),
                    message = "Code V4"
                  ),
                  class = c("branch", "samebranch", "main", "right")
                )
            )
        )
    ),
    div(class = "box", id = ns("local"), 
        fluidRow(column(2, "Local "),
                 column(2, actionButton(ns("clone"), "Clone")),
                 column(3, actionButton(ns("pull"), "Pull"), actionButton(ns("push"), "Push")),
                 column(4, 
                        actionButton(ns("branch"), "Branch"),
                        actionButton(ns("commit"), "Commit") ),
                 style = "margin-bottom: 3px; width : 90%;"
        )
        # )
        
    )
    
    
  )
}

remotesq2_server <- function(id){
  moduleServer(
    id,
    function(input, output, session){
      ns <- session$ns
      n_commits <- reactiveVal(0)
      remote_branch <- reactiveVal(FALSE)
      local_branch <- reactiveVal(FALSE)
      remote_commit <- reactiveVal(0)
      current_dot <- reactiveVal("dot4")
      
      observeEvent(input$close,{
        removeModal()
      })
      
      observeEvent(input$clone, {
        insertUI(
          selector = paste0("#",  ns("local")),
          where = "beforeEnd",
          ui = div(class = "graph", id = ns("graph"),
                   div(class = "slice", id = ns("slice1l"),
                       div(class = c("branch", "main", "left"), 
                           "Main",
                           div(
                             id = ns('dot1'),
                             class = "dot",
                             message = "Code V1"
                           ),
                           div(
                             id = ns('dot2'),
                             class = c("dot", "tagged"),
                             message = "Code V2"
                           ),
                           style = "z-index: 3;"
                       )
                   ),
                   div(class = "slice", id = ns("slice2l"), style = "z-index: 2;",
                       div(
                         div(
                           id = ns('dot3'),
                           class = "dot",
                           message = "Code V3"
                         ),
                         div(
                           id = ns('dot4'),
                           class = c("dot", 'head'),
                           message = "Code V4"
                         ),
                         class = c("branch", "samebranch", "main", "right")
                       )
                   )
          )
        )
        
        disable("clone")
        
      })
      
      observeEvent(input$branch, {
        
        if(input$clone > 0 & current_dot() == "dot2"){
          insertUI(
            selector = paste0("#",  ns("slice1l")),
            where = "afterBegin",
            ui = div(class= "branch")
          )
          
          insertUI(
            selector = paste0("#",  ns("slice2l")),
            where = "afterBegin",
            ui = div(class = c("branch", "topbranch"), id = ns("b1l"))
          )
          disable("branch")
          local_branch(TRUE)
        } else if(input$clone == 0){
          showModal(
            modalDialog(
              title = "Oops",
              p("You need to clone before you can branch"),
              footer=tagList(
                actionButton(ns('close'), 'close')
              )
            )
          )
        } else if(current_dot() != "dot2"){
          showModal(
            modalDialog(
              title = "Oops",
              p("You need to 'checkout' to the tagged commit, by clicking on it before you can branch."),
              footer=tagList(
                actionButton(ns('close'), 'close')
              )
            )
          )
        }
      })
      
      observeEvent(input$commit, {
        
        if(local_branch()){
          if(current_dot() %in% c("dot2", "dot1-b1")){
            shinyjs::runjs(
              str_glue(
                "$('#{ns('graph')}').find('.head').first().removeClass('head');
              $('#{ns('b1l')}').addClass('analysis2');"
              )
              
            )
            
            insertUI(
              selector = paste0("#",  ns("b1l")),
              where = "beforeEnd",
              ui = div(
                id = ns(paste0('dot',input$commit,'-b1')),
                class = c("dot", 'head'),
                message = ifelse(n_commits() == 0, "Journal Update",
                                 "Final Journal Update")
              )
              
            )
            n_commits(n_commits()+1)
            if(n_commits() >= 2){
              disable("commit")
            }
          } else {
            showModal(
              modalDialog(
                title = "Oops",
                p("For now we are only adding commits to our new branch, try again"),
                footer=tagList(
                  actionButton(ns('close'), 'close')
                )
              )
            )
          }
          
        } else {
          showModal(
            modalDialog(
              title = "Oops",
              p("You need to branch before you can commit"),
              footer=tagList(
                actionButton(ns('close'), 'close')
              )
            )
          )
          
        }
      })
      
      observeEvent(input$push,{
        if(input$branch > 0 & !remote_branch()){
          insertUI(
            selector = paste0("#",  ns("slice1r")),
            where = "afterBegin",
            ui = div(class= "branch")
          )
          
           if(n_commits() > 0) {
             br_classes <- c("branch", "topbranch", "analysis2")
           } else {
             br_classes <- c("branch", "topbranch")
           }
          
          insertUI(
            selector = paste0("#",  ns("slice2r")),
            where = "afterBegin",
            ui = div(class = br_classes, id = ns("b1r"))
          )
          remote_branch(TRUE)
        }
        
        if(input$branch > 0 & n_commits() >= 1 & remote_commit() == 0){
          insertUI(
            selector = paste0("#",  ns("b1r")),
            where = "beforeEnd",
            ui = div(
              class = c("dot"),
              message = "Journal Update"
            )
          )
          
          shinyjs::runjs(
            str_glue(
              "$('#{ns('b1r')}').addClass('analysis2');"
            )
            
          )
          
          remote_commit(1)
        }
            
          
        if(input$branch > 0 & n_commits() >= 2 & remote_commit() < 2){
          insertUI(
            selector = paste0("#",  ns("b1r")),
            where = "beforeEnd",
            ui = div(
              class = c("dot"),
              message = "Final Journal Update"
            )
          )
          remote_commit(2)
        }
        
      })
      
      dot_update("dot1", ns, session, current_dot)
      dot_update("dot2", ns, session, current_dot)
      dot_update("dot3", ns, session, current_dot)
      dot_update("dot4", ns, session, current_dot)
      dot_update("dot1-b1", ns, session, current_dot)
      dot_update("dot2-b1", ns, session, current_dot)
  
      
    }
  )
}
