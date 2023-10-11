### global.R

list.files(path = "mods/", full.names = TRUE) |> 
  map(source)

code_versions <- readRDS("data/code_versions.rds")







dot_update <- function(loc,ns, session){
  onclick(loc, { 
    
    shinyjs::runjs(
      str_glue(
        "$('#{ns('graph')}').find('.head').first().removeClass('head');
         $('#{ns(loc)}').addClass('head');
        Shiny.setInputValue(\"{ns('head')}\", '{loc}');
        "
      )
    )
    updateAceEditor(session, "code_box", 
                    value = code_versions[loc],
                    theme = "chrome",
                    mode = "r")
    
  })
}
