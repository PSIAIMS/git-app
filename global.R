### global.R

list.files(path = "mods/", full.names = TRUE) |> 
  map(source)

code_versions <- list.files("data/", full.names = TRUE) |> 
  keep(str_detect, "dot") |>
  map(read_file)
names(code_versions) <- list.files("data/") |>
  keep(str_detect, "dot") |>
  str_remove(".R")


dot_update <- function(loc,ns, session, current_dot){
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
    
    current_dot(loc)
    
  })
}
