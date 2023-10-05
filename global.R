### global.R

list.files(path = "mods/", full.names = TRUE) |> 
  map(source)

code_versions <- readRDS("data/code_versions.rds")