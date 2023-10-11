# This file is just used to quickly make all the images in case 
# we need to refresh them in the future

library(tidyverse)
list.files("data/", full.names = TRUE) |> 
  keep(str_detect, "dot") |>
  map(source)
