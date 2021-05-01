biblioteca <- function(x) {
  
  for (i in seq_along(x)) {
    
    if (nzchar(system.file(package = x[i])) == FALSE) {
      
      message(paste0("installing ", x[i], "..."))
      install.packages(x[i])
      
    }
    
    message(paste0("loading `", x[i], "` library"))
    suppressPackageStartupMessages(library(x[i], character.only = T))
    
  }
  
}

biblioteca(c("here", "tidyverse", "janitor", "EpiEstim", "glue", "ggtext"))

set_seed <- 46342
set.seed(set_seed)

# suppressPackageStartupMessages({
#   library(here)
#   library(tidyverse)
#   library(janitor)
#   library(EpiEstim)
#   library(glue)
#   library(ggtext)
# })

source(here("code", "functions.R"))