library(tidyverse)
library(jsonlite)

# funcao que importa a lista de personagens
importCharacters <- function(filename="./data/characters.json"){

  gotChars <- fromJSON(filename, flatten = F)
  
  gotChars$characters %>% 
    select(characterName, royal, houseName, characterImageThumb) %>% 
    as_tibble() %>% 
    mutate(
      houseName = map_chr(houseName, function(vct) {
        if (is.null(vct)) return(NA)
        if(length(vct)>1) vct <- vct %>% sort() %>% paste(collapse = "-")
        vct
      })
    ) %>% 
    select( characterName, houseName ) %>% 
    mutate( houseName = ifelse(is.na(houseName), "none", houseName)) %>% 
    mutate( houseName = as.factor(houseName) ) %>% 
    arrange(characterName) %>% 
    set_names(c("name","house")) %>% 
    distinct() %>% 
    return()
}


