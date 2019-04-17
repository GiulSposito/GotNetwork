library(tidyverse)
library(jsonlite)

# funcao que importa a lista de personagens
importCharacters <- function(filename="./data/characters.json"){

  gotChars <- fromJSON(filename, flatten = F)
  
  gotChars$characters %>% 
    select(characterName, royal, houseName, characterImageThumb) %>% 
    as.tibble() %>% 
    mutate(
      houseName = map(houseName, function(vct) {
        if (is.null(vct)) return(tibble(houseName=vector("character",0)))
        tibble(houseName=vct)
      })
    ) %>% 
    unnest() %>%
    select( characterName, houseName ) %>% 
    mutate( houseName = as.factor(houseName) ) %>% 
    arrange(characterName) %>% 
    set_names(c("name","house")) %>% 
    return()

}


