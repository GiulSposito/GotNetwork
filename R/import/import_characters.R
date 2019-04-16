library(tidyverse)
library(jsonlite)


gotChars <- fromJSON("./data/characters.json", flatten = F)

gotChars$characters %>% 
  names()

gotChars$characters %>% 
  select(characterName, royal, houseName, characterImageThumb) %>% 
  as.tibble() %>% 
  mutate(
    houseName = map(houseName, function(vct) {
      if (is.null(vct)) return(tibble(houseName=""))
      tibble(houseName=vct)
    })
  )

