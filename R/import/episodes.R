library(tidyverse)
library(jsonlite)
library(lubridate)

# funcao que processa os personagens de cada cena
processSceneCharacters <- function(listOfScenes){

  # para cada cena
  listOfScenes %>% 
    as_tibble() %>% 
    mutate(
      # calcula duracao da cena
      sceneDuration = difftime(
        strptime(sceneEnd, format = "%H:%M:%S"),
        strptime(sceneStart, format = "%H:%M:%S"),
        units = "mins"
      ) %>% as.numeric(),
      # adiciona na scena os caracteres mapeados como um graph
      charNetwork = map(characters, buildCharNetwork)
    ) %>% 
    return()
  
}

# funcao que transforma o dataframe de personagens em uma lista
# de arestas dos personagens
buildCharNetwork <- function(sceneCharacters){
  
  # verifica se a lista de caracteres:
  #  - nao eh vazia
  #  - tem mais de um personagem
  #  - tem o nome dos personagens
  if (!is.null(sceneCharacters)){
    if(nrow(sceneCharacters)>1) {
      if("name" %in% names(sceneCharacters)) {
        
        # tranforma o data.frame de personagens
        sceneCharNetwork <- sceneCharacters %>% 
          select(name) %>% 
          unlist() %>%     # pega a coluna nome e transforma num vetor
          sort() %>%       # e ordena
          combn(2) %>%     # combina dois a dois (arestas)
          t() %>%          # coloca de pe e transforma em um 'from'/'to'
          as_tibble() %>% 
          set_names(c("from","to"))
        
        return(sceneCharNetwork)
      } 
    } 
  } 
  return(tibble(from=vector("character",0),to=vector("character",0)))
}


# importa e trata os dados dos episodios

importEpisodes <- function(filename = "./data/episodes.json"){

  # arquivo de episodios
  eps_json <- fromJSON(filename)
  
  # adiciona alguns contadores e seleciona campos de interesse
  eps <- eps_json$episodes %>% 
    as_tibble() %>%
    select(-episodeLink, -episodeAirDate, -episodeDescription, -openingSequenceLocations) %>% 
    add_count(seasonNum, name="episodesCount") %>% 
    mutate( scenesCount = map_int(scenes, nrow) ) %>% 
    filter( scenesCount>0 ) %>%
    mutate( scenes = map(scenes, processSceneCharacters) )
  
  return(eps)  
}