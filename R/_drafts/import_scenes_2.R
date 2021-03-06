library(tidyverse)
library(jsonlite)
library(tidygraph)
library(igraph)
library(ggraph)

# funcao que processa os personagens de cada cena
processSceneCharacters <- function(listOfScenes){

  # para cada cena
  listOfScenes %>% 
    as_tibble() %>% 
    mutate(
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
          as.tibble() %>% 
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
    as.tibble() %>%
    select(-episodeLink, -episodeAirDate, -episodeDescription, -openingSequenceLocations) %>% 
    add_count(seasonNum, name="episodesCount") %>% 
    mutate( scenesCount = map_int(scenes, nrow) ) %>% 
    filter( scenesCount>0 ) %>%
    mutate( scenes = map(scenes, processSceneCharacters) )
  
  return(eps)  
}


# retorna a rede de characters de uma cena de um episodio e uma temporada
getSceneCharNetwork <- function(.episodes, .season, .episode, .scene) {
  .episodes %>% 
    filter(
      seasonNum  == .season, 
      episodeNum == .episode
    ) -> episode
  
  cena <- episode$scenes[[1]]
  
  cena[.scene,]$charNetwork[[1]] %>% 
    return()
}

getSceneCharNetwork(eps,6,3,1)


.episodes <- eps
.season <- 1
.episode <- 5
.scene <- 10

# fixando o layout previamente para todos os plots terem a mesma disposicao
g_layout <- create_layout(g, layout = "kk")

# plotando o graph
ggraph(g_layout) +
  geom_edge_fan() +
  geom_node_point(color="blue",alpha=0.8, size=8) +
  geom_node_text(aes(label=name), color="black") +
  theme_void() +
  theme( legend.position = "none" )
