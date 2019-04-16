library(tidyverse)
library(jsonlite)
library(tidygraph)
library(igraph)
library(ggraph)

# extrai os personagens de uma cena criando uma rede (edges list)
characterNetwork <- function(.scene){
  
  # cena sem personagens
  if( !("characters" %in% names(.scene)) ) return(
    tibble(
      from="",
      to=""
    )
  )
  
  # pega a lista de personagens
  .scene %>% 
    select(characters) %>% 
    unlist() -> chars
  
  # se a lista tem menos que 2 pessoa, cria um dataset vazio
  if (length(chars)<2) return(
    tibble(
      from="",
      to=""
    )
  )
  
  # se tem mais de um cria uma rede combinando os caractes da cena
  chars %>% 
    sort() %>% 
    combn(2) %>% 
    t() %>% 
    as.tibble() %>% 
    set_names(c("from","to"))
}


# arquivo de episodios
eps_json <- fromJSON("./data/episodes.json")

# adiciona alguns contadores e seleciona campos de interesse
eps <- eps_json$episodes %>% 
  as.tibble() %>%
  add_count(seasonNum, name="episodesCount") %>% 
  mutate(
    scenesCount = map_int(scenes, nrow)
  ) %>%  
  select(-episodeLink, -episodeAirDate, 
         -episodeDescription, -openingSequenceLocations) %>% 
  mutate(
    scenesNetwork = map(eps$scenes, characterNetwork)
  )





eps %>% 
  
  
  
  
  mutate(
    # para cada episodio
    void = map(scenes, function(.scenes){
      
      .scenes %>% 
        class() %>% 
        print()
          
    
    })
  )

.scenes <- eps[1,]$scenes[[1]]

.scenes %>% tibble()

.scene <- scene[1,]

.scene %>% 
  as.tibble()

eps[1,]$scenes[[1]] %>% 
  as.tibble() -> cena

cena %>% nrow()

cena$characters[[20]] %>% 
  select(name) %>% 
  unlist() %>% 
  combn(2) %>% 
  t() %>% 
  graph.edgelist(directed = FALSE) %>% 
  as_tbl_graph() -> g

# fixando o layout previamente para todos os plots terem a mesma disposicao
g_layout <- create_layout(g, layout = "kk")

# plotando o graph
ggraph(g_layout) +
  geom_edge_fan() +
  geom_node_point(color="blue",alpha=0.8, size=8) +
  geom_node_text(aes(label=name), color="black") +
  theme_void() +
  theme( legend.position = "none" )
