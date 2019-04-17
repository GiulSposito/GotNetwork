# import
source("./R/import/episodes.R")
source("./R/import/characters.R")

# importa os episodios
gotCharacters <- importCharacters("./data/characters.json") 
episodes <- importEpisodes("./data/episodes.json")

# pega campos que interessa e desanhinha as cenas
episodes %>% 
  unnest(scenes) %>% 
  mutate( sceneSequence = 1:nrow(.) ) %>% 
  select( seasonNum, episodeNum, sceneSequence, 
          sceneStart, charNetwork, episodeTitle ) -> scenes

library(tidygraph)
library(ggraph)
library(igraph)


# extrai uma sequencia de episodios 
# e gera um graph de ferquencia de parecimento (da aresta)

scene_index <- 3000
seq_size    <- 100

composeNetwork <- function(.scene_idx, .seq_size, .presenca, .scenes, .characters) {

  # gera um grafo
  .scenes %>% 
    # pega as ultimas (.seq_size) cenas a partir da cena (.scene_idx)
    filter( sceneSequence > max(0,.scene_idx-.seq_size),
            sceneSequence < .scene_idx ) %>% 
    # desaninha a tabela de arestas de personagens de cada uma delas
    # conta as aparicoes
    select(charNetwork) %>% 
    unnest() %>% 
    group_by(from,to) %>% 
    summarise( weight=n() ) %>% 
    ungroup() %>% 
    # filtra a participacao minima (.presenca) para o grafico
    filter(weight>.presenca) %>%
    arrange(from, to) %>% 
    # gera a rede
    graph.data.frame( directed = F ) %>% 
    as_tbl_graph() %>% 
    activate(nodes) %>% 
    # coloca a informacao de "casa" nos nodes (personagens)
    left_join(.characters, by="name") %>% 
    # adiciona centralidades
    mutate(
      degree = centrality_degree(normalized = T),
      between = centrality_betweenness(normalized = F)
    ) %>% 
    return()
}

# gera um grafo
scenes %>% 
  filter( sceneSequence > max(0,scene_index-seq_size),
          sceneSequence < scene_index ) %>% 
  select(charNetwork) %>% 
  unnest() %>% 
  group_by(from,to) %>% 
  summarise( weight=n() ) %>% 
  ungroup() %>% 
  filter(weight>3) %>%
  arrange(from, to) %>% 
  graph.data.frame( directed = F ) %>% 
  as_tbl_graph() -> g

g_crt <- g %>% 
  activate(nodes) %>% 
  left_join(gotCharacters, by="name")

# calculando centralidades
g_ctr <- g_crt %>% 
  mutate(
    degree = centrality_degree(normalized = T),
    between = centrality_betweenness(normalized = F)
  )

# fixando o layout previamente para todos os plots terem a mesma disposicao
composeNetwork(300, 100, 3, scenes, gotCharacters) %>% 
  create_layout(layout = "fr") %>% 
  ggraph(g_layout) +
    geom_edge_fan(aes(alpha=weight), width=1) +
    geom_node_point(aes(size=degree, color=house), alpha=.6) +
    geom_node_text(aes(label=name, size=degree), color="black") +
    theme_void() +
    scale_color_discrete(rainbow(18), labels=levels(gotCharacters$house)) +
    theme( legend.position = "none" )
  


