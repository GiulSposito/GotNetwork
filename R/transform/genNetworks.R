library(tidyverse)
library(tidygraph)
library(ggraph)
library(igraph)

# pega campos que interessa e desanhinha as cenas
extractScenes <- function(.episodes){
  episodes %>% 
    unnest(scenes) %>% 
    mutate( sceneSequence = 1:nrow(.) ) %>% 
    select( seasonNum, episodeNum, sceneSequence, 
            sceneStart, charNetwork, episodeTitle ) %>% 
    return()
}
 
# extrai uma sequencia de episodios 
# e gera um graph de ferquencia de parecimento (da aresta)
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


# plota uma rede
plotNetwork <- function(.sceneNetwork, .characterHouses){
  p <- .sceneNetwork %>% 
    create_layout(layout = "fr") %>% 
    ggraph(g_layout) +
    geom_edge_fan(aes(alpha=weight), width=1) +
    geom_node_point(aes(size=degree, color=house), alpha=.6) +
    geom_node_text(aes(label=name, size=degree), color="black") +
    theme_void() +
    scale_color_discrete(rainbow(18), labels=levels(.characterHouses$house)) +
    theme( legend.position = "none" )
  
  return(p)
}


  


