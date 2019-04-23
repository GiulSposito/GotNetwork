library(tidyverse)
library(tidygraph)
library(ggraph)
library(igraph)
# layouts alternativos para ggraph
library(graphlayouts)

# pega campos que interessa e desanhinha as cenas
extractScenes <- function(.episodes){
  episodes %>% 
    unnest(scenes) %>% 
    mutate( sceneSequence = 1:nrow(.) ) %>% 
    select( seasonNum, episodeNum, sceneSequence, 
            sceneStart, sceneDuration, charNetwork, episodeTitle ) %>% 
    return()
}
 
# extrai uma sequencia de episodios 
# e gera um graph de ferquencia de parecimento (da aresta)
composeNetwork <- function(.scene_idx, .seq_size, .presenca, .scenes, .characters) {
  
  # checa qual a cena
  print(.scene_idx)

  # gera um grafo
  .scenes %>% 
    # pega as ultimas (.seq_size) cenas a partir da cena (.scene_idx)
    filter( sceneSequence > max(1, .scene_idx-.seq_size),
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
    as_tbl_graph() %>% # as_tibble() %>% left_join(.characters, by="name") %>% View()
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
plotNetwork <- function(.sceneNetwork, .characterHouses, .epTitle="", .epSubtitle=""){
  set.seed(1975)
  p <- .sceneNetwork %>% 
    create_layout(layout = "stress") %>% 
    ggraph(g_layout) +
    geom_edge_fan(aes(alpha=weight), width=1) +
    geom_node_point(aes(color=house, alpha=degree), size=6, alpha=.6) +
    geom_node_text(aes(label=name), color="black") +
    ggtitle(.epTitle, .epSubtitle) +
    theme_void() +
    scale_color_discrete(rainbow(18), labels=levels(.characterHouses$house)) +
    theme( legend.position = "none", 
           title = element_text(size=18))
  
  return(p)
}


  


