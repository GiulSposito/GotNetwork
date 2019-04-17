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
g_layout <- create_layout(g_ctr, layout = "fr")

# plotando o graph
ggraph(g_layout) +
  geom_edge_fan(aes(alpha=weight), width=1) +
  geom_node_point(aes(size=degree, color=house), alpha=.6) +
  geom_node_text(aes(label=name, size=degree), color="black") +
  theme_void() +
  scale_color_discrete(rainbow(18), labels=levels(gotCharacters$house)) +
  theme( legend.position = "none" )
  


