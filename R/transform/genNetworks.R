# importa os episodios
episodes <- importEpisodes("./data/episodes.json")

# pega campos que interessa e desanhinha as cenas
episodes %>% 
  unnest(scenes) %>% 
  mutate( sceneSequence = 1:nrow(.) ) %>% 
  select( seasonNum, episodeNum, sceneSequence, 
          sceneStart, charNetwork, episodeTitle ) -> scenes

# extrai uma sequencia de episodios 
# e gera um graph de ferquencia de parecimento (da aresta)

scene_index <- 2880
seq_size    <- 100

scenes %>% 
  filter( sceneSequence > max(0,scene_index-seq_size),
          sceneSequence < scene_index ) %>% 
  select(charNetwork) %>% 
  unnest() %>% 
  group_by(from,to) %>% 
  summarise( weight=n() ) %>% 
  ungroup() %>% 
  filter(weight>2) %>%
  arrange(from, to) %>% 
  graph.data.frame( directed = F ) %>% 
  as_tbl_graph() -> g

# fixando o layout previamente para todos os plots terem a mesma disposicao
g_layout <- create_layout(g, layout = "fr")

# plotando o graph
ggraph(g_layout) +
  geom_edge_fan(aes(alpha=weight), width=1) +
  geom_node_point(color="blue",alpha=0.8, size=8) +
  geom_node_text(aes(label=name), color="black", size=4) +
  theme_void() +
  theme( legend.position = "none" )
  
