# import
source("./R/import/episodes.R")
source("./R/import/characters.R")
source("./R/transform/genNetworks.R")

# importa os episodios
gotCharacters <- importCharacters("./data/characters.json") 
episodes <- importEpisodes("./data/episodes.json")
scenes <- extractScenes(episodes)

# season netwokr
scenes %>% 
  filter( seasonNum == 7 ) %>% 
  select( sceneDuration, charNetwork) %>% 
  unnest( charNetwork ) %>% 
  group_by(from, to) %>% 
  summarise( scenePresence = sum(sceneDuration) ) %>% 
  ungroup() %>% 
  mutate( presenceTop75pct = quantile(scenePresence, 0.90)) %>% 
  filter( scenePresence > presenceTop75pct ) %>% 
  # gera a rede
  graph.data.frame( directed = F ) %>% 
  as_tbl_graph() %>% # as_tibble() %>% left_join(.characters, by="name") %>% View()
  activate(nodes) %>% 
  # coloca a informacao de "casa" nos nodes (personagens)
  left_join(gotCharacters, by="name") %>% 
  mutate(
    degree = centrality_degree(weights = E(.)$scenePresence)
  ) %>% 
  create_layout(layout = "stress") %>% 
  ggraph() +
  geom_edge_fan(aes(alpha=scenePresence), width=1) +
  geom_node_point(aes(color=house,  size=degree, alpha=degree), alpha=.6) +
  geom_node_text(aes(label=name, size=degree), color="black") +
  theme_void() +
  scale_color_discrete(rainbow(18), labels=levels(gotCharacters$house)) +
  theme( legend.position = "none" )


obs <- rnorm(200)
qq <-  quantile(obs, probs=(.25))
obs_qq <- obs[ obs>= qq ]

summary(obs)
qq
summary(obs_qq)
