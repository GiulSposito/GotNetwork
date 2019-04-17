# import
source("./R/import/episodes.R")
source("./R/import/characters.R")
source("./R/transform/genNetworks.R")

# importa os episodios
gotCharacters <- importCharacters("./data/characters.json") 
episodes <- importEpisodes("./data/episodes.json")
scenes <- extractScenes(episodes)

# compondo as animacoes
scenes %>% 
  select( -sceneStart ) %>% 
  filter( sceneSequence >= 11 ) %>% 
  mutate(
    network = map( sceneSequence, composeNetwork, 100, 3, scenes, gotCharacters)
  ) -> scenesNetwork

saveRDS(scenesNetwork, "./data/sceneNetwork.rds")

# fixando o layout previamente para todos os plots terem a mesma disposicao
# composeNetwork(11, 100, 3, scenes, gotCharacters) %>% 
#   plotNetwork(gotCharacters)

scenesIndexes <- scenesNetwork$sceneSequence


for(.i in scenesIndexes[1:200]){

  # compoe o nome do arquivo de imagem
  img_name <- paste0( "./images/scene_",formatC(.i, width = 4, flag="0"), ".png" )
  
  # abre o device de imagem
  png(img_name)
  
  # plota
  print(plotNetwork(scenesNetwork[.i,]$network[[1]], gotCharacters))
  
  # fecha e salva imagem
  dev.off()
  
}


images_name <- scenesIndexes %>% 
  map_chr(function(.i, .scns, .chars){
    
    # compoe o nome do arquivo de imagem
    img_name <- paste0( "./images/scene_",formatC(.i, width = 4, flag="0"), ".png" )
    
    # abre o device de imagem
    png(img_name)
    
    # plota
    .scns[.i,]$network[[1]] %>% 
      plotNetwork(.chars)
    
    # fecha e salva imagem
    dev.off()
    
    return(img_name)
  }, scenesNetwork, gotCharacters)


.scns[71,]$network[[1]] %>% 
  create_layout(layout = "gem") %>% 
  ggraph(g_layout) +
  geom_edge_fan(aes(alpha=weight), width=1) +
  geom_node_point(aes(size=degree, color=house), alpha=.6) +
  geom_node_text(aes(label=name, size=degree), color="black") +
  theme_void() +
  scale_color_discrete(rainbow(18), labels=levels(gotCharacters$house)) +
  theme( legend.position = "none" )


