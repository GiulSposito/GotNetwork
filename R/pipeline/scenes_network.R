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
  filter( sceneSequence >=10, sceneSequence < 1000 ) %>% 
  mutate(
    network = map( sceneSequence, composeNetwork, 100, 2, scenes, gotCharacters)
  ) -> scenesNetwork

scenes$sceneSequence

# fixando o layout previamente para todos os plots terem a mesma disposicao
composeNetwork(760, 100, 2, scenes, gotCharacters)

%>% 
  plotNetwork(gotCharacters)
