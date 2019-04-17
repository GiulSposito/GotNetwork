# import
source("./R/import/episodes.R")
source("./R/import/characters.R")
source("./R/transform/genNetworks.R")

# importa os episodios
gotCharacters <- importCharacters("./data/characters.json") 
episodes <- importEpisodes("./data/episodes.json")
scenes <- extractScenes(episodes)

# fixando o layout previamente para todos os plots terem a mesma disposicao
composeNetwork(1200, 100, 5, scenes, gotCharacters) %>% 
  plotNetwork(gotCharacters)
