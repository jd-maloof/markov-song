

markovsong <- function(inputCorpus, inputNWords){
  
  require(markovchain)
  
  #################################
  ## 1. Fit markov chain to corpus
  fit_markov <- inputCorpus %>% markovchainFit(method = "laplace")
  set.seed(2202)
  
  #################################
  ## 2. Generate Lyrics
  newSong <-round(rnorm(1, mean(inputNWords), sd(inputNWords))) %>%
    markovchainSequence(markovchain = fit_markov$estimate)
  
  return(newSong)
}
