
scrapeLyrics <- function(bandname){
  
  require(rvest)
  require(tidyverse)
  require(tidytext)
      
  ##################################################
  ## 1. Scrape Song titles from a particular artist
  name <- bandname %>%
    paste("lyrics", sep= " ") %>%
    str_replace_all(pattern = " ", replacement = "-") %>%
    tolower() %>%
    as.vector()
  
  firstletter <- substr(name, 1, 1)
  
  artist <- read_html(paste("https://azlyrics.biz", firstletter, name, "", sep = "/"))
  
  songs <- html_elements(artist, "ol")
  titles <- songs %>% 
    html_elements("a") %>% 
    html_attr("href") %>% 
    as.character()
  
  if(length(titles) == 0) {
    warning("Unable to find songs for requested artist")
  }
  
  ############################################
  ## 2. Initialize corpus and number of words
  
  corpus <- vector()
  nwords <- vector()
  
  ###########################################################
  ## 3. Scrape lyrics from song titles obtained in block A.1
  
  for (i in 1:length(titles)){
    temp <- read_html(titles[i]) %>% 
      html_elements("p") %>% 
      html_text2 %>%
      as.character
    temp <- temp[-length(temp)] %>%
      str_replace_all(pattern = "\n", " ") %>%
      str_remove_all(pattern = "(?![.,!])[[:punct:]]") %>%
      str_split(" ") %>%
      unlist()
    if(i == 1){
      corpus <- temp
      nwords <- length(temp)
    } else{
      corpus <- append(corpus, temp, length(corpus))
      nwords <- append(nwords, length(temp), length(nwords))
    }
    rm(temp)
  }
  rm(i)
  
  output <- list(corpus, nwords)
  
  return(output)
}







