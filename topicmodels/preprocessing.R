library(igraph)
library(stringr)
library(jsonlite)
library(corpus)
library(topicmodels)

pre_processing <- function(output_path){
  corpus <- SimpleCorpus(DirSource(output_path, encoding = "UTF-8"), control = list(language = "en"))
  corpus <- tm_map(corpus, stripWhitespace) # remove white spaces
  corpus <- tm_map(corpus, removeNumbers)   # remove numbers
  corpus <- tm_map(corpus, removePunctuation) # remove punctuation
  corpus <- tm_map(corpus, content_transformer(tolower)) # transform everything to lower case
  # Apply stopword lists
  corpus <- tm_map(corpus, removeWords, stopwords("en")) 
  corpus <- tm_map(corpus, removeWords, stopwords(language = "en", source = "smart"))
  corpus <- tm_map(corpus, removeWords, stopwords(language = "en", source = "snowball"))
  corpus <- tm_map(corpus, removeWords, stopwords(language = "en", source = "stopwords-iso"))
  corpus <- tm_map(corpus, removeWords, c("common","class","apache","author","equals","equal","instance","return","false","true","src","thrown","throw","throws","breaks","getter","setter","main","run","url","set","abstract", "assert","boolean","break","byte", "case","catch","char","continue","default","do","double","else","enum","extends","final","finally","float","for","if","implements","import","instanceof","int","interface","long","native","new","package","private","protected","public","return","short","static","strictfp","super","syncronized","this","throw","throws","transient","try","void","volatile","while","goto","const","java", "class", "import","github", "http", "html", "for", "while", "then", "private", "public", "protected", "try", "catch","instead","https","http","href","ibm","throw","throws","clone","javadoc","bug", "string","method","list","array","object","println","char","obj","junit","switch","case","javadoc","args"))
  # Applying stemming
  corpus <- tm_map(corpus, stemDocument, language = "english")
  
  # Build the document-by-term matrix
  tdm <- DocumentTermMatrix(corpus, control = list(stemming = TRUE, wordLengths = c(2,Inf), bounds = list(global=c(2,Inf))))
  
  # Apply tf-idf weighting schema
  tdm2 <- weightSMART(tdm, "ntn")
  tdm$v <- as.integer(round(tdm2$v))
  
  return(tdm)
}

convert2SparseMatrix <- function(dtm) {
  matrix <- Matrix(0, nrow = dtm$nrow, ncol = dtm$ncol, sparse = TRUE)
  
  for (index in 1:length(dtm$i)) {
    matrix[dtm$i[index], dtm$j[index]] <- dtm$v[index]
  }
  
  colnames(matrix) <- colnames(dtm)
  return(matrix)
}

shuffle_tdm <- function(tdm){
  n = nrow(tdm)
  for (index in 1:20){
    other_index <- sample(1:n, 1)
    
    temp <- tdm[index, ]
    tdm[index, ] <- tdm[other_index, ]
    tdm[other_index, ] = temp
  }
  return(tdm)
}
