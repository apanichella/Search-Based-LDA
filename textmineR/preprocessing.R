library(igraph)
library(stringr)
library(jsonlite)
library(corpus)
library(textmineR)

pre_processing <- function(output_path){
  corpus <- SimpleCorpus(DirSource(output_path, encoding = "UTF-8"), control = list(language = "en"))
  #corpus <- tm_map(corpus, stripWhitespace) # remove white spaces
  #corpus <- tm_map(corpus, removeNumbers)   # remove numbers
  #corpus <- tm_map(corpus, removePunctuation) # remove punctuation
  #corpus <- tm_map(corpus, content_transformer(tolower)) # transform everything to lower case
  # Apply stopword lists
  #corpus <- tm_map(corpus, removeWords, stopwords("en")) 
  #corpus <- tm_map(corpus, removeWords, stopwords(language = "en", source = "smart"))
  #corpus <- tm_map(corpus, removeWords, stopwords(language = "en", source = "snowball"))
  #corpus <- tm_map(corpus, removeWords, stopwords(language = "en", source = "stopwords-iso"))
  #corpus <- tm_map(corpus, removeWords, c("abstract", "assert","boolean","break","byte", "case","catch","char","continue","default","do","double","else","enum","extends","final","finally","float","for","if","implements","import","instanceof","int","interface","long","native","new","package","private","protected","public","return","short","static","strictfp","super","syncronized","this","throw","throws","transient","try","void","volatile","while","goto","const","java", "class", "import","github", "http", "html", "for", "while", "then", "private", "public", "protected", "try", "catch","instead","https","http","href","ibm","throw","throws","clone","javadoc","bug", "string","method","list","array","object"))
  # Applying stemming
  #corpus <- tm_map(corpus, stemDocument, language = "english")
  
  # Convert the corpus to a vector of documents
  text <- as_corpus_text(corpus)
  
  # 7. Build the document-by-term matrix
  dtm <- CreateDtm(text, doc_names = names(text), ngram_window = c(1, 1),
                   stopword_vec = c(stopwords::stopwords("en"),
                                    stopwords::stopwords(source = "smart"),
                                    stopwords::stopwords(source = "snowball"),
                                    stopwords::stopwords(source = "stopwords-iso"),
                                    c("abstract", "assert","boolean","break","byte", "case","catch","char","continue","default","do","double","else","enum","extends","final","finally","float","for","if","implements","import","instanceof","int","interface","long","native","new","package","private","protected","public","return","short","static","strictfp","super","syncronized","this","throw","throws","transient","try","void","volatile","while","goto","const","java", "class", "import","github", "http", "html", "for", "while", "then", "private", "public", "protected", "try", "catch","instead","https","http","href","ibm","throw","throws","clone","javadoc","bug", "string","method","list","array","object") 
                                    ), lower = TRUE,
                   remove_punctuation = TRUE, remove_numbers = TRUE,
                   stem_lemma_function = function(x) SnowballC::wordStem(x, "porter"), verbose = TRUE)
  
  # get a tf-idf matrix
#  tf_sample <- TermDocFreq(dtm)
#  tf_sample$idf[ is.infinite(tf_sample$idf) ] <- 0 # fix idf for missing words
#  tf_sample$idf[ is.na(tf_sample$idf) ] <- 0
#  tf_idf <- t(t(dtm) * tf_sample$idf)
  
  return(dtm)
}
