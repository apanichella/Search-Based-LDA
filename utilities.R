library(igraph)
library(stringr)
library(jsonlite)

split_document <- function(files, output_path){
  for (file in files){
    xmlfile <- xmlParse(paste(input_path, file, sep=""))
    rootnode <- xmlRoot(xmlfile)
    size = xmlSize(rootnode)
    
    for (i in 1:size){
      bug_id = xmlGetAttr(rootnode[i]$bug, name="id")
      bug_report = as.character(xmlToDataFrame(rootnode)$buginformation[i]);
      # camel case splitting and other transformation
      text <- to_sentence_case(bug_report)
      # replace punctuation with spaces
      text <- gsub("[[:punct:]]", " ", text)
      text <- gsub("username", "user name", text)
      write.table(text, paste(output_path, bug_id, ".txt", sep=""), row.names = FALSE, col.names = FALSE)
    }
  }
}

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
  corpus <- tm_map(corpus, removeWords, c("abstract", "assert","boolean","break","byte", "case","catch","char","continue","default","do","double","else","enum","extends","final","finally","float","for","if","implements","import","instanceof","int","interface","long","native","new","package","private","protected","public","return","short","static","strictfp","super","syncronized","this","throw","throws","transient","try","void","volatile","while","goto","const","java", "class", "import","github", "http", "html", "for", "while", "then", "private", "public", "protected", "try", "catch","instead","https","http","href","ibm","throw","throws","clone","javadoc","bug", "string","method","list","array","object"))
  # Applying stemming
  corpus <- tm_map(corpus, stemDocument, language = "english")
  
  # 7. Build the document-by-term matrix
  tdm <- DocumentTermMatrix(corpus, control = list(stemming = TRUE, wordLengths = c(2,Inf), bounds = list(global=c(2,Inf))))
  
  return(tdm)
}

save_results <- function(system, approach, file_output, x, top5, top10, top15, top20, time){
  result_frame <- data.frame(system, approach, x[1], x[2], x[3], x[4], top5, top10, top15, top20, time)
  colnames(result_frame) <- c("System","Approach","x1","x2","x3","x4","TOP-5","TOP-10","TOP-15","TOP-20","Time")
  if (file.exists(file_output))
    write.table(result_frame, file_output, sep = ",", col.names = F, append = T)
  else
    write.table(result_frame, file_output, sep = ",", col.names = T, append = T)
}

oracle2graph <- function(path) {
  oracle <- fromJSON(path)
  duplicate_graph <- make_empty_graph(directed = FALSE) 
  data <- oracle[[1]]
  for (i in 1:nrow(data)){
    v1 = vertex(as.character(data[i,1]))
    if (!as.character(data[i,1]) %in% V(duplicate_graph)$name)
      duplicate_graph <- duplicate_graph + vertex(as.character(data[i,1]))
    if (!as.character(data[i,2]) %in% V(duplicate_graph)$name)
      duplicate_graph <- duplicate_graph + vertex(as.character(data[i,2]))
    duplicate_graph <- duplicate_graph + edge(as.character(data[i,1]), as.character(data[i,2]))
  }
  return(duplicate_graph)
}

topk <- function(graph, distances, k){
  clusters <- components(graph)
  counter <- 0
  numerator <- 0 
  for (doc in rownames(distances)){
    doc_name <- unlist(strsplit(doc, split='.', fixed=TRUE))[1]
    if (doc_name %in% V(graph)$name){
      get_cluster <- clusters$membership[names(clusters$membership) == doc_name] # cluster of the document 'doc_name'
      cluster_members <- names(clusters$membership[clusters$membership == get_cluster]) # all members in the same cluster
      cluster_members <- cluster_members[cluster_members!= doc_name]
      rank <- distances[rownames(distances) == doc, ]
      rank <- sort(rank, decreasing = FALSE)
      rank <- str_remove(names(rank), '[.]txt')
      matches <- match(cluster_members, rank)
      in_top_k = (matches <= k)
      if (any(in_top_k)){
        numerator <- numerator + 1
      }
      counter <- counter + 1
      # print(min(matches))
    } 
  }
  return(numerator/counter)
}

evaluate_LDA <- function(x){
  numero_topic<-round(x[1]) #x[1] = number of topics k
  iteration<-round(x[2])    #x[2] =  number of gibbs iteration
  pAlpha<-x[3]              #x[3] = Alpha
  pDelta<-x[4]              #x[4] = Beta
  
  ldm <- LDA(tdm, method="Gibbs", control = list(alpha=pAlpha, delta=pDelta, iter=iteration, seed=5, nstart=1), k = numero_topic)  # k = num of topics
  pldm <- posterior(ldm)
  document2topic <- pldm$topics
  
  distances <- as.matrix(dist(document2topic, method = "euclidean", diag = T, upper = T))
  rownames(distances) = rownames(document2topic)
  colnames(distances) = rownames(document2topic)
  return(distances)
}

