if (!require(corpus)){ install.packages("corpus")  }

library(igraph)
library(stringr)
library(jsonlite)
library(corpus)

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

save_results <- function(system, lib, algorithm, fitness, file_output, x, top5, top10, top15, top20, time){
  result_frame <- data.frame(system, lib,  algorithm, fitness, x[1], x[2], x[3], x[4], top5, top10, top15, top20, time)
  colnames(result_frame) <- c("System","Library","Algorithm","FitnessFunction","x1","x2","x3","x4","TOP-5","TOP-10","TOP-15","TOP-20","Time")
  if (file.exists(file_output))
    write.table(result_frame, file_output, sep = ",", col.names = F, row.names = F, append = T)
  else
    write.table(result_frame, file_output, sep = ",", col.names = T, row.names = F, append = T)
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

detach_package <- function(pkg, character.only = FALSE)
{
  if(!character.only)
  {
    pkg <- deparse(substitute(pkg))
  }
  search_item <- paste("package", pkg, sep = ":")
  while(search_item %in% search())
  {
    detach(search_item, unload = TRUE, character.only = TRUE)
  }
}

silhouette_coefficient <- function(matrix, distances){
  # computing number of clusters
  clustering<-matrix("",length(rownames(matrix)),1)
  for (i in 1:length(rownames(matrix))) {
    flag<-(matrix[i,]==max(matrix[i,]))# each documents belongs to the cluster with the higher probability
    flag<-which(flag==TRUE)
    topics <- sort(flag)
    clustering[i,1]<-paste(topics, collapse = '_')
  }
  rownames(clustering)<-rownames(matrix)
  
  # assign the clusters
  clusters<-unique(clustering)
  count <- 1
  for (clust in clusters){
    clustering[clustering[,1] == clust,1] <- count
    count <- count+1
  }
  cluster_objects<-list(); 
  cluster_objects$clustering <- as.numeric(clustering)
  
  sil = silhouette(x=cluster_objects$clustering, dmatrix=distances)
  sil = summary(sil)
  
  # # compute the cohesion for each documents
  # cohesion <- matrix(nrow = length(rownames(distances)), ncol = 1)
  # for (i in 1:length(rownames(distances))){
  #   cohesion[i,1] <- max(distances[clustering[,1] == clustering[i,1],i])
  # }
  # 
  # # compute the separation from other clusters 
  # separation <- matrix(nrow = length(rownames(distances)), ncol = 1)
  # for (i in 1:length(rownames(distances))){
  #   separation[i,1] <- min(distances[clustering[,1] != clustering[i,1],i])
  # }
  # 
  # # compute the silhouette coefficient
  # sil <- matrix(nrow = length(rownames(distances)), ncol = 1)
  # for (i in 1:length(rownames(distances))){
  #   if (sum(clustering[i,1] == clustering)>1)
  #     sil[i,1] <- (separation[i,1] - cohesion[i,1]) / max(separation[i,1], cohesion[i,1])
  #   else
  #     sil[i,1] <- 0 # if the cluster contanis only one document, the Silohuette Coeff. is zero
  # }
  # return(mean(sil))
  return(sil$avg.width)
}
