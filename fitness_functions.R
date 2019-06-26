#fitness function
fitness_LDA<-function(x=c()){
  numero_topic<-round(x[1]) #x[1] = number of topics k
  iteration<-round(x[2])    #x[2] =  number of gibbs iteration
  pAlpha<-x[3]              #x[3] = Alpha
  pDelta<-x[4]              #x[4] = Beta
  
  #apply LDA to the term-by-document matrix
  ldm <- LDA(tdm, method="Gibbs", control = list(alpha=pAlpha, delta=pDelta, iter=iteration, seed=5, nstart=1), k = numero_topic)  # k = num of topics
  pldm <- posterior(ldm)
  names(pldm)
  
  #compute the topic-by-term matrix    
  names(tdm$dimnames)
  docs<-tdm$dimnames$Docs
  topics<-names(terms(ldm))
  matrix<-pldm$topics
  dimnames(matrix)<-list(docs,topics)
  
  #compute the distance between documents in the topics space
  distances <- as.matrix(dist(matrix, method = "euclidean", diag = T, upper = T))
  
  #computing number of clusters
  clustering<-matrix("",length(rownames(matrix)),1)
  for (i in 1:length(rownames(matrix))) {
    flag<-(matrix[i,]==max(matrix[i,]))# each documents belongs to the cluster with the higher probability
    flag<-which(flag==TRUE)
    topics <- sort(flag)
    clustering[i,1]<-paste(topics, collapse = '_')
  }
  rownames(clustering)<-rownames(matrix)
  
  clusters<-unique(clustering)
  count <- 1
  for (clust in clusters){
    clustering[clustering[,1] == clust,1] <- count
    count <- count+1
  }
   cluster_objects<-list(); 
   cluster_objects$clustering <- as.numeric(clustering)
  
  cohesion <- matrix(nrow = length(rownames(distances)), ncol = 1)
  for (i in 1:length(rownames(distances))){
    cohesion[i,1] <- max(distances[clustering[,1] == clustering[i,1],i])
  }
  
  separation <- matrix(nrow = length(rownames(distances)), ncol = 1)
  for (i in 1:length(rownames(distances))){
    separation[i,1] <- min(distances[clustering[,1] != clustering[i,1],i])
  }
  
  sil <- matrix(nrow = length(rownames(distances)), ncol = 1)
  for (i in 1:length(rownames(distances))){
    if (sum(clustering[i,1] == clustering)>1)
      sil[i,1] <- (separation[i,1] - cohesion[i,1]) / max(separation[i,1], cohesion[i,1])
    else
      sil[i,1] <- 0
  }
  #silhouette_coeffs <- silhouette(cluster_objects, dmatrix = distances, FUN=mean(), full=T)
  #silhouette_summary <- summary(silhouette_coeffs)
  #print(paste(silhouette_summary$avg.width, x[1], topk(duplicate_graph, distances, 15)))
  #return(silhouette_summary$avg.width) # for GA package
  #print(paste(mean(sil), x[1], topk(duplicate_graph, distances, 15)))
  return(mean(sil))
}

min_fitness_LDA<-function(x=c()){
  value <- 2-fitness_LDA(x)
  return(value)
}
