library(philentropy)

source(paste(main_path, "/lda_utilities.R", sep=""))

# This function implements the Silhouette Coefficient (fitness function to maximize).  
# The Silhouette coefficient takes values in [-1; 1]. Higher Silhouette Coefficient  
# denotes better clustering quality
fitness_max <- function(x=c()){
  n = length(x)
  for (index in 1:n){
    if (x[index] < lower_bounds[index] || x[index] > upper_bounds[index])
      return(-Inf)
  }
  
  if (fitness_name  == "silhouette"){
    lda_model = train_lda(x, lib, tdm)
    document2topic = document_by_topic_mixture(lda_model, lib)
    
    # compute the distance between documents in the topics space
    distances <- as.matrix(dist(document2topic, method = "euclidean", diag = T, upper = T))
    coefficient <- silhouette_coefficient(document2topic, distances)
    return(coefficient)
  } else if (fitness_name  == "coherence"){
    lda_model = train_lda(x, lib, tdm)
    top_words = get_top_words(lda_model, lib, 10)
    numero_topic = round(x[1])
    coherence <- matrix(nrow = numero_topic, ncol = 1)
    for (i in 1:numero_topic){
      coherence[i,1] = SpeedReader::topic_coherence(top_words[,i], sparse_tdm, vocabulary = colnames(sparse_tdm), numeric_top_words = FALSE, K=10)
    }
    return(mean(coherence, na.rm=T))
  } else if (fitness_name == "raw_score") {
    n_topics = x[1]
    n_runs = 4
    n = 7 # number of overlap words
    runs  <- vector(mode = "list", length = n_runs)
    count <- 1
    x2 <- x
    for (i in 1:n_runs){
      tdm <- shuffle_tdm(tdm)
      for (j in 1:n_runs){
        lda_model = train_lda(x2, lib, tdm)
        top_words = get_top_words(lda_model, lib, 10)
        runs[[count]] <- list(top_words)
        count = count + 1
        remove(lda_model)
        }
      .jcall("java/lang/System", "V", "gc") # call the garbage collector
      gc()
      }
    scores <- matrix(data=0, nrow = 1, ncol = n_topics)
    for (topic_index in 1:n_topics){
      run1 <- runs[[1]][[1]]
      for (run_index in 2:(n_runs*2)){
        run2 <- runs[[run_index]][[1]]
        count <- overlap_count(run1[,topic_index], run2)
        if (max(count) >= n){
          scores[topic_index] = scores[topic_index]+1
        }
      }
    }
    median(scores/n_runs)
    return(median(scores/n_runs))
  } else {
    stop(paste("Fitness function", fitness_name, "not found!"), call. = TRUE)
  }
}

overlap_count <- function(topic, topic_matrix){
  n_topics <- ncol(topic_matrix)
  count <- matrix(0, 1, n_topics)
  for (index in 1:n_topics){
    intersection = intersect(topic, topic_matrix[,index])
    count[index] = length(intersection)
  }
  return(count)
}

test_fitness <- function(){
  runs <-  vector(mode = "list", length = 4)
  for (i in 1:4)
    runs[[i]] <-  list(matrix("",10,4))
  
  runs[[1]][[1]][,1]<-c("glori", "telemetri", "command", "spacecraft", "trace", "smrd", "tim", "spec", "pip", "parent")
  runs[[1]][[1]][,2]<-c("spec", "smrd", "parent", "child", "glori", "artifact", "referenc", "verif", "matrix", "data")
  runs[[1]][[1]][,3]<-c("test", "case", "accuraci", "roll", "document", "glori", "plan", "pitch", "yaw", "valu")
  runs[[1]][[1]][,4]<-c("command", "specifi", "ground", "softwar", "telemetri", "initi", "pip", "data", "configur", "band")
  
  runs[[2]][[1]][,1]<-c("command", "specifi", "ground", "softwar", "initi", "telemetri", "data", "pip", "configur", "band")
  runs[[2]][[1]][,2]<-c("document", "test", "glori", "initi", "plan", "roll", "case", "pitch", "yaw", "point")
  runs[[2]][[1]][,3]<-c("spec", "smrd", "parent", "child", "glori", "artifact", "verif", "points", "matrix", "spacecraft")
  runs[[2]][[1]][,4]<-c("pip", "capabi", "glori", "ground", "command", "smrd", "spec", "tim", "list", "spacecraft")
  
  runs[[3]][[1]][,1]<-c("spec", "smrd", "parent", "child", "glori", "referenc", "artifact", "verif", "matrix", "spacecraft")
  runs[[3]][[1]][,2]<-c("command", "telemetri", "pip", "ground", "spacecraft", "mode", "software", "document", "spec", "glori")
  runs[[3]][[1]][,3]<-c("command", "specifi", "ground", "softwar", "initi", "telemetri", "data", "configur", "pip", "band")
  runs[[3]][[1]][,4]<-c("test", "initi", "accuraci", "glori", "roll", "plan", "matrix", "yaw", "pitch", "valu")
  
  runs[[4]][[1]][,1]<-c("command", "specifi", "tim", "pip", "ground", "telemetri", "glori", "includ", "softwar", "document")
  runs[[4]][[1]][,2]<-c("test", "initi", "glori", "plan", "roll", "accuraci", "case", "pip", "point", "yaw")
  runs[[4]][[1]][,3]<-c("command", "specifi", "ground", "softwar", "telemetri", "initi", "data", "pip", "configur", "band")
  runs[[4]][[1]][,4]<-c("spec", "smrd", "child", "glori", "artifact", "referenc", "parent", "verif", "matrix", "data")
}

jaccard_similarity <- function(v1, v2){
  U = union(v1, v2)
  I = intersect(v1, v2)
  return(length(I)/length(U))
}

# This function is a wrapper around the silhouette coefficient. Minimizing this function corresponds 
# to maximizing the original silhouette coefficient. Meta-heuristics that minimize the fitness 
# function should use this function.
fitness_min <- function(x=c()){
  value <- (-fitness_max(x))
  return(value)
}
