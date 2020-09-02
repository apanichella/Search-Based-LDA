library(topicmodels)
library(textmineR)

train_lda <- function(x=c(), library_name, tdm){
  numero_topic<-round(x[1]) #x[1] = number of topics k
  iteration<-round(x[2])    #x[2] =  number of gibbs iteration
  pAlpha<-x[3]              #x[3] = Alpha
  pBeta<-x[4]              #x[4] = Beta
  
  if (library_name == "topicmodels"){
    ldm <- topicmodels::LDA(tdm, method="Gibbs", control = list(alpha=pAlpha, delta=pBeta, iter=iteration, seed=r.seeds, nstart=n.starts), k = numero_topic)  # k = num of topics
    topicmodels::posterior(ldm)
    lda_model = ldm
  } else if (library_name == "textmineR"){
    model <- textmineR::FitLdaModel(tdm, k=numero_topic, iteration=iteration, burnin=-1, calc_coherence = TRUE, alpha = pAlpha, beta = pBeta, smooth=FALSE)
    lda_model = model
  } else {
    stop(paste("Lbrary not", library_name,"found!"), call. = TRUE)
  }
  return(lda_model)
}

document_by_topic_mixture <- function(lda_model, library_name){
  if (library_name == "topicmodels"){
    pldm <- topicmodels::posterior(lda_model)
    names(tdm$dimnames)
    docs<-tdm$dimnames$Docs
    topics<-names(terms(lda_model))
    matrix<-pldm$topics
    dimnames(matrix)<-list(docs,topics)
    mixture = matrix
  } else if (library_name == "textmineR"){
    document2topic <- textmineR::predict(lda_model, tdm, method = "gibbs", iterations=200)
    mixture = document2topic
  }
  
  return(mixture)
}

get_top_words <- function(lda_model, library_name, n_words){
  if (library_name == "topicmodels"){
    top_words=topicmodels::terms(lda_model, n_words)
  } else if (library_name == "textmineR"){
    top_words=textmineR::GetTopTerms(lda_model$phi, n_words)
  }
  
  return(top_words)
}

evaluate_LDA <- function(x, library_name, tdm){
  lda_model = train_lda(x, library_name, tdm)
  document2topic = document_by_topic_mixture(lda_model, library_name)
  distances <- as.matrix(dist(document2topic, method = "euclidean", diag = T, upper = T))
  rownames(distances) = rownames(document2topic)
  colnames(distances) = rownames(document2topic)
  return(distances)
}
