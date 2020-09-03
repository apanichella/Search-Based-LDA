# let's install all required R packages

if (!require(devtools)){ install.packages(c('devtools','curl'), dependencies=T)  }
library("devtools")
if (!require(XML)){ install.packages("XML", repos = "http://www.omegahat.net/R") }
if (!require(SpeedReader)){ devtools::install_github("matthewjdenny/SpeedReader")}
if (!require(tm)){ install.packages("tm") }
if (!require(jsonlite)){ install.packages("jsonlite")}
if (!require(slam)){ install.packages("slam") }
if (!require(igraph)){ install.packages("igraph") }
if (!require(stringr)){ install.packages("stringr") }
if (!require(cluster)){ install.packages("cluster") }
if (!require(cmaes)){ install.packages("cmaes") }
if (!require(snakecase)){ install.packages("snakecase") }
if (!require(stopwords)){ install.packages("stopwords") }
if (!require(NMOF)){ install.packages("NMOF") }
if (!require(xtable)){ install.packages("xtable") }
if (!require(Matrix)){ install.packages("Matrix") }
if (!require(SpeedReader)){ devtools::install_github("matthewjdenny/SpeedReader") }
if (!require(GA)){ install.packages("GA")  }
if (!require(slam)){ install.packages("slam")  }
if (!require(DEoptim)){ install.packages("DEoptim")  }
if (!require(GenSA)){ install.packages("GenSA")  }
if (!require(metaheuristicOpt)){ install.packages("metaheuristicOpt")  }
if (!require(randomsearch)){ install.packages("randomsearch")  }
if (!require(randomsearch)){ install.packages("randomsearch")  }
if (!require(parma)){ install.packages("parma")  }
if (!require(pracma)){ install.packages("pracma")  }
if (!require(philentropy)){ install.packages("philentropy")  }
if (!require(topicmodels)){ install.packages('topicmodels', type='topicmodels')  }
if (!require(textmineR)){ install.packages('textmineR', type='textmineR')  }

