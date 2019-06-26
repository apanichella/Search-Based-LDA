library(XML)
library(tm)
library(topicmodels)
library(jsonlite)
library(slam)
library(igraph)
library(stringr)
library(cluster)
library(snakecase)
library(stopwords)
library(NMOF)
library(xtable)

#set the path to your project directory
setwd("<path to the main folder>")

## 1. load utility functions 
source("<path to the main folder>/utilities.R")
source("<path to the main folder>/metaheuristics.R")

## 2. Set path to the dataset
system <- "datasets/LANG"
input_path <- paste(system, "/bugrepo/repository/", sep="");
output_path <- paste(system, "/bugrepo/splitted_repository/", sep="");

## 3. read the oracle 
duplicate_graph <- paste(system, "/bugrepo/duplicates.json", sep="");
duplicate_graph <- oracle2graph(duplicate_graph)
plot(duplicate_graph)

## 4. Set the path to the output directory
file_output <- "results.csv"
files <- list.files(input_path)

## 5. split the main file (cataining all reports) in separate files (one for each report)
# 5.1 Create the folder where to save the (separated reports)
dir.create(output_path, showWarnings = FALSE)
# 5.2 Split reports in sub-files
split_document(files, output_path)

## 6. create the corpus with textmatrix
tdm <- pre_processing(output_path)

# 7. Apply tf-idf weighting schema
tdm2 <- weightSMART(tdm, "ntn")
tdm$v <- as.integer(round(tdm2$v))

# 8. Meta-heuristic setting
pop_size = 10
n_iterations = 5
lower_bounds <- c(10, # n. topics
                  50, # n. iterations
                  0,  # alpha
                  0   # beta
)

upper_bounds <- c(length(tdm$dimnames$Docs), # n. topics
                  100,                       # n. iterations
                  1,                         # alpha
                  1                          # beta
)

# number in independent runs
numberOfRuns = 1

#running GA
for (i in 1:numberOfRuns){
  start.time <- proc.time()
  metaheuristic_name = "SA" # available metaheurstics: GA, DE, RANDOM, SA, PSO
  x <- run_metaheurisic(metaheuristic_name, lower_bounds, upper_bounds, n_iterations, pop_size)
  end.time <- proc.time()
  
  # Compute LDA optimized
  distances <- evaluate_LDA(x)
  
  # evaluate the LDA configuration using TOP-k metric
  top5 <- topk(duplicate_graph, distances, 5)
  top10 <- topk(duplicate_graph, distances, 10)
  top15 <- topk(duplicate_graph, distances, 15)
  top20 <- topk(duplicate_graph, distances, 20)
  
  print(paste("Top 5 = ", top5))
  print(paste("Top 10 = ", top10))
  print(paste("Top 15 = ", top15))
  print(paste("Top 20 = ", top20))
  
  save_results(system, metaheuristic_name, file_output, x, top5, top10, top15, top20, as.numeric(end.time-start.time)[3])
}
