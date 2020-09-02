library(XML)
library(tm)
library(jsonlite)
library(slam)
library(igraph)
library(stringr)
library(cluster) 
library(cmaes)
library(snakecase)
library(stopwords) 
library(NMOF) 
library(xtable) 
library(Matrix)
library(devtools) 
library(SpeedReader)

main_path <- "/home/SSBSE-LDA"
#set the path to your project directory
setwd(main_path)

## 1. load utility functions 
source(paste(main_path, "/utilities.R", sep=""))
source(paste(main_path, "/metaheuristics.R", sep=""))

## 2. Set path to the dataset
print("Please select the project to analyze. The available projects are: COLLECTIONS, DATACMNS, HIVE, IO, LANG, MATH, ROO, SEC, SPR, WFLY.")
dataset_name <- readline(prompt="Enter system name: ")
system <- paste("datasets/", dataset_name, sep="")
input_path <- paste(system, "/bugrepo/repository/", sep="");
output_path <- paste(system, "/bugrepo/splitted_repository/", sep="");

## 3. read the oracle 
duplicate_graph <- paste(system, "/bugrepo/duplicates.json", sep="");
duplicate_graph <- oracle2graph(duplicate_graph)
#plot(duplicate_graph)

## 4. Set the path to the output directory
file_output <- "results.csv"
files <- list.files(input_path)

## 5. split the main file (cataining all reports) in separate files (one for each report)
# 5.1 Create the folder where to save the (separated reports)
file.exists(output_path)
dir.create(output_path, showWarnings = FALSE)
# 5.2 Split reports in sub-files
split_document(files, output_path)

## 6.1 Choose the fitness function
print("Please select the fitness function to use. The available fitness functions are: 
      silhouette, coherence, raw_score")
fitness_name = readline(prompt = "Enter fitness function name: ")

## 6. Choose the R package to use
lib = "topicmodels"
if (lib == "textmineR"){
  detach_package(topicmodels)
  source(paste(main_path, "/textmineR/preprocessing.R", sep=""))
} else if (lib == "topicmodels") {
  detach_package(textmineR)
  source(paste(main_path, "/topicmodels/preprocessing.R", sep=""))
} 

source(paste(main_path, "/fitness_functions.R", sep=""))
source(paste(main_path, "/lda_utilities.R", sep=""))

## 7. create the document by term matrix
tdm <- pre_processing(output_path)

if (lib == "textmineR"){
  n_documents = length(dimnames(tdm)[[1]])
} else if (lib == "topicmodels") {
  n_documents = length(tdm$dimnames$Docs)
  sparse_tdm = convert2SparseMatrix(tdm)
  if (fitness_name == "raw_score"){
    n.starts <- 1
  } else {
    n.starts <- 3
  }
} 

# 8. Meta-heuristic setting
pop_size = 10
n_iterations = 5
lower_bounds <- c(10.0, # n. topics
                  100.0, # n. iterations
                  0.0000001,  # alpha
                  0.0000001   # beta
)

upper_bounds <- c(n_documents, # n. topics
                  200,         # n. iterations
                  1.0,           # alpha
                  1.0            # beta
)

# number in independent runs
numberOfRuns = 100

# 9. Select metaheuristic

print("Please select the meta-heuristic to use. \n
The available meta-heuristics are: CMAES, DE, GA, SA, PSO, RANDOM, LS \n")
metaheuristic_name = readline(prompt = "Enter fitness function name: ")

for (i in 1:numberOfRuns){
    print(paste("Run N.", i, sep=" "))
    start.time <- proc.time()
    
    random_seeds = sample(1:1000000, n.starts)
    r.seeds <- random_seeds
    #print(paste("Random seed", random_seed))
    #set.seed(random_seed) # setting the random seed for the search
    
    #metaheuristic_name = "SA" # available metaheurstics: GA, DE, RANDOM, SA, PSO
    x <- run_metaheurisic(metaheuristic_name, lower_bounds, upper_bounds, n_iterations, pop_size)
    end.time <- proc.time()
    
    # if multiple equally-good solutions, just get one
    n_solutions = nrow(x)
    if (!is.null(n_solutions) && n_solutions>1)
      x = x[1,]
    
    # Compute LDA optimized
    #set.seed(random_seed) # setting the same random seed used for the search
    distances <- evaluate_LDA(x, lib, tdm)
    
    # evaluate the LDA configuration using TOP-k metric
    top5 <- topk(duplicate_graph, distances, 5)
    top10 <- topk(duplicate_graph, distances, 10)
    top15 <- topk(duplicate_graph, distances, 15)
    top20 <- topk(duplicate_graph, distances, 20)
    
    print(paste("Top 5 = ", top5))
    print(paste("Top 10 = ", top10))
    print(paste("Top 15 = ", top15))
    print(paste("Top 20 = ", top20))
    
    save_results(system, lib, metaheuristic_name, fitness_name, file_output, x, top5, top10, top15, top20, as.numeric(end.time-start.time)[3])
    
    remove(distances, top5, top10, top15, top20)
}

###
#library(ggplot2)
#tmp <- read.csv(file_output)
#ggplot(tmp, aes(x = tmp$Algorithm, y = tmp$TOP.5, fill = tmp$FitnessFunction)) + 
#  geom_boxplot() + 
#  facet_wrap(~tmp$System, scales="free", ncol = 2) + 
#  ylab("TOP.5") + xlab("Meta-heuristics") + 
#  labs(fill = "Fitness Functions")  + theme(legend.position = c(1, 0), legend.justification = c(1, 0)) 

