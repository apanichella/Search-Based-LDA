library(GA)
library(slam)
library(DEoptim)
library(GenSA)
library(metaheuristicOpt)
library(randomsearch)

## 1. Fitness functions
source(paste(main_path, "/fitness_functions.R", sep=""))

# Factory that run each meta-heuristic with the proper setting
run_metaheurisic<-function(name, lower_bounds=c(), upper_bounds=c(), n_iterations, pop_size){
  if (name == "GA"){
    print("Running Genetic Algorithms")
    res<-ga(type = "real-valued", fitness = fitness_LDA, lower=lower_bounds, upper=upper_bounds, pmutation=1/4, maxiter=nGen, run=n_iterations, popSize=pop_size, mutation=gareal_raMutation, crossover = gareal_blxCrossover)
    best <- summary(res)
    return(best$solution)
  } else if (name == "DE") {
    print("Running Differential Evolution")
    res <- DEoptim(fn = min_fitness_LDA, lower=lower_bounds, upper=upper_bounds, control = DEoptim.control(itermax = n_iterations, NP=pop_size))
    return(res$optim$bestmem)
  } else if (name == "RANDOM") {
    print("Running Random Search")
    res <- randomsearch(fun = min_fitness_LDA, minimize=T, max.evals=pop_size*n_iterations, lower=lower_bounds, upper=upper_bounds)
    summary = summary(res)
    return(summary$best.x$x)
  } else if (name == "SA"){
    print("Running Simulated Annealing")
    initial.point <- c(max(10,runif(1)*upper_bounds[1]),runif(1)*upper_bounds[2],runif(1)*upper_bounds[3],runif(1)*upper_bounds[4])
    res <- SAopt(OF=min_fitness_LDA, algo=list(nS=pop_size, nT=n_iterations, nD=pop_size*n_iterations, neighbour=neighbour_fun, x0=initial.point))
    return(res$xbest)
  } else if (name == "PSO"){
    print("Running Particle Swarm Optimization")
    res <- PSopt(OF=min_fitness_LDA, algo=list(nP=pop_size, nG=n_iterations, min=lower_bounds, max=upper_bounds))
    x = res$xbest
    if (x[3]<0)
      x[3]=0.0001
    if (x[4]<0)
      x[4]=0.0001
    return(x)
  }
}

# Function used by Simulated Annealing (SA) to generate neighbors. The package 'GenSA' does not have
# a native faction to mutate/perturbate a solution. 
neighbour_fun <- function(x){
  r <- runif(1)
  x2 <- x
  for (i in 1:4){
    if (runif(1)<0.25){
      x2[i] <- x[i] + x[i] * rnorm(1)
    }
  }
  
  x2[1] <- max(lower_bounds[1], x2[1])
  x2[1] <- min(upper_bounds[1], x2[1])
  x2[2] <- max(x2[2], lower_bounds[2])
  x2[2] <- min(x2[2], upper_bounds[2])
  x2[3] <- max(x2[3], lower_bounds[3])
  x2[3] <- min(x2[3], upper_bounds[3])
  x2[4] <- max(x2[4], lower_bounds[4])
  x2[4] <- min(x2[4], upper_bounds[4])
  print(x2)
  return(x2)
}
