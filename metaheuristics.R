library(GA)
library(slam)
library(DEoptim)
library(GenSA)
library(metaheuristicOpt)
library(randomsearch)
library(parma)
library(pracma)

# Factory that run each meta-heuristic with the proper setting
run_metaheurisic<-function(name, lower_bounds=c(), upper_bounds=c(), n_iterations, pop_size){
  if (name == "GA"){
    print("Running Genetic Algorithms")
    res<-ga(type = "real-valued", fitness = fitness_max, lower=lower_bounds, upper=upper_bounds, pmutation=1/4, maxiter=n_iterations, run=n_iterations, popSize=pop_size, selection = ga_tourSelection, mutation = ga_polynomial_mutation, crossover = gareal_blxCrossover)
    best <- summary(res)
    return(best$solution)
  } else if (name == "CMAES") {
    print("CMA-ES")
    initial.point <- random_chromosome(lower_bounds, upper_bounds)
    res <- cmaes(pars=initial.point, fun=fitness_min, lower=lower_bounds, upper=upper_bounds, ctrl = cmaes.control(options = list(MaxIter=n_iterations, PopSize=pop_size, Restarts = 0)))
    return(res$par)
  } else if (name == "DE") {
    print("Running Differential Evolution")
    res <- DEoptim(fn = fitness_min, lower=lower_bounds, upper=upper_bounds, control = DEoptim.control(itermax = n_iterations, NP=pop_size))
    return(res$optim$bestmem)
  } else if (name == "RANDOM") {
    print("Running Random Search")
    res <- randomsearch(fun = fitness_min, minimize=T, max.evals=pop_size*n_iterations, lower=lower_bounds, upper=upper_bounds)
    summary = summary(res)
    return(summary$best.x$x)
  } else if (name == "SA"){
    print("Running Simulated Annealing")
    initial.point <- random_chromosome(lower_bounds, upper_bounds)
    res <- SAopt(OF=fitness_min, algo=list(nS=pop_size, nT=n_iterations, nD=pop_size*n_iterations, neighbour=neighbour_fun, x0=initial.point, printDetail=F))
    return(res$xbest)
  } else if (name == "PSO"){
    print("Running Particle Swarm Optimization")
    res <- PSopt(OF=fitness_min, algo=list(nP=pop_size, nG=n_iterations, min=lower_bounds, max=upper_bounds, c1=1.8, c2=1.8, printDetail=1, iner=0.6))
    x = res$xbest
    return(x)
  } else if (name == "LS"){
    print("Running the simple stochastic Local Search")
    initial.point <- random_chromosome(lower_bounds, upper_bounds)
    res <- LSopt(OF=fitness_min, algo = list(nS = n_iterations * pop_size, neighbour=neighbour_fun, x0=initial.point, printDetail=F, OF.target=-Inf))
    x <- res$xbest
    return(x)
  } else {
    stop("Meta-heuristic not found!", call. = TRUE)
  }
}

# Function used by Simulated Annealing (SA) to generate neighbors. The package 'GenSA' does not have
# a native faction to mutate/perturbate a solution. 
neighbour_fun <- function(x){
  return(polynomial_mutation(x, 1/4, lower_bounds, upper_bounds))
}

random_chromosome <- function(lower_bounds, upper_bounds){
  chromosome = c()
  
  n = length(lower_bounds)
  for (i in 1:n){
    chromosome <- c(chromosome, lower_bounds[i] + runif(1) * (upper_bounds[i]-lower_bounds[i]))
  }
  
  return(chromosome)
}


ga_polynomial_mutation <- function(ga, parent){
  mutate <- ga@population[parent,]
  mutate <- polynomial_mutation(mutate, ga@pmutation, ga@lower, ga@upper)
  return(mutate)
}

polynomial_mutation <- function(parent, pmutation, lower, upper){
  eta.m = 20 #
  
  offspring = parent
  n = length(parent)
  #i = sample(1:n, 1)
  
  for (index in 1:n){
    if (runif(1,0,1) < pmutation){
      u <- runif(1,0,1)
      if (u <= 0.5){
        sigma.l = (2*u)^(1/(1+eta.m)) -1
        offspring[index] <- parent[index] + sigma.l * (parent[index] - lower[index])
      } else {
         sigma.r = 1 - (2*(1-u))^(1/(1+eta.m))
         offspring[index] <- parent[index] + sigma.r * (upper[index] - parent[index])
      }
      #print("Parent >>")
      #print(parent)
      #print("Offspr >>")
      #print(offspring)
      if (offspring[index] < lower[index] || offspring[index]>upper[index]){
        offspring[index] <- lower[index] + runif(1,0,1) * (upper[index] - lower[index] )
      }
      #offspring[index] = max(offspring[index], lower[index], na.rm=TRUE)
      #offspring[index] = min(offspring[index], upper[index], na.rm=TRUE)
    }
  }
  remove(index, n)
  return(offspring)
}
