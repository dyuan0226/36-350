generate_data <- function(n, p){
  covariates <- matrix(rnorm(n*p), nrow=n)
  responses <- rnorm(n)
  return(list(covariates=covariates, responses=responses))
}

model_select <- function(covariates, responses, cutoff){
  first_lm <- lm(responses ~ covariates)
  p_values <- summary(first_lm)$coefficients[,"Pr(>|t|)"][-1]
  cov_to_keep <- which(p_values<=cutoff)
  if(length(cov_to_keep) == 0) return(vector())
  else{
    second_lm <- lm(responses ~ covariates[,cov_to_keep])
    return(summary(second_lm)$coefficients[,"Pr(>|t|)"])
  }
}

run_simulation <- function(n_trials, n, p, cutoff){
  p_values <- vector(mode="numeric")
  for (trial in 1:n_trials){
    sim <- generate_data(n, p)
    p_values <- c(p_values, model_select(sim$covariates, sim$responses, cutoff))
  }
  return(ggplot(mapping=aes(x=p_values)) + geom_histogram())
}

for (n in c(100, 1000, 10000)){
  for (p in c(10, 20, 50)){
    run_simulation(n_trials=1000, n=n, p=p, cutoff=0.05)
  }
}
