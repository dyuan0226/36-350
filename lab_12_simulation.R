generate_data <- function(n, p){
  covariates <- matrix(rnorm(n*p), nrow=n)
  responses <- rnorm(n)
  return(list(covariates=covariates, responses=responses))
}

