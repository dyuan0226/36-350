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

