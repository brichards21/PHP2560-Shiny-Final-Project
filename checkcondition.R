checkcondition <- function(deathrate, epsilon, lambda, theta, gamma){
  #' This function was created to check the parameters of the model that we are applying 
  #' We apply this for our randomly generated data in our function 
  #' 
  #' @param deathrate, the rate of death set in the model 
  #' @param epsilon, the epsilong
  #' @return the parraaters that fit the model 
  
  while(sum( epsilon, lambda) > 1){
    epsilon <- runif(1)
    lambda <- runif(1)
  } 
  while(sum(deathrate, theta) >1 | sum(deathrate, gamma) > 1 ){
    deathrate <- runif(1)
    theta <- runif(1)
    gamma <- runif(1)
  } 
  j <- data.frame(deathrate, epsilon, lambda, theta, gamma )
  return(j)
}

A <- checkcondition(.51, .51, .51, .51, .51)