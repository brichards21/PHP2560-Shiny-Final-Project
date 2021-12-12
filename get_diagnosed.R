
get_diagnosed <- function(e = 477, i = 286, q = 191, 
                          j = 848, r = 1213, epsilon = (1/3)*(2/5),
                          lambda = (1/3)*(3/5), delta = (15/100)*(1/21), 
                          theta = 1/3, sigma = 1/3, gamma = 1/21, k = 0.1, b = (31 + t)/(22 + 5*t)) {
  
#' get_diagnosed generates epidemic data with exposed, infected, quarantined, diagnosed, and recovered individuals for 400 days
#'@param e initial number of exposed
#'@param i initial number of infectives
#'@param q inital number of quarantined
#'@param j initial number of diagnosed
#'@param r initial number of recovered
#'@param epsilon rate of exposed people moving into infective class
#'@param lambda rate of exposed people moving into quarantined class
#'@param delta rate of infected and diagnosed people dying
#'@param theta rate of infectives moving into diagnosed class
#'@param sigma rate of quarantined people moving into diagnosed class
#'@param gamma rate of diagnosed moving into recovered class
#'@param k infectivity fraction for the exposed individuals compared with individuals in the infective class
#'@param b transmission rate
#'@return all_data data frame containing number of individuals in each population class

  
  days <- 400 # collect data for 400 days
  
  
  for (t in 1:(days - 1)) {
    
  # compute number of exposed, infected, quarantined, diagnosed, and recovered each day  
    e[t+1] <- e[t] + b*((k*e[t])+i[t]) - (epsilon+lambda)*e[t]
    i[t+1] <- i[t] + epsilon*e[t] - (delta + theta)*i[t]
    q[t+1] <- q[t] + lambda*e[t] - sigma*q[t]
    j[t+1] <- j[t] + theta*i[t] + sigma*q[t] - (delta + gamma)*j[t]
    r[t+1] <- r[t] + gamma*j[t]
    
    
    if (b == (31 + t)/(22 + 5*t)) {
    
    # update transmission rate if transmission rate is default
      
      b <- (31 + (t+1))/(22 + 5*(t+1))
      
    }
    
    
    
  }
  
  # combine population data into one data frame
  
  all_data <- data.frame(cbind(Time = 1:days, Exposed = e, Infectives = i, 
                               Quarantined = q, Diagnosed = j, Recovered = r))
  
  # transform data into longer format for plotting purposes
  
  all_data <- all_data %>%
    pivot_longer(!Time, names_to = "people_type", values_to = "num_people")
  
  return(all_data)
  
}
