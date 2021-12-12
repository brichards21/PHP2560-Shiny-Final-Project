get_diagnosed <- function(e = 477, i = 286, q = 191, 
                          j = 848, r = 1213, epsilon = (1/3)*(2/5),
                          lambda = (1/3)*(3/5), delta = (15/100)*(1/21), 
                          theta = 1/3, sigma = 1/3, gamma = 1/21, k = 0.1, b = (31 + t)/(22 + 5*t)) {
  
  #' get_diagnosed
  days <- 400
  for (t in 1:(days - 1)) {
    e[t+1] <- e[t] + b*((k*e[t])+i[t]) - (epsilon+lambda)*e[t]
    i[t+1] <- i[t] + epsilon*e[t] - (delta + theta)*i[t]
    q[t+1] <- q[t] + lambda*e[t] - sigma*q[t]
    j[t+1] <- j[t] + theta*i[t] + sigma*q[t] - (delta + gamma)*j[t]
    r[t+1] <- r[t] + gamma*j[t]
    if (b == (31 + t)/(22 + 5*t)) {
      
      b <- (31 + (t+1))/(22 + 5*(t+1))
      
    }
  }
  all_data <- as.data.frame(cbind(Time = 1:days, Exposed = e, Infectives = i, 
                                  Quarantined = q, Diagnosed = j, Recovered = r))
  index <- seq(1, days, 2)
  all_data <- all_data[index, ] 
  rownames(all_data) <- 1:nrow(all_data)
  all_data <- all_data %>%
    pivot_longer(!Time, names_to = "people_type", values_to = "num_people")
  return(all_data)
}
