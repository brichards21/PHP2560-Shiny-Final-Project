
# This first function runs our simulation, the initial values are set to be 
# what is given in the model in the paper 

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
  
  all_data <- data.frame(cbind(Day = 1:days, Exposed = e, Infectives = i, 
                               Quarantined = q, Diagnosed = j, Recovered = r))
  
  # transform data into longer format for plotting purposes
  
  all_data <- all_data %>%
    pivot_longer(!Day, names_to = "people_type", values_to = "num_people")
  
  return(all_data)
  
}

# This next function runs the animation that the user sees and can interact with 
run_animation <- function(df_use) {
  
  #' run_animation displays an interactive plot 
  #'@param df_use filtered data set based on user input
  #'@return fig interactive plot
  
  df_use <- df_use %>%
    # rename for improved user readability/comprehension
    rename("Population Group" = people_type,
           "Number of People" = num_people)  
  
  accumulate_by <- function(dat, var) {
    #' accumulate_by accumulates observations for each value of var to create the frame variable to be used by an interactive plot
    #'@param dat data frame
    #'@param var variable to accumulate by 
    #'@return accumulated data with frame variable
    
    var <- lazyeval::f_eval(var, dat)
    lvls <- plotly:::getLevels(var)
    dats <- lapply(seq_along(lvls), function(x) {
      cbind(dat[var %in% lvls[seq(1, x)], ], frame = lvls[[x]])
    })
    return(dplyr::bind_rows(dats))
    
  }
  
  
  df_use <- accumulate_by(df_use, ~ Day)  # accumulate data by day to create frames for interactive plot
  
  
  # form basic ggplot and separate lines by the population types (i.e. Exposed, Infectives, etc.) selected by user input
  
  p <- ggplot(df_use, aes(frame = frame)) +
    geom_line(aes(x = Day, y = `Number of People`, color = `Population Group`)) +
    theme_minimal() +
    theme(legend.title = element_blank())
  
  
  # translate static ggplot into an interactive plot
  
  fig <- ggplotly(p)
  
  fig <- fig %>%
    layout (
      # add x-axis label
      xaxis = list(
        title = "Day",
        zeroline = F
      ),
      yaxis = list(
        # add y-axis label
        title = "Number of People",
        zeroline = F
      ) %>%
        animation_opts(
          # set animation options
          # 2 milliseconds between frames
          frame = 2,
          # do not redraw plot at end of transition
          redraw = F
        )
    )
  
  
  return(fig)
  
  
}


# This function helps us create a data frame of the max number of people in each group selected 

create_table <- function(dataframe) {
  #' This function will generate a data frame that returns the time at which
  #' the maximum number of people occurs for the groups of interest.
  #' 
  #' @param dataframe, the data frame consisting of number of people, group, and time
  #' @return data frame with the maximum number of people and time for a specific group   
  newdf <- dataframe %>%
    group_by(people_type) %>%
    arrange(desc(num_people)) %>%
    slice(1) %>%
    rename("Group" = people_type,
           "Number of People" = num_people)
  return(newdf)
}

# Our last function checks the parameter conditions of our model to ensure it is valid for the model 
# If a user changes these conditions manually we validate in shiny that they fit 

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
