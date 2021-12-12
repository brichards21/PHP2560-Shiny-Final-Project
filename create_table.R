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
    select(people_type, Day, num_people) %>% 
    rename("Group" = people_type,
           "Number of People" = num_people)
  return(newdf)
}