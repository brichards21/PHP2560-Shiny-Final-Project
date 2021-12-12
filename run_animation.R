
run_animation <- function(df_use) {
  
  wide_dat <- df_use %>%
    pivot_wider(names_from = "people_type", values_from = "num_people")
  
  max_data <- data.frame(max_p = apply(wide_dat[, -1], 2, max)) 
  
  max_data$names <- rownames(max_data)
  
  
  df_use <- accumulate_by(df_use, ~ Time) 
  
  
  
  times <- length(grep(max_data$names, df_use$people_type))
  
  df_use <- df_use %>%
    mutate(max_num = rep(max_data$max_p, times))
  
  df_use_check <- df_use %>%
    filter(num_people %in% max_data$max_p)
  
  max_data$max_time <- unique(df_use_check$Time)
  
  
  
  p <- ggplot(df_use, aes(frame = frame)) +
    geom_line(aes(x = Time, y = num_people, color = people_type)) +
    theme_minimal() +
    theme(legend.title = element_blank())
  
  
  fig <- ggplotly(p)
  
  fig %>%
    layout (
      xaxis = list(
        title = "Day",
        # range = c()
        zeroline = F
      ),
      yaxis = list(
        title = "Number of People",
        zeroline = F
      ) %>%
        animation_opts(
          frame = 2,
          transition = 0,
          redraw = T
        )
    )
  
  
}