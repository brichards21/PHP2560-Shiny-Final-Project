run_animation <- function(df_use) {
  
  wide_dat <- df_use %>%
    pivot_wider(names_from = "people_type", values_from = "num_people")
  
  accumulate_by <- function(dat, var) {
    var <- lazyeval::f_eval(var, dat)
    lvls <- plotly:::getLevels(var)
    dats <- lapply(seq_along(lvls), function(x) {
      cbind(dat[var %in% lvls[seq(1, x)], ], frame = lvls[[x]])
    })
    return(dplyr::bind_rows(dats))
    
  }
  
  
  df_use <- accumulate_by(df_use, ~ Time) 
  
  
  
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