run_animation <- function(df_use) {
  
#' run_animation displays an interactive plot 
#'@param df_use filtered data set based on user input
#'@return fig interactive plot
  
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
  
  
  df_use <- accumulate_by(df_use, ~ Time) # accumulate data by time to create frames for interactive plot
  
  
  # form basic ggplot and separate lines by the population types (i.e. Exposed, Infectives, etc.) selected by user input
  
  p <- ggplot(df_use, aes(frame = frame)) +
    geom_line(aes(x = Time, y = num_people, color = people_type)) +
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