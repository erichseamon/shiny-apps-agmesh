# This is where you import any libraries that will be used in computations or plot generation:
# e.g. library(xts), library(ggplot2); library(tm); library(dplyr); library(tidyr); library(stargazer)

# This is where you add dashboard-specific functions to use in ui.R and server.R
# e.g.
start_date <- function(date_range) {
  return(Sys.Date() - (switch(date_range, daily = 2, weekly = 14, monthly = 60, quarterly = 90) + 1))
}
