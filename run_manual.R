source("libraries.R")

district  <- "Nanded"               # name of district/block/place 
file_name <- "nanded_positives.csv" # looks in `data/input` folder

y <- input_r_est(
  file     = file_name,
  district = district,
  save     = TRUE
)

plot_manual_tvr(
  dat        = y,
  start_date = "2021-01-01", # date format: YYYY-MM-DD
  end_date   = Sys.Date(),
  district   = district,
  save       = TRUE # set save = TRUE to save plot
)
