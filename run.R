source("libraries.R")

set_seed <- 46342
set.seed(set_seed)

# select state and district - NEED TO SUPPLY BOTH ----------
state    <- "Maharashtra"
# view_districts(state = state) # see what districts are available in given state
district <- "Nanded"

# calculate R from covid19india.org data ----------
d <- covid19india_r_est(
  st   = state,
  dist = district,
  save = TRUE # set save = TRUE to save data as csv
)

d %>% tail() # preview data

# calculate R from manual input data ----------
y <- input_r_est(
  file = "nanded_positives.csv",
  district = "Nanded",
  save  = TRUE
)


# plot time-varying R estimate ----------
plot_tvr(
  dat      = d,
  district = district,
  save     = TRUE # set save = TRUE to save plot
)

plot_manual_tvr(
  dat        = y,
  start_date = "2021-01-01", # date format: YYYY-MM-DD
  end_date   = Sys.Date(),
  district   = "Nanded",
  save       = TRUE # set save = TRUE to save plot
)

