source("libraries.R")

set_seed <- 46342
set.seed(set_seed)

# select state and district - NEED TO SUPPLY BOTH ----------
state    <- "Delhi"

view_districts(state = state) # see what districts are available in given state

district <- "Delhi"

# calculate R from covid19india.org data ----------
d <- covid19india_r_est(
  st   = state,
  dist = district,
  save = TRUE # set save = TRUE to save data as csv
)

d # preview data

# calculate R from manual input data ----------
y <- input_r_est(
  file = "test.csv",
  district = "Kolkata",
  save  = TRUE
)

# plot time-varying R estimate ----------
plot_tvr(
  dat      = d,
  district = district,
  save     = TRUE # set save = TRUE to save plot
)

plot_tvr(
  dat      = y,
  district = "Kolkata",
  save     = TRUE # set save = TRUE to save plot
)

