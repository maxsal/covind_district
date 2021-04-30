# est_r for states function -----------
estR0_out <- function(dat) {
  
  t_start <- seq(2, nrow(dat) - 4)
  t_end   <- t_start + 4
  
  res <- EpiEstim::estimate_R(
    incid = dat$daily_cases,
    method = "parametric_si",
    config = make_config(list(
      mean_si             = 7,
      std_si              = 4.5,
      si_parametric_distr = "G",
      t_start             = t_start,
      t_end               = t_end,
      seed                = set_seed))
  ) 
  
  tibble::tibble(
    date_num = res$dates
  ) %>% dplyr::left_join(
    res$R, by = c("date_num" = "t_end")
  ) %>%
    dplyr::select(
      date_num, t_start, r = `Mean(R)`, lower = `Quantile.0.025(R)`, upper = `Quantile.0.975(R)`
    ) %>%
    tibble::add_column(date = dat$date) %>%
    dplyr::select(-date_num) %>%
    dplyr::select(date, tidyselect::everything())
  
}

# get_r0 -----------
get_r0 <- function(dat) {
  
  tmp_dat <- dat %>%
    dplyr::filter(daily_cases > 0) %>%
    # dplyr::group_by(place) %>%
    dplyr::mutate(
      ns = n()
    ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(ns >=7)
  
  options(warn = -1)
  tmp_est <- tmp_dat %>%
    dplyr::select(date, daily_cases, district) %>%
    tidyr::nest(data = c(-district)) %>%
    dplyr::mutate(
      estR0 = purrr::map(data, ~estR0_out(dat = .x))
    ) %>%
    tidyr::unnest(estR0) %>%
    dplyr::select(-data) %>%
    dplyr::filter(date >= "2020-04-26") %>%
    tidyr::drop_na()
  options(warn = 1)
  
  return(tmp_est)
}

# view districts ----------
view_districts <- function(state) {
  
  tmp <- state
  
  read_csv(here("data", "districts.csv"),
           col_types = cols()) %>%
    filter(state == tmp)
}

# calculate R estimate for a given state and district pair
# from covid19india.org data
covid19india_r_est <- function(st, dist, save = FALSE) {
  
  message("pulling data from covid19india.org")
  tmp_tvr <- read_csv("https://api.covid19india.org/csv/latest/districts.csv",
           col_types = cols()) %>%
    janitor::clean_names() %>%
    dplyr::filter(state == st & district == dist) %>%
    rename(
      cases  = confirmed,
      deaths = deceased
    ) %>%
    group_by(state, district) %>%
    arrange(date) %>%
    mutate(
      daily_cases     = cases - dplyr::lag(cases),
      daily_recovered = recovered - dplyr::lag(recovered),
      daily_deaths    = deaths - dplyr::lag(deaths)
    ) %>%
    ungroup() %>%
    get_r0()
  
  if (save == TRUE) {
    tmp_filename <- glue("{dist}_r_data_{format(Sys.Date(), '%Y%m%d')}.csv")
    message(glue("saving time-varying R estimate data to `data/output` subfolder as `{tmp_filename}`"))
    write_csv(tmp_tvr, file = here("data", "output", tmp_filename))
  }
  
  return(tmp_tvr)
  
}

# user provided data -----------
input_r_est <- function(file, district = NULL, save = FALSE) {
  
  message("reading `.csv` input file")
  tmp <- read_csv(here("data", "input", file))
  
  tmp_tvr <- tmp %>%
    arrange(date) %>%
    get_r0()
  
  if (save == TRUE) {
    if (is.null(district)) {
      stop("please add `district` argument to `input_r_est()` function")
    } else {
      tmp_filename <- glue("{district}_r_data_{format(Sys.Date(), '%Y%m%d')}.csv")
      message(glue("saving time-varying R estimate data to `data/output` subfolder as `{tmp_filename}`"))
      write_csv(tmp_tvr, file = here("data", "output", tmp_filename))
    }
  }
  
  return(tmp_tvr)
  
}

# plot tvr -----------
plot_tvr <- function(dat, district, save = FALSE) {
  
  tmp_plt <- dat %>%
    ggplot(aes(x = date, y = r)) +
    geom_hline(yintercept = 1, color = "#FF9933", size = 0.5) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.5, fill = "#138808") +
    geom_line(color = "#138808", size = 1) +
    geom_point(size = 0.5, color = "black") +
    labs(
      title   = glue("Time-varying R estimate in {district}"),
      x       = "Date",
      y       = "R(t)",
      caption = "**Data source:** covid19india.org"
    ) +
    theme_classic() +
    theme(
      plot.title = element_text(face = "bold"),
      plot.caption = element_markdown(hjust = 0)
    )
  
  if (save == TRUE) {
    tmp_filename <- glue("{district}_tvr_plot_{format(Sys.Date(), '%Y%m%d')}.pdf")
    message(glue("saving plot to `fig` subfolder as `{tmp_filename}`"))
    ggsave(tmp_plt, filename = here("fig", tmp_filename),
           width = 8, height = 4)
  }
  
  return(tmp_plt)
  
}