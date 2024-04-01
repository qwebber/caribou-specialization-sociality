

## Generate random steps
rand_by <- function(x, y, t, n, sl_distr, ta_distr, crs) {
  trk <- track(x = x, y = y, t = t, crs = crs) %>%
    steps() %>%
    time_of_day() %>%
    random_steps(n = n, sl_distr, ta_distr) 
}