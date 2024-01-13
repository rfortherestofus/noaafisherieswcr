#' vwm
#'
#' @param s
#' @param V
#'
#' @return
#' @export
#'
#' @examples
vwm <- function(s, V) { # function for variance-weighted mean
  inv.var <- 1 / V # survival rate and the variance of that
  w <- inv.var / sum(inv.var) # survival rate
  s.mean <- sum(s * w)
  s.var <- 1 / sum(inv.var)

  list(
    surv = s.mean,
    v.surv = s.var
  )
}

#' Calculate JPE Data
#'
#' @param JPI
#' @param V.JPI
#' @param timing Prelminary or final
#'
#' @return A tibble with JPE data
#' @export
#'
#' @examples
calculate_jpe_data <- function(
    JPI,
    V.JPI,
    timing
) {

  f.hat <- 0.4946
  V.f <- 0.006719416

  z <- tibble::tribble(
         ~year, ~surv.redding.sac, ~SE.redding.sac, ~surv.salt.sac, ~SE.salt.sac,
         2013L,         0.1486486,       0.0292418,      0.1639998,    0.0319734,
         2014L,         0.3644401,       0.0255175,      0.4084506,    0.0276114,
         2015L,         0.4943878,       0.0212285,      0.5947637,    0.0219756,
         2016L,         0.5052794,       0.0209422,      0.5360735,    0.0215223,
         2017L,         0.4502784,       0.0208726,      0.5938153,    0.0323989,
         2018L,         0.2776186,       0.0183141,       0.336723,    0.0212856,
         2019L,         0.3853557,       0.0191263,       0.414898,    0.0202036,
         2020L,         0.1494039,       0.0159109,       0.178062,    0.0186417,
         2021L,         0.1027021,       0.0128423,      0.1492142,    0.0182299,
         2022L,         0.1335669,       0.0142614,      0.1654651,    0.0173925
         )


  ## Natural-origin
  s.nat <- z$surv.salt.sac # CJS-estimated surv rates (salt-sac)
  V.s.nat <- z$SE.salt.sac^2 # variances of s.nat

  s.nat.mean <- vwm(s.nat, V.s.nat)$surv
  s.nat.var <- vwm(s.nat, V.s.nat)$v.surv
  round.s.nat.mean <- round(s.nat.mean, 4) # force to 4 digits after decimal

  ## Hatchery-origin
  s.hat <- z$surv.redding.sac # CJS-estimated surv rates (redding-sac)
  V.s.hat <- z$SE.redding.sac^2 # variances of s.hat

  s.hat.mean <- vwm(s.hat, V.s.hat)$surv
  s.hat.var <- vwm(s.hat, V.s.hat)$v.surv

  ## ==============================================================================
  ## Forecast natural-origin JPE

  JPE <- JPI * f.hat * round(s.nat.mean, 4) # JPE forecast
  # enforce 4 sig digits for surv rate

  V.JPE <- (JPI^2 - V.JPI) * # JPE variance (Gray 1999)
    ((f.hat^2 * s.nat.var) + (round.s.nat.mean^2 * V.f) - (V.f * s.nat.var)) +
    (f.hat^2 * round.s.nat.mean^2 * V.JPI)

  sd.JPE <- sqrt(V.JPE) # JPE standard deviation

  lci <- JPE - (1.96 * sd.JPE) # lower 95% confidence interval
  uci <- JPE + (1.96 * sd.JPE) # upper 95% confidence interval

  tibble::tibble(
    JPI = JPI,
    `natural surv rate` = round(s.nat.mean, 4),
    `V natural surv rate` = s.nat.var,
    f.hat = round(f.hat, 4),
    JPE = round(JPE, 0),
    `V JPE` = round(V.JPE),
    `sd JPE` = round(sd.JPE),
    `hatchery surv rate` = round(s.hat.mean, 4),
    `V hatchery surv rate` = s.hat.var,
    `JPE lower 95% CI` = round(lci, 0),
    `JPE upper 95% CI` = round(uci, 0)
  ) |>
    tidyr::pivot_longer(cols = tidyr::everything(),
                 names_to = "Variable") |>
    dplyr::mutate(timing = timing)
}
