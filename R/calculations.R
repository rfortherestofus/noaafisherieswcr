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
#' @param timing Preliminary or final
#'
#' @return A tibble with JPE data
#' @export
#'
#' @references The Juvenile Production Estimate (JPE) reported here, and
#'   implemented in the package WCR.Fisheries.xxxx (available at <NMFS internal
#'   github>), uses the "Method 2" approach described in O'Farrell et al. 2018.
#'   Staff at the NOAA Fisheries Southwest Fisheries Science Center provide the
#'   latest estimates each year for outmigration survival (Arnold Amman) and the
#'   fry-to-smolt survival rate (Michael O'Farrell). O’Farrell M. R., W. H.
#'   Satterthwaite, A. N. Hendrix, and M. S. Mohr. 2018. Alternative Juvenile
#'   Production Estimate (JPE) forecast approaches for Sacramento River
#'   winter-run Chinook Salmon. San Francisco Estuary & Watershed Science
#'   16(4):4.  https://doi.org/10.15447/sfews.2018v16iss4art4
#'
#' @examples
calculate_jpe_data <- function(
    JPI,
    V.JPI,
    timing
) {

  f.hat <- 0.4946
  V.f <- 0.006719416

  z <- survival_by_year

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
