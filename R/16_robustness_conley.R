# =============================================================================
# 16_robustness_conley.R — Robustness: Conley (1999) spatial standard errors
# The 3 km bandwidth is motivated empirically by the Moran correlogram of the
# 2SLS residuals (see appendix): autocorrelation is concentrated in 0–1 km and
# statistically indistinguishable from zero beyond 3 km.
# =============================================================================

robustness_conley <- function(smpl) {

  # ── WGS84 coordinates ───────────────────────────────────────────────────────
  dat_sf <- smpl %>%
    filter(dummy_subway_10km == 1) %>%
    st_transform(4326)

  sf::sf_use_s2(FALSE)
  coords <- sf::st_coordinates(sf::st_centroid(sf::st_geometry(dat_sf)))

  dat <- dat_sf %>%
    sf::st_drop_geometry() %>%
    mutate(lon = coords[, 1], lat = coords[, 2])

  # ── Clean dataset (aligned with what feols uses) ────────────────────────────
  dat_clean <- dat %>%
    filter(
      !is.na(dlog_income_per_capita),
      !is.na(income_per_capita_2000),  income_per_capita_2000  > 0,
      !is.na(dist_cbd_2000), dist_cbd_2000 > 0,
      !is.na(dist_highway_2000),      dist_highway_2000      > 0,
      !is.na(share_college_complete_2000),
      !is.na(share_illiterate_2000),
      !is.na(share_over_65_2000),
      !is.na(pop_2000), pop_2000 > 0
    )
  ra_counts   <- table(dat_clean$ra_id)
  dat_clean <- dat_clean %>%
    filter(ra_id %in% names(ra_counts[ra_counts > 1]))

  # ── Moran correlogram (specification 4) ─────────────────────────────────────
  controls_4 <- paste(
    "log(income_per_capita_2000) + log(dist_cbd_2000) +",
    "log(dist_highway_2000) + share_college_complete_2000 +",
    "share_illiterate_2000 + share_over_65_2000 + log(pop_2000)"
  )
  f4 <- as.formula(paste0(
    "dlog_income_per_capita ~ ", controls_4,
    " | ra_id | dummy_1000m ~ dummyp_1000m"
  ))

  m_base <- feols(f4, data = dat_clean, se = "cluster", cluster = ~ra_id)
  res    <- residuals(m_base)
  coords_mat <- as.matrix(dat_clean[, c("lon", "lat")])

  bands <- seq(0, 15, by = 1)
  correlogram <- lapply(seq_len(length(bands) - 1), function(k) {
    nb <- tryCatch(
      spdep::dnearneigh(coords_mat, bands[k], bands[k + 1], longlat = TRUE),
      error = function(e) NULL
    )
    if (is.null(nb) || sum(spdep::card(nb)) < 10) return(NULL)
    w  <- spdep::nb2listw(nb, style = "W", zero.policy = TRUE)
    mt <- spdep::moran.test(res, w, zero.policy = TRUE)
    data.frame(
      d_low = bands[k],
      d_up  = bands[k + 1],
      d_mid = (bands[k] + bands[k + 1]) / 2,
      I     = unname(mt$estimate["Moran I statistic"]),
      EI    = unname(mt$estimate["Expectation"]),
      Z     = unname(mt$statistic),
      p     = mt$p.value
    )
  })
  correlogram_df <- do.call(rbind, Filter(Negate(is.null), correlogram))

  # ── Estimation with Conley 3 km (four specs) ────────────────────────────────
  specs <- list(
    eq1 = "log(income_per_capita_2000)",
    eq2 = paste("log(income_per_capita_2000) +",
                "log(dist_cbd_2000) + log(dist_highway_2000)"),
    eq3 = paste("log(income_per_capita_2000) +",
                "share_college_complete_2000 + share_illiterate_2000 +",
                "share_over_65_2000 + log(pop_2000)"),
    eq4 = controls_4
  )

  models <- lapply(specs, function(ctrl) {
    f <- as.formula(paste0(
      "dlog_income_per_capita ~ ", ctrl,
      " | ra_id | dummy_1000m ~ dummyp_1000m"
    ))
    list(
      cluster_ra = feols(f, data = dat_clean,
                         se = "cluster", cluster = ~ra_id),
      conley_3km = feols(f, data = dat_clean,
                         vcov = conley(cutoff = 3))
    )
  })

  list(correlogram = correlogram_df, models = models)
}
