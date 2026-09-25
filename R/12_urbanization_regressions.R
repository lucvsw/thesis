# =============================================================================
# 12_urbanization_regressions.R — Regressions with urbanization as the dependent variable
#
# Question: did subway access increase the share of the tract's area that is urbanized?
# DV: change in the urban-area share (MapBiomas) between 2000 and 2010
# Strategy: 2SLS identical to 6_main_regressions.R
# =============================================================================

prepare_urbanization_sample <- function(census_urbanization) {

  # 2000 controls
  controls_2000 <- census_urbanization %>%
    filter(year == 2000) %>%
    st_drop_geometry() %>%
    select(code_tract,
           share_college_complete,
           share_illiterate,
           share_over_65,
           dist_cbd,
           income_per_capita,
           pop,
           dist_highway,
           share_urban_mapbiomas) %>%
    rename_with(~ paste0(.x, "_2000"), -code_tract)

  # Base: year 2010 with treatment/instrument variables
  census_urbanization %>%
    filter(year == 2010) %>%
    left_join(controls_2000, by = "code_tract") %>%
    mutate(
      # DV: change in the urbanized share 2000 → 2010
      d_share_urban = share_urban_mapbiomas - share_urban_mapbiomas_2000
    )
}

run_regressions_urbanization <- function(smpl) {

  dat <- smpl %>% filter(dummy_subway_10km == 1)

  # (1) Baseline share_urban only + RA FE
  eq_urb_1 <- feols(
    d_share_urban ~ share_urban_mapbiomas_2000
    | ra_id
    | dummy_1000m ~ dummyp_1000m,
    data = dat, se = "cluster", cluster = ~ra_id
  )

  # (2) + geographic controls
  eq_urb_2 <- feols(
    d_share_urban ~ share_urban_mapbiomas_2000 +
      log(dist_cbd_2000) + log(dist_highway_2000)
    | ra_id
    | dummy_1000m ~ dummyp_1000m,
    data = dat, se = "cluster", cluster = ~ra_id
  )

  # (3) + socioeconomic controls
  eq_urb_3 <- feols(
    d_share_urban ~ share_urban_mapbiomas_2000 +
      share_college_complete_2000 + share_illiterate_2000 +
      share_over_65_2000 + log(pop_2000)
    | ra_id
    | dummy_1000m ~ dummyp_1000m,
    data = dat, se = "cluster", cluster = ~ra_id
  )

  # (4) All controls
  eq_urb_4 <- feols(
    d_share_urban ~ share_urban_mapbiomas_2000 +
      log(dist_cbd_2000) + log(dist_highway_2000) +
      share_college_complete_2000 + share_illiterate_2000 +
      share_over_65_2000 + log(pop_2000)
    | ra_id
    | dummy_1000m ~ dummyp_1000m,
    data = dat, se = "cluster", cluster = ~ra_id
  )

  list(eq_urb_1 = eq_urb_1, eq_urb_2 = eq_urb_2,
       eq_urb_3 = eq_urb_3, eq_urb_4 = eq_urb_4)
}

# -----------------------------------------------------------------------------
# Reading the results
# -----------------------------------------------------------------------------
# res <- tar_read(results_urbanization)
# 
# # Second stage — effect of the subway on the change in urban cover
# etable(res$eq_urb_1, res$eq_urb_2, res$eq_urb_3, res$eq_urb_4,
#        stage = 2, headers = c("(1)", "(2)", "(3)", "(4)"))
# 
# # First stage — instrument diagnostics
# etable(res$eq_urb_1, res$eq_urb_2, res$eq_urb_3, res$eq_urb_4,
#        stage = 1, headers = c("(1)", "(2)", "(3)", "(4)"))
# 
# # Specific model
# summary(res$eq_urb_1)
# summary(res$eq_urb_2)
# summary(res$eq_urb_3)
# summary(res$eq_urb_4)
