# =============================================================================
# 18_heterogeneity_cfam.R — Heterogeneity by the RA's maximum CfAM
#
# Question: was the subway effect on income larger in RAs with more permissive
#           land-use regulation (higher maximum floor-area ratio,
#           CfAM)?
# Strategy: 2SLS with a continuous interaction dummy_1000m × cfam_max_ra,
#             instrumented by dummyp_1000m × cfam_max_ra.
#             The CfAM main effect is absorbed by the RA FE.
# Source: PDOT/2009 (LC 803/2009; values taken from Annex V).
#        The Plano Piloto (governed by the PPCUB) and six RAs without a value in Annex V
#        receive estimates — see cfam_estimated.
# =============================================================================

# -----------------------------------------------------------------------------
# Lookup: standard maximum CfAM by RA (numeric code as a string)
# Criterion: maximum value of the "Vias de Atividades" or "Centro Urbano" column
#           — ceiling of permissiveness of the most dynamic zone, excluding
#           exceptions for specific lots.
# RAs flagged with cfam_estimated = TRUE are not in Annex V; values are
# assigned by analogy with urban characteristics and neighboring RAs.
# -----------------------------------------------------------------------------
cfam_lookup <- tibble::tribble(
  ~ra_id, ~cfam_max_ra, ~cfam_estimated,
  "1",   2.00, TRUE,   # Plano Piloto   — UNESCO heritage listing/PPCUB; estimate for the most permissive zones
  "2",   3.00, FALSE,  # Gama
  "3",   7.00, FALSE,  # Taguatinga
  "4",   3.00, FALSE,  # Brazlândia
  "5",   7.00, FALSE,  # Sobradinho
  "6",   5.00, FALSE,  # Planaltina
  "7",   4.00, FALSE,  # Paranoá
  "8",   5.00, FALSE,  # Núcleo Bandeirante
  "9",   6.00, FALSE,  # Ceilândia
  "10",  3.00, FALSE,  # Guará
  "11",  2.50, TRUE,   # Cruzeiro           — estimate; residential superblocks
  "12",  6.00, FALSE,  # Samambaia
  "13",  2.50, FALSE,  # Santa Maria
  "14",  4.00, FALSE,  # São Sebastião
  "15",  2.00, FALSE,  # Recanto das Emas
  "16",  3.00, FALSE,  # Lago Sul
  "17",  3.00, FALSE,  # Riacho Fundo
  "18",  4.44, FALSE,  # Lago Norte
  "19",  3.00, FALSE,  # Candangolândia
  "20",  7.00, FALSE,  # Águas Claras
  "21",  3.00, FALSE,  # Riacho Fundo II
  "22",  4.00, TRUE,   # Sudoeste/Octogonal — estimate; high planned density
  "23",  2.40, FALSE,  # Varjão
  "24",  1.00, FALSE,  # Park Way
  "25",  2.00, FALSE,  # SCIA
  "26",  3.00, FALSE,  # Sobradinho II
  "27",  1.00, TRUE,   # Jardim Botânico    — estimate; low-density housing developments
  "29",  3.00, FALSE,  # SIA
  "30",  2.50, TRUE,   # Vicente Pires      — estimate; recent regularization
  "32",  6.00, TRUE,   # Sol Nascente/Pôr do Sol — estimate; split from Ceilândia
  "33",  7.00, TRUE    # Arniqueira         — estimate; split from Águas Claras
)

add_cfam <- function(smpl) {
  smpl %>%
    left_join(cfam_lookup, by = "ra_id") %>%
    mutate(
      cfam_centered     = cfam_max_ra - mean(cfam_max_ra, na.rm = TRUE),
      i_dummy_cfam      = dummy_1000m  * cfam_centered,
      i_dummyp_cfam     = dummyp_1000m * cfam_centered
    )
}

run_heterogeneity_cfam <- function(smpl) {

  dat <- smpl %>%
    filter(dummy_subway_10km == 1) %>%
    add_cfam() %>%
    filter(!is.na(cfam_max_ra))

  controls <- paste(
    "log(income_per_capita_2000)",
    "log(dist_cbd_2000)", "log(dist_highway_2000)",
    "share_college_complete_2000", "share_illiterate_2000",
    "share_over_65_2000", "log(pop_2000)",
    sep = " + "
  )

  fml <- as.formula(paste0(
    "dlog_income_per_capita ~ ", controls,
    " | ra_id",
    " | dummy_1000m + i_dummy_cfam ~ dummyp_1000m + i_dummyp_cfam"
  ))

  # Full sample (including RAs with estimated CfAM)
  m_full <- feols(fml, data = dat, se = "cluster", cluster = ~ra_id)

  # Sample restricted to RAs with an official value from Annex V
  m_official <- feols(fml,
    data = dat %>% filter(!cfam_estimated),
    se = "cluster", cluster = ~ra_id
  )

  list(
    m_full    = m_full,
    m_official = m_official,
    cfam_lookup = cfam_lookup
  )
}
