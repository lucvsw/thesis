# =============================================================================
# 5b_descriptive_statistics.R — Descriptive statistics table
# Sample: census tracts within 10 km of the nearest station
# By exposure (≤ 1 km vs > 1 km) and by year (2000 vs 2010)
# =============================================================================

descriptive_statistics_table <- function(census_final) {

  smpl <- census_final %>%
    filter(dummy_subway_10km == 1) %>%
    st_drop_geometry()

  d00_e <- smpl %>% filter(year == 2000, dummy_1000m == 1)
  d00_n <- smpl %>% filter(year == 2000, dummy_1000m == 0)
  d10_e <- smpl %>% filter(year == 2010, dummy_1000m == 1)
  d10_n <- smpl %>% filter(year == 2010, dummy_1000m == 0)

  N_e <- nrow(d00_e)
  N_n <- nrow(d00_n)

  # Returns c(mean_str, "(sd_str)") for a numeric vector
  ms <- function(x, f = "%.0f") {
    x <- x[!is.na(x)]
    if (length(x) == 0) return(c("---", ""))
    c(sprintf(f, mean(x)), paste0("(", sprintf(f, sd(x)), ")"))
  }

  na_ms <- c("---", "")

  # Two-line row: mean line then SD-in-parentheses line
  # Columns: label | exposed_2000 | exposed_2010 | nonexp_2000 | nonexp_2010
  r2 <- function(label, a, b, c, d) {
    c(
      paste0(label, " & ", a[1], " & ", b[1], " & ", c[1], " & ", d[1], "\\\\"),
      paste0(" & ",  a[2], " & ", b[2], " & ", c[2], " & ", d[2], "\\\\[4pt]")
    )
  }

  paste(c(
    "\\begin{table}[!h]",
    "\\centering",
    "\\caption{Descriptive statistics by subway exposure --- census tracts within 10 km (2000 and 2010)}",
    "\\label{tab:descriptive}",
    "\\resizebox{0.99\\textwidth}{!}{%",
    "\\begin{tabular}{lcccc}",
    "\\toprule",
    " & \\multicolumn{2}{c}{Exposed ($\\leq$ 1 km)} & \\multicolumn{2}{c}{Non-exposed ($>$ 1 km)} \\\\",
    "\\cmidrule(lr){2-3}\\cmidrule(lr){4-5}",
    " & 2000 & 2010 & 2000 & 2010 \\\\",
    paste0("Number of tracts & ", N_e, " & ", N_e, " & ", N_n, " & ", N_n, " \\\\"),
    "\\midrule",
    "\\multicolumn{5}{l}{\\textit{Panel A: Outcome}} \\\\[2pt]",
    r2("Per capita income (R\\$)",
       ms(d00_e$income_per_capita),
       ms(d10_e$income_per_capita),
       ms(d00_n$income_per_capita),
       ms(d10_n$income_per_capita)),
    "\\midrule",
    "\\multicolumn{5}{l}{\\textit{Panel B: Geographic controls}} \\\\[2pt]",
    r2("Distance to CBD (meters)",
       ms(d00_e$dist_cbd),
       na_ms,
       ms(d00_n$dist_cbd),
       na_ms),
    r2("Distance to nearest road (meters)",
       ms(d00_e$dist_highway),
       na_ms,
       ms(d00_n$dist_highway),
       na_ms),
    "\\midrule",
    "\\multicolumn{5}{l}{\\textit{Panel C: Socioeconomic baseline (2000)}} \\\\[2pt]",
    r2("Population",
       ms(d00_e$pop),
       ms(d10_e$pop),
       ms(d00_n$pop),
       ms(d10_n$pop)),
    r2("Share of illiterate residents",
       ms(d00_e$share_illiterate, "%.3f"),
       na_ms,
       ms(d00_n$share_illiterate, "%.3f"),
       na_ms),
    r2("Share with college degree",
       ms(d00_e$share_college_complete, "%.3f"),
       na_ms,
       ms(d00_n$share_college_complete, "%.3f"),
       na_ms),
    r2("Share aged 65 or older",
       ms(d00_e$share_over_65, "%.3f"),
       na_ms,
       ms(d00_n$share_over_65, "%.3f"),
       na_ms),
    "\\bottomrule",
    "\\end{tabular}%",
    "}",
    "\\footnotesize",
    "\\begin{flushleft}",
    paste0("\\textit{Note}: Standard deviations in parentheses. ",
           "Exposed tracts lie within 1 km of the nearest subway station; non-exposed tracts lie between 1 km and 10 km. ",
           "Geographic and socioeconomic variables enter the regressions as 2000 baseline characteristics. ",
           "Statistics for 2010 are not reported for variables not available in the 2010 Census extracts."),
    "\\end{flushleft}",
    "\\end{table}"
  ), collapse = "\n")
}

# tar_read(descriptive_table) |> cat()
