

# # Regressão IV para o ano 2000:
# # Nesse exemplo, a variável independente é a população e a variável endógena é dummy_metro.
# # O instrumento é dummy_projeto.
# regressao_com_ivreg_2000 <- function(censo_sf) {
#   censo_sf %>%
#     filter(ano == 2000, !is.na(pop), pop > 0) %>%
#     ivreg(log(pop) ~ dummy_metro_passa | dummy_projeto_passa, data = .)
# }
# 
# # Regressão IV para o ano 2010:
# regressao_com_ivreg_2010 <- function(censo_sf) {
#   censo_sf %>%
#     filter(ano == 2010, !is.na(pop), pop > 0) %>%
#     ivreg(log(pop) ~ dummy_metro_passa | dummy_projeto_passa, data = .)
# }
# 
# eq3 <- feols(log(pop)  ~ - 1 | dummy_metro_passa ~ dummy_projeto_passa,
#              se="hetero", data=censo_sf)
# summary(eq3)
# summary(eq3, stage=1)
