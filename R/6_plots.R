# # ### PLOTS (deve-se alterar o ano no filtro para visualizar em 2000 e 2010)
# censo_sf_2000 <- censo_sf %>%
#   filter(ano == 2010) %>%
#   mutate(
#     exposto_metro_pop   = replace_na(as.integer(dummy_metro_passa == 1 & pop > 0), 0),
#     exposto_projeto_pop = replace_na(as.integer(dummy_projeto_passa == 1 & pop > 0), 0)
#   )
# # # ---- Plot 1: Exposição à linha de metro efetiva ----
# ggplot() +
#   # Preenche os setores censitários de acordo com dummy_metro (exposição se a distância for <= 1km)
#   geom_sf(data = censo_sf_2000, aes(fill = factor(exposto_metro_pop, levels = c("1", "0"))), color = "black", alpha = 1) +
#   # Sobrepõe a linha de metro
#   geom_sf(data = linhas_sf, color = "blue", size = 1) +
#   scale_fill_manual(values = c("1" = "#BBD8A3", "0" = "#F2F2F2"),
#                     name = NULL,
#                     labels = c("1" = "Exposto", "0" = "Não exposto")) +
#   labs(title = "Exposição dos Setores à Linha de Metro (2010)") +
#   theme(legend.position = "bottom",
#         legend.text.position = "right",
#         legend.title.position = "top",
#         legend.direction = "horizontal",
#         panel.background = element_rect(fill = "white", colour = "black"),
#         panel.grid = element_line(colour = "darkgrey", linewidth = 0.2),
#         axis.text.x = element_blank(),
#         axis.text.y = element_blank(),
#         axis.ticks.x = element_blank(),
#         axis.ticks.y = element_blank(),
#         legend.margin = margin(t = -4, r = 0, b = 0, l = 0),
#         legend.key.height = unit(0.4, "cm"),
#         legend.key.width = unit(0.6, "cm"),
#         legend.key = element_rect(linewidth = 0.05),
#         legend.text = element_text(size = 15),
#         title = element_text(size = 15),
#         theme(text = element_text(family = "Roboto")))
# 
# # ---- Plot 2: Exposição ao projeto de metro descartado ----
# ggplot() +
#   # Preenche os setores censitários de acordo com dummy_projeto (exposição se a distância for <= 1km)
#   geom_sf(data = censo_sf_2000, aes(fill = factor(exposto_projeto_pop, levels = c("1", "0"))), color = "black", alpha = 1) +
#   # Sobrepõe a geometria do projeto descartado
#   geom_sf(data = projeto_sf, color = "red", size = 1) +
#   scale_fill_manual(values = c("0" = "#F2F2F2", "1" = "#d9b38c"),
#                     name = NULL,
#                     labels = c("1" = "Exposto", "0" = "Não exposto")) +
#   labs(title = "Exposição dos Setores ao Projeto de Metro Descartado (2010)") +
#   theme(legend.position = "bottom",
#         legend.text.position = "right",
#         legend.title.position = "top",
#         legend.direction = "horizontal",
#         panel.background = element_rect(fill = "white", colour = "black"),
#         panel.grid = element_line(colour = "darkgrey", linewidth = 0.2),
#         axis.text.x = element_blank(),
#         axis.text.y = element_blank(),
#         axis.ticks.x = element_blank(),
#         axis.ticks.y = element_blank(),
#         legend.margin = margin(t = -4, r = 0, b = 0, l = 0),
#         legend.key.height = unit(0.4, "cm"),
#         legend.key.width = unit(0.6, "cm"),
#         legend.key = element_rect(linewidth = 0.05),
#         legend.text = element_text(size = 15),
#         title = element_text(size = 15),
#         theme(text = element_text(family = "Roboto")))
# 
# ggplot() +
#   geom_sf(
#     data = censo_sf %>% filter(ano == 2000, renda_por_domicilios > 0),
#     aes(fill = renda_por_domicilios), alpha = 1, color = "black") +
#   scale_fill_distiller(
#     palette = "Blues",
#     trans = "log10",
#     direction = 1,
#     name = "Renda (log)",
#     breaks = range(censo_sf$renda_por_domicilios[censo_sf$ano == 2010 & censo_sf$renda_por_domicilios > 0], na.rm = TRUE)
#   ) +
#   theme(legend.position = "bottom",
#         legend.text.position = "bottom",
#         legend.title.position = "top",
#         legend.direction = "horizontal",
#         panel.background = element_rect(fill = "white", colour = "black"),
#         panel.grid = element_line(colour = "darkgrey", linewidth = 0.2),
#         axis.text.x = element_blank(),
#         axis.text.y = element_blank(),
#         axis.ticks.x = element_blank(),
#         axis.ticks.y = element_blank(),
#         legend.margin = margin(t = -4, r = 0, b = 0, l = 0),
#         legend.key.height = unit(0.4, "cm"),
#         legend.key.width = unit(1, "cm"),
#         legend.key = element_rect(linewidth = 0.05),
#         legend.text = element_text(size = 10),
#         title = element_text(size = 14),
#         theme(text = element_text(family = "Roboto"))) +
#   labs(
#     title = "Renda por domicílios - 2010"
#   )
