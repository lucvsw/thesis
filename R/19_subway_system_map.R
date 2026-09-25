# 19_subway_system_map.R
# Descriptive figure for the Section "The Brasília Subway System".
# Generates an orientation map that names the Administrative Regions
# (RAs) served by the subway and shows the lines, the stations and the
# Central station (terminal at the Plano Piloto). The goal is to let the
# reader (especially an international one) understand how the system works
# as an inter-regional connector between the periphery and the employment
# core (Plano Piloto), which the text mentions by name.

generate_subway_system_map <- function(stations_sf, lines_sf, RAs_sf,
                                     output_path = NULL) {

  default_crs <- 31983  # SIRGAS 2000 / UTM 23S (meters)

  stations_sf <- st_zm(st_transform(stations_sf, default_crs))
  lines_sf   <- st_zm(st_transform(lines_sf,   default_crs))
  RAs_sf      <- st_zm(st_transform(RAs_sf,      default_crs))

  # ---- Administrative Regions served by the system (6, as in the text) ----
  ras_served <- c("PLANO PILOTO", "GUARÁ", "ÁGUAS CLARAS",
                    "TAGUATINGA", "CEILÂNDIA", "SAMAMBAIA")

  RAs_sf <- RAs_sf %>%
    mutate(served = ra_name %in% ras_served)

  ras_served_sf <- RAs_sf %>% filter(served)

  # Title-case labels for display
  ra_labels <- c(
    "PLANO PILOTO" = "Plano Piloto",
    "GUARÁ"        = "Guará",
    "ÁGUAS CLARAS" = "Águas Claras",
    "TAGUATINGA"   = "Taguatinga",
    "CEILÂNDIA"    = "Ceilândia",
    "SAMAMBAIA"    = "Samambaia"
  )

  # RAs whose name is displayed (Plano Piloto is omitted: it is already identified by
  # the Central station label on the right)
  ras_labeled <- setdiff(ras_served, "PLANO PILOTO")

  centroids_ra <- ras_served_sf %>%
    filter(ra_name %in% ras_labeled) %>%
    st_centroid() %>%
    mutate(label_txt = ra_labels[ra_name])
  coords_ra <- st_coordinates(centroids_ra)
  centroids_ra$x <- coords_ra[, 1]
  centroids_ra$y <- coords_ra[, 2]

  # Small manual adjustments to avoid overlap with the lines/stations
  nudge <- list(
    "PLANO PILOTO" = c(  3500,  4200),
    "GUARÁ"        = c(  1600,   900),
    "ÁGUAS CLARAS" = c( -1000,  1300),
    "TAGUATINGA"   = c(   400,  1200),
    "CEILÂNDIA"    = c(  7000,  4200),
    "SAMAMBAIA"    = c(  2500,   300)
  )
  for (nm in names(nudge)) {
    i <- which(centroids_ra$ra_name == nm)
    centroids_ra$x[i] <- centroids_ra$x[i] + nudge[[nm]][1]
    centroids_ra$y[i] <- centroids_ra$y[i] + nudge[[nm]][2]
  }

  # ---- Lines: Green (Central–Ceilândia) and Orange (Central–Samambaia) ----
  # The two lines share the central trunk (Central <-> junction). In the
  # cartographic base, the trunk is represented only within the long segment
  # (Green route) and the Samambaia branch appears as a short segment, so
  # the Orange line would not visually reach Central. To make both lines
  # visible, we rebuild the full Orange route
  # (Samambaia -> junction -> Central) reusing the Green trunk and then
  # shift the two lines laterally (parallel offset, standard in subway
  # maps) so that the shared trunk shows both.
  seg_lengths <- as.numeric(st_length(lines_sf$geometry))
  idx_green <- which.max(seg_lengths)  # Ceilândia–Central route (includes the trunk)
  idx_others <- setdiff(seq_along(seg_lengths), idx_green)
  idx_spur <- idx_others[which.max(seg_lengths[idx_others])]  # Samambaia branch

  green_coords <- st_coordinates(lines_sf$geometry[[idx_green]])[, 1:2]
  spur_coords  <- st_coordinates(lines_sf$geometry[[idx_spur]])[, 1:2]

  central_xy <- as.numeric(
    st_coordinates(stations_sf[stations_sf$station_name == "ESTAÇÃO CENTRAL", ])[1, 1:2]  # "ESTAÇÃO CENTRAL" = Central station (name in the source data)
  )

  # Orient the Green line as Ceilândia(1) -> Central(n)
  if (sum((green_coords[1, ] - central_xy)^2) <
      sum((green_coords[nrow(green_coords), ] - central_xy)^2)) {
    green_coords <- green_coords[nrow(green_coords):1, , drop = FALSE]
  }

  # Squared distance from a point to all vertices of the Green line
  dist2_green <- function(p) colSums((t(green_coords) - p)^2)

  # Junction = end of the branch closest to the Green line
  e1 <- spur_coords[1, ]; e2 <- spur_coords[nrow(spur_coords), ]
  if (min(dist2_green(e1)) < min(dist2_green(e2))) {
    junction <- e1
    spur_coords <- spur_coords[nrow(spur_coords):1, , drop = FALSE]  # Samambaia -> junction
  } else {
    junction <- e2
  }

  # Shared trunk: from the junction to Central (final part of the Green line)
  j_idx <- which.min(dist2_green(junction))
  trunk_coords <- green_coords[j_idx:nrow(green_coords), , drop = FALSE]

  # Full Orange route: Samambaia -> junction -> Central
  orange_coords <- rbind(spur_coords, trunk_coords)

  # Both lines are solid. So that both are visible and
  # distinguishable on the shared trunk (without the solid Green line hiding the
  # Orange one), we apply a small lateral shift (parallel offset,
  # standard in subway maps) ONLY on the trunk. The branches stay on the
  # actual geometry, so each branch station sits exactly on its
  # line; on the trunk, the (shared) stations are centered between the
  # two parallel lines.
  line_offset <- function(coords, d) {
    n <- nrow(coords)
    seg <- diff(coords)
    seglen <- sqrt(rowSums(seg^2))
    seglen[seglen == 0] <- 1e-9
    seg_unit <- seg / seglen
    norm_seg <- cbind(-seg_unit[, 2], seg_unit[, 1])  # left normal
    vnorm <- matrix(0, n, 2)
    vnorm[1, ] <- norm_seg[1, ]
    vnorm[n, ] <- norm_seg[n - 1, ]
    if (n > 2) {
      for (i in 2:(n - 1)) {
        v <- norm_seg[i - 1, ] + norm_seg[i, ]
        vl <- sqrt(sum(v^2))
        vnorm[i, ] <- if (vl < 1e-9) norm_seg[i, ] else v / vl
      }
    }
    coords + d * vnorm
  }

  d_off <- 130  # meters of shift on each side of the trunk

  green_branch  <- green_coords[seq_len(j_idx - 1), , drop = FALSE]  # Ceilândia -> junction
  trunk_green   <- line_offset(trunk_coords,  d_off)
  trunk_orange  <- line_offset(trunk_coords, -d_off)

  green_line_coords  <- rbind(green_branch, trunk_green)
  orange_line_coords <- rbind(spur_coords,  trunk_orange)  # Samambaia -> junction -> Central

  lines_plot <- rbind(
    st_sf(line_name = "Green Line  (Central – Ceilândia)",
          geometry = st_sfc(st_linestring(green_line_coords),  crs = default_crs)),
    st_sf(line_name = "Orange Line  (Central – Samambaia)",
          geometry = st_sfc(st_linestring(orange_line_coords), crs = default_crs))
  )

  line_colors <- c(
    "Green Line  (Central – Ceilândia)"  = "#1a9850",
    "Orange Line  (Central – Samambaia)" = "#e6720a"
  )

  # ---- Stations ----
  stations_sf <- stations_sf %>%
    mutate(in_operation = status == "EM OPERAÇÃO")  # "EM OPERAÇÃO" = in operation (value in the source data)
  central_sf <- stations_sf %>% filter(station_name == "ESTAÇÃO CENTRAL")
  cc <- st_coordinates(central_sf)

  # ---- Framing: subway corridor (bbox of the served RAs + margin) ----
  bb <- st_bbox(ras_served_sf)
  mx <- 2500
  xlim <- c(bb["xmin"] - mx, bb["xmax"] + mx)
  ylim <- c(bb["ymin"] - mx, bb["ymax"] + mx)

  # ---- Manual scale bar (5 km) ----
  scale_len <- 5000
  scale_x0 <- xlim[1] + 0.06 * diff(xlim)
  scale_y0 <- ylim[1] + 0.05 * diff(ylim)
  scale_bar <- data.frame(x = scale_x0, xend = scale_x0 + scale_len,
                             y = scale_y0, yend = scale_y0)

  # ---- Plot ----
  p <- ggplot() +
    # context: other RAs of the Federal District
    geom_sf(data = RAs_sf, fill = "grey95", color = "grey80", linewidth = 0.2) +
    # served RAs highlighted
    geom_sf(data = ras_served_sf, aes(), fill = "#dceaf2",
            color = "grey55", linewidth = 0.35) +
    # subway lines (both solid; shared trunk with parallel offset)
    geom_sf(data = lines_plot, aes(color = line_name),
            linewidth = 1.2, lineend = "round") +
    # stations
    geom_sf(data = stations_sf, shape = 21, fill = "white",
            color = "grey20", size = 1.7, stroke = 0.5) +
    # Central station (terminal)
    geom_sf(data = central_sf, shape = 22, fill = "black",
            color = "black", size = 3.2) +
    annotate("text", x = cc[1] + 1200, y = cc[2] - 200,
             label = "Central Station\n(Plano Piloto)",
             hjust = 0, vjust = 1, size = 3, fontface = "bold") +
    # labels of the served RAs
    geom_text(data = centroids_ra,
              aes(x = x, y = y, label = label_txt),
              size = 3.1, fontface = "bold", color = "grey15",
              lineheight = 0.9) +
    # scale bar
    geom_segment(data = scale_bar,
                 aes(x = x, xend = xend, y = y, yend = yend),
                 linewidth = 0.8) +
    annotate("text", x = scale_x0 + scale_len / 2, y = scale_y0 + 0.018 * diff(ylim),
             label = "5 km", size = 2.8) +
    # simple north arrow
    annotate("segment", x = xlim[2] - 0.04 * diff(xlim),
             xend = xlim[2] - 0.04 * diff(xlim),
             y = ylim[2] - 0.12 * diff(ylim),
             yend = ylim[2] - 0.05 * diff(ylim),
             arrow = arrow(length = unit(0.18, "cm"), type = "closed")) +
    annotate("text", x = xlim[2] - 0.04 * diff(xlim),
             y = ylim[2] - 0.135 * diff(ylim), label = "N", size = 3) +
    scale_color_manual(values = line_colors, name = NULL) +
    coord_sf(xlim = xlim, ylim = ylim, expand = FALSE, datum = NA) +
    theme_minimal(base_size = 11) +
    theme(
      legend.position = c(0.015, 0.99),
      legend.justification = c(0, 1),
      legend.background = element_rect(fill = scales::alpha("white", 0.7),
                                       color = NA),
      legend.key = element_blank(),
      legend.text = element_text(size = 9),
      axis.title = element_blank(),
      axis.text = element_blank(),
      panel.grid = element_blank(),
      panel.border = element_rect(color = "grey40", fill = NA, linewidth = 0.4)
    )

  # ---- Save into the paper's image folder (paper/images) ----
  if (is.null(output_path)) {
    dir_img <- here::here("paper", "images")
    dir.create(dir_img, showWarnings = FALSE, recursive = TRUE)
    output_path <- file.path(dir_img, "subway_system_bsb.png")
  }
  ggsave(output_path, plot = p, width = 8, height = 6.2,
         dpi = 300, bg = "white")

  return(normalizePath(output_path))
}
