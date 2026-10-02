library(ggspatial)
library(spdep)
library(sf)
library(sp)
library(dplyr)
library(ggplot2)
library(tidyverse)
load("data/roads_mtr.RData")
load("data/lop_vlg.RData")
load("data/lop_kc.RData")

mtrvlg <- lop_vlg %>% 
  subset(WADMKK == "Kota Mataram")

mtrvlg <- mtrvlg %>%
  mutate(
    kelurahan = sub("^Kelurahan\\s+", "", NAMOBJ)
  )

mtrvlg <- mtrvlg %>%
  mutate(
    kelurahan = recode(
      kelurahan,
      "Sayangsayang" = "Sayang-sayang"
    )
  )

mtrvlg_umkm <- mtrvlg %>%
  left_join(
    umkm,
    by = "kelurahan"
  )

umkm_kelu <- umkm %>%
  count(kelurahan, name = "jumlah_umkm")

mtrvlg_umkm <- mtrvlg %>%
  left_join(
    umkm_kelu,
    by = "kelurahan"
  ) %>%
  mutate(
    jumlah_umkm = replace_na(jumlah_umkm, 0)
  )

roads_mtr <- st_read("data/mtr/JALAN_LN_25K.shp")
roads_mtr <- st_make_valid(roads_mtr)
roads_mtr <- roads_mtr[!st_is_empty(roads_mtr), ]
roads_mtr <- st_intersection(roads_mtr, mtrvlg)

river_mtr <- st_read("data/mtr/SUNGAI_LN_25K.shp")
river_mtr <- st_make_valid(river_mtr)
river_mtr <- river_mtr[!st_is_empty(river_mtr), ]
river_mtr <- st_intersection(river_mtr, mtrvlg)

mtr_map <- ggplot() +
  geom_sf(data = mtrvlg,
          fill = "#1e2f47",
          color = "#f4f1ea",
          linewidth = 0.5) +        # batas paling tegas
  geom_sf(data = roads_mtr,
          color = "#f4f1ea",
          linewidth = 0.25,
          alpha = 0.7) +
  theme_void()

ggsave("figs/mataram_roads.png",
       width = 10,
       height = 5.6,
       dpi = 300)

custom_palette <- c(
  "#2c3e63",
  "#e26a2c",
  "#5e8e83",
  "#b0bdc1",
  "#fad02c"
)

lop_map <- ggplot() +
  geom_sf(data = lop_kc,
          aes(fill = WADMKK),
          color = "#f4f1ea",
          linewidth = 0.25) +
  scale_fill_manual(values = custom_palette) +
  theme_void() +
  theme(
    legend.position = "none"
  )

ggsave("figs/lop.png",
       width = 10,
       height = 5.6,
       dpi = 300)


library(patchwork)

lop_map +
  geom_circle(aes(x0 = 116.11, y0 = -8.59, r = 0.05),
              color = "#2c3e63",
              fill = NA,
              linewidth = 1) +
  inset_element(
    mtr_map,
    left   = 0.02,
    bottom = 0.45,
    right  = 0.45,
    top    = 0.98
  ) +
theme(
  plot.background = element_rect(
    fill = "NA",
    color = "#1e2f47",
    linewidth = 0.8
  ),
  plot.margin = margin(5,5,5,5)
)

ggsave("figs/mtrlop.png",
       width = 10,
       height = 5.6,
       dpi = 300)

library(ggforce)

lop_map +
  geom_circle(aes(x0 = 116.11, y0 = -8.59, r = 0.05),
              color = "#2c3e63",
              fill = NA,
              linewidth = 1.5)



library(sf)
library(spdep)
library(dplyr)
library(ggplot2)

# ============================================================
# 1. Membentuk neighbourhood berdasarkan queen contiguity
# ============================================================

nb_kel <- poly2nb(
  mtrvlg,
  queen = TRUE
)

# ============================================================
# 2. Ringkasan jumlah tetangga
# ============================================================

neighbour_summary <- tibble(
  kelurahan = mtrvlg$kelurahan,
  n_neighbour = card(nb_kel)
)

neighbour_summary

table(card(nb_kel))
summary(card(nb_kel))

# Centroid kelurahan
centroid_kel <- st_centroid(mtrvlg)

# Koordinat centroid
coords <- st_coordinates(centroid_kel)

# Membentuk pasangan tetangga
edges <- lapply(seq_along(nb_kel), function(i) {
  
  if (length(nb_kel[[i]]) == 0) return(NULL)
  
  tibble(
    from = i,
    to = nb_kel[[i]]
  )
}) %>%
  bind_rows()

# Menghindari duplikasi hubungan A-B dan B-A
edges <- edges %>%
  filter(from < to)

# Koordinat garis
edges_sf <- edges %>%
  mutate(
    x1 = coords[from, 1],
    y1 = coords[from, 2],
    x2 = coords[to, 1],
    y2 = coords[to, 2]
  )

ggplot() +
  geom_sf(
    data = mtrvlg,
    fill = "grey95",
    color = "grey60",
    linewidth = 0.3
  ) +
  geom_segment(
    data = edges_sf,
    aes(
      x = x1,
      y = y1,
      xend = x2,
      yend = y2
    ),
    color = "grey30",
    linewidth = 0.4,
    alpha = 0.6
  ) +
  geom_sf(
    data = centroid_kel,
    size = 1.5
  ) +
  theme_void()