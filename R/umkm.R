library(osmdata)
library(sf)
library(osrm)
library(dplyr)
library(ggplot2)
library(ggspatial)

q_shop <- opq("Mataram, Indonesia") %>%
  add_osm_feature(key = "shop")

q_amenity <- opq("Mataram, Indonesia") %>%
  add_osm_feature(key = "amenity",
                  value = c("restaurant","cafe","fast_food","pharmacy"))

q_craft <- opq("Mataram, Indonesia") %>%
  add_osm_feature(key = "craft")

q_office <- opq("Mataram, Indonesia") %>%
  add_osm_feature(key = "office",
                  value = c("travel_agent","accountant"))

q_tourism <- opq("Mataram, Indonesia") %>%
  add_osm_feature(key = "tourism",
                  value = c("guest_house","homestay"))

d_shop <- osmdata_sf(q_shop)
d_amenity <- osmdata_sf(q_amenity)
d_craft <- osmdata_sf(q_craft)
d_office <- osmdata_sf(q_office)
d_tourism <- osmdata_sf(q_tourism)

all_points <- dplyr::bind_rows(
  d_shop$osm_points,
  d_amenity$osm_points,
  d_craft$osm_points,
  d_office$osm_points,
  d_tourism$osm_points
)

umkm <- all_points %>% filter(!is.na(name))
umkm <- umkm %>%
  filter(
    !grepl("alfamart|indomaret|mall|supermarket|niaga", name, ignore.case = TRUE)
  )

umkm <- st_transform(umkm, st_crs(mtrvlg))
umkm <- st_intersection(umkm, mtrvlg) %>% 
  select(osm_id, name, amenity, short_name, shop, addr.street, addr.subdistrict)

umkm_kc <- st_join(umkm, mtrvlg)
umkm_count <- umkm_kc %>%
  count(WADMKC, name = "n")


amenity_only <- d_amenity$osm_points %>%
  dplyr::filter(!is.na(amenity)) %>%
  dplyr::filter(amenity %in% c("restaurant","cafe","fast_food"))


amenity_only <- st_intersection(amenity_only, mtrvlg)

# make the coordinate system identical
amenity_only <- st_transform(amenity_only, st_crs(mtrvlg))

# plot the amenity_only data into mataram map
ggplot() +
  annotation_map_tile(type = "osm", zoom = 15) +
  # Base map: kecamatan polygons
  geom_sf(
    data = mtrvlg,
    aes(fill = WADMKC),
    alpha = 0.7,
    color = "gray", linewidth = 0.3
  ) +
  geom_sf(
    data = amenity_only,
    color = "black",   # warm orange
    size = 2, alpha = 0.7
  ) +
  scale_fill_viridis_d(option = "mako") +
  theme_minimal(base_size = 9) +
  theme(
    plot.background  = element_rect(fill = "white", color = NA),
    panel.grid       = element_blank(),
    strip.text       = element_text(face = "bold", size = 9),
    legend.position  = "bottom",
    legend.title     = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
  ) 

