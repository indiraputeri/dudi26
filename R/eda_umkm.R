library(ggspatial)
library(spdep)
library(sf)
library(sp)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(lavaan)
library(gtsummary)
library(tidygeocoder)
library(leaflet)
library(mice)
library(GGally)
conflicted::conflict_prefer("select", "dplyr")
conflicted::conflict_prefer("filter", "dplyr")
conflicted::conflict_prefer("recode", "dplyr")

load("data/lop_vlg.RData")
load("data/lop_kc.RData")
set.seed(0803)
# EDA data UMKM Kota Mataram
# Data berupa: 1. Data UMKM dari Dinas Koperasi, 2. Data survei, dan
#    3. Data administratif kota Mataram

# DATA UMKM (DINAS)-------------------------------------------------------------
## Membaca data UMKM dari dinas koperasi----------------------------------------
umkm_dat <- read_excel(
  "data/umkm.xlsx",
  sheet = "DATA UMKM TAHUN 2024",
  skip = 2
) |>
  clean_names()

umkm_dat <- umkm_dat %>% 
  select(-no, -nib, 
         -email, -nomor_telp,
         -nama_pemilik_usaha) %>% 
  rename(investasi = jumlah_investasi_rp,
         luas = luas_tanah_mm
  ) %>% 
  mutate(
    kecamatan = recode(
      kecamatan,
      "Selaprang" = "Selaparang"
    ),
    luas = parse_number(
      luas,
      locale = locale(decimal_mark = ",",
                      grouping_mark = ".")
    )
    )

## Checking the missing value in 'luas'
umkm_dat %>%
  summarise(
    n = n(),
    missing_luas = sum(is.na(luas)),
    percent_missing = mean(is.na(luas)) * 100
  )

# output:
# # A tibble: 1 × 3
# n missing_luas percent_missing
# <int>        <int>           <dbl>
#   1  4185            1          0.0239

umkm_dat %>%
  filter(is.na(luas)) %>%
  select(nama_perusahaan, kecamatan, kelurahan, luas)

umkm_dat <- umkm_dat %>%
  filter(
    skala_usaha %in% c("Usaha Mikro", "Usaha Kecil")
  )

# output
# # A tibble: 1 × 4
# nama_perusahaan     kecamatan  kelurahan    luas
# <chr>               <chr>      <chr>       <dbl>
#   1 YAYUK SILPIANA SARI Selaparang Dasan Agung    NA

# Checking missing value across all variables
umkm_dat %>%
  summarise(
    across(
      everything(),
      ~sum(is.na(.))
    )
  )
# output:
# # A tibble: 1 × 11
# nama_perusahaan jenis_perusahaan risiko_proyek skala_usaha kecamatan kelurahan  kbli judul_kbli  luas investasi tki_org
# <int>            <int>         <int>       <int>     <int>     <int> <int>      <int> <int>     <int>   <int>
#   1               0                0             0           0         0         0     0          0     1         0       0

head(umkm_dat)

# A tibble: 6 × 11
# nama_perusahaan  jenis_perusahaan risiko_proyek skala_usaha kecamatan kelurahan kbli  judul_kbli luas  investasi
# <chr>            <chr>            <chr>         <chr>       <chr>     <chr>     <chr> <chr>      <chr>     <dbl>
#   1 ION CAFE         Perorangan       MT            Usaha Kecil Sekarbela Kekalik … 47729 Perdagang… 132      1   e9
# 2 KIOS SEDERHANA   Perorangan       R             Usaha Mikro Ampenan   Taman Sa… 47192 Perdagang… 100      2   e8
# 3 UD. BERSAMA      Perorangan       R             Usaha Mikro Sandubaya Bertais   47712 Perdagang… 120      4.89e8
# 4 UD RIZKI RAYA    Perorangan       R             Usaha Mikro Mataram   Pagutan … 47112 Perdagang… 70       1.5 e8
# 5 UD. SARI SAMUDR… Perorangan       R             Usaha Mikro Sandubaya Dasan Ce… 47215 Perdagang… 200      2.5 e8
# 6 UD. GETAP BERSA… Perorangan       R             Usaha Mikro Cakraneg… Sayang-s… 47112 Perdagang… 200      1   e8
# # ℹ 1 more variable: tki_org <dbl>

save(umkm_dat, file = "data/umkm_dat.RData")

# DATA ADMINISTRATIF------------------------------------------------------------
## Membaca data administratif kota Mataram--------------------------------------

mtrvlg <- lop_vlg %>% 
  subset(WADMKK == "Kota Mataram")

mtrvlg <- mtrvlg %>%
  mutate(
    kelurahan = sub("^Kelurahan\\s+", "", NAMOBJ)
  ) %>% 
  select(-NAMOBJ, -WADMKK) %>% 
  rename(kecamatan = WADMKC)

mtrvlg <- mtrvlg %>%
  mutate(
    kelurahan = recode(
      kelurahan,
      "Sayangsayang" = "Sayang-sayang"
    )
  )

mtrvlg <- mtrvlg %>%
  select(kelurahan, kecamatan, geometry)

## left joining the administrative data with umkm_dat by 'kelurahan'-------------
mtrvlg_umkm <- mtrvlg %>%
  left_join(
    umkm_dat %>% select(-kecamatan),
    by = "kelurahan"
  )

## extracting only the street name, store it on 'query_jalan' column------------
# to be able to conduct a geocode process (longlat)
umkm_dat <- umkm_dat %>%
  mutate(
    alamat_jalan = alamat_usaha %>%
      str_replace(
        regex("^\\s*Jl\\.?\\s*", ignore_case = TRUE),
        "Jalan "
      ) %>%
      str_replace(
        regex("\\s+No\\.?\\s*.*$", ignore_case = TRUE),
        ""
      ) %>%
      str_squish(),
    
    query_jalan = paste(
      alamat_jalan,
      "Mataram",
      "Indonesia",
      sep = ", "
    )
  )

umkm_geo <- umkm_dat %>%
  geocode(
    address = query_jalan,
    method = "osm",
    lat = latitude,
    long = longitude
  )

umkm_geo <- umkm_geo %>%
  mutate(id = row_number())

#filter only usaha kecil & mikro
umkm_geo2 <- umkm_geo %>%
  filter(
    skala_usaha %in% c("Usaha Mikro", "Usaha Kecil")
  )

# failed_geo <- umkm_geo %>%
#   filter(is.na(latitude) | is.na(longitude)) %>% 
#   select(-longitude, -latitude)
# 
# failed_geo <- failed_geo %>%
#   mutate(
#     alamat_jalan2 = alamat_usaha %>%
#       str_replace(
#         regex("^\\s*Jl\\.?\\s*", ignore_case = TRUE),
#         "Jalan "
#       ) %>%
#       str_replace(
#         regex("\\s+No\\.?\\s*.*$", ignore_case = TRUE),
#         ""
#       ) %>%
#       str_squish(),
#     
#     query_mtr = paste(
#       alamat_jalan2,
#       "Mataram",
#       "Indonesia",
#       sep = ", "
#     )
#   ) %>%
#   geocode(
#     address = query_mtr,
#     method = "osm",
#     lat = latitude,
#     long = longitude
#   )

# check the successful longlat extraction
umkm_geo %>%
  summarise(
    berhasil = sum(!is.na(latitude)),
    gagal = sum(is.na(latitude)),
    total = n()
  )

save(umkm_geo, file ="data/umkm_geo.RData")
save(umkm_geo2, file ="data/umkm_geo2.RData") 

# # A tibble: 1 × 3
# berhasil gagal total
# <int> <int> <int>
#   1     2686  1499  4185

## plotting umkm data point in the kota Mataram map-----------------------------

load("data/umkm_geo.RData")

umkm_sf <- umkm_geo2 %>%
  filter(
    !is.na(latitude),
    !is.na(longitude)
  ) %>%
  st_as_sf(
    coords = c("longitude", "latitude"),
    crs = 4326,
    remove = FALSE
  )


jmlh_umkm <- umkm_geo2 %>%
  count(kelurahan, name = "jumlah_umkm")

mtrvlg_umkm2 <- mtrvlg %>%
  left_join(
    jmlh_umkm,
    by = "kelurahan"
  )

mtrvlg_umkm2 <- mtrvlg_umkm2 %>%
  st_transform(4326)

skala_colors <- c(
  "Usaha Mikro" = "blue",
  "Usaha Kecil" = "green",
  "Usaha Menengah" = "orange",
  "Usaha Besar" = "red"
)

leaflet() %>%
  addTiles() %>%
  addPolygons(
  data = mtrvlg_umkm2,
  
  fillColor = ~colorNumeric(
    palette = "YlOrRd",
    domain = mtrvlg_umkm2$jumlah_umkm
  )(jumlah_umkm),
  
  fillOpacity = 0.6,
  color = "white",
  weight = 1,
  
  label = ~paste0(
    kelurahan,
    " | ",
    kecamatan,
    " | UMKM: ",
    jumlah_umkm
  ),
  
  popup = ~paste0(
    "<b>Kelurahan:</b> ", kelurahan, "<br>",
    "<b>Kecamatan:</b> ", kecamatan, "<br>",
    "<b>Jumlah UMKM:</b> ", jumlah_umkm
  )
) %>%
  
# UMKM points

addCircleMarkers(
  data = umkm_sf,
  
  radius = 2.5,
  stroke = FALSE,
  weight = 1,
  fillOpacity = 0.5,
  # color = "black",
  fillColor = ~skala_colors[skala_usaha], #still not working!
  
  label = ~judul_kbli,
  
  popup = ~paste0(
    "<b>", judul_kbli, "</b><br><br>",
    
    "<b>Jenis usaha:</b> ", jenis_perusahaan, "<br>",
    "<b>Skala usaha:</b> ", skala_usaha, "<br>",
    "<b>Kelurahan:</b> ", kelurahan, "<br>",
    "<b>Kecamatan:</b> ", kecamatan, "<br>",
    "<b>KBLI:</b> ", kbli, "<br>",
    "<b>Luas:</b> ", luas, " m²<br>",
    "<b>Investasi:</b> Rp ",
    format(investasi, big.mark = ".", scientific = FALSE),
    "<br>",
    "<b>TKI:</b> ", tki_org
  )
)

# DATA SURVEI-------------------------------------------------------------------
## load, read, and clean the survey data----------------------------------------

srv <- read_csv("data/dri.csv")

# Kolom kelurahan dari Google Form
kolom_kelurahan <- c(
  "Pilih kelurahan...5",
  "Pilih kelurahan...6",
  "Pilih kelurahan...7",
  "Pilih kelurahan...8",
  "Pilih kelurahan...9",
  "Pilih Kelurahan"
)

# Membuat satu kolom kelurahan
usrv <- srv %>%
  mutate(
    kelurahan = coalesce(!!!syms(kolom_kelurahan)),
    kode_umkm = sprintf("UMKM%03d", row_number())
  ) %>% 
  rename(
    kecamatan = `Lokasi usaha (kecamatan)`,
    jenis_usaha = `Jenis usaha`,
    online = `Apakah kegiatan usaha (penjualan/pembelian) juga dilakukan secara online?`,
    usia_usaha = `Usia Usaha`,
    tenaga_kerja = `Banyaknya tenaga kerja/karyawan`,
    omset = `Omset (penjualan) per bulan`,
    usia_pemilik = `Usia pemilik usaha`,
    pendidikan = `Pendidikan terakhir`,
    INF1 = `Koneksi internet di wilayah usaha saya stabil dan cukup cepat`,
    INF2 = `Saya memiliki perangkat (HP/Laptop) yang cukup untuk operasional`,
    INF3 = `Saya menggunakan aplikasi online/cloud (google drive, dll)`,
    
    SKL1 = `Saya atau karyawan memiliki kemampuan dasar menggunakan teknologi`,
    SKL2 = `Kami pernah mengikuti pelatihan digital dalam 1 tahun terakhir`,
    SKL3 = `Saya memahami pemasaran digital (media sosial, marketplace)`,
    
    BUS1 = `Pembukuan usaha sudah dilakukan secara digital`,
    BUS2 = `Pengelolaan stok dilakukan dengan bantuan teknologi`,
    BUS3 = `Data pelanggan disimpan secara rapi (misalnya Excel/WA)`,
    
    STR1 = `Saya memiliki rencana untuk mengembangkan usaha secara digital`,
    STR2 = `Saya menyisihkan anggaran untuk teknologi/digitalisasi`,
    
    SEC1 = `Saya menggunakan backup data usaha secara rutin`,
    SEC2 = `Saya menggunakan perlindungan dasar (password/antivirus)`,
    
    DAT1 = `Saya menggunakan data untuk mengambil keputusan usaha`,
    DAT2 = `Saya pernah mencoba cara baru berbasis digital untuk usaha`,
    
    ECO1 = `Usaha saya memiliki akun media sosial aktif untuk promosi`,
    ECO2 = `Saya menjual produk melalui marketplace/online`,
    ECO3 = `Sebagian penjualan berasal dari online`,
    
    HAL1 = `Saya memahami pentingnya sertifikasi halal bagi usaha`,
    HAL2 = `Sertifikasi halal meningkatkan kepercayaan konsumen`,
    HAL3 = `Saya mengetahui prosedur pengurusan sertifikasi halal`,
    HAL4 = `Biaya sertifikasi halal masih menjadi kendala usaha saya`,
    HAL5 = `Pemerintah memberikan sosialisasi yang cukup terkait sertifikasi halal`,
    HAL6 = `Sertifikasi halal meningkatkan daya saing produk`,
    HAL7 = `Saya memiliki kesiapan administrasi untuk sertifikasi halal`,
    HAL8 = `Saya membutuhkan pendampingan dalam proses sertifikasi halal`,
    HAL9 = `Sertifikasi halal membantu memperluas pasar usaha`,
    HAL10 = `Lokasi usaha mempengaruhi akses terhadap layanan sertifikasi halal`
  ) %>% 
  select(-starts_with("Pilih kelurahan"),
         -Timestamp,
         -`Email Address`,
         -`Nama usaha (UMKM)`,
         -`Surveyor (referral)`,
         -`foto responden/lokasi usaha responden`,
         -`Nama pemilik usaha (dapat berupa inisial/anonim)`
  )

usrv <- usrv %>%
  select(
    kode_umkm,
    kecamatan,
    kelurahan,
    
    jenis_usaha,
    online,
    usia_usaha,
    tenaga_kerja,
    omset,
    usia_pemilik,
    pendidikan,
    
    starts_with("INF"),
    starts_with("SKL"),
    starts_with("BUS"),
    starts_with("STR"),
    starts_with("SEC"),
    starts_with("DAT"),
    starts_with("ECO"),
    starts_with("HAL")
  )

head(usrv)


dri_score <- usrv %>%
  mutate(
    Skill = rowMeans(
      dplyr::select(., SKL1, SKL2, SKL3),
      na.rm = TRUE
    ),
    Business = rowMeans(
      dplyr::select(., BUS1, BUS2, BUS3),
      na.rm = TRUE
    ),
    Strategy = rowMeans(
      dplyr::select(., STR1, STR2),
      na.rm = TRUE
    ),
    Security = rowMeans(
      dplyr::select(., SEC1, SEC2),
      na.rm = TRUE
    ),
    Data = rowMeans(
      dplyr::select(., DAT1, DAT2),
      na.rm = TRUE
    ),
    Ecomm = rowMeans(
      dplyr::select(., ECO1, ECO2, ECO3),
      na.rm = TRUE
    ),
    Infr = rowMeans(
      dplyr::select(., INF1, INF2, INF3),
      na.rm = TRUE
    )
  )


# data standardizing

# dri_score <- dri_score %>%
#   mutate(
#     Skill_z = as.numeric(scale(Skill)),
#     Business_z = as.numeric(scale(Business)),
#     Strategy_z = as.numeric(scale(Strategy)),
#     Security_z = as.numeric(scale(Security)),
#     Data_z = as.numeric(scale(Data))
#   )

# compute dri score
dri_score <- dri_score %>%
  mutate(
    DRI = rowMeans(
      dplyr::select(
        .,
        Skill,
        Business,
        Strategy,
        Security,
        Data
      ),
      na.rm = TRUE
    )
  )


save(dri_score, file ="data/dri_score.RData")


# cfa_model <- '
#   Capability =~ INF3 + SKL1 + SKL2 + SKL3 +
#                 BUS1 + BUS2 + BUS3 +
#                 STR1 + STR2 +
#                 SEC1 + SEC2 +
#                 DAT1 + DAT2
# 
#   Market =~ ECO1 + ECO2 + ECO3
# 
#   Infrastructure =~ INF1 + INF2
# '
# 
# fit_cfa <- cfa(
#   cfa_model,
#   data = dri_data,
#   ordered = names(dri_data),
#   estimator = "WLSMV"
# )
# 
# summary(
#   fit_cfa,
#   fit.measures = TRUE,
#   standardized = TRUE
# )
# 
# fitMeasures(
#   fit_cfa,
#   c(
#     "chisq",
#     "df",
#     "cfi",
#     "tli",
#     "rmsea",
#     "srmr"
#   )
# )
# 

# Agregasi DRI per kelurahan
dri_kelurahan <- dri_score %>%
  group_by(kelurahan) %>%
  summarise(
    DRI = median(DRI, na.rm = TRUE),
    n_umkm = n(),
    .groups = "drop"
  )

dri_kelurahan <- dri_kelurahan %>%
  mutate(
    kelurahan = recode(
      kelurahan,
      "Cakra Barat"     = "Cakranegara Barat",
      "Cakra Selatan"   = "Cakranegara Selatan",
      "Kebon Sari"      = "Kebun Sari",
      "Pejarakan Karya" = "Pajarakan Karya",
      "Sayang-Sayang"   = "Sayang-sayang"
    )
  )

# Gabungkan dengan shapefile kelurahan
mtrvlg_dri <- mtrvlg %>%
  left_join(
    dri_kelurahan,
    by = "kelurahan"
  ) 

# Plot DRI per kelurahan
ggplot(mtrvlg_dri) +
  geom_sf(aes(fill = DRI), color = "white", linewidth = 0.2) +
  scale_fill_viridis_c(
    option = "viridis",
    na.value = "grey90"
  ) +
  labs(
    title = "Digital Readiness Index (DRI) per Kelurahan",
    fill = "DRI"
  ) +
  theme_minimal()

# Preparing dataset for ANALYSIS-------
## Aggregating the covariates---------

# covariate 1: business scale

# from survey data: usrv & dri_score
ecomm_area <- dri_score %>%
  group_by(kelurahan) %>%
  summarise(
    mean_ecomm = mean(Ecomm, na.rm = TRUE),
    mean_infr  = mean(Infr, na.rm = TRUE),
    .groups = "drop"
  )

online_area <- usrv %>%
  group_by(kelurahan) %>%
  summarise(
    prop_online = mean(online == "ya", na.rm = TRUE),
    .groups = "drop"
  )

# from data dinas: umkm_dat
scale_area <- umkm_dat %>%
  filter(!is.na(skala_usaha)) %>%
  group_by(kelurahan) %>%
  summarise(
    n_scale = n(),
    
    prop_micro =
      mean(skala_usaha == "Usaha Mikro"),
    
    prop_small =
      mean(skala_usaha == "Usaha Kecil"),
    
    prop_medium =
      mean(skala_usaha == "Usaha Menengah"),
    prop_large =
      mean(skala_usaha == "Usaha Besar")
  )

# scaling to juta IDR
umkm_dat <- umkm_dat %>%
  mutate(
    investasi_jt = investasi / 1e6
  )

# grouping jenis_usaha (ju) into 3 groups, instead of more than 3
umkm_dat <- umkm_dat %>%
  mutate(
    klp_ju = case_when(
      jenis_perusahaan == "Perorangan" ~ "Perorangan",
      jenis_perusahaan %in% c("CV", "PT") ~ "CV_PT",
      TRUE ~ "Lainnya"
    )
  )

# covariate 2: investment
investment_area <- umkm_dat %>%
  group_by(kelurahan) %>%
  summarise(
    n_investment = sum(!is.na(investasi_jt)),
    
    mean_investment =
      mean(investasi_jt, na.rm = TRUE),
    
    median_investment = 
      median(investasi_jt, na.rm = TRUE),
    
    total_investment =
      sum(investasi_jt, na.rm = TRUE)
  ) %>%
  mutate(
    log_inv_per_umkm =
      log1p(mean_investment)
  )

# covariate 3: labour
tki_area <- umkm_dat %>%
  group_by(kelurahan) %>%
  summarise(
    n_tk = sum(!is.na(tki_org)),
    
    total_tk =
      sum(tki_org, na.rm = TRUE),
    
    mean_tk =
      mean(tki_org, na.rm = TRUE),
    median_tk = 
      median(tki_org, na.rm = TRUE)
  )

# covariate 4: bussiness type
badan_area <- umkm_dat %>%
  filter(!is.na(klp_ju)) %>%
  group_by(kelurahan) %>%
  summarise(
    n_badan = n(),
    
    prop_perorangan =
      mean(klp_ju == "Perorangan"),
    
    prop_cv_pt =
      mean(klp_ju == "CV_PT"),
    
    prop_lainnya =
      mean(klp_ju == "Lainnya"),
    
    .groups = "drop"
  )

# joining all the covariates with dri_score and mataram sf

model_vlg <- mtrvlg_dri %>%
  left_join(scale_area, by = "kelurahan") %>%
  left_join(investment_area, by = "kelurahan") %>%
  left_join(tki_area, by = "kelurahan") %>%
  left_join(badan_area, by = "kelurahan") %>% 
  left_join(ecomm_area, by = "kelurahan") %>% 
  left_join(online_area, by = "kelurahan")


## data imputation----

impute_dat <- model_vlg %>%
  st_drop_geometry()

imp_vars <- impute_dat %>%
  dplyr::select(
    kelurahan,
    prop_micro,
    prop_small,
    prop_medium,
    prop_large,
    median_tk,
    prop_perorangan,
    prop_cv_pt,
    prop_lainnya,
    mean_ecomm,
    mean_infr,
    prop_online
  )

meth <- make.method(imp_vars)
pred <- make.predictorMatrix(imp_vars)

meth["mean_ecomm"] <- "pmm"
meth["mean_infr"] <- "pmm"
meth["prop_online"] <- "pmm"

pred[, "kelurahan"] <- 0
pred["kelurahan", ] <- 0

imp <- mice(
  imp_vars,
  m = 20,
  method = meth,
  predictorMatrix = pred,
  maxit = 20,
  seed = 1234,
  printFlag = TRUE
)

completed_1 <- complete(imp, 1)

car_data_1 <- model_vlg %>%
  st_drop_geometry() %>%
  dplyr::select(
    kelurahan,
    DRI,
    n_umkm,
    prop_small,
    prop_medium,
    prop_large,
    median_investment,
    mean_investment,
    median_tk,
    prop_cv_pt,
    prop_perorangan
  ) %>%
  left_join(
    completed_1 %>%
      dplyr::select(
        kelurahan,
        mean_ecomm,
        mean_infr,
        prop_online
      ),
    by = "kelurahan"
  )

car_data <- car_data_1 %>%
  st_drop_geometry() %>%
  mutate(
    dri_std = as.numeric(scale(DRI)),
    prop_small_std = as.numeric(scale(prop_small)),
    prop_medium_std = as.numeric(scale(prop_medium)),
    prop_large_std = as.numeric(scale(prop_large)),
    med_inv_std = as.numeric(scale(median_investment)),
    mean_inv_std = as.numeric(scale(mean_investment)),
    med_tk_std = as.numeric(scale(median_tk)),
    prop_cv_pt_std = as.numeric(scale(prop_cv_pt)),
    prop_perorangan_std = as.numeric(scale(prop_perorangan)),
    mean_ecomm_std = as.numeric(scale(mean_ecomm)),
    mean_infr_std = as.numeric(scale(mean_infr)),
    prop_online_std  = as.numeric(scale(prop_online))
  )


## plot scatterplots matrices----------
# prepare data for plotting
gg_data <- model_vlg %>%
  st_drop_geometry() %>%
  select(
    DRI,
    mean_ecomm,
    mean_infr,
    prop_online,
    median_investment,
    median_tk
  )

# plotting
ggpairs(
  gg_data,
  upper = list(continuous = wrap("cor", size = 4)),
  lower = list(
    continuous = wrap("points", alpha = 0.6, size = 1.5)
  ),
  diag = list(
    continuous = wrap("densityDiag", alpha = 0.5)
  )
)

# THE MODELS------------
## OLS model-----

model_vlg %>%
  st_drop_geometry() %>%
  select(
    DRI,
    mean_ecomm,
    mean_infr,
    prop_online,
    median_investment,
    median_tk
  ) %>%
  cor(use = "pairwise.complete.obs")

ols_model <- lm(
  DRI ~
    mean_ecomm +
    mean_infr +
    prop_online +
    log(mean_investment) +
    median_tk,
  data = model_vlg
)

summary(ols_model)

## GLM model
model.glm <- S.glm(formula  = DRI ~
                     mean_ecomm +
                     mean_infr +
                     prop_online +
                     log(mean_investment) +
                     median_tk +
                     prop_perorangan_std,
                   data     = car_data,
                   family   = "gaussian",
                   n.chain  = 3,
                   n.cores  = 3,
                   burnin   = 10000,
                   n.sample = 20000,
                   thin     = 1)
model.glm

## Spatial model (CAR) ----
### Neighbourhood of Kota Mataram----

nb <- poly2nb(model_vlg)
summary(nb)

# Neighbour list object:
#   Number of regions: 50 
# Number of nonzero links: 270 
# Percentage nonzero weights: 10.8 
# Average number of links: 5.4 
# Link number distribution:
#   
#   2  3  4  5  6  7  8  9 
# 1  3 12 10 12  8  3  1 
# 1 least connected region:
#   28 with 2 links
# 1 most connected region:
#   10 with 9 links

which(card(nb) == 0)
# integer(0)

W <- nb2mat(
  nb,
  style = "W",
  zero.policy = TRUE
)

new_W            <- W + t(W) 
new_W[new_W > 0] <- 1

dim(W)
isSymmetric(W)

# memastikan jumlah baris sama dengan W
nrow(car_data)
dim(W)

# modelling with S.CARleroux
model_car <- S.CARleroux(
  formula = DRI ~
    mean_ecomm +
    mean_infr +
    prop_online +
    log(mean_investment) +
    median_tk +
    prop_perorangan_std,
  family = "gaussian",
  data = car_data,
  W = new_W,
  burnin = 20000,
  n.sample = 100000,
  thin = 10,
  verbose = TRUE
)

model_car
summary(model_car)

# get the phi samples
phi_samples <- model_car$samples$phi

phi_summary <- data.frame(
  index = 1:ncol(phi_samples),
  mean = apply(phi_samples, 2, mean),
  lower = apply(phi_samples, 2, quantile, probs = 0.025),
  upper = apply(phi_samples, 2, quantile, probs = 0.975)
)

# Posterior mean for each kelurahan
phi_mean <- colMeans(phi_samples)

# Create phi data using the same order as car_data
phi_data <- data.frame(
  kelurahan = car_data$kelurahan,
  phi = phi_mean
)

# plot phi values
# Join to Kota Mataram polygons
mtrvlg_phi <- mtrvlg %>%
  left_join(phi_data, by = "kelurahan")

# adjust color palette for phi
lim <- max(abs(mtrvlg_phi$phi), na.rm = TRUE)
pal_phi <- colorNumeric(
  palette = "RdBu",
  domain = c(-lim, lim),
  reverse = TRUE
)

# plotting
leaflet(mtrvlg_phi) %>%
  addTiles() %>%
  addPolygons(
    fillColor = ~pal_phi(phi),
    fillOpacity = 0.95,
    color = "white",
    weight = 1,
    opacity = 1,
    label = ~paste0(
      kelurahan,
      ": ",
      round(phi, 3)
    ),
    highlightOptions = highlightOptions(
      weight = 2,
      color = "black",
      bringToFront = TRUE
    )
  ) %>%
  addLegend(
    pal = pal_phi,
    values = ~phi,
    title = expression(phi[i]),
    position = "bottomright",
    opacity = 1
  )


lw <- mat2listw(
  new_W,
  style = "W",
  zero.policy = TRUE
)

# Kelurahan dengan DRI yang tersedia
idx <- !is.na(car_data$DRI)

# Data dan spatial weights harus menggunakan kelurahan yang sama
DRI_complete <- car_data$DRI[idx]

W_complete <- new_W[idx, idx]

# Spatial weights matrix
lw_complete <- mat2listw(
  W_complete,
  style = "W",
  zero.policy = TRUE
)

# check the Moran's I
moran_dri <- moran.test(
  DRI_complete,
  lw_complete,
  zero.policy = TRUE
)

moran_dri

## HALAL aspect analysis----

halal_score <- dri_score %>%
  mutate(
    
    # Pemahaman dan persepsi manfaat sertifikasi halal
    HAL_awareness = rowMeans(
      select(
        .,
        HAL1,
        HAL2,
        HAL6,
        HAL9
      ),
      na.rm = TRUE
    ),
    
    # Pengetahuan prosedur dan kesiapan administratif
    HAL_readiness = rowMeans(
      select(
        .,
        HAL3,
        HAL7
      ),
      na.rm = TRUE
    ),
    
    # Hambatan dalam proses sertifikasi
    HAL_barrier = rowMeans(
      select(
        .,
        HAL4,
        HAL8
      ),
      na.rm = TRUE
    ),
    
    # Persepsi terhadap dukungan pemerintah
    HAL_support = HAL5,
    
    # Persepsi akses terhadap layanan sertifikasi
    HAL_access = HAL10
  )

halal_score %>%
  summarise(
    n = n(),
    awareness = sum(!is.na(HAL_awareness)),
    readiness = sum(!is.na(HAL_readiness)),
    barrier = sum(!is.na(HAL_barrier)),
    support = sum(!is.na(HAL_support)),
    access = sum(!is.na(HAL_access))
  )

halal_score %>%
  summarise(
    across(
      starts_with("HAL"),
      ~ sum(is.na(.))
    )
  )

halal_score %>%
  select(
    HAL1:HAL10
  ) %>%
  summarise(
    across(
      everything(),
      list(
        mean = ~ mean(.x, na.rm = TRUE),
        median = ~ median(.x, na.rm = TRUE),
        sd = ~ sd(.x, na.rm = TRUE)
      )
    )
  )

save(halal_score, file = "data/halal_score.RData")

psych::polychoric(
  halal_score %>%
    select(HAL1:HAL10)
) -> poly_halal

round(
  poly_halal$rho,
  3
)

psych::polychoric(
  halal_score %>%
    select(
      HAL1,
      HAL2,
      HAL6,
      HAL9
    )
)$rho

cor_df <- as.data.frame(poly_halal$rho) %>%
  mutate(item1 = rownames(.)) %>%
  pivot_longer(
    cols = -item1,
    names_to = "item2",
    values_to = "rho"
  )

save(cor_df, file="data/cor_df.RData")

psych::polychoric(
  halal_score %>%
    select(
      HAL3,
      HAL7
    )
)$rho

# Reliability test

psych::alpha(
  halal_score %>%
    select(
      HAL1,
      HAL2,
      HAL6,
      HAL9
    )
)

psych::alpha(
  halal_score %>%
    select(
      HAL3,
      HAL7
    )
)

## lavaan + WLSMV

model_halal <- '
  Awareness =~ HAL1 + HAL2 + HAL6 + HAL9
  Readiness =~ HAL3 + HAL7
  Barrier   =~ HAL4 + HAL8
'

fit_halal <- lavaan::cfa(
  model_halal,
  data = halal_score,
  ordered = paste0("HAL", 1:10),
  estimator = "WLSMV"
)
save(fit_halal,file = "data/fit_halal.RData")

summary(
  fit_halal,
  fit.measures = TRUE,
  standardized = TRUE
)

## observed loading
standardizedSolution(
  fit_halal
) %>%
  filter(op == "=~") %>%
  select(
    lhs,
    rhs,
    est.std,
    pvalue,
    ci.lower,
    ci.upper
  )

parameterEstimates(
  fit_halal,
  standardized = TRUE
) %>%
  filter(
    op == "~~"
  )

## analysis of Halal score and DRI
cor(
  halal_score %>%
    select(
      DRI,
      HAL_awareness,
      HAL_readiness,
      HAL_barrier,
      HAL_support,
      HAL_access
    ),
  use = "pairwise.complete.obs"
)

model_readiness <- lm(
  HAL_readiness ~ DRI,
  data = halal_score
)

summary(model_readiness)

## add other predictors/covariates
model_readiness <- lm(
  HAL_readiness ~
    DRI +
    prop_online +
    usia_usaha +
    tenaga_kerja +
    omset +
    pendidikan,
  data = halal_score
)

summary(model_readiness)