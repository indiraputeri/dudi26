# ============================================================
# UMKM Kota Mataram - Interactive Spatial Dashboard
# ============================================================

library(shiny)
library(bslib)
library(sf)
library(dplyr)
library(leaflet)
library(plotly)
library(ggplot2)
library(scales)

# ------------------------------------------------------------
# 1. LOAD DATA
# ------------------------------------------------------------

# Replace these with your actual data-loading code
#
# load("data/mtrvlg.RData")
# load("data/car_data2.RData")
# load("data/umkm_dat.RData")

# Expected:
# mtrvlg    : sf object containing 50 kelurahan polygons
# car_data2 : sf/data.frame containing spatial indicators
# umkm_dat  : individual UMKM data, optional


# ------------------------------------------------------------
# 2. PREPARE SPATIAL DATA
# ------------------------------------------------------------

# Make sure the key variable has the same name
# in both datasets.

# Example:
# mtrvlg <- mtrvlg %>%
#   rename(kelurahan = NAMOBJ)

# car_data2 <- car_data2 %>%
#   mutate(kelurahan = as.character(kelurahan))


# Join analytical indicators to polygon data

map_data <- mtrvlg %>%
  left_join(
    car_data2 %>%
      st_drop_geometry(),
    by = "kelurahan"
  )


# ------------------------------------------------------------
# 3. INDICATORS AVAILABLE IN THE DASHBOARD
# ------------------------------------------------------------

indicator_labels <- c(
  mean_ecomm =
    "Aktivitas digital / e-commerce",
  
  mean_infr =
    "Infrastruktur digital",
  
  mean_halreadiness =
    "Kesiapan sertifikasi halal",
  
  mean_halbarrier =
    "Hambatan sertifikasi halal",
  
  dri =
    "Digital Readiness Index"
)


# ------------------------------------------------------------
# 4. UI
# ------------------------------------------------------------

ui <- page_navbar(
  
  title = "UMKM Kota Mataram",
  
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly"
  ),
  
  # ----------------------------------------------------------
  # TAB 1: OVERVIEW
  # ----------------------------------------------------------
  
  nav_panel(
    "Ringkasan",
    
    layout_column_wrap(
      width = 1/4,
      
      value_box(
        title = "Total UMKM",
        value = textOutput("total_umkm"),
        showcase = icon("store")
      ),
      
      value_box(
        title = "Kelurahan",
        value = textOutput("total_kelurahan"),
        showcase = icon("map")
      ),
      
      value_box(
        title = "UMKM Mikro",
        value = textOutput("total_mikro"),
        showcase = icon("shop")
      ),
      
      value_box(
        title = "UMKM Geocoded",
        value = textOutput("total_geocoded"),
        showcase = icon("location-dot")
      )
    ),
    
    br(),
    
    layout_columns(
      
      bslib::card(
        card_header("Distribusi UMKM berdasarkan skala usaha"),
        plotlyOutput("scale_plot", height = "400px")
      ),
      
      bslib::card(
        card_header("10 Kelompok Kegiatan Usaha Terbesar"),
        plotlyOutput("kbli_plot", height = "400px")
      ),
      
      col_widths = c(6, 6)
    )
  ),
  
  
  # ----------------------------------------------------------
  # TAB 2: MAP
  # ----------------------------------------------------------
  
  nav_panel(
    "Peta Interaktif",
    
    layout_sidebar(
      
      sidebar = sidebar(
        
        selectInput(
          "indicator",
          "Indikator",
          choices = indicator_labels,
          selected = "mean_ecomm"
        ),
        
        checkboxInput(
          "show_points",
          "Tampilkan lokasi UMKM",
          value = FALSE
        ),
        
        hr(),
        
        strong("Filter skala usaha"),
        
        checkboxGroupInput(
          "scale_filter",
          NULL,
          choices = c(
            "Mikro",
            "Kecil",
            "Menengah",
            "Besar"
          ),
          selected = c(
            "Mikro",
            "Kecil",
            "Menengah",
            "Besar"
          )
        )
      ),
      
      bslib::card(
        full_screen = TRUE,
        leafletOutput(
          "map",
          height = "750px"
        )
      )
    )
  ),
  
  
  # ----------------------------------------------------------
  # TAB 3: PROFIL KELURAHAN
  # ----------------------------------------------------------
  
  nav_panel(
    "Profil Kelurahan",
    
    layout_sidebar(
      
      sidebar = sidebar(
        
        selectInput(
          "indicator",
          "Indikator",
          choices = c(
            "Aktivitas digital / e-commerce" = "mean_ecomm",
            "Infrastruktur digital" = "mean_infr",
            "Kesiapan sertifikasi halal" = "mean_halreadiness",
            "Hambatan sertifikasi halal" = "mean_halbarrier",
            "Digital Readiness Index" = "dri"
          ),
          selected = "mean_ecomm"
        )
      ),
      
      layout_columns(
        
        value_box(
          title = "Jumlah UMKM",
          value = textOutput("profile_total"),
          showcase = icon("store")
        ),
        
        value_box(
          title = "Digital",
          value = textOutput("profile_ecomm"),
          showcase = icon("laptop")
        ),
        
        value_box(
          title = "Infrastruktur",
          value = textOutput("profile_infr"),
          showcase = icon("tower-cell")
        ),
        
        value_box(
          title = "Halal Readiness",
          value = textOutput("profile_halal"),
          showcase = icon("certificate")
        ),
        
        col_widths = c(3, 3, 3, 3)
      ),
      
      br(),
      
      bslib::card(
        card_header("Profil indikator"),
        plotlyOutput(
          "profile_plot",
          height = "450px"
        )
      )
    )
  )
)


# ------------------------------------------------------------
# 5. SERVER
# ------------------------------------------------------------

server <- function(input, output, session) {
  
  # ----------------------------------------------------------
  # SUMMARY
  # ----------------------------------------------------------
  
  output$total_umkm <- renderText({
    format(
      nrow(umkm_dat),
      big.mark = "."
    )
  })
  
  
  output$total_kelurahan <- renderText({
    nrow(mtrvlg)
  })
  
  
  output$total_mikro <- renderText({
    
    umkm_dat %>%
      filter(skala_usaha == "Mikro") %>%
      nrow() %>%
      format(big.mark = ".")
  })
  
  
  # If you have a geocoded indicator
  output$total_geocoded <- renderText({
    
    if ("longitude" %in% names(umkm_dat)) {
      
      sum(
        !is.na(umkm_dat$longitude)
      ) %>%
        format(big.mark = ".")
      
    } else {
      
      "–"
      
    }
  })
  
  
  # ----------------------------------------------------------
  # SCALE PLOT
  # ----------------------------------------------------------
  
  output$scale_plot <- renderPlotly({
    
    scale_dat <- umkm_dat %>%
      count(skala_usaha) %>%
      mutate(
        percentage = n / sum(n)
      )
    
    p <- ggplot(
      scale_dat,
      aes(
        x = skala_usaha,
        y = n,
        text = paste0(
          "Skala: ", skala_usaha,
          "<br>Jumlah: ", comma(n),
          "<br>Persentase: ",
          percent(percentage, accuracy = 0.1)
        )
      )
    ) +
      
      geom_col() +
      
      labs(
        x = NULL,
        y = "Jumlah UMKM"
      ) +
      
      theme_minimal(base_size = 13)
    
    ggplotly(
      p,
      tooltip = "text"
    )
  })
  
  
  # ----------------------------------------------------------
  # KBLI PLOT
  # ----------------------------------------------------------
  
  output$kbli_plot <- renderPlotly({
    
    kelompok_kbli <- umkm_dat %>%
      count(judul_kbli, sort = TRUE) %>%
      slice_head(n = 10)
    
    p <- ggplot(
      kelompok_kbli,
      aes(
        x = reorder(judul_kbli, n),
        y = n,
        text = paste0(
          judul_kbli,
          "<br>Jumlah: ",
          comma(n)
        )
      )
    ) +
      
      geom_col() +
      
      coord_flip() +
      
      labs(
        x = NULL,
        y = "Jumlah UMKM"
      ) +
      
      theme_minimal(base_size = 12)
    
    ggplotly(
      p,
      tooltip = "text"
    )
  })
  
  
  # ----------------------------------------------------------
  # KELURAHAN SELECTOR
  # ----------------------------------------------------------
  
  observe({
    
    req(input$indicator)
    
    variable <- as.character(input$indicator)
    
    # Check that the selected variable exists
    req(variable %in% names(map_data))
    
    dat <- map_data
    
    # Extract values safely
    values <- dat[, variable, drop = TRUE]
    
    # Make sure it is numeric
    req(is.numeric(values))
    
    # Get indicator label safely
    indicator_name <- if (variable %in% names(indicator_labels)) {
      unname(indicator_labels[variable])
    } else {
      variable
    }
    
    pal <- leaflet::colorNumeric(
      palette = "viridis",
      domain = values,
      na.color = "#cccccc"
    )
    
    labels <- paste0(
      "<strong>Kelurahan:</strong> ",
      dat$kelurahan,
      "<br><strong>",
      indicator_name,
      ":</strong> ",
      round(values, 3)
    )
    
    leaflet::leafletProxy("map") %>%
      leaflet::clearShapes() %>%
      leaflet::clearControls() %>%
      leaflet::addPolygons(
        data = dat,
        layerId = ~kelurahan,
        fillColor = pal(values),
        fillOpacity = 0.75,
        color = "white",
        weight = 1,
        popup = labels,
        highlightOptions = leaflet::highlightOptions(
          weight = 3,
          bringToFront = TRUE
        )
      ) %>%
      leaflet::addLegend(
        pal = pal,
        values = values,
        title = indicator_name,
        position = "bottomright"
      )
    
  })
  
  
  # ----------------------------------------------------------
  # MAP
  # ----------------------------------------------------------
  
  output$map <- renderLeaflet({
    
    leaflet(
      options = leafletOptions(
        minZoom = 11,
        maxZoom = 18
      )
    ) %>%
      
      addTiles(
        urlTemplate = "https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",
        attribution = '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors'
      )  %>%
      
      setView(
        lng = 116.116,
        lat = -8.583,
        zoom = 12
      )
  })
  
  
  # ----------------------------------------------------------
  # UPDATE MAP WHEN INDICATOR CHANGES
  # ----------------------------------------------------------
  
  # observe({
  #   
  #   req(input$indicator)
  #   
  #   variable <- input$indicator
  #   
  #   dat <- map_data
  #   
  #   pal <- colorNumeric(
  #     palette = "viridis",
  #     domain = dat[[variable]],
  #     na.color = "#cccccc"
  #   )
  #   
  #   labels <- paste0(
  #     "<strong>Kelurahan: </strong>",
  #     dat$kelurahan,
  #     "<br>",
  #     "<strong>",
  #     indicator_labels[[variable]],
  #     ": </strong>",
  #     round(dat[[variable]], 3)
  #   )
  #   
  #   leafletProxy("map") %>%
  #     
  #     clearShapes() %>%
  #     
  #     clearControls() %>%
  #     
  #     addPolygons(
  #       data = dat,
  #       fillColor = ~pal(dat[[variable]]),
  #       fillOpacity = 0.75,
  #       color = "white",
  #       weight = 1,
  #       popup = labels,
  #       highlightOptions = highlightOptions(
  #         weight = 3,
  #         bringToFront = TRUE
  #       )
  #     ) %>%
  #     
  #     addLegend(
  #       pal = pal,
  #       values = dat[[variable]],
  #       title = indicator_labels[[variable]],
  #       position = "bottomright"
  #     )
  #   
  # })
  # 
  # 
  # # ----------------------------------------------------------
  # # OPTIONAL UMKM POINTS
  # # ----------------------------------------------------------
  # 
  # observe({
  #   
  #   req(input$show_points)
  #   
  #   if (!all(
  #     c("longitude", "latitude") %in%
  #     names(umkm_dat)
  #   )) {
  #     return()
  #   }
  #   
  #   points <- umkm_dat %>%
  #     filter(
  #       !is.na(longitude),
  #       !is.na(latitude),
  #       skala_usaha %in% input$scale_filter
  #     )
  #   
  #   leafletProxy("map") %>%
  #     
  #     clearMarkers() %>%
  #     
  #     addCircleMarkers(
  #       data = points,
  #       lng = ~longitude,
  #       lat = ~latitude,
  #       radius = 4,
  #       stroke = FALSE,
  #       fillOpacity = 0.65,
  #       popup = ~paste0(
  #         "<strong>",
  #         judul_kbli,
  #         "</strong>",
  #         "<br>Skala: ",
  #         skala_usaha
  #       )
  #     )
  # })
  # 
  # 
  # observeEvent(
  #   input$show_points,
  #   {
  #     if (!input$show_points) {
  #       leafletProxy("map") %>%
  #         clearMarkers()
  #     }
  #   }
  # )
  # 
  # 
  # # ----------------------------------------------------------
  # # CLICK KELURAHAN ON MAP
  # # ----------------------------------------------------------
  # 
  # observeEvent(
  #   input$map_shape_click,
  #   {
  #     
  #     click <- input$map_shape_click
  #     
  #     req(click$id)
  #     
  #     updateSelectInput(
  #       session,
  #       "selected_kelurahan",
  #       selected = click$id
  #     )
  #     
  #     updateNavbarPage(
  #       session,
  #       "navbar",
  #       selected = "Profil Kelurahan"
  #     )
  #   }
  # )
  # 
  # 
  # ----------------------------------------------------------
  # KELURAHAN PROFILE
  # ----------------------------------------------------------
  
  profile_data <- reactive({
    
    req(input$selected_kelurahan)
    
    map_data %>%
      filter(
        kelurahan ==
          input$selected_kelurahan
      )
  })
  
  
  output$profile_total <- renderText({
    
    # Replace this with the actual UMKM count
    # variable in car_data2 if available.
    
    if ("n_umkm" %in% names(profile_data())) {
      
      format(
        profile_data()$n_umkm,
        big.mark = "."
      )
      
    } else {
      
      "–"
    }
  })
  
  
  output$profile_ecomm <- renderText({
    
    round(
      profile_data()$mean_ecomm,
      2
    )
  })
  
  
  output$profile_infr <- renderText({
    
    round(
      profile_data()$mean_infr,
      2
    )
  })
  
  
  output$profile_halal <- renderText({
    
    round(
      profile_data()$mean_halreadiness,
      2
    )
  })
  
  
  # ----------------------------------------------------------
  # PROFILE RADAR-LIKE BAR CHART
  # ----------------------------------------------------------
  
  output$profile_plot <- renderPlotly({
    
    dat <- profile_data()
    
    profile <- tibble(
      indikator = c(
        "Aktivitas digital",
        "Infrastruktur",
        "Halal readiness",
        "Halal barriers"
      ),
      
      nilai = c(
        dat$mean_ecomm,
        dat$mean_infr,
        dat$mean_halreadiness,
        dat$mean_halbarrier
      )
    )
    
    p <- ggplot(
      profile,
      aes(
        x = indikator,
        y = nilai,
        text = paste0(
          indikator,
          "<br>Nilai: ",
          round(nilai, 3)
        )
      )
    ) +
      
      geom_col() +
      
      coord_flip() +
      
      labs(
        x = NULL,
        y = "Nilai"
      ) +
      
      theme_minimal(
        base_size = 13
      )
    
    ggplotly(
      p,
      tooltip = "text"
    )
  })
  
}


# ------------------------------------------------------------
# 6. RUN APP
# ------------------------------------------------------------

shinyApp(
  ui = ui,
  server = server
)

