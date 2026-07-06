plot_oxygen_map <- function(field, observations, depth_m = NULL, tolerance = 1.5) {
  if (is.null(field) || nrow(field) == 0) {
    return(NULL)
  }

  if (is.null(depth_m)) {
    positive_depths <- field$depth_m[field$depth_m > 0 & !is.na(field$depth_m)]
    depth_m <- if (length(positive_depths) > 0) {
      min(positive_depths)
    } else {
      median(field$depth_m, na.rm = TRUE)
    }
  }

  field_slice <- field[
    abs(field$depth_m - depth_m) <= tolerance & !is.na(field$dissolved_oxygen),
    ,
    drop = FALSE
  ]

  obs <- as.data.frame(observations)
  obs_depth <- if ("depth" %in% names(obs)) obs$depth else obs$sea_water_pressure
  obs_slice <- obs[
    !is.na(obs$longitude) &
      !is.na(obs$latitude) &
      !is.na(obs_depth) &
      !is.na(obs$dissolved_oxygen) &
      abs(obs_depth - depth_m) <= tolerance,
    ,
    drop = FALSE
  ]
  obs_slice$depth_m_obs <- obs_depth[
    !is.na(obs$longitude) &
      !is.na(obs$latitude) &
      !is.na(obs_depth) &
      !is.na(obs$dissolved_oxygen) &
      abs(obs_depth - depth_m) <= tolerance
  ]

  if (nrow(field_slice) == 0) {
    return(NULL)
  }

  lons <- sort(unique(field_slice$longitude))
  lats <- sort(unique(field_slice$latitude), decreasing = TRUE)
  n_lon <- length(lons)
  n_lat <- length(lats)

  mat <- matrix(NA_real_, nrow = n_lat, ncol = n_lon)
  lon_idx <- match(round(field_slice$longitude, 9), round(lons, 9))
  lat_idx <- match(round(field_slice$latitude, 9), round(lats, 9))
  for (i in seq_len(nrow(field_slice))) {
    mat[lat_idx[i], lon_idx[i]] <- field_slice$dissolved_oxygen[i]
  }

  dx <- if (n_lon > 1) diff(lons)[1] / 2 else 0.01
  dy <- if (n_lat > 1) diff(rev(sort(lats)))[1] / 2 else 0.01

  r <- raster::raster(
    mat,
    xmn = min(lons) - dx,
    xmx = max(lons) + dx,
    ymn = min(lats) - dy,
    ymx = max(lats) + dy,
    crs = sp::CRS("+proj=longlat +datum=WGS84")
  )

  o2_range <- range(
    c(field_slice$dissolved_oxygen, obs_slice$dissolved_oxygen),
    na.rm = TRUE
  )
  pal <- leaflet::colorNumeric(
    viridisLite::viridis(256, option = "magma"),
    domain = o2_range,
    na.color = "transparent"
  )

  bbox <- raster::extent(r)
  depth_label <- round(depth_m, 1)

  leaflet::leaflet(width = "100%", height = 520) |>
    leaflet::addProviderTiles(
      leaflet::providers$Esri.OceanBasemap,
      group = "Ocean"
    ) |>
    leaflet::addProviderTiles(
      leaflet::providers$CartoDB.Positron,
      group = "Light"
    ) |>
    leaflet::addProviderTiles(
      leaflet::providers$OpenStreetMap,
      group = "OpenStreetMap"
    ) |>
    leaflet::addRasterImage(
      r,
      colors = pal,
      opacity = 0.75,
      group = "Interpolated field"
    ) |>
    leaflet::addCircleMarkers(
      data = obs_slice,
      lng = ~longitude,
      lat = ~latitude,
      radius = 6,
      fillColor = ~pal(dissolved_oxygen),
      fillOpacity = 0.95,
      color = "#1a1a1a",
      weight = 1,
      stroke = TRUE,
      label = ~sprintf(
        "O\u2082: %.2f mg/L\nDepth: %.1f m",
        dissolved_oxygen,
        depth_m_obs
      ),
      group = "CTD observations"
    ) |>
    leaflet::addLayersControl(
      baseGroups = c("Ocean", "Light", "OpenStreetMap"),
      overlayGroups = c("Interpolated field", "CTD observations"),
      options = leaflet::layersControlOptions(collapsed = FALSE)
    ) |>
    leaflet::addLegend(
      pal = pal,
      values = o2_range,
      title = "O\u2082 (mg/L)",
      position = "bottomright"
    ) |>
    leaflet::addScaleBar(position = "bottomleft") |>
    leaflet::addControl(
      html = sprintf(
        "<strong>Dissolved oxygen at %s m</strong><br>%d observations (±%s m)",
        depth_label,
        nrow(obs_slice),
        tolerance
      ),
      position = "topright"
    ) |>
    leaflet::fitBounds(
      lng1 = bbox@xmin,
      lat1 = bbox@ymin,
      lng2 = bbox@xmax,
      lat2 = bbox@ymax
    )
}
