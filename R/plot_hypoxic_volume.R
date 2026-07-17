DEFAULT_HYPOXIC_VOLUME_OPACITY <- 0.6
DEFAULT_HYPOXIC_VOLUME_SURFACE_COUNT <- 12L

hypoxic_volume_surface_opacity <- function(
    opacity = DEFAULT_HYPOXIC_VOLUME_OPACITY,
    surface_count = DEFAULT_HYPOXIC_VOLUME_SURFACE_COUNT) {
  surface_count <- max(as.integer(surface_count), 1L)
  1 - (1 - opacity)^(1 / surface_count)
}

plot_hypoxic_volume <- function(
    grid_df,
    threshold,
    title = NULL,
    opacity = DEFAULT_HYPOXIC_VOLUME_OPACITY,
    bathymetry = NULL,
    bath_root = here::here("data", "bathymetry")) {
  if (!requireNamespace("plotly", quietly = TRUE)) {
    stop("Package 'plotly' is required for the hypoxic volume display.")
  }

  if (is.null(grid_df) || nrow(grid_df) == 0) {
    return(NULL)
  }

  active <- grid_df$pct_hypoxic[
    is.finite(grid_df$pct_hypoxic) & grid_df$pct_hypoxic > 0
  ]
  if (length(active) == 0) {
    return(NULL)
  }

  if (!exists("hypoxic_volume_fill_limits", mode = "function")) {
    if (requireNamespace("here", quietly = TRUE)) {
      source(here::here("R/hypoxic_field_summary.R"), local = FALSE)
    } else {
      source("R/hypoxic_field_summary.R", local = FALSE)
    }
  }

  if (is.null(bathymetry) && !exists("sample_volume_bathymetry", mode = "function")) {
    if (requireNamespace("here", quietly = TRUE)) {
      source(here::here("R/volume_bathymetry.R"), local = FALSE)
    } else {
      source("R/volume_bathymetry.R", local = FALSE)
    }
  }

  fill_limits <- hypoxic_volume_log_limits(grid_df)
  volume_isomin <- fill_limits[1]
  volume_isomax <- fill_limits[2]
  color_ticks <- hypoxic_pct_log_breaks()
  plot_df <- prepare_hypoxic_volume_plot_data(grid_df)
  surface_count <- DEFAULT_HYPOXIC_VOLUME_SURFACE_COUNT
  surface_opacity <- hypoxic_volume_surface_opacity(opacity, surface_count)
  if (is.null(title)) {
    title <- paste0(
      "Hypoxic volume (O\u2082 < ",
      threshold,
      " mg/L)"
    )
  }

  lons <- sort(unique(grid_df$longitude))
  lats <- sort(unique(grid_df$latitude))
  lon_min <- min(lons)
  lon_max <- max(lons)
  lat_min <- min(lats)
  lat_max <- max(lats)
  if (is.null(bathymetry)) {
    bathymetry <- sample_volume_bathymetry(lons, lats, bath_root = bath_root)
  }

  fig <- plotly::plot_ly()

  coastline <- coastline_path_for_plotly(
    coastline_segments_for_bbox(lon_min, lon_max, lat_min, lat_max),
    z = 0
  )

  if (!is.null(bathymetry) && any(is.finite(bathymetry$depth))) {
    depth_mat <- bathymetry$depth
    depth_mat[!is.finite(depth_mat)] <- NA
    depth_range <- range(depth_mat, na.rm = TRUE)

    fig <- fig |>
      plotly::add_trace(
        type = "surface",
        x = bathymetry$lons,
        y = bathymetry$lats,
        z = depth_mat,
        colorscale = bathymetry_surface_colorscale(),
        cmin = depth_range[1],
        cmax = depth_range[2],
        showscale = FALSE,
        opacity = 0.95,
        connectgaps = FALSE,
        name = "Bathymetry",
        hoverinfo = "skip"
      )
  }

  fig <- fig |>
    plotly::add_trace(
      data = plot_df,
      type = "volume",
      x = ~longitude,
      y = ~latitude,
      z = ~depth_m,
      value = ~volume_value_log,
      isomin = volume_isomin,
      isomax = volume_isomax,
      cauto = FALSE,
      cmin = 0,
      cmax = log10(100),
      opacity = surface_opacity,
      opacityscale = hypoxic_volume_opacityscale_log(surface_opacity, volume_isomin),
      colorscale = hypoxic_volume_colorscale(),
      colorbar = list(
        title = "% cruises\nhypoxic",
        tickmode = "array",
        tickvals = color_ticks$tickvals,
        ticktext = color_ticks$ticktext,
        len = 0.75,
        y = 0.5,
        yanchor = "middle",
        outlinewidth = 0
      ),
      surface = list(count = surface_count),
      caps = list(
        x = list(show = FALSE),
        y = list(show = FALSE),
        z = list(show = FALSE)
      ),
      name = "Hypoxic volume",
      hoverinfo = "skip"
    )

  if (!is.null(coastline)) {
    fig <- fig |>
      plotly::add_trace(
        type = "scatter3d",
        mode = "lines",
        x = coastline$lon,
        y = coastline$lat,
        z = coastline$z,
        line = list(color = "#1f2933", width = 4),
        showlegend = FALSE,
        hoverinfo = "skip",
        name = "Coastline"
      )
  }

  fig |>
    plotly::layout(
      title = list(text = title, x = 0.01, xanchor = "left"),
      margin = list(l = 0, r = 0, b = 0, t = 40),
      hovermode = FALSE,
      scene = list(
        xaxis = list(title = "Longitude"),
        yaxis = list(title = "Latitude"),
        zaxis = list(title = "Depth (m)", autorange = "reversed"),
        aspectmode = "manual",
        aspectratio = list(x = 1, y = 1, z = 0.45),
        camera = list(eye = list(x = 1.6, y = 1.6, z = 0.9))
      )
    ) |>
    plotly::config(displaylogo = FALSE)
}
