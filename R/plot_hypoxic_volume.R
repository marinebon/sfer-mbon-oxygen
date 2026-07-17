plot_hypoxic_volume <- function(
    grid_df,
    threshold,
    title = NULL,
    opacity = 0.3) {
  if (!requireNamespace("plotly", quietly = TRUE)) {
    stop("Package 'plotly' is required for the hypoxic volume display.")
  }

  if (is.null(grid_df) || nrow(grid_df) == 0) {
    return(NULL)
  }

  active <- grid_df$volume_value[
    is.finite(grid_df$volume_value) & grid_df$volume_value > 0
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

  fill_limits <- hypoxic_volume_fill_limits(grid_df)
  if (is.null(title)) {
    title <- paste0(
      "Hypoxic volume (O\u2082 < ",
      threshold,
      " mg/L)"
    )
  }

  plotly::plot_ly(
    data = grid_df,
    type = "volume",
    x = ~longitude,
    y = ~latitude,
    z = ~depth_m,
    value = ~volume_value,
    isomin = fill_limits[1],
    isomax = fill_limits[2],
    opacity = opacity,
    opacityscale = hypoxic_volume_opacityscale(opacity),
    colorscale = hypoxic_volume_colorscale(),
    colorbar = list(
      title = "% cruises\nhypoxic",
      tickformat = ".0%",
      len = 0.75
    ),
    surface = list(count = 0),
    caps = list(
      x = list(show = FALSE),
      y = list(show = FALSE),
      z = list(show = FALSE)
    ),
    hovertemplate = paste0(
      "Lon: %{x:.4f}<br>",
      "Lat: %{y:.4f}<br>",
      "Depth: %{z:.0f} m<br>",
      "Hypoxic in %{customdata[0]} of %{customdata[1]} cruises (%{customdata[2]:.0f}%)",
      "<extra></extra>"
    ),
    customdata = ~cbind(n_hypoxic, n_cruises, pct_hypoxic)
  ) |>
    plotly::layout(
      title = list(text = title, x = 0.01, xanchor = "left"),
      margin = list(l = 0, r = 0, b = 0, t = 40),
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
