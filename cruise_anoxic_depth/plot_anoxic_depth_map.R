plot_anoxic_depth_map <- function(field, observations, threshold = NULL) {
  if (is.null(field) || nrow(field) == 0) {
    return(NULL)
  }

  obs <- as.data.frame(observations)
  if (is.null(threshold)) {
    threshold <- default_anoxic_threshold(field, obs)
  }

  grid <- anoxic_depth_grid(field, threshold)
  grid <- anoxic_depth_grid_rectangles(grid)
  if (is.null(grid) || nrow(grid) == 0) {
    return(NULL)
  }

  finite_depths <- grid$anoxic_depth_m[is.finite(grid$anoxic_depth_m)]
  depth_range <- if (length(finite_depths) > 0) {
    range(finite_depths, na.rm = TRUE)
  } else {
    range(field$depth_m, na.rm = TRUE)
  }
  pal <- leaflet::colorNumeric(
    viridisLite::viridis(256),
    domain = depth_range,
    na.color = "transparent"
  )

  lon_min <- min(grid$longitude, na.rm = TRUE)
  lon_max <- max(grid$longitude, na.rm = TRUE)
  lat_min <- min(grid$latitude, na.rm = TRUE)
  lat_max <- max(grid$latitude, na.rm = TRUE)

  lons <- sort(unique(grid$longitude))
  lats <- sort(unique(grid$latitude))
  dx <- if (length(lons) > 1) diff(lons)[1] / 2 else 0.01
  dy <- if (length(lats) > 1) diff(lats)[1] / 2 else 0.01

  o2_values <- c(field$dissolved_oxygen, obs$dissolved_oxygen)
  o2_values <- o2_values[is.finite(o2_values)]
  o2_min <- min(o2_values, na.rm = TRUE)
  o2_max <- max(o2_values, na.rm = TRUE)
  slider_min <- max(0, floor(o2_min * 10) / 10)
  slider_max <- ceiling(o2_max * 10) / 10
  slider_step <- max(0.1, round((slider_max - slider_min) / 100, digits = 2))

  widget_id <- paste0("anoxic-depth-map-", sample.int(1e8, 1L))
  profile_payload <- build_profile_payload(field)
  profile_json <- jsonlite::toJSON(profile_payload, auto_unbox = FALSE)

  map <- leaflet::leaflet(
    width = "100%",
    height = 520,
    elementId = widget_id
  ) |>
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
    )

  has_obs <- nrow(obs) > 0 && "dissolved_oxygen" %in% names(obs)
  overlay_groups <- character()
  if (has_obs) {
    obs_depth <- if ("depth" %in% names(obs)) obs$depth else obs$sea_water_pressure
    obs$depth_m_obs <- obs_depth
    overlay_groups <- c(overlay_groups, "observations")
    map <- map |>
      leaflet::addCircleMarkers(
        data = obs,
        lng = ~longitude,
        lat = ~latitude,
        radius = 5,
        fillColor = "#ffffff",
        fillOpacity = 0.85,
        color = "#1a1a1a",
        weight = 1,
        stroke = TRUE,
        label = ~sprintf(
          "O\u2082: %.2f mg/L\nDepth: %.1f m",
          dissolved_oxygen,
          depth_m_obs
        ),
        group = "observations"
      )
  }

  map <- map |>
    leaflet::addLayersControl(
      baseGroups = c("Ocean", "Light", "OpenStreetMap"),
      overlayGroups = if (length(overlay_groups) > 0) overlay_groups else NULL,
      options = leaflet::layersControlOptions(collapsed = TRUE)
    ) |>
    leaflet::addLegend(
      pal = pal,
      values = depth_range,
      title = sprintf("Depth of O\u2082 < %.1f mg/L (m)", threshold),
      position = "bottomright"
    ) |>
    leaflet::addScaleBar(position = "bottomleft") |>
    leaflet::fitBounds(
      lng1 = lon_min,
      lat1 = lat_min,
      lng2 = lon_max,
      lat2 = lat_max
    ) |>
    htmlwidgets::onRender(sprintf(
      "
function(el, x) {
  var profiles = %s;
  var cells = Array.isArray(profiles.cells)
    ? profiles.cells
    : Object.values(profiles.cells || {});
  var threshold = %s;
  var sliderMin = %s;
  var sliderMax = %s;
  var sliderStep = %s;
  var depthRange = [%s, %s];
  var map = null;
  var layerGroup = null;
  var legendEl = null;
  var thresholdLabelEl = null;

  function resolveMap() {
    if (window.HTMLWidgets && typeof window.HTMLWidgets.getInstance === 'function') {
      var widget = window.HTMLWidgets.getInstance(el);
      if (widget && typeof widget.getMap === 'function') {
        return widget.getMap();
      }
    }
    if (window.jQuery) {
      var shinyMap = window.jQuery(el).data('leaflet-map');
      if (shinyMap) {
        return shinyMap;
      }
    }
    if (this && this.layerManager) {
      return this;
    }
    return null;
  }

  function shallowAnoxicDepth(cell, o2Threshold) {
    var depths = Array.isArray(cell.depths) ? cell.depths : [cell.depths];
    var oxygens = Array.isArray(cell.oxygens) ? cell.oxygens : [cell.oxygens];
    var minDepth = null;
    for (var i = 0; i < depths.length; i++) {
      if (oxygens[i] < o2Threshold) {
        if (minDepth === null || depths[i] < minDepth) {
          minDepth = depths[i];
        }
      }
    }
    return minDepth;
  }

  function colorForDepth(depth, minDepth, maxDepth) {
    if (depth === null || !isFinite(depth)) {
      return 'transparent';
    }
    var t = (depth - minDepth) / (maxDepth - minDepth);
    if (!isFinite(t)) {
      t = 0.5;
    }
    t = Math.max(0, Math.min(1, t));
    var palette = [
      '#440154', '#482878', '#3E4A89', '#31688E', '#26828E',
      '#1F9E89', '#35B779', '#6DCD59', '#B4DE2C', '#FDE725'
    ];
    var idx = Math.round(t * (palette.length - 1));
    return palette[idx];
  }

  function updateFieldLayer(o2Threshold) {
    if (!map || !layerGroup) return;

    var depths = [];
    cells.forEach(function(cell) {
      var d = shallowAnoxicDepth(cell, o2Threshold);
      if (d !== null && isFinite(d)) {
        depths.push(d);
      }
    });

    var minDepth = depths.length ? Math.min.apply(null, depths) : depthRange[0];
    var maxDepth = depths.length ? Math.max.apply(null, depths) : depthRange[1];

    layerGroup.clearLayers();
    cells.forEach(function(cell) {
      var d = shallowAnoxicDepth(cell, o2Threshold);
      if (d === null || !isFinite(d)) {
        return;
      }
      var bounds = L.latLngBounds(
        [cell.lat - %s, cell.lon - %s],
        [cell.lat + %s, cell.lon + %s]
      );
      L.rectangle(bounds, {
        color: 'transparent',
        weight: 0,
        fillColor: colorForDepth(d, minDepth, maxDepth),
        fillOpacity: 0.88
      }).addTo(layerGroup);
    });

    if (thresholdLabelEl) {
      thresholdLabelEl.textContent = 'O\u2082 threshold: ' + o2Threshold.toFixed(1) + ' mg/L';
    }
    if (legendEl) {
      legendEl.textContent = 'Depth of O\u2082 < ' + o2Threshold.toFixed(1) + ' mg/L (m)';
    }
  }

  function initThresholdControl() {
    var control = L.control({ position: 'topright' });
    control.onAdd = function() {
      var div = L.DomUtil.create('div', 'anoxic-threshold-control');
      div.style.cssText = [
        'background:#fff',
        'padding:10px 12px',
        'border-radius:4px',
        'box-shadow:0 1px 4px rgba(0,0,0,.3)',
        'font:13px/1.4 sans-serif',
        'min-width:260px'
      ].join(';');
      div.innerHTML = [
        '<div class=\"threshold-label\" style=\"font-weight:700;margin-bottom:8px;\"></div>',
        '<input type=\"range\" class=\"threshold-slider\" style=\"width:100%%;\">',
        '<div style=\"display:flex;justify-content:space-between;color:#444;font-size:11px;margin-top:4px;\">',
        '<span>' + sliderMin.toFixed(1) + ' mg/L</span>',
        '<span>' + sliderMax.toFixed(1) + ' mg/L</span>',
        '</div>',
        '<div class=\"legend-label\" style=\"margin-top:8px;color:#444;font-size:12px;\"></div>'
      ].join('');

      thresholdLabelEl = div.querySelector('.threshold-label');
      legendEl = div.querySelector('.legend-label');
      var slider = div.querySelector('.threshold-slider');
      slider.min = sliderMin;
      slider.max = sliderMax;
      slider.step = sliderStep;
      slider.value = threshold;

      slider.addEventListener('input', function() {
        updateFieldLayer(parseFloat(slider.value));
      });

      L.DomEvent.disableClickPropagation(div);
      return div;
    };
    control.addTo(map);
  }

  function start() {
    map = resolveMap.call(this);
    if (!map) {
      setTimeout(start, 25);
      return;
    }
    layerGroup = L.layerGroup().addTo(map);
    initThresholdControl();
    updateFieldLayer(threshold);
  }

  start();
}
",
      profile_json,
      threshold,
      slider_min,
      slider_max,
      slider_step,
      depth_range[1],
      depth_range[2],
      dy,
      dx,
      dy,
      dx
    ))

  map
}
