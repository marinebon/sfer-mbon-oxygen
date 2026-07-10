plot_anoxic_depth_map <- function(field, observations, threshold = NULL) {
  if (is.null(field) || nrow(field) == 0) {
    return(NULL)
  }

  if (!exists("anoxic_depth_color_domain")) {
    if (requireNamespace("here", quietly = TRUE)) {
      source(here::here("R/color_scales.R"), local = FALSE)
    } else {
      source("R/color_scales.R", local = FALSE)
    }
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

  color_domain <- anoxic_depth_color_domain(field)
  color_limits <- anoxic_depth_color_limits(field)
  depth_palette <- ANOXIC_DEPTH_PALETTE(256)

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
  depth_palette_json <- jsonlite::toJSON(depth_palette, auto_unbox = FALSE)

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
  var colorDomain = [%s, %s];
  var colorLimits = [%s, %s];
  var depthPalette = %s;
  var map = null;
  var layerGroup = null;
  var legendEl = null;
  var thresholdLabelEl = null;
  var colorMinInput = null;
  var colorMaxInput = null;
  var colorGradientEl = null;
  var currentThreshold = threshold;

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

  function colorForDepth(depth) {
    if (depth === null || !isFinite(depth)) {
      return 'transparent';
    }
    var span = colorDomain[1] - colorDomain[0];
    var t = span === 0 ? 0 : (depth - colorDomain[0]) / span;
    if (!isFinite(t)) {
      t = 0;
    }
    t = Math.max(0, Math.min(1, t));
    var idx = Math.round(t * (depthPalette.length - 1));
    return depthPalette[idx];
  }

  function updateColorGradient() {
    if (!colorGradientEl) return;
    var stops = [];
    for (var i = 0; i <= 20; i++) {
      var t = i / 20;
      var paletteIdx = Math.round(t * (depthPalette.length - 1));
      var pct = (t * 100).toFixed(1) + '%%';
      stops.push(depthPalette[paletteIdx] + ' ' + pct);
    }
    colorGradientEl.style.background =
      'linear-gradient(to right, ' + stops.join(', ') + ')';
  }

  function syncColorInputs() {
    if (colorMinInput) colorMinInput.value = colorDomain[0].toFixed(0);
    if (colorMaxInput) colorMaxInput.value = colorDomain[1].toFixed(0);
    updateColorGradient();
  }

  function applyColorDomain() {
    var minVal = parseFloat(colorMinInput.value);
    var maxVal = parseFloat(colorMaxInput.value);
    if (!isFinite(minVal) || !isFinite(maxVal)) return;
    minVal = Math.max(colorLimits[0], Math.min(colorLimits[1], minVal));
    maxVal = Math.max(colorLimits[0], Math.min(colorLimits[1], maxVal));
    if (minVal >= maxVal) {
      maxVal = Math.min(colorLimits[1], minVal + 1);
    }
    colorDomain[0] = minVal;
    colorDomain[1] = maxVal;
    syncColorInputs();
    updateFieldLayer(currentThreshold);
  }

  function updateFieldLayer(o2Threshold) {
    if (!map || !layerGroup) return;
    currentThreshold = o2Threshold;

    var depths = [];
    cells.forEach(function(cell) {
      var d = shallowAnoxicDepth(cell, o2Threshold);
      if (d !== null && isFinite(d)) {
        depths.push(d);
      }
    });

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
        fillColor: colorForDepth(d),
        fillOpacity: 0.88
      }).bindTooltip(
        'Depth: ' + d.toFixed(1) + ' m',
        { sticky: true, direction: 'top' }
      ).addTo(layerGroup);
    });

    if (thresholdLabelEl) {
      thresholdLabelEl.textContent = 'O\u2082 threshold: ' + o2Threshold.toFixed(1) + ' mg/L';
    }
    if (legendEl) {
      if (depths.length === 0) {
        legendEl.textContent = 'No cells below this O\u2082 threshold';
      } else {
        var activeMin = Math.min.apply(null, depths);
        var activeMax = Math.max.apply(null, depths);
        if (activeMin === activeMax) {
          legendEl.textContent =
            'All displayed cells: ' + activeMin.toFixed(1) + ' m (shallowest grid depth)';
        } else {
          legendEl.textContent =
            'Displayed depths: ' + activeMin.toFixed(1) + '\u2013' + activeMax.toFixed(1) + ' m';
        }
      }
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
        '<div style=\"font-weight:700;margin:12px 0 8px;\">Depth color scale (m)</div>',
        '<div style=\"display:flex;align-items:center;gap:6px;\">',
        '<input type=\"number\" class=\"color-min\" step=\"1\" style=\"width:72px;padding:2px 4px;\">',
        '<span style=\"color:#444;\">to</span>',
        '<input type=\"number\" class=\"color-max\" step=\"1\" style=\"width:72px;padding:2px 4px;\">',
        '</div>',
        '<div class=\"color-gradient\" style=\"margin-top:8px;height:14px;border:1px solid #ccc;border-radius:2px;\"></div>',
        '<div class=\"legend-label\" style=\"margin-top:8px;color:#444;font-size:12px;\"></div>'
      ].join('');

      thresholdLabelEl = div.querySelector('.threshold-label');
      legendEl = div.querySelector('.legend-label');
      colorMinInput = div.querySelector('.color-min');
      colorMaxInput = div.querySelector('.color-max');
      colorGradientEl = div.querySelector('.color-gradient');
      colorMinInput.min = colorLimits[0];
      colorMinInput.max = colorLimits[1];
      colorMaxInput.min = colorLimits[0];
      colorMaxInput.max = colorLimits[1];
      var slider = div.querySelector('.threshold-slider');
      slider.min = sliderMin;
      slider.max = sliderMax;
      slider.step = sliderStep;
      slider.value = threshold;

      slider.addEventListener('input', function() {
        updateFieldLayer(parseFloat(slider.value));
      });
      colorMinInput.addEventListener('change', applyColorDomain);
      colorMaxInput.addEventListener('change', applyColorDomain);
      syncColorInputs();

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
      color_domain[1],
      color_domain[2],
      color_limits[1],
      color_limits[2],
      depth_palette_json,
      dy,
      dx,
      dy,
      dx
    ))

  map
}
