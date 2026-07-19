plot_hypoxic_frequency_map <- function(
    frequency_df,
    layers,
    threshold) {
  if (!exists("build_hypoxic_frequency_map_payload", mode = "function")) {
    if (requireNamespace("here", quietly = TRUE)) {
      source(here::here("R/hypoxic_field_summary.R"), local = FALSE)
    } else {
      source("R/hypoxic_field_summary.R", local = FALSE)
    }
  }

  depth_layers <- build_hypoxic_frequency_map_payload(frequency_df, layers)
  if (length(depth_layers) == 0) {
    return(NULL)
  }

  if (!requireNamespace("viridisLite", quietly = TRUE)) {
    stop("Package 'viridisLite' is required.")
  }

  if (!exists("hypoxic_pct_palette", mode = "function")) {
    if (requireNamespace("here", quietly = TRUE)) {
      source(here::here("R/hypoxic_field_summary.R"), local = FALSE)
    } else {
      source("R/hypoxic_field_summary.R", local = FALSE)
    }
  }

  palette <- hypoxic_pct_palette(256)
  palette_json <- jsonlite::toJSON(palette, auto_unbox = FALSE)

  color_limits <- hypoxic_pct_log_limits()
  color_log_min <- color_limits$log_min
  color_log_max <- color_limits$log_max
  legend_gradient <- hypoxic_pct_legend_gradient(palette = palette)
  legend_gradient_json <- jsonlite::toJSON(legend_gradient, auto_unbox = TRUE)

  bounds <- hypoxic_frequency_map_bounds(frequency_df)
  if (is.null(bounds)) {
    return(NULL)
  }

  widget_id <- paste0("hypoxic-frequency-map-", sample.int(1e8, 1L))
  depth_layers_json <- jsonlite::toJSON(depth_layers, auto_unbox = TRUE)

  map <- leaflet::leaflet(
    width = "100%",
    height = 560,
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
    ) |>
    leaflet::addLayersControl(
      baseGroups = c("Ocean", "Light", "OpenStreetMap"),
      options = leaflet::layersControlOptions(collapsed = TRUE)
    ) |>
    leaflet::addScaleBar(position = "bottomleft") |>
    leaflet::fitBounds(
      lng1 = bounds$lon_min,
      lat1 = bounds$lat_min,
      lng2 = bounds$lon_max,
      lat2 = bounds$lat_max
    ) |>
    htmlwidgets::onRender(sprintf(
      "
function(el, x) {
  var depthLayers = %s;
  var palette = %s;
  var threshold = %s;
  var colorLogMin = %s;
  var colorLogMax = %s;
  var legendGradient = %s;
  var idx = 0;
  var map = null;
  var fieldGroup = null;
  var depthLabelEl = null;
  var depthMetaEl = null;
  var legendEl = null;
  var prevBtn = null;
  var nextBtn = null;

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

  function colorForPct(pct) {
    if (pct === null || !isFinite(pct) || pct <= 0) {
      return 'transparent';
    }
    pct = Math.max(pct, Math.pow(10, colorLogMin));
    var logVal = Math.log10(pct);
    var span = colorLogMax - colorLogMin;
    var t = span === 0 ? 0 : (logVal - colorLogMin) / span;
    t = Math.max(0, Math.min(1, t));
    var paletteIdx = Math.round(t * (palette.length - 1));
    return palette[paletteIdx];
  }

  function legendGradientCss() {
    return 'linear-gradient(to right, ' + legendGradient.map(function(stop) {
      return stop.color + ' ' + stop.position.toFixed(1) + '%%';
    }).join(', ') + ')';
  }

  function legendTickLabels() {
    return '<span>1%%</span><span>100%%</span>';
  }

  function renderDepthLayer(layerIdx) {
    if (!map || !fieldGroup) return;
    var layer = depthLayers[layerIdx];
    if (!layer) return;

    fieldGroup.clearLayers();
    (layer.cells || []).forEach(function(cell) {
      if (cell.pct <= 0) {
        return;
      }
      L.rectangle(
        [[cell.lat1, cell.lon1], [cell.lat2, cell.lon2]],
        {
          color: 'transparent',
          weight: 0,
          fillColor: colorForPct(cell.pct),
          fillOpacity: 0.88
        }
      )
        .bindTooltip(cell.label, { sticky: true, direction: 'top' })
        .bindPopup(cell.popup)
        .addTo(fieldGroup);
    });

    if (depthLabelEl) {
      depthLabelEl.textContent = layer.label;
    }
    if (depthMetaEl) {
      depthMetaEl.textContent = (layerIdx + 1) + ' of ' + depthLayers.length;
    }
    if (legendEl) {
      legendEl.textContent =
        'Color: log-scaled %% of cruises with O\u2082 below ' + threshold.toFixed(1) + ' mg/L';
    }
  }

  function initMapControls() {
    var control = L.control({ position: 'topright' });
    control.onAdd = function() {
      var div = L.DomUtil.create('div', 'hypoxic-frequency-map-controls');
      div.style.cssText = [
        'background:#fff',
        'padding:10px 12px',
        'border-radius:4px',
        'box-shadow:0 1px 4px rgba(0,0,0,.3)',
        'font:13px/1.4 sans-serif',
        'min-width:260px'
      ].join(';');
      div.innerHTML = [
        '<div style=\"display:flex;align-items:center;justify-content:space-between;gap:8px;\">',
        '<button type=\"button\" class=\"depth-prev\" style=\"padding:4px 8px;\">&#9664; Prev</button>',
        '<div class=\"depth-current\" style=\"flex:1;text-align:center;font-size:16px;font-weight:700;\"></div>',
        '<button type=\"button\" class=\"depth-next\" style=\"padding:4px 8px;\">Next &#9654;</button>',
        '</div>',
        '<div class=\"depth-meta\" style=\"margin-top:6px;text-align:center;color:#444;font-size:12px;\"></div>',
        '<div class=\"legend-label\" style=\"margin-top:8px;color:#444;font-size:12px;\"></div>',
        '<div style=\"margin-top:8px;height:14px;border:1px solid #ccc;border-radius:2px;',
        'background:' + legendGradientCss() + ';\"></div>',
        '<div style=\"display:flex;justify-content:space-between;color:#444;font-size:11px;margin-top:4px;\">',
        legendTickLabels(),
        '</div>'
      ].join('');

      depthLabelEl = div.querySelector('.depth-current');
      depthMetaEl = div.querySelector('.depth-meta');
      legendEl = div.querySelector('.legend-label');
      prevBtn = div.querySelector('.depth-prev');
      nextBtn = div.querySelector('.depth-next');

      if (prevBtn) {
        prevBtn.addEventListener('click', function() {
          idx = (idx - 1 + depthLayers.length) %% depthLayers.length;
          renderDepthLayer(idx);
        });
      }
      if (nextBtn) {
        nextBtn.addEventListener('click', function() {
          idx = (idx + 1) %% depthLayers.length;
          renderDepthLayer(idx);
        });
      }

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
    fieldGroup = L.layerGroup().addTo(map);
    initMapControls();
    renderDepthLayer(idx);
  }

  start();
}
",
      depth_layers_json,
      palette_json,
      threshold,
      color_log_min,
      color_log_max,
      legend_gradient_json
    ))

  map
}
