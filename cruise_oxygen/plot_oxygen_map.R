plot_oxygen_map <- function(field, observations) {
  if (is.null(field) || nrow(field) == 0) {
    return(NULL)
  }

  if (!exists("oxygen_color_domain")) {
    if (requireNamespace("here", quietly = TRUE)) {
      source(here::here("R/color_scales.R"), local = FALSE)
    } else {
      source("R/color_scales.R", local = FALSE)
    }
  }

  max_depth <- max(field$depth_m, na.rm = TRUE)
  layers <- oxygen_depth_layers(max_depth = max_depth)
  depth_layers <- build_oxygen_map_payload(field, observations, layers)
  if (length(depth_layers) == 0) {
    return(NULL)
  }

  obs <- as.data.frame(observations)
  color_domain <- oxygen_color_domain(field, obs)
  color_limits <- color_domain
  o2_palette <- OXYGEN_PALETTE(256)

  lon_min <- min(field$longitude, na.rm = TRUE)
  lon_max <- max(field$longitude, na.rm = TRUE)
  lat_min <- min(field$latitude, na.rm = TRUE)
  lat_max <- max(field$latitude, na.rm = TRUE)

  widget_id <- paste0("oxygen-map-", sample.int(1e8, 1L))
  depth_layers_json <- jsonlite::toJSON(depth_layers, auto_unbox = TRUE)
  o2_palette_json <- jsonlite::toJSON(o2_palette, auto_unbox = FALSE)

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
    ) |>
    leaflet::addLayersControl(
      baseGroups = c("Ocean", "Light", "OpenStreetMap"),
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
  var depthLayers = %s;
  var colorDomain = [%s, %s];
  var colorLimits = [%s, %s];
  var o2Palette = %s;
  var idx = 0;
  var map = null;
  var fieldGroup = null;
  var obsGroup = null;
  var depthLabelEl = null;
  var depthMetaEl = null;
  var prevBtn = null;
  var nextBtn = null;
  var colorMinInput = null;
  var colorMaxInput = null;
  var colorGradientEl = null;

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

  function colorForValue(value) {
    if (value === null || !isFinite(value)) {
      return 'transparent';
    }
    var span = colorDomain[1] - colorDomain[0];
    var t = span === 0 ? 0 : (value - colorDomain[0]) / span;
    if (!isFinite(t)) {
      t = 0;
    }
    t = Math.max(0, Math.min(1, t));
    var paletteIdx = Math.round(t * (o2Palette.length - 1));
    return o2Palette[paletteIdx];
  }

  function updateColorGradient() {
    if (!colorGradientEl) return;
    var stops = [];
    for (var i = 0; i <= 20; i++) {
      var t = i / 20;
      var paletteIdx = Math.round(t * (o2Palette.length - 1));
      var pct = (t * 100).toFixed(1) + '%%';
      stops.push(o2Palette[paletteIdx] + ' ' + pct);
    }
    colorGradientEl.style.background =
      'linear-gradient(to right, ' + stops.join(', ') + ')';
  }

  function syncColorInputs() {
    if (colorMinInput) colorMinInput.value = colorDomain[0].toFixed(1);
    if (colorMaxInput) colorMaxInput.value = colorDomain[1].toFixed(1);
    updateColorGradient();
  }

  function applyColorDomain() {
    var minVal = parseFloat(colorMinInput.value);
    var maxVal = parseFloat(colorMaxInput.value);
    if (!isFinite(minVal) || !isFinite(maxVal)) return;
    minVal = Math.max(colorLimits[0], Math.min(colorLimits[1], minVal));
    maxVal = Math.max(colorLimits[0], Math.min(colorLimits[1], maxVal));
    if (minVal >= maxVal) {
      maxVal = Math.min(colorLimits[1], minVal + 0.1);
    }
    colorDomain[0] = minVal;
    colorDomain[1] = maxVal;
    syncColorInputs();
    renderDepthLayer(idx);
  }

  function renderDepthLayer(layerIdx) {
    if (!map || !fieldGroup || !obsGroup) return;
    var layer = depthLayers[layerIdx];
    if (!layer) return;

    fieldGroup.clearLayers();
    obsGroup.clearLayers();

    layer.field.forEach(function(cell) {
      L.rectangle(
        [[cell.lat1, cell.lon1], [cell.lat2, cell.lon2]],
        {
          color: 'transparent',
          weight: 0,
          fillColor: colorForValue(cell.o2),
          fillOpacity: 0.88
        }
      ).bindPopup(cell.popup).addTo(fieldGroup);
    });

    layer.obs.forEach(function(obs) {
      L.circleMarker([obs.lat, obs.lon], {
        radius: 6,
        fillColor: colorForValue(obs.o2),
        fillOpacity: 0.95,
        color: '#1a1a1a',
        weight: 1
      })
        .bindTooltip(obs.label, { sticky: true, direction: 'top' })
        .bindPopup(obs.popup)
        .addTo(obsGroup);
    });

    if (depthLabelEl) {
      depthLabelEl.textContent = layer.label;
    }
    if (depthMetaEl) {
      depthMetaEl.textContent = (layerIdx + 1) + ' of ' + depthLayers.length;
    }
  }

  function initMapControls() {
    var control = L.control({ position: 'topright' });
    control.onAdd = function() {
      var div = L.DomUtil.create('div', 'oxygen-map-controls');
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
        '<div style=\"font-weight:700;margin:12px 0 8px;\">O\u2082 color scale (mg/L)</div>',
        '<div style=\"display:flex;align-items:center;gap:6px;\">',
        '<input type=\"number\" class=\"color-min\" step=\"0.1\" style=\"width:72px;padding:2px 4px;\">',
        '<span style=\"color:#444;\">to</span>',
        '<input type=\"number\" class=\"color-max\" step=\"0.1\" style=\"width:72px;padding:2px 4px;\">',
        '</div>',
        '<div class=\"color-gradient\" style=\"margin-top:8px;height:14px;border:1px solid #ccc;border-radius:2px;\"></div>'
      ].join('');

      depthLabelEl = div.querySelector('.depth-current');
      depthMetaEl = div.querySelector('.depth-meta');
      prevBtn = div.querySelector('.depth-prev');
      nextBtn = div.querySelector('.depth-next');
      colorMinInput = div.querySelector('.color-min');
      colorMaxInput = div.querySelector('.color-max');
      colorGradientEl = div.querySelector('.color-gradient');
      colorMinInput.min = colorLimits[0];
      colorMinInput.max = colorLimits[1];
      colorMaxInput.min = colorLimits[0];
      colorMaxInput.max = colorLimits[1];

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
    fieldGroup = L.layerGroup().addTo(map);
    obsGroup = L.layerGroup().addTo(map);
    initMapControls();
    renderDepthLayer(idx);
  }

  start();
}
",
      depth_layers_json,
      color_domain[1],
      color_domain[2],
      color_limits[1],
      color_limits[2],
      o2_palette_json
    ))
}
