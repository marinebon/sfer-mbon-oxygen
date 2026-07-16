plot_hypoxic_events_map <- function(
    observations,
    threshold,
    layers = NULL) {
  if (!exists("build_hypoxic_events_map_payload", mode = "function")) {
    if (requireNamespace("here", quietly = TRUE)) {
      source(here::here("R/hypoxic_summary.R"), local = FALSE)
    } else {
      source("R/hypoxic_summary.R", local = FALSE)
    }
  }

  obs <- as.data.frame(observations)
  depth_layers <- build_hypoxic_events_map_payload(obs, threshold, layers)
  if (length(depth_layers) == 0) {
    return(NULL)
  }

  valid <- is.finite(obs$longitude) & is.finite(obs$latitude)
  lon_min <- min(obs$longitude[valid], na.rm = TRUE)
  lon_max <- max(obs$longitude[valid], na.rm = TRUE)
  lat_min <- min(obs$latitude[valid], na.rm = TRUE)
  lat_max <- max(obs$latitude[valid], na.rm = TRUE)

  widget_id <- paste0("hypoxic-events-map-", sample.int(1e8, 1L))
  depth_layers_json <- jsonlite::toJSON(depth_layers, auto_unbox = TRUE)

  leaflet::leaflet(
    width = "100%",
    height = 560,
    elementId = widget_id,
    options = leaflet::leafletOptions(preferCanvas = TRUE)
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
  var idx = 0;
  var map = null;
  var eventsGroup = null;
  var depthLabelEl = null;
  var depthMetaEl = null;
  var countEl = null;
  var prevBtn = null;
  var nextBtn = null;

  var xIcon = L.divIcon({
    className: 'hypoxic-x-marker',
    html: '<div style=\"font-size:26px;font-weight:700;color:#c41a1a;text-shadow:-1px -1px 0 #fff,1px -1px 0 #fff,-1px 1px 0 #fff,1px 1px 0 #fff;line-height:1;transform:translate(-50%%,-50%%);\">×</div>',
    iconSize: [26, 26],
    iconAnchor: [13, 13]
  });

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

  function renderDepthLayer(layerIdx) {
    if (!map || !eventsGroup) return;
    var layer = depthLayers[layerIdx];
    if (!layer) return;

    eventsGroup.clearLayers();
    (layer.events || []).forEach(function(evt) {
      L.marker([evt.lat, evt.lon], { icon: xIcon })
        .bindTooltip(evt.label, { sticky: true, direction: 'top' })
        .bindPopup(evt.popup)
        .addTo(eventsGroup);
    });

    if (depthLabelEl) {
      depthLabelEl.textContent = layer.label;
    }
    if (depthMetaEl) {
      depthMetaEl.textContent = (layerIdx + 1) + ' of ' + depthLayers.length;
    }
    if (countEl) {
      countEl.textContent = (layer.n_events || layer.events.length) + ' hypoxic samples';
    }
  }

  function initMapControls() {
    var control = L.control({ position: 'topright' });
    control.onAdd = function() {
      var div = L.DomUtil.create('div', 'hypoxic-map-controls');
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
        '<div class=\"event-count\" style=\"margin-top:6px;text-align:center;color:#444;font-size:12px;\"></div>'
      ].join('');

      depthLabelEl = div.querySelector('.depth-current');
      depthMetaEl = div.querySelector('.depth-meta');
      countEl = div.querySelector('.event-count');
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
    eventsGroup = L.layerGroup().addTo(map);
    initMapControls();
    renderDepthLayer(idx);
  }

  start();
}
",
      depth_layers_json
    ))
}
