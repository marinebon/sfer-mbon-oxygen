field_slice_grid <- function(field_slice) {
  lons <- sort(unique(field_slice$longitude))
  lats <- sort(unique(field_slice$latitude))
  n_lon <- length(lons)
  n_lat <- length(lats)

  dx <- if (n_lon > 1) diff(lons)[1] / 2 else 0.01
  dy <- if (n_lat > 1) diff(lats)[1] / 2 else 0.01

  field_slice$lon1 <- field_slice$longitude - dx
  field_slice$lon2 <- field_slice$longitude + dx
  field_slice$lat1 <- field_slice$latitude - dy
  field_slice$lat2 <- field_slice$latitude + dy
  field_slice
}

plot_oxygen_map <- function(field, observations) {
  if (is.null(field) || nrow(field) == 0) {
    return(NULL)
  }

  max_depth <- max(field$depth_m, na.rm = TRUE)
  layers <- oxygen_depth_layers(max_depth = max_depth)

  obs <- as.data.frame(observations)
  global_o2_range <- range(
    c(field$dissolved_oxygen, obs$dissolved_oxygen),
    na.rm = TRUE
  )
  pal <- leaflet::colorNumeric(
    viridisLite::turbo(256),
    domain = global_o2_range,
    na.color = "transparent"
  )

  lon_min <- min(field$longitude, na.rm = TRUE)
  lon_max <- max(field$longitude, na.rm = TRUE)
  lat_min <- min(field$latitude, na.rm = TRUE)
  lat_max <- max(field$latitude, na.rm = TRUE)

  widget_id <- paste0("oxygen-map-", sample.int(1e8, 1L))
  depth_groups <- list()

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

  for (i in seq_along(layers)) {
    layer <- layers[[i]]
    field_group <- sprintf("field-%s", layer$id)
    obs_group <- sprintf("obs-%s", layer$id)

    field_slice <- aggregate_field_layer(
      field,
      layer$depth_min,
      layer$depth_max
    )
    if (is.null(field_slice) || nrow(field_slice) == 0) {
      next
    }

    grid_slice <- field_slice_grid(field_slice)
    grid_slice$popup_label <- sprintf(
      "O\u2082: %.2f mg/L",
      grid_slice$dissolved_oxygen
    )
    obs_slice <- slice_observations_layer(
      observations,
      layer$depth_min,
      layer$depth_max
    )

    depth_groups[[length(depth_groups) + 1L]] <- list(
      field = field_group,
      obs = if (nrow(obs_slice) > 0) obs_group else NULL,
      label = layer$range_label
    )

    map <- map |>
      leaflet::addRectangles(
        data = grid_slice,
        lng1 = ~lon1,
        lat1 = ~lat1,
        lng2 = ~lon2,
        lat2 = ~lat2,
        fillColor = ~pal(dissolved_oxygen),
        fillOpacity = 0.88,
        color = NA,
        weight = 0,
        popup = ~popup_label,
        group = field_group,
        highlightOptions = leaflet::highlightOptions(
          weight = 1,
          color = "#333333",
          fillOpacity = 0.95,
          bringToFront = TRUE
        )
      )

    if (nrow(obs_slice) > 0) {
      map <- map |>
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
          group = obs_group
        )
    }
  }

  if (length(depth_groups) == 0) {
    return(NULL)
  }

  overlay_groups <- unique(c(
    vapply(depth_groups, function(g) g$field, character(1)),
    vapply(depth_groups, function(g) g$obs, character(1), USE.NAMES = FALSE)
  ))
  overlay_groups <- overlay_groups[!is.na(overlay_groups) & overlay_groups != ""]

  depth_groups_json <- jsonlite::toJSON(depth_groups, auto_unbox = TRUE)

  map <- map |>
    leaflet::addLayersControl(
      baseGroups = c("Ocean", "Light", "OpenStreetMap"),
      overlayGroups = overlay_groups,
      options = leaflet::layersControlOptions(collapsed = TRUE)
    ) |>
    leaflet::addLegend(
      pal = pal,
      values = global_o2_range,
      title = "O\u2082 (mg/L)",
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
  var groups = %s;
  var idx = 0;
  var depthLabelEl = null;
  var depthMetaEl = null;
  var prevBtn = null;
  var nextBtn = null;
  var map = null;

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

  function setGroupVisibility(groupName, visible) {
    if (!groupName || !map || !map.layerManager) return;
    var layer = map.layerManager.getLayerGroup(groupName, true);
    if (!layer) return;
    if (visible) {
      if (!map.hasLayer(layer)) {
        map.addLayer(layer);
      }
    } else if (map.hasLayer(layer)) {
      map.removeLayer(layer);
    }
  }

  function showDepth(i) {
    groups.forEach(function(g, j) {
      var active = j === i;
      setGroupVisibility(g.field, active);
      setGroupVisibility(g.obs, active);
    });
    if (depthLabelEl) {
      depthLabelEl.textContent = groups[i].label;
    }
    if (depthMetaEl) {
      depthMetaEl.textContent = (i + 1) + ' of ' + groups.length;
    }
  }

  function bindButtons() {
    if (prevBtn) {
      prevBtn.addEventListener('click', function() {
        idx = (idx - 1 + groups.length) %% groups.length;
        showDepth(idx);
      });
    }
    if (nextBtn) {
      nextBtn.addEventListener('click', function() {
        idx = (idx + 1) %% groups.length;
        showDepth(idx);
      });
    }
  }

  function initDepthControl() {
    var control = L.control({ position: 'topright' });
    control.onAdd = function() {
      var div = L.DomUtil.create('div', 'depth-nav');
      div.style.cssText = [
        'background:#fff',
        'padding:10px 12px',
        'border-radius:4px',
        'box-shadow:0 1px 4px rgba(0,0,0,.3)',
        'font:13px/1.4 sans-serif',
        'min-width:240px'
      ].join(';');
      div.innerHTML = [
        '<div style=\"display:flex;align-items:center;justify-content:space-between;gap:8px;\">',
        '<button type=\"button\" class=\"depth-prev\" style=\"padding:4px 8px;\">&#9664; Prev</button>',
        '<div class=\"depth-current\" style=\"flex:1;text-align:center;font-size:16px;font-weight:700;\"></div>',
        '<button type=\"button\" class=\"depth-next\" style=\"padding:4px 8px;\">Next &#9654;</button>',
        '</div>',
        '<div class=\"depth-meta\" style=\"margin-top:6px;text-align:center;color:#444;font-size:12px;\"></div>'
      ].join('');
      depthLabelEl = div.querySelector('.depth-current');
      depthMetaEl = div.querySelector('.depth-meta');
      prevBtn = div.querySelector('.depth-prev');
      nextBtn = div.querySelector('.depth-next');
      L.DomEvent.disableClickPropagation(div);
      bindButtons();
      return div;
    };
    control.addTo(map);
  }

  function start() {
    map = resolveMap.call(this);
    if (!map || !map.layerManager) {
      setTimeout(start, 25);
      return;
    }
    initDepthControl();
    showDepth(0);
  }

  start();
}
",
      depth_groups_json
    ))
}
