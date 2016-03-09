# devtools::install_github('rCharts', 'bbest') # tweak to make var geojsonLayer available
require(rCharts)

require(jsonlite)

regions=RJSONIO::fromJSON(file.path('cuadrante-shps', 'cuads.json'))

lmap <- Leaflet$new()
lmap$tileLayer(provide='Stamen.TonerLite')
lmap$setView(c(19.4333, -99.1333), zoom = 9)
lmap$geoJson(
  regions, 
  style = "#! function(feature) {
  var rgn2col = {1:'red',2:'blue',4:'green'};     
  return {
  color: rgn2col[feature.properties['id']],
  strokeWidth: '1px',
  strokeOpacity: 0.5,
  fillOpacity: 0.2
  }; } !#",
  onEachFeature = "#! function (feature, layer) {
  
  // info rollover
  if (document.getElementsByClassName('info leaflet-control').length == 0 ){
  info = L.control({position: 'topright'});  // NOTE: made global b/c not ideal place to put this function
  info.onAdd = function (map) {
  this._div = L.DomUtil.create('div', 'info');
  this.update();
  return this._div;
  };
  info.update = function (props) {
  this._div.innerHTML = '<h4>Field Name</h4>' +  (props ?
  props['region_id'] + ': <b> + props[fld] + </b>'
  : 'Hover over a region');
  };
  info.addTo(map);
  };
  
  // mouse events
  layer.on({
  
  // mouseover to highlightFeature
  mouseover: function (e) {
  var layer = e.target;
  layer.setStyle({
  strokeWidth: '3px',
  strokeOpacity: 0.7,
  fillOpacity: 0.5
  });
  if (!L.Browser.ie && !L.Browser.opera) {
  layer.bringToFront();
  }
  info.update(layer.feature.properties);
  },
  
  // mouseout to resetHighlight
  mouseout: function (e) {
  geojsonLayer.resetStyle(e.target);
  info.update();
  },
  
  // click to zoom
  click: function (e) {
  var layer = e.target;        
  if ( feature.geometry.type === 'MultiPolygon' ) {        
  // for multipolygons get true extent
  var bounds = layer.getBounds(); // get the bounds for the first polygon that makes up the multipolygon
  // loop through coordinates array, skip first element as the bounds var represents the bounds for that element
  for ( var i = 1, il = feature.geometry.coordinates[0].length; i < il; i++ ) {
  var ring = feature.geometry.coordinates[0][i];
  var latLngs = ring.map(function(pair) {
  return new L.LatLng(pair[1], pair[0]);
  });
  var nextBounds = new L.LatLngBounds(latLngs);
  bounds.extend(nextBounds);
  }
  map.fitBounds(bounds);
  } else {
  // otherwise use native target bounds
  map.fitBounds(e.target.getBounds());
  }
  }
  });
  } !#")
legend_vec = c('red'='high', 'blue'='medium', 'green'='low')
lmap$legend(position = 'bottomright', 
            colors   =  names(legend_vec), 
            labels   =  as.vector(legend_vec))
lmap
