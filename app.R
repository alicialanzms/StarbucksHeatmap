library(shiny)
library(DT)
library(leaflet)
library(leaflet.extras)
library(htmltools)
library(crosstalk)
library(dplyr)

starbucks <- read.csv("starbucks.csv")
starbucks <- starbucks %>% mutate(ID = row_number())

sdf <- SharedData$new(
  starbucks,
  key   = ~ID,
  group = "SharedData"
)

lassoPlugin <- htmlDependency(
  name    = "Leaflet.lasso",
  version = "2.2.13",
  src     = c(file = "node_modules/leaflet-lasso/dist"),
  script  = "leaflet-lasso.umd.min.js"
)

registerPlugin <- function(map, plugin) {
  map$dependencies <- c(map$dependencies, list(plugin))
  map
}

ui <- fluidPage(
  titlePanel("Spatial App: Starbucks"),
  
  fluidRow(
    column(
      width = 8,
      leafletOutput("map", height = 600),
      DTOutput("table1")
    ),
    column(
      width = 4,
      actionButton("clear_lasso", "Clear Selection"),
      tags$div(
        id = "mySidebar",
        style = "background-color: #f2f2f2; padding: 1em; margin-top: 10px;",
        "No points selected"
      )
    )
  )
)

server <- function(input, output, session) {
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.DarkMatter) %>%
      setView(lng = -100, lat = 40, zoom = 4) %>%
      
      addHeatmap(
        data = starbucks,
        lng = starbucks$Longitude, lat = starbucks$Latitude,
        blur = 40, max = 0.05, radius = 18,
        group = "Heatmap"
      ) %>%
      
      addCircleMarkers(
        data    = sdf,
        lat     = ~Latitude,
        lng     = ~Longitude,
        layerId = ~ID,
        radius  = 0.0000025,
        fillOpacity = 0.2,
        weight = 0.5,
        popup = ~paste0(
          "<b>Store #:</b> ", `StoreNumber`, "<br/>",
          "<b>Street:</b> ", `StreetAddress`
        )
      ) %>%
      
      registerPlugin(lassoPlugin) %>%
      
      htmlwidgets::onRender("
        function(el, x) {
          setTimeout(() => {

            var sheet = window.document.styleSheets[0];
            sheet.insertRule('.selectedMarker { filter: hue-rotate(135deg); }', sheet.cssRules.length);
            sheet.insertRule('.leaflet-popup { z-index: 999999 !important; }', sheet.cssRules.length);
            sheet.insertRule('.leaflet-tooltip { z-index: 999999 !important; }', sheet.cssRules.length);

            var map = this;

            const lassoControl = L.control.lasso({ position: 'topleft' }).addTo(map);

            var ct_filter = new crosstalk.FilterHandle('SharedData');
            ct_filter.setGroup('SharedData');
            var ct_sel = new crosstalk.SelectionHandle('SharedData');
            ct_sel.setGroup('SharedData');

            function resetSelectedState() {
              map.eachLayer(function(layer) {
                if (layer instanceof L.Marker) {
                  layer.setIcon(new L.Icon.Default());
                } else if (layer instanceof L.Path) {
                  layer.setStyle({ color: '#3388ff' });
                }
              });
            }

            function setSelectedLayers(layers) {
              resetSelectedState();
              let markerCount = 0;
              let ids = [];

              layers.forEach(function(layer) {
                if (layer instanceof L.Marker || layer instanceof L.CircleMarker) {
                  markerCount++;
                  if (layer.setIcon) {
                    layer.setIcon(new L.Icon.Default({ className: 'selectedMarker' }));
                  } else if (layer.setStyle) {
                    layer.setStyle({
                      color: '#ff4620',
                      fillColor: '#ff4620',
                      fillOpacity: 1
                    });
                  }
                } else if (layer instanceof L.Path) {
                  layer.setStyle({ color: '#ff4620' });
                }
                if (layer.options && layer.options.layerId) {
                  ids.push(layer.options.layerId);
                }
              });

              ct_filter.set(ids);

              var sidebarEl = document.getElementById('mySidebar');
              if (sidebarEl) {
                if (markerCount === 0) {
                  sidebarEl.innerHTML = '<p>No points selected</p>';
                } else {
                  sidebarEl.innerHTML = '<b>' + markerCount + ' point(s) selected</b>';
                }
              }
            }

            map.on('lasso.finished', function(ev) {
              setSelectedLayers(ev.layers);
            });

            lassoControl.setOptions({ intersect: true });

            function clearSel() {
              ct_filter.clear();
              ct_sel.clear();
              resetSelectedState();
              var sidebarEl = document.getElementById('mySidebar');
              if (sidebarEl) {
                sidebarEl.innerHTML = '<p>No points selected</p>';
              }
            }
            if (document.getElementById('clear_lasso')) {
              document.getElementById('clear_lasso').onclick = clearSel;
            }

          }, 50);
        }
      ")
  })
  
  output$table1 <- renderDT({
    datatable(sdf)
  }, server = FALSE)
}

shinyApp(ui, server)