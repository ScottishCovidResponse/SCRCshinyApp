library(shiny)
library(hdf5r)
library(dplyr)
library(ggplot2)
library(leaflet)
library(SPARQL)
library(rgdal)
library(demographicData)
library(spdplyr)
library(shinydashboard)
library(shinycssloaders)
library(SCRCdataAPI)


dz.sf <- rgdal::readOGR(file.path("..", "SCRCdataAPI", "data-raw",
                                  "datazone_shapefile",
                                  "SG_DataZone_Bdry_2011.shp"))


h5filename <- file.path("..", "SCRCdataAPI", "demographics.h5")
# file_structure(h5filename)
original.dat <- reconstruct_object(h5filename, "dz/1year/persons") %>%
  tibble::rownames_to_column("DataZone") %>%
  dplyr::mutate(total = rowSums(dplyr::select(., -DataZone))) %>%
  dplyr::select(DataZone, total)

# Simplify the polygons
# simple.dz.sf <- rmapshaper::ms_simplify(dz.sf)

# simple.dz.sf <- rgeos::gSimplify(dz.sf, tol = 5)
# mapview::mapview(simple.dz.sf)

plot.map <- merge(dz.sf, original.dat, by = "DataZone")
# Convert to Longitude / Latitude with WGS84 Coordinate System
wgs84 <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
merged_ll <- sp::spTransform(plot.map, sp::CRS(wgs84))
# mapview::mapview(merged_ll)

bins <- c(seq(0, 3000, 500), Inf)
pal <- leaflet::colorBin("YlOrRd", domain = merged_ll$total, bins = bins)

map <- leaflet::leaflet(merged_ll) %>%
  leaflet::addProviderTiles("CartoDB.Positron",
                            options = leaflet::providerTileOptions(opacity = 0.99)) %>%
  leaflet::addPolygons(fillColor = ~pal(total),
                       weight = 2,
                       opacity = 1,
                       color = "white",
                       dashArray = "3",
                       fillOpacity = 0.7,
                       highlight = leaflet::highlightOptions(
                         weight = 2,
                         color = "#666",
                         dashArray = "",
                         fillOpacity = 0.7,
                         bringToFront = TRUE),
                       label = ~paste(merged_ll$total ),
                       labelOptions = leaflet::labelOptions(textsize = "15px",
                                                            direction = "auto"))

mapLegend <- map %>%
  leaflet::addLegend(pal = pal,
                     values = ~total,
                     opacity = 0.7,
                     title = "total",
                     position = "bottomright")

