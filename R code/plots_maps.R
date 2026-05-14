
# functions ---------------------------------------------------------------

#' @title Map of lat-long points
#' 
#' Generate a map using latitude/longitude and colour-coded for a chosen metric.
#' @param df Dataframe being plotted
#' @param lat.field,long.field Field containing the latitude and longitude. Can be numeric rather than converted to lat/long using leaflet and sf.
#' @param colour.field Field by which colour datapoints
#' @param colour.approach Choose between "raw" for smoothed colouring on raw numbers (default), or "quintile" or "decile" for discrete grouping by size
#' @param colour.low,colour.middle,colour.high 3-layer colour scale to apply to data
#' @param colour.opacity Opacity to apply to colour, from 0 (transparent) to 1 (opaque). Default = 0.8.
#' @param mouseover.text Field containing mouseover text
#' @param radius Either a fixed numeric value to have equally sized circles (default = 1,000) or a field name containing numeric field to scale the size
#' @param radius.scale If radius is a field name, you may need to scale the figures up/down to make the circles a sensible size. This requires trial and error.
#' @param legend.title,legend.position Title and position of the legend. legend.position should be one of "topright", "bottomright", "bottomleft", or "topleft".
plt_map_latlong <- function(df, lat.field, long.field, colour.field, colour.approach = "raw",
                           colour.low = "#F8766D", colour.middle = "white", colour.high = "#00B0F6", colour.opacity = 0.8,
                           mouseover.text = NULL, radius = 1000, radius.scale = 1, 
                           legend.title = "Legend", legend.position = "bottomright", ...) {
  
  # set up
  
  colour.approach <- tolower(colour.approach)
  
  if (!colour.approach %in% c("raw", "quintile", "decile")) {
    stop("colour.approach should be raw, quintile, or decile")
  }
  
  if (!legend.position %in% c("topright", "bottomright", "bottomleft", "topleft")) {
    stop('legend.position should be one of "topright", "bottomright", "bottomleft", "topleft".')
  }
  
  lat.field = as.name(lat.field)
  long.field = as.name(long.field)
  mouseover.text = if (gtools::invalid(mouseover.text)) {NULL} else {as.name(mouseover.text)}
  radius.field = if (is.numeric(radius)) {NULL} else {as.name(radius)}
  colour.field = as.name(colour.field)
  
  # dataframe for plotting
  
  df.plt <- df %>% 
    rename(Latitude = !!lat.field,
           Longitude = !!long.field,
           Mouseover = !!mouseover.text,
           Radius = !!radius.field,
           Color = !!colour.field) %>% 
    rowwise() %>% 
    mutate(Size = ifelse(is.numeric(radius), radius, Radius * radius.scale)) %>% 
    ungroup() %>% 
    mutate(Percentile = case_when(
             colour.approach == "quintile" ~ ntile(Color, 5),
             colour.approach == "decile" ~ ntile(Color, 10),
             TRUE ~ NA
           )
    )
  
  
  # colours
  
  ## if percentile, number of bins
  
  bins <- if (colour.approach == "raw") {
    NA
  } else if (colour.approach == "quintile") {
    5
  } else if (colour.approach == "decile") {
    10
  } else {
    1
  }
  
  ## colour range
  
  ## if percentile run this with color factors, else with raw palette
  
  if (colour.approach %in% c("quintile", "decile")) {
    
    colour_list <-  pal_range_cats(colour.start = colour.low,
                                   colour.middle = colour.middle,
                                   colour.end = colour.high,
                                   num.categories = bins)
    
    color_levels <- unique(sort(df.plt$Percentile))
    color_factor <- leaflet::colorFactor(palette = colour_list,
                                         df.plt$Percentile,
                                         ordered = TRUE, 
                                         levels = color_levels
    )
    
    map_layers = function(df) {
      
      # base map
      map = leaflet::leaflet() %>%
        leaflet::addProviderTiles(leaflet::providers$Esri.WorldTopoMap) %>%
        leaflet.extras::addFullscreenControl() %>%
        
        # add the datapoints
        leaflet::addCircles(
          data = df,
          lng = ~Longitude, lat = ~Latitude, 
          radius = df$Size,
          label = if(is.null(mouseover.text)) {NULL} else {~lapply(Mouseover, htmltools::HTML)},
          fill = TRUE,
          stroke = FALSE,
          fillColor = ~color_factor(Percentile),
          fillOpacity = colour.opacity
        ) %>% 
        
        # add the legend
        leaflet::addLegend(position = legend.position, 
                           pal = color_factor, 
                           values = df$Percentile,
                           title = legend.title,
                           opacity = 0.8)
    }
    
    
    # plot the map
    map = map_layers(df.plt)
    
  } else if (colour.approach == "raw") {
    
    color_numeric <- leaflet::colorNumeric(
      palette = c(colour.low, colour.middle, colour.high), 
      domain = df.plt$Color
    )
    
    map_layers = function(df) {
      
      # base map
      map = leaflet::leaflet() %>%
        leaflet::addProviderTiles(leaflet::providers$Esri.WorldTopoMap) %>%
        leaflet.extras::addFullscreenControl() %>%
        
        # add the datapoints
        leaflet::addCircles(
          data = df,
          lng = ~Longitude, lat = ~Latitude, 
          radius = df$Size,
          label = if(is.null(mouseover.text)) {NULL} else {~lapply(Mouseover, htmltools::HTML)},
          fill = TRUE,
          stroke = FALSE,
          fillColor = ~color_numeric(Color),
          fillOpacity = colour.opacity
        ) %>% 
        
        # add the legend
        leaflet::addLegend(position = legend.position, 
                           pal = color_numeric, 
                           values = df$Color,
                           title = legend.title,
                           opacity = 0.8)
    }
    
    
    # plot the map
    map = map_layers(df.plt)
  }
  
  return(map)
  
}  


#' @title Map of pie charts by lat-long points
#' 
#' Generate a map using latitude/longitude and a pie chart showing distribution by geography. 
#' Data needs pre-grouping before the dataframe is passed into this function.
#' @param df Dataframe being plotted
#' @param lat.field,long.field Field containing the latitude and longitude. Can be numeric rather than converted to lat/long using leaflet and sf.
#' @param numeric.field Field by which pie chart is segmented
#' @param colour.field Field by which colour datapoints
#' @param geog.field Geography name to appear in centre of pie chart
#' @param colour.palette Color palette to use. Default is R's default.
#' @param colour.opacity Opacity to apply to colour, from 0 (transparent) to 1 (opaque). Default = 0.8.
#' @param radius Either a fixed numeric value to have equally sized circles (default = 50) or a field name containing numeric field to scale the size
#' @param radius.scale If radius is a field name, you may need to scale the figures up/down to make the circles a sensible size. This requires trial and error.
#' @param legend.title,legend.position Title and position of the legend. legend.position should be one of "topright", "bottomright", "bottomleft", or "topleft".
plt_map_latlong_pie <- function(df, lat.field, long.field, 
                                numeric.field, colour.field, geog.field,
                                colour.palette = NULL, colour.opacity = 0.8,
                                radius = 50, radius.scale = 1,
                                legend.title = "Legend", legend.position = "bottomright", ...) {
  
  # set up
  if (!legend.position %in% c("topright", "bottomright", "bottomleft", "topleft")) {
    stop('legend.position should be one of "topright", "bottomright", "bottomleft", "topleft".')
  }
  
  lat.field = as.name(lat.field)
  long.field = as.name(long.field)
  radius.field = if (is.numeric(radius)) {NULL} else {as.name(radius)}
  numeric.field = as.name(numeric.field)
  colour.field = as.name(colour.field)
  geog.field = as.name(geog.field)
  
  # dataframe for plotting
  
  df.plt <<- df %>% 
    rename(Latitude = !!lat.field,
           Longitude = !!long.field,
           Radius = !!radius.field,
           Color = !!colour.field,
           Numeric = !!numeric.field,
           Geog = !!geog.field) %>% 
    rowwise() %>% 
    mutate(Size = ifelse(is.numeric(radius), radius, Radius * radius.scale)) %>% 
    ungroup() %>% 
    pivot_wider(values_from = Numeric,
                values_fill = 0,
                names_from = Color)
  
  
  # colours
  
  # palette for the number of bins needed
  color_levels <- unique(df[[colour.field]]) %>% sort()
  color_length <- length(color_levels)
  palette_length <- length(colour.palette)
  
  if(gtools::invalid(colour.palette)) {
    colour.palette.clean <- scales::hue_pal()(color_length)
  } else if(palette_length < color_length) {
    warning(paste0("colour.palette is of length ", palette_length, " but ", color_length, " required. Using default palette."))
    colour.palette.clean <- scales::hue_pal()(color_length)
  } else {
    colour.palette.clean <- colour.palette
  }
  
  map_palette <- head(colour.palette.clean, color_length)
  
  # first, give the palette for every possible site to ensure consistency
  color_factor <- leaflet::colorFactor(
    palette = map_palette, 
    df[[colour.field]], 
    ordered = TRUE, 
    levels = color_levels
  )
  
  # then, just the relevant colors needed
  uniques <- colnames(df.plt %>% select(any_of(color_levels)))
  indexes <- match(uniques, color_levels)
  map_palette_pie <- map_palette[indexes]
  
  
  map_layers = function(df) {
    
    # base map
    map = leaflet::leaflet() %>%
      leaflet::addProviderTiles(leaflet::providers$Esri.WorldTopoMap) %>%
      leaflet.extras::addFullscreenControl() %>%
      
      # add the datapoints
      leaflet.minicharts::addMinicharts(
        chartdata = df %>% select(any_of(color_levels)), 
        lng = df$Longitude, lat = df$Latitude, 
        width = df$Size, height = df$Size,
        showLabels = FALSE,
        type = "pie",
        colorPalette = map_palette_pie,
        opacity = colour.opacity,
        legend = FALSE,
        transitionTime = 0,
        popupOptions = list(closeButton = FALSE, showTitle = TRUE)
      ) %>% 
      
      # add site marker
      leaflet::addCircleMarkers(
        data = df,
        lng = df$Longitude, lat = df$Latitude,
        radius = df$Size*0.2,
        stroke = FALSE, fill = TRUE, fillOpacity = 0.2, fillColor = "black",
        label = df$Geog
      ) %>% 
      
      # add the legend
      leaflet::addLegend(position = legend.position, 
                         pal = color_factor, values = color_levels,
                         title = legend.title,
                         opacity = 0.8)
  }
  
  
  # plot the map
  map = map_layers(df.plt)
  
  return(map)
  
}


#' @title Map by LSOA
#' 
#' Generate a map using LSOAs and colour-coded for a chosen metric. Could also be used by MSOA, electoral wards, etc.
#' @param df Dataframe with the data being plotted
#' @param geog.field Field containing the name of the geography which must match the naming in the shape file
#' @param shape.df A shape file containing LSOA (or relevant) boundaries. df will be inner joined onto shape.df
#' @param shape.field Field containing the name of the geography which must match the naming in "df"
#' @param colour.field Field by which colour datapoints
#' @param colour.approach Choose between "raw" for smoothed colouring on raw numbers (default), or "quintile" or "decile" for discrete grouping by size
#' @param colour.low,colour.middle,colour.high 3-layer colour scale to apply to data
#' @param colour.opacity Opacity to apply to colour, from 0 (transparent) to 1 (opaque). Default = 0.8.
#' @param mouseover.text Field containing mouseover text
#' @param legend.title,legend.position Title and position of the legend. legend.position should be one of "topright", "bottomright", "bottomleft", or "topleft".
plt_map_lsoa <- function(df, geog.field, shape.df, shape.field,
                         colour.field, colour.approach = "raw",
                         colour.low = "#F8766D", colour.middle = "white", colour.high = "#00B0F6", colour.opacity = 0.8,
                         mouseover.text = NULL, 
                         legend.title = "Legend", legend.position = "bottomright", ...) {
  # set up
  
  colour.approach <- tolower(colour.approach)
  
  if (!colour.approach %in% c("raw", "quintile", "decile")) {
    stop("colour.approach should be raw, quintile, or decile")
  }
  
  if (!legend.position %in% c("topright", "bottomright", "bottomleft", "topleft")) {
    stop('legend.position should be one of "topright", "bottomright", "bottomleft", "topleft".')
  }
  
  mouseover.text = if (gtools::invalid(mouseover.text)) {NULL} else {as.name(mouseover.text)}
  colour.field = as.name(colour.field)
  geog.field = as.name(geog.field)
  shape.field = as.name(shape.field)
  
  
  # dataframe for plotting
  
  df.plt <- shape.df %>% 
    rename(Fill = !!shape.field) %>% 
    inner_join(df, by = c("Fill" = as.character(geog.field))) %>% 
    rename(Mouseover = !!mouseover.text,
           Color = !!colour.field) %>% 
    mutate(Percentile = case_when(
      colour.approach == "quintile" ~ ntile(Color, 5),
      colour.approach == "decile" ~ ntile(Color, 10),
      TRUE ~ NA
    ))
  
  
  # colours
  
  ## if percentile, number of bins
  
  bins <- if (colour.approach == "raw") {
    NA
  } else if (colour.approach == "quintile") {
    5
  } else if (colour.approach == "decile") {
    10
  } else {
    1
  }
  
  # produce maps
  
  if (colour.approach %in% c("quintile", "decile")) {
    
    colour_list <-  pal_range_cats(colour.start = colour.low,
                                   colour.middle = colour.middle,
                                   colour.end = colour.high,
                                   num.categories = bins)
    
    color_levels <- unique(sort(df.plt$Percentile))
    color_factor <- leaflet::colorFactor(palette = colour_list,
                                         df.plt$Percentile,
                                         ordered = TRUE, 
                                         levels = color_levels
    )
    
    map_layers = function(df) {
      
      # base map
      map = leaflet::leaflet() %>%
        leaflet::addProviderTiles(leaflet::providers$Esri.WorldTopoMap) %>%
        leaflet.extras::addFullscreenControl() %>%
        
        # add the datapoints
        leaflet::addPolygons(
          data = df.plt,
          weight = 0.5, opacity = 1, color = "grey",
          fillColor = ~color_factor(Percentile), fillOpacity = colour.opacity,
          label = if(is.null(mouseover.text)) {NULL} else {~lapply(Mouseover, htmltools::HTML)},
        ) %>% 
        
        # add the legend
        leaflet::addLegend(position = legend.position, 
                           pal = color_factor,
                           values = df.plt$Percentile,
                           title = legend.title,
                           opacity = 0.8)
    }
    
    
    # plot the map
    map = map_layers(df.plt)
    
  } else if (colour.approach == "raw") {
    
    color_numeric <- leaflet::colorNumeric(
      palette = c(colour.low, colour.middle, colour.high), 
      domain = df.plt$Color
    )
    
    map_layers = function(df) {
      
      # base map
      map = leaflet::leaflet() %>%
        leaflet::addProviderTiles(leaflet::providers$Esri.WorldTopoMap) %>%
        leaflet.extras::addFullscreenControl() %>%
        
        # add the datapoints
        leaflet::addPolygons(
          data = df.plt,
          weight = 0.5, opacity = 1, color = "grey",
          fillColor = ~color_numeric(Color), fillOpacity = colour.opacity,
          label = if(is.null(mouseover.text)) {NULL} else {~lapply(Mouseover, htmltools::HTML)},
        ) %>% 
        
        # add the legend
        leaflet::addLegend(position = legend.position, 
                           pal = color_numeric, 
                           values = df.plt$Color,
                           title = legend.title,
                           opacity = 0.8)
    }
    
    # plot the map
    map = map_layers(df.plt)
    
  }
  
  return(map)
  
}


# examples ----------------------------------------------------------------

library(sf)
library(leaflet)
library(leaflet.minicharts)

stores_latlong <- readr::read_csv("stores_latlong.csv")
stores_fruit <- readr::read_csv("stores_fruit.csv") %>% 
  left_join(stores_latlong, by = "store")


## Lat/long heatmap ----
df_prices <- stores_fruit %>% 
  filter(product == "apple"
         & !is.na(ppu)) %>% 
  group_by(store, lat, long) %>%
  summarise(ppu = mean(ppu, na.rm = TRUE),
            quantity = mean(quantity, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(label = paste0("<b>", store, "</b>",
                        "<br> Mean price: ", prettyNum(ppu, big.mark = ","),
                        "<br> Mean quantity: ", prettyNum(quantity, big.mark = ",")
                        )
         )

plt.store.heatmap <- plt_map_latlong(df = df_prices,
                                     lat.field = "lat",
                                     long.field = "long", 
                                     colour.field = "ppu",
                                     colour.approach = "raw",
                                     colour.low = "blue",
                                     colour.middle = "gold",
                                     colour.high = "red",
                                     colour.opacity = 0.5,
                                     mouseover.text = "label",
                                     radius = "quantity",
                                     radius.scale = 10,
                                     legend.title = "Price of apples per unit",
                                     legend.position = "bottomright"
)


## Lat/long pie chart ----
df_quant <- stores_fruit %>% 
  group_by(store, lat, long, product) %>%
  summarise(quantity = sum(quantity, na.rm = TRUE)) %>% 
  ungroup() %>% 
  group_by(store, lat, long) %>%
  mutate(total = sum(quantity, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(radius = total / max(total))

plt.map.gp.piechart <- plt_map_latlong_pie(df = df_quant,
                                          lat.field = "lat", # set the latitude
                                          long.field = "long", # set the longitude
                                          
                                          numeric.field = "quantity", # for what drives the pie chart. Also appears in labels
                                          colour.field = "product", # set a field to colour the points by
                                          geog.field = "store", # title for the labels
                                          
                                          colour.palette = c("blue", "gold", "red"),
                                          colour.opacity = 0.8,
                                          
                                          radius = "radius", # what should the size of the dot be based on?
                                          radius.scale = 30, # start at 1, then scale up/down as needed
                                          legend.title = "Product",
                                          legend.position = "bottomright"
)


## LSOA heatmap ----

# not a practical example, just generally showing how the function might work
df_prices_lsoa <- stores_fruit %>% 
  filter(product == "apple"
         & !is.na(ppu)) %>% 
  group_by(store, lsoa_21) %>%
  summarise(ppu = mean(ppu, na.rm = TRUE),
            quantity = mean(quantity, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(label = paste0("<b>", store, "</b>",
                        "<br> Mean price: ", prettyNum(ppu, big.mark = ","),
                        "<br> Mean quantity: ", prettyNum(quantity, big.mark = ",")
  ))

lsoa.shp <- read_sf(dsn = "C:/Users/Elesh.Mistry/OneDrive - NHS/Documents/Geography/LSOA_2021_SHP/LSOA_2021_EW_BGC.shp") %>%
  sf::st_transform('+proj=longlat +datum=WGS84')


plt.map.lsoa.heatmap <- plt_map_lsoa(df = df_prices_lsoa,
                                     geog.field = "lsoa_21",
                                     shape.df = lsoa.shp,
                                     shape.field = "LSOA21CD",
                                     colour.field = "ppu",
                                     colour.approach = "raw",
                                     colour.low = "blue",
                                     colour.middle = "gold",
                                     colour.high = "red",
                                     colour.opacity = 0.5,
                                     mouseover.text = "label",
                                     legend.title = "Price of apples per unit",
                                     legend.position = "bottomright")
