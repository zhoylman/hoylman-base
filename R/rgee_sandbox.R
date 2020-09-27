# conda create --name gee-base      # Create a conda environment
# 
# conda activate gee-base# Activate the environment
# 
# conda install python=3.7
# 
# conda install -c conda-forge earthengine-api # Install the Earth Engine Python API
# 
# earthengine authenticate          # Authenticate your access with the command line tool
# 
# conda install pandas
# conda install numpy



library(reticulate)
library(rgee)
library(cptcity)
library(raster)
library(stars)
library(sf)


use_condaenv("gee-base", conda = "auto",required = TRUE)
ee = import("ee")
#ee$Initialize()
ee_Initialize(email = 'zhoylman@gmail.com', drive = TRUE)

# Define a region of interest with sf
ee_roi <- st_read("/home/zhoylman/mesonet-dashboard/data/shp/states.shp") %>%
  filter(STATE_ABBR == 'MT') %>%
  st_geometry() %>%
  sf_as_ee()

# Search into the Earth Engine’s public data archive
# ee_search_dataset() %>%
#   ee_search_title("smap") %>%
#   ee_search_title("nasa") %>%
#   ee_search_display()

smap <- ee$ImageCollection("NASA_USDA/HSL/SMAP_soil_moisture")

smap_composite <- smap$
  filter(ee$Filter$date('2018-01-01', '2018-12-31'))

# Create a annual composite
smap_composite <- smap$
  filter(ee$Filter$date('2018-04-01', '2018-10-31'))$
  select("ssma")$
  median()

# Display results
scale <- 0.1
Map$setCenter(lon = -110,lat = 47,zoom = 6)
Map$addLayer(
  eeObject = smap_composite,
  visParams = list(
    min = -2,
    max = 2,
    palette = c("#FF0000","#FFFFFF","#0000FF")
  )
) + Map$addLayer(ee_roi)

# Download raster
ee_raster <- ee_as_raster(
  image = smap_composite,
  region = ee_roi, #ee_roi$geometry()
  dsn = "/home/zhoylman/temp/smap_test.tif",
  scale = 10000,
  via = "drive"
)

# ee_manage_cancel_all_running_task()
ndvi_mean_sf <- ee_extract(
  x = smap_composite,
  y = ee_roi,
  fun = ee$Reducer$mean(),
  scale = 10000,
  sf = TRUE
)




library(tidyverse)
library(gganimate)
library(rgee)
library(sf)

ee_Initialize()
ee_user_info()

# Define a Image or ImageCollection: Terraclimate
terraclimate <- ee$ImageCollection("IDAHO_EPSCOR/TERRACLIMATE")$
  filterDate("2001-01-01", "2002-01-01")$
  map(function(x){
    date <- ee$Date(x$get("system:time_start"))$format('YYYY_MM_dd')
    name <- ee$String$cat("Terraclimate_pp_", date)
    x$select("pr")$reproject("EPSG:4326")$set("RGEE_NAME", name)
  })

# Define a geometry
nc <- st_read(
  dsn = system.file("shape/nc.shp", package = "sf"),
  stringsAsFactors = FALSE,
  quiet = TRUE
)

# Extract values
ee_nc_rain <- ee_extract(
  x = terraclimate,
  y = nc,
  scale = 250,
  fun = ee$Reducer$mean(),
  sf = FALSE
)

# gganimate
colnames(ee_nc_rain) <- sprintf("%02d", 1:12)
ee_nc_rain$name <- nc$NAME

ee_nc_rain %>%
  pivot_longer(-name, names_to = "month", values_to = "pr") %>%
  ggplot(aes(x = as.integer(month), y = pr, color = pr)) +
  geom_line(alpha = 0.8, size = 2) +
  xlab("Month") +
  ylab("Precipitation (mm)") +
  theme_minimal() +
  transition_states(name) +
  shadow_mark(size = 0.4, colour = "grey")











# Define a Image or ImageCollection: Terraclimate
terraclimate <- ee$ImageCollection("IDAHO_EPSCOR/TERRACLIMATE")$
  filterDate("2001-01-01", "2002-01-01")$
  map(function(x){
    date <- ee$Date(x$get("system:time_start"))$format('YYYY_MM_dd')
    name <- ee$String$cat("Terraclimate_pp_", date)
    x$select("pr")$reproject("EPSG:4326")$set("RGEE_NAME", name)
  })

# Define a geometry
nc <- st_read("/home/zhoylman/mesonet-dashboard/data/shp/states.shp")

# Extract values
ee_nc_rain <- ee_extract(
  x = terraclimate,
  y = nc,
  scale = 250,
  fun = ee$Reducer$mean(),
  sf = TRUE
)

# Spatial plot
plot(
  ee_nc_rain["X200110_pr"],
  main = "2001 Jan Precipitation - Terraclimate",
  reset = FALSE
)
