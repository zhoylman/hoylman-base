#conda create --name gee-demo      # Create a conda environment
#
#conda activate gee-demo           # Activate the environment
#
#conda install -c conda-forge earthengine-api # Install the Earth Engine Python API
#
#earthengine authenticate          # Authenticate your access with the command line tool
#
#conda install pandas
#conda install numpy



library(reticulate)
library(rgee)
library(cptcity)
library(raster)
library(stars)
library(rgee)
library(sf)


use_condaenv("gee-demo", conda = "auto",required = TRUE)
ee = import("ee")
#ee$Initialize()
ee_Initialize()

# Define a region of interest with sf
ee_roi <- st_read("/home/zhoylman/mesonet-dashboard/data/shp/states.shp") %>%
  st_geometry() %>%
  sf_as_ee()

# Search into the Earth Engine’s public data archive
ee_search_dataset() %>%
  ee_search_title("smap") %>%
  ee_search_title("nasa") %>%
  ee_search_display()

smap <- ee$ImageCollection("NASA_USDA/HSL/SMAP_soil_moisture")

smap_composite <- smap$
  filter(ee$Filter$date('2018-01-01', '2018-12-31'))

# Create a annual composite
smap_composite <- smap$
  filter(ee$Filter$date('2017-04-01', '2017-10-31'))$
  select("ssma")$
  median()

# Display results
scale <- 0.1
Map$setCenter(lon = -79,lat = 35,zoom = 6)
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
  image = ndvi_composite,
  region = ee_roi$geometry(),
  dsn = "/home/aybarpc01/dd.tif",
  scale = 2000,
  via = "drive"
)

# ee_manage_cancel_all_running_task()
ndvi_mean_sf <- ee_extract(
  x = ndvi_composite,
  y = ee_roi,
  fun = ee$Reducer$mean(),
  scale = 2000,
  sf = TRUE
)














# Define a region of interest with sf
ee_roi <- st_read(system.file("shape/nc.shp", package="sf")) %>%
  st_geometry() %>%
  sf_as_ee()

# Search into the Earth Engine’s public data archive
ee_search_dataset() %>%
  ee_search_title("mod13") %>%
  ee_search_title("1km") %>%
  ee_search_display()

modis_ndvi <- ee$ImageCollection("MODIS/006/MOD13A2")


# Filter out poor quality pixels
getQABits <- function(image, qa) {
  # Convert binary (character) to decimal (little endian)
  qa <- sum(2^(which(rev(unlist(strsplit(as.character(qa), "")) == 1))-1))
  # Return a mask band image, giving the qa value.
  image$bitwiseAnd(qa)$lt(1)
}

# Using getQABits we construct a single-argument function 'mod13A2_clean'
mod13A2_clean <- function(img) {
  # Extract the NDVI band
  ndvi_values <- img$select("NDVI")
  
  # Extract the quality band
  ndvi_qa <- img$select("SummaryQA")
  
  # Select pixels to mask
  quality_mask <- getQABits(ndvi_qa, "11")
  
  # Mask pixels with value zero.
  ndvi_values$updateMask(quality_mask)
}

# Create a monthly composite
ndvi_composite <- modis_ndvi$
  filter(ee$Filter$date('2001-01-01', '2019-12-31'))$
  filter(ee$Filter$calendarRange(1, field = "month"))$
  map(mod13A2_clean)$
  median()

# Display results
scale <- 0.0001
Map$setCenter(lon = -79,lat = 35,zoom = 6)
Map$addLayer(
  eeObject = ndvi_composite,
  visParams = list(
    min = 0.2 / scale,
    max = 0.7 / scale,
    palette = cpt("grass_ndvi", 10)
  )
) + Map$addLayer(ee_roi)

# Download raster
ee_raster <- ee_as_raster(
  image = ndvi_composite,
  region = ee_roi$geometry(),
  dsn = "/home/aybarpc01/dd.tif",
  scale = 2000,
  via = "drive"
)

# ee_manage_cancel_all_running_task()
ndvi_mean_sf <- ee_extract(
  x = ndvi_composite,
  y = ee_roi,
  fun = ee$Reducer$mean(),
  scale = 2000,
  sf = TRUE
)


#########################################


library(rgee)
library(raster)
ee_Initialize()
ee_user_info()

## 1. Download small images
dem <- ee$Image("WWF/HydroSHEDS/03CONDEM")
roi <- ee$Geometry$Point(-73,-12)$buffer(1000)$bounds()
# TIP: Use the dsn argument to specify where to save the raster.
dem_cusco_raster <- ee_as_raster(dem, roi)
dem_cusco_stars <- ee_as_stars(dem, roi)

## 2. Download large images
dem <- ee$Image("WWF/HydroSHEDS/03CONDEM")
roi <- ee$Geometry$Point(-73,-12)$buffer(10**5)$bounds()
# Map$centerObject(roi)
# Map$addLayer(dem$clip(roi))

# You need to upload your Google Drive credentials, rgee will make
# it automatically for you if you forgot it!
ee_Initialize(drive = TRUE)

dem_cusco_raster <- ee_as_raster(image = dem, region = roi, via = "drive")
dem_cusco_stars <- ee_as_stars(image = dem, region = roi, via = "drive")

# Clean a Google Drive folder
ee_clean_container(name = "rgee_backup", type = "drive")

###############################################


library(stars)
library(rgee)
library(sf)

ee_Initialize(gcs = TRUE)
ee_user_info()

# 1. Convert a sf to ee$FeatureCollection (small sf objects)
x <- st_read(system.file("shape/nc.shp", package = "sf"))
ee_x <- sf_as_ee(x)

ee_x_bounds <- sf_as_ee(st_as_sfc(st_bbox(x)))
Map$centerObject(eeObject = ee_x_bounds)
Map$addLayer(ee_x)

# 2. Convert a sf to ee$FeatureCollection (large sf objects)
x <- st_read(system.file("shape/nc.shp", package = "sf"))
ee_x <- sf_as_ee(
  x = x,
  via = "gcs_to_asset",
  assetId = paste0(ee_get_assethome(),"/nc"),
  bucket = "rgee_dev",
  overwrite = TRUE
)
Map$addLayer(ee_x)

# 3. Convert a stars(raster) to ee$Image
tif <- system.file("tif/L7_ETMs.tif", package = "stars")
x <- read_stars(tif)
assetId <- sprintf("%s/%s",ee_get_assethome(),'stars_l7')
ee_raster_02 <- raster_as_ee(
  x = x,
  assetId = assetId,
  overwrite = TRUE,
  bucket = "rgee_dev"
)
Map$centerObject(ee_raster_02)
Map$addLayer(ee_raster_02)


# library(reticulate)
# library(rgee)
# use_condaenv("gee-demo", conda = "auto", required = TRUE)
# ee = import("ee")
# ee$Initialize()
# np = import("numpy")
# pd = import("pandas")
# geometry = ee$Geometry$Point(13.481643640792527,52.48959983479137);
# S2 = ee$ImageCollection("COPERNICUS/S2_SR")
# S2 = S2$filterDate(ee$Date('2019-05-01'), ee$Date('2019-08-01'))$filterBounds(geometry)
# nbrImages = S2$size()$getInfo()
# nbrImages

# 
# S2 = S2$sort("CLOUD_COVERAGE_ASSESSMENT")$first() # select the least cloudy image
# S2 = S2$select(list("B2", "B3", "B4", "B8"))
# 
# ndvi = S2$normalizedDifference(c('B8', 'B4'))     # get the NDVI as new layer
# ndvi = ndvi$multiply(1000)$clip(geometry$buffer(500))$rename("ndvi") # clip to the park area  
# 
# latlng = ee$Image$pixelLonLat()$addBands(S2)$addBands(ndvi)   # get selected bands and ndvi  
# latlng = latlng$reduceRegion(reducer   = ee$Reducer$toList(), 
#                              geometry  = geometry$buffer(500), 
#                              maxPixels = 1e8, 
#                              scale     = 10)
# 
# lats = np$array((ee$Array(latlng$get("latitude"))$getInfo()))    # convert lat lon values to numpy array
# lngs = np$array((ee$Array(latlng$get("longitude"))$getInfo()))
# 
# 
# ndvi_values = np$array((ee$Array(latlng$get("ndvi"))$getInfo())) # convert band values to numpy array
# B2_values   = np$array((ee$Array(latlng$get("B2"))$getInfo()))
# B3_values   = np$array((ee$Array(latlng$get("B3"))$getInfo()))
# B4_values   = np$array((ee$Array(latlng$get("B4"))$getInfo()))
# B8_values   = np$array((ee$Array(latlng$get("B8"))$getInfo()))
# 
# df <- data.frame(x = lngs, y = lats, ndvi = ndvi_values, B2 = B2_values, 
#                  B3 = B3_values, B4 = B4_values, B8 = B8_values)
# 
# library(raster)
# XYZ <- rasterFromXYZ(df)
# 
# plotRGB(XYZ, 5, 4, 3, interpolate = TRUE)
# plotRGB(XYZ, 4, 3, 2, interpolate = TRUE, stretch = "lin")
