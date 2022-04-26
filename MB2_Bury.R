#############################################################################################################

##                                     Vineyard Analysis Wuerzburg 2021 - Sentinel 2                       ##

#############################################################################################################
#
######## Final task for the "Introduction to Programming and Geostatistics - MB2 class".             ########
#
# R Script developed by: Andreas Bury #
# 
# Email: Andreas_Bury@hotmail.de #
# 




#############################################################################################################



# Load required Packages #

if(!require(rgdal)){
  install.packages("rgdal")
  library(rgdal)
}

if(!require(terra)){
  install.packages("terra")
  library(terra)
}

if(!require(sp)){
  install.packages("sp")
  library(sp)
}

if(!require(sf)){
  install.packages("sf")
  library(sf)
}


if(!require(dplyr)){
  install.packages("dplyr")
  library(dplyr)
}

if(!require(ggplot2)){
  install.packages("ggplot2")
  library(ggplot2)
}


#############################################################################################################

# Set Main Directory 

Main_Folder <-choose.dir(getwd(), "Choose your Main Data Folder")
setwd(Main_Folder)

## Create Data Folders

dir.create("01_Vector", showWarnings = FALSE)
Vec_Folder <- paste0(Main_Folder,"/01_Vector")

dir.create("02_Raster", showWarnings = FALSE)
Raster_Folder <- paste0(Main_Folder,"/02_Vector")


#############################################################################################################

# Download Data sets

# Predefined Shapefile of the Study Area

Study_Area <- file.path("./Vineyard.zip")

Shape_URL <- "https://github.com/Ioadidaiz/MB-2/raw/main/Vineyard.zip"


if (!file.exists(Study_Area)) {
  download.file(Shape_URL ,Study_Area, mode="wb")
  unzip(Study_Area, exdir = file.path("./01_Vector"),overwrite = TRUE)
} else {
  unzip(Study_Area, exdir = file.path("./01_Vector"),overwrite = TRUE)
}

#############################################################################################################

## Sentinel 2 Data



# Here we use Preprocessed Level 2A Sentinel Data with BOA Data

# Dateset Description: https://sentinels.copernicus.eu/web/sentinel/user-guides/sentinel-2-msi/product-types/level-2a

# The S2A Data was filtered for Area of Interest, Bands (4 and 8), Cloud Cover and Time span. This filtered data is then downloaded via Google Earth Engine 

# January     :   None (clouds)
# Februaray   :   COPERNICUS/S2_SR/20210213T103039_20210213T103035_T32UNA
# March       :   COPERNICUS/S2_SR/20210307T102021_20210307T102135_T32UNA
# April       :   COPERNICUS/S2_SR/20210426T102021_20210426T102624_T32UNA
# May         :   COPERNICUS/S2_SR/20210509T103021_20210509T103104_T32UNA
# June        :   COPERNICUS/S2_SR/20210613T102559_20210613T103528_T32UNA
# July        :   COPERNICUS/S2_SR/20210718T103031_20210718T103025_T32UNA
# August      :   COPERNICUS/S2_SR/20210814T102031_20210814T102609_T32UNA
# September   :   COPERNICUS/S2_SR/20210906T103021_20210906T103518_T32UNA
# October     :   None (clouds)
# November    :   None (clouds)
# December    :   None (clouds)


#############################################################################################################


# Download filtered Raster Data

S2A_Vineyards <- "https://github.com/Ioadidaiz/MB-2/raw/main/Raster.zip"

Raster <- file.path("./Raster.zip")


if (!file.exists(Raster)) {
  download.file(S2A_Vineyards ,Raster, mode="wb")
  unzip(Raster, exdir = file.path("./02_Raster"),overwrite = TRUE)
} else {
  unzip(Raster, exdir = file.path("./02_Raster"),overwrite = TRUE)
}


######################    clear Environment 

rm(S2A_Vineyards)
rm(Main_Folder)
rm(Raster)
rm(Raster_Folder)
rm(Shape_URL)
rm(Study_Area)
rm(Vec_Folder)

gc()

#############################################################################################################

# load Shapefile

Vineyard <- readOGR(dsn="01_Vector", layer="Vineyard")

# load S2A Raster

S2A_Feb <- rast("./02_Raster/S2A_13_02_21.tif")
S2A_Ma <- rast("./02_Raster/S2A_07_03_21.tif")
S2A_Apr <- rast("./02_Raster/S2A_26_04_21.tif")
S2A_May <- rast("./02_Raster/S2A_09_05_21.tif")
S2A_June <- rast("./02_Raster/S2A_13__06_21.tif")
S2A_July <- rast("./02_Raster/S2A_18_07_21.tif")
S2A_August <- rast("./02_Raster/S2A_14_08_21.tif")
S2A_Sept <- rast("./02_Raster/S2A_06_09_21.tif")


#############################################################################################################

# Function for NDVI 

NDVI <- function(img, NIR, RED) {
  BNIR <- img[[NIR]]
  BRED <- img[[RED]]
  NDVI <- (BNIR - BRED) / (BNIR + BRED)
  return(NDVI)
}


# Calc NDVI per Month 


## Feb 
NDVI_Feb <- NDVI(S2A_Feb, 2, 1)
plot(NDVI_Feb, col=rev(terrain.colors(10)), main = "Sentinel 2A - NDVI February")

### classify to NDVI values above 0.4 

NDVI_Feb_Veg <- classify(NDVI_Feb, cbind(-Inf, 0.4, NA))
plot(NDVI_Feb_Veg, main="Sentinel 2A - NDVI February - Vegetation")

## March

NDVI_March <- NDVI(S2A_Ma, 2, 1)
plot(NDVI_March, col=rev(terrain.colors(10)), main = "Sentinel 2A - NDVI March")

### classify to NDVI values above 0.4 

NDVI_March_Veg <- classify(NDVI_March, cbind(-Inf, 0.4, NA))
plot(NDVI_March_Veg, main="Sentinel 2A - NDVI March - Vegetation")


## April

NDVI_April <- NDVI(S2A_Apr, 2, 1)
plot(NDVI_April, col=rev(terrain.colors(10)), main = "Sentinel 2A - NDVI April")

### classify to NDVI values above 0.4 

NDVI_April_Veg <- classify(NDVI_April, cbind(-Inf, 0.4, NA))
plot(NDVI_April_Veg, main="Sentinel 2A - NDVI April - Vegetation")

## May

NDVI_May <- NDVI(S2A_May, 2, 1)
plot(NDVI_May, col=rev(terrain.colors(10)), main = "Sentinel 2A - NDVI May")

### classify to NDVI values above 0.4 

NDVI_May_Veg <- classify(NDVI_May, cbind(-Inf, 0.4, NA))
plot(NDVI_May_Veg, main="Sentinel 2A - NDVI May - Vegetation")


# June

NDVI_June <- NDVI(S2A_June, 2, 1)
plot(NDVI_June, col=rev(terrain.colors(10)), main = "Sentinel 2A - NDVI June")

### classify to NDVI values above 0.4 

NDVI_June_Veg <- classify(NDVI_June, cbind(-Inf, 0.4, NA))
plot(NDVI_June_Veg, main="Sentinel 2A - NDVI June - Vegetation")


# July

NDVI_July <- NDVI(S2A_July, 2, 1)
plot(NDVI_July, col=rev(terrain.colors(10)), main = "Sentinel 2A - NDVI July")

### classify to NDVI values above 0.4 

NDVI_July_Veg <- classify(NDVI_July, cbind(-Inf, 0.4, NA))
plot(NDVI_July_Veg, main="Sentinel 2A - NDVI July - Vegetation")


# August 

NDVI_August <- NDVI(S2A_August, 2, 1)
plot(NDVI_August, col=rev(terrain.colors(10)), main = "Sentinel 2A - NDVI August")

### classify to NDVI values above 0.4 

NDVI_August_Veg <- classify(NDVI_August, cbind(-Inf, 0.4, NA))
plot(NDVI_August_Veg, main="Sentinel 2A - NDVI August - Vegetation")


# September

NDVI_September <- NDVI(S2A_Sept, 2, 1)
plot(NDVI_September, col=rev(terrain.colors(10)), main = "Sentinel 2A - NDVI September")

### classify to NDVI values above 0.4 

NDVI_September_Veg <- classify(NDVI_September, cbind(-Inf, 0.4, NA))
plot(NDVI_September_Veg, main="Sentinel 2A - NDVI September - Vegetation")




######################    clear Environment 

rm(S2A_Feb)
rm(S2A_Ma)
rm(S2A_Apr)
rm(S2A_May)
rm(S2A_June)
rm(S2A_July)
rm(S2A_August)
rm(S2A_Sept)

gc()

#############################################################################################################

#### stack NDVI Raster


NDVI_R_2021 <- c(NDVI_Feb,NDVI_March,NDVI_April,NDVI_May,NDVI_June,NDVI_July,NDVI_August,NDVI_September)

#############################################################################################################

# Plot all NDVI_Months

plot(NDVI_R_2021)

#############################################################################################################

## Download Random Plots

Plots <- "https://github.com/Ioadidaiz/MB-2/raw/main/Plots.zip"

RandomPlots <- file.path("./Plots.zip")

# Download Data
if (!file.exists(RandomPlots)) {
  download.file(Plots ,RandomPlots, mode="wb")
  unzip(RandomPlots, exdir = file.path("./01_Vector"),overwrite = TRUE)
} else {
  unzip(RandomPlots, exdir = file.path("./01_Vector"),overwrite = TRUE)
}

#############################################################################################################

### load Plots

Plots <- st_read("./01_Vector/Plots.shp") 

#############################################################################################################

## Extract NDVI Values

NDVI_P_Feb    <- terra::extract(NDVI_Feb, vect(Plots))
NDVI_P_March  <- terra::extract(NDVI_March, vect(Plots))
NDVI_P_April  <- terra::extract(NDVI_April, vect(Plots))
NDVI_P_May    <- terra::extract(NDVI_May, vect(Plots))
NDVI_P_June   <- terra::extract(NDVI_June, vect(Plots))
NDVI_P_July   <- terra::extract(NDVI_July, vect(Plots))
NDVI_P_August <- terra::extract(NDVI_August, vect(Plots))
NDVI_P_Sept   <- terra::extract(NDVI_September, vect(Plots))

#############################################################################################################

# Combine extracted Values

NDVI_2021    <- cbind(NDVI_P_Feb,NDVI_P_March,NDVI_P_April,NDVI_P_May, NDVI_P_June,NDVI_P_July,NDVI_P_August, NDVI_P_Sept)

#############################################################################################################

######################    clear Environment 

rm(Plots)
rm(RandomPlots)


gc()


NDVI_2021_df <- as.data.frame(NDVI_2021)

# delete unused columns     

NDVI_2021_df <- NDVI_2021_df[,c(1,2,4,6,8,10,12,14,16)]

# rename columns

names(NDVI_2021_df) <- c("ID","NDVI_Feb","NDVI_March","NDVI_April","NDVI_May","NDVI_June","NDVI_July","NDVI_August","NDVI_September")


