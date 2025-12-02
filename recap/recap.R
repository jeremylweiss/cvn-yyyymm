

# Code downloads monthly PRISM data and computes anomalies of previous month of current year versus 1991-2020 normals, and saves results as .RDS files

# !!! Review recap month and year, AZMet start and end dates in PARAMETERS before running !!!

# TODO: Make a function for this


# SETUP --------------------


library(dplyr)
library(prism)
library(raster)
library(sf)
library(stars)

prism::prism_set_dl_dir("./prism-tmp", create = TRUE)

# For AVA-level statistics
azClip <- sf::st_read("./spatial-data-layers/tl-2019-us-state-az-bndbox.shp")
azLatLon <- read.csv(file = "./spatial-data-layers/prism-mesh-gcsna1983-az-bndbox.csv", header = TRUE)
azLon <- unique(azLatLon$x_center)
azLat <- sort(x = unique(azLatLon$y_center), decreasing = TRUE)


# PARAMETERS --------------------


recapMonth <- 10
recapYear <- 2025

# PRISM monthly variables, options are "ppt", "tmax", "tmean", "tmin", "vpdmax", and "vpdmin"
climVars <- c("ppt", "tmean", "tmax", "tmin")

avas <- c("sonoita-ava", "verde-valley-ava", "willcox-ava")


# FUNCTIONS --------------------


# c2f: convert degrees Celsius to degrees Fahrenheit -----
#' @param: valueIn - variable with units of Celsius
#' @return: valueOut - variable with units of Fahrenheit

c2f <- function(valueIn) {
  valueOut <- (valueIn * (9/5)) + 32
  return(valueOut)
}

# extractPrism: extract data from a PRISM raster for an Arizona AVA -----
#' @param: avaName - name of Arizona AVA in analysis
#' @param: prismRaster - individual PRISM raster of one climate variable
#' @return: prismExtract - dataframe with PRISM data for AVA gridcells

extractPrism <- function(avaName, prismRaster) {
  # Initialize a dataframe that takes values for each PRISM gridcell within the AVA
  prismExtract <- 
    as.data.frame(matrix(data = NA, nrow = 1, ncol = nrow(avaLatLon)))
  
  for (gridcell in 1:nrow(avaLatLon)) {
    print(gridcell)
    flush.console()
    
    prismExtract[, gridcell] <- as.numeric(prismRaster[ 
      # Find matching latitude between AVA and state values
      which(azLat == avaLatLon$y_center[gridcell], arr.ind = FALSE),
      # Find matching longitude between AVA and state values
      which(azLon == avaLatLon$x_center[gridcell], arr.ind = FALSE)
    ])
    
    # Set column name to latitude and longitude values of current gridcell
    gridcellColname <- 
      paste(
        as.character(avaLatLon$y_center[gridcell]),
        as.character(avaLatLon$x_center[gridcell]),
        sep = "-"
      )
    
    # Iteratively build list of column names
    if (gridcell == 1) {
      gridcellColnameList <- gridcellColname
    } else {
      gridcellColnameList <- c(gridcellColnameList, gridcellColname)
    }
    
    # Write column names after data for all gridcells has been extracted
    if (gridcell == nrow(avaLatLon)) {
      colnames(prismExtract) <- gridcellColnameList
    }
    
    rm(gridcellColname)
  }
  
  return(prismExtract)
}

# mm2in: convert millimeters to inches -----
#' @param: valueIn - variable with units of millimeters
#' @return: valueOut - variable with units of inches

mm2in <- function(valueIn) {
  valueOut <- valueIn / 25.4
  return(valueOut)
}

# summarizePrism: statistical summaries of PRISM data for an Arizona AVA -----
#' @param: climVar - PRISM climate variable
#' @param: prismExtract - dataframe from funtion 'extractPrism'
#' @return: prismSummary - dataframe with data summaries, other attributes

summarizePrism <- function(climVar, prismExtract) {
  # Initialize a dataframe that takes values of year, month, variable, 
  # area-average, normal, and AVA name
  prismSummary <- as.data.frame(matrix(data = NA, nrow = 1, ncol = 5))
  colnames(prismSummary) <- 
    c("Year", "Month", "Variable", "AreaAverageValue", "AVA")
  
  prismSummary$Year <- recapYear
  prismSummary$Month <- recapMonth
  prismSummary$Variable <- climVar
  prismSummary$AreaAverageValue <- rowMeans(prismExtract)
  prismSummary$AVA <- ava
  
  return(prismSummary)
}


# PRISM DATA ETL --------------------


for (cv in climVars) {
  print(paste0("Downloading and processing monthly data for: ", cv))
  flush.console()
  prism::get_prism_monthlys(
    type = cv,
    resolution = "4km",
    years = recapYear,
    mon = recapMonth,
    keepZip = FALSE
  )
  
  print(paste0("Downloading and processing normals data for: ", cv))
  flush.console()
  prism::get_prism_normals(
    type = cv,
    resolution = "4km",
    mon = recapMonth,
    keepZip = FALSE
  )
}
rm(cv)

# Convert files to raster (monthly are .bil and normals are .tif at this time)
for (cv in climVars) {
  prismMonth <- 
    raster::raster(
      paste0(
        "./prism-tmp/", 
        "prism_", cv, "_us_25m_", recapYear, recapMonth, "/",
        "prism_", cv, "_us_25m_", recapYear, recapMonth, ".bil"
      )
    )
    
  prismNormal <- 
    raster::raster(
      paste0(
        "./prism-tmp/",
        "prism_", cv, "_us_25m_2020", recapMonth, "_avg_30y/",
        "prism_", cv, "_us_25m_2020", recapMonth, "_avg_30y.tif"
      )
    )
    
  if (cv == "ppt") {
    pptMonth <- prismMonth
    pptNormal <- prismNormal
  } else if (cv == "tmax") {
    tmaxMonth <- prismMonth
    tmaxNormal <- prismNormal
  } else if (cv == "tmean") {
    tmeanMonth <- prismMonth
    tmeanNormal <- prismNormal
  } else if (cv == "tmin") {
    tminMonth <- prismMonth
    tminNormal <- prismNormal
  }
}
rm(cv, prismMonth, prismNormal)

# Convert units, compute anomalies (these are nation-wide, full PRISM rasters)
pptMonth <- mm2in(pptMonth)
pptNormal <- mm2in(raster::reclassify(pptNormal, cbind(0, 0.0001))) # Can't divide by zero
pptPercentNormal <- (pptMonth / pptNormal) * 100

tmaxMonth <- c2f(tmaxMonth)
tmaxNormal <- c2f(tmaxNormal)
tmaxDepartureNormal <- tmaxMonth - tmaxNormal

tmeanMonth <- c2f(tmeanMonth)
tmeanNormal <- c2f(tmeanNormal)
tmeanDepartureNormal <- tmeanMonth - tmeanNormal

tminMonth <- c2f(tminMonth)
tminNormal <- c2f(tminNormal)
tminDepartureNormal <- tminMonth - tminNormal


# SHAPEFILE EXPORT --------------------


# https://r-spatial.github.io/stars/articles/stars5.html
# https://datacarpentry.org/semester-biology/materials/spatial-data-saving-R/
# https://datacarpentry.org/semester-biology/materials/spatial-data-cropping-R/
boundingBox <- 
  sf::st_bbox(
    c(xmin = -119.0, ymin = 30.0, xmax = -106.0, ymax = 40.0), 
    crs = sf::st_crs(pptMonth)
  )

pptPercentNormalShp <- 
  sf::st_crop(stars::st_as_stars(pptPercentNormal), boundingBox)

pptPercentNormalShp <- pptPercentNormalShp %>%
  sf::st_as_sf(as_points = FALSE, merge = TRUE)

sf::st_write(pptPercentNormalShp, "./spatial-data-layers/pptPercentNormal.shp")

tmeanDepartureNormalShp <- 
  sf::st_crop(stars::st_as_stars(tmeanDepartureNormal), boundingBox)

tmeanDepartureNormalShp <- tmeanDepartureNormalShp %>%
  sf::st_as_sf(as_points = FALSE, merge = TRUE)

sf::st_write(tmeanDepartureNormalShp, "./spatial-data-layers/tmeanDepartureNormal.shp")


# AVA STATISTICS --------------------


for (ava in avas) {
  # Load latitude and longitude values for the AVA that correspond to gridcell centers of the 4-km PRISM grid mesh
  avaLatLon <- 
    read.csv(paste0("./spatial-data-layers/prism-mesh-gcsna1983-", ava, ".csv"))
  
  for (cv in climVars) {
    print(paste0("Starting summaries for: ", ava, " and ", cv))
    flush.console()
    
    if (cv == "ppt") {
      prismRasterMonth <- raster::crop(x = pptMonth, y = azClip)
      prismRasterNormal <- raster::crop(x = pptNormal, y = azClip)
    } else if (cv == "tmax") {
      prismRasterMonth <- raster::crop(x = tmaxMonth, y = azClip)
      prismRasterNormal <- raster::crop(x = tmaxNormal, y = azClip)
    } else if (cv == "tmean") {
      prismRasterMonth <- raster::crop(x = tmeanMonth, y = azClip)
      prismRasterNormal <- raster::crop(x = tmeanNormal, y = azClip)
    } else if (cv == "tmin") {
      prismRasterMonth <- raster::crop(x = tminMonth, y = azClip)
      prismRasterNormal <- raster::crop(x = tminNormal, y = azClip)
    }
    
    if (exists("avaStatsMonth") == FALSE) {
      avaStatsMonth <- 
        summarizePrism(
          climVar = cv, 
          prismExtract = extractPrism(avaName = ava, prismRaster = prismRasterMonth)
        )
    } else {
      avaStatsMonth <- 
        rbind(
          avaStatsMonth, 
          summarizePrism(
            climVar = cv, 
            prismExtract = extractPrism(avaName = ava, prismRaster = prismRasterMonth)
          )
        )
    }
    
    if (exists("avaStatsNormal") == FALSE) {
      avaStatsNormal <- 
        summarizePrism(
          climVar = cv, 
          prismExtract = extractPrism(avaName = ava, prismRaster = prismRasterNormal)
        )
    } else {
      avaStatsNormal <- 
        rbind(
          avaStatsNormal, 
          summarizePrism(
            climVar = cv, 
            prismExtract = extractPrism(avaName = ava, prismRaster = prismRasterNormal)
          )
        )
    }
  }
}
rm(ava)

dplyr::as_tibble(avaStatsMonth)
dplyr::as_tibble(avaStatsNormal)

saveRDS(avaStatsMonth, file = "avaStatsMonth.RDS")
saveRDS(avaStatsNormal, file = "avaStatsNormal.RDS")
