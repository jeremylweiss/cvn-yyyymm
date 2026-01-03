

# Code downloads and save shapefiles of latest CPC monthly outlook for temperature and precipitation, then downloads monthly PRISM data and computes anomalies of current month of previous year versus 1991-2020 normals, and saves results as .RDS files
 
# !!! Set outlook month and year in PARAMETERS before running !!!

# TODO: Make a function for this


# SETUP --------------------


library(dplyr)
library(httr)
library(prism)
library(raster)
library(sf)
library(stars)

cpc30DayTextURL <- "https://www.cpc.ncep.noaa.gov/products/predictions/long_range/fxus07.html"
cpcPrcpFigURL <- "https://www.cpc.ncep.noaa.gov/products/predictions/long_range/lead14/off15_prcp.gif"
cpcTempFigURL <- "https://www.cpc.ncep.noaa.gov/products/predictions/long_range/lead14/off15_temp.gif"
cpcPrcpURL <- "https://ftp.cpc.ncep.noaa.gov/GIS/us_tempprcpfcst/monthlyupdate/monthupd_prcp_latest.zip"
cpcTempURL <- "https://ftp.cpc.ncep.noaa.gov/GIS/us_tempprcpfcst/monthlyupdate/monthupd_temp_latest.zip"

prism::prism_set_dl_dir("./outlook/prism-tmp", create = TRUE)

# For AVA-level statistics
azClip <- sf::st_read("./spatial-data/tl-2019-us-state-az-bndbox.shp")
azLatLon <- read.csv(file = "./spatial-data/prism-mesh-gcsna1983-az-bndbox.csv", header = TRUE)
azLon <- unique(azLatLon$x_center)
azLat <- sort(x = unique(azLatLon$y_center), decreasing = TRUE)


# PARAMETERS --------------------


# outlookMonth <- 12
# outlookYear <- 2024 # <YEAR - 1>

if (outlookMonth < 10) {
  outlookMonthText <- paste0("0", outlookMonth)
} else {
  as.character(outlookMonth)
}

# PRISM monthly variables, options are "ppt", "tmax", "tmean", "tmin", "vpdmax", and "vpdmin"
climVars <- c("ppt", "tmax", "tmean", "tmin")

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
  
  prismSummary$Year <- outlookYear
  prismSummary$Month <- outlookMonth
  prismSummary$Variable <- climVar
  prismSummary$AreaAverageValue <- rowMeans(prismExtract)
  prismSummary$AVA <- ava
  
  return(prismSummary)
}


# DOWNLOAD CPC FIGURES AND SHAPEFILES --------------------


cpc30DayText <- httr::content(httr::GET(cpc30DayTextURL), "text", encoding = "UTF-8")
writeLines(cpc30DayText, con = "./outlook/cpc30DayText.html")

download.file(url = cpcPrcpFigURL, destfile = "./outlook/off15_prcp.gif")
download.file(url = cpcTempFigURL, destfile = "./outlook/off15_temp.gif")

for(climVar in c("prcp", "temp")) {
  tf <- tempfile()
  
  if(climVar == "prcp") {
    download.file(url = cpcPrcpURL, destfile = tf)
    unzip(zipfile = tf, exdir = "./outlook/")
    unlink(tf)
  } else {
    download.file(url = cpcTempURL, destfile = tf)
    unzip(zipfile = tf, exdir = "./outlook/")
    unlink(tf)
  }
}
rm(climVar, tf)


# PRISM DATA ETL -------------------- 


for (cv in climVars) {
  print(paste0("Downloading and processing monthly data for: ", cv))
  flush.console()
  prism::get_prism_monthlys(
    type = cv,
    resolution = "4km",
    years = outlookYear,
    mon = outlookMonth,
    keepZip = FALSE
  )
  
  print(paste0("Downloading and processing normals data for: ", cv))
  flush.console()
  prism::get_prism_normals(
    type = cv,
    resolution = "4km",
    mon = outlookMonth,
    keepZip = FALSE
  )
}
rm(cv)

# Convert files to raster (monthly are .bil and normals are .tif at this time)
for (cv in climVars) {
  prismMonth <- 
    raster::raster(
      paste0(
        "./outlook/prism-tmp/", 
        "prism_", cv, "_us_25m_", outlookYear, outlookMonthText, "/",
        "prism_", cv, "_us_25m_", outlookYear, outlookMonthText, ".bil"
      )
    )
  
  prismNormal <- 
    raster::raster(
      paste0(
        "./outlook/prism-tmp/",
        "prism_", cv, "_us_25m_2020", outlookMonthText, "_avg_30y/",
        "prism_", cv, "_us_25m_2020", outlookMonthText, "_avg_30y.tif"
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


# AVA STATISTICS --------------------


for (ava in avas) {
  # Load latitude and longitude values for the AVA that correspond to gridcell centers of the 4-km PRISM grid mesh
  avaLatLon <- 
    read.csv(paste0("./spatial-data/prism-mesh-gcsna1983-", ava, ".csv"))
  
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

saveRDS(avaStatsMonth, file = "./outlook/avaStatsMonth.RDS")
saveRDS(avaStatsNormal, file = "./outlook/avaStatsNormal.RDS")
