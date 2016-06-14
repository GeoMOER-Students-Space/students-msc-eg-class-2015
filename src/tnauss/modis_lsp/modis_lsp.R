# Install MODIS package
# devtools::install_github("MatMatt/MODIS", ref = "develop")
# setRepositories() 
# install.packages(c(' mapdata', 'ptw '),dependencies=TRUE)

# Load libraries and define directories
library(rgdal)
library(MODIS)

filepath_base <- "/media/tnauss/myWork/analysis/moc_rs/data/"

localArcPath <- paste0(filepath_base, "modis/modis_arc/")
outDirPath <- paste0(filepath_base, "modis/processed/")

cropPath <-paste0(filepath_base, "modis/croped/")

shpPath <- paste0(filepath_base, "shp/")

rasterOptions(tmpdir=paste0(filepath_base, "tmp/")) 

# Set processing options
MODISoptions(localArcPath = localArcPath, outProj = "+init=epsg:32626",
             outDirPath = outDirPath, MODISserverOrder = c("LPDAAC", "LAADS"))
             # gdalPath = "C:/OSGeo4W64/bin")


# Download MODIS tiles
modis_product <- "MOD09A1"
job_name <- paste0(modis_product, "_CPV")

collection <- getCollection(modis_product, forceCheck = TRUE)

runGdal(modis_product, collection = collection, job = job_name, 
        tileH = 15, tileV = 7,
        begin = "2004274", end = "2005365")

# Post-processing
# Get country boundaries
# country_shp <- getData(country = "CPV", level = 0, path = shpPath)
country_shp <- readOGR(paste0(shpPath, "CAP_admin_UTM26_SHP/CAP_UTM26.shp"), 
                       layer = "CAP_UTM26")
projection(country_shp) <- 
  CRS("+proj=utm +zone=26 +ellps=WGS84 +datum=WGS84 +units=m +no_defs ")
country_shp <- spTransform(country_shp, CRS = CRS("+init=epsg:32626"))
# plot(country_shp)

modis_files <- list.files(paste0(getOption("MODIS_outDirPath"), "/", job_name),
                          pattern = glob2rx("*.tif"), full.names = TRUE)
modis_data <- stack(modis_files)
modis_data_crop <- crop(modis_data, country_shp)     
writeRaster(modis_data_crop, paste0(cropPath, "MODIS.tif"), bylayer = TRUE)


# lst <- lapply(c("NDVI", "VI_Quality"), function(i) {
#   fls <- list.files(paste0(getOption("MODIS_outDirPath"), "/MOD09A1_CPV"), 
#                     pattern = paste0(i, ".tif$"), full.names = TRUE)
#   rst <- stack(fls)
#   rst_crp <- crop(rst, shp)     
#   if (i == "NDVI") 
#     rst_crp <- rst_crp * 0.0001
#   return(rst_crp)
# })

# ## install and load 'MODIS' version 0.10-18 (assuming that the older version of 
# ## 'MODIS' is located inside your current working directory)
# detach("package:MODIS", unload = TRUE)
# install.packages("MODIS_0.10-18.tar.gz", repos = NULL, type = "source")
# library(MODIS)