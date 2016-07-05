# Extract windows arround observations plots from satellite observations

library(raster)
library(rgdal)
library(sp)
library(tools)


# Set path ---------------------------------------------------------------------
if(Sys.info()["sysname"] == "Windows"){
  filepath_base <- "F:/analysis/moc_rs/"
} else {
  filepath_base <- "/media/tnauss/myWork/analysis/moc_rs/"
}

path_landsat <- paste0(filepath_base, "data/Landsat_AtmCorr_PanSharp/")
path_rapideye <- paste0(filepath_base, "data/rapideye/")
path_results <- paste0(filepath_base, "data/rdata/")
path_plots <- paste0(filepath_base, "data/plots/")
path_temp <- paste0(filepath_base, "data/temp/")
path_source <- paste0(filepath_base, "msc-c2015/src/tnauss/be_veg_releves/")

rasterOptions(tmpdir= path_temp)


source(paste0(path_source, "be_rs_analysis_satdata_functions.R"))


# Read plot geodata ------------------------------------------------------------
plt_alb <- readOGR(paste0(path_plots, "polyAlbEp.shp"), layer = "polyAlbEp")
plt_hai <- readOGR(paste0(path_plots, "polyHaiEp.shp"), layer = "polyHaiEp")
plt_sch <- readOGR(paste0(path_plots, "polySchEp.shp"), layer = "polySchEp")
plt <- list(alb = plt_alb, hai = plt_hai, sch = plt_sch)
plt <- lapply(plt, function(x){
  x@data$EP <- as.character(x@data$EP)
  x@data$EP[nchar(x@data$EP) == 4] <- paste0(
    substr(x@data$EP[nchar(x@data$EP) == 4], 1, 3), "0", 
    substr(x@data$EP[nchar(x@data$EP) == 4], 4, 4))
  return(x)
})


# Get filenames of satellite datasets ------------------------------------------
# Use filenames (stacks) for RapidEye
list.re <- list.files(path_rapideye, full.names = TRUE, recursive = TRUE,
                      pattern = glob2rx("*ortho.tif"))
be_id <- c("alb", "hai", "sch")

# Use path (individual layers) for Landsat
list.ls <- unique(dirname(list.files(path_landsat, full.names = TRUE, recursive = TRUE,
                                     pattern = glob2rx("*AtmosCorr_PAN_sharpend.tif"))))
be_id_ls <- c("Alb", "Hai", "Cho")

files <- append(list.re, list.ls)


# Extract values within window -------------------------------------------------
for(file in files){
  # Cases for RapidEye and Landsat
  if(grepl("rapideye", file)){
    sat <- stack(file)
    names(sat) <- paste0("B00", seq(5))
    be <- be_id[sapply(be_id, grepl, file)]
    date_pos <- c(1, 10)
    sat_name <- "re"
    date <- substr(file_path_sans_ext(basename(file)), date_pos[1], date_pos[2])
  } else {
    stack_files <- list.files(file, full.names = TRUE,
                              pattern = glob2rx("*AtmosCorr_PAN_sharpend.tif"))
    sat <- stack(stack_files)
    names(sat) <- substr(names(sat), 5, 8)
    be <- be_id[sapply(be_id_ls, grepl, file)]
    date_pos <- c(10, 16)
    sat_name = "ls"
    date <- substr(file_path_sans_ext(basename(file)), date_pos[1], date_pos[2])
    date <- as.character(strptime(date, "%Y %j"))
  }

  plots <- plt[[be]]
  plots <- plots[grepl("EG", plots@data$EP), ]
  plots <- spTransform(plots, CRS(projection(sat)))
  centroids <- extract(sat, coordinates(plots), cellnumbers = TRUE,
                       layer = 1, nl = 1, df = TRUE)
  centroids$EPID <- plots@data$EP
  sat_vals <- satdata_windows(data = sat, center = centroids$cells, 
                              cnames = centroids$EPID, size = 7)
  
  filepath_out <- paste0(path_results, be, "_", sat_name, "_", date, ".RData")
  save(sat_vals, file = filepath_out)
}


