# Compute set of independent variables from multiple satellite observations

library(glcm)
library(tools)


# Set path ---------------------------------------------------------------------
if(Sys.info()["sysname"] == "Windows"){
  filepath_base <- "F:/analysis/moc_rs/"
} else {
  filepath_base <- "/media/tnauss/myWork/analysis/moc_rs/"
}

path_results <- paste0(filepath_base, "data/rdata/")
path_temp <- paste0(filepath_base, "data/temp/")
path_source <- paste0(filepath_base, "msc-c2015/src/tnauss/be_veg_releves/")

rasterOptions(tmpdir= path_temp)


source(paste0(path_source, "be_rs_analysis_satdata_functions.R"))

# Read satdata -----------------------------------------------------------------
files_data <- list.files(path_results, full.names = TRUE, recursive = TRUE,
                      pattern = glob2rx("*.RData"))
be_id <- c("alb", "hai", "sch")
files_data <- files_data[apply(sapply(be_id, grepl, files_data), 1, any) &
                           !apply(sapply("specdiv", grepl, files_data), 1, any)]

for(f in files_data){
  load(f)
  if(grepl("_re_", f)){
    sat_name <- "re"
    date <- file_path_sans_ext(basename(f))
    date <- substr(date, nchar(date)-9, nchar(date))
    bands <- data.frame(blue = 1, green = 2, red = 3, 
                        nir = 4, swir1 = 5)
  } else {
    sat_name <- "ls"
    bands <- data.frame(ca = 1, blue = 2, green = 3, red = 4, 
                        nir = 5, swir1 = 6, swir2 = 7,
                        pan = 8, cir = 9)
    date <- file_path_sans_ext(basename(f))
    date <- substr(date, nchar(date)-9, nchar(date))
  }
  vals <- specdiv(data = sat_vals, date = date, sat_name = sat_name)
  filepath_out <- paste0(path_results, file_path_sans_ext(basename(f)), 
                         "_specdiv.RData")
  save(vals, file = filepath_out)
}
