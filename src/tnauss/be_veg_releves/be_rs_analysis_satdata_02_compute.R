# Compute set of independent variables from multiple satellite observations

library(tools)


# Set path ---------------------------------------------------------------------
if(Sys.info()["sysname"] == "Windows"){
  filepath_base <- "F:/analysis/moc_rs/"
} else {
  filepath_base <- "/media/tnauss/myWork/analysis/moc_rs/"
}

path_results <- paste0(filepath_base, "data/rdata/")
path_temp <- paste0(filepath_base, "data/temp/")

rasterOptions(tmpdir= path_temp)


# Read satdata -----------------------------------------------------------------
files_data <- list.files(path_results, full.names = TRUE, recursive = TRUE,
                      pattern = glob2rx("*.RData"))
be_id <- c("alb", "hai", "sch")
files_data <- files_data[apply(sapply(be_id, grepl, files_data), 1, any)]

for(f in files_data){
  load(f)
  assign(file_path_sans_ext(basename(f)), sat_vals)
}
rm(sat_vals)



