# Combine satellite and in-situ observations


# Set path ---------------------------------------------------------------------
if(Sys.info()["sysname"] == "Windows"){
  filepath_base <- "F:/analysis/moc_rs/"
} else {
  filepath_base <- "/media/tnauss/myWork/analysis/moc_rs/"
}

path_results <- paste0(filepath_base, "data/rdata/")
path_temp <- paste0(filepath_base, "data/temp/")
path_source <- paste0(filepath_base, "msc-c2015/src/tnauss/be_veg_releves/")

# Read satdata -----------------------------------------------------------------
files_data <- list.files(path_results, full.names = TRUE, recursive = TRUE,
                      pattern = glob2rx("*specdiv.RData"))
sat_data <- lapply(files_data, function(f){
  load(f)
  return(vals)
})
sat_data <- do.call("rbind", sat_data)


# Merge with in-situ observations from script be_rs_analysis_03_veg_releves.R --
load(paste0(path_results, "be_veg_releves.RData"))

sat_data$year <- substr(sat_data$date, 1, 4)

sat_veg <- merge(sat_data, veg_2014_2015, by = c("epid", "year"))

save(sat_veg, file = paste0(path_results, "be_sat_veg.RData"))
