# Predict in-situ observations using satellite data
library(gpm)

# Set path ---------------------------------------------------------------------
if(Sys.info()["sysname"] == "Windows"){
  filepath_base <- "F:/analysis/moc_rs/"
} else {
  filepath_base <- "/media/tnauss/myWork/analysis/moc_rs/"
  filepath_base <- "/media/dogbert/myWork/analysis/moc_rs/"
}

path_results <- paste0(filepath_base, "data/rdata/")
path_temp <- paste0(filepath_base, "data/temp/")
path_source <- paste0(filepath_base, "msc-c2015/src/tnauss/be_veg_releves/")
filepath_results <- paste0(filepath_base, "data/rdata/")

source(paste0(path_source, "be_rs_analysis_functions.R"))

# Read combined dataset from script be_rs_analysis_04_combine.R ----------------
load(paste0(path_results, "be_sat_veg.RData"))



# Prepare gpm object -----------------------------------------------------------
# Use maximum NDVI until July 2014 to identify the respective scenes.
# dataset <- sat_veg[sat_veg$sensor == "re", ]
# dataset <- maxndvi(data = dataset, date = c("2014-01-01", "2014-06-01"))

response <- "specrich"
dataset <- sat_veg[sat_veg$sensor == "ls", ]
# dataset <- sat_veg[sat_veg$sensor == "re", ]
# dataset <- maxndvi(data = dataset, date = list(c("2014-01-01", "2014-05-01"),
#                                                c("2015-01-01", "2015-05-01")))
dataset <- maxndvi(data = dataset, date = list(c("2014-01-01", "2014-05-01")))
dataset <- dataset[!is.na(dataset[, response]),]



# GPM --------------------------------------------------------------------------
response <- response

col_biodiv <- c(122:124, 131:141)
col_sat <- c(3:118)
col_meta <- seq(ncol(dataset))[!seq(ncol(dataset)) %in% c(col_biodiv, col_sat)]
              
meta <- createGPMMeta(dataset, type = "input",
                      selector = 1, response = col_biodiv, 
                      independent = col_sat, meta = col_meta)
obsv <- gpm(dataset, meta, scale = TRUE)


# Compile model evaluation dataset ---------------------------------------------
selid <- obsv@data$input$epid
# selid <- paste(obsv@data$input$epid, obsv@data$input$year, sep = "_")
obsv_resamples <- resamplingsByVariable(x = obsv@data$input, 
                                        selector = selid, 
                                        grabs = 1,
                                        resample = 10)


# Split dataset into testing and training samples for each individual species --
obsv_trte <- splitMultResp(x = obsv@data$input, 
                           response = response,
                           resamples = obsv_resamples,
                           p = 0.75)

# obsv_trte <- splitMultResp(x = obsv@data$input, 
#                            response = response,
#                            resamples = obsv_resamples,
#                            p = 0.75,
#                            selector = "year")

# Evaluate prediction models ---------------------------------------------------
independent <- obsv@meta$input$INDEPENDENT

independent_3x3 <- obsv@meta$input$INDEPENDENT[grep("3x3", independent)]

independent_ls <- c("tvi_movwin_med_3x3", "msavi_movwin_med_7x7", 
                 "rvi_movwin_med_3x3", "ndvi_movwin_med_3x3", 
                 "ndvi_movwin_med_7x7")

independent_re <- c("mtvi_movwin_medsd_5x5", "mtvi_movwin_med_5x5", 
                 "mtvi_movwin_med_7x7", "rvi_movwin_medsd_5x5", 
                 "mtvi_movwin_med_3x3")

independent <- independent_ls
independent <- independent_3x3

n_vars <- c(seq(length(independent)))

models_gam <- trainModel(x = obsv, mode = "ffs",
                         response = response, independent = independent,
                         resamples = obsv_trte, n_var = n_vars,
                         mthd = "gam", seed_nbr = 11, cv_nbr = 5,
                         var_selection = "sd",
                         filepath_tmp = filepath_results)


models_rf <- trainModel(x = obsv, mode = "ffs",
                         response = response, independent = independent,
                         resamples = obsv_trte, n_var = n_vars,
                         mthd = "rf", seed_nbr = 11, cv_nbr = 5,
                         var_selection = "sd",
                         filepath_tmp = filepath_results)

save(models_gam, file = paste0(path_results, "models_gam_ls.RData"))
save(models_rf, file = paste0(path_results, "models_rf_ls.RData"))

# load(paste0(filepath_results, "models_gam_ls.RData"))

models <- models_gam
models <- models_rf
test <- compRegrTests(models, per_model = TRUE)
boxplot(unique(test$r_squared))
mean(aggregate(test$r_squared, by = list(test$sample), FUN = "mean")$x)

lapply(models, function(m){
  resamples <- lapply(m, function(r){
    r$model$finalModel$xNames
  })  
  return(do.call("rbind", resamples))
})


# models_pls <- trainModel(x = obsv, mode = "ffs",
#                          response = response, independent = independent,
#                          resamples = obsv_trte, n_var = n_vars,
#                          mthd = "pls", seed_nbr = 11, cv_nbr = 5,
#                          var_selection = "sd",
#                          filepath_tmp = filepath_results)
# 
# 
# models <- trainModel(x = obsv,
#                      response = response, independent = independent,
#                      resamples = obsv_trte, n_var = n_vars,
#                      mthd = "rf", seed_nbr = 11, cv_nbr = 5,
#                      var_selection = "sd",
#                      filepath_tmp = filepath_results)
# 
# models <- models_gam
# test <- compRegrTests(models, per_model = TRUE)
# unique(test$r_squared)
# mean(aggregate(test$r_squared, by = list(test$sample), FUN = "mean")$x)
# 
# 
# 
var_imp <- compVarImp(models, scale = FALSE)

var_imp_scale <- compVarImp(models, scale = TRUE)

var_imp_plot <- plotVarImp(var_imp_scale)
# 
# var_imp_heat <- plotVarImpHeatmap(var_imp_scale, xlab = "Species", ylab = "Band")
# 
# tstat <- compContTests(models, mean = TRUE)
# 
# tstat_mean <- merge(tstat[[1]], prevalence, by.x = "Response", by.y = "RESPONSE")
# 
# tstat_mean[order(tstat_mean$Kappa_mean, decreasing = TRUE),]
# 
# ggplot(data = tstat_mean, aes(x = OCCURENCE, y = Kappa_mean)) + geom_point() + geom_smooth()
# 
# 
# 
# 
