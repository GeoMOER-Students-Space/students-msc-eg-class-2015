# Predict in-situ observations using satellite data
library(gpm)

# Set path ---------------------------------------------------------------------
if(Sys.info()["sysname"] == "Windows"){
  filepath_base <- "F:/analysis/moc_rs/"
} else {
  filepath_base <- "/media/tnauss/myWork/analysis/moc_rs/"
  filepath_base <- "/media/dogbert/myWork/analysis/moc_rs/"
}

path_results <- paste0(filepath_base, "islands/rdata/")
path_temp <- paste0(filepath_base, "data/temp/")
path_source <- paste0(filepath_base, "msc-c2015/src/tnauss/be_veg_releves/")
filepath_results <- paste0(filepath_base, "islands/rdata/")

source(paste0(path_source, "be_rs_analysis_functions.R"))

# Read combined dataset from MODIS and biodiv data -----------------------------
load(paste0(path_results, "MOD09A1_2014001_combined.RData"))

mi <- mi[mi$Island != "Corvo" & mi$Island != "Sao_Miguel", ]

ggplot(data = mi, aes(x = sd_NDVI_modis, y = species_rich)) + 
  geom_point() + 
  geom_smooth()

plot(mi$species_rich, mi$sd_NDVI_modis)
linm <- lm(mi$species_rich ~ mi$sd_NDVI_modis)
summary(linm)



# GPM --------------------------------------------------------------------------
names(mi)
col_dep <- c(11:13, 219:392)
col_indp <- c(3:10, 14:218)
col_meta <- seq(ncol(mi))[!seq(ncol(mi)) %in% c(col_dep, col_indp)]
              
meta <- createGPMMeta(mi, type = "input",
                      selector = 2, response = col_dep, 
                      independent = col_indp, meta = col_meta)
obsv <- gpm(mi, meta, scale = TRUE)

response <- "species_rich"

# Compile model evaluation dataset ---------------------------------------------
selid <- obsv@data$input$Island
obsv_resamples <- resamplingsByVariable(x = obsv@data$input, 
                                        selector = selid, 
                                        grabs = 1,
                                        resample = 3)


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

independent <- c("area", "sd_PC1_modis", "sd_PC2_modis", "mean_PC1_modis", 
                 "mean_PC2_modis", "sd_NDVI_modis", "mean_NDVI_modis")

n_vars <- c(seq(length(independent)))


pls_area <- lapply(obsv_trte[[1]], function(x){
  r <- obsv@data$input[x$training$SAMPLES, response]
  i <- obsv@data$input[x$training$SAMPLES, "area"]
  m <- plsr(r ~ i)
  p <- predict(m, data.frame(i = obsv@data$input[x$testing$SAMPLES, "area"]))
  d <- data.frame(p = p,
             r = obsv@data$input[x$testing$SAMPLES, response])
  return(d)
})

pls_area_r2 <- lapply(pls_area, function(x){
  summary(lm(x[,1]~x[,2]))$r.squared
})
mean(unlist(pls_area_r2))

models_pls <- trainModel(x = obsv, mode = "ffs",
                         response = response, independent = independent,
                         resamples = obsv_trte, n_var = n_vars,
                         mthd = "pls", seed_nbr = 11, cv_nbr = 5,
                         var_selection = "sd",
                         filepath_tmp = filepath_results)

models_rf <- trainModel(x = obsv, mode = "ffs",
                         response = response, independent = independent,
                         resamples = obsv_trte, n_var = n_vars,
                         mthd = "rf", seed_nbr = 11, cv_nbr = 5,
                         var_selection = "sd",
                         filepath_tmp = filepath_results)

# save(models_pls, file = paste0(path_results, "models_pls.RData"))
# save(models_rf, file = paste0(path_results, "models_rf.RData"))

# load(paste0(filepath_results, "models_gam_ls.RData"))

models <- models_pls
models <- models_rf
lapply(models[[1]], function(x){
  x$model$finalModel$xNames
})
test <- compRegrTests(models, per_model = TRUE)
unique(test$r_squared)
boxplot(unique(test$r_squared))
mean(aggregate(test$r_squared, by = list(test$sample), FUN = "mean")$x)


comb <- melt(data.frame("area" = unlist(pls_area_r2),
                   "area_plus" = unique(test$r_squared)))

ggplot(data=comb, aes(x = variable, y = value, fill = variable)) + 
  geom_boxplot(notch = FALSE)

lapply(models, function(m){
  resamples <- lapply(m, function(r){
    r$model$finalModel$xNames
  })  
  return(do.call("rbind", resamples))
})

# var_imp <- compVarImp(models, scale = FALSE)
# 
# var_imp_scale <- compVarImp(models, scale = TRUE)
# 
# var_imp_plot <- plotVarImp(var_imp_scale)
# 
# var_imp_heat <- plotVarImpHeatmap(var_imp_scale, xlab = "Species", ylab = "Band")
