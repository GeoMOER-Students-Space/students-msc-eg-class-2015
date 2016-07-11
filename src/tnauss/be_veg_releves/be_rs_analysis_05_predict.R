# Predict in-situ observations using satellite data
library(gpm)

# Set path ---------------------------------------------------------------------
if(Sys.info()["sysname"] == "Windows"){
  filepath_base <- "F:/analysis/moc_rs/"
} else {
  filepath_base <- "/media/tnauss/myWork/analysis/moc_rs/"
}

path_results <- paste0(filepath_base, "data/rdata/")
path_temp <- paste0(filepath_base, "data/temp/")
path_source <- paste0(filepath_base, "msc-c2015/src/tnauss/be_veg_releves/")

source(paste0(path_source, "be_rs_analysis_functions.R"))

# Read combined dataset from script be_rs_analysis_04_combine.R ----------------
load(paste0(path_results, "be_sat_veg.RData"))



# Prepare gpm object -----------------------------------------------------------
# Use maximum NDVI until July 2014 to identify the respective scenes.
re_veg_2014_ndvi_max <- sat_veg[sat_veg$sensor == "re", ]
re_veg_2014_ndvi_max <- maxndvi(data = re_veg_2014_ndvi_max, date = c("2014-01-01", "2014-06-01"))








ggplot(data = re_veg_2014_ndvi_max, 
       aes(x = shannon, y = ndvi_movwin_sdv_7x7)) + 
  geom_point(size = 4) + 
  geom_smooth() + 
  theme_bw() +
  theme(axis.title = element_text(size = 30), axis.text = element_text(size = 22)) + 
  labs(x = "Shannon index, vasular plants", y = "Standard deviation of NDVI") 

ggplot(data = re_veg_2014_ndvi_max, 
       aes(x = shannon, y = mal_base_movwin_med_7x7)) + 
  geom_point(size = 4) + 
  geom_smooth() + 
  theme_bw() +
  theme(axis.title = element_text(size = 30), axis.text = element_text(size = 22)) + 
  labs(x = "Shannon index, vasular plants", y = "Median of Mahalanobis distance")




tune_gam = list(fncs = gamFuncs,
                tunegr = expand.grid(.method = c("GCV.Cp", "GACV.Cp", "REML",
                                                 "P-REML", "ML", "P-ML"),
                                     .select = c(TRUE, FALSE)))

tunegrid <- tune_gam$tunegr
mthd <- "gam"
dep <- "shannon"
indp <- c("mal_base_movwin_med_7x7", "ndvi_movwin_sdv_7x7")


set.seed(11)
train <- createDataPartition(re_veg_2014_ndvi_max[, dep], p = 0.90,
                             list = FALSE,
                             times = 5)
train

train_ctrl <- trainControl(method = "cv", number = 10, p = 0.90)

model_perf <- lapply(seq(ncol(train)), function(x){
  print(x)
  model <- train(re_veg_2014_ndvi_max[train[, x], indp],
                 re_veg_2014_ndvi_max[train[, x], dep],
                 method=mthd,
                 trControl=train_ctrl,
                 tuneGrid = tunegrid)
  pred <- predict(model, re_veg_2014_ndvi_max[-train[, x], indp])
  test <- re_veg_2014_ndvi_max[-train[, x], dep]
  error <- postResample(pred, test)
  data.frame(pred = pred,
             test = test,
             rmse = error[1],
             rsqrd = error[2])
})
model_perf <- do.call("rbind", model_perf)
unique(model_perf$rsqrd)
mean(unique(model_perf$rsqrd))




















response <- "specrich"

col_biodiv <- c(122:124, 131:141)
col_sat <- c(3:118)
col_meta <- seq(ncol(re_veg_2014_ndvi_max))[!seq(ncol(re_veg_2014_ndvi_max)) %in% c(col_biodiv, col_sat)]
              
meta <- createGPMMeta(re_veg_2014_ndvi_max, type = "input",
                      selector = 1, response = col_biodiv, 
                      independent = col_sat, meta = col_meta)
obsv <- gpm(re_veg_2014_ndvi_max, meta, scale = TRUE)


# Compile model evaluation dataset ---------------------------------------------
epid <- obsv@data$input$epid
obsv_resamples <- resamplingsByVariable(x = obsv@data$input, 
                                        selector = epid, 
                                        grabs = 1,
                                        resample = 5)


# Split dataset into testing and training samples for each individual species --
obsv_trte <- splitMultResp(x = obsv@data$input, 
                                 response = response,
                                 resamples = obsv_resamples)


# Evaluate prediction models ---------------------------------------------------
independent <- obsv@meta$input$INDEPENDENT
independent <- c("mal_base_movwin_med_3x3", "ndvi_movwin_sdv_3x3")
n_vars <- c(seq(length(independent)))

models <- trainModel(x = obsv,
                     response = response, independent = independent,
                     resamples = obsv_trte, n_var = n_vars,
                     mthd = "rf", seed_nbr = 11, cv_nbr = 5,
                     var_selection = "sd",
                     filepath_tmp = filepath_results)

var_imp <- compVarImp(models, scale = FALSE)

var_imp_scale <- compVarImp(models, scale = TRUE)

var_imp_plot <- plotVarImp(var_imp)

var_imp_heat <- plotVarImpHeatmap(var_imp_scale, xlab = "Species", ylab = "Band")

tstat <- compContTests(models, mean = TRUE)

tstat_mean <- merge(tstat[[1]], prevalence, by.x = "Response", by.y = "RESPONSE")

tstat_mean[order(tstat_mean$Kappa_mean, decreasing = TRUE),]

ggplot(data = tstat_mean, aes(x = OCCURENCE, y = Kappa_mean)) + geom_point() + geom_smooth()




