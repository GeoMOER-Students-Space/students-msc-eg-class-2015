setwd("/media/dogbert/myWork/analysis/moc_rs/MachineLearning/Data/rdata")

load("006re100biomass_grf.rdata")

do.call("rbind", lapply(models[[1]], function(r){
  r$model$bestSubset  
}))


test <- compRegrTests(models, per_model = TRUE)
boxplot(unique(test$r_squared))
mean(aggregate(test$r_squared, by = list(test$sample), FUN = "mean")$x)
mean(test$r_squared)


summary(lm((models[[1]][[1]]$testing$PREDICTED ~ models[[1]][[1]]$testing$RESPONSE))

aggregate(test$r_squared, 
          by = list(test$model_response),
          FUN = mean)

test$sample
