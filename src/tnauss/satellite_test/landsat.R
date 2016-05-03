path <- "/media/tnauss/myWork/analysis/moc_rs/LC82080372014106LGN00"

files <- list.files(path, pattern = glob2rx("*.TIF"), full.names = TRUE)

sat <- satellite(files)
sat <- crop(sat, extent(matrix(c(250000, 3600000, 350000, 3650000), nrow=2)))

sat_atmos <- calcAtmosCorr(sat, model = "DOS2", esun_method = "Model")
plot(sat_atmos, bcde = "B003n_REF_AtmosCorr")

