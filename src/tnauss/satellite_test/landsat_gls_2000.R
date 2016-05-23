path <- "/media/tnauss/myWork/analysis/moc_rs/GLS2010"
files <- list.files(path, pattern = glob2rx("*.TIF"), full.names = TRUE)

sat <- satellite(files)
sat <- crop(sat, extent(matrix(c(650000, 5400000, 660000, 5450000), nrow=2)))
sat_atmos <- calcAtmosCorr(sat, model = "DOS2", esun_method = "Table")

plot(sat_atmos, bcde = "B003n_REF_AtmosCorr")
