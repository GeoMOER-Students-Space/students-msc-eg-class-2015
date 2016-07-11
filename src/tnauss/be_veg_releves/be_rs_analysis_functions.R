library(glcm)
library(reshape2)

satdata_windows <- function(data, center, cnames, size = 3){
  cols <- ncol(data)
  way <- (size - 1)/2
  all <- lapply(center, function(c){
    cp <- lapply(seq(-way, way), function(wr){
      cpr <- c + cols * wr
      pos <- seq(cpr-way, cpr+way)
      return(pos)
    })
    return(as.data.frame(data[unlist(cp)]))
  })
  names(all) <- cnames
  return(all)
}


glcm_texture <- function(data, idname){
  texture <- lapply(seq(3, 5, by = 2), function(w){D
    texture <- glcm(matrix(data, 7, 7), n_grey=255, 
                          window=c(w,w), list(c(0,1), c(1,1), c(1,0), c(1,-1)),
                          statistics = c("mean", "variance", "homogeneity", 
                                         "contrast", "dissimilarity", "entropy", 
                                         "second_moment", "correlation"),
                          min_x=NULL, max_x=NULL, na_opt="any", 
                          na_val=NA, scale_factor=1, asinteger=FALSE)
    texture <- as.data.frame(texture)[4, seq(4, 56,  by = 7)]
    colnames(texture) <- paste0(idname, "_", c("mean", "variance", "homogeneity", 
                                               "contrast", "dissimilarity", "entropy", 
                                               "second_moment", "correlation"),
                                "_", as.character(w), "x", as.character(w))
    return(texture)
  })
  texture <- do.call("cbind", texture)
  # texture$window <- c("w3x3", "w5x5")
  # texture$idname <- idname
  # texture <- melt(texture, id.vars = c("idname", "window"))
  return(texture)
}


movwin <- function(data, idname){
  movwin <- lapply(seq(3, 7, by = 2), function(w){
    way <- (w-1)/2
    movwin <- data.frame(
      med = median(data[seq(4-way, 4+way),seq(4-way, 4+way)]),
      sdv = sd(data[seq(4-way, 4+way),seq(4-way, 4+way)]),
      iqr = quantile(data[seq(4-way, 4+way),seq(4-way, 4+way)], 
                     probs = 0.25, na.rm = TRUE) -
        quantile(data[seq(4-way, 4+way),seq(4-way, 4+way)], 
                 probs = 0.75, na.rm = TRUE),
      medsd = (quantile(data[seq(4-way, 4+way),seq(4-way, 4+way)], 
                        probs = 0.25, na.rm = TRUE) -
                 quantile(data[seq(4-way, 4+way),seq(4-way, 4+way)], 
                          probs = 0.75, na.rm = TRUE)) / 
        sd(data[seq(4-way, 4+way),seq(4-way, 4+way)]))
    colnames(movwin) <- paste0(idname, "_", colnames(movwin), "_", 
                               as.character(w), "x", as.character(w))
    return(movwin)
  })
  movwin <- do.call("cbind", movwin)
  # movwin$window <- c("w3x3", "w5x5", "w7x7")
  # movwin$idname <- idname
  # movwin <- melt(movwin, id.vars = c("idname", "window"))
  return(movwin)
}


specdiv <- function(data, date, sat_name){
  specdiv <- lapply(seq(length(data)), function(d){
    if(any(is.na(data[[d]]))){
      NULL
    } else {
      if(sat_name == "ls"){
        mal_data <- data[[d]][, -c(bands$ca, bands$pan, bands$cir)]
      } else {
        mal_data <- data[[d]]
      }
      mal_base <- mahalanobis(mal_data, colMeans(mal_data), var(mal_data))
      
      if(sat_name == "ls"){
        mal_data <- data[[d]][, -bands$pan]
      } 
      mal_ext <- mahalanobis(mal_data, colMeans(mal_data), var(mal_data))
      
      ndvi <- (data[[d]][, bands$nir] - data[[d]][, bands$red]) / 
        (data[[d]][, bands$nir] + data[[d]][, bands$red])
      
      rvi <- (data[[d]][, bands$nir] / data[[d]][, bands$red])
      
      msavi <- (2 * data[[d]][, bands$nir] + 1 - 
                  sqrt((2 * data[[d]][, bands$nir] + 1)^2-8 * 
                         (data[[d]][, bands$nir] - 
                            data[[d]][, bands$red]))) / 2
      
      mtvi <- 1.2*(1.2*(data[[d]][, bands$nir]-data[[d]][, bands$green])
                   -2.5*(data[[d]][, bands$red] - data[[d]][, bands$green]))
      
      tvi <- sqrt((data[[d]][, bands$nir]-data[[d]][, bands$red]) / 
                    (data[[d]][, bands$nir]+data[[d]][, bands$red])+0.5)
      
      
      mal_base_glcm <- glcm_texture(matrix(mal_base, 7, 7), idname = "mal_base_glcm")
      ndvi_glcm <- glcm_texture(matrix(ndvi, 7, 7), idname = "ndvi_glcm")
      
      mal_base_movwin <- movwin(matrix(mal_base, 7, 7), idname = "mal_base_movwin")
      mal_ext_movwin <- movwin(matrix(mal_ext, 7, 7), idname = "mal_ext_movwin")
      ndvi_movwin <- movwin(matrix(ndvi, 7, 7), idname = "ndvi_movwin")
      rvi_movwin <- movwin(matrix(rvi, 7, 7), idname = "rvi_movwin")
      msavi_movwin <- movwin(matrix(msavi, 7, 7), idname = "msavi_movwin")
      mtvi_movwin <- movwin(matrix(mtvi, 7, 7), idname = "mtvi_movwin")
      tvi_movwin <- movwin(matrix(tvi, 7, 7), idname = "tvi_movwin")

      specdiv <- cbind(mal_base_glcm, ndvi_glcm,
                       mal_base_movwin, mal_ext_movwin,
                       ndvi_movwin, rvi_movwin, 
                       msavi_movwin, mtvi_movwin, 
                       tvi_movwin)
                       
      specdiv$epid <- names(data)[d]
      specdiv$date <- as.Date(date)
      specdiv$sensor <- sat_name
      return(specdiv)
    }
  })
  specdiv <- do.call("rbind", specdiv)
  return(specdiv)
}


maxndvi <- function(data, date){
  date <- as.Date(date)
  act <- data[data$date >= date[1] & data$date <= date[2], ]
  maxndvi <- lapply(unique(act$epid), function(p){
    sub <- act[act$epid == p, ]
    return(sub[which.max(sub$ndvi_movwin_med_3x3),])
  })
  maxndvi <- do.call("rbind", maxndvi)
  return(maxndvi)
}