satdata_windows <- function(data, center, cnames, size = 3){
  cols <- ncol(data)
  way <- (size - 1)/2
  all <- lapply(center, function(c){
    cp <- do.call("rbind", lapply(seq(-way, way), function(wr){
      cpr <- c + cols * wr
      vals <- do.call("rbind", lapply(seq(-way, way), function(wc){
        v <- as.data.frame(data[cpr + wc])
        return(v)
      }))
      return(vals)
    }))
  })
  names(all) <- cnames
  return(all)
}