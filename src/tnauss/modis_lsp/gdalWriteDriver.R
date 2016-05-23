gdalWriteDriver <- function(renew = FALSE, quiet = TRUE,...)
{
  iw   <- options()$warn 
  options(warn=-1)
  on.exit(options(warn=iw))
  
  opt <- MODIS:::combineOptions(...)
  
  outfile <- paste0(opt$outDirPath,".auxiliaries/gdalOutDriver.RData")
  
  if (!is.null(getOption("MODIS_gdalOutDriver"))) # take it from options()
  {
    gdalOutDriver <- getOption("MODIS_gdalOutDriver")
  } else if(file.exists(outfile)) # or from RData
  {
    load(outfile)
  }  
  
  if(exists("gdalOutDriver"))
  {
    if (nrow(gdalOutDriver)<5)
    {
      renew <- TRUE
    }
  } else
  {
    renew <- TRUE
  }
  
  if (renew)
  {
    if(!quiet)
    {
      message("Detecting available write drivers!")
    }
    
    cmd <- paste0(opt$gdalPath,"gdalinfo --formats")
    
    # list all drivers with (rw)
    if (.Platform$OS=="unix")
    {
      gdalOutDriver <- system(cmd,intern=TRUE)
    } else
    {
      gdalOutDriver <- shell(cmd,intern=TRUE)
    }
    
    gdalOutDriver <- grep(gdalOutDriver,pattern="\\(rw",value=TRUE) # this regex must be preciser
    name          <- sapply(gdalOutDriver,function(x){strsplit(x,"\\(")[[1]][1]})
    name          <- gsub(as.character(name), pattern=" ", replacement="")
    name          <- gsub(as.character(name), pattern=" ", replacement="")
    name          <- sapply(name, function(x){return(strsplit(x, "-")[[1]][1])})
    
    description <- as.character(sapply(gdalOutDriver,function(x){strsplit(x,"\\): ")[[1]][2]}))
    
    if(!quiet)
    {
      message("Found: ",length(name)," candidate drivers, detecting file extensions...")
    }
    
    extension <- rep(NA,length(name))
    for (i in seq_along(name))
    {
      ind <- grep(name, pattern=paste0("^",name[i],"$"), ignore.case=TRUE, value=FALSE)
      
      if (length(ind)!=0)
      {
        extension[i] <- MODIS:::getExtension(name[ind],gdalPath = opt$gdalPath)
      }
    }
    if(!quiet)
    {
      message(sum(!is.na(extension))," usable drivers detected!")
    }
    gdalOutDriver <- data.frame(name=name[!is.na(extension)], description=description[!is.na(extension)], extension=extension[!is.na(extension)], stringsAsFactors=FALSE)        
    
    if(!file.exists(opt$outDirPath))
    {
      opt$outDirPath <- setPath(opt$outDirPath,ask = TRUE)
      opt$auxPath    <- setPath(paste0(opt$outDirPath,".auxiliaries"),ask=FALSE)
    }
    
    if(file.exists(opt$auxPath))
    {
      save(gdalOutDriver, file=outfile)
    }
  }
  gdalOutDriver
}