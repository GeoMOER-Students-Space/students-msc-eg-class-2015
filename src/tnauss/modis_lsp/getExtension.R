getExtension <- function(dataFormat,...)
{
  if(toupper(dataFormat) %in% c("HDF-EOS","HDF4IMAGE")) # MRT + GDAL
  {
    return(".hdf")
  } else if (toupper(dataFormat) %in% c("GTIFF","GEOTIFF"))  # MRT + GDAL
  {
    return(".tif")
  } else if (tolower(dataFormat) =="raw binary")  # MRT + GDAL
  {
    return(".hdr")
  } else if (toupper(dataFormat)=="ENVI") 
  {
    return("") # should generate a '.hdr' file + a file without extension
  } else if (dataFormat=="FIT") 
  {
    return(NA)    
  } else if (toupper(dataFormat)=="ILWIS")
  {
    return(".mpr") # is this ok?
  } else 
  {
    gdalPath <- MODIS:::combineOptions()$gdalPath
    cmd <- paste0(gdalPath,'gdalinfo --format ')
    
    if(.Platform$OS.type=="unix")
    {
      ext <- system(paste0(cmd, dataFormat),intern=TRUE)   
    } else
    {
      ext <- shell(paste0(cmd, dataFormat),intern=TRUE)   
    }
    
    ext <- grep(ext,pattern="Extension:",value=TRUE)
    
    if(length(ext)==0)
    {
      return(NA)
    } else
    {
      ext <- gsub(strsplit(ext,":")[[1]][2],pattern=" ",replacement="")
      
      if (ext!="")
      {
        ext <- paste0(".",ext)
      }
      return(ext)
    }
  }
}