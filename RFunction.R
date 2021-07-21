library('move')
library('raster')
library('sf')
library('stars')

rFunction <- function(data,grid,typ)
{
  Sys.setenv(tz="GMT")
  
  if (is.null(grid))
  {
    logger.info("You have not defined a grid cell size. By default we apply 100000 = 100 km here. This may not fit your data set, please go back and configure the App correctly.")
    grid <- 100000
  }
  if (is.null(typ))
  {
    logger.info("You have not selected a preferred raster file format. By default, here we apply usual raster format '.grd'. If you need something else, go back and configure the App accordingly.")
    typ <- "raster"
  }
      
      | is.null(rel) | is.null(valu)) logger.info("One of your parameters has not been set. This will lead to an error.")
  
  logger.info(paste("You request a raster output file of type",typ,"with a grid size of",grid,"metres."))
  
  data.split <- move::split(data)
  data.split_nozero <- data.split[unlist(lapply(data.split, length) > 1)]
  if (length(data.split_nozero)==0) logger.info("Warning! Error! There are no segments (or at least 2 positions) in your data set. No rasterization of the tracks possible.") # this is very unlikely, therefore not adaption in the below code for it.
  
  midlon <- round(mean(coordinates(data)[,1]))
  midlat <- round(mean(coordinates(data)[,2]))
  
  datat <- spTransform(data,CRSobj=paste0("+proj=aeqd +lat_0=",midlat," +lon_0=",midlon," +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs"))
  sarea <- st_bbox(datat, crs=CRS(paste0("+proj=aeqd +lat_0=",midlat," +lon_0=",midlon," +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs")))
  grd <- st_as_stars(sarea,dx=grid,dy=grid,values=0) #have to transform into aequ, unit= m
  sfrast <- lapply(data.split_nozero, function (x) {
    xt <- spTransform(x,CRSobj=paste0("+proj=aeqd +lat_0=",midlat," +lon_0=",midlon," +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs"))
    ls <- st_sf(a = 1, st_sfc(st_linestring(coordinates(xt))), crs=CRS("+proj=longlat +datum=WGS84"))
    tmp <- st_rasterize(ls, grd, options="ALL_TOUCHED=TRUE")
    tmp[is.na(tmp)] <- 0
    return(tmp)
    message(paste("Done with ", xt@idData$local_identifier, ".", sep=""))
  })
  sumRas <- sfrast[[1]]
  for (i in seq(along=sfrast)[-1]) sumRas <- sumRas+sfrast[[i]]
  sumRas[sumRas==0] <- NA
  res <- as(sumRas,"Raster")
  
  if (typ=="raster") writeRaster(res,filename=paste0(Sys.getenv(x = "APP_ARTIFACTS_DIR", "/tmp/"),"data_raster.grd"),format="raster")
  
  if (typ=="ascii") writeRaster(res,filename=paste0(Sys.getenv(x = "APP_ARTIFACTS_DIR", "/tmp/"),"data_raster.asc"),format="ascii")
  
  if (typ=="CDF") writeRaster(res,filename=paste0(Sys.getenv(x = "APP_ARTIFACTS_DIR", "/tmp/"),"data_raster.nc"),format="CDF")
  
  if (typ=="GTiff") writeRaster(res,filename=paste0(Sys.getenv(x = "APP_ARTIFACTS_DIR", "/tmp/"),"data_raster.tif"),format="GTiff")
  
  #writeRaster(res,filename="data_raster.grd",format="raster")
  
  result <- data
  return(result)
}

  
  
  
  
  
  
  
  
  
  
