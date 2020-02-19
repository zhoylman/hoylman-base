# project a source raster grid to match a target grid using R and GDAL
gdalProjRaster <- function(img,target,outname=NULL,method="bilinear",wm=1000,nt=1,ot="Float32",NAflag=-9999){
  library(raster)#;library(gdalUtils)
  x<-tempfile()
  if(is.character(img)) fname<-img else writeRaster(img,fname<-paste(tempfile(),".tif",sep=""))
  timg <- if(is.character(target)) raster(target) else target
  if(is.null(outname)) outname<-paste(tempfile(),".tif",sep="")
  dims <- dim(timg)
  ext <- extent(timg)
  te <- paste(c(attr(ext,"xmin"),attr(ext,"ymin"),attr(ext,"xmax"),attr(ext,"ymax")),collapse=" ")
  tr <- paste(res(timg)[2:1],collapse=" ")
  ts <- paste(dim(timg)[2:1],collapse=" ")
  t_srs <- projection(timg)
  if(Sys.info()[[4]]=="COPERNICUS"){
    library(gdalUtils)
    gdalwarp(fname,outname,t_srs=t_srs,te=te,ts=ts,r="bilinear",output_Raster=F)
  } else system(paste0("gdalwarp -wm ",wm," -multi -wo 'NUM_THREADS=",nt,"' -t_srs '",t_srs,"' -te ",te," -ts ",ts," -r ",method," -ot ",ot," -co 'COMPRESS=LZW' -co 'BIGTIFF=YES' -q -dstnodata ",NAflag," ",fname," ",outname))
  invisible(raster(outname))
}
