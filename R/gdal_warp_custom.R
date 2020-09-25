gdal_warp = function(file_in, file_out, proj_in, proj_out, res){
  warp_cmd = paste0("gdalwarp -tr ", res, " ", res, " -s_srs ", proj_in, 
                    " -t_srs ", proj_out, " -r bilinear -of vrt -overwrite ", file_in, " ", file_in, ".vrt")
  
  translate_cmd = paste0("gdal_translate -co compress=LZW -co BIGTIFF=YES -co TILED=YES ", file_in, ".vrt ", file_out)
  
  system(warp_cmd)
  system(translate_cmd)
  invisible(file.remove(paste0(file_in, ".vrt")))
}
gdal_warp(file_in = "/home/zhoylman/temp/dem_30m_conus_wgs84.tif", file_out = "/home/zhoylman/temp/dem_30m_conus_epsg5070.tif",
          proj_in = "EPSG:4326", proj_out = "EPSG:5070", res = 30)


# gdal_warp(file_in = "/home/zhoylman/temp/percent_error_4k_clipped.tif", file_out = "/home/zhoylman/temp/percent_error_almostGlobal_p5degree.tif",
#           proj_in = "EPSG:4326", proj_out = "EPSG:3857", res = 4000)
# gdal_warp(file_in = "/home/zhoylman/Downloads/hillshade_mt.tif", file_out = "/home/zhoylman/MCO/MCO_data_requests/data/montana_hillshade.tif",
#           proj_in = "EPSG:4326", proj_out = "EPSG:4326", res = 0.01)
