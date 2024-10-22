
setwd()

system(sprintf(
  'cd %s
    gdal_translate -projwin %s NA_NALCMS_landcover_2020_30m.tif cropped.tif
    gdal_calc.py -A cropped.tif --outfile=cat_cropped.tif --calc="%s" --format=GTiff --overwrite
    rm cropped.tif
    gdal_translate -ot Float32 cat_cropped.tif num_cropped.tif
    rm cat_cropped.tif
    gdalwarp -tr %s --config -wm 8000 -wo 8 -r average -overwrite num_cropped.tif output.tif
    rm num_cropped.tif
    # https://trac.osgeo.org/gdal/wiki/CloudOptimizedGeoTIFF
    # compress to COG
    gdal_translate output.tif %s.tif -co TILED=YES -co COPY_SRC_OVERVIEWS=YES -co COMPRESS=LZW
    rm output.tif
   ', 
  "/home/frousseu/Downloads/land_cover_2020_30m_tif/NA_NALCMS_landcover_2020_30m/data", 
  "1729000 600000 2150000 300000", 
  "(A==18)", 
  "30 30", 
  "fsl"
))


fsl <- rast("/home/frousseu/Downloads/land_cover_2020_30m_tif/NA_NALCMS_landcover_2020_30m/data/fsl.tif")
b <- st_cast(region, "MULTILINESTRING") |>
  st_transform(st_crs(fsl)) |>
  st_buffer(30000)
plot(fsl)
plot(st_geometry(b), add = TRUE)
fsl <- terra::crop(fsl, b, mask = TRUE, overwrite = TRUE, NAflag = NA)
fsl <- ifel(fsl == 1, 1, NA)


x <- st_read("/home/frousseu/Downloads/04/GRHQ_04OU_GRP.gdb", layer = "RH_S") |> st_zm()
st_write(x, "hydroqc.fgb")
x <- x[x$UDH == "04OU",] |> st_zm()
x <- x[rev(order(st_area(x))), ]
o <- st_transform(x[1:10,], st_crs(fsl))
fsl <- mask(fsl, o, inverse = TRUE, updatevalue = 1)


pols <- as.polygons(fsl, values = FALSE) |> st_as_sf() |> ms_explode()
plot(st_geometry(pols), col = "cyan", lwd = 0.1)
o <- rev(order(st_area(pols)))[1:3000]
sl <- pols[o, ]
sl <- st_buffer(sl, 75) |> st_union() |> st_buffer(-75) |> st_as_sf() |> ms_explode()
sl <- sl[rev(order(st_area(sl)))[1], ]

sl2 <- st_transform(sl, st_crs(p))
r <- terra::rasterize(sl2, p)


r <- setValues(p$distfsl, rep(0, ncell(p)))
r <- mask(r, st_transform(sl2, st_crs(r)), inverse = TRUE, updatevalue = 1)
writeRaster(r, "/home/frousseu/Documents/github/sdm_emv_finance/output.tif", overwrite = TRUE)

system(sprintf(
  'cd %s
   gdal_proximity.py output.tif output.tif -values 1 -nodata 1
   gdal_translate output.tif dstlaurent.tif -co TILED=YES -co COPY_SRC_OVERVIEWS=YES -co COMPRESS=LZW -co BIGTIFF=YES
   rm output.tif
   ',
  "/home/frousseu/Documents/github/sdm_emv_finance"
))

dstlaurent <- rast("/home/frousseu/Documents/github/sdm_emv_finance/dstlaurent.tif")
dstlaurent <- mask(dstlaurent, p[[1]])
plot(log(dstlaurent + 0.01))

par(mfrow = c(2, 1))
plot(terra::crop(p$distfsl, sl2))
plot(terra::crop(log(dstlaurent + 0.001), sl2))

