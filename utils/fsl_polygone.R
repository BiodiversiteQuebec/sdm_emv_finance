
library(sf)
library(terra)
library(rmapshaper)

path <- "/home/frousseu/Downloads"
setwd(path)

### Generate water raster using CEC
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
  file.path(path, "land_cover_2020_30m_tif/NA_NALCMS_landcover_2020_30m/data"), 
  "1729000 600000 2150000 300000", 
  "(A==18)", 
  "30 30", 
  "fsl"
))


fsl <- rast(file.path(path, "land_cover_2020_30m_tif/NA_NALCMS_landcover_2020_30m/data/fsl.tif"))
fsl <- ifel(fsl == 1, 1, NA)

### Check watersheds
#lf <- list.files("04", full = TRUE)
#i <- 8
#st_layers(lf[i])
#grhq <- st_read(lf[i], layer = "UDH")
#grhq <- st_transform(grhq, st_crs(fsl))
#grhq <- st_zm(grhq)
#plot(fsl)
#plot(st_geometry(grhq), add = TRUE)


### Get GRHQ polygons
# https://www.donneesquebec.ca/recherche/dataset/grhq
x00 <- st_read(file.path(path, "04/GRHQ_04AL_GRP.gdb"), layer = "RH_S") |> st_zm()
x00 <- x00[c(23316, 10876, 126, 23315, 201, 23312, 23314), ] # Gatineau River polygons are mostly unnamed...
x0 <- st_read(file.path(path, "04/GRHQ_04OU_GRP.gdb"), layer = "RH_S") |> st_zm()
x0 <- x0[grep("Outaouais|Gatineau|Brasserie", x0$TOPONYME), ]
x1 <- st_read(file.path(path, "00/GRHQ_00AA.gdb"), layer = "RH_S") |> st_zm()
x1 <- x1[rev(order(st_area(x1))), ]
x1 <- x1[1:10,]
x1 <- x1[-which(x1$TOPONYME == "Loch Garry"), ]
x2 <- st_read(file.path(path, "00/GRHQ_00AB.gdb"), layer = "RH_S") |> st_zm()
x2 <- x2[rev(order(st_area(x2))), ]
x2 <- x2[1:10,]
x3 <- st_read(file.path(path, "00/GRHQ_00AC.gdb"), layer = "RH_S") |> st_zm()
x3 <- x3[rev(order(st_area(x3))), ]
x3 <- x3[1:1,]
x4 <- st_read(file.path(path, "03/GRHQ_03AC_GRP.gdb"), layer = "RH_S") |> st_zm()
x4 <- x4[grep("Richelieu|Champlain|Chambly", x4$TOPONYME), ]
x <- rbind(x00, x0, x1, x2, x3, x4)
x <- st_transform(x, st_crs(fsl))
par(mar = c(0, 0, 0, 0))
plot(st_geometry(x), col = "cyan", border = NA, axes = TRUE)
xb <- st_buffer(x, 5000) |> st_union()
plot(st_geometry(xb), add = TRUE)

fsl <- terra::crop(fsl, vect(xb), mask = TRUE, overwrite = TRUE, NAflag = NA)
fsl <- mask(fsl, x, inverse = TRUE, updatevalue = 1)

### Turn to polygons
pols <- as.polygons(fsl, values = FALSE) |> st_as_sf() |> ms_explode()
plot(st_geometry(pols), col = "cyan", lwd = 0.1)
o <- rev(order(st_area(pols)))[1:3000]
sl <- pols[o, ]
sl <- st_buffer(sl, 50) |> st_union() |> st_buffer(-50) |> st_as_sf() |> ms_explode()
sl <- sl[rev(order(st_area(sl)))[1], ]

polfsl <- st_transform(sl, 4326)
st_write(polfsl, "/home/frousseu/Downloads/fleuvestlaurent.gpkg")




######### extras ##########################
p <- rast("/home/frousseu/Documents/github/sdm_emv_finance/predictors_emv.tif")
x <- st_read("https://object-arbutus.cloud.computecanada.ca/bq-io/io/fleuvestlaurent/fleuvestlaurent.gpkg") |>
     st_transform(st_crs(p))

r <- rast(resolution = 100, extent = ext(p), crs = crs(p))
r <- setValues(r, rep(0, length.out = ncell(r)))
r <- terra::mask(r, vect(st_transform(x, st_crs(r))), inverse = FALSE, touches = FALSE)
writeRaster(r, "/home/frousseu/Documents/github/sdm_emv_finance/stlaurent.tif", overwrite = TRUE)

system(sprintf(
  'cd %s
   gdal_proximity.py stlaurent.tif stlaurent.tif -values 0 -nodata 0 -distunits GEO
   gdal_translate stlaurent.tif dstlaurent.tif -co TILED=YES -co COPY_SRC_OVERVIEWS=YES -co COMPRESS=LZW -co BIGTIFF=YES
   #rm output.tif
   ',
  "/home/frousseu/Documents/github/sdm_emv_finance"
))

dstlaurent <- rast("/home/frousseu/Documents/github/sdm_emv_finance/dstlaurent.tif")
dstlaurent <- crop(dstlaurent, x)
plot(dstlaurent)
plot(log(dstlaurent + 0.0001))
plot(terra::crop(log(dstlaurent + 0.1), st_transform(sl, st_crs(dstlaurent))))

plot(terra::crop(log(dstlaurent + 0.1), st_transform(sl, st_crs(dstlaurent))))


f <- system.file("ex/elev.tif", package="terra")
r <- rast(f)
msk <- ifel(r < 400, NA, 1)

m <- mask(r, msk)

f <- system.file("ex/lux.shp", package="terra")
v <- vect(f)[1,] |> 
     st_as_sf() |>
     st_transform(6624) |>
     st_buffer(-5000) |>
     st_transform(st_crs(r)) |>
     vect()

mv1 <- terra::mask(r, v, inverse = TRUE)
plot(mv1)
mv2 <- terra::crop(r, v, mask=TRUE)
plot(mv2)

library(terra)
sp <- "Hemidactylium_scutatum_glmbinomial" 
spf <- paste0(sp, ".tif")  
f <- file.path("/vsicurl/https://object-arbutus.cloud.computecanada.ca/bq-io/acer/sdm_emv_finance", spf)
describe(f)
r <- rast(f)
plot(r)
global(r,"range",na.rm=TRUE)






