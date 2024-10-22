
library(sf)
library(terra)
library(rmapshaper)



system(sprintf(
  'cd %s
    gdal_translate -projwin %s CAN_NALCMS_landcover_2020_30m.tif cropped.tif
    #cats=$cats
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
  "/home/frousseu/Downloads/can_land_cover_2020_30m_tif/CAN_NALCMS_landcover_2020_30m/data", 
  "1000000 1550000 2750000 -250000", 
  "(A==0)", 
  "30 30", 
  "ocean"
))


ocean <- rast("/home/frousseu/Downloads/can_land_cover_2020_30m_tif/CAN_NALCMS_landcover_2020_30m/data/ocean.tif")
ocean <- aggregate(ocean, 2)
ocean <- ifel(ocean >= 0.75, 1, NA)
plot(ocean)




usa <- rast("/home/frousseu/Downloads/land_cover_2020_30m_tif/NA_NALCMS_landcover_2020_30m/data/NA_NALCMS_landcover_2020_30m.tif")
usa <- rast("/home/frousseu/Downloads/land_cover_2020_30m_tif/NA_NALCMS_landcover_2020_30m/data/ocean.tif")

system(sprintf(
  'cd %s
   gdal_translate -projwin %s NA_NALCMS_landcover_2020_30m.tif input_raster.tif
   gdal_proximity.py input_raster.tif input_raster.tif -values 0 -nodata 0
   rm input_raster.tif
   gdal_translate output.tif %s.tif -co TILED=YES -co COPY_SRC_OVERVIEWS=YES -co COMPRESS=LZW -co BIGTIFF=YES
   rm output.tif
   ',
    "/home/frousseu/Downloads/land_cover_2020_30m_tif/NA_NALCMS_landcover_2020_30m/data", 
    "750000 2250000 3330000 -550000", 
    "ocean"
))

ocean <- rast("/home/frousseu/Downloads/land_cover_2020_30m_tif/NA_NALCMS_landcover_2020_30m/data/input_raster.tif")
plot(log(30 * ocean + 1))

system(sprintf(
  'cd %s
   #gdal_calc.py -A ocean.tif --outfile=temp.tif --calc="A * 30" --NoDataValue=999
   gdalwarp -tr %s --config -wm 8000 -wo 8 -r average -overwrite ocean.tif output.tif
   rm temp.tif
   ',
   "/home/frousseu/Downloads/land_cover_2020_30m_tif/NA_NALCMS_landcover_2020_30m/data", 
   "300 300"
))

#############################################
### Dist FSL ################################
#############################################

system(sprintf(
  'cd %s
    gdal_translate -projwin %s NA_NALCMS_landcover_2020_30m.tif cropped.tif
    #cats=$cats
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
  "1700000 650000 2175000 250000", 
  "(A==18)", 
  "30 30", 
  "fsl"
))

#fsl <- rast("/home/frousseu/Downloads/utilisation_territoire_2020/utilisation_territoire_2020/fsl.tif")
fsl <- rast("/home/frousseu/Downloads/land_cover_2020_30m_tif/NA_NALCMS_landcover_2020_30m/data/fsl.tif")
b <- st_cast(region, "MULTILINESTRING") |>
     st_transform(st_crs(fsl)) |>
     st_buffer(30000)
plot(fsl)
plot(st_geometry(b), add = TRUE)
fsl <- terra::crop(fsl, b, mask = TRUE, overwrite = TRUE, NAflag = NA)
#fsl <- aggregate(fsl, 2)
#fsl <- ifel(fsl >= 0.75, 1, NA)
fsl <- ifel(fsl == 1, 1, NA)

lf <- list.files("/home/frousseu/Downloads/04", full = TRUE)
i <- 13
#st_layers(lf[i])
#grhq <- st_read(lf[i], layer = "UDH")
#grhq <- st_transform(grhq, st_crs(fsl))
#grhq <- st_zm(grhq)
#plot(fsl)
#plot(st_geometry(grhq), add = TRUE)

x <- st_read("/home/frousseu/Downloads/04/GRHQ_04OU_GRP.gdb", layer = "RH_S")
x <- x[x$UDH == "04OU",] |> st_zm()
x <- x[rev(order(st_area(x))), ]
o <- st_transform(x[1:10,], st_crs(fsl))
fsl <- mask(fsl, o, inverse = TRUE, updatevalue = 1)


plot(fsl)

pols <- as.polygons(fsl, values = FALSE) |> st_as_sf() |> ms_explode()
plot(st_geometry(pols), col = "cyan", lwd = 0.1)
o <- rev(order(st_area(pols)))[1:3000]
sl <- pols[o, ]
sl <- st_buffer(sl, 50) |> st_union() |> st_buffer(-50) |> st_as_sf() |> ms_explode()
sl <- sl[rev(order(st_area(sl)))[1], ]
#plot(st_geometry(sl), col = "cyan", lwd = 0.1)

sl2 <- st_transform(sl, st_crs(p))

r <- terra::rasterize(sl2, p)




p2 <- aggregate(p[[1]], 5)
#p2 <- p
coo <- xyFromCell(p2, 1:ncell(p2)) |>
  as.data.frame() |>
  st_as_sf(coords = c("x", "y"), crs = st_crs(p)) |>
  st_transform(st_crs(sl2))

dis <- st_distance(coo, ms_simplify(sl2, 0.01)) |> as.matrix()
coo$distfl <- dis[, 1]
coo <- st_transform(coo, st_crs(p))

distfl <- setValues(p2[[1]], coo$distfl)
dfl <- project(distfl, p[[1]])
th <- 10
dfl[dfl < th] <- th
dfl <- subst(dfl, NA, th)
names(dfl) <- "distfsl"

plot(dfl)
plot(log(terra::crop(dfl, st_buffer(st_transform(sl, st_crs(p)), 100000)) + 1))


ex <- ext(1860000, 1870000, 327000, 335000)
ex <- project(ex, crs(fsl), crs(ut))
ut <- rast("/home/frousseu/Downloads/utilisation_territoire_2020/utilisation_territoire_2020/quebec_ut.tif")
ut <- terra::crop(ut, ex)
fsl2 <- terra::crop(fsl, ex)
usa2 <- terra::crop(usa, ex)

plot(ut[[19]])
plot(usa2)

library(rnaturalearth)
rivers <- ne_download(scale = 10, type = "rivers_north_america", category = "physical")
rivers <- ne_download(scale = 10, type = "lakes_north_america", category = "physical")
rivers <- st_transform(rivers, st_crs(fsl))
plot(rivers, col = "blue", add = TRUE)

lf <- list.files("/home/frousseu/Downloads/04", full = TRUE)
i <- 13
st_layers(lf[i])
grhq <- st_read(lf[i], layer = "UDH")
grhq <- st_transform(grhq, st_crs(fsl))
grhq <- st_zm(grhq)
plot(fsl)
plot(st_geometry(grhq), add = TRUE)


x <- st_read(lf[i], layer = "RH_S")
x <- x[x$UDH == "04OU",] |> st_zm()


plot(st_geometry(x))

x <- x[rev(order(st_area(x))), ]
o <- st_transform(x[1:10,], st_crs(fsl))
plot(fsl)
plot(st_geometry(ms_simplify(o, 0.01)), col = "cyan", border = NA, add = TRUE)

fsl2 <- aggregate(fsl, 2)
fsl2 <- mask(fsl, o, inverse = TRUE, updatevalue = 1)
plot(fsl2)

mapview(o)


r <- setValues(p$distfsl, rep(0, ncell(p)))
r <- mask(r, st_transform(sl2, st_crs(r)), inverse = TRUE, updatevalue = 1)
writeRaster(r, "/home/frousseu/Documents/github/sdm_emv_finance/stlaurent.tif", overwrite = TRUE)

system(sprintf(
  'cd %s
   gdal_proximity.py stlaurent.tif output.tif -values 1 -nodata 1
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
plot(terra::crop(log(dstlaurent + 0.01), sl2))





lf <- list.files("/home/frousseu/Downloads/04", full = TRUE)
i <- 8
st_layers(lf[i])
grhq <- st_read(lf[i], layer = "UDH")
grhq <- st_transform(grhq, st_crs(fsl))
grhq <- st_zm(grhq)
plot(fsl)
plot(st_geometry(grhq), add = TRUE)



x00 <- st_read("/home/frousseu/Downloads/04/GRHQ_04AL_GRP.gdb", layer = "RH_S") |> st_zm()
#x00 <- x00[ , c("PERIM_M", "TOPONYME")]
#x00 <- x00[grep("Gatineau", x00$TOPONYME), ]
x00 <- x00[c(23316, 10876, 126, 23315, 201, 23312, 23314), ] # Gatineau River polygons are mostly unnamed...
x0 <- st_read("/home/frousseu/Downloads/04/GRHQ_04OU_GRP.gdb", layer = "RH_S") |> st_zm()
x0 <- x0[grep("Outaouais|Gatineau|Brasserie", x0$TOPONYME), ]
x1 <- st_read("/home/frousseu/Downloads/00/GRHQ_00AA.gdb", layer = "RH_S") |> st_zm()
x1 <- x1[rev(order(st_area(x1))), ]
x1 <- x1[1:10,]
x1 <- x1[-which(x1$TOPONYME == "Loch Garry"), ]
x2 <- st_read("/home/frousseu/Downloads/00/GRHQ_00AB.gdb", layer = "RH_S") |> st_zm()
x2 <- x2[rev(order(st_area(x2))), ]
x2 <- x2[1:10,]
x3 <- st_read("/home/frousseu/Downloads/00/GRHQ_00AC.gdb", layer = "RH_S") |> st_zm()
x3 <- x3[rev(order(st_area(x3))), ]
x3 <- x3[1:1,]
x4 <- st_read("/home/frousseu/Downloads/03/GRHQ_03AC_GRP.gdb", layer = "RH_S") |> st_zm()
x4 <- x4[grep("Richelieu|Champlain|Chambly", x4$TOPONYME), ]
x <- rbind(x00, x0, x1, x2, x3, x4)
x <- st_transform(x, st_crs(fsl))
par(mar = c(0, 0, 0, 0))
plot(st_geometry(x), col = "cyan", border = NA, axes = TRUE)
xb <- st_buffer(x, 5000) |> st_union()
plot(st_geometry(xb), add = TRUE)




plot(fsl)
plot(st_geometry(st_transform(x, st_crs(fsl))), col = "cyan", border = NA, add = TRUE)
plot(st_geometry(st_transform(xb, st_crs(fsl))), border = "white", add = TRUE)

fsl <- terra::crop(fsl, vect(xb), mask = TRUE, overwrite = TRUE, NAflag = NA)
fsl <- mask(fsl, x, inverse = TRUE, updatevalue = 1)
plot(fsl)

     