
library(sf)
library(geodata)
library(rmapshaper)

epsg <- 6623

can <- gadm("CAN", level = 1, path = getwd()) |> st_as_sf()
usa <- gadm("USA", level = 1, path = getwd()) |> st_as_sf()
usa <- usa[!usa$NAME_1 %in% c("Hawaii", "Alaska"), ]
qc <- can[can$NAME_1 %in% c("Québec"), ] |> st_transform(crs = epsg)
region <- ms_simplify(qc, 0.01)
land <- rbind(can, usa) |>
        ms_simplify(0.01) |>
        st_union() |>
        st_transform(epsg)

#par(mar = c(0, 0, 0, 0))
#plot(st_geometry(qc), lwd = 3)
#plot(st_geometry(region), border = "white", add = TRUE, lwd = 0.5)

ocean <- st_buffer(region, 1000000) |> 
         st_difference(land)

ocean <- st_buffer(land, 0) |> 
         st_difference(land)


ps <- aggregate(p[[1]], 10)
#ps <- p[[1]]
xy <- ps |>
      terra::xyFromCell(1:ncell(ps)) |>
      as.data.frame() |>
      st_as_sf(coords = c("x", "y"), crs = st_crs(p))

dis1 <- as.numeric(st_distance(ocean, xy)[1, ])
dis2 <- as.numeric(st_distance(land, xy)[1, ])
dis12 <- ifelse(dis1 <= 0, -dis2, dis1)
pps <- setValues(ps, dis12)
pps <- project(pps, p)


plot(log(pps+1))
plot(log(terra::crop(pps, occs) + 1))
plot(st_geometry(occs), add = TRUE)
plot(st_geometry(qc), add = TRUE)


library(rnaturalearth)
