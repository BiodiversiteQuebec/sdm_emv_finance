


dens <- rast("/vsicurl/https://object-arbutus.cloud.computecanada.ca/bq-io/atlas/density/density_2024-05-29.tif")
#b <- spatSample(dens, size = 50000, method = "weights", as.points = TRUE)

buffer_dist <- 250 * 1000

bin <- st_transform(occs, epsg) |>
  st_buffer(dist = buffer_dist) |>
  st_union() |>
  vect()
bin <- terra::crop(dens[[group]], y = bin, mask = FALSE, overwrite = TRUE)
tmpFiles(TRUE, TRUE, TRUE, TRUE)
bin <- terra::mask(bin, vect(region)) # for some reason the spatSample in the pipe does not end
tmpFiles(TRUE, TRUE, TRUE, TRUE)
bin <- spatSample(bin, size = 50000, method = "weights", as.points = TRUE, replace = TRUE) |>
  st_as_sf() |>
  st_transform(st_crs(region))




#rr <- aggregate(bin,1000,fun=sum)
#plot(rr)
#ss <- spatSample(rr, size = 100, method = "weights", as.points = TRUE, replace = TRUE) |> st_as_sf() |> st_jitter(amount = 10000) |> st_geometry()
#plot(ss, add = TRUE)



source("https://raw.githubusercontent.com/BiodiversiteQuebec/sdmtools/main/backgroundBuffer.R")

bout <- backgroundBuffer(occs, region, n = 5000, dist = buffer_dist, convex = FALSE)
st_geometry(bout) <- "geometry"

plot(p$deciduous, mar = 0, axes = FALSE, legend = FALSE)
plot(st_geometry(bout), pch = 21, col = adjustcolor("grey70", 0.65), lwd = 1, bg = adjustcolor("grey70", 0.25), cex = 1, add = TRUE)
plot(st_geometry(bin), pch = 21, col = adjustcolor("grey30", 0.65), lwd = 1, bg = adjustcolor("grey30", 0.25), cex = 1, add = TRUE)
plot(st_geometry(occs), pch = 21, col = adjustcolor("forestgreen", 0.65), lwd = 1, bg = adjustcolor("forestgreen", 0.25), cex = 1, add = TRUE)
plot(st_geometry(region), add = TRUE)

background <- rbind(bin[, "geometry"], bout[, "geometry"])
background$pres <- 0


obs <- occs
st_geometry(obs) <- "geometry"
obs <- obs[, "geometry"]
obs$pres <- 1

d <- rbind(obs, background)

#bv <- st_buffer(obs, dist = buffer_dist) |> st_union() |> st_as_sf()
#plot(st_geometry(region))
#plot(st_geometry(obs), add = TRUE)
#plot(st_geometry(bv), add = TRUE)


#r <- rasterize(bv, p[[1]], background = 0) |> mask(p[[1]])
#names(r) <- "bv"

