

source("functions.r")
source("prelim.R")
source("predictors.R")

library(terra)
library(viridisLite)

source("https://raw.githubusercontent.com/frousseu/FRutils/master/R/colo.scale.R")

options(terra.pal=rev(terrain.colors(200)))
#options(terra.pal=adjustcolor(colo.scale(1:500, c("grey95", viridis(5))), 0.75))
#options(terra.pal=adjustcolor(colo.scale(1:500, c("grey95", viridis(5)[1], "orange")), 0.75))

#sdm <- list.files("outputs", full = TRUE) |>
#         lapply(rast) |> 
#         rast() |>

lf <- list.files("outputs", full = TRUE)
lapply(lf, function(i){
  r <- rast(i) |>
       project(p[[1]]) |>
       subst(NA, 0) |>
       mask(region)
  writeRaster(r, i, filetype = "COG", overwrite = TRUE)  
  print(i)   
})


         lapply(rast) |>
         lapply(project, p[[1]]) |> 
         rast() |>
         

#r <- project(sdm[[1]], p[[1]])
#r <- subst(sdm, NA, 0) |> mask(region)






qcrange <- st_read("/vsicurl/https://object-arbutus.cloud.computecanada.ca/bq-io/io/qc_range_maps/qc_range_maps.fgb")



lf <- list.files("outputs", full = TRUE)
sdm <- lf[67] |>
         rast() |>
         project(p[[1]]) |> 
         subst(NA, 0) |>
         mask(region)
species <- paste(strsplit(names(sdm), "_")[[1]][1:2],collapse = " ")
source("inputs.r")
ii <- match(species, species_list$species)
species <- species_list$species[ii]
group <- species_list$group[ii]
period <- c(species_list$start[ii], species_list$end[ii])
source("occurrences.R")
occs <- st_transform(occs, st_crs(sdm))

r <- terra::crop(sdm, st_buffer(occs, 1e5))
#r <- aggregate(r, 10)
plot(r)
plot(st_geometry(qcrange[trimws(qcrange$NOM_SCIENT) == species, ]), col = adjustcolor("black", 0.075), border = NA, add = TRUE)
plot(st_geometry(occs), pch = 16, cex = 1, lwd = 0.25, col = adjustcolor("black", 0.5), add = TRUE)



library(terra)
r <- rast("https://object-arbutus.cloud.computecanada.ca/bq-io/acer/sdm_emv_finance/Hemidactylium_scutatum_rf.tif")
