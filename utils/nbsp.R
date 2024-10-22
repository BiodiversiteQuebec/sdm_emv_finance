



library(duckdbfs)
library(dplyr)
library(sf)
library(geodata)
library(rmapshaper)

epsg <- 6623

can <- gadm("CAN", level = 1, path = getwd()) |> st_as_sf()
qc <- can[can$NAME_1 %in% c("Québec"), ] |> st_transform(crs = epsg) |>
        ms_simplify(0.01)

source("https://object-arbutus.cloud.computecanada.ca/bq-io/atlas/parquet/bq-atlas-parquet.R")

atlas <- atlas_remote(parquet_date = "2024-07-09")

occs <- atlas |> 
  filter(class == "Insecta") |> 
  mutate(geom = ST_Point(as.numeric(longitude), as.numeric(latitude))) |> 
  to_sf(crs = 4326) |> 
  collect() |>
  st_transform(epsg)

occs <- occs[qc, ]

res <- 50000
r <- rast(resolution = res, extent = st_bbox(st_buffer(qc, res)))

ids <- cells(r, vect(occs))

nbsp <- cbind(occs, ids) |>
        st_drop_geometry() |>
        _[, c("valid_scientific_name", "cell")] |>
        as.data.table() |>
        unique() |>
        _[, .(n = .N), by = "cell"]

r[as.integer(nbsp$cell)] <- nbsp$n        

plot(r)
plot(st_geometry(qc), add = TRUE)



