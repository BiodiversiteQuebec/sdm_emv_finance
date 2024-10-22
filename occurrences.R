
library(duckdbfs)
library(dplyr)

source("https://object-arbutus.cloud.computecanada.ca/bq-io/atlas/parquet/bq-atlas-parquet.R")
atlas <- atlas_remote(parquet_date = "2024-07-16")
#atlas <- duckdbfs::open_dataset("/data/sdm_emv_finance/atlas_2024-07-09.parquet", tblname = "atlas")

#atlas <- duckdbfs::open_dataset("/home/frousseu/Downloads/atlas_2024-05-29.parquet", tblname = "atlas")
#atlas <- duckdbfs::open_dataset("/home/frousseu/Downloads/atlas_2024-07-09.parquet", tblname = "atlas")

genus <- strsplit(species, " ")[[1]][1] # temp fix to also get subspecies and string manipulations do not seem to work when dplyr remote

occs <- atlas |> 
  filter(genus == !!genus) |> 
  #filter(valid_scientific_name == species) |> 
  mutate(geom = ST_Point(as.numeric(longitude), as.numeric(latitude))) |> 
  to_sf(crs = 4326) |> 
  collect()

occs <- occs |>
  mutate(species = sapply(strsplit(valid_scientific_name, " "), function(i){paste(i[1:2], collapse = " ")})) |>
  filter(observation_value != "0") |>
  #mutate(species = sub("^(([^ ]+ ){1}[^ ]+).*", "\\1", valid_scientific_name)) |>
  filter(species == !!species)
        

occs$date <- paste(occs$year_obs, formatC(as.integer(occs$month_obs), width = 2, flag = 0), formatC(as.integer(occs$day_obs), width = 2, flag = 0), sep = "-")
if(!all(is.na(period))){
  doy <- substr(occs$date, 6, 10)
  occs <- occs[doy >= period[1] & doy <= period[2], ]
}
occs$coordinate_uncertainty <- as.numeric(occs$coordinate_uncertainty)
th <- 300 # accuracy threshold
table(ifelse(is.na(occs$coordinate_uncertainty), NA, ifelse(occs$coordinate_uncertainty > th, 0, 1)), useNA = "always")

if(FALSE){ # remove large or unknown uncertainties or not
  if(group %in% c("birds", "mammals", "fishes")){
    w <- which(occs$coordinate_uncertainty >= 26000)
    if(any(w)){
      occs <- occs[-w, ] # remove large known or obscured accuracies
    }
  }else{
    occs <- occs[!is.na(occs$coordinate_uncertainty), ]
    occs <- occs[occs$coordinate_uncertainty <= th, ]
  }
}

#occs <- occs[substr(occs$date, 6, 10) >= "06-10" & substr(occs$date, 6, 10) <= "08-10", ]

occs <- st_transform(occs, st_crs(region))




#cec <- rast("/vsicurl/https://object-arbutus.cloud.computecanada.ca/bq-io/io/CEC_land_cover/NA_NALCMS_landcover_2020_30m.tif")


#e <- extract(cec, st_transform(occs, st_crs(cec)))




