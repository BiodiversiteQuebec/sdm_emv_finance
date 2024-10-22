

library(data.table)
library(ebirdst)

eb <- ebirdst_runs |>
      as.data.table() |>
      _[ , .(scientific_name, breeding_start, breeding_end)] |>
      _[ , start := substr(breeding_start, 6, 10)] |> 
      _[ , end := substr(breeding_end, 6, 10)] |> 
      _[ , species := scientific_name] |>
      _[ , scientific_name := NULL]

classes <- c("Amphibia", "Reptilia", "Aves", "Chondrichthyes", "Petromyzontida", "Actinopterygii", "Mammalia")
replacement <- c("reptiles", "reptiles", "birds", "fishes", "fishes", "fishes", "mammals")

# https://www.donneesquebec.ca/recherche/dataset/liste-de-la-faune-vertebree-du-quebec
emv <- fread("/data/sdm_emv_finance/LFVQ_18_04_2024.csv") |>
         #_[ , .(Nom_francais, Nom_scientifique, GRAND_GROUPE, CLASSE, ORDRE, SOUS_ESPECE_POP, Rang_S, STATUT_LEMV)] |>
         _[STATUT_LEMV %in% c("Menacée", "Susceptible", "Vulnérable"), ] |>
         _[, species := sapply(strsplit(Nom_scientifique, " "), function(i){paste(i[1:2], collapse = " ")})] |>
         _[ -grep("Requin|Marsouin|Rorqual|Phoque|Baleine|Béluga", Nom_francais), ] |>
         _[ , group := replacement[match(CLASSE, classes)]] |>
         _[ , statut := STATUT_LEMV] |>
         _[ , .(species, statut, group)] |>
         unique()

emv <- eb[emv, on = .(species)] |>
       _[ , .(species, group, statut, start, end)]

species_list <- as.data.frame(emv)

#species_list <- species_list[c(39:40), ]



