

print(paste("Starting", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))

source("functions.r")
source("prelim.R")
source("predictors.R")
source("inputs.r")

for(ii in 1:nrow(species_list)){

  species <- species_list$species[ii]
  group <- species_list$group[ii]
  period <- c(species_list$start[ii], species_list$end[ii])
  
  t1 <- Sys.time()
  print(paste("Start", paste(ii,"/", nrow(species_list)), species, format(t1, "%H:%M:%S"), sep = " - "))

  source("occurrences.R")
  
  if(nrow(occs) < 10){
    next
  }

  if(nrow(occs[region, ])/nrow(occs) <= 0.2){
    next
  }

  source("background.R")
  source("parameters.r")
  source("model_glmbinomial.R")
  source("model_rf.R")

  t2 <- Sys.time()
  td <- round(as.numeric(difftime(t2, t1, units = "mins")), 1)
  print(paste("Done", paste(ii,"/", nrow(species_list)), species, format(t2, "%H:%M:%S"), paste(td, "minutes"), sep = " - "))

}

print(paste("Completed", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))





