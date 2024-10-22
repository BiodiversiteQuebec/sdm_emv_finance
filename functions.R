write_predictions <- function(method = NULL){
    filename <- paste(species, method) |> 
                  paste0(".tif") |>
                  gsub(" ", "_", x = _)
    names(predictions) <- gsub(".tif", "", filename)              
    writeRaster(predictions, file.path("outputs", filename), overwrite = TRUE)
}