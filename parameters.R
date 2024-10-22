



vall <- terra::extract(p, d)[, -1]
voccs <- vall[d$pres == 1, ]

coverage <- apply(voccs, 2, function(i){diff(range(i, na.rm = TRUE))})/apply(vall, 2, function(i){diff(range(i, na.rm = TRUE))})


means <- apply(vall, 2, mean, na.rm = TRUE) |> as.data.frame()
sds <- apply(vall, 2, sd, na.rm = TRUE) |> as.data.frame()
scaling <- cbind(means, sds) |>
  setNames(c("mean", "sd"))

pscaled <- (p - matrix(scaling[, 1], ncol = nlyr(p))) / (matrix(scaling[, 2], ncol = nlyr(p)))