
library(ggeffects)
library(patchwork)
library(ggplot2)
library(car)


vars <- names(coverage)

dat <- st_drop_geometry(d)
dat <- cbind(dat, vall)

dat <- dat[, vars]

dat$pres <- d$pres
dat <- na.omit(dat)

m <- ranger(pres ~ ., data = dat, probability = TRUE, importance = "impurity")
rev(sort(importance(m)))


#rev(sort(colMeans(extract(p, st_transform(occs, st_crs(p)))[, -1])))
pp <- terra::crop(p, st_buffer(occs, 400000))
newdata <- values(pp, dataframe = TRUE)
newdata$id <- 1:nrow(newdata)
newdata2 <- na.omit(newdata)
ppp <- rep(NA, nrow(newdata))

nl <- 1:nrow(newdata2)
nl <- split(nl, ceiling(seq_along(nl) / 500000))
ppp[newdata2$id] <- lapply(seq_along(nl), function(i) {
  cat("\r", paste(i, "/", length(nl))); flush.console()
  predict(m, data = newdata2[nl[[i]], ], type = "response", num.threads = 5)$predictions[, 1]
}) |> unlist()

# predict(m, data = newdata2, type = "response", num.threads = 1)
predictions <- setValues(pp[[1]], ppp)
plot(predictions, axes = FALSE, legend = TRUE)
plot(st_geometry(occs), cex = 0.5, lwd = 0.2, add = TRUE)
#plot(st_geometry(st_buffer(occs,dist = occs$coordinate_uncertainty)), cex = 0.5, lwd = 0.2, add = TRUE)
plot(st_geometry(region), cex = 0.5, lwd = 0.2, add = TRUE)

#occs[rev(order(st_coordinates(occs)[,2])),][1:10,]


write_predictions("rf")



