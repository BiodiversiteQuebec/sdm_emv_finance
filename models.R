
library(ggeffects)
library(patchwork)
library(ggplot2)
library(car)


pp <- terra::crop(p, st_buffer(occs, 300000))

e1 <- terra::extract(p, occs)[, -1]
e2 <- terra::extract(p, d)[, -1]

coverage <- apply(e1, 2, function(i){diff(range(i, na.rm = TRUE))})/apply(e2, 2, function(i){diff(range(i, na.rm = TRUE))})
vars <- names(coverage)[which(coverage >= 0.25)]
vars <- unique(c(vars, "tmean", "prec", "elevation", "geomflat", "distfsl"))
vars <- vars[-grep("mixed", vars)]
dat <- st_drop_geometry(d[, vars])

#dat2 <- lapply(dat, function(i){i^2})
#names(dat2) <- paste0(names(dat), 2)
#dat <- cbind(dat, dat2)
f <- as.formula(paste('pres ~', paste('poly(', colnames(dat),',2)', collapse = ' + ')))
#dat2 <- lapply(dat, function(i){i^2}) |> as.data.frame()
#nanmes(dat2)
dat$pres <- d$pres
dat <- na.omit(dat)

#f <- formula(pres ~ x * y + poly(x, 2) + poly(y, 2) + poly(x * y, 2))
m <- glm(f, data = dat, family = binomial)
m <- ranger(pres ~ ., data = dat, probability = TRUE, importance = "impurity")
summary(m)
gg <- lapply(all.vars(terms(m)[[3]]), function(i){ggpredict(m, terms = paste(i, "[all]"))})
lims <- range(c(do.call("rbind", gg)$predicted, do.call("rbind", gg)$predicted))
g <- lapply(gg, plot, limit_range = TRUE, limits = lims)#, show_data = TRUE, jitter = TRUE)
wrap_plots(g)

#rev(sort(colMeans(extract(p, st_transform(occs, st_crs(p)))[, -1])))
newdata <- values(pp, dataframe = TRUE)
ppp <- predict(m, newdata = newdata, type = "response")
ppp <- setValues(pp[[1]], ppp)
plot(ppp, axes = FALSE)
plot(st_geometry(occs), cex = 0.5, lwd = 0.2, add = TRUE)
plot(st_geometry(st_buffer(occs,dist = occs$coordinate_uncertainty)), cex = 0.5, lwd = 0.2, add = TRUE)


occs[rev(order(st_coordinates(occs)[,2])),][1:10,]






     