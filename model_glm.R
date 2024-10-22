
library(ggeffects)
library(patchwork)
library(ggplot2)
library(car)



vars <- names(coverage)[which(coverage >= 0.25)]
vars <- unique(c(vars, "tmean", "prec", "elevation", "geomflat", "distfsl", "distroads", "y"))
#vars <- vars[-grep("mixed", vars)]
vars <- vars[-grep("distroads", vars)]

means <- apply(e2, 2, mean, na.rm = TRUE) |> as.data.frame()
sds <- apply(e2, 2, sd, na.rm = TRUE) |> as.data.frame()
scaling <- cbind(means, sds) |>
  setNames(c("mean", "sd"))

pscaled <- (p - matrix(scaling[, 1], ncol = nlyr(p))) / (matrix(scaling[, 2], ncol = nlyr(p)))

e <- extract(pscaled, st_transform(d, st_crs(pscaled)))
dat <- st_drop_geometry(d)
dat <- cbind(dat, e[, -1])

dat <- dat[, vars]

#dat2 <- lapply(dat, function(i){i^2})
#names(dat2) <- paste0(names(dat), 2)
#dat <- cbind(dat, dat2)
f <- as.formula(paste('pres ~', paste('poly(', colnames(dat),',2)', collapse = ' + ')))
#dat2 <- lapply(dat, function(i){i^2}) |> as.data.frame()
#nanmes(dat2)
dat$pres <- d$pres
dat <- na.omit(dat)

#f <- formula(pres ~ .)
#f <- formula(pres ~ poly(tmean, 2) + .)
m <- glm(f, data = dat, family = binomial)
summary(m)
gg <- lapply(all.vars(terms(m)[[3]]), function(i){ggpredict(m, terms = paste(i, "[all]"))})
lims <- range(c(do.call("rbind", gg)$predicted, do.call("rbind", gg)$predicted))
g <- lapply(gg, plot, limit_range = TRUE, limits = lims, show_ci = FALSE, show_data = FALSE, jitter = TRUE, dot_alpha = 0.05, colors = "tomato")#, show_data = TRUE, jitter = TRUE)
wrap_plots(g)

varimp <- sort(coef(m)[-1])
par(mar=c(4,8,1,1))
plot(varimp, as.integer(factor(names(varimp), levels = names(varimp))), xaxt = "n", yaxt = "n", xlab = "", ylab = "")
axis(1, at = pretty(varimp))
axis(2, at = as.integer(factor(names(varimp), levels = names(varimp))), labels = names(varimp), las = 2)
mtext(side = 1, line = 2, "coef")
mtext(side = 2, line = 2, "var")

#rev(sort(colMeans(extract(p, st_transform(occs, st_crs(p)))[, -1])))


pp <- terra::crop(pscaled, st_buffer(occs, 200000))
newdata <- values(pp, dataframe = TRUE)
ppp <- predict(m, newdata = newdata, type = "response")
ppp_glm <- setValues(pp[[1]], ppp)
plot(ppp_glm, axes = FALSE)
#plot(st_geometry(d), pch = 16, col = adjustcolor("black", 0.2), cex = 0.5, lwd = 0.2, add = TRUE)
plot(st_geometry(occs), cex = 0.5, lwd = 0.2, add = TRUE)
plot(st_geometry(region), lwd = 0.1, add = TRUE)
#plot(st_geometry(st_buffer(occs, dist = occs$coordinate_uncertainty)), cex = 0.5, lwd = 0.2, add = TRUE)


#occs[rev(order(st_coordinates(occs)[,2])),][1:10,]


#r <- rast(matrix(1:25, ncol = 5))
#r <- c(r, 2 * r)
#r
#r - matrix(c(1, 5), ncol = 2)
#e1 <- extract(r, 1:ncell(r))







  