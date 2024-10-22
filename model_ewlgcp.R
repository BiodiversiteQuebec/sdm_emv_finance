
#remotes:::install_github("frousseu/ewlgcpSDM",auth_token=readLines("C:/Users/rouf1703/github_token"))


library(ewlgcpSDM)
library(rmapshaper)
library(INLA)
#remotes::install_github("frousseu/ewlgcpSDM", auth_token = readLines("/home/rouf1703/github_token_pose")[2])

options(terra.pal=rev(terrain.colors(200)))

### Build mesh
domain <- st_sample(st_buffer(region, 5000), 5000)
domain <- inla.nonconvex.hull(st_coordinates(domain), convex = -0.015,resolution = 75)


pedge <- 0.005
edge <- min(c(diff(st_bbox(region)[c(1, 3)]) * pedge, diff(st_bbox(region)[c(2, 4)]) * pedge))
edge

mesh <- inla.mesh.2d(loc.domain = NULL,
                     max.edge = c(edge, edge*3),
                     min.angle = 21,
                     cutoff = edge/1,
                     offset = c(edge, edge*3),
                     boundary = domain,#inla.mesh.segment(domainloc),
                     crs = st_crs(region))


plan(multicore, workers = 10)
dmesh <- dmesh_mesh(mesh)
#plan(sequential)

### Compute weights
dmesh <- dmesh_weights(dmesh, region)

### Summarize predictors
dmesh <- dmesh_predictors(dmesh, wrap(p))

plan(sequential)

#head(dmesh$predictors)




#params <- dmesh
#dm<-params$dmesh
#dm$deciduous<-params$predictors$deciduous
#plot(dm[,"deciduous"],border=NA,nbreaks=100,pal=function(x){rev(terrain.colors(x))},reset=FALSE,key.pos=NULL)
#plot(st_geometry(qc),border=adjustcolor("black",0.25),add=TRUE)
#plot(st_geometry(lakes),col="white",border=adjustcolor("black",0.25),add=TRUE)


dmesh <- dmesh_effort(dmesh, obs = d[d$pres == 1, ], background = d[d$pres %in% c(0,1), ], buffer = NULL, adjust = FALSE)

#if(params$usepredictors=="Predictors"){
  #f <- paste("y ~",paste(c(vars, "tmax2"),collapse=" + ")) |> as.formula()
  f <- paste("y ~", paste(c("tmean", "tmean2", "water", "water2", "distfsl", "distfsl2", "wetland", "wetland2", "urban", "urban2"), collapse = " + ")) |> as.formula()
  #f <- paste("y ~",paste(c(colnames(dmesh$predictors)[-c(45:48)]),collapse=" + ")) |> as.formula()
#}else{
#  f <- paste("y ~","dummy") |> as.formula()
#}


## Run model

#dmesh$predictors[,1]<-runif(nrow(dmesh$predictors))

#x<-dmesh$dmesh

#x<-st_transform(x, paste0("+units=km +init=epsg:",epsg))
#x<-st_transform(x,st_crs(x)$input)

m <- ewlgcp(
  formula=f,
  dmesh=dmesh,
  effort = TRUE,#if(params$bias=="Bias"){TRUE}else{FALSE},
  adjust = FALSE,
  buffer = FALSE,
  orthogonal = TRUE,
  prior.beta = NULL,#prior.beta<-list(prec=list(default=1/(0.000000001)^2,Intercept=1/(20)^2),mean=list(default=0,Intercept=0)),
  prior.range = c(5000, 0.01),
  prior.sigma = c(1, 0.1),#if(params$spatial=="Spatial"){c(1,0.01)}else{c(0.00001,NA)},
  smooth = 2,
  num.threads = 1:1,
  #blas.num.threads=2,
  control.inla = list(
    strategy = "adaptive", # "adaptive"
    int.strategy = "eb", # "eb"
    huge = FALSE, # apparently ignored
    control.vb = list(
      enable = TRUE,
      verbose = TRUE
    )
  ),# adaptive, eb
  inla.mode = "experimental",
  control.compute = list(config = TRUE, openmp.strategy = "pardiso"),
  verbose = TRUE,
  safe = FALSE
)


## Map results

sdm <- ewlgcpSDM::map(model = m,
                    dmesh = dmesh,
                    dims = c(1000, 1000),
                    region = region
)

#preds<-exp(sdm$linkmean-sdm$spacemean)
#preds<-exp(sdm$linkmean)
#preds<-sdm$spacemean
#preds<-sdm$mean
#preds<-exp(sdm$"link0.5quant")
#preds<-sdm[[c("0.025quant","mean","0.975quant")]]
preds <- exp(sdm[[c("linkmean")]])
preds <- terra::crop(preds, st_buffer(st_convex_hull(occs), 100000))
predictions <- mask(preds, region)
plot(predictions, mar = c(0, 0, 0, 6))
plot(st_geometry(occs), add = TRUE)


write_predictions("ewlgcp")

#params <- dmesh
#dm<-params$dmesh
#dm$val<-params$effort$nobs/params$effort$nbackground
#plot(dm[,"val"],border=NA,nbreaks=100,pal=function(x){rev(terrain.colors(x))},reset=FALSE)
