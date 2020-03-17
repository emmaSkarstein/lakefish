.libPaths( c( "~/Documents/R/win-library" , .libPaths() ) )
.libPaths()
setwd("~/Documents/GitRepo/lakefish")
library(ggplot2)
library(sf) # for sf objects
library(plyr)
library(dplyr) # for smoother dataframe-manipulation
library(here) # for cleaner filepath-handling
library(ggmap) # also for nice maps
library(maps)

library(PointedSDMs)
library(sp)
library(spatstat)
library(maptools)

######## MakePoissonStack #########
MakePoissonStack=function(data, observs, tag="poisson", intercept=TRUE, mesh, presname="NPres", effortname=NULL,
                        coordnames=NULL, InclCoords=FALSE, polynoms = NULL, scale = FALSE) {
  
  if(length(presname)>1) stop("more than one name given for presences column")
  # if(length(trialname)>1) stop("more than one name given for number of trials column")
  if(!presname%in%names(observs@data)) stop(paste(presname," not in names of presences data frame", sep=""))
  # if(!is.logical(observs@data[,presname]) & !trialname%in%names(observs@data))
  #   stop(paste(trialname," not in names of presences data frame", sep=""))
  
  if(is.null(coordnames)) coordnames <- colnames(data@coords)
  if(!is.null(polynoms)) {
    if(class(polynoms) != "SpatialPolygonsDataFrame" & class(polynoms) != "SpatialPolygons")
      stop("polynoms should be a spatial polygon")
  }
  
  NearestCovs <- GetNearestCovariate(points=observs, covs=data)
  if(InclCoords) {    data@data[,coordnames] <- data@coords  }
  if(intercept) NearestCovs@data[,paste("int",tag,sep=".")] <- 1 # add intercept
  if(!is.null(polynoms)) {
    NearestCovs <- AddDistToRangeToSpatialPoints(data = NearestCovs, polynoms = polynoms, scale=scale)
  }
  
  # If presences are Boolean, reformat
  if(is.logical(observs@data[,presname])) {
    observs@data[,presname] <- as.integer(observs@data[,presname])
    # observs@data[,trialname] <- rep(1, nrow(observs@data))
  }
  # if(is.null(effortname)) {
  #   E <- rep(0,nrow(NearestCovs))
  # }else {
  #   E <- observs@data[,effortname]
  # }
  # Projector matrix from mesh to data.
  projmat <- inla.spde.make.A(mesh, as.matrix(NearestCovs@coords)) # from mesh to point observations
  stk.poisson <- inla.stack(data=list(resp=cbind(observs@data[,presname],NA), e=rep(0,nrow(NearestCovs))), A=list(1,projmat), tag=tag,
                          effects=list(NearestCovs@data, list(i=1:mesh$n))) #, Ntrials=observs@data[,trialname]
  
  stk.poisson
}
#################

print("Packages loaded.")

Data_survey <- readRDS(here::here("data", "NordicSurvey", "NordicSurveyData_filtered.rds"))

Data_gillnet <- readRDS(here::here("data","TranscribedGillnet","TranscribedGillnet_filtered.rds"))
Data_gillnet <- Data_gillnet[complete.cases(Data_gillnet$individualCount),]
#Data_gillnet$Ntrials <- rep(1,nrow(Data_gillnet))

Data_gbif <- readRDS(here::here("data","GBIF","GBIF_filtered.rds"))

covariateData <- readRDS(here::here("data","Covariates.rds"))
covariateData <- covariateData[complete.cases(covariateData$decimalLatitude,covariateData$decimalLongitude,covariateData$area_km2,covariateData$HFP),]
covariateData <- mutate(covariateData, log_area_km2 = log(area_km2))

norway <- ggplot2::map_data("world", region = "Norway(?!:Svalbard)")
norway <- setdiff(norway, filter(norway, subregion == "Jan Mayen"))

##### MAP #####
Projection <- CRS("+proj=longlat +ellps=WGS84")
norwayfill <- map("world", "norway", fill=TRUE, plot=FALSE, ylim=c(58,72),xlim=c(4,32))
IDs <- sapply(strsplit(norwayfill$names, ":"), function(x) x[1])
norway.poly <- map2SpatialPolygons(norwayfill, IDs = IDs, proj4string = Projection)
# sapply(slot(norway.poly, "polygons"),
#        function(x) length(slot(x, "Polygons")))

##### COVARIATES #####
Use <- c("HFP","eurolst_bio10","decimalLongitude","decimalLatitude", "log_area_km2")
Covariates <- SpatialPointsDataFrame(covariateData[,c("decimalLongitude","decimalLatitude")],
                                     data=covariateData[,Use], proj4string = Projection)
Covariates@data <-data.frame(apply(Covariates@data,2, scale))  # scale the covariates
#spplot(Covariates, layout=c(4,1), col.regions=brewer.pal(6, "Blues")[-1], key.space="right", pretty=TRUE)
print("Covariates loaded.")
##### DATA #####
Data_survey <- SpatialPointsDataFrame(Data_survey[,c("decimalLongitude","decimalLatitude")], 
                                       data = Data_survey[,c("occurrenceStatus","species")],
                                       proj4string = Projection)

Data_gillnet <- SpatialPointsDataFrame(Data_gillnet[,c("decimalLongitude","decimalLatitude")], 
                                       data = Data_gillnet[,c("individualCount","species","samplingEffort")],
                                       proj4string = Projection)
Data_gbif <- SpatialPointsDataFrame(Data_gbif[,c("decimalLongitude","decimalLatitude")],
                                    data = Data_gbif[,c("species")],
                                    proj4string = Projection)
print("Data loaded.")
# Data_survey.colors <- c(gsub(TRUE,"red",Data_survey$occurrenceStatus))
# Data_survey.colors <- gsub(FALSE,"blue",Data_survey.colors)
# #MapCols <- c(brewer.pal(4, "Paired"), grey(c(0.4,0.1)))
# MapCols <- c("blue", "red", "grey70")
# names(MapCols) <- c("Survey, absent", "Survey, present", "Region")
# par(mar=rep(0,4))
# plot(norway.poly, col=MapCols["Region"])
# points(Data_survey@coords, cex=0.7, pch=19, col=Data_survey.colors)
# legend(norway.poly@bbox["x","min"]+0.01*diff(norway.poly@bbox["x",]),
#        norway.poly@bbox["y","min"]+0.95*diff(norway.poly@bbox["y",]),
#        legend = names(MapCols), fill = MapCols, cex=0.8)

##### MESHGRID #####
Meshpars <- list(cutoff=0.08, max.edge=c(1, 3), offset=c(1,1))
Mesh <- MakeSpatialRegion(data=NULL, bdry=norway.poly, meshpars=Meshpars,
                          proj = Projection)
stk.ip <- MakeIntegrationStack(mesh=Mesh$mesh, data=Covariates, area=Mesh$w, 
                               tag='ip', InclCoords=TRUE)
print("Mesh created.")
##### PREDICTIONS #####
Nxy.scale <- 0.1 # use this to change the resolution of the predictions
Nxy <- round(c(diff(norway.poly@bbox[1,]), diff(norway.poly@bbox[2,]))/Nxy.scale)
stk.pred <- MakeProjectionGrid(nxy=Nxy, mesh=Mesh$mesh, data=Covariates, 
                               tag='pred',boundary=norway.poly)
print("Prediction stack created.")
##### STACKING AND MODELLING #####
stk.survey <- MakeBinomStack(observs=Data_survey, data=Covariates, mesh=Mesh$mesh,
                             presname="occurrenceStatus",
                             tag="survey", InclCoords=TRUE)
stk.gillnet <- MakePoissonStack(observs=Data_gillnet, data=Covariates, mesh=Mesh$mesh, 
                              presname="individualCount", effortname = "samplingEffort",
                              tag="gillnet", InclCoords=TRUE)
stk.gbif <- MakePointsStack(presences = )
print("Data stacks created.")
###########
FitModelTest <- function(..., formula=NULL, CovNames=NULL, mesh, spat.ind = "i", predictions=FALSE, tag.pred="pred",
                     control.fixed=NULL, waic=FALSE, dic=FALSE) {
  
  stck <- inla.stack(...)
  
  if(is.null(CovNames)) {
    CovNames <- unlist(stck$effects$names)
    CovNames <- CovNames[!CovNames%in%c(spat.ind)]
  } else {
    if(!is.null(formula)) {
      warning("CovNames and formula are both not NULL: CovNames will be ignored")
    }
  }
  
  mesh <- inla.spde2.matern(mesh)
  
  if(!is.null(spat.ind)) {
    CovNames <- c(CovNames, paste0("f(", spat.ind, ", model=mesh)"))
  }
  # I'm sure there's a nicer way of doing this, but ... won't work
  if(is.null(control.fixed)) {
    control.fixed <- list(mean=0)
  }
  
  if(is.null(formula)) {
    Formula <- formula(paste(c("resp ~ 0 ", CovNames), collapse="+"))
  } else {
    if(is.null(spat.ind)) {
      Formula <- formula
    } else {
      if(any(grepl(paste0('(', spat.ind, ','), formula, fixed=TRUE))) {
        warning(paste0(spat.ind, " already in formula, so will be ignored"))
      } else {
        Formula <- update(formula, paste0(" ~ . + f(", spat.ind, ", model=mesh)"))    }
    }
  }
  
  # Fit model including predictions
  mod <- inla(Formula, family=c('poisson','binomial'),
              control.family = list(list(link = "log"), list(link = "cloglog")),
              data=inla.stack.data(stck), verbose=TRUE,
              control.results=list(return.marginals.random=FALSE,
                                   return.marginals.predictor=FALSE),
              control.predictor=list(A=inla.stack.A(stck), link=NULL, compute=TRUE),
              control.fixed=control.fixed,
              E=inla.stack.data(stck)$e,
              control.compute=list(waic=waic, dic=dic)) #Ntrials=inla.stack.data(stck)$Ntrials,
  
  if(predictions) {
    id <- inla.stack.index(stck,tag.pred)$data
    pred <- data.frame(mean=mod$summary.fitted.values$mean[id],
                       stddev=mod$summary.fitted.values$sd[id])
    res <- list(model=mod, predictions=pred)
  } else {
    res <- mod
  }
  res
}
###########
NorwegianModel <- FitModelTest(stk.gillnet, stk.survey, stk.ip, stk.pred$stk, 
                               CovNames = Use, mesh = Mesh$mesh, predictions = TRUE) #, spat.ind = NULL
NorwegianModel.summary <- summary(NorwegianModel$model)$fixed
print("Model fitted.")
#save(NorwegianModel.summary, file = "ModelSummary.RData")

Pred <- SpatialPixelsDataFrame(points=stk.pred$predcoords, data=NorwegianModel$predictions, proj4string=Projection)
Pred@data$precision <- Pred@data$stddev^-2
#temp <- gsub(" ", "_", paste0("NorwegianModelWorkspace", Sys.time(),".RData"), fixed = TRUE)
#save.image(file = temp)
save.image(file = "WS.Rdata")
print("Image saved.")
#load(here::here("Workspaces","NorwegianModelWorkspace6.RData"))
#load("WS.RData")
