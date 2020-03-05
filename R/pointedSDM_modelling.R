.libPaths( c( "~/Documents/R/win-library" , .libPaths() ) )
.libPaths()
setwd("~/Documents/GitRepo/lakefish")
library(ggplot2)
library(sf) # for sf objects
library(plyr)
library(dplyr) # for smoother dataframe-manipulation
library(here) # for cleaner filepath-handling
# set_here(path="~/Documents/GitRepo/lakefish")
library(ggmap) # also for nice maps
library(maps)

library(PointedSDMs)
library(sp)
library(spatstat)
library(maptools)
print("Packages loaded.")

Data_survey <- readRDS(here::here("data", "NordicSurvey", "NordicSurveyData_filtered.rds"))

Data_gillnet <- readRDS(here::here("data","TranscribedGillnet","TranscribedGillnet_filtered.rds"))
Data_gillnet <- Data_gillnet[complete.cases(Data_gillnet$individualCount),]
Data_gillnet$Ntrials <- rep(1,nrow(Data_gillnet))

covariateData <- readRDS(here::here("data","Covariates.rds"))
covariateData <- covariateData[complete.cases(covariateData$decimalLatitude,covariateData$decimalLongitude,covariateData$area_km2,covariateData$HFP),]
covariateData <- mutate(covariateData, log_area_km2 = log(area_km2))

norway <- ggplot2::map_data("world", region = "Norway(?!:Svalbard)")
norway <- setdiff(norway, filter(norway, subregion == "Jan Mayen"))

##### MAP #####
Projection <- CRS("+proj=longlat +ellps=WGS84")
norwayfill <- map("world", "norway", fill=TRUE, plot=TRUE, ylim=c(58,72),xlim=c(4,32))
IDs <- sapply(strsplit(norwayfill$names, ":"), function(x) x[1])
norway.poly <- map2SpatialPolygons(norwayfill, IDs = IDs, proj4string = Projection)
# sapply(slot(norway.poly, "polygons"),
#        function(x) length(slot(x, "Polygons")))

##### COVARIATES #####
Use <- c("HFP","eurolst_bio10","log_area_km2") #"area_km2","decimalLongitude","decimalLatitude",
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
                                       data = Data_gillnet[,c("individualCount","species","samplingEffort","Ntrials")],
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
                             tag="survey", InclCoords=TRUE) #polynoms = norway.poly,
stk.gillnet <- MakeBinomStack(observs=Data_gillnet, data=Covariates, mesh=Mesh$mesh, 
                              presname="individualCount", trialname = "Ntrials",
                              tag="gillnet", InclCoords=TRUE) #polynoms = norway.poly,
print("Data stacks created.")

#Formula <- formula(paste(c("resp ~ 0 + log(area_km2)",Use), collapse="+"))
NorwegianModel <- FitModel(stk.survey, stk.gillnet, stk.ip,
                              stk.pred$stk, CovNames = Use, mesh = Mesh$mesh,
                        predictions = TRUE)#, spat.ind = NULL, CovNames=Use)
NorwegianModel.summary <- summary(NorwegianModel$model)$fixed
print("Model fitted.")
#save(NorwegianModel.summary, file = "ModelSummary.RData")

Pred <- SpatialPixelsDataFrame(points=stk.pred$predcoords, data=NorwegianModel$predictions, proj4string=Projection)
Pred@data$precision <- Pred@data$stddev^-2
save.image(file = "NorwegianModelWorkspace.RData")
print("Image saved.")
#load(here::here("NorwegianModelWorkspace.RData"))
