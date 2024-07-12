## eBird status and trends - Download breeding distribution from species
## Changes in bird migration phenology over six decades: a perspective from the Neotropical non-breeding grounds
## Script by Bryam Mateus and Camila Gomez



##Install packages
install.packages(c("tidyverse", "raster", "sf", "ebirdst", "rnaturalearth"))
remotes::install_github("CornellLabofOrnithology/ebirdst")
install.packages("ebirdst")
install.packages("rworldmap")


## Load
library(rnaturalearth)
library(ebirdst)
library(raster)
library(dplyr)
library(tidyverse)
library(sf)
library(sp)
library(rgdal)
library(rworldmap)
library(terra)


#set directory

setwd("your directory")
getwd()

#Map

worldMap <- getMap()

## Color pallets for later
FineGray <- c("gray80", "gray72", "gray64", "gray56", "gray48", "gray42", "gray34", "gray26", "gray18", "gray10", "gray2", "black")
WinteringBlue <- c("#1e90ff","#126ad2" ,"#00308f")
BreedingOrange <- c("#ffc100", "#ff7400", "#ff0000")
Forest <- c("#FFFFFF0A", "lightgreen")
Map <- c("#FFFFFF0A", "orange", "lightgreen", "darkgreen")
Baseline <- c("#FFFFFF0A", "#40E0D06E" )

## Call the elevation layer as we'll use it as base to cut it into our interest region (USA and Canada)

Ele <- raster("AltWorld.tif")
template <- Ele
##  Change coors depending on the season (Breeding vs Wintering)
extent(template) <- c(-200, -20, 20, 80)
extent(template) <- c(-80, -65, -5, 14) #wintering grounds in Colombia
c(-200, -20, -60, 100) #America
c(-200, -20, 20, 80) #Breeding grounds in North America
Elevation <- crop(Ele,template)
plot(Elevation)

## Activate your key to access eBird layers

set_ebirdst_access_key("your key", overwrite = TRUE)


#####  Blackburnian Warbler ########################################################        
dl_path <- ebirdst_download(species = "Blackburnian Warbler") # Change name of species here

## Load rasters that have the surfice of abundance for each week of the year

BKWA <- load_raster(product = "abundance", path = dl_path)


## Weeks for the breeding season for BKWA

BKWA25 <- BKWA[[25]]
BKWA26 <- BKWA[[26]]
BKWA27 <- BKWA[[27]]
BKWA28 <- BKWA[[28]]
BKWA29 <- BKWA[[29]]
BKWA30 <- BKWA[[30]]
BKWA31 <- BKWA[[31]]

## Combine weeks

BKWA_Breeding <- BKWA25+BKWA26+BKWA27+BKWA28+BKWA29+BKWA30+BKWA31
plot(BKWA_Breeding)

## Re-project using the Elevation layer to change it to WGS84

BKWA_Breeding_WGS <- project(BKWA_Breeding, Elevation@crs)
plot(BKWA_Breeding_WGS)

## Change to the region of interest

template <- BKWA_Breeding_WGS
ext(template)<- c(-200, -20, 20, 80)
CW2 <- crop(BKWA_Breeding_WGS,template)
plot(CW2)

## Save the raster

writeRaster(CW2, "BKWA_breeding.tif", overwrite =T)
BKWA_Breeding <- raster("BKWA_Breeding.tif")

## Explore data to standardize

BKWA_Breeding
hist(BKWA_Breeding, ylim=c(0,10000))
quantile(BKWA_Breeding, c(.95, .975, .99)) 

## 99 quantile is 4.497958, we'll use it to standardize the layer in the values between 0 y 1 

## 10% lowest values > NA

BKWA_Breeding1 <- BKWA_Breeding/4.497958  
BKWA_Breeding2 <- calc(BKWA_Breeding1, fun=function(x){ x[x > 1] <- 1; return(x)} )
BKWA_Breeding3 <- calc(BKWA_Breeding2, fun=function(x){ x[x < 0.01] <- NA; return(x)} )

## Save
writeRaster(BKWA_Breeding3, "BKWA_Breeding3.tif", format = "GTiff")

## Map

plot(Elevation, col=FineGray,cex.axis=1.5, legend=FALSE)
plot(BKWA_Breeding3, col = BreedingOrange, add=TRUE, legend=TRUE)
plot(worldMap, add=TRUE, border = "white", cex = 2, lwd = 1.5,  legend=FALSE)
scalebar(2500, xy=c(-160, 20), below = "Kilometers", type='bar', divs=2)

#### Do this for each species ####

#### Here are the weeks for the breeding season for each species based on the status and trends weeks provided by eBird

## Summer Tanager - Piranga rubra

SMTN25 <- SMTN[[25]]
SMTN26 <- SMTN[[26]]
SMTN27 <- SMTN[[27]]
SMTN28 <- SMTN[[28]]
SMTN29 <- SMTN[[29]]
SMTN30 <- SMTN[[30]]
SMTN31 <- SMTN[[31]]
SMTN32 <- SMTN[[32]]
SMTN33 <- SMTN[[33]]
SMTN34 <- SMTN[[34]]

## American Redstart - Setophaga ruticilla

AMRE26 <- AMRE[[26]]
AMRE27 <- AMRE[[27]]
AMRE28 <- AMRE[[28]]
AMRE29 <- AMRE[[29]]

## Yellow Warbler - Setophaga petechia

YEWA26 <- YEWA[[26]]
YEWA27 <- YEWA[[27]]
YEWA28 <- YEWA[[28]]
YEWA29 <- YEWA[[29]]

## Bay-breasted Warbler - Setophaga castanea

BBWA25 <- BBWA[[25]]
BBWA26 <- BBWA[[26]]
BBWA27 <- BBWA[[27]]
BBWA28 <- BBWA[[28]]
BBWA29 <- BBWA[[29]]
BBWA30 <- BBWA[[30]]

## Northern Waterthrush - Parkesia noveboracensis

NOWA25 <- NOWA[[25]]
NOWA26 <- NOWA[[26]]
NOWA27 <- NOWA[[27]]
NOWA28 <- NOWA[[28]]
NOWA29 <- NOWA[[29]]

## Black and White Warbler - Mniotilta varia

BWWA25 <- BWWA[[25]]
BWWA26 <- BWWA[[26]]
BWWA27 <- BWWA[[27]]
BWWA28 <- BWWA[[28]]

## Tennessee Warbler - Leiothlypis peregrina

TEWA26 <- TEWA[[26]]
TEWA27 <- TEWA[[27]]
TEWA28 <- TEWA[[28]]


## Mourning Warbler - Geothlypis philadelphia

MOWA25 <- MOWA[[25]]
MOWA26 <- MOWA[[26]]
MOWA27 <- MOWA[[27]]
MOWA28 <- MOWA[[28]]
MOWA29 <- MOWA[[29]]
MOWA30 <- MOWA[[30]]
MOWA31 <- MOWA[[31]]
MOWA32 <- MOWA[[32]]
MOWA33 <- MOWA[[33]]

## Swainson's Thrush - Catharus ustulatus

SWTH26 <- SWTH[[26]]
SWTH27 <- SWTH[[27]]
SWTH28 <- SWTH[[28]]
SWTH29 <- SWTH[[29]]
SWTH30 <- SWTH[[30]]
SWTH31 <- SWTH[[31]]
SWTH32 <- SWTH[[32]]
SWTH33 <- SWTH[[33]]
SWTH34 <- SWTH[[34]]

## Great Crested Flycatcher - Myiarchus crinitus

GCFL24 <- GCFL[[24]]
GCFL25 <- GCFL[[25]]
GCFL26 <- GCFL[[26]]
GCFL27 <- GCFL[[27]]
GCFL28 <- GCFL[[28]]
GCFL29 <- GCFL[[29]]
GCFL30 <- GCFL[[30]]
GCFL31 <- GCFL[[31]]
GCFL32 <- GCFL[[32]]
GCFL33 <- GCFL[[33]]

## Acadian Flycatcher - Empidonax virescens

ACFL22 <- ACFL[[22]]
ACFL23 <- ACFL[[23]]
ACFL24 <- ACFL[[24]]
ACFL25 <- ACFL[[25]]
ACFL26 <- ACFL[[26]]
ACFL27 <- ACFL[[27]]
ACFL28 <- ACFL[[28]]
ACFL29 <- ACFL[[29]]
ACFL30 <- ACFL[[30]]
ACFL31 <- ACFL[[31]]
