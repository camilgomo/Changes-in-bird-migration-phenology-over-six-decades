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

setwd("C:/Users/Usuario/Documents/SELVA/Migration_Phenology")
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

## Call the elevation layer as we'll use it as base to cut it into our interest region (South America)

Ele <- raster("AltWorld.tif")
template <- Ele
##  Change coors depending on the season (Breeding vs Wintering)
#extent(template) <- c(left, right, down, north)

extent(template) <- c(-82, -33, -57, 14)
Elevation <- crop(Ele,template)


## Activate your key to access eBird layers

set_ebirdst_access_key("your key goes here", overwrite = TRUE)


##### EXAMPLE ########################################################        
dl_path <- ebirdst_download(species = "Acadian Flycatcher") # Change name of species here

## Load rasters that have the surfice of abundance for each week of the year

ACFL <- load_raster(product = "abundance", path = dl_path)


## Weeks for the wintering season 

ACFL50 <- ACFL[[50]]
ACFL51 <- ACFL[[51]]
ACFL52 <- ACFL[[52]]
ACFL1 <- ACFL[[1]]
ACFL2 <- ACFL[[2]]
ACFL3 <- ACFL[[3]]
ACFL4 <- ACFL[[4]]
ACFL5 <- ACFL[[5]]
ACFL6 <- ACFL[[6]]
ACFL7 <- ACFL[[7]]
ACFL8 <- ACFL[[8]]

## Combine weeks

ACFL_Wintering <- ACFL50+ACFL51+ACFL52+ACFL1+ACFL2+ACFL3+ACFL4+ACFL5+ACFL6+ACFL7+ACFL8

## Re-project using the Elevation layer to change it to WGS84

ACFL_Wintering_WGS <- project(ACFL_Wintering, Elevation@crs)

## Change to the region of interest

template <- ACFL_Wintering_WGS
ext(template)<- c(-82, -33, -57, 14)
CW2 <- crop(ACFL_Wintering_WGS,template)
plot(CW2)

## Save the raster

writeRaster(CW2, "ACFL_Wintering.tif", overwrite =T)
ACFL_Wintering <- raster("ACFL_Wintering.tif")

## Explore data to standardize

ACFL_Wintering
hist(ACFL_Wintering, ylim=c(0,10000))
quantile(ACFL_Wintering, c(.95, .975, .99)) 

## 99 quantile is 4.497958, we'll use it to standardize the layer in the values between 0 y 1 

## 10% lowest values > NA

ACFL_Wintering1 <- ACFL_Wintering/1.351928               
ACFL_Wintering2 <- calc(ACFL_Wintering1, fun=function(x){ x[x > 1] <- 1; return(x)} )
ACFL_Wintering3 <- calc(ACFL_Wintering2, fun=function(x){ x[x < 0.01] <- NA; return(x)} )

## Save
writeRaster(ACFL_Wintering3, "ACFL_Wintering3.tif", format = "GTiff")

## Map

plot(Elevation, col=FineGray,cex.axis=1.5, legend=FALSE)
plot(BBWA_Wintering3, col = WinteringBlue, add=TRUE, legend=TRUE)
plot(worldMap, add=TRUE, border = "white", cex = 2, lwd = 1.5,  legend=FALSE)
scalebar(2500, xy=c(-160, 20), below = "Kilometers", type='bar', divs=2)

#### Do this for each species ####

#### Here are the weeks for the wintering season for each species based on the status and trends weeks provided by eBird

## Blackburnian Warbler - Setophaga fusca

BKWA50 <- BKWA[[50]]
BKWA51 <- BKWA[[51]]
BKWA52 <- BKWA[[52]]
BKWA1 <- BKWA[[1]]
BKWA2 <- BKWA[[2]]
BKWA3 <- BKWA[[3]]
BKWA4 <- BKWA[[4]]
BKWA5 <- BKWA[[5]]
BKWA6 <- BKWA[[6]]
BKWA7 <- BKWA[[7]]
BKWA8 <- BKWA[[8]]
BKWA9 <- BKWA[[9]]
BKWA10 <- BKWA[[10]]

## Summer Tanager - Piranga rubra

SMTN48 <- SMTN[[48]]
SMTN49 <- SMTN[[49]]
SMTN50 <- SMTN[[50]]
SMTN51 <- SMTN[[51]]
SMTN52 <- SMTN[[52]]
SMTN1 <- SMTN[[1]]
SMTN2 <- SMTN[[2]]
SMTN3 <- SMTN[[3]]
SMTN4 <- SMTN[[4]]
SMTN5 <- SMTN[[5]]
SMTN6 <- SMTN[[6]]
SMTN7 <- SMTN[[7]]
SMTN8 <- SMTN[[8]]
SMTN9 <- SMTN[[9]]

## American Redstart - Setophaga ruticilla

AMRE49 <- AMRE[[49]]
AMRE50 <- AMRE[[50]]
AMRE51 <- AMRE[[51]]
AMRE52 <- AMRE[[52]]
AMRE1 <- AMRE[[1]]
AMRE2 <- AMRE[[2]]
AMRE3 <- AMRE[[3]]
AMRE4 <- AMRE[[4]]
AMRE5 <- AMRE[[5]]
AMRE6 <- AMRE[[6]]
AMRE7 <- AMRE[[7]]
AMRE8 <- AMRE[[8]]
AMRE9 <- AMRE[[9]]
AMRE10 <- AMRE[[10]]
AMRE11 <- AMRE[[11]]
AMRE12 <- AMRE[[12]]
AMRE13 <- AMRE[[13]]

## Yellow Warbler - Setophaga petechia

YEWA47 <- YEWA[[47]]
YEWA48 <- YEWA[[48]]
YEWA49 <- YEWA[[49]]
YEWA50 <- YEWA[[50]]
YEWA51 <- YEWA[[51]]
YEWA52 <- YEWA[[52]]
YEWA1 <- YEWA[[1]]
YEWA2 <- YEWA[[2]]
YEWA3 <- YEWA[[3]]
YEWA4 <- YEWA[[4]]
YEWA5 <- YEWA[[5]]
YEWA6 <- YEWA[[6]]
YEWA7 <- YEWA[[7]]
YEWA8 <- YEWA[[8]]

## Bay-breasted Warbler - Setophaga castanea

BBWA47 <- BBWA[[47]]
BBWA48 <- BBWA[[48]]
BBWA49 <- BBWA[[49]]
BBWA50 <- BBWA[[50]]
BBWA51 <- BBWA[[51]]
BBWA52 <- BBWA[[52]]
BBWA1 <- BBWA[[1]]
BBWA2 <- BBWA[[2]]
BBWA3 <- BBWA[[3]]
BBWA4 <- BBWA[[4]]
BBWA5 <- BBWA[[5]]
BBWA6 <- BBWA[[6]]
BBWA7 <- BBWA[[7]]
BBWA8 <- BBWA[[8]]
BBWA9 <- BBWA[[9]]
BBWA10 <- BBWA[[10]]
BBWA11 <- BBWA[[11]]
BBWA12 <- BBWA[[12]]
BBWA13 <- BBWA[[13]]


## Northern Waterthrush - Parkesia noveboracensis

NOWA45 <- NOWA[[45]]
NOWA46 <- NOWA[[46]]
NOWA47 <- NOWA[[47]]
NOWA48 <- NOWA[[48]]
NOWA49 <- NOWA[[49]]
NOWA50 <- NOWA[[50]]
NOWA51 <- NOWA[[51]]
NOWA1 <- NOWA[[1]]
NOWA2 <- NOWA[[2]]
NOWA3 <- NOWA[[3]]
NOWA4 <- NOWA[[4]]
NOWA5 <- NOWA[[5]]
NOWA6 <- NOWA[[6]]
NOWA7 <- NOWA[[7]]
NOWA8 <- NOWA[[8]]
NOWA9 <- NOWA[[9]]
NOWA10 <- NOWA[[10]]
NOWA11 <- NOWA[[11]]
NOWA12 <- NOWA[[12]]
NOWA13 <- NOWA[[13]]

NOWA_Wintering <- NOWA45+NOWA46+NOWA47+NOWA48+NOWA49+NOWA50+NOWA51+NOWA1+NOWA2+NOWA3+NOWA4+NOWA5+NOWA6+NOWA7+NOWA8+NOWA9+NOWA10+NOWA11+NOWA12+NOWA13
plot(NOWA_Wintering)

## Black and White Warbler - Mniotilta varia

BWWA47 <- BWWA[[47]]
BWWA48 <- BWWA[[48]]
BWWA49 <- BWWA[[49]]
BWWA50 <- BWWA[[50]]
BWWA51 <- BWWA[[51]]
BWWA52 <- BWWA[[52]]
BWWA1 <- BWWA[[1]]
BWWA2 <- BWWA[[2]]
BWWA3 <- BWWA[[3]]
BWWA4 <- BWWA[[4]]
BWWA5 <- BWWA[[5]]
BWWA6 <- BWWA[[6]]
BWWA7 <- BWWA[[7]]
BWWA8 <- BWWA[[8]]

BWWA_Wintering <- BWWA47+BWWA48+BWWA49+BWWA50+BWWA51+BWWA52+BWWA1+BWWA2+BWWA3+BWWA4+BWWA5+BWWA6+BWWA7+BWWA8

## Tennessee Warbler - Leiothlypis peregrina

TEWA50 <- TEWA[[50]]
TEWA51 <- TEWA[[51]]
TEWA52 <- TEWA[[52]]
TEWA1 <- TEWA[[1]]
TEWA2 <- TEWA[[2]]
TEWA3 <- TEWA[[3]]
TEWA4 <- TEWA[[4]]
TEWA5 <- TEWA[[5]]
TEWA6 <- TEWA[[6]]
TEWA7 <- TEWA[[7]]
TEWA8 <- TEWA[[8]]
TEWA9 <- TEWA[[9]]
TEWA10 <- TEWA[[10]]
TEWA11 <- TEWA[[11]]

TEWA_Wintering <- TEWA50+TEWA51+TEWA52+TEWA1+TEWA2+TEWA3+TEWA4+TEWA5+TEWA6+TEWA7+TEWA8+TEWA9+TEWA10+TEWA11

## Mourning Warbler - Geothlypis philadelphia

MOWA46 <- MOWA[[46]]
MOWA47 <- MOWA[[47]]
MOWA48 <- MOWA[[48]]
MOWA49 <- MOWA[[49]]
MOWA50 <- MOWA[[50]]
MOWA51 <- MOWA[[51]]
MOWA52 <- MOWA[[52]]
MOWA1 <- MOWA[[1]]
MOWA2 <- MOWA[[2]]
MOWA3 <- MOWA[[3]]
MOWA4 <- MOWA[[4]]
MOWA5 <- MOWA[[5]]
MOWA6 <- MOWA[[6]]
MOWA7 <- MOWA[[7]]
MOWA8 <- MOWA[[8]]
MOWA9 <- MOWA[[9]]
MOWA10 <- MOWA[[10]]
MOWA11 <- MOWA[[11]]
MOWA12 <- MOWA[[12]]
MOWA13 <- MOWA[[13]]

MOWA_Wintering <- MOWA46+MOWA47+MOWA48+MOWA49+MOWA50+MOWA51+MOWA52+MOWA1+MOWA2+MOWA3+MOWA4+MOWA5+MOWA6+MOWA7+MOWA8+MOWA9+MOWA10+MOWA11+MOWA12+MOWA13

## Swainson's Thrush - Catharus ustulatus

SWTH48 <- SWTH[[48]]
SWTH49 <- SWTH[[49]]
SWTH50 <- SWTH[[50]]
SWTH51 <- SWTH[[51]]
SWTH52 <- SWTH[[53]]
SWTH1 <- SWTH[[1]]
SWTH2 <- SWTH[[2]]
SWTH3 <- SWTH[[3]]
SWTH4 <- SWTH[[4]]
SWTH5 <- SWTH[[5]]
SWTH6 <- SWTH[[6]]
SWTH7 <- SWTH[[7]]
SWTH8 <- SWTH[[8]]

SWTH_Wintering <- SWTH48+SWTH49+SWTH50+SWTH51+SWTH52+SWTH1+SWTH2+SWTH3+SWTH4+SWTH5+SWTH6+SWTH7+SWTH8

## Great Crested Flycatcher - Myiarchus crinitus

GCFL44 <- GCFL[[44]]
GCFL45 <- GCFL[[45]]
GCFL46 <- GCFL[[46]]
GCFL47 <- GCFL[[47]]
GCFL48 <- GCFL[[48]]
GCFL49 <- GCFL[[49]]
GCFL50 <- GCFL[[50]]
GCFL51 <- GCFL[[51]]
GCFL52 <- GCFL[[52]]
GCFL1 <- GCFL[[1]]
GCFL2 <- GCFL[[2]]
GCFL3 <- GCFL[[3]]
GCFL4 <- GCFL[[4]]
GCFL5 <- GCFL[[5]]
GCFL6 <- GCFL[[6]]
GCFL7 <- GCFL[[7]]
GCFL8 <- GCFL[[8]]
GCFL9 <- GCFL[[9]]
GCFL10 <- GCFL[[10]]

GCFL_Wintering <- GCFL44+GCFL45+GCFL46+GCFL47+GCFL48+GCFL49+GCFL50+GCFL51+GCFL52+GCFL1+GCFL2+GCFL3+GCFL4+GCFL5+GCFL6+GCFL7+GCFL8+GCFL9+GCFL10


## Acadian Flycatcher - Empidonax virescens

ACFL50 <- ACFL[[50]]
ACFL51 <- ACFL[[51]]
ACFL52 <- ACFL[[52]]
ACFL1 <- ACFL[[1]]
ACFL2 <- ACFL[[2]]
ACFL3 <- ACFL[[3]]
ACFL4 <- ACFL[[4]]
ACFL5 <- ACFL[[5]]
ACFL6 <- ACFL[[6]]
ACFL7 <- ACFL[[7]]
ACFL8 <- ACFL[[8]]

ACFL_Wintering <- ACFL50+ACFL51+ACFL52+ACFL1+ACFL2+ACFL3+ACFL4+ACFL5+ACFL6+ACFL7+ACFL8
