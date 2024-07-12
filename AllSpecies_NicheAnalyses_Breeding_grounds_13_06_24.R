### Niche comparisons per species, breeding grounds, from: 
## Changes in bird migration phenology over six decades: a perspective from the Neotropical non-breeding grounds
## Script by Bryam Mateus and Camila Gomez
## Adapted from: Broennimann, O., et al (2015)



### First, install packages
library(devtools)
library(ecospat)
library(ade4)
library(raster)
library(rworldmap)
library(sf)
library(dplyr)
library(terra)
library(ggplot2)
library(ggpubr)


### Set library
setwd("Your library")
getwd()

### Get the WorldClim .tif files with the variables
tif_directory <- "your data" ## your data goes here

tif_files <- list.files(tif_directory, pattern = "\\.tif$", full.names = TRUE)

clim <- stack(tif_files)

var<-c("alt","aspect","seapre_s","seatemp_s","slope","tmax_s", "tmin_s")
clim<-clim[[which(names(clim)%in%var)]]

print(clim)

#Get Background climate
# create mask fo area background
ctry = c("CAN", "USA")
bkg.nam<-aggregate(countriesCoarseLessIslands[countriesCoarseLessIslands$ADM0_A3%in%ctry,])

# extract backgound climate data from the rasters 
clim.bkg.nam<-mask(crop(clim,bbox(bkg.nam)),bkg.nam) #se demora en correr
print(clim.bkg.nam)

#This was a template to generate the random coordenates in ArcMap
#x <- rast(clim.bkg.nam)
#terra::writeRaster(x, "backgr.tif")

#plot
plot(clim.bkg.nam)

## Coordinates for climatic space available in all North America "background" (it is used to calibrate the PCA with available climatic space vs the climate space that the species uses)

bkgr.coords <- as.data.frame(read_sf("/BKG_coords.shp")) 

bkgr_xy <- bkgr.coords %>% select(Lat, Lon) %>% mutate(x = Lon, y = Lat) %>% select(x,y)



#### All Species ####
## Here we get all the coords in .shp file
## .shp files for every species available at: https://github.com/camilgomo/Fenologia-de-la-migracion-en-Colombia


bwwa.coords <- as.data.frame(read_sf("Data/wetransfer_acfl_coords-cpg_2023-10-26_1934/bwwa_coords.shp"))
bwwa_xy <- bwwa.coords %>% select(Lat, Lon) %>% mutate(x = Lon, y = Lat) %>% select(x,y) %>% mutate(sp = "bwwa")

bkwa.coords <- as.data.frame(read_sf("Data/wetransfer_acfl_coords-cpg_2023-10-26_1934/bkwa_coords.shp"))
bkwa_xy <- bkwa.coords %>% select(Lat, Lon) %>% mutate(x = Lon, y = Lat) %>% select(x,y) %>% mutate(sp = "bkwa")

yewa.coords <- as.data.frame(read_sf("Data/wetransfer_acfl_coords-cpg_2023-10-26_1934/yewa_coords.shp"))
yewa_xy <- yewa.coords %>% select(Lat, Lon) %>% mutate(x = Lon, y = Lat) %>% select(x,y) %>% mutate(sp = "yewa")

amre.coords <- as.data.frame(read_sf("Data/wetransfer_acfl_coords-cpg_2023-10-26_1934/amre_coords.shp"))
amre_xy <- amre.coords %>% select(Lat, Lon) %>% mutate(x = Lon, y = Lat) %>% select(x,y) %>% mutate(sp = "amre")

acfl.coords <- as.data.frame(read_sf("Data/wetransfer_acfl_coords-cpg_2023-10-26_1934/acfl_coords.shp"))
acfl_xy <- acfl.coords %>% select(Lat, Lon) %>% mutate(x = Lon, y = Lat) %>% select(x,y) %>% mutate(sp = "acfl")

nowa.coords <- as.data.frame(read_sf("Data/wetransfer_acfl_coords-cpg_2023-10-26_1934/nowa_coords.shp"))
nowa_xy <- nowa.coords %>% select(Lat, Lon) %>% mutate(x = Lon, y = Lat) %>% select(x,y) %>% mutate(sp = "nowa")

gcfl.coords <- as.data.frame(read_sf("Data/wetransfer_acfl_coords-cpg_2023-10-26_1934/gcfl_coords.shp"))
gcfl_xy <- gcfl.coords %>% select(Lat, Lon) %>% mutate(x = Lon, y = Lat) %>% select(x,y) %>% mutate(sp = "gcfl")

mowa.coords <- as.data.frame(read_sf("Data/wetransfer_acfl_coords-cpg_2023-10-26_1934/mowa_coords.shp"))
mowa_xy <- mowa.coords %>% select(Lat, Lon) %>% mutate(x = Lon, y = Lat) %>% select(x,y) %>% mutate(sp = "mowa")

swth.coords <- as.data.frame(read_sf("Data/wetransfer_acfl_coords-cpg_2023-10-26_1934/swth_coords.shp"))
swth_xy <- swth.coords %>% select(Lat, Lon) %>% mutate(x = Lon, y = Lat) %>% select(x,y) %>% mutate(sp = "swth")

tewa.coords <- as.data.frame(read_sf("Data/wetransfer_acfl_coords-cpg_2023-10-26_1934/tewa_coords.shp"))
tewa_xy <- tewa.coords %>% select(Lat, Lon) %>% mutate(x = Lon, y = Lat) %>% select(x,y) %>% mutate(sp = "tewa")

smtn.coords <- as.data.frame(read_sf("Data/wetransfer_acfl_coords-cpg_2023-10-26_1934/smtn_coords.shp"), na.rm = TRUE)
smtn_xy <- smtn.coords %>% select(Lat, Lon) %>% mutate(x = Lon, y = Lat) %>% select(x,y) %>% mutate(sp = "smtn")

bbwa.coords <- as.data.frame(read_sf("Data/wetransfer_acfl_coords-cpg_2023-10-26_1934/bbwa_coords.shp"), na.rm = TRUE)
bbwa_xy <- bbwa.coords %>% select(Lat, Lon) %>% mutate(x = Lon, y = Lat) %>% select(x,y) %>% mutate(sp = "bbwa")


## Just Run this one time at the beggining
clim.bkg<-na.exclude(data.frame(extract(clim.bkg.nam,bkgr_xy[,1:2])))



#### Pair between species ####

## Change the pair of species each time to run the analyses

#pair_xy <- na.exclude(rbind(#species 1_xy, #species 2_xy))


# extract climate data from the rasters

clim.occ<-na.exclude(data.frame(extract(clim.bkg.nam,pair_xy[,1:2])))

# calibration of PCA-env 
pca.env <-dudi.pca(clim.bkg, center = T, scale = T, scannf = F, nf = 2)

# selection of species to analyze
sp.choice<- c("SPECIES 1","SPECIES 2") #CHOOSE THE SET OF SPECIES FOR PAIRWISE ANALYSES
sp.combn<-combn(sp.choice,2) 
nsp<-ncol(sp.combn)

# niche quantification for each combination of species
i=1
spa<-sp.combn[1,i] #name of species a
spb<-sp.combn[2,i] #name of species b
clim.spa<-clim.occ[pair_xy$sp==spa,] #env data for species a
clim.spb<-clim.occ[pair_xy$sp==spb,] #env data for species b

# PCA scores
scores.bkg<- pca.env$li	#scores for global climate
scores.spa<- na.omit(suprow(pca.env,clim.spa)$lisup)				#scores for spa
scores.spb<- na.omit(suprow(pca.env,clim.spb)$lisup)				#scores for spb

## Summary of the scores

summary(scores.bkg)
summary(scores.spa)
summary(scores.spb)

### Have to use this. Use it only for the species marked, otherwise it would generate an error
## GCFL & SMTN
scores.spb <- scores.spb %>% 
  filter(Axis1<=3.80714)

scores.spa <- scores.spa %>% 
  filter(Axis1<=3.80714)
####
####
## SWTH
scores.spb <- scores.spb %>% 
  filter(Axis2>=-8.2978)

scores.spa <- scores.spa %>% 
  filter(Axis2>=-8.2978)
####



# calculation of occurence density
za<- ecospat.grid.clim.dyn(scores.bkg,scores.bkg,scores.spa,100)
zb<- ecospat.grid.clim.dyn(scores.bkg,scores.bkg,scores.spb,100)

# overlap corrected by availabilty of background conditions ESTE ES EL VALOR QUE NOS INTERESA
ecospat.niche.overlap(za,zb,cor=F)


# both za and zb are randomly shifted in the background (previous versions of ecospat implemented rand.type =2)

ecospat.plot.niche.dyn(za, zb, title = "SPECIES 1-SPECIES 2", name.axis1 = "PC1",name.axis2 = "PC2", interest = 1, colZ1 =
                         "green3", colZ2 = "red3", xlim = c(-5, 5), ylim = c(-8, 10), transparency = 50)


