library(tidyverse)
library(here)
library(dplyr)
library(sf)
library(raster)
library(terra)
library(rgdal)

#KgC
"calc_kgc" = function(df)
{
  # 1/06 kmk
  # revised 5/07
  # a subroutine for calculating kgC of individual trees
  # using their dbhs & spp specific allometries
  # x.name is the name of the object the fnctn is being applied to in ""
  # spp is the column number that contains the spp names
  # dbh is the column number(s) that contain the dbhs in cm
  # allometries calculate biomass (kg) from diameters (cm)
  # many of the equations deal with units other than kg & cm, so conversions are done within the equations
  # "if" statements allow there to be no members of a given spp
  kgc <- df$dbh
  
  #Fraxinus americana (White ash) 	
  ash <- which(df$spp == "ash")
  if(sum(ash) > 0)
    kgc[ash] <- 3.2031 - 0.2337 * (df$dbh[ash]*10) + 0.006061 * (df$dbh[ash]*10)^2
  
  #Betula lenta (Black birch) 
  bb <- which(df$spp == "bb")
  if(sum(bb) > 0)
    kgc[bb] <- (1.6542 * (df$dbh[bb]/2.54)^2.6606)/2.2	
  
  #Fagus grandifolia (American beech)     
  beech <- which(df$spp == "beech")
  if(sum(beech) > 0)
    kgc[beech] <- (10^(2.028 + 2.3981 * log10(df$dbh[beech])) + 10^(1.4182 + 2.5509 * log10(df$dbh[beech])))/1000
  
  #Quercus velutina (Black oak) 
  bo <- which(df$spp == "bo")
  if(sum(bo) > 0)
    kgc[bo] <- (2.14567 * (df$dbh[bo]/2.54)^2.50304)/2.2 
  
  #Prunus serotina  (Black cherry)                
  cherry <- which(df$spp == "cherry")
  if(sum(cherry) > 0)
    kgc[cherry] <- (1.8082 * (df$dbh[cherry]/2.54)^2.6174)/2.2 
  
  #Betula populifolia (Gray birch)                   
  gb <- which(df$spp == "gb")
  if(sum(gb) > 0)
    kgc[gb] <- (exp(0.6541776 + 2.529903 * log(df$dbh[gb]/2.54)) + exp(-0.3969096 + 1.628732 * log(df$dbh[gb]/2.54)))/2.2
  
  #Tsuga canadensis (Eastern hemlock) 
  hem <- which(df$spp == "hem")
  if(sum(hem) > 0)
    kgc[hem] <- (exp(0.2607714 + 2.366161 * log(df$dbh[hem]/2.54)) + exp(-1.775095 + 2.703338 * log(df$dbh[hem]/2.54)))/2.2
  
  #Acer rubrum (Red maple) 
  rm <- which(df$spp == "rm")
  if(sum(rm) > 0)
    kgc[rm] <- (exp(5.02 + 2.33 * log(df$dbh[rm])))/1000
  
  #Quercus rubra (Northern red oak)  & Castenea dentate (American chestnut)
  
  ro <- which(df$spp == "ro" | df$spp == "chestnut")
  if(sum(ro) > 0)
    kgc[ro] <- (exp(5.03 + 2.39 * log(df$dbh[ro])))/1000
  
  #Pinus resinosa (Red pine) 
  rp <- which(df$spp == "rp")
  if(sum(rp) > 0)
    kgc[rp] <- (exp(0.3078881 + 2.448125 * log(df$dbh[rp]/2.54)) + exp(-1.508616 + 2.501085 * log(df$dbh[rp]/2.54)))/2.2
  
  #Acer pennsylvanicum (Striped maple) 
  sm <- which(df$spp == "sm")
  if(sum(sm) > 0)
    kgc[sm] <- (exp(4.68 + 2.38 * log(df$dbh[sm])))/1000
  
  #Betula papyrifera (Paper birch) 
  wb <- which(df$spp == "wb")
  if(sum(wb) > 0)
    kgc[wb] <- (exp(-0.1463825 + 2.896804 * log(df$dbh[wb]/2.54)) + exp(-1.053626 + 2.249402 * log(df$dbh[wb]/2.54)))/2.2
  
  #Hamamelis virginiania (witch hazel)
  wh <- which(df$spp == "wh")
  if(sum(wh) > 0)
    kgc[wh] <- (38.111 * (df$dbh[wh]^2.9))/1000
  
  #Quercus alba (White oak)
  wo <- which(df$spp == "wo")
  if(sum(wo) > 0)
    kgc[wo] <- exp(-0.82061 + 2.84694 * log(df$dbh[wo]/2.54))
  
  #Pinus strobus (white pine) 
  wp <- which(df$spp == "wp")
  if(sum(wp) > 0)
    kgc[wp] <- (exp(-0.04659059 + 2.545864 * log(df$dbh[wp]/2.54)) + exp(-1.701115 + 2.485762 * log(df$dbh[wp]/2.54)))/2.2
  
  #Picea glauca (white spruce) 
  #general spruce equation for all df$spp.
  ws <- which(df$spp == "ws")
  if(sum(ws) > 0)
    kgc[ws] <- (exp(0.3960433 + 2.374645 * log(df$dbh[ws]/2.54)) + exp(-1.351318 + 2.338385 * log(df$dbh[ws]/2.54)))/2.2
  
  #Betula alleghaniensis (Yellow birch) 
  yb <- which(df$spp == "yb")
  if(sum(yb) > 0)
    kgc[yb] <- (10^(2.1413 + 2.2683 * log10(df$dbh[yb])) + 10^(1.0535 + 2.7995 * log10(df$dbh[yb])))/1000
  #----------------------------------------------
  # these next equations are for the sapling data
  # they have units cm & g, so we do the conversion to in and lbs 
  # within the equations to be consistent will all the others
  
  #Vaccinium df$spp. (high bush blueberry) & Rhus typhina (Staghorn sumac)
  hbb <- which(df$spp == "hbb" | df$spp == "SS")
  if (sum(hbb) > 0)
    kgc[hbb] <- (95.143 * (df$dbh[hbb])^3.706)/1000
  
  #Viburnum df$spp. (Northern wild raisin)
  nwr <- which(df$spp == "nwr")
  if (sum(nwr) > 0)
    kgc[nwr] <- (29.615 * (df$dbh[nwr])^3.243)/1000
  
  #Spiraea df$spp.
  SPI <- which(df$spp == "SPI")
  if (sum(SPI) > 0)
    kgc[SPI] <- (36.648 * (df$dbh[SPI])^2.579)/1000
  
  #Prunus pennsylvanica (pin cherry)
  FPC <- which(df$spp == "FPC")
  if (sum(FPC) > 0)
    kgc[FPC] <- (49.916 * (df$dbh[FPC])^2.547)/1000
  
  #Crataegus df$spp. (Hawthorn)
  # for hawthorn df$spp use the general equation for mixed hardwood from Jenkins et al.
  # biomass is converted from kg to lbs in the equation
  haw <- which(df$spp == "haw")
  if (sum(haw) > 0)
    kgc[haw] <- exp(-2.48 + 2.4835*logb(df$dbh[haw]))
  #------------------------------------------------
  # convert from biomass to C
  kgc <- kgc/2
  return(kgc)
}


# make forestgeo environmental dataset
soils <- readOGR('GitHub/Dendrometer/enviro_data', layer = 'soils')
elev <- raster('GitHub/Dendrometer/enviro_data/dblbnd.adf')

# calculate slope and aspect from elevation data
slopeaspect <- terrain(elev, opt = c('slope', 'aspect'), unit='degrees')

# load in forestgeo data 
tree14 <- read_csv("GitHub/Dendrometer/forestgeo_data/hf253-04-stems-2014.csv")
tree19 <- read_csv("GitHub/Dendrometer/forestgeo_data/hf253-05-stems-2019.csv")

# make year col 
tree14 <- tree14 %>%
  mutate(year = '2014')

tree19 <- tree19 %>%
  mutate(year = '2019')

# combine
forestgeo <- rbind(tree19, tree14)


# add coordinates
# Coordinates of the corner of the Megaplot for mapping
mgeast = 731592.9 # Coordinate easting
mgnorth = 4713221.9  # Coordinate northing


# Create quadrat and coordinate columns
# Calculate basal area and kgc
forestgeo <- forestgeo %>%
  mutate(quadrat = stringr::str_pad(quadrat, width=4, pad="0", side="left"),
         coords = paste(gx,gy),
         gx_utm = mgeast+gx,
         gy_utm = mgnorth+gy,
         basal_area = dbh^2 * 0.005454) %>%
  filter(df.status == 'alive', sp != "deadhw", sp != "deadsw", sp != "betuxx", sp != "uniden") 

forestgeo <- forestgeo %>%
  rename('spp' = 'sp')

forestgeo$kgc <- calc_kgc(forestgeo)
forestgeo <- forestgeo %>%
  mutate(basal_area =  pi*(dbh/2)^2)

# remove unnecessary columns
forestgeo <- forestgeo %>%
  subset(select = -c(dbh.id, census.id,codes,
                     count.pom, jd))

# make a shape file from coords
coord <- forestgeo[,c('gx_utm',"gy_utm")]
crs1 <- CRS("+proj=utm +zone=18 +datum=WGS84")
dat <- SpatialPointsDataFrame(coords = coord,
                              data = forestgeo,
                              proj4string = crs1)
plot(dat)

# reproject to match elevation CRS
newcrs <- crs(elev)
dat2 <- spTransform(dat,newcrs)

# Plot to check overlap
plot(elev)
plot(dat2, add = TRUE)
plot(soils, add = TRUE)

# Extract topographic data for every EMS plot
forestgeo <- forestgeo %>% mutate(elevation = raster::extract(elev, dat2),
                              slope = raster::extract(slopeaspect$slope, dat2),
                              aspect = raster::extract(slopeaspect$aspect, dat2))

# calculate northness
forestgeo <- forestgeo %>%
  mutate(northness = cos(aspect))

#----------------------------------------#
#----------------------------------------#
#---------------calculate NCI-------------
#----------------------------------------#
#----------------------------------------#
# create a dataset for each year
forest19 <- filter(forestgeo, year == '2019')
forest14 <- filter(forestgeo, year == '2014')

# do 2019 first
# create a column for NCI
forest19$NCI<-rep(NA, nrow(forest19))
forest19$n_trees<-rep(NA, nrow(forest19))
# Run function to grab information for every tree within 5 meters of an emergent
# Create empty dataframe
surrounding19 <- data.frame()
# Define a radius to grab information from
radius = 5
# Loop through every emergent tree
for (i in 1:nrow(forest19)){
  # define coordinates for tree of interest
  midx <- forest19$gx_utm[i]
  midy <- forest19$gy_utm[i]
  # crop dat1a to contain only points within the radius of the tree of interest
  temp<-forest19[forest19$gx_utm>=(midx-radius) & forest19$gx_utm<=(midx+radius) & forest19$gy_utm>=(midy-radius) & forest19$gy_utm<=(midy+radius), ]
  # remove tree of interest
  temp <- subset(temp, tree.id != forest19$tree.id[i] & stem.id != forest19$stem.id[i])
  # calculate NCI and distance from tree of interest
  temp$dis<-rep(NA, nrow(temp))
  NCI = 0
  for(j in 1:nrow(temp))
  {
    temp$dis[j]<-sqrt((forest19$gx_utm[i]-temp$gx_utm[j])^2+(forest19$gy_utm[i]-temp$gy_utm[j])^2)
    NCI<-NCI+sum((temp$dbh[j]^2/temp$dis[j]^2)[temp$dis[j]<=radius & temp$dis[j]>0],na.rm=T)
  }
  forest19$NCI[i] <- NCI
  # remove trees outside of buffer
  temp <- subset(temp, dis <= 5)
  
  forest19$n_trees[i] <- nrow(temp)
}


# do 2014
# create a column for NCI
forest14$NCI<-rep(NA, nrow(forest14))
forest14$n_trees<-rep(NA, nrow(forest14))
# Run function to grab information for every tree within 5 meters of an emergent
# Create empty dataframe
surrounding14 <- data.frame()
# Define a radius to grab information from
radius = 5
# Loop through every emergent tree
for (i in 1:nrow(forest14)){
  # define coordinates for tree of interest
  midx <- forest14$gx_utm[i]
  midy <- forest14$gy_utm[i]
  # crop dat1a to contain only points within the radius of the tree of interest
  temp<-forest14[forest14$gx_utm>=(midx-radius) & forest14$gx_utm<=(midx+radius) & forest14$gy_utm>=(midy-radius) & forest14$gy_utm<=(midy+radius), ]
  # remove tree of interest
  temp <- subset(temp, tree.id != forest14$tree.id[i] & stem.id != forest14$stem.id[i])
  # calculate NCI and distance from tree of interest
  temp$dis<-rep(NA, nrow(temp))
  NCI = 0
  for(j in 1:nrow(temp))
  {
    temp$dis[j]<-sqrt((forest14$gx_utm[i]-temp$gx_utm[j])^2+(forest14$gy_utm[i]-temp$gy_utm[j])^2)
    NCI<-NCI+sum((temp$dbh[j]^2/temp$dis[j]^2)[temp$dis[j]<=radius & temp$dis[j]>0],na.rm=T)
  }
  forest14$NCI[i] <- NCI
  # remove trees outside of buffer
  temp <- subset(temp, dis <= 5)
  
  forest14$n_trees[i] <- nrow(temp)
}

# combine
forestall <- rbind(forest19, forest14)

# export
write_csv(forestall, 'GitHub/Dendrometer/forestgeo_nci.csv')

#----------------------------------------#
#----------------------------------------#
#-----------------plots-------------------
#----------------------------------------#
#----------------------------------------#
nci <- read_csv('GitHub/Dendrometer/forestgeo_nci.csv')
nci <- nci %>%
  mutate(basal_area = pi*(dbh/2)^2)

# split dataframes by year and then recombine to add growth rates
nci19 <- filter(nci, year == '2019')
nci14 <- filter(nci, year == '2014')

# merge and calculate growth
nci_full <- inner_join(nci14, nci19, by = c('tree.id', 'stem.id'))

nci_full <- nci_full %>%
  mutate(basal_growth = (basal_area.y-basal_area.x)/5,
         kgc_growth = (kgc.y-kgc.x)/5)

# remove growth outliers
nci_full <- filter(nci_full, basal_growth < 10000)

plot(nci_full$NCI.y, nci_full$basal_growth)

hist(nci_full$basal_growth)

hist(nci_full$NCI.y)

summary(lm(basal_growth~NCI.y, data = nci_full))

ggplot()+
  geom_boxplot(nci_full, mapping = aes(y = log(NCI.y), x = spp.x, color = spp.x))+
  theme_minimal()


# subset to only red oaks
redoak <- filter(nci_full, spp.x == 'querru')

plot(redoak$NCI.y, redoak$basal_growth)

ggplot()+
  geom_point(redoak, mapping = aes(x = NCI.y, y = basal_growth, color = dbh.x))+
  theme_minimal()+
  xlim(0, 20000)


# create size and growth class bins
# then make hist to show differences in classes
redoakbinned <- redoak %>%
  mutate(grow_bin = cut(basal_growth, breaks=c(-100, 0,10,20,40,400)))

ggplot()+
  geom_point(redoak, mapping = aes(x = NCI.y, y = basal_growth, color = dbh.x))+
  theme_minimal()+
  xlim(0, 20000)

ggplot()+
  geom_boxplot(redoakbinned, mapping = aes(x = grow_bin, y=log(NCI.y), fill = grow_bin))+
  theme_minimal()

# try binnng size classes
redoakbinned <- redoak %>%
  mutate(size_bin = cut(dbh.y, breaks=c(0,25,33,40,82)))

# plot
ggplot()+
  geom_boxplot(redoakbinned, mapping = aes(x = size_bin, y=log(NCI.y), fill = size_bin))+
  theme_minimal()

#----------------------------------------#
#----------------hemlocks-----------------
#----------------------------------------#
hemlock <- filter(nci_full, spp.x == 'tsugca')

summary(hemlock$dbh.y)

hemlock <- hemlock %>%
  mutate(grow_bin = cut(basal_growth, breaks=c(-205, 0,1,6,400)),
         size_bin = cut(dbh.y, breaks=c(0,3,7,100)))

ggplot()+
  geom_boxplot(hemlock, mapping = aes(x = size_bin, y=log(NCI.y), fill = size_bin))+
  theme_minimal()
ggplot()+
  geom_boxplot(hemlock, mapping = aes(x = grow_bin, y=log(NCI.y), fill = grow_bin))+
  theme_minimal()
#----------------------------------------#
#----------------------------------------#
#-------------extract soils---------------
#----------------------------------------#
#----------------------------------------#

# extract soils data 
dat3 = st_as_sf(dat2)
soils2 = st_as_sf(soils)

## intersect polygons with points, keeping the information from both
soil_df = st_intersection(dat3, soils2)

# remove unnecessary cols
soil_df <- subset(soil_df, select = c('tree.id','stem.id','SIMMONS_SO','MESIC_SOIL','DRAINAGE_C',
                                      'TYPE_'))
# merge with full dataset
fulldat <- inner_join(forestgeo,soil_df,by = c('tree.id','stem.id'), relationship = 'many-to-many')


