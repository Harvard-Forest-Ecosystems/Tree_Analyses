library(tidyverse)
library(here)
library(dplyr)
library(sf)
library(raster)
library(terra)
library(rgdal)

# load in environmental data-------------
soils <- readOGR('GitHub/Dendrometer/enviro_data', layer = 'soils')
elev <- raster('GitHub/Dendrometer/enviro_data/dblbnd.adf')

# functions----------
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

#---------------------------------------#
#---------------------------------------#
#-------------Make a dataframe-----------
#---------------------------------------#
#---------------------------------------#

# load in environmental data
soils <- readOGR('~/Documents/GitHub/Dendrometer/enviro_data', layer = 'soils')
plot(soils)
elev <- raster('~/Documents/GitHub/Dendrometer/enviro_data/dblbnd.adf')

# calculate slope and aspect from elevation data
slopeaspect <- terrain(elev, opt = c('slope', 'aspect'), unit='degrees')

# load in forestgeo data 
tree14 <- read_csv("GitHub/Dendrometer/forestgeo_data/hf253-04-stems-2014.csv")
tree19 <- read_csv("GitHub/Dendrometer/forestgeo_data/hf253-05-stems-2019.csv")


# add coordinates
# Coordinates of the corner of the Megaplot for mapping
mgeast = 731592.9 # Coordinate easting
mgnorth = 4713221.9  # Coordinate northing


# Create quadrat and coordinate columns
# Calculate basal area and kgc
dat_14 <- tree14 %>%
  mutate(quadrat = stringr::str_pad(quadrat, width=4, pad="0", side="left"),
         coords = paste(gx,gy),
         gx_utm = mgeast+gx,
         gy_utm = mgnorth+gy,
         basal_area = dbh^2 * 0.005454) %>%
  filter(sp != "deadhw", sp != "deadsw", sp != "betuxx", sp != "uniden") 

dat_14 <- dat_14 %>%
  rename('spp' = 'sp')

dat_14$kgc <- calc_kgc(dat_14)

dat_19 <- tree19 %>%
  mutate(quadrat = stringr::str_pad(quadrat, width=4, pad="0", side="left"),
         coords = paste(gx,gy),
         gx_utm = mgeast+gx,
         gy_utm = mgnorth+gy,
         basal_area = dbh^2 * 0.005454) %>%
  filter( sp != "deadhw", sp != "deadsw", sp != "betuxx", sp != "uniden") 

dat_19 <- dat_19 %>%
  rename('spp' = 'sp')

dat_19$kgc <- calc_kgc(dat_19)

# remove unnecessary columns
dat_14 <- dat_14 %>%
  subset(select = -c(dbh.id, census.id,codes,
                     count.pom, jd,  gx_utm, gy_utm))

dat_19 <- dat_19 %>% 
  subset(select = -c(spp,stem.tag,quadrat,gx,gy,dbh.id, census.id,codes,
                     count.pom, jd, coords,
                     tag))


# merge datasets based on tree and stem id
treedat <- full_join(dat_14, dat_19, by = c('tree.id','stem.id'))

treedat <- treedat %>% drop_na(gx_utm)

# make a shape file from coords
coord <- treedat[,c('gx_utm',"gy_utm")]
crs1 <- CRS("+proj=utm +zone=18 +datum=WGS84")
dat <- SpatialPointsDataFrame(coords = coord,
                              data = treedat,
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
treedat <- treedat %>% mutate(elevation = raster::extract(elev, dat2),
                              slope = raster::extract(slopeaspect$slope, dat2),
                              aspect = raster::extract(slopeaspect$aspect, dat2))

# calculate northness
treedat <- treedat %>%
  mutate(northness = cos(aspect))

# extract soils data 
dat3 = st_as_sf(dat2)
soils2 = st_as_sf(soils)

## intersect polygons with points, keeping the information from both
soil_df = st_intersection(dat3, soils2)

# remove unnecessary cols
soil_df <- subset(soil_df, select = c('tree.id','stem.id','SIMMONS_SO','MESIC_SOIL','DRAINAGE_C',
                                      'TYPE_'))
# merge with full dataset
fulldat <- inner_join(treedat,soil_df,by = c('tree.id','stem.id'))

# export
write_csv(fulldat, 'GitHub/Dendrometer/forestgeo_w_enviro.csv')

#---------------------------------------#
#-------------Make Soil Map-------------
#---------------------------------------#
ggplot()+
  geom_point(fulldat, mapping = aes(x=gx, y=gy, col = TYPE_))
ggplot()+
  geom_point(fulldat, mapping = aes(x=gx, y=gy, col = DRAINAGE_C))
ggplot()+
  geom_point(fulldat, mapping = aes(x=gx, y=gy, col = SIMMONS_SO))





#---------------------------------------#
#--------Calculate Growth Rates----------
#---------------------------------------#

fulldat <- fulldat %>%
  mutate(dbh_gr = (dbh.y-dbh.x)/5,
         basal_gr = (basal_area.y-basal_area.x)/5,
         kgc_gr = (kgc.y-kgc.x)/5)


#---------------------------------------#
#---------------------------------------#
#------------Red Oak Analyses------------
#---------------------------------------#
#---------------------------------------#
# make dataset
grow_ro <- filter(fulldat, spp == 'tsugca')
# remove weirdo trees
grow_ro <- filter(grow_ro, dbh_gr < 10)

# Plot general growth rate trends
ggplot()+
  geom_point(data = grow_ro, aes(x=gx, y = gy, col = basal_gr), alpha = .5, size = 1)

# jk group by quadrat  lol
grow_ro_quad <- grow_ro %>%
  group_by(quadrat) %>%
  summarize(gxx = mean(gx, na.rm = TRUE),
            gyy = mean(gy, na.rm = TRUE),
            basal = mean(basal_gr, na.rm = TRUE),
            kgc = mean(kgc_gr, na.rm = TRUE),
            num = n())

# plot red oak growth on top of soil class map
ggplot()+
  geom_point(fulldat, mapping = aes(x=gx, y=gy, col = SIMMONS_SO))+
  geom_point(data = grow_ro_quad, aes(x=gxx, y = gyy, size = kgc), alpha = .5)

ggplot()+
  geom_point(data = grow_ro_quad, aes(x=gxx, y = gyy, size = basal, col = basal))

# plot relationship between number of trees in a quadrat and mean growth rate
ggplot()+
  geom_point(data = grow_ro_quad, aes(x=num, y = basal))

# plot relationship between soils and growth rates
ggplot()+
  geom_boxplot(data = grow_ro, aes(x= TYPE_, y = log(basal_gr), fill = TYPE_))

ggplot()+
  geom_boxplot(data = grow_ro, aes(x= TYPE_, y = log(kgc_gr), fill = TYPE_))

# plot distribution/map of red oak dbhs
hist(grow_ro$dbh.x)
hist(grow_ro$dbh.y)
ggplot()+
  geom_point(data = grow_ro, aes(x=gx, y = gy, size = dbh.y, col = dbh.y), alpha = 0.5)

#---------------------------------------#
#---------------------------------------#
#------------Hemlock Analyses------------
#---------------------------------------#
#---------------------------------------#
# make dataset
grow_hem <- filter(fulldat, spp == 'querru')

# remove weirdo trees
grow_hem <- filter(grow_hem, dbh_gr < 10)
# Plot general growth rate trends
ggplot()+
  geom_point(data = grow_hem, aes(x=gx, y = gy, col = basal_gr), alpha = .5, size = 2)
ggplot()+
  geom_point(data = grow_hem, aes(x=gx, y = gy, col = log(basal_gr)), alpha = .7, size = 1.5)

# jk group by quadrat  lol
grow_hem_quad <- grow_hem %>%
  group_by(quadrat) %>%
  summarize(gxx = mean(gx, na.rm = TRUE),
            gyy = mean(gy, na.rm = TRUE),
            basal = mean(basal_gr, na.rm = TRUE),
            kgc = mean(kgc_gr, na.rm = TRUE),
            num = n())

# plot red oak growth on top of soil class map
ggplot()+
  geom_point(fulldat, mapping = aes(x=gx, y=gy, col = SIMMONS_SO))+
  geom_point(data = grow_hem_quad, aes(x=gxx, y = gyy, size = kgc), alpha = .5)


ggplot()+
  geom_point(data = grow_hem_quad, aes(x=gxx, y = gyy, size = basal, col = basal))

# plot relationship between number of trees in a quadrat and mean growth rate
ggplot()+
  geom_point(data = grow_hem_quad, aes(x=num, y = basal))

# plot relationship between soils and growth rates
ggplot()+
  geom_boxplot(data = grow_hem, aes(x= TYPE_, y = log(basal_gr), fill = TYPE_))

ggplot()+
  geom_boxplot(data = grow_hem, aes(x= TYPE_, y = log(kgc_gr), fill = TYPE_))

# plot distribution/map of hem dbhs
hist(grow_hem$dbh.x)
hist(grow_hem$dbh.y)
ggplot()+
  geom_point(data = grow_hem, aes(x=gx, y = gy, size = dbh.y, col = dbh.y), alpha = 0.5)


# to dos:
# calculate mortality rates and make similar plots
# create maps of trees that died to see if there is a spatial correlation

dat_19 <- tree19 %>%
  mutate(quadrat = stringr::str_pad(quadrat, width=4, pad="0", side="left"),
         coords = paste(gx,gy),
         gx_utm = mgeast+gx,
         gy_utm = mgnorth+gy,
         basal_area = dbh^2 * 0.005454) %>%
  filter(sp != "deadhw", sp != "deadsw", sp != "betuxx", sp != "uniden") 

hem <- filter(dat_19, sp == 'querru')

# make map colored by dead and alive trees
ggplot()+
  geom_point(data = hem, aes(x = gx, y=gy, col = df.status))

# look at mortality rates by quadrat (wont be perf cause i need to look at 2014 data to see which ones were alive then and dead now)
hem_quad <- hem %>% 
  group_by(quadrat) %>%
  summarize(tot_dead = ,
            tot = n())
