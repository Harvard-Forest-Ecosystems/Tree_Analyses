library(tidyverse)
library(here)
library(dplyr)

# required functions:
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

# ForestGeo data
# read in 2014 and 2019 censuses
dat_14 <- read_csv("GitHub/Dendrometer/data/forestgeo_data/hf253-04-stems-2014.csv")

dat_19 <- read_csv("GitHub/Dendrometer/Data/forestgeo_data/hf253-05-stems-2019.csv")


# Coordinates of the corner of the Megaplot for mapping
mgeast = 731592.9 # Coordinate easting
mgnorth = 4713221.9  # Coordinate northing

# Create quadrat and coordinate columns
dat_14 <- dat_14 %>%
  mutate(quadrat = stringr::str_pad(quadrat, width=4, pad="0", side="left"),
         coords = paste(gx,gy),
         gx_utm = mgeast+gx,
         gy_utm = mgnorth+gy) %>%
  filter(df.status == 'alive', sp != "deadhw", sp != "deadsw", sp != "betuxx", sp != "uniden") 

dat_19 <- dat_19 %>%
  mutate(quadrat = stringr::str_pad(quadrat, width=4, pad="0", side="left"),
         coords = paste(gx,gy),
         gx_utm = mgeast+gx,
         gy_utm = mgnorth+gy) %>%
  filter(df.status == 'alive', sp != "deadhw", sp != "deadsw", sp != "betuxx", sp != "uniden") 

# rename species column to spp
dat_14 <- dat_14 %>%
  rename('spp'='sp')
dat_19 <- dat_19 %>%
  rename('spp'='sp')

# calculate kgc
dat_14$kgc <- calc_kgc(dat_14)

dat_19$kgc <- calc_kgc(dat_19)

# calculate basal area
dat_14 <- dat_14 %>% 
  mutate(basal_area = signif(pi*(dbh/2)^2, 4))

dat_19 <- dat_19 %>% 
  mutate(basal_area = signif(pi*(dbh/2)^2, 4))

# merge datasets based on tree and stem id
dat <- inner_join(dat_14, dat_19, by = c('tree.id','stem.id'))

# calculate time between both censuses
dat$diff_in_days<- difftime(dat$exact.date.y ,dat$exact.date.x , units = c("days"))

dat <- dat%>%
  mutate(diff_in_days = as.numeric(substr(diff_in_days,1,4)),
         diff_in_years = diff_in_days/365)


# calculate growth with accurate time span
geo_grow <- dat %>%
  mutate(dbh_grow = (dbh.y-dbh.x)/diff_in_years,
         kgc_grow = (kgc.y-kgc.x)/diff_in_years,
         ba_grow= (basal_area.y-basal_area.x)/diff_in_years)

# export
write_csv(geo_grow, 'GitHub/Dendrometer/data/forestgeo_data/forestgeo_growth_all.csv')




# NOTE: need to calculate growth using specific dates, not just assuming 5 years
# red oaks
grow_ro <- geo_grow %>% 
  filter(spp.x == 'querru' & kgc_grow < 15)

grow_ro <- grow_ro %>%
  mutate(size_bin = cut(dbh.x, breaks=c(0,23.20,30.50,30.86,79.00)))

ggplot()+
  geom_boxplot(grow_ro, mapping = aes(y = kgc_grow, x = size_bin))+
  theme_minimal()+
  ggtitle('Red Oaks')

# hemlocks
grow_hem <- geo_grow %>% 
  filter(spp.x == 'tsugca' & kgc_grow < 15)

grow_hem <- grow_hem %>%
  mutate(size_bin = cut(dbh.x, breaks=c(0,2.5,6.2,11.46,100)))

ggplot()+
  geom_boxplot(grow_hem, mapping = aes(y = kgc_grow, x = size_bin))+
  theme_minimal()+
  ggtitle('Hemlocks')

# Red Maples
grow_rm <- geo_grow %>% 
  filter(spp.x == 'acerru' & kgc_grow < 100)

grow_rm <- grow_rm %>%
  mutate(size_bin = cut(dbh.x, breaks=c(0,8.7,14,20.2,80)))

ggplot()+
  geom_boxplot(grow_rm, mapping = aes(y = kgc_grow, x = size_bin))+
  theme_minimal()+
  ggtitle('Red Maples')
