library(tidyverse)
library(here)
library(dplyr)

# Required Functions--------------

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
#----------------------------------
#---------------------------------#
#------------dead trees ----------#
#---------------------------------#
#---------------------------------#
treedat <- read_csv('GitHub/Dendrometer/ems-tree-summ-w-2023.csv')
treedat <- treedat %>%
  rename('dbh' = 'dbh_cm',
         'basal_area' = 'basal_area_cm2',
         'kgc' = 'biomass_kgc')

treedead <- filter(treedat, tree_type == 'dead')
treedead <- treedead[!duplicated(treedead['plottag']),]

treedead <- filter(treedead, year != '1998')

tree_dead <- treedead %>% 
  group_by(year, species) %>%
  summarise(kgc = sum(kgc, na.rm = TRUE),
            basal = sum(basal_area, na.rm = TRUE),
            n_trees = n())

ggplot()+
  geom_point(tree_dead, mapping = aes(y = n_trees, x = year, col = species))+
  geom_line(tree_dead, mapping = aes(y = n_trees, x = year, col = species))+
  geom_hline(yintercept = 0)+
  theme_minimal()+
  ylab('Number of Trees')+
  xlab('Year')+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+ 
  labs(col='Species') 


ggplot()+
  geom_point(tree_dead, mapping = aes(y = kgc, x = year, col = species))+
  geom_line(tree_dead, mapping = aes(y = kgc, x = year, col = species))+
  geom_hline(yintercept = 0)+
  theme_minimal()+
  ylab('KgC')+
  xlab('Year')+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+ 
  labs(col='Species') 

#----------------------------------
#---------------------------------#
#-----Total Biomass by species-----#
#---------------------------------#
#---------------------------------#
# load in archive data
treedat <- read_csv('GitHub/Dendrometer/ems-tree-summ-w-2023.csv')
treedat <- filter(treedat, tree_type != 'dead')
#"GitHub/Dendrometer/2022_archive/ems-tree-summ.csv")

# Clean data

# manually change first census of 2023 w/ E3-769 to 
# equal last census of 2022 instead
# bc new band dbh measurement messes up the growth rate
# (big jump between calc dbhs and the actual measured dbh)
treedat$dbh_cm[treedat$plottag == 'E3-769' & 
                 treedat$doy == '103' & 
                 treedat$year == '2023'] <- 47.6

treedat <- treedat %>%
  rename('dbh' = 'dbh_cm',
         'basal_area' = 'basal_area_cm2',
         'kgc' = 'biomass_kgc')

# recalculate kgc and basal area
treedat$spp <- treedat$species
treedat$kgc <- calc_kgc(treedat)
treedat <- treedat %>% 
  mutate(basal_area = signif(pi*(dbh/2)^2, 4))


# only include data from plots 1-3
treedat <- treedat %>%
  mutate(pl_col = str_sub(plot, start = 2, end = 2))%>%
  filter(pl_col <= 3)


# summarize total biomass and basal area by species
# average per plot
# subset to only include last census of every year
tree_dates <- treedat %>%
  group_by(year)%>%
  summarize(lastday = max(doy, na.rm = TRUE))

treedat2 <- left_join(treedat, tree_dates, by = 'year')

treedat2 <- treedat2 %>%
  filter(year != '1993')%>%
  filter(doy == lastday)

tree_species <- treedat2 %>% 
  group_by(year, species) %>%
  summarise(kgc = sum(kgc, na.rm = TRUE)/(10^2 * pi *22),
            basal = sum(basal_area, na.rm = TRUE)/(10^2 * pi *22),
            n_trees = n())

# plot
ggplot()+
  geom_point(tree_species, mapping = aes(y = n_trees, x = year, col = species))+
  geom_line(tree_species, mapping = aes(y = n_trees, x = year, col = species))+
  geom_hline(yintercept = 0)+
  theme_minimal()+
  ylab('Number of Trees')+
  xlab('Year')+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+ 
  labs(col='Species') 

ggplot()+
  geom_point(tree_species, mapping = aes(y = kgc, x = year, col = species))+
  geom_line(tree_species, mapping = aes(y = kgc, x = year, col = species))+
  geom_hline(yintercept = 0)+
  theme_minimal()+
  ylab('KgC')+
  xlab('Year')+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+ 
  labs(col='Species') 


#----------by plot
tree_spplot <- treedat2 %>% 
  group_by(year, species, plot) %>%
  summarise(kgc = sum(kgc, na.rm = TRUE)/(10^2 * pi),
            basal = sum(basal_area, na.rm = TRUE)/(10^2 * pi),
            n_trees = n())
ggplot()+
  geom_point(tree_spplot, mapping = aes(y = kgc, x = year, col = plot))+
  geom_line(tree_spplot, mapping = aes(y = n_trees, x = year, col = plot))+
  geom_hline(yintercept = 0)+
  theme_minimal()+
  ylab('Number of Trees')+
  xlab('Year')+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+ 
  labs(col='Plot') +
  facet_wrap(~species)

ggplot()+
  geom_point(tree_spplot, mapping = aes(y = kgc, x = year, col = plot))+
  geom_line(tree_spplot, mapping = aes(y = kgc, x = year, col = plot))+
  geom_hline(yintercept = 0)+
  theme_minimal()+
  ylab('KgC')+
  xlab('Year')+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+ 
  labs(col='Species') +
  facet_wrap(~species)

#-----------------------------#
#-----Increment Growth--------#
#-----------------------------#
#------------------------------------------------
#------------------------------------------------#
# Load in growth data (created in anomalies.R)
#------------------------------------------------#
#------------------------------------------------#
growth <- read_csv('GitHub/Dendrometer/Data/ems_growth_data.csv')

#------------------------------
#-----------------------------#
#--Overall Growth by species--#
#-----------------------------#
#-----------------------------#
# subset to only include plots 3 and under
growth_3 <- filter(growth, pl_col <= 3)

# calculate growth averages for each year
# convert units to kg/yr*ha
# divided by 22 (total number of plots included)
grow_species <- growth_3 %>% 
  group_by(year.x, species) %>%
  summarise(growth = sum(kgc_gr, na.rm = TRUE)/(10^2 * pi *22),
            growth_inc = sum(kgc_inc, na.rm = TRUE)/(10^2 * pi *22))

# calculate plot averages
grow_species_avg <- grow_species %>%
  group_by(species)%>%
  summarize(growth_av = mean(growth, na.rm = TRUE),
            growth_av_inc = mean(growth_inc, na.rm = TRUE))

# combine averages with annual growths
grow_all <- left_join(grow_species, grow_species_avg, by = 'species')

# calculate anomaly
grow_all <- grow_all %>%
  mutate(anomaly = growth-growth_av,
         anomaly_inc = growth_inc-growth_av_inc)

# plot anomaly
ggplot()+
  geom_point(grow_all, mapping = aes(y = anomaly_inc, x = year.x, col = species))+
  geom_line(grow_all, mapping = aes(y = anomaly_inc, x = year.x, col = species))+
  geom_hline(yintercept = 0)+
  theme_minimal()+
  ylab('Increment (KgC/Year)')+
  xlab('Year')+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+ 
  labs(col='Species') 

# plot increment
ggplot()+
  geom_point(grow_all, mapping = aes(y = growth_inc, x = year.x, col = species))+
  geom_line(grow_all, mapping = aes(y = growth_inc, x = year.x, col = species))+
  theme_minimal()+
  ylab('Increment (KgC/Year)')+
  xlab('Year')+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+ 
  labs(col='Species') 

#------------------------------
#-----------------------------#
#---Overall Growth by plot----#
#-----------------------------#
#-----------------------------#
# subset to only include plots 3 and under
growth_3 <- filter(growth, pl_col <= 3)

# calculate growth averages for each year
# convert units to kg/yr*ha
# divided by 21 (c3 has no red oaks) and the size of the plot (average per plot) (kg/yr*ha)
grow_plot <- growth_3 %>% 
  group_by(year.x, plot.x) %>%
  summarise(growth = sum(kgc_gr, na.rm = TRUE)/(10^2 * pi ),
            growth_inc = sum(kgc_inc, na.rm = TRUE)/(10^2 * pi ))

# calculate plot averages
grow_plot_avg <- grow_plot %>%
  group_by(plot.x)%>%
  summarize(growth_av = mean(growth, na.rm = TRUE),
            growth_av_inc = mean(growth_inc, na.rm = TRUE))

# combine averages with annual growths
grow_all <- left_join(grow_plot, grow_plot_avg, by = 'plot.x')

# calculate anomaly
grow_all <- grow_all %>%
  mutate(anomaly = growth-growth_av,
         anomaly_inc = growth_inc-growth_av_inc)

# plot
ggplot()+
  geom_point(grow_all, mapping = aes(y = anomaly_inc, x = year.x, col = plot.x))+
  geom_line(grow_all, mapping = aes(y = anomaly_inc, x = year.x, col = plot.x))+
  geom_hline(yintercept = 0)+
  theme_minimal()+
  ylab('Increment (KgC/Year)')+
  xlab('Year')+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+ 
  labs(col='Plot') 


#-----------------------------
#-----------------------------#
#-----------Red Oaks----------#
#-----------------------------#
#-----------------------------#
# subset red oaks
grow_ro <- subset(growth, species == 'ro')

# subset to only include plots 3 and under
grow_ro <- filter(grow_ro, pl_col <= 3)

# calculate growth averages for each year
# convert units to kg/yr*ha
# divided by 21 (c3 has no red oaks) and the size of the plot (average per plot) (kg/yr*ha)
grow_ro_annual <- grow_ro %>% 
  group_by(year.x) %>%
  summarise(growth = sum(kgc_gr, na.rm = TRUE)/(10^2 * pi * 21),
            growth_inc = sum(kgc_inc, na.rm = TRUE)/(10^2 * pi * 21))

# calculate average over all years
overall_average <- mean(grow_ro_annual$growth, na.rm = TRUE)
overall_average_inc <- mean(grow_ro_annual$growth_inc, na.rm = TRUE)

# calculate the anomaly
grow_ro_annual <- grow_ro_annual %>%
  mutate(anomaly = growth - overall_average,
         anomaly_inc = growth_inc - overall_average_inc)

# make incremental anomaly plot
ggplot()+
  geom_point(grow_ro_annual, mapping = aes(y = anomaly_inc, x = year.x), col = 'darkred')+
  geom_line(grow_ro_annual, mapping = aes(y = anomaly_inc, x = year.x), col = 'darkred')+
  geom_hline(yintercept = 0)+
  theme_minimal()+
  ylab('Increment (KgC/Year)')+
  xlab('Year')+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))


#--------------------------#
#----size class growth-----
#--------------------------#
# make 4 equally sized bins
grow_ro <- grow_ro %>% 
  mutate(size_bin = cut(dbh.x, breaks=c(12.39,32.37,39.86,48.5,78)))

# calculate the anomaly
grow_ro_bin <- grow_ro %>% 
  group_by(year.x, size_bin) %>%
  summarise(growth = sum(kgc_gr, na.rm = TRUE)/(10^2 * pi *22),
            growth_inc = sum(kgc_inc, na.rm = TRUE)/(10^2 * pi *22))
# calculate plot averages
grow_ro_avg <- grow_ro_bin %>%
  group_by(size_bin)%>%
  summarize(growth_av = mean(growth, na.rm = TRUE),
            growth_av_inc = mean(growth_inc, na.rm = TRUE))

# combine averages with annual growths
grow_all <- left_join(grow_ro_bin, grow_ro_avg, by = 'size_bin')

# calculate anomaly
grow_all <- grow_all %>%
  mutate(anomaly = growth-growth_av,
         anomaly_inc = growth_inc-growth_av_inc)

grow_ro_annual <- grow_ro %>% 
  group_by(year.x, size_bin) %>%
  summarise(growth = sum(kgc_gr, na.rm = TRUE)/(10^2 * pi * 21),
            growth_inc = sum(kgc_inc, na.rm = TRUE)/(10^2 * pi * 21))

# make incremental anomaly plot
ggplot()+
  geom_point(grow_ro_annual, mapping = aes(y = growth_inc, x = year.x, col = size_bin))+
  geom_line(grow_ro_annual, mapping = aes(y = growth_inc, x = year.x, col = size_bin))+
  geom_hline(yintercept = 0)+
  theme_minimal()+
  ylab('Increment (KgC/Year)')+
  xlab('Year')+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))

ggplot()+
  geom_point(grow_ro_annual, mapping = aes(y = growth, x = year.x, col = size_bin))+
  geom_line(grow_ro_annual, mapping = aes(y = growth, x = year.x, col = size_bin))+
  geom_hline(yintercept = 0)+
  theme_minimal()+
  ylab('KgC/yr*ha')+
  xlab('Year')+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))

#write_csv(grow_ro_annual, 'GitHub/Dendrometer/Data/ro_growth_annual.csv')



#-----------------------------
#-----------------------------#
#----------Hemlocks-----------#
#-----------------------------#
#-----------------------------#
# subset hemlocks
grow_hem <- subset(growth, species == 'hem')

# subset to only include plots 3 and under
grow_hem <- filter(grow_hem, pl_col <= 3)

# calculate growth averages for each year
# convert units to kg/yr*ha
# divided by 21 (c3 has no red oaks) and the size of the plot (average per plot) (kg/yr*ha)
grow_hem_annual <- grow_hem %>% 
  group_by(year.x) %>%
  summarise(growth = sum(kgc_gr, na.rm = TRUE)/(10^2 * pi * 21),
            growth_inc = sum(kgc_inc, na.rm = TRUE)/(10^2 * pi * 21))

# calculate average over all years
overall_average <- mean(grow_hem_annual$growth, na.rm = TRUE)
overall_average_inc <- mean(grow_hem_annual$growth_inc, na.rm = TRUE)

# calculate the anomaly
grow_hem_annual <- grow_hem_annual %>%
  mutate(anomaly = growth - overall_average,
         anomaly_inc = growth_inc - overall_average_inc)

# make incremental anomaly plot
ggplot()+
  geom_point(grow_hem_annual, mapping = aes(y = anomaly_inc, x = year.x), col = 'darkgreen')+
  geom_line(grow_hem_annual, mapping = aes(y = anomaly_inc, x = year.x), col = 'darkgreen')+
  geom_hline(yintercept = 0)+
  theme_minimal()+
  ylab('Increment (KgC/Year)')+
  xlab('Year')+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))

#--------------------------#
#----size class growth-----
#--------------------------#
# make 4 equally sized bins
grow_hem <- grow_hem %>% 
  mutate(size_bin = cut(dbh.x, breaks=c(9.89,12.96,19.45,33.09,64)))

# calculate the anomaly
grow_hem_bin <- grow_hem %>% 
  group_by(year.x, size_bin) %>%
  summarise(growth = sum(kgc_gr, na.rm = TRUE)/(10^2 * pi *22),
            growth_inc = sum(kgc_inc, na.rm = TRUE)/(10^2 * pi *22))
# calculate plot averages
grow_hem_avg <- grow_hem_bin %>%
  group_by(size_bin)%>%
  summarize(growth_av = mean(growth, na.rm = TRUE),
            growth_av_inc = mean(growth_inc, na.rm = TRUE))

# combine averages with annual growths
grow_all <- left_join(grow_hem_bin, grow_hem_avg, by = 'size_bin')

# calculate anomaly
grow_all <- grow_all %>%
  mutate(anomaly = growth-growth_av,
         anomaly_inc = growth_inc-growth_av_inc)

grow_hem_annual <- grow_hem %>% 
  group_by(year.x, size_bin) %>%
  summarise(growth = sum(kgc_gr, na.rm = TRUE)/(10^2 * pi * 21),
            growth_inc = sum(kgc_inc, na.rm = TRUE)/(10^2 * pi * 21))

# make incremental anomaly plot
ggplot()+
  geom_point(grow_hem_annual, mapping = aes(y = growth_inc, x = year.x, col = size_bin))+
  geom_line(grow_hem_annual, mapping = aes(y = growth_inc, x = year.x, col = size_bin))+
  geom_hline(yintercept = 0)+
  theme_minimal()+
  ylab('Increment (KgC/Year)')+
  xlab('Year')+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))
ggplot()+
  geom_point(grow_hem_annual, mapping = aes(y = growth, x = year.x, col = size_bin))+
  geom_line(grow_hem_annual, mapping = aes(y = growth, x = year.x, col = size_bin))+
  geom_hline(yintercept = 0)+
  theme_minimal()+
  ylab('KgC/Year*ha')+
  xlab('Year')+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))

#-------------------------------
#------------------------------#
#----------Red Maples----------#
#------------------------------#
#------------------------------#
# subset hemlocks
grow_rm <- subset(growth, species == 'rm')

# subset to only include plots 3 and under
grow_rm <- filter(grow_rm, pl_col <= 3)

# calculate growth averages for each year
# convert units to kg/yr*ha
# divided by 21 (c3 has no red oaks) and the size of the plot (average per plot) (kg/yr*ha)
grow_rm_annual <- grow_rm %>% 
  group_by(year.x) %>%
  summarise(growth = sum(kgc_gr, na.rm = TRUE)/(10^2 * pi * 21),
            growth_inc = sum(kgc_inc, na.rm = TRUE)/(10^2 * pi * 21))

# calculate average over all years
overall_average <- mean(grow_rm_annual$growth, na.rm = TRUE)
overall_average_inc <- mean(grow_rm_annual$growth_inc, na.rm = TRUE)

# calculate the anomaly
grow_rm_annual <- grow_rm_annual %>%
  mutate(anomaly = growth - overall_average,
         anomaly_inc = growth_inc - overall_average_inc)

# make incremental anomaly plot
ggplot()+
  geom_point(grow_rm_annual, mapping = aes(y = anomaly_inc, x = year.x), col = 'darkgreen')+
  geom_line(grow_rm_annual, mapping = aes(y = anomaly_inc, x = year.x), col = 'darkgreen')+
  geom_hline(yintercept = 0)+
  theme_minimal()+
  ylab('Increment (KgC/Year)')+
  xlab('Year')+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))



#--------------------------#
#----size class growth-----
#--------------------------#
# make 4 equally sized bins
grow_rm <- grow_rm %>% 
  mutate(size_bin = cut(dbh.x, breaks=c(9.55,14.67,19.23,25.36,60)))

# calculate the anomaly
grow_rm_bin <- grow_rm %>% 
  group_by(year.x, size_bin) %>%
  summarise(growth = sum(kgc_gr, na.rm = TRUE)/(10^2 * pi *22),
            growth_inc = sum(kgc_inc, na.rm = TRUE)/(10^2 * pi *22))
# calculate plot averages
grow_rm_avg <- grow_rm_bin %>%
  group_by(size_bin)%>%
  summarize(growth_av = mean(growth, na.rm = TRUE),
            growth_av_inc = mean(growth_inc, na.rm = TRUE))

# combine averages with annual growths
grow_all <- left_join(grow_rm_bin, grow_rm_avg, by = 'size_bin')

# calculate anomaly
grow_all <- grow_all %>%
  mutate(anomaly = growth-growth_av,
         anomaly_inc = growth_inc-growth_av_inc)

grow_rm_annual <- grow_rm %>% 
  group_by(year.x, size_bin) %>%
  summarise(growth = sum(kgc_gr, na.rm = TRUE)/(10^2 * pi * 21),
            growth_inc = sum(kgc_inc, na.rm = TRUE)/(10^2 * pi * 21))

# make incremental anomaly plot
ggplot()+
  geom_point(grow_rm_annual, mapping = aes(y = growth_inc, x = year.x, col = size_bin))+
  geom_line(grow_rm_annual, mapping = aes(y = growth_inc, x = year.x, col = size_bin))+
  geom_hline(yintercept = 0)+
  theme_minimal()+
  ylab('Increment (KgC/Year)')+
  xlab('Year')+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))

ggplot()+
  geom_point(grow_rm_annual, mapping = aes(y = growth, x = year.x, col = size_bin))+
  geom_line(grow_rm_annual, mapping = aes(y = growth, x = year.x, col = size_bin))+
  geom_hline(yintercept = 0)+
  theme_minimal()+
  ylab('KgC/yr*ha')+
  xlab('Year')+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))


#-----------------------------
#-----------------------------#
#-------Growth by plot--------#
#-----------------------------#
#-----------------------------#
# subset red oaks
grow_ro <- subset(growth, species == 'ro')

# subset to only include plots 3 and under
grow_ro <- filter(grow_ro, pl_col <= 3)

# calculate growth averages for each year
# convert units to kg/yr*ha
# divided by 21 (c3 has no red oaks) and the size of the plot (average per plot) (kg/yr*ha)
grow_ro_plot <- grow_ro %>% 
  group_by(year.x, plot.x) %>%
  summarise(growth = sum(kgc_gr, na.rm = TRUE)/(10^2 * pi ),
            growth_inc = sum(kgc_inc, na.rm = TRUE)/(10^2 * pi ))

# calculate plot averages
grow_ro_plot_avg <- grow_ro_plot %>%
  group_by(plot.x)%>%
  summarize(growth_av = mean(growth, na.rm = TRUE),
            growth_av_inc = mean(growth_inc, na.rm = TRUE))

# combine averages with annual growths
grow_ro_all <- left_join(grow_ro_plot, grow_ro_plot_avg, by = 'plot.x')

# calculate anomaly
grow_ro_all <- grow_ro_all %>%
  mutate(anomaly = growth-growth_av,
         anomaly_inc = growth_inc-growth_av_inc)

# incremental growth plot
ggplot()+
  geom_point(grow_ro_all, mapping = aes(y = anomaly_inc, x = year.x, col = plot.x))+
  geom_line(grow_ro_all, mapping = aes(y = anomaly_inc, x = year.x, col = plot.x))+
  geom_hline(yintercept = 0)+
  theme_minimal()+
  ylab('Increment (KgC/Year)')+
  xlab('Year')+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+ 
  labs(col='Plot') 


#------------------------------
#-----------------------------#
#------Hemlocks by plot-------#
#-----------------------------#
#-----------------------------#
# subset Hemlocks
grow_hem <- subset(growth, species == 'hem')

# subset to only include plots 3 and under
grow_hem <- filter(grow_hem, pl_col <= 3)

# calculate growth averages for each year
# convert units to kg/yr*ha
# divided by 21 (c3 has no red oaks) and the size of the plot (average per plot) (kg/yr*ha)
grow_hem_plot <- grow_hem %>% 
  group_by(year.x, plot.x) %>%
  summarise(growth = sum(kgc_gr, na.rm = TRUE)/(10^2 * pi ),
            growth_inc = sum(kgc_inc, na.rm = TRUE)/(10^2 * pi ))

# calculate plot averages
grow_hem_plot_avg <- grow_hem_plot %>%
  group_by(plot.x)%>%
  summarize(growth_av = mean(growth, na.rm = TRUE),
            growth_av_inc = mean(growth_inc, na.rm = TRUE))

# combine averages with annual growths
grow_hem_all <- left_join(grow_hem_plot, grow_hem_plot_avg, by = 'plot.x')

# calculate anomaly
grow_hem_all <- grow_hem_all %>%
  mutate(anomaly = growth-growth_av,
         anomaly_inc = growth_inc-growth_av_inc)

# plot
ggplot()+
  geom_point(grow_hem_all, mapping = aes(y = anomaly_inc, x = year.x, col = plot.x))+
  geom_line(grow_hem_all, mapping = aes(y = anomaly_inc, x = year.x, col = plot.x))+
  geom_hline(yintercept = 0)+
  theme_minimal()+
  ylab('Increment (KgC/Year)')+
  xlab('Year')+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+ 
  labs(col='Plot') 






#-----------------------------#
#----------Mortality----------#
#-----------------------------#


#-----------------------------#
#---------Mortality-----------#
#-----------------------------#

