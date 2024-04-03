# Goals:
#- sum oak biomass divided by 22 and the size of the plot (average per plot) (kg/yr*ha)
#- do same for hemlock
#- sum of biomass anomaly per plot (actual-mean)
#- do same with climate data
#- spring, summer, annual 
#- both total change and anomaly (actual-mean)
#- make sure to clip to only include period that overlaps with flux tower

library(tidyverse)
library(here)
library(dplyr)
d

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


# load in archive data
treedat <- read_csv('GitHub/Dendrometer/ems-tree-summ-w-2023.csv')

# filter to only 2023
treedat <- filter(treedat, year == '2023')

treedat2 <- distinct(treedat, plottag, .keep_all = TRUE)


# total trees in each plot
treedat_plot <- treedat2 %>%
  group_by(plot) %>%
  summarise(total = sum(basal_area_cm2, na.rm =TRUE))

# number of red oak
ro <- filter(treedat, species == 'ro' & tree_type == 'live')

# subset to 2023
ro <- filter(ro, year == '2023')
ro2 <- distinct(ro, plottag, .keep_all = TRUE)

redoak_count <- ro2 %>%
  group_by(plot)%>%
  summarise(ro_ba = sum(basal_area_cm2, na.rm =TRUE))%>%
  mutate(pl_ro = substr(plot, 1,1),
         pl_col= substr(plot,2,2))
# combine with treedat_plot
ro_merge <- left_join(redoak_count, treedat_plot, by = c('plot'))

redoak_count2 <- ro_merge %>%
  mutate(basal_area_fraction = ro_ba/total)%>%
  mutate(fil = 1)%>%
  filter(pl_col <=3)

ggplot()+
  geom_tile(redoak_count2, mapping = aes(x=plot, y = fil, fill = basal_area_fraction))+
  theme_minimal()




# unique trees in each year
redoak_count <- ro %>%
  group_by(year, plot) %>%
  summarise(n_unique = n_distinct(plottag))%>%
  mutate(pl_ro = substr(plot, 1,1),
          pl_col= substr(plot,2,2))

# combine with treedat_plot
ro_merge <- left_join(redoak_count, treedat_plot, by = c('plot','year'))

redoak_count2 <- ro_merge %>%
  mutate(fraction = n_unique/total)%>%
  group_by(plot) %>%
  summarise(fract_mean = mean(fraction, na.rm = TRUE),
            act_mean = mean(n_unique, na.rm = TRUE))%>%
  mutate(pl_ro = substr(plot, 1,1),
         pl_col= substr(plot,2,2))

redoak_count2 <- redoak_count2 %>%
  mutate(fil = 1)%>%
  filter(pl_col <=3)

ggplot()+
  geom_tile(redoak_count2, mapping = aes(x=plot, y = fil, fill = fract_mean))+
  theme_minimal()

ggplot()+
  geom_tile(redoak_count2, mapping = aes(x=plot, y = fil, fill = act_mean))+
  theme_minimal()

#"GitHub/Dendrometer/2022_archive/ems-tree-summ.csv")

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

# remove dead trees
treedat <- filter(treedat, tree_type != 'dead')

# calculate growth rates
temp <- filter(treedat,year != '1993')
years <- unique(temp$year)

growth <- data.frame()

for(year in 1:(length(years)-1)){
  current <- years[year]
  future <- years[year+1]
  print(current)
  print(future)
  old <- filter(treedat, year ==  current & tree_type != "dead")
  new <- filter(treedat, year == future & tree_type != "dead")
  old_date <- min(old$doy, na.rm = TRUE)
  new_date <- min(new$doy, na.rm = TRUE)
  old <- filter(old, doy <= old_date+14)
  new <- filter(new, doy <= new_date+14)
  # only include unique observations
  old <- old %>% distinct(plottag, .keep_all = TRUE)
  new <- new %>% distinct(plottag, .keep_all = TRUE)
  old <- subset(old, select = c('plottag', 'dbh','year','plot','species','kgc','basal_area'))
  new <- subset(new, select = c('plottag', 'dbh','year','plot','kgc','basal_area'))
  full <- left_join(old, new, by = "plottag")
  # calculate 
  temp <- full %>%
    mutate(dbh_gr = dbh.y-dbh.x,
           basal_gr = (basal_area.y-basal_area.x)/basal_area.x,
           dbh_inc = (dbh.y-dbh.x)/dbh.x,
           kgc_inc = (kgc.y-kgc.x)/kgc.x,
           basal_inc = basal_area.y-basal_area.x,
           kgc_gr = kgc.y-kgc.x,
           pl_row = str_sub(plot.x,1,1),
           pl_col = as.numeric(str_sub(plot.x,2,2)))
  growth <- rbind(growth,temp)
} 

#-----------------------------#
#-----------------------------#
#---export growth data--------
#-----------------------------#
#-----------------------------#
write_csv(growth, 'GitHub/Dendrometer/Data/ems_growth_data.csv')


#-----------------------------#
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


# make anomaly plot
ggplot()+
  geom_point(grow_ro_annual, mapping = aes(y = anomaly, x = year.x))+
  geom_line(grow_ro_annual, mapping = aes(y = anomaly, x = year.x))+
  geom_hline(yintercept = 0)+
  theme_minimal()

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

# export as eps file
ggsave(file = "GitHub/Dendrometer/overallredoaks_bold.eps") 

# make incremental anomaly plot
ggplot()+
  geom_point(grow_ro_annual, mapping = aes(y = anomaly_inc, x = year.x))+
  geom_line(grow_ro_annual, mapping = aes(y = anomaly_inc, x = year.x))+
  geom_hline(yintercept = 0)+
  theme_minimal()+
  ylab('Increment (KgC/Year)')+
  xlab('Year')+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14))

# export as eps file
ggsave(file = "GitHub/Dendrometer/overallredoaks.eps") 

#-----------------------------#
#------look at e3------------
#-----------------------------#
e3 <- filter(grow_ro, plot.x == 'E3')

# add tree index starting from 1
plottag = unique(e3$plottag)
trees_e3 <- as.data.frame(plottag)
trees_e3$index <- 1:nrow(trees_e3)
e3 <- left_join(e3, trees_e3, by = 'plottag')

# calculate individual anomalies
# calculate average over all years
overall_average_inc <- mean(e3$kgc_inc, na.rm = TRUE)

# calculate the anomaly
e3 <- e3 %>%
  mutate(anomaly_inc = kgc_inc - overall_average_inc)

# plot
ggplot()+
  geom_point(e3, mapping = aes(y = anomaly_inc, x = year.x, group = index, col = index))+
  geom_line(e3, mapping = aes(y = anomaly_inc, x = year.x, group = index, col = index))+
  geom_hline(yintercept = 0)+
  theme_minimal()+
  ylab('Increment (KgC/Year)')+
  xlab('Year')+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))

# export as eps file
ggsave(file = "GitHub/Dendrometer/e3_redoaks_bold.eps")  

# plot
ggplot()+
  geom_point(e3, mapping = aes(y = anomaly_inc, x = year.x, group = index, col = index))+
  geom_line(e3, mapping = aes(y = anomaly_inc, x = year.x, group = index, col = index))+
  geom_hline(yintercept = 0)+
  theme_minimal()+
  ylab('Increment (KgC/Year)')+
  xlab('Year')+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14))
# export as eps file
ggsave(file = "GitHub/Dendrometer/e3_redoaks.eps")  

#-----------------------------#
#------look at b3------------
#-----------------------------#
b3 <- filter(grow_ro, plot.x == 'B3')

# add tree index starting from 1
plottag = unique(b3$plottag)
trees_b3 <- as.data.frame(plottag)
trees_b3$index <- 1:nrow(trees_b3)
b3 <- left_join(b3, trees_b3, by = 'plottag')

# calculate individual anomalies
# calculate average over all years
overall_average_inc <- mean(b3$kgc_inc, na.rm = TRUE)

# calculate the anomaly
b3 <- b3 %>%
  mutate(anomaly_inc = kgc_inc - overall_average_inc)

# plot
ggplot()+
  geom_point(b3, mapping = aes(y = anomaly_inc, x = year.x, group = index, col = index))+
  geom_line(b3, mapping = aes(y = anomaly_inc, x = year.x, group = index, col = index))+
  geom_hline(yintercept = 0)+
  theme_minimal()+
  ylab('Increment (KgC/Year)')+
  xlab('Year')+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))

# export as eps file
ggsave(file = "GitHub/Dendrometer/b3_redoaks_bold.eps") 


# plot
ggplot()+
  geom_point(b3, mapping = aes(y = anomaly_inc, x = year.x, group = index, col = index))+
  geom_line(b3, mapping = aes(y = anomaly_inc, x = year.x, group = index, col = index))+
  geom_hline(yintercept = 0)+
  theme_minimal()+
  ylab('Increment (KgC/Year)')+
  xlab('Year')+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14))

# export as eps file
ggsave(file = "GitHub/Dendrometer/b3_redoaks.eps") 

#-----------------------------#
#-----------------------------#
#----------Hemlocks------------
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


# make anomaly plot
ggplot()+
  geom_point(grow_hem_annual, mapping = aes(y = anomaly, x = year.x))+
  geom_line(grow_hem_annual, mapping = aes(y = anomaly, x = year.x))+
  geom_hline(yintercept = 0)+
  theme_minimal()

# make incremental anomaly plot
ggplot()+
  geom_point(grow_hem_annual, mapping = aes(y = anomaly_inc, x = year.x))+
  geom_line(grow_hem_annual, mapping = aes(y = anomaly_inc, x = year.x))+
  geom_hline(yintercept = 0)+
  theme_minimal()

#-----------------------------#
#-----------------------------#
#-------Anomaly by plot--------
#-----------------------------#
#-----------------------------#
# subset red oaks
grow_ro <- subset(growth, species == 'ro')

grow_23 <- filter(grow_ro, year.x == '2022' & plot.x == 'E3')
grow_22 <- filter(grow_ro, year.x == '2021' & plot.x == 'E3')
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

# plot
ggplot()+
  geom_point(grow_ro_all, mapping = aes(y = anomaly, x = year.x, col = plot.x))+
  geom_line(grow_ro_all, mapping = aes(y = anomaly, x = year.x, col = plot.x))+
  geom_hline(yintercept = 0)+
  theme_minimal()

ggplot()+
  geom_point(grow_ro_all, mapping = aes(y = anomaly_inc, x = year.x, col = plot.x))+
  geom_line(grow_ro_all, mapping = aes(y = anomaly_inc, x = year.x, col = plot.x))+
  geom_hline(yintercept = 0)+
  theme_minimal()
#-----------------------------#
#-----------------------------#
#------Hemlocks by plot--------
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
  geom_point(grow_hem_all, mapping = aes(y = anomaly, x = year.x, col = plot.x))+
  geom_line(grow_hem_all, mapping = aes(y = anomaly, x = year.x, col = plot.x))+
  geom_hline(yintercept = 0)+
  theme_minimal()

ggplot()+
  geom_point(grow_hem_all, mapping = aes(y = anomaly_inc, x = year.x, col = plot.x))+
  geom_line(grow_hem_all, mapping = aes(y = anomaly_inc, x = year.x, col = plot.x))+
  geom_hline(yintercept = 0)+
  theme_minimal()

#-----------------------------#
#-----------------------------#
#------Combine data??? think about this t test--------
#-----------------------------#
#-----------------------------#
grow_hem_ann <- grow_hem_annual %>%
  subset(select = c(year.x, anomaly_inc, growth_inc)) %>%
  rename('year' = 'year.x',
         'hem_anom' = 'anomaly_inc',
         'hem_growth' = 'growth_inc')

grow_ro_ann <- grow_ro_annual %>%
  subset(select = c(year.x, anomaly_inc, growth_inc)) %>%
  rename('year' = 'year.x',
         'ro_anom' = 'anomaly_inc',
         'ro_growth' = 'growth_inc')


hem_oak <- inner_join(grow_ro_ann, grow_hem_ann, by = 'year')

ttest <- t.test(hem_oak$ro_anom, hem_oak$hem_anom, paired = TRUE)



#-----------------------------#
#-----------------------------#
#-----------Climate-----------
#-----------------------------#
#-----------------------------#
# read in climate data
mon_clim <- read_csv("~/GitHub/Dendrometer/archive_data/hf300-03-monthly-m.csv")
clim <- read_csv("~/GitHub/Dendrometer/archive_data/hf300-01-annual-m.csv")

# create month and year column
mon_clim <- mon_clim %>%
  mutate(month = str_sub(date, 6,7),
         year = as.numeric(str_sub(date, 1,4)),
         prev_year = as.numeric(str_sub(date, 1,4))-1)

# subset years to 1998-2022
mon_clim <- filter(mon_clim, year >=1998)

# make different climate metrics
# summer (may-sept) (5-9)
summer = c('05','06','07','08','09')
summr <- subset(mon_clim, month %in% summer)

sum_sum <- summr %>%
  group_by(year)%>%
  summarize(sumair_t = mean(airt, na.rm = TRUE),
            sumair_tmin = mean(airtmin, na.rm = TRUE),
            sumair_tmax = mean(airtmax, na.rm = TRUE),
            sumprecip = mean(prec, na.rm = TRUE))

# early summer (may/june)
early_sum = c('05','06')
early_sm <- subset(mon_clim, month %in% early_sum)

early_sum_sum <- early_sm %>%
  group_by(year)%>%
  summarize(sprair_t = mean(airt, na.rm = TRUE),
            sprair_tmin = mean(airtmin, na.rm = TRUE),
            sprair_tmax = mean(airtmax, na.rm = TRUE),
            sprprecip = mean(prec, na.rm = TRUE))

# add to annual climate dataframe
climate <- right_join(clim, early_sum_sum, by = 'year')
climate <- right_join(climate, sum_sum, by = 'year')

# subtract overall mean from each value to find anomaly
overall_means2 <- as.data.frame(colMeans(climate))

overall_means2[1,]

# create df of anomalies
climate_anomaly <- climate

for (col in 2:length(colnames(climate_anomaly))){ 
  for (row in 1:nrow(climate_anomaly)){
    climate_anomaly[row, col] <- climate_anomaly[row, col] - overall_means2[col,]
  }
}

# make climate anomoly plots
ggplot()+
  geom_point(climate_anomaly, mapping = aes(y = airt, x = year))+
  geom_line(climate_anomaly, mapping = aes(y = airt, x = year))+
  geom_hline(yintercept = 0)+
  theme_minimal()

ggplot()+
  geom_point(climate_anomaly, mapping = aes(y = prec, x = year))+
  geom_line(climate_anomaly, mapping = aes(y = prec, x = year))+
  geom_hline(yintercept = 0)+
  theme_minimal()

ggplot()+
  geom_point(climate_anomaly, mapping = aes(y = sprair_t, x = year))+
  geom_line(climate_anomaly, mapping = aes(y = sprair_t, x = year))+
  geom_hline(yintercept = 0)+
  theme_minimal()

ggplot()+
  geom_point(climate_anomaly, mapping = aes(y = sprprecip, x = year))+
  geom_line(climate_anomaly, mapping = aes(y = sprprecip, x = year))+
  geom_hline(yintercept = 0)+
  theme_minimal()


ggplot()+
  geom_point(climate_anomaly, mapping = aes(y = sumair_t, x = year))+
  geom_line(climate_anomaly, mapping = aes(y = sumair_t, x = year))+
  geom_hline(yintercept = 0)+
  theme_minimal()

ggplot()+
  geom_point(climate_anomaly, mapping = aes(y = sumprecip, x = year))+
  geom_line(climate_anomaly, mapping = aes(y = sumprecip, x = year))+
  geom_hline(yintercept = 0)+
  theme_minimal()


#-----------------------------#
#-----------------------------#
#------Climate and Growth------
#-----------------------------#
#-----------------------------#
# summer precip
coeff = 1500
ggplot()+
  geom_point(grow_hem_annual, mapping = aes(y = anomaly, x = year.x), col = 'chartreuse4')+
  geom_line(grow_hem_annual, mapping = aes(y = anomaly, x = year.x), col = 'chartreuse4')+
  geom_point(grow_ro_annual, mapping = aes(y = anomaly, x = year.x), col = 'red')+
  geom_line(grow_ro_annual, mapping = aes(y = anomaly, x = year.x), col = 'red')+
  geom_point(climate_anomaly, mapping = aes(y = sumprecip/coeff, x = year), col = 'purple')+
  geom_line(climate_anomaly, mapping = aes(y = sumprecip/coeff, x = year), col = 'purple')+
  geom_hline(yintercept = 0)+
  theme_minimal()+
  scale_y_continuous(
    limits = c(-0.03,0.06),
    # Features of the first axis
    name = "Growth Anomaly",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*coeff, name="Summer Prec Anomaly")
  )

# spring precip
coeff = 1500
ggplot()+
  geom_point(grow_hem_annual, mapping = aes(y = anomaly, x = year.x), col = 'chartreuse4')+
  geom_line(grow_hem_annual, mapping = aes(y = anomaly, x = year.x), col = 'chartreuse4')+
  geom_point(grow_ro_annual, mapping = aes(y = anomaly, x = year.x), col = 'red')+
  geom_line(grow_ro_annual, mapping = aes(y = anomaly, x = year.x), col = 'red')+
  geom_point(climate_anomaly, mapping = aes(y = sprprecip/coeff, x = year), col = 'purple')+
  geom_line(climate_anomaly, mapping = aes(y = sprprecip/coeff, x = year), col = 'purple')+
  geom_hline(yintercept = 0)+
  theme_minimal()+
  scale_y_continuous(
    limits = c(-0.04,0.06),
     #Features of the first axis
     name = "Growth Anomaly",
    
    # Add a second axis and specify its features
   sec.axis = sec_axis(~.*coeff, name="Spring Prec Anomaly")
  )


# Annual Precip
coeff = 10000
ggplot()+
  geom_point(grow_hem_annual, mapping = aes(y = anomaly, x = year.x), col = 'chartreuse4')+
  geom_line(grow_hem_annual, mapping = aes(y = anomaly, x = year.x), col = 'chartreuse4')+
  geom_point(grow_ro_annual, mapping = aes(y = anomaly, x = year.x), col = 'red')+
  geom_line(grow_ro_annual, mapping = aes(y = anomaly, x = year.x), col = 'red')+
  geom_point(climate_anomaly, mapping = aes(y = prec/coeff, x = year), col = 'purple')+
  geom_line(climate_anomaly, mapping = aes(y = prec/coeff, x = year), col = 'purple')+
  geom_hline(yintercept = 0)+
  theme_minimal()+
  scale_y_continuous(
    limits = c(-0.04,0.07),
  #Features of the first axis
   name = "Growth Anomaly",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*coeff, name="Preciptiation Anomaly")
  )

# summer air temp
coeff = 50
ggplot()+
  geom_point(grow_hem_annual, mapping = aes(y = anomaly, x = year.x), col = 'chartreuse4')+
  geom_line(grow_hem_annual, mapping = aes(y = anomaly, x = year.x), col = 'chartreuse4')+
  geom_point(grow_ro_annual, mapping = aes(y = anomaly, x = year.x), col = 'red')+
  geom_line(grow_ro_annual, mapping = aes(y = anomaly, x = year.x), col = 'red')+
  geom_point(climate_anomaly, mapping = aes(y = sumair_t/coeff, x = year), col = 'purple')+
  geom_line(climate_anomaly, mapping = aes(y = sumair_t/coeff, x = year), col = 'purple')+
  geom_hline(yintercept = 0)+
  theme_minimal()+
  scale_y_continuous(
    limits = c(-0.03,0.04),
    # Features of the first axis
    name = "Growth Anomaly",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*coeff, name="Summer Air Temp Anomaly")
  )

# spring air temp
coeff = 50
ggplot()+
  geom_point(grow_hem_annual, mapping = aes(y = anomaly, x = year.x), col = 'chartreuse4')+
  geom_line(grow_hem_annual, mapping = aes(y = anomaly, x = year.x), col = 'chartreuse4')+
  geom_point(grow_ro_annual, mapping = aes(y = anomaly, x = year.x), col = 'red')+
  geom_line(grow_ro_annual, mapping = aes(y = anomaly, x = year.x), col = 'red')+
  geom_point(climate_anomaly, mapping = aes(y = sprair_t/coeff, x = year), col = 'purple')+
  geom_line(climate_anomaly, mapping = aes(y = sprair_t/coeff, x = year), col = 'purple')+
  geom_hline(yintercept = 0)+
  theme_minimal()+
  scale_y_continuous(
    limits = c(-0.03,0.04),
    # Features of the first axis
    name = "Growth Anomaly",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*coeff, name="Spring Air Temp Anomaly")
  )

# Annual air temp
coeff = 50
ggplot()+
  geom_point(grow_hem_annual, mapping = aes(y = anomaly, x = year.x), col = 'chartreuse4')+
  geom_line(grow_hem_annual, mapping = aes(y = anomaly, x = year.x), col = 'chartreuse4')+
  geom_point(grow_ro_annual, mapping = aes(y = anomaly, x = year.x), col = 'red')+
  geom_line(grow_ro_annual, mapping = aes(y = anomaly, x = year.x), col = 'red')+
  geom_point(climate_anomaly, mapping = aes(y = airt/coeff, x = year), col = 'purple')+
  geom_line(climate_anomaly, mapping = aes(y = airt/coeff, x = year), col = 'purple')+
  geom_hline(yintercept = 0)+
  theme_minimal()+
  scale_y_continuous(
    limits = c(-0.03,0.04),
    #Features of the first axis
    name = "Growth Anomaly",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*coeff, name="Air Temp Anomaly")
  )
