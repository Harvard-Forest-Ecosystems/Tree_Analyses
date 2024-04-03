library(tidyverse)
library(here)
library(dplyr)
library(gridExtra)
library(segmented)
#------------data-----------------
growth <- read_csv('GitHub/Dendrometer/Data/ems_growth_data.csv')


# try using end of year to end of year for growth
# load in archive data
treedat <- read_csv('GitHub/Dendrometer/ems-tree-summ-w-2023.csv')
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
           pl_row = substr(plot.x,1,1),
           pl_col = as.numeric(substr(plot.x,2,2)))
  growth <- rbind(growth,temp)
} 



#-------------------------------------#
#------------All Trees-----------------
#-------------------------------------#
# subset to only include first 3 plots and red oaks
grow_all <- growth %>%
  filter(pl_col <= 3)

grow_an <- grow_all %>% 
  group_by(year.x) %>%
  summarise(growth = sum(kgc_gr, na.rm = TRUE)/(10^2 * pi * 21),
            growth_ha = growth*10000,
            trees = n())

ggplot()+
  geom_point(grow_an, mapping = aes(y = growth_ha, x = year.x))+
  geom_line(grow_an, mapping = aes(y = growth_ha, x = year.x))+
  theme_minimal()+
  ylab('KgC/ha')+
  xlab('Year')+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))

# segmented linear regression
out.lm<-lm(growth_ha~year.x,data=grow_an)
o<-segmented(out.lm)
summary(o)
#plot original data
plot(grow_an$year.x, grow_an$growth_ha, pch=16, col='steelblue')
#add segmented regression model
plot(o, add=T)
#-----------------------------------#
#------------Red Oaks----------------
#-----------------------------------#
# subset to only include first 3 plots and red oaks
grow_ro <- growth %>%
  filter(species == 'ro')%>%
  filter(pl_col <= 3)

grow_ro <- grow_ro %>% 
  group_by(year.x) %>%
  summarise(growth = sum(kgc_gr, na.rm = TRUE)/(10^2 * pi * 21),
            growth_ha = growth*10000)

ggplot()+
  geom_point(grow_ro, mapping = aes(y = growth_ha, x = year.x), col = 'indianred')+
  geom_line(grow_ro, mapping = aes(y = growth_ha, x = year.x), col = 'indianred')+
  geom_hline(yintercept = 0)+
  theme_minimal()+
  ylab('KgC/ha')+
  ggtitle('Red Oaks')+
  xlab('Year')+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14),
        title = element_text(size=14,face="bold"))

# segmented linear regression
out.lm<-lm(growth_ha~year.x,data=grow_ro)
o<-segmented(out.lm)
summary(o)

#plot original data
plot(grow_ro$year.x, grow_ro$growth_ha, pch=16, col='steelblue')
#add segmented regression model
plot(o, add=T)

#-----------------------------------#
#------------Hemlocks----------------
#-----------------------------------#
# subset to only include first 3 plots and red oaks
grow_hem <- growth %>%
  filter(species == 'hem')%>%
  filter(pl_col <= 3)

grow_hem <- grow_hem %>% 
  group_by(year.x) %>%
  summarise(growth = sum(kgc_gr, na.rm = TRUE)/(10^2 * pi * 21),
            growth_ha = growth*10000)

ggplot()+
  geom_point(grow_hem, mapping = aes(y = growth_ha, x = year.x), col = 'cornflowerblue')+
  geom_line(grow_hem, mapping = aes(y = growth_ha, x = year.x), col = 'cornflowerblue')+
  geom_hline(yintercept = 0)+
  theme_minimal()+
  ylab('KgC/ha')+
  ggtitle('Hemlocks')+
  xlab('Year')+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14),
        title = element_text(size=14,face="bold"))

# segmented linear regression
out.lm<-lm(growth_ha~year.x,data=grow_hem)
o<-segmented(out.lm)
summary(o)
#plot original data
plot(grow_hem$year.x, grow_hem$growth_ha, pch=16, col='steelblue')
#add segmented regression model
plot(o, add=T)

#-----------------------------------#
#-----------Red Maples---------------
#-----------------------------------#
# subset to only include first 3 plots and red oaks
grow_rm <- growth %>%
  filter(species == 'rm')%>%
  filter(pl_col <= 3)

grow_rm <- grow_rm %>% 
  group_by(year.x) %>%
  summarise(growth = sum(kgc_gr, na.rm = TRUE)/(10^2 * pi * 22),
            growth_ha = growth*10000)

ggplot()+
  geom_point(grow_rm, mapping = aes(y = growth_ha, x = year.x), col = 'orange3')+
  geom_line(grow_rm, mapping = aes(y = growth_ha, x = year.x), col = 'orange3')+
  geom_hline(yintercept = 0)+
  theme_minimal()+
  ylab('KgC/ha')+
  ggtitle('Red Maples')+
  xlab('Year')+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14),
        title = element_text(size=14,face="bold"))


# segmented linear regression
out.lm<-lm(growth_ha~year.x,data=grow_rm)
o<-segmented(out.lm)
summary(o)

#plot original data
plot(grow_rm$year.x, grow_rm$growth_ha, pch=16, col='steelblue')
#add segmented regression model
plot(o, add=T)
#-----------------------------------#
#-----------plot panel---------------
#-----------------------------------#
p1 <- ggplot()+
  geom_point(grow_an, mapping = aes(y = growth_ha, x = year.x))+
  geom_line(grow_an, mapping = aes(y = growth_ha, x = year.x))+
  theme_minimal()+
  ylab('KgC/ha')+
  ggtitle('All Trees')+
  xlab('Year')+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14),
        title = element_text(size=14,face="bold"))

p2 <- ggplot()+
  geom_point(grow_ro, mapping = aes(y = growth_ha, x = year.x), col = 'indianred')+
  geom_line(grow_ro, mapping = aes(y = growth_ha, x = year.x), col = 'indianred')+
  theme_minimal()+
  ylab('KgC/ha')+
  ggtitle('Red Oaks')+
  xlab('Year')+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14),
        title = element_text(size=14,face="bold"))

p3 <- ggplot()+
  geom_point(grow_rm, mapping = aes(y = growth_ha, x = year.x), col = 'orange3')+
  geom_line(grow_rm, mapping = aes(y = growth_ha, x = year.x), col = 'orange3')+
  theme_minimal()+
  ylab('KgC/ha')+
  ggtitle('Red Maples')+
  xlab('Year')+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14),
        title = element_text(size=14,face="bold"))

p4 <- ggplot()+
  geom_point(grow_hem, mapping = aes(y = growth_ha, x = year.x), col = 'cornflowerblue')+
  geom_line(grow_hem, mapping = aes(y = growth_ha, x = year.x), col = 'cornflowerblue')+
  theme_minimal()+
  ylab('KgC/ha')+
  ggtitle('Hemlocks')+
  xlab('Year')+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14),
        title = element_text(size=14,face="bold"))


grid.arrange(p1, p2, p3, p4, nrow = 2)




#-----------------------------------#
#-----------------------------------#
#-----Make Grid in units of gC/m2----
#-----------------------------------#
#-----------------------------------#
#-----------------------------------#
#-----------plot panel---------------
#-----------------------------------#
p1 <- ggplot()+
  geom_point(grow_an, mapping = aes(y = growth*1000, x = year.x))+
  geom_line(grow_an, mapping = aes(y = growth*1000, x = year.x))+
  theme_minimal()+
  ylab('gC/m2')+
  ggtitle('All Trees')+
  xlab('Year')+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14),
        title = element_text(size=14,face="bold"))

p2 <- ggplot()+
  geom_point(grow_ro, mapping = aes(y = growth*1000, x = year.x), col = 'indianred')+
  geom_line(grow_ro, mapping = aes(y = growth*1000, x = year.x), col = 'indianred')+
  theme_minimal()+
  ylab('gC/m2')+
  ggtitle('Red Oaks')+
  xlab('Year')+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14),
        title = element_text(size=14,face="bold"))

p3 <- ggplot()+
  geom_point(grow_rm, mapping = aes(y = growth*1000, x = year.x), col = 'orange3')+
  geom_line(grow_rm, mapping = aes(y = growth*1000, x = year.x), col = 'orange3')+
  theme_minimal()+
  ylab('gC/m2')+
  ggtitle('Red Maples')+
  xlab('Year')+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14),
        title = element_text(size=14,face="bold"))

p4 <- ggplot()+
  geom_point(grow_hem, mapping = aes(y = growth*1000, x = year.x), col = 'cornflowerblue')+
  geom_line(grow_hem, mapping = aes(y = growth*1000, x = year.x), col = 'cornflowerblue')+
  theme_minimal()+
  ylab('gC/m2')+
  ggtitle('Hemlocks')+
  xlab('Year')+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14),
        title = element_text(size=14,face="bold"))


grid.arrange(p1, p2, p3, p4, nrow = 2)



# look at birches quick
#-----------------------------------#
#-----------Yellow Birch---------------
#-----------------------------------#
# subset to only include first 3 plots and red oaks
grow_yb <- growth %>%
  filter(species == 'yb')%>%
  filter(pl_col <= 3)

grow_yb <- grow_yb %>% 
  group_by(year.x) %>%
  summarise(growth = sum(kgc_gr, na.rm = TRUE)/(10^2 * pi * 14),
            growth_ha = growth*0.0001)

ggplot()+
  geom_point(grow_yb, mapping = aes(y = growth_ha, x = year.x), col = 'orange3')+
  geom_line(grow_yb, mapping = aes(y = growth_ha, x = year.x), col = 'orange3')+
  geom_hline(yintercept = 0)+
  theme_minimal()+
  ylab('KgC/ha')+
  ggtitle('Yellow Birch')+
  xlab('Year')+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14),
        title = element_text(size=14,face="bold"))

#-----------------------------------#
#-----------Black Birch--------------
#-----------------------------------#
# subset to only include first 3 plots and red oaks
grow_bb <- growth %>%
  filter(species == 'bb')%>%
  filter(pl_col <= 3)

grow_bb <- grow_bb %>% 
  group_by(year.x) %>%
  summarise(growth = sum(kgc_gr, na.rm = TRUE)/(10^2 * pi * 7),
            growth_ha = growth*0.0001)

ggplot()+
  geom_point(grow_bb, mapping = aes(y = growth_ha, x = year.x), col = 'orange3')+
  geom_line(grow_bb, mapping = aes(y = growth_ha, x = year.x), col = 'orange3')+
  geom_hline(yintercept = 0)+
  theme_minimal()+
  ylab('KgC/ha')+
  ggtitle('Black Birch')+
  xlab('Year')+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14),
        title = element_text(size=14,face="bold"))

