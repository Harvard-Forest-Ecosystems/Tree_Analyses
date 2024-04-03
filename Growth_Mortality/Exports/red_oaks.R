# Red oak script
# use archive data to look at growth rates of red oak throughout the years
library(tidyverse)
library(here)
library(dplyr)

dat <- read_csv("~/Documents/Harvard_Forest/Dendrometer/fulldata.csv")
lai <- read_csv("~/Documents/Harvard_Forest/Dendrometer/lai_dat.csv")
alltreedat <- read_csv("~/Documents/Harvard_Forest/Dendrometer/fulldata_trees.csv")
treedat <- filter(alltreedat, tree_type == 'live')

# plot lai change over time
lai %>% 
  ggplot()+
  geom_point(aes(date, lai, color = plot))+
  geom_line(aes(date, lai, color = plot))

# plot change in summary stats over time
# agwi
ggplot()+
  geom_point(aes(dat$year, dat$agwi))

# agwb
ggplot()+
  geom_point(aes(dat$year, dat$agwb))


# calculate growth rates by species for each year
possible_years <-unique(treedat$year)

gr_dat_sp <- NULL

for (y in possible_years){
  year_dat <- filter(treedat, year == y)
  spring_doy <- min(year_dat$doy)
  winter_doy <- max(year_dat$doy)
  spring <- filter(year_dat, doy==spring_doy)
  winter <- filter(year_dat, doy==winter_doy)
  
  growth_rates <- inner_join(spring, winter, by = c("plottag"))
  time <- (growth_rates$doy.y-growth_rates$doy.x)/365
  # assign dend at time 1 (size1) and time 2 (size2)
  size2 <- growth_rates$dbh.y
  size1 <- growth_rates$dbh.x
  
  # calculate growth rates: 
  growth_rates$annual_increment <- (size2 - size1)/time
  growth_rates$relative_gr      <- (log(size2) - log(size1))/time
  # group by species and year
  growth_sp <- growth_rates %>%
    group_by(species.x, year.x) %>%
    summarize(an_incr = mean(annual_increment, na.rm =TRUE),
              rel_gr = mean(relative_gr, na.rm = TRUE))
  
  gr_dat_sp <- rbind(gr_dat_sp, growth_sp)
  
}

# plot growth rates for each year colored by species
gr_dat_sp %>%
  ggplot()+
  geom_point(aes(year.x, an_incr, col = species.x))+
  geom_line(aes(year.x, an_incr, col = species.x))

gr_dat_sp %>%
  ggplot()+
  geom_point(aes(year.x, an_incr, col = species.x))+
  geom_line(aes(year.x, an_incr, col = species.x))+
  ylim(0,1)

gr_dat_sp %>%
  ggplot()+
  geom_point(aes(year.x, rel_gr, col = species.x))+
  geom_line(aes(year.x, rel_gr, col = species.x))

gr_dat_sp %>%
  ggplot()+
  geom_point(aes(year.x, rel_gr, col = species.x))+
  geom_line(aes(year.x, rel_gr, col = species.x))+
  ylim(0,.1)


# plot just red oak
redoak <- filter(gr_dat_sp, species.x=="ro")
redoak %>%
  ggplot()+
  geom_point(aes(year.x, an_incr, col = species.x))+
  geom_line(aes(year.x, an_incr, col = species.x))

redoak %>%
  ggplot()+
  geom_point(aes(year.x, rel_gr, col = species.x))+
  geom_line(aes(year.x, rel_gr, col = species.x))


# take out chestnuts for a bit
gr_filter <- filter(gr_dat_sp, species.x != "chestnut")

gr_filter %>%
  ggplot()+
  geom_point(aes(year.x, an_incr, col = species.x))+
  geom_line(aes(year.x, an_incr, col = species.x))

gr_filter %>%
  ggplot()+
  geom_point(aes(year.x, rel_gr, col = species.x))+
  geom_line(aes(year.x, rel_gr, col = species.x))

# limit to only species present in plot as of 2021
sp_2021 <- filter(gr_filter, year.x == 2021)
possible_sp <- unique(sp_2021$species.x)

current_sp_gr <- filter(gr_filter, species.x %in% possible_sp)

# plot
current_sp_gr %>%
  ggplot()+
  geom_point(aes(year.x, rel_gr, col = species.x))+
  geom_line(aes(year.x, rel_gr, col = species.x))

current_sp_gr %>%
  ggplot()+
  geom_point(aes(year.x, an_incr, col = species.x))+
  geom_line(aes(year.x, an_incr, col = species.x))




# calculate growth rates by species for each season
possible_years <-unique(treedat$year)

for (y in possible_years){
  year_dat <- filter(treedat, year = y),
  spring <- 
    summer <- 
    fall <- 
    winter <- 
    
}
