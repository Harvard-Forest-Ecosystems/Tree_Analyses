# look at how red oak growth rates change during each season
library(tidyverse)
library(here)
library(ggplot2)

# Load in data--------
treedat <- read_csv("~/Documents/Harvard_Forest/Dendrometer/fulldata_trees.csv")
coords <- read_csv("~/Documents/GitHub/Dendrometer/EMS_plots_utm.csv")

coords <- coords %>% mutate(plot = Plot)
coords <- subset(coords, select = c('E.1',"N.1", 'plot'))

# merge coords with tree data
fulldat <- left_join(treedat, coords, by = c("plot"))

# filter to only include redoaks
fulldat <- filter(fulldat, species == 'ro')

#--------------------------------------------------#
#--------------------------------------------------#


#--------------------------------------------------#
#--------------------------------------------------#
#------------Annual growth rates by plot-----------
#--------------------------------------------------#
#--------------------------------------------------#

# remove 1993 and then make list of years
temp <- filter(fulldat,year != '1993')
years <- unique(temp$year)

# Create dataframe to store growth rates
growth <- data.frame()

# loop through every year
for(year in 1:(length(years)-1)){
  # subset data to only include alive trees from 
  # one year and the following year
  current <- years[year]
  future <- years[year+1]
  old <- filter(fulldat, year ==  current & tree_type != "dead")
  new <- filter(fulldat, year == future & tree_type != "dead")
  
  # find the first census day from each year
  old_date <- min(old$doy, na.rm = TRUE)
  new_date <- min(new$doy, na.rm = TRUE)
  
  # filter to only include data from first census of the year
  # add a 15 day buffer to cover conditions where
  # census didn't occur all in one day
  old <- filter(old, doy <= old_date+15)
  new <- filter(new, doy <= new_date+15)
  
  # subset to only include vital columns
  old <- subset(old, select = c('plottag', 'dbh','year','plot'))
  new <- subset(new, select = c('plottag', 'dbh','year','plot'))
  
  # join the two years data frames by plottag
  full <- left_join(old, new, by = "plottag")
  
  # calculate growth rates for each quadrat
  temp <- full %>%
    # merge according to EMS plot and year
    group_by(year.x, plot.x) %>%
    # calculate growth rate by adding together all
    # of the differences between dbh measurements
    # and divide by the number of red oaks in a given plot
    summarize(growth_rate = sum(dbh.y-dbh.x)/n()) %>%
    # create columns containing the EMS plot letter
    # and the EMS plot number 
    # to create heat map
    mutate(pl_row = str_sub(plot.x,1,1),
           pl_col = as.numeric(str_sub(plot.x,2,2)))
  # combine all annual growth rates into one dataframe
  growth <- rbind(growth,temp)
}

# create a heatmap
ggplot() + 
  geom_tile(growth, mapping = aes(x=pl_row, y=pl_col, fill= growth_rate)) +
  facet_wrap(~year.x)


#--------------------------------------------------#
#--------------------------------------------------#
#---------Annual growth rates and weather-----------
#--------------------------------------------------#
#--------------------------------------------------#
weather <- read_csv('~/Documents/GitHub/Dendrometer/hf300-01-annual-m.csv')

growth$year <- growth$year.x

# merge with growth rate data
grow_clim <- left_join(growth, weather, by = 'year')

# Make plots of red oak growth rates and annual weather patterns
grow_clim %>%
  ggplot()+
  geom_point(aes(year.x, growth_rate))+
  geom_line(aes(year.x, growth_rate))+
  facet_wrap(~plot.x)

grow_clim %>%
  ggplot()+
  geom_point(aes(year.x, growth_rate, col = plot.x))+
  geom_line(aes(year.x, growth_rate, col = plot.x))
  geom_point(aes(year.x, prec))+
  geom_line(aes(year.x, prec))


#--------------------------------------------------#
#--------------------------------------------------#
#---------------Growth rates by season--------------
#--------------------------------------------------#
#--------------------------------------------------#
grow <- data.frame()
# loop through every year
for(year in 1:(length(years))){
  current <- years[year]
  old <- filter(fulldat, year ==  current & tree_type != "dead")
  # make a list of the census days for each year in order
  old_days <- lapply(unique(old$doy), sort)
  
  for(census in 1:(length(old_days)-1)){
    print(census)
    print('hi')
    print(length(old_days))
    #
    #future_cen <- census+1
    #if(old_days[census] - old_days[future_cen] > 30){
    #  old_date <- filter(old, doy == old_days[census])
    #  new_date <- filter(old, doy == old_days[future_cen])
    #  old_date <- subset(old_date, select = c('plottag', 'dbh','year','doy','plot'))
    #  new_date <- subset(new_date, select = c('plottag', 'dbh','year','doy', 'plot'))
    #  full <- left_join(old_date, new_date, by = "plottag")
    #  # calculate growth rates for each quadrat
    #  temp <- full %>%
    #    # merge according to EMS plot and year
    #    group_by(year.x, doy.x, plot.x) %>%
    #    # calculate growth rate by adding together all
    #    # of the differences between dbh measurements
    #    # and divide by the number of red oaks in a given plot
    #    summarize(growth_rate = sum(dbh.y-dbh.x)/n()/(doy.y-doy.x)) %>%
    #    # create columns containing the EMS plot letter
    #    # and the EMS plot number 
    #    # to create heat map
    #    mutate(pl_row = str_sub(plot.x,1,1),
    #           pl_col = as.numeric(str_sub(plot.x,2,2)))
    #  # combine all annual growth rates into one dataframe
    #  grow <- rbind(grow,temp)
    #}
    ##else{
    # # old_date <- filter(old, doy == old_days[census] | old_days[census+1])
    ##  new_date <- filter(old, doy == old_days[census+2])
    ##  census <- census+1
    ##}
  }
}



