

# download neon tree data
zipsByProduct('DP1.10098.001', site = 'HARV')
stackByTable("filesToStack10098")

# load in data
neon_tree <- read_csv('neon_trees.csv')
neon_species <- read_csv('neon_tree_species.csv')

# try to merge
neon_all <- left_join(neon_tree,neon_species, by = 'individualID',
                      relationship = 'many-to-many')

# suset to only useful columns
neon_all <- neon_all %>%
  subset(select = c('date.x','individualID','plotID.x',
                    'stemDiameter','taxonID','scientificName','plantStatus'))%>%
  rename('date' = 'date.x')

# calculate kgc from stem diameter and species




# calculate growth

# list of years
neon_all <- neon_all %>% mutate(year = substr(date,1,4),
                                status_simple = substr(plantStatus,1,4)) 

years <- unique(neon_all$year)

# create empty dataframe
growth <- data.frame()

# grab code from ems plot dendro growth calc
# loop through all years
for(year in 1:(length(years)-1)){
  # set year 1 and year 2 values
  current <- years[year]
  future <- years[year+1]
  print(current)
  print(future)
  
  # filter dataset to only include only alive trees from both censuses
  old <- filter(neon_all, year ==  current & status_simple == "Live")
  new <- filter(neon_all, year == future & status_simple == "Live")
  
  # filter each observation to remove duplicates
  old <- old %>% distinct(individualID, .keep_all = TRUE)
  new <- new %>% distinct(individualID, .keep_all = TRUE)
  
  # combine the two datasets
  full <- left_join(old, new, by = "individualID")
  
  # calculate growth
  temp <- full %>%
    mutate(time = date.y-date.x,
           years_past = as.numeric(substr(time,1,3))/365,
           dbh_gr = (stemDiameter.y-stemDiameter.x)/years_past)
           #basal_gr = (basal_area.y-basal_area.x)/basal_area.x,
           #dbh_inc = (dbh.y-dbh.x)/dbh.x,
           #kgc_inc = (kgc.y-kgc.x)/kgc.x,
           #basal_inc = basal_area.y-basal_area.x,
           #kgc_gr = kgc.y-kgc.x,
           #pl_row = str_sub(plot.x,1,1),
           #pl_col = as.numeric(str_sub(plot.x,2,2)))
  
  # add calculated growths to dataframe
  growth <- rbind(growth,temp)
} 



# plot mean growth over time
growth_annual <- growth %>%
  group_by(year.x)%>%
  summarize(dbh_mean = mean(dbh_gr, na.rm = TRUE))

ggplot()+
  geom_point(growth_annual, mapping = aes(x = year.x, y=dbh_mean))+
  geom_line(growth_annual, mapping = aes(y=dbh_mean,x = year.x))+
  theme_minimal()

summary(growth$dbh_gr)

# subset out unreasonable data points for now

growth_subset <- filter(growth, dbh_gr <=1.3 & dbh_gr >-3)

summary(growth_subset$dbh_gr)

growth_annual <- growth_subset %>%
  group_by(year.x)%>%
  summarize(dbh_mean = mean(dbh_gr, na.rm = TRUE))

ggplot()+
  geom_point(growth_annual, mapping = aes(x = year.x, y=dbh_mean))+
  geom_line(growth_annual, mapping = aes(y=dbh_mean,x = year.x))+
  theme_minimal()

grow_ems <- read_csv('GitHub/Dendrometer/Data/ems_growth_data.csv')

grow_ems_sub <- filter(grow_ems, dbh_gr < -1)

summary(grow_ems)

# max growth from all years of ems plots is 1.18, so I am going to remove neon trees that are greater than maybe 1.3?
# ask jackie, neon data is really messy lol