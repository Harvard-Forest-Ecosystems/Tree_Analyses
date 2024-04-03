library(tidyverse)
library(here)
library(ggplot2)


#----------------------------------------------------------#
#----------------------------------------------------------#
#----Compare Growth and Mortality with more accurate forestgeo sample------
#----------------------------------------------------------#
#----------------------------------------------------------#
# merge 2019 and 2014 data
forestgeo <- left_join(forest_ems14, forest_ems19, by = c('tree.id','stem.id'), relationship = "many-to-many")

# calculate mortality rate by quadrat
mortality <- forestgeo %>%
  group_by(plot.x) %>%
  summarize(mort_rate = sum(df.status.y == 'stem dead', na.rm = TRUE)/n(),
            quad_x = round(mean(gx_utm.x, na.rm = TRUE), 0),
            quad_y = round(mean(gy_utm.x, na.rm = TRUE), 0)) %>%
  mutate(pl_row = str_sub(plot.x,1,1),
         pl_col = as.numeric(str_sub(plot.x,2,2)))


# according to the forestgeo data, only 1 tree died in those five years
# seems dubious

# let's look at growth!
# Calculate growth rates for EMS plots using ForestGEO data
growth_geo <- forestgeo %>%
  group_by(plot.x) %>%
  # calculate average annual growth rate for each quadrat
  # sum the difference between the second and first census dbh
  # divide by 5 (number of years between 2014-2019)
  # divide by number of individuals in the quadrat
  summarize(growth_rate = sum((dbh.y-dbh.x), na.rm = TRUE)/5/n(),
            pop = n()) %>%
  mutate(pl_row = str_sub(plot.x,1,1),
         pl_col = as.numeric(str_sub(plot.x,2,2)))
# plot
ggplot() + 
  geom_tile(growth_geo, mapping = aes(x=pl_row, y=pl_col, fill= growth_rate))

# now do the same but for red oak and hemlock
hem <- filter(forestgeo, sp.x == 'tsugca')
hem_grow <- hem %>%
  group_by(plot.x) %>%
  # calculate average annual growth rate for each quadrat
  # sum the difference between the second and first census dbh
  # divide by 5 (number of years between 2014-2019)
  # divide by number of individuals in the quadrat
  summarize(growth_rate = sum((dbh.y-dbh.x), na.rm = TRUE)/5/n(),
            pop = n()) %>%
  mutate(pl_row = str_sub(plot.x,1,1),
         pl_col = as.numeric(str_sub(plot.x,2,2)))

ro <- filter(forestgeo, sp.x == 'querru')
ro_grow <- ro %>%
  group_by(plot.x) %>%
  # calculate average annual growth rate for each quadrat
  # sum the difference between the second and first census dbh
  # divide by 5 (number of years between 2014-2019)
  # divide by number of individuals in the quadrat
  summarize(growth_rate = sum((dbh.y-dbh.x), na.rm = TRUE)/5/n(),
            pop = n()) %>%
  mutate(pl_row = str_sub(plot.x,1,1),
         pl_col = as.numeric(str_sub(plot.x,2,2)))

hem_grow %>% ggplot() + 
  geom_tile(growth_geo, mapping = aes(x=pl_row, y=pl_col, fill= growth_rate))+
  scale_fill_gradient(low="chartreuse", high = 'black')
ro_grow %>% ggplot() + 
  geom_tile(growth_geo, mapping = aes(x=pl_row, y=pl_col, fill= growth_rate))+
  scale_fill_gradient(low="firebrick1", high = 'black')


