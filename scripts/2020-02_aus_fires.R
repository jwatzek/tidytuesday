library(tidyverse)
library(cowplot) # for theme_map
library(ggrepel) # for geom_text_repel
library(glue) # for glue
library(raster)
library(maps) # for world.cities
library(rnaturalearth) # for ne_states Oz outline

# Daily Max Temp for 2020-01-05 from http://www.bom.gov.au/jsp/awap/temp/index.jsp
d = raster('data/2020-02_bom.grid') %>% 
    rasterToPoints() %>% 
    as_tibble() %>% 
    rename(long = x, lat = y, temp = X2020.02_bom) %>% 
    mutate(tempF = temp*9/5 + 32)

# write_csv(d, 'data/2020-02_aus_fires.csv')

cities = world.cities %>% filter(country.etc == 'Australia', pop > 5e5)

oz = ne_states(geounit = 'Australia')

extreme = d %>% 
    filter(temp %in% c(min(temp), max(temp))) %>% 
    group_by(temp) %>% 
    sample_n(1) %>% 
    mutate(long2 = long - 2, lat2 = lat + 1)

labs = tibble(
    x = c(131.3, 142.2),
    y = c(-39.7, -30.3),
    label = c(glue('Lowest: {round(min(d$temp), 1)}°C / {round(min(d$tempF), 1)}°F'), 
              glue('Highest: {round(max(d$temp), 1)}°C / {round(max(d$tempF), 1)}°F')))

ggplot(d, aes(long, lat)) +
    geom_raster(aes(fill = temp)) +
    geom_path(aes(group = group), data = oz, col = 'black', alpha = .5) +
    geom_point(data = cities, col = 'white') +
    geom_curve(aes(x = long2, xend = long, y = lat2, yend = lat), data = extreme, col = c('white', 'black'), arrow = arrow(length = unit(.2, 'cm'))) +
    geom_label(data = labs, aes(label = label, x = x, y = y), hjust = 0, size = 3, col = c('white', 'black'), family = 'Vollkorn', fill = NA, label.size = 0) +
    annotate('text', x = 113.3, y = -12, label = 'Australia is burning', family = 'Vollkorn', size = 6, fontface = 'bold', hjust = 0) +
    annotate('text', x = 113.3, y = -14, label = 'Daily maximum temperature\nfor Sunday, January 5, 2020', family = 'Vollkorn', size = 3.5, lineheight = .9, hjust = 0) +
    annotate('text', x = 113.3, y = -42.1, label = "Data by Australia's Bureau of Meteorology\n#TidyTuesday • @watzoever", family = 'Vollkorn', size = 3, hjust = 0, col = 'white') +
    geom_text_repel(aes(label = name), data = cities, force = 1.5, col = 'white', family = 'Vollkorn', size = 3) +
    scale_fill_viridis_c(option = 'inferno', guide = F) +
    coord_equal(expand = F) +
    xlim(112, 156.25) + ylim(-44.5, -10) +
    theme_map()

ggsave('plots/2020-02_aus_fires.png', width = 8, height = 6.4, bg = 'transparent')

