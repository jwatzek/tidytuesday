library(tidyverse)
library(cowplot)
library(patchwork)
library(ggtext)
library(osmdata)
library(sf)
library(ggpointdensity)

d = read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-12-03/tickets.csv')
# write_csv(d, 'data/2019-49_tickets.csv')
beep()

coord = getbb('Philadelphia, PA')

streets1 = opq(coord) %>% 
    add_osm_feature('highway', c('motorway', 'primary', 'secondary', 'tertiary')) %>% 
    osmdata_sf()
beep()

streets2 = opq(coord) %>% 
    add_osm_feature('highway', c('residential', 'living_street', 'unclassified', 'service', 'foodway')) %>% 
    osmdata_sf()
beep()

d2 = filter(d, between(lon, coord[1, 1], coord[1, 2]), between(lat, coord[2, 1], coord[2, 2]))
d3 = filter(d2, str_detect(violation_desc, 'METER EXPIRED'))

viridis::viridis(10)[c(3, 10)]
colorspace::lighten('#3E4A89', .1)
viridis::inferno(10)[c(3, 8)]

p = ggplot(sample_n(d2, 1e4)) +
    geom_sf(data = streets2$osm_lines, col = 'grey40', size = .1) +
    geom_sf(data = streets1$osm_lines, col = 'grey40', size = .4) +
    geom_pointdensity(aes(lon, lat), alpha = .8) +
    geom_sf(data = streets2$osm_lines, col = alpha('grey40', .2), size = .1) +
    geom_sf(data = streets1$osm_lines, col = alpha('grey40', .2), size = .4) +
    scale_color_viridis_c(option = 'viridis') +
    coord_sf(xlim = coord[1,], ylim = coord[2,], expand = F) +
    labs(
        title = 'PARKING VIOLATION',
        subtitle = '<b style="color:#FDFDFD;">This vehicle is illegally parked in the City of Philadelphia.</b><br/><br/>
                    Parking tickets are issued <b style="color:#FDE725;">more frequently</b> near the city center<br/>
                    and <b style="color:#505A98;">less frequently</b> elsewhere.',
        caption = 'Data by Open Data Philly\n#TidyTuesday • @watzoever') +
    theme_map() +
    theme(legend.position = 'none',
          text = element_text(family = 'Poppins Latin', color = 'grey60', size = 18),
          plot.background = element_rect(fill = 'grey20', color = NA),
          plot.title = element_text(family = 'Impact', color = '#fdfdfd', size = 42),
          plot.subtitle = element_markdown(margin = margin(b = 20)),
          plot.margin = unit(rep(2, 4), 'cm'))

system.time(ggsave('plots/2019-49_tickets.png', p, width = 10, height = 12, bg = 'grey20'))
beep()

p2 = ggplot(d3) +
    geom_sf(data = streets2$osm_lines, col = 'black', size = .1) +
    geom_sf(data = streets1$osm_lines, col = 'black', size = .4) +
    geom_pointdensity(aes(lon, lat), size = 3, alpha = .8) +
    geom_sf(data = streets2$osm_lines, col = alpha('black', .2), size = .1) +
    geom_sf(data = streets1$osm_lines, col = alpha('black', .2), size = .4) +
    scale_color_viridis_c(option = 'inferno') +
    coord_sf(xlim = coord[1,], ylim = coord[2,], expand = F) +
    labs(
        title = 'PARKING VIOLATION',
        subtitle = '**This vehicle is illegally parked in the City of Philadelphia.**<br/><br/>
                    Parking tickets for expired meters are issued <b style="color:#F7D03C;">more frequently</b> near<br/>
                    the city center and <b style="color:#4B0C6B;">less frequently</b> elsewhere.',
        caption = 'Data by Open Data Philly\n#TidyTuesday • @watzoever') +
    theme_map() +
    theme(legend.position = 'none',
          text = element_text(family = 'Poppins Latin', size = 18),
          plot.title = element_text(family = 'Impact', size = 42),
          plot.subtitle = element_markdown(margin = margin(b = 20)),
          plot.margin = unit(rep(2, 4), 'cm'))

system.time(ggsave('plots/2019-49_tickets_meters.png', p2, width = 10, height = 12))
beep()

p3 = p + p2
system.time(ggsave('plots/2019-49_tickets_both.png', p3, width = 20, height = 12))
beep()
