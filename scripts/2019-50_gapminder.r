library(tidyverse)
library(cowplot)
library(ggsci)
library(gganimate)
library(ggrepel)
library(beepr)

theme_set(theme_cowplot())
theme_update(text = element_text(family = 'Karla'))

# TILE MAP
tiles = read_csv('data/2019-50_worldtilegrid.csv') %>% 
    filter(region != 'Antarctica')

map = ggplot(tiles, aes(x = x, y = y, fill = region)) +
    geom_tile(color = "white") +
    scale_y_reverse() +
    scale_fill_simpsons(guide = F) +
    coord_equal() +
    theme_map()

# ggsave('plots/2019-50_map.png', map, width = .8, height = .8)

# GAPMINDER DATA
load('data/2019-50_gapminder.rda')

d = gapminder %>% 
    as_tibble %>% 
    filter(!is.na(fertility), !is.na(life_expectancy), !is.na(population))

# animated
p = ggplot(d, aes(fertility, life_expectancy, fill = continent, size = population, group = country)) +
    geom_point(shape = 21, alpha = .8) +
    scale_fill_simpsons(guide = F) +
    scale_size(range = c(4, 30), guide = F) +
    scale_x_continuous(breaks = seq(2, 8, 2)) +
    scale_y_continuous(breaks = seq(20, 80, 20)) +
    labs(x = '\nFertility Rate (Births per Woman)', y = 'Life Expectancy at Birth\n',
         title = 'Gapminder | Year {frame_time}', 
         subtitle = 'The rise and fall of life expectancies and fertility rates from 1960 to 2015.\n',
         caption = 'Data by Gapminder\n#TidyTuesday | @watzoever') +
    transition_time(year) +
    ease_aes('linear') +
    theme(legend.position = 'bottom') +
    guides(fill = guide_legend(override.aes = list(size = 4)))

anim_save('plots/2019-50_gapminder.gif', p, width = 480, height = 480)
beep()

# static
flag = c('Sierra Leone', 'Mauritius', 'Botswana', 'Tunisia', 'Angola', 'Serbia',
         'Serbia', 'Germany', 'Netherlands', 'Iceland',
         'United States', 'Haiti', 'Bolivia', 'Guatemala',
         'Australia', 'New Zealand', 'Vanuatu', 'Samoa', 'Papua New Guinea',
         'China', 'Turkey', 'Cambodia', 'Oman')

p2 = ggplot(filter(d2, country %in% flag), aes(fertility, life_expectancy, fill = continent, size = population, alpha = year)) +
    facet_wrap(~ continent) +
    geom_path(aes(group = country), size = .5) +
    geom_point(shape = 21, data = filter(d2, year %in% seq(1960, 2015, 5))) +
    geom_text_repel(aes(label = country), data = filter(d2, year == 2015), family = 'Karla', force = 2.5, size = 3, box.padding = 3) +
    scale_fill_simpsons(guide = F) +
    scale_alpha(guide = F) +
    scale_size(range = c(2, 15), guide = F) +
    scale_x_continuous(breaks = seq(2, 8, 2)) +
    expand_limits(x = c(1, 8), y = c(25, 90)) +
    panel_border() +
    labs(x = '\nFertility Rate (Births per Woman)', y = 'Life Expectancy at Birth\n',
         title = 'Gapminder', 
         subtitle = 'The rise and fall of selected countries\' life expectancies and fertility rates from 1960 to 2015.\n',
         caption = 'Data by Gapminder\n#TidyTuesday | @watzoever')

ggdraw(p2) +
    draw_plot(map, .67, .12, .35, .35)

ggsave('plots/2019-50_gapminder_static.png', width = 7, height = 5, scale = 1.5)
