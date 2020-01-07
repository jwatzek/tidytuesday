library(tidyverse)
library(lubridate)

d = read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-07/temperature.csv')
# write_csv(d, 'data/2020-02_aus_fires_strips.csv')

d2 = d %>% 
    mutate(city_name = str_to_title(city_name), year = year(date)) %>% 
    filter(temp_type == 'max', !is.na(temperature), year < 2019) %>% 
    group_by(city_name, year) %>% 
    summarise(mean_max = mean(temperature, na.rm = T)) %>% 
    mutate(
        mean_max2 = scale(mean_max),
        mean_max2 = ifelse(mean_max2 < -2.7, -2.7, mean_max2)
        )

ggplot(d2, aes(year, 1, fill = mean_max2)) +
    facet_wrap(~ city_name, ncol = 1) +
    geom_tile() +
    scale_fill_distiller(limits = c(-2.7, 2.7), palette = 'RdBu', guide = F) +
    labs(title = 'Australian cities are getting hotter', 
         subtitle = 'Mean yearly maximum temperature from 1910 to 2018.\nColor range standardized by city.',
         caption = "Data by Australia's Bureau of Meteorology\n#TidyTuesday • @watzoever") +
    theme_void() +
    scale_x_continuous(expand = expand_scale(0)) +
    theme(
        text = element_text(family = 'Poppins Latin'),
        plot.title = element_text(face = 'bold'),
        plot.subtitle = element_text(margin = margin(t = 5, b = 10)),
        plot.caption = element_text(margin = margin(t = 10)),
        strip.text = element_text(hjust = 1),
        plot.margin = unit(c(1, 1, 1, 1), 'cm')
        )

ggsave('plots/2020-02_aus_fires2.png', width = 7, height = 7)
