library(tidyverse)
library(ggtext)

d = read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-11/hotels.csv')
# write_csv(d, 'data/2020-07_hotels.csv')

d2 = d %>% 
    rename(yr = arrival_date_year, mo = arrival_date_month) %>% 
    mutate(mo = str_sub(mo, end = 3), mo = factor(mo, levels = month.abb)) %>% 
    group_by(hotel, yr, mo) %>% 
    summarise(M_ADR = mean(adr)) %>% 
    pivot_wider(names_from = yr, values_from = M_ADR)

ggplot(d2, aes(mo)) +
    facet_wrap(~ hotel, strip.position = 'bottom') +
    geom_bar(aes(y = `2017`, fill = '2017'), stat = 'identity', width = .2) +
    geom_bar(aes(y = `2016`, fill = '2016'), stat = 'identity', width = .4) +
    geom_bar(aes(y = `2015`, fill = '2015'), stat = 'identity', width = .6) +
    scale_fill_manual('', values = c('grey50', 'grey65', 'grey80')) +
    scale_y_continuous(expand = expansion(c(0, .05))) +
    expand_limits(y = 0) +
    labs(x = '', y = 'Average Daily Rate',
     title = 'Hotel Bookings', 
     subtitle = '**Average daily rate for two hotels in Portugal from July 2015 to August 2017.**<br/>The average daily rate increased for both hotels from year to year, but the <br/>resort hotel in the Algarve has more pronounced seasonal fluctuations <br/>than the city hotel in Lisbon.', 
     caption = 'Data by Antonio, de Almeida, & Nunes (2019) *Data in Brief* **22**:41-49<br/> #TidyTuesday • @watzoever') +
    cowplot::theme_minimal_hgrid() +
    theme(text = element_text(family = 'Source Sans Pro'),
          axis.line.x = element_line(color = 'grey80'), axis.ticks = element_blank(),
          axis.title.y = element_text(size = 12, face = 'bold', margin = margin(r = 10)),
          legend.position = 'bottom',
          strip.placement = 'outside', strip.text = element_text(size = 12, face = 'bold'),
          plot.title = element_text(size = 24),
          plot.subtitle = element_markdown(lineheight = 1.15), 
          plot.caption = element_markdown(lineheight = 1.15, margin = margin(t = -12)),
          plot.margin = unit(c(2, 2, 2, 2), 'cm'))

ggsave('plots/2020-07_hotels.png', width = 7, height = 5, scale = 1.7)
