library(tidyverse)
library(cowplot)
library(ggimage)

songs = read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-12-24/christmas_songs.csv')
# write_csv(songs, 'data/2019-52_xmas_songs.csv')

d = songs %>% 
    filter(year >= 2000) %>%
    count(song, year) %>%
    mutate(song = str_to_title(song)) %>% 
    group_by(song) %>%
    mutate(n = n_distinct(year)) %>% 
    arrange(desc(year), desc(n))

num = length(unique(d$song))

d2 = tibble(
    song = unique(d$song), 
    image = rep(c('plots/christmas-ball-red.png', 'plots/christmas-ball-green.png'), length.out = num)
    )

d3 = left_join(d, d2) %>% 
    ungroup %>% 
    mutate(song = factor(song, levels = rev(d2$song)))

labs = tibble(
    x = c(2011.8, 2001),
    y = c(11, 29),
    label = c('9 Christmas songs made\nthe Top 100 in 2011.', 
              "Mariah Carey's song made the\nTop 100 8 times from 2000-2017\nand topped the list in Dec 2019\n(not shown).")
    )

ggplot(d3, aes(year, song)) +
    geom_image(aes(image = image), size = .03) +
    geom_label(data = labs, aes(label = label, x = x, y = y), hjust = 0, size = 4.7, family = 'Roboto', label.size = 0) +
    geom_curve(x = 2001.5, xend = 2000.5, y = 31, yend = 32, arrow = arrow(length = unit(.2, 'cm'))) +
    geom_curve(x = 2011.5, xend = 2011, y = 11, yend = 13, arrow = arrow(length = unit(.2, 'cm')), curvature = -.5) +
    labs(x = '', y = '', 
         title = 'Billboard Top 100 • Christmas Songs', 
         subtitle = 'Ornaments indicate that a song made the Top 100 that year.',
         caption = 'Data by Billboard Top 100\n#TidyTuesday • @watzoever') +
    theme_minimal_vgrid(18) +
    theme(
        plot.margin = unit(c(2, 3, 2, 2), 'cm'),
        plot.title = element_text(family = 'Arima Madurai', face = 'bold', size = 26, margin = margin(b = 15)),
        plot.subtitle = element_text(margin = margin(b = 30)),
        text = element_text(family = 'Roboto'),
        axis.ticks = element_blank(), axis.line = element_blank()
        )

ggsave('plots/2019-52_xmas_songs.png', width = 15, height = 15)


