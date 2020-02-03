library(tidyverse)

d = read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-28/sf_trees.csv')
# write_csv(d, 'data/2020-05_sf_trees.csv')

d2 = d %>% 
    mutate(year = lubridate::year(date)) %>% 
    filter(year >= 1970) %>% 
    separate(species, c('sp_latin', 'sp_common'), sep = ' :: ')

top = d2 %>% 
    filter(!is.na(sp_common)) %>% 
    count(sp_common, sort = T) %>% 
    top_n(3) %>% 
    mutate(idx = row_number()) %>% 
    select(-n)

d3 = d2 %>% 
    inner_join(top) %>% 
    count(year, idx, sp_common) %>% 
    mutate(sp_common = reorder(sp_common, idx),
        year2 = year + (idx - 2)*.3)

ggplot(d3, aes(year2, n, fill = sp_common)) +
    geom_segment(aes(xend = year2, size = sqrt(idx)*4), alpha = .8, yend = 0, col = 'chocolate3') +
    geom_point(aes(size = idx), shape = 24, col = 'black') +
    ggforce::geom_mark_ellipse(aes(filter = idx == 1 & year == 1995, label = sp_common), fill = NA, label.fontsize = 10, 
                               description = 'The most commonly planted species overall') +
    ggforce::geom_mark_ellipse(aes(filter = idx == 2 & year == 1985, label = sp_common), fill = NA, label.fontsize = 10, 
                               description = 'In 2nd place overall and vying for a name change') +
    ggforce::geom_mark_ellipse(aes(filter = idx == 3 & year == 2008, label = sp_common), fill = NA, label.fontsize = 10, 
                               description = 'With 489 trees, the most planted trees in any year', label.buffer = unit(.2, 'cm')) +
    scale_size_continuous(range = c(4, .5), guide = F) +
    scale_y_continuous(expand = expansion(c(0, .08))) +
    scale_fill_brewer(palette = 'Greens', direction = -1, guide = F) +
    labs(x = '', y = '',
         title = '50 Years of Tree Planting in San Francisco', 
         subtitle = 'For the three most commonly planted tree species.\nTrees of unidentified species were excluded.', 
         caption = 'Data by Data SF\n #TidyTuesday • @watzoever') +
    cowplot::theme_minimal_hgrid() +
    theme(text = element_text(family = 'Poppins Latin'),
          axis.line.x = element_line(color = 'chocolate4'),
          axis.text.y = element_text(size = 10, color = 'grey'),
          plot.margin = unit(c(2, 2, 2, 2), 'cm'))

ggsave('plots/2020-05_sf_trees.png', width = 7, height = 4, scale = 1.8)

