library(tidyverse)
library(cowplot)
library(waffle)

d = read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-11-19/nz_bird.csv')
# write_csv(d, 'data/2019-47_nz_bird.csv')

## assign voter ID for instant runoff
# mutate(id = (row_number() - 1) %/% 5, vote_rank = ...)

d2 = d %>% 
    mutate(vote_rank = str_sub(vote_rank, -1)) %>% 
    filter(!is.na(bird_breed)) %>% 
    # tally votes
    count(bird_breed, vote_rank, name = 'votes') %>% 
    group_by(bird_breed) %>% 
    mutate(all_votes = sum(votes)) %>% 
    ungroup %>% 
    # top5 birds
    filter(dense_rank(-all_votes) <= 5) %>% 
    mutate(bird_breed = reorder(bird_breed, -votes))

# waffle chart
p = ggplot(d2, aes(values = votes/25, x = votes, fill = vote_rank)) +
    facet_wrap(~ bird_breed, ncol = 1, strip.position = 'left') +
    geom_waffle(n_rows = 10, col = 'grey20', size = .15) +
    scale_x_continuous(breaks = seq(0, 1000, 500)/25, labels = seq(0, 1000, 500)*10, expand = expand_scale(mult = c(.1, .05))) +
    scale_y_continuous(expand = expand_scale(mult = .2)) +
    scale_fill_viridis_d('Vote Rank', option = 'plasma') +
    coord_equal() +
    labs(xlab = 'Votes', ylab = '', 
         title = 'New Zealand • Bird of the Year 2019', 
         subtitle = 'Top 5 • 1 square = 25 votes', 
         caption = 'Data by New Zealand Forest and Bird Organization\n#TidyTuesday • @watzoever') +
    theme_map(font_family = 'Roboto') +
    theme(
        text = element_text(color = 'grey85'),
        plot.margin = unit(c(1, 1, 2, 1), 'cm'),
        strip.text.y = element_text(angle = 180, hjust = 0, margin = margin(r = 10)),
        panel.spacing.y = unit(0, 'line'),
        panel.grid.major.x = element_line(unit(.5, 'line'), color = 'grey60'),
        axis.text = element_text(size = 10, color = 'grey60'),
        axis.text.y = element_blank(),
        plot.caption = element_text(size = 10, color = 'grey60', hjust = 0, margin = margin(t = 20)),
        legend.key.size = unit(1, 'line'), 
        legend.title = element_text(size = 12), legend.text = element_text(size = 10),
        plot.background = element_rect(fill = 'grey20', color = NA)
    )

p2 = ggdraw(p) + theme(plot.background = element_rect(fill = 'grey20', color = NA))
ggsave('plots/2019-47_nz_bird.png', p2, width = 8, height = 7)

