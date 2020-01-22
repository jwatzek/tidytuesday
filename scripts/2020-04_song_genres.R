library(tidyverse)

d = read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-21/spotify_songs.csv')
# write_csv(d, 'data/2020-04_song_genres.csv')

d2 = d %>% 
    rename(genre = playlist_genre, duration = duration_ms) %>% 
    select(genre, danceability, energy, loudness, speechiness, liveness, valence, tempo, duration) %>% 
    mutate_at(vars(-genre), scale) %>%
    group_by(genre) %>%
    summarise_at(vars(everything()), mean) %>%
    pivot_longer(-genre, names_to = 'feature', values_to = 'mean') %>%
    mutate(mean = mean + 1, feature2 = as.numeric(factor(feature)))

d3 = d2 %>% 
    bind_rows(mutate(d2, feature2 = feature2 + .5, mean = 0)) %>%
    arrange(feature) %>% 
    bind_rows(filter(d2, feature2 == 1) %>% mutate(feature2 = feature2 - .5, mean = 0))

labs = d3 %>%
    filter(feature2 %in% 1:8) %>% 
    count(feature, feature2) %>% 
    add_column(
        genre = c('latin', 'rock', 'edm', 'edm', 'edm', 'rap', 'edm', 'latin'),
        mean = .08,
        angle = c(68, 22, -22, -68, 68, 22, -22, -68),
        hjust = rep(0:1, each = 4))

ggplot(d3, aes(feature2, mean, group = genre)) +
    facet_wrap(~ genre) +
    geom_area(fill = '#1cd760') +
    geom_text(aes(label = feature, angle = angle, hjust = hjust), family = 'Roboto', fontface = 'bold', size = 3, data = labs) +
    geom_text(aes(label = genre), x = 0, y = -.5, col = '#1cd760') +
    expand_limits(y = -.5) +
    coord_polar(clip = 'off') +
    labs(title = 'Spotify Playlist Genres', 
         subtitle = 'Petals indicate the relative extent of selected audio features', 
         caption = 'Data by Spotify\n #TidyTuesday • @watzoever') +
    theme_void() +
    theme(text = element_text(family = 'Roboto', color = '#1cd760'),
          plot.title = element_text(face = 'bold', size = 24),
          plot.subtitle = element_text(size = 16, margin = margin(t = 10)),
          plot.caption = element_text(size = 10),
          strip.text = element_blank(),
          plot.margin = unit(c(1, 2, 1, 2), 'cm'),
          panel.spacing = unit(-5, 'lines'),
          plot.background = element_rect(fill = 'black', color = NA))

ggsave('plots/2020-04_song_genres.png', width = 7, height = 6, scale = 1.4, bg = 'black')

