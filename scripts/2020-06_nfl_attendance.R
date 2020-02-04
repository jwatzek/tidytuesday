library(tidyverse)
library(ggtext)

d = read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-04/attendance.csv')

d2 = d %>% 
    group_by(team_name, year) %>% 
    filter(row_number() == 1) %>% 
    ungroup %>% 
    mutate(
        team = ifelse(team_name == 'Rams', 'St. Louis / LA', ifelse(team_name == 'Chargers', 'San Diego / LA', team)),
        team2 = str_c(team, team_name, sep = ' ')) %>% 
    pivot_longer(home:away, names_to = 'location', values_to = 'attendance') %>% 
    mutate(attendancek = attendance/1e3, location = factor(toupper(location), levels = c('HOME', 'AWAY'))) %>% 
    complete(team2, year, location)

labs = tibble(
    team2 = c('San Diego / LA Chargers', 'Dallas Cowboys', 'Arizona Cardinals'),
    year = c(2017, 2009, 2006),
    location = factor(rep('HOME', 3), levels = c('HOME', 'AWAY')),
    lab = 1:3,
    col = c('black', 'white', 'black')
)

ggplot(d2, aes(year, fct_rev(team2))) +
    facet_wrap(. ~ location) +
    geom_tile(aes(fill = attendancek), col = 'black', alpha = .8) +
    geom_text(aes(label = lab, col = col), data = labs, family = 'Karla', size = 3) +
    scale_x_continuous(expand = expansion(c(0, .05))) +
    scale_color_identity() +
    scale_fill_gradientn('', breaks = 3:7*100, labels = str_c(3:7*100, 'k'),
                         colors = wesanderson::wes_palette('Moonrise1', 100, type = 'continuous')[30:100], na.value = 'white') +
    coord_equal() +
    labs(x = '', y = '',
         title = toupper('NFL Attendance'), 
         subtitle = '<strong>Attendance for home games varies much more than that of away games.</strong><br/><br/>
         <span style="font-size: 10pt;">
         Some notes:<br/><br/>
         (1) The Chargers moved to Los Angeles.<br/>
         (2) The Cowboys moved into the AT&T Stadium.<br/>
         (3) The Cardinals moved into the State Farm Stadium.</span>', 
         caption = 'Data by Pro Football Reference\n #TidyTuesday | @watzoever') +
    cowplot::theme_cowplot() +
    theme(text = element_text(family = 'Karla'),
          axis.text = element_text(size = 10), axis.line = element_blank(), axis.ticks = element_blank(),
          legend.position = 'bottom', legend.text = element_text(size = 10), legend.key.width = unit(1.5, 'cm'),
          strip.background = element_blank(), panel.spacing.x = unit(1, 'cm'),
          strip.text = element_text(family = 'Freshman', face = 'bold', margin = margin(b = 10)),
          plot.title = element_text(family = 'Freshman', size = 22),
          plot.subtitle = element_markdown(margin = margin(t = 10, b = 20)),
          plot.margin = unit(c(2, 2.5, 2, 0), 'cm'))

ggsave('plots/2020-06_nfl_attendance.png', width = 7, height = 6, scale = 1.7)

