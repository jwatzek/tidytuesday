library(tidyverse)
library(patchwork)
library(cowplot)
library(ggtext)
library(ggrepel)
library(scales)
library(viridis)

theme_set(theme_minimal_hgrid())
theme_update(text = element_text(family = 'Roboto'), 
             plot.caption = element_text(hjust = 0, margin = margin(t = 20)),
             axis.line.y = element_blank(), axis.line.x = element_line(color = 'black'), 
             axis.text.x = element_markdown(hjust = 0), axis.ticks.x = element_blank())

d = read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-11-26/loans.csv")
# write_csv(d, 'data/2019-48_loans.csv')

# d %>% filter(!complete.cases(.))

d2 = d %>% 
    mutate_at(vars(starting:wage_garnishments), function(.) replace_na(./1e9, 0)) %>%
    mutate(inventory = starting + added, yearQ = str_c('20', year, ' Q', quarter)) %>% 
    pivot_longer(total:inventory, names_to = 'method', values_to = 'amount_B') %>% 
    mutate(method = str_to_title(str_replace_all(method, '_', ' '))) %>% 
    group_by(yearQ, year, method) %>% 
    summarise(amount_B = sum(amount_B))

d3b = filter(d2, !(method %in% c('Inventory', 'Total')))
d3a = setdiff(d2, d3b) %>% 
    pivot_wider(names_from = method, values_from = amount_B) %>% 
    rename(amount_B = Total) %>%
    mutate(
        lab3a = ifelse(yearQ %in% c('2015 Q4', '2018 Q4'), str_c('$', round(Inventory, 1), 'B'), ''),
        lab3b = percent(amount_B/Inventory, accuracy = .1),
        method = 'Consolidation'
        )

year_cols = str_sub(viridis(4)[-1], end = -3)
year_labs = str_c('<b style="color:', year_cols, '">', 2016:2018, '</b>')

p1 = ggplot(d3a, aes(yearQ, Inventory, group = 1)) +
    geom_area(position = 'identity', alpha = .2) +
    geom_line() +
    geom_point(aes(fill = year), shape = 21, size = 3, show.legend = F) +
    geom_text_repel(aes(label = lab3a), size = 3.2, point.padding = .2) +
    expand_limits(y = 0) +
    scale_y_continuous(labels = dollar_format(suffix = 'B'), expand = expand_scale(mult = c(0, .05))) +
    scale_x_discrete(breaks = c('2016 Q2', '2017 Q2', '2018 Q2'), labels = year_labs) +
    scale_fill_viridis_c() +
    labs(x = '', y = 'Total Outstanding Debt', title = 'U.S. Student Loans', 
         subtitle = 'The total amount of outstanding debt is more than 2.5 times higher than it\nwas at the end of 2015.')

p2 = ggplot(d3b, aes(yearQ, amount_B, fill = year)) +
    geom_bar(fill = 'white', stat = 'identity', width = .8) +
    geom_bar(aes(fill = year, alpha = method), stat = 'identity', col = 'black', width = .8) +
    geom_text(aes(label = lab3b), data = d3a, size = 3.2, vjust = -.8) +
    expand_limits(y = 0) +
    scale_y_continuous(labels = dollar_format(suffix = 'B'), expand = expand_scale(mult = c(0, .07))) +
    scale_x_discrete(breaks = c('2016 Q2', '2017 Q2', '2018 Q2'), labels = year_labs) +
    scale_fill_viridis_c() +
    scale_alpha_discrete('Recovery Method') +
    labs(x = '', y = 'Total Repaid', title = '', 
         subtitle = 'Only a small percentage of the total outstanding debt is collected. Most is\ncollected through rehabilitation.',
         caption = 'Data by U.S. Department of Education\n#TidyTuesday • @watzoever') +
    guides(fill = F)

p1 / p2 + plot_annotation(theme = theme(plot.margin = unit(c(2, 2, 2, 2), 'cm')))
ggsave('plots/2019-48_loans.png', width = 10, height = 10)

