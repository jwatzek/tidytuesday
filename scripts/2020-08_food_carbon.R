library(tidyverse)
library(ggtext)

d = read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-18/food_consumption.csv')
# write_csv(d, 'data/2020-08_food_carbon.csv')

d2 = d %>% 
    mutate(panel = 2,
           food_category = recode(food_category, 'Wheat and Wheat Products' = 'Wheat & Wheat Products', 'Milk - inc. cheese' = 'Milk inc. Cheese'),
           product_cat = ifelse(food_category %in% c('Wheat & Wheat Products', 'Rice', 'Soybeans', 'Nuts inc. Peanut Butter'), 'Non-Animal Products', 'Animal Products'))

d3 = d2 %>% 
    group_by(product_cat, food_category) %>% 
    summarise(co2_emmission = mean(co2_emmission)) %>% 
    arrange(product_cat, desc(co2_emmission)) %>% 
    mutate(country = '**Average**', panel = 1)

cat_order = d3$food_category

d4 = d2 %>% 
    bind_rows(d3) %>% 
    group_by(country) %>%
    mutate(total_by_country = sum(co2_emmission)) %>% 
    ungroup %>% 
    # mutate(rank = dense_rank(total_by_country)) %>% 
    # filter(country == 'Average' | rank <= 20 | rank > max(rank) - 20) %>%
    mutate(country = reorder(country, total_by_country),
           country = fct_relevel(country, '**Average**', after = Inf),
           food_category = factor(food_category, levels = rev(cat_order)),
           co2_emmission2 = ifelse(product_cat == 'Animal Products', co2_emmission, -co2_emmission))

pal = wesanderson::wes_palette('Rushmore1', 20, type = 'continuous')[c(6:8, 10, 14:20)]

p = ggplot(d4, aes(country, co2_emmission2)) +
    lemon::facet_rep_grid(panel ~ ., scales = 'free_y', space = 'free_y', repeat.tick.labels = T) +
    geom_bar(aes(fill = food_category), stat = 'identity', width = .8) +
    coord_flip() +
    scale_y_continuous(expand = c(.05, .1)) +
    scale_fill_manual('', values = pal[c(1:4, 11:5)], breaks = rev(cat_order)[c(1:4, 11:5)]) +
    scale_color_identity() +
    labs(x = '', y = '**CO<sub>2</sub> emission** [kg/person/year]',
         title = 'Are you on a low carb(on) diet?', 
         subtitle = glue::glue("130 countries' CO<sub>2</sub> emission based on the amount of <b style='color: {pal[4]}'>non-animal products</b> and <b style='color: {pal[11]}'>animal products</b> that a country<br/>supplied for consumption in 2013. This may include a percentage of food that is not consumed, e.g. food waste."), 
         caption = 'Data by the Food and Agriculture Organization of the United Nations (via nu3)\n#TidyTuesday • @watzoever') +
    cowplot::theme_minimal_vgrid() +
    theme(text = element_text(family = 'Source Sans Pro'),
          axis.text.x = element_text(color = 'grey40'),
          axis.text.y = element_markdown(),
          axis.line.y = element_blank(), axis.ticks = element_blank(),
          axis.title.x = element_markdown(size = 12, margin = margin(t = 10)),
          legend.position = c(.96, .9), legend.background = element_rect(fill = 'white'),
          legend.text = element_text(size = 11), legend.key.size = unit(.4, 'cm'),
          strip.text.y = element_blank(),
          plot.title = element_text(size = 20),
          plot.subtitle = element_markdown(margin = margin(b = 25)),
          plot.caption = element_text(margin = margin(t = 25), hjust = 0),
          plot.margin = unit(c(2, 6.5, 2, 2), 'cm'))

ggsave('plots/2020-08_food_carbon.png', p, width = 8, height = 15, scale = 1.7)

