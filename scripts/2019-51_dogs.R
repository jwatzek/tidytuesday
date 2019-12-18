library(tidyverse)
library(ggforce)
library(ggtext)
library(glue)
library(cowplot)
library(viridis)
library(beepr)

# dog_moves = read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-12-17/dog_moves.csv')
# dog_travel = read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-12-17/dog_travel.csv')
dog_descriptions = read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-12-17/dog_descriptions.csv')
# beep()

# write_csv(dog_moves, 'data/2019-51_dog_moves.csv')
# write_csv(dog_travel, 'data/2019-51_dog_travel.csv')
# write_csv(dog_descriptions, 'data/2019-51_dog_descriptions.csv')
# beep()

ga = dog_descriptions %>% filter(contact_state == 'GA') %>% 
    select(id, breed_primary:breed_unknown, age:coat)

top15_prim = count(ga, breed_primary, sort = T) %>% top_n(15) %>% pull(breed_primary)
top15_sec = count(ga, breed_secondary, sort = T) %>% top_n(15) %>% pull(breed_secondary)
top = union(top15_prim, top15_sec[-1])

count(ga, breed_primary %in% top) %>% mutate(p = prop.table(n))

cnt = ga %>% 
    filter(breed_primary %in% top) %>%
    mutate(
        breed_secondary = replace_na(breed_secondary, ''),
        breed_secondary = ifelse(!breed_mixed, breed_primary, breed_secondary),
        breed_secondary = ifelse(breed_secondary %in% top, breed_secondary, 'Other'),
        breed_primary = factor(breed_primary, levels = c(top, 'Other', '')),
        breed_secondary = factor(breed_secondary, levels = c(top, 'Other', ''))
    ) %>% 
    count(breed_primary, breed_secondary, sort = T, .drop = F) %>% 
    gather_set_data(1:2)

lab = filter(cnt, x == 'breed_primary') %>% mutate(y = fct_recode(y, ' ' = 'Other'))
lab2 = filter(cnt, x == 'breed_secondary')

cols = c(plasma(16), '#050505', NA)

ggplot(cnt, aes(x, id = id, split = y, value = n)) +
    geom_parallel_sets(aes(fill = breed_primary), sep = .2, alpha = .7) +
    geom_parallel_sets_axes(data = lab, aes(fill = breed_primary), axis.width = .05, sep = .2) +
    geom_parallel_sets_axes(data = lab2, aes(fill = breed_secondary), axis.width = .05, sep = .2) +
    geom_text(data = lab, stat = 'parallel_sets_axes', sep = .2, hjust = 1, nudge_x = -.05, col = 'grey80', fontface = 'bold', family = 'Roboto') +
    geom_text(data = lab2, stat = 'parallel_sets_axes', sep = .2, hjust = 0, nudge_x = .05, col = 'grey80', family = 'Roboto') +
    scale_fill_manual(values = cols, guide = F) +
    scale_x_discrete(labels = c('Primary Breed', 'Secondary Breed')) +
    labs(title = 'Adoptable Dogs in Georgia by Breed', 
         subtitle = glue('The most common **primary** and secondary dog breeds of {nrow(ga)} dogs listed<br/>
                          as adoptable on Petfinder in the state of Georgia, USA on Sep 20, 2019.'), 
         caption = 'Data by Petfinder • #TidyTuesday • @watzoever<br/>
                    Scraped & cleaned by Amber Thomas @ProQuesAsker') +
    theme_map() +
    theme(text = element_text(family = 'Roboto', color = 'grey80'), 
          plot.title = element_text(family = 'Lilita One', size = 18),
          plot.subtitle = element_markdown(lineheight = 1.4, margin = margin(b = 10)),
          plot.caption = element_markdown(lineheight = 1.4),
          plot.margin = unit(c(2, 2, 2, 2), 'cm'),
          plot.background = element_rect(fill = 'grey10', color = NA))

ggsave('plots/2019-51_dogs.png', width = 10, height = 8)


