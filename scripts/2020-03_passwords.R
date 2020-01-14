library(tidyverse)

# txt = readLines('data/2020-03_passwords_rockyou-withcount.txt')
# total = sum(as.numeric(str_sub(txt, end = 8)))
total = 32603388

d = read_delim('data/2020-03_passwords_rockyou-withcount.txt', delim = ' ', col_names = c('count', 'password'), trim_ws = T, n_max = 1.5e3)

d2 = d %>% 
    mutate(perc = count/total,
           len = str_length(password),
           type = case_when(
               str_detect(password, '^[[:digit:]]+$') ~ 'numeric',
               str_detect(password, '^[[:lower:]|[:digit:]]+$') ~ 'lowercase alphanumeric',
               str_detect(password, '^[[:alpha:]]+$') ~ 'lower & uppercase alphanumeric',
               str_detect(password, '^[[:graph:]|[:space:]]+$') ~ 'lower & uppercase alphanumeric & symbols',
               T ~ 'other'
           ))

# write_csv(d2, 'data/2020-03_passwords1500.csv')

d3 = top_n(d2, 20, count) %>% mutate(perc2 = str_c(round(perc*100, 2), '%'))
top_n(d2, 20, count) %>% count(type, sort = T) %>% mutate(perc = n/sum(n))

ggplot(d3, aes(reorder(password, perc), perc)) +
    geom_bar(aes(fill = type), col = 'black', stat = 'identity', width = .7) +
    annotate('rect', ymin = .009, ymax = .0092, xmin = 22.5, xmax = 23.5, fill = 'red3', col = 'black') +
    annotate('text', y = .0091, x = 23, label = '×', col = 'white', size = 6, fontface = 'bold') +
    annotate('rect', ymin = c(.00293, .005), ymax = c(.00384, .0059), xmin = 21.5, xmax = 21.5+.7, fill = c('grey70', NA), col = 'black') +
    geom_text(aes(y = 0, label = password), hjust = 0, nudge_y = 4e-5, family = 'Courier New', fontface = 'bold', size = 2.8) +
    geom_text(aes(label = perc2), hjust = 0, nudge_y = 6e-5, family = 'Courier New', size = 2.8) +
    scale_y_continuous(labels = scales::percent, expand = expansion(c(0, .05))) +
    scale_fill_manual(values = c('grey70', 'white'), guide = F) +
    coord_flip(clip = 'off', xlim = c(0, 21), ylim = c(0, .00945)) +
    labs(x = '', y = '',
         title = 'Top 20 of 32.6 Million Passwords from 2009 RockYou Leak', 
         subtitle = '7 in 10 of these passwords were lower case alphanumeric and 3 in 10 were numeric only',
         caption = "Data by SkullSecurity • #TidyTuesday • @watzoever") +
    theme_void() +
    theme(text = element_text(family = 'Courier New', face = 'bold'),
          plot.title = element_text(size = 18, margin = margin(b = 5)),
          plot.caption = element_text(hjust = 0),
          plot.margin = unit(c(2, 0, 2, 2), 'cm'))

ggsave('plots/2020-03_passwords.png', width = 7, height = 4, scale = 2)

