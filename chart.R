library(ggplot2)
library(tidyverse)
library(databrew)



df <- data_frame(Country = c('Wales',
                             'Scotland',
                             'Texas',
                             'Catalonia',
                             'Quebec'),
                 percent = c(5, 44.7, 26, 48, 28),
                 source = c('https://www.icmunlimited.com/polls/bbc-wales-st-davids-day-poll-2018/',
                              'https://en.wikipedia.org/wiki/Scottish_independence_referendum,_2014',
                            'https://en.wikipedia.org/wiki/Texas_secession_movements#Opinion_polling',
                            'https://goo.gl/RQG9q7',
                            'https://en.wikipedia.org/wiki/Quebec_sovereignty_movement')) %>%
  arrange(percent) %>%
  mutate(Country = factor(Country, levels = Country))

ggplot(data = df %>% filter(Country != 'Texas'),
       aes(x = Country,
           y = percent)) +
  geom_bar(stat = 'identity',
           fill = 'darkorange',
           alpha = 0.7) +
  labs(y = '% pro-independence',
       x = NULL) +
  theme_databrew() +
  geom_label(aes(label = round(percent),
                 y = percent + 3),
             size = 5) +
  theme(axis.text.x = element_text(size = 18),
        axis.title.y = element_text(size = 20))

text_plot <- function(labels = df$Country, 
                      values = df$percent,
                      v = 5){
  labels <- as.character(labels)
  a_length <- nchar(labels)
  max_a <- max(a_length)
  add_a <- max_a - a_length + 1
  for(i in 1:length(labels)){
    labels[i] <- paste0(labels[i],
                        paste0(rep(' ', add_a[i]), collapse = ''))
  }
  vs <- round(values / v)
  vss <- c()
  for(i in 1:length(vs)){
    vss[i] <- paste0(rep('=', vs[i]), collapse = '')
  }
  cat(paste0(labels, vss, ' ', values, '%', collapse = '\n'))
  
}
text_plot(v = 10)
