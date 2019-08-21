library(tidyverse)
library(ggthemes)
nuclear_explosions <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-08-20/nuclear_explosions.csv")

nuclear_explosions <- nuclear_explosions %>% 
  mutate(decade = ifelse(year %in% 1940:1949, "1940s",
                         ifelse(year %in% 1950:1959, "1950s",
                                ifelse(year %in% 1960:1969, "1960s",
                                       ifelse(year %in% 1970:1979, "1970s",
                                              ifelse(year %in% 1980:1989, "1980s",
                                                     ifelse(year %in% 1990:1999, "1990s", "WIP")))))))
table(nuclear_explosions$decade)

by_country <- group_by(nuclear_explosions, country, decade) %>% 
  summarise(count = n()) %>% 
  ungroup() %>% 
  group_by(decade) %>% 
  mutate(percent_in_decade = paste(round((count/sum(count))*100,1), "%")) %>% 
  arrange(decade, -count) %>% 
  ungroup()

by_country$decade <- factor(by_country$decade, levels = c("1990s", "1980s", "1970s", "1960s", "1950s", "1940s"),
                                                          labels = c("1990s", "1980s", "1970s", "1960s", "1950s", "1940s"))

by_country %>%
  mutate(country = reorder(country, count)) %>% 
  ggplot(aes(decade, count, fill = country)) + 
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = by_country$percent_in_decade), position = position_dodge(width = 1), vjust = 0.5, hjust = -0.2, size = 2)  +
  theme_igray()+
  labs(title = "Number and percentage of Nukes deployed by counties per decade",
       subtitle = "Note: The percentages in the chat represent percentage of Nukes deployed by each country in that specific decade",
       caption = "Data by SIPRI | Plot by @NdiranguMartin_") +
  theme(plot.title = element_text(hjust = 0.5, size = 12, family = "lato"),
        plot.subtitle = element_text(hjust = 0.5, size = 8)) +
  coord_flip()

ggsave("Nuclear_plot.png", width = 10, height = 4.5)
