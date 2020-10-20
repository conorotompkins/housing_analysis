library(tidyverse)
library(lubridate)
library(janitor)
library(hrbrthemes)
library(gganimate)

options(scipen = 999, digits = 4)

theme_set(theme_ipsum())

zillow_house_value <- list.files("data", full.names = TRUE) %>% 
  set_names() %>% 
  map_dfr(read_csv) %>% 
  pivot_longer(cols = matches("\\d{4}-\\d{2}-\\d{2}"),
               names_to = "date", values_to = "zhvi") %>% 
  clean_names() %>% 
  mutate(date = ymd(date),
         region_name = str_squish(region_name))

glimpse(zillow_house_value)

zillow_house_value %>% 
  group_by(region_name) %>% 
  summarize(pct_na = mean(is.na(zhvi))) %>% 
  arrange(desc(pct_na))

zillow_house_value %>%
  distinct(region_name) %>% 
  filter(str_detect(region_name, "Pittsburgh"))

zillow_house_value %>% 
  distinct(date) %>% 
  filter(date == min(date) | date == max(date))

zillow_house_value %>% 
  ggplot(aes(date, zhvi, group = region_name)) +
  geom_line(alpha = .1, size = .5)
  

#highlighed bar graph

zillow_house_value %>% 
  mutate(test_label = "test") %>% 
  ggplot(aes(date, zhvi, group = region_name)) +
  geom_line(alpha = .1, size = .5) +
  annotate(geom = "text", x = ymd("2010-05-01"), y = 10^6, label = "Test") +
  scale_color_manual(values = c("gold", "red"))




df_top_regions <- zillow_house_value %>% 
  group_by(region_name) %>% 
  summarize(sd = sd(zhvi)) %>% 
  ungroup() %>% 
  arrange(desc(sd)) %>% 
  slice(1:25) %>% 
  mutate(region_name_rank = str_c("#", row_number(), " ", region_name, sep = ""))
  

region_name_highlight_fct <- df_top_regions %>% 
  pull(region_name)

region_name_rank_fct <- df_top_regions %>% 
  pull(region_name_rank)

df_highlights <- zillow_house_value %>% 
  inner_join(df_top_regions) %>% 
  mutate(region_name_highlight = region_name,
         region_name_highlight = factor(region_name_highlight, levels = region_name_highlight_fct),
         region_name_rank = factor(region_name_rank, levels = region_name_rank_fct))


housing_animation <- zillow_house_value %>% 
  ggplot() +
  geom_line(aes(date, zhvi, group = region_name), alpha = .1, size = .5) +
  # geom_line(data = zillow_house_value %>% 
  #             filter(region_name == "United States"),
  #           aes(date, zhvi),
  #           color = "blue", size = 3, lty = 2) +
  geom_line(data = df_highlights,
            aes(date, zhvi),
            color = "red", size = 1.5) +
  #annotate(geom = "text", x = ymd("2010-05-01"), y = 10^6)
  scale_y_continuous(labels = scales::dollar_format()) +
  transition_manual(region_name_rank) +
  labs(title = "Top 25 most volatile housing markets 1996-2020",
       subtitle = "Region: {current_frame}",
       x = NULL,
       y = "Zillow Housing Value Index") +
  theme(plot.subtitle = element_text(size = 15),
        axis.title.y = element_text(size = 15))
  

anim_save("output/housing_animation.gif", animation = housing_animation,
          duration = 40, fps = 20, width = 1000, height = 500)
