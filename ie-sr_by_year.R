
ie_dat_raw <- read_xlsx("Data/ier-published_2023-10-27.xlsx")

ie_dat <- ie_dat_raw %>% 
  clean_names() %>% 
  mutate(id = as.character(id)) %>% 
  rename(year = year_of_publication)

ie_by_year <- ie_dat %>% 
  filter(year != 999,
         year < 2023) %>% 
  drop_na(year) %>% 
  group_by(year) %>% 
  summarise(year_tot = n_distinct(id)) %>% 
  mutate(cum_tot = cumsum(year_tot))

plot_ie_year <- ggplot(
  data = ie_by_year,
  aes(x = year)) +
  geom_line(aes(y = year_tot, color = "Yearly Total", group = 1), linewidth = 0.75) +
  # geom_line(aes(y = cum_tot, color = "Cumulative Total", group = 1), linewidth = 0.75) +
  labs(title = "Impact evaluations published",
       x = "Year",
       y = "Number of Studies",
       caption = "Source: 3ie Development Evidence Portal as of October 2023") +
  # scale_color_manual(values = c("Yearly Total" = "blue", "Cumulative Total" = "red")) +
  scale_color_manual(values = c("Yearly Total" = "blue")) +
  theme(
    text = element_text(family = "Arial"),
    legend.title = element_blank(),
    legend.text = element_text(size = 32),
    plot.title = element_text(size = 32,
                              face = "bold",
                              hjust = 0.5,
                              margin = margin(b = 0.5, unit = "cm")),
    panel.background = element_blank(),
    axis.text.x = element_text(angle = 45,
                               size = 18),
    axis.text.y = element_text(size = 18),
    axis.title = element_text(size = 22),
    panel.grid.major.y = element_line(
      linewidth = 0.25,
      colour = "#DEDEDE"
    )
  )

plot_ie_year

ggsave(
  filename = "C:/Users/ME/Desktop/IEs over time.png",
  plot = plot_ie_year,
  device = "png",
  scale = 1,
  width = 24,
  height = 12,
  dpi = 400,
  limitsize = FALSE,
  bg = NULL
)
