library(nflreadr)
library(tidyverse)
library(nflplotR)
library(paletteer)

# set theme
theme_owen <- function() {
  theme_minimal(base_size = 10) %+replace%
    theme(
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = 'floralwhite', 
                                     color = 'floralwhite')
    )
}

# participation data
participation <- load_participation(seasons = 2021, include_pbp = T)|> 
  filter(play_type == "pass")

# personnel groupings by posteam
df <- participation |> 
  group_by(posteam, offense_personnel) |> 
  summarize(dropbacks = sum(qb_dropback),
            mean_epa = mean(epa, na.rm = T)) |> 
  ungroup() |> 
  group_by(posteam) |> 
  mutate(total_dropbacks = sum(dropbacks)) |> 
  ungroup() |> 
  mutate(perc = dropbacks / total_dropbacks) |> 
  filter(perc >= 0.05)

# efficiency by personnel usage
pal <- "ggthemes::Jewel_Bright"

df |> 
  ggplot(aes(x = perc, y = mean_epa, color = offense_personnel#, size = perc
             )) +
  geom_hline(yintercept = 0, linetype = 'dashed', size = 0.2, alpha = 0.5) +
  geom_point(size = 3) +
  facet_wrap(vars(posteam), nrow = 4, scales = 'free') +
  scale_y_continuous(limits = c(-0.45, 0.6), 
                     breaks = seq(-0.45, 0.60, by = 0.15),
                     labels = scales::number_format()) +
  scale_x_continuous(limits = c(0, 1), 
                     breaks = seq(0.20, 0.80, by = 0.20),
                     labels = scales::percent_format()) + 
  guides(col = guide_legend(nrow = 1), size = "none") +
  scale_fill_paletteer_d(pal) +
  scale_color_paletteer_d(pal) +
  labs(x = "Dropback Rate",
       y = "EPA per Play",
       title = "Efficiency and Usage by Personnel, 2021 Regular Season",
       subtitle = "Inspired by @reinhardNFL | min 10% of Snaps in Groupings") +
  theme_owen() +
  theme(legend.position = 'bottom',
        legend.title = element_blank(),
        legend.box = "horizontal",
        axis.line = element_line(color = 'black'),
        strip.text = element_nfl_wordmark(size = .75),
        axis.title = element_text(size = 10.5),
        plot.title = element_text(size = 13, face = 'bold'),
        plot.title.position = 'plot')



