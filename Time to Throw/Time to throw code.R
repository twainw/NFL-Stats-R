library(tidyverse)
library(nflreadr)
library(nflplotR)
library(rvest)
library(httr)
library(jsonlite)
library(glue)
`%nin%` = Negate(`%in%`)

#--------------------------------------------------------
# PBP + FTN + Misc Data
#--------------------------------------------------------

season = 2024

# Step: Team colors logos

teams <- nflfastR::teams_colors_logos |> 
  filter(team_abbr %nin% c("LAR", "SD", "OAK", "STL"))

# Step: Load FTN game and play id mappings

game_mappings <- load_ftn_charting(seasons = season) |> 
  distinct(nflverse_game_id, ftn_game_id, season, week)

# Step: Get the first and last ftn game id

start <- game_mappings |> select(ftn_game_id) |> arrange(ftn_game_id) |> slice(1) |> pull()
end <- game_mappings |> select(ftn_game_id) |> arrange(ftn_game_id) |> slice(n()) |> pull()

# Step: Declare parameters for the loop

num <- seq(start+1, end, by = 1)

# Step: Test the first game to make sure API works

url <- paste0("https://data.ftndata.com/game/", start, "/charts")
req <- GET(url, add_headers(Authorization = key))
ftn_pbp <- as.data.frame(fromJSON(rawToChar(req$content)))

# Step: For loop that pulls every game's charting data

for(i in seq_along(num)){
  try(url <- paste0("https://data.ftndata.com/game/", num[i], "/charts"))
  try(req <- GET(url, add_headers(Authorization = key)))
  try(ftn_pbp <- bind_rows(ftn_pbp, as.data.frame(fromJSON(rawToChar(req$content)))))
}

# Step: Load FTN Charting data - for game and play ids to join w ftn_pbp

ftn_charting <- load_ftn_charting(seasons = season)

# Step: Load PBP from nflreadR

pbp <- load_pbp(seasons = season)

# Step: Inner join ftn play and game ids and pbp, 
# pull variables from ftn_pbp

pbp_w_ftn <- pbp |> 
  select(-week, -qtr) |> 
  inner_join(ftn_charting, 
             by = c("game_id" = "nflverse_game_id", 
                    "play_id" = "nflverse_play_id",
                    "season")) |> 
  relocate(ftn_game_id, .before = "game_id") |> 
  relocate(ftn_play_id, .before = "play_id") |> 
  inner_join(ftn_pbp, 
             by = c("ftn_play_id" = "pid", 
                    "ftn_game_id" = "gid"))

week <- max(pbp_w_ftn$week)

#--------------------------------------------------------
# Data prep
#--------------------------------------------------------

# Step: PBP passes

passes <- pbp_w_ftn |>
  filter(!is.na(epa), !is.na(posteam), pass == 1, two_point_attempt != 1, play_type != 'no_play',
         !is.na(ttp)) |>
  filter(!is.na(passer_player_id), sack != 1) |> 
  mutate(ttp = ifelse(ftn_play_id == 1064336, 2.5, ttp))
  
# Step: Find players with at least 75 attempts

pass_summary <- passes |> 
  group_by(passer_player_id, passer_player_name) |> 
  summarize(passes = sum(pass_attempt),
            avg_ttp = mean(ttp)) |> 
  ungroup() |> 
  filter(passes >= 75)

# Step: Filter for the selected players

df <- passes |> 
  filter(passer_player_id %in% pass_summary$passer_player_id)

# Step: Avg time to pass for each player

avg_ttp <- df |> 
  group_by(passer_player_id, passer_player_name, posteam) |> 
  summarize(avg_ttp = mean(ttp, na.rm = TRUE),
            outlier_threshold = quantile(ttp, probs = 0.75, na.rm = T),
            attempts = n(),
            .groups = 'drop') |> 
  ungroup()

#--------------------------------------------------------
# Visual
#--------------------------------------------------------

# All throws

df |>
  mutate(passer_player_name = fct_reorder(passer_player_name, 
                                          .x = ttp, .fun = mean, 
                                          .desc = F)) |> 
  ggplot(aes(y = passer_player_name, x = ttp)) +
  geom_boxplot(alpha = 0.5, aes(color = posteam)) + 
  ggbeeswarm::geom_quasirandom(alpha = 0.15, aes(color = posteam)) + 
  scale_x_continuous(breaks = seq(0, 13, 1)) + 
  nflplotR::geom_nfl_logos(data = avg_ttp, aes(x = -2.15, y = passer_player_name, team_abbr = posteam), width = 0.0355) +
  geom_text(data = avg_ttp, aes(x = -1.5, y = passer_player_name, 
                               label = sprintf("%.2f (%d)", avg_ttp, attempts),
                               color = posteam),
            hjust = 0, vjust = -0.25, size = 2.5, family = "Rockwell") +
  nflplotR::scale_color_nfl() +
  labs(x = "Time to Pass (seconds)",
       y = "",
       title = "Time to Pass Leaderboard",
       subtitle = glue("Wks 1-{week}, 2024 | Min. 75 attempts | Labels: Avg Time to Pass and (Attempts)"),
       caption = "Plot: @twain_w | Data: FTN"
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "Rockwell"),
    plot.title = element_text(size = 22, face = "bold", hjust = 0.5, margin = margin(b = 7.5)),
    plot.title.position = 'plot',
    plot.subtitle = element_text(size = 12, hjust = 0.5, margin = margin(b = 10)),
    axis.text.y = element_text(size = 10, color = "#505050", hjust = 1),
    axis.ticks.length = unit(0.1, "cm"),
    panel.grid.minor = element_blank(),
    plot.caption = element_text(size = 10, color = "#808080"),
    plot.background = element_blank(),
    axis.line = element_line(color = "black", size = 1),
    axis.ticks = element_line(color = "black", size = 1)
  ) +
  coord_cartesian(xlim = c(-2, 13), clip = "off")

# Save the plot

ggsave("ttp.png", width = 6.87, height = 6.84, dpi = 300, bg = 'white', type = "cairo")




