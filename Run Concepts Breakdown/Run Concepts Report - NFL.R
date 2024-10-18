library(tidyverse)
library(nflreadr)
library(ggrepel)
library(nflplotR)
library(rvest)
library(httr)
library(jsonlite)
library(ggpath)
library(gt)
library(gtExtras)
library(scales)
library(Cairo)
library(glue)
library(ggtext)
options(scipen = 999)
`%nin%` = Negate(`%in%`)
source('https://gist.github.com/andreweatherman/0a81c7122a133a015583587573a123ff/raw/4cd6b9db1d39c46e1b93b069bc801e4eac846b19/gt_plt_bar_pct.R')
ftn_logo <- "https://images.squarespace-cdn.com/content/v1/62e59f74eae4a22fe8361e61/bdc7fd6e-a078-4fc8-b327-0e716427a0a0/data.png?format=1500w"

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

# Step: Inner join ftn play and game ids and pbp, filter for pass_attempts and
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

pbp_w_ftn |> 
  group_by(concept) |> 
  tally()

# Step: PBP runs

runs <- pbp_w_ftn |> 
  filter(!is.na(epa), rush == 1 | scrm == 1 | qb_kneel == 1, season_type == "REG",
         two_point_attempt != 1, play_type != 'no_play') |> 
  mutate(concept = ifelse(scrm == 1, "Scramble", concept)) |> 
  mutate(concept = ifelse(concept %in% c('', 'No Design'), "Unknown", concept)) |> 
  mutate(rush = ifelse(scrm == 1, 1, rush)) |> 
  mutate(rush = ifelse(qb_kneel == 1, 1, rush)) |> 
  mutate(rusher = ifelse(scrm == 1, passer, rusher)) |> 
  mutate(explosive_run = ifelse(rush == 1 & yards_gained >= 10, 1, 0))

runs |> 
  group_by(concept) |> 
  tally()

# Step: Data Selection
## Gap Runs (5): Man/Duo, Power, Counter, Draw, Trap
## Zone Runs (2): IZ, OZ,
## Others (4): Trick/WR Run, QB Sneak, missing, FB Run

categories <- tibble(
  concept = runs |> distinct(concept) |> pull(),
  plot_name = runs |> distinct(concept) |> pull()) |> 
  mutate(group = case_when(
    concept %in% c("Man/Duo", "Power", "Counter", "Draw", "Trap") ~ "Gap Scheme",
    concept %in% c("Inside Zone", "Outside Zone") ~ "Zone Scheme",
    TRUE ~ "Other"
  )) |> 
  mutate(group_num = case_when(
    concept %in% c("Man/Duo", "Power", "Counter", "Draw", "Trap") ~ 1,
    concept %in% c("Inside Zone", "Outside Zone") ~ 2,
    TRUE ~ 3
  ))

# Step: QC w PFR

# https://www.pro-football-reference.com/years/2024/
runs |> 
  group_by(season) |> 
  summarize(plays = sum(rush),
            , rush_epa = sum(epa*rush) / sum(rush)
            , rush_sr = sum(success*rush) / sum(rush)
            , yards = sum(yards_gained)
  ) # validate w PFR

# For a specific team

qc <- runs |> 
  group_by(season, team = posteam) |> 
  summarize(plays = sum(rush),
            , rush_epa = sum(epa*rush) / sum(rush)
            , rush_sr = sum(success*rush) / sum(rush)
            , yards = sum(yards_gained)
  )

# Step: Season and team level summary

df <- runs |> 
  filter(concept %nin% c("Kneel")) |> 
  group_by(season, concept, team = posteam) |> 
  summarize(plays = sum(rush),
            , rush_epa = sum(epa*rush) / sum(rush)
            , rush_sr = sum(success*rush) / sum(rush)
            , yards = sum(yards_gained)
            ) |> 
  group_by(season, team) |> 
  mutate(total_plays = sum(plays)) |> 
  ungroup() |> 
  mutate(play_freq = plays / total_plays) |> 
  select(-total_plays) |> 
  group_by(season, concept) |>
  mutate(across(c(play_freq, rush_epa, rush_sr)
                , ~ rank(-.x, ties.method = 'min')
                , .names = "{col}_rank")) |> 
  ungroup() |> 
  select(team, concept, plays, play_freq, play_freq_rank, 
         rush_epa, rush_epa_rank,
         rush_sr, rush_sr_rank, yards
         ) |> 
  left_join(categories,  by = c("concept"))

# Step: Most efficient running teams

most_efficient_running_team <- runs |> 
  group_by(season, team = posteam) |> 
  summarize(plays = sum(rush),
            , rush_epa = sum(epa*rush) / sum(rush)
            , rush_sr = sum(success*rush) / sum(rush)
            , yards = sum(yards_gained)
  ) |> 
  ungroup() |> 
  arrange(-rush_epa)

# Step: 90% and 10% percentiles of rush epa

rush_epa_90th <- quantile(df$rush_epa, 0.9, na.rm = TRUE)
rush_epa_10th <- quantile(df$rush_epa, 0.1, na.rm = TRUE)

#--------------------------------------------------------
# Visual
#--------------------------------------------------------

run_concept_viz <- function(tm) {
  
  # Step: Plot Functionality
  team_data <- teams |> filter(team_abbr == tm)
  
  title_text = glue('Run Concept Breakdown: {tm}')
  subtitle_text = glue('Data through Week {week}, {season} NFL Season')
  
  title_header <- glue(
    "<div style='display: flex; justify-content: space-between; height: 80px;'>
     <div style='flex-grow: 1;'>
       <span style='font-weight: bold; font-size: 24px; line-height: 1;'>{title_text}</span><br>
       <span style='font-size: 16px; font-weight: normal; line-height: 1;'>{subtitle_text}</span>
     </div>
       <img src='{team_data$team_logo_espn}' style='height: 65px; width: auto;'>
     </div>
   </div>"
  )
  
  footnote2 <- glue("League rank in how often {tm} ran that concept")
  
  df |> 
    filter(team == tm) |> 
    group_by(group) |> 
    arrange(group_num, group, -play_freq) |> 
    gt() |> 
    cols_hide(c(group_num, team, concept, rush_sr_rank, rush_epa_rank)) |>
    cols_move_to_start(c(plot_name, plays, play_freq, play_freq_rank, yards, rush_epa, rush_sr)) |> 
    cols_width(play_freq_rank ~ px(50), play_freq ~ px(150), rush_epa ~ px(50), rush_sr ~ px(50),
               yards ~ px(50)) |>
    cols_align(columns = c(everything(), -plot_name, -play_freq), 'right') |> 
    cols_align(columns = c(plot_name), 'left') |> 
    cols_align(columns = c(play_freq), 'center') |> 
    fmt_percent(columns = c(play_freq, rush_sr), decimals = 1) |> 
    fmt_number(columns = c(rush_epa), decimals = 2) |> 
    tab_footnote(locations = cells_column_labels(columns = play_freq_rank),
                 footnote = footnote2) |> 
    gt_bars(play_freq, labels = T, fill = team_data$team_color, 
            domain = c(0, 0.63), scale_label = T, font_size = "12px") |> 
    gt_hulk_col_numeric(columns = rush_epa, domain = c(rush_epa_10th, rush_epa_90th)) |> 
    tab_style(
      style = list(
        cell_fill(color = '#762a83'),
        cell_text(color = "white", weight = 'bold')
      ),
      locations = cells_body(
        columns = rush_epa, 
        rows = rush_epa < rush_epa_10th
      )
    ) |> 
    tab_style(
      style = list(
        cell_fill(color = '#1b7837'),
        cell_text(color = "white", weight = 'bold')
      ),
      locations = cells_body(
        columns = rush_epa, 
        rows = rush_epa > rush_epa_90th
      )
    ) |> 
    tab_style(
      style = list(
        cell_fill(color = '#C4D8F3'),
        cell_text(weight = 'bold')
      ),
      locations = cells_body(
        columns = play_freq_rank, 
        rows = play_freq_rank <= 10
      )
    ) |> 
    cols_label(
      plot_name = md("Concept"), 
      plays = 'Runs', play_freq = md("% of All Runs"), play_freq_rank = md("Run %<br>Rank"),
      rush_epa = md("EPA/<br>Play"),
      rush_sr = md("Success<br>%")
    ) |> 
    tab_style(locations = cells_row_groups(), style = cell_text(weight = 'bold')) |> 
    tab_style(locations = cells_row_groups(), style = list(cell_fill(color = 'grey90'))) |> 
    tab_options(data_row.padding = 3.5,
      table.margin.left = px(10),
      table.margin.right = px(10)) |> 
    tab_header(title = html(title_header)) |> 
    tab_source_note(source_note = md("**Data: FTN & nflverse<br>Table: @twain_w**"))  |>  
    opt_css(
      '
      #player_report .gt_sourcenote{
        line-height: 1.2;
        padding-top: 9px !important;
      }
      #player_report .gt_group_heading {
        padding-top: 5px !important;
        padding-bottom: 5px !important;
      }
      #player_report .gt_footnote {
        padding-top: 7px !important;
        padding-bottom: 7px !important;
        line-height: 0.2;
      }
      '
    ) |> 
    gt_theme_538() |> 
    gtsave(glue('{tm}.png'))
}

run_concept_viz("KC")

df |> 
  filter(concept == 'Power') |> 
  slice_max(play_freq, n = 5) |> 
  select(team, plays, play_freq) |> 
  gt() |> 
  fmt_percent(columns = c(play_freq), decimals = 1) |> 
  tab_header("Top-5 Power Teams") |> 
  gt_theme_538() |> 
  gtsave("power.png")


