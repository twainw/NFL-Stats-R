# https://github.com/andreweatherman/Viz-Tutorials/blob/main/player_report/player_report.md

library(tidyverse)
library(cbbdata)
library(gt)
library(gtExtras)
library(glue)
library(here)
library(magick)

# Data Selection

cols_to_plot <- tibble(
  stat = c('mpg', 'ppg', 'oreb', 'dreb', 'rpg', 'bpg', 'apg', 'spg', 'tov', 'efg', 'three_pct', 'ft_pct', 'three_rate', 'ortg', 'drtg', 'usg', 'porpag'),
  plot_name = c('Minutes', 'Points', 'Off. Rebounds', 'Def. Rebounds', 'Rebounds', 'Blocks', 'Assists', 'Steals', 'Turnovers', 'eFG%', '3FG%', 'FT%', '3FGA/FGA', 'Off. Rating', 'Def. Rating', 'Usage', 'PORPAG'),
  group = c(rep('Box (Per-Game)', 9), rep('Shooting', 4), rep('Advanced', 4))
)

# Compute percentiles and grab player data
cbbdata::cbd_login(username = 'xxx', password = 'xxx')
season_averages <- cbd_torvik_player_season(year = 2024)

get_season_percentiles <- function(min_cutoff, games_cutoff, cols_to_plot = cols_to_plot) {
  
  hm <- c('ACC', 'BE', 'B10', 'B12', 'P12', 'SEC')
  mm <- c('MWC', 'A10', 'Amer', 'MVC', 'WCC', 'Ivy', 'CUSA', 'SC')
  reverse_cols <- c('tov_', 'drtg_')
  
  ptiles <- season_averages %>% 
    mutate(fg_pct = fgm / fga, three_pct = three_m / three_a, ft_pct = ftm / fta,
           efg = (0.5 * three_m + fgm) / fga, three_rate = (three_a / fga) * 100,
           conf_group = case_when(conf %in% hm ~ 'High-Majors', conf %in% mm ~ 'Mid-Majors',
                                  .default = 'Low-Majors'),
           pos_group = case_when(pos %in% c('Combo G', 'Pure PG', 'Scoring PG') ~ 'guards',
                                 pos %in% c('Wing F', 'Wing G', 'Stretch 4') ~ 'wings',
                                 pos %in% c('C', 'PF/C') ~ 'bigs')) %>% 
    filter(min >= min_cutoff & g >= games_cutoff) %>% 
    select(all_of(c('player', 'team', 'conf_group', 'pos_group', cols_to_plot$stat))) %>%
    mutate(across(-c(player, team), percent_rank, .names = '{.col}_ptile'), .by = pos_group) %>% 
    mutate(across(-c(player, team), percent_rank, .names = '{.col}_conf_ptile'),
           .by = c(conf_group, pos_group)) %>% 
    # subtract 1 from reverse cols
    mutate(across(all_of(starts_with(reverse_cols)), ~1 - .x))
  
  return(ptiles)
  
}

get_player <- function(player, team, season_percentiles = NULL, min_cutoff = NULL, games_cutoff = NULL) {
  
  if(is.null(season_percentiles)) {
    season_percentiles <- get_season_percentiles(min_cutoff = min_cutoff,
                                                 games_cutoff = games_cutoff,
                                                 cols_to_plot)
  }
  
  player_data <- season_percentiles %>% 
    filter(player == !!player & team == !!team) %>% 
    pivot_longer(cols = -c(player, team, conf_group, pos_group), names_to = "variable", values_to = "value") %>% 
    extract(variable, into = c("stat", "measure"), "^(.+?)(?:_(ptile|conf_ptile))?$") %>% 
    mutate(measure = ifelse(measure == '', 'raw', measure)) %>% 
    pivot_wider(id_cols = c(player, team, conf_group, pos_group, stat), names_from = measure, values_from = value) %>%
    filter(!is.na(ptile) & !is.na(conf_ptile)) %>% 
    left_join(cols_to_plot, by = 'stat') %>% 
    mutate(
      natl_grade = case_when(
        ptile >= 0.95 ~ "A+",
        ptile >= 0.90 ~ "A",
        ptile >= 0.85 ~ "A-",
        ptile >= 0.80 ~ "B+",
        ptile >= 0.75 ~ "B",
        ptile >= 0.70 ~ "B-",
        ptile >= 0.65 ~ "C+",
        ptile >= 0.60 ~ "C",
        ptile >= 0.55 ~ "C-",
        ptile >= 0.50 ~ "D+",
        ptile >= 0.45 ~ "D",
        ptile >= 0.40 ~ "D-",
        .default = "F"
      )
    )
  return(player_data)
  
}

source('https://gist.github.com/andreweatherman/0a81c7122a133a015583587573a123ff/raw/4cd6b9db1d39c46e1b93b069bc801e4eac846b19/gt_plt_bar_pct.R')

plot_player_report <- function(player, team) {
  
  # get team color and logo for plotting
  team_data <- cbd_teams() %>% filter(torvik_team == team)
  player_data <- get_player(player, team, min_cutoff = 40, games_cutoff = 5)
  conf_group <- player_data$conf_group %>% unique()
  pos_group <- player_data$pos_group %>% unique()
  
  font <- 'PT Sans'
  to_scale_percent <- which(player_data$stat %in% c('efg', 'three_pct', 'ft_pct'))
  to_scale_number <- setdiff(1:nrow(player_data), to_scale_percent)
  
  title_text = glue('Player Report Card: {player}')
  subtitle_text = glue('Position groups are determined by Barttorvik. Data through {format(Sys.Date(), "%b %e, %Y")}')
  
  title_header <- glue(
    "<div style='display: flex; justify-content: space-between; align-items: center;'>
     <div style='flex-grow: 1;'>
       <span style='font-weight: bold; font-size: 24px; line-height: 0.5;'>{title_text}</span><br>
       <span style='font-size: 16px; font-weight: normal; line-height: 0.5;'>{subtitle_text}</span>
     </div>
     <div>
       <img src='{team_data$logo}' style='height: 65px; width: auto; vertical-align: middle;'>
     </div>
   </div>"
  )
  
  table <- player_data %>% 
    group_by(group) %>% 
    gt(id = 'player_report') %>% 
    gt_theme_538() %>% 
    cols_hide(c(player, team, stat, conf_group, pos_group)) %>% 
    cols_move_to_start(plot_name) %>% 
    cols_width(ptile ~ px(225), conf_ptile ~ px(225), plot_name ~ px(110)) %>% 
    cols_align(columns = c(everything(), -plot_name), 'center') %>% 
    cols_align(columns = c(plot_name), 'left') %>% 
    gt_bars(ptile, labels = TRUE, fill = team_data$color, domain = 0:1, digits = 0) %>% 
    gt_bars(conf_ptile, labels = TRUE, fill = team_data$color, domain = 0:1, digits = 0) %>% 
    fmt_percent(columns = raw, rows = to_scale_percent, decimals = 1) %>% 
    fmt_number(columns = raw, rows = to_scale_number, decimals = 1) %>% 
    gt_add_divider(raw, color = 'black', include_labels = FALSE) %>% 
    cols_label(plot_name = 'Stat', raw = 'Value', ptile = 'National', conf_ptile = conf_group, natl_grade = 'Grade') %>% 
    # bold stat name and value
    tab_style(locations = cells_row_groups(), style = cell_text(weight = 'bold')) %>%
    # change group headings
    tab_style(locations = cells_row_groups(), style = list(cell_fill(color = 'grey90'),cell_text(font = google_font(font)))) %>%
    tab_style(locations = list(cells_title('title'), cells_source_notes()), style = cell_text(font = google_font(font))) %>% 
    tab_style(locations = cells_body(), style = cell_text(font = google_font(font), weight = 400)) %>% 
    tab_style(locations = cells_body(columns = c(raw, plot_name, natl_grade)), style = cell_text(font = google_font(font), weight = 'bold')) %>% 
    tab_style(locations = list(cells_column_labels(), cells_column_spanners()), 
              style = cell_text(font = google_font(font))) %>% 
    tab_spanner(columns = ends_with('ptile'), label = glue('Percentile relative to {pos_group} across...')) %>% 
    tab_options(data_row.padding = 3.5) %>% 
    # footnotes for specific stats
    tab_footnote(locations = cells_body(columns = plot_name, row = which(player_data$plot_name == '3FGA/FGA')),
                 footnote = "3FG attempts per 100 FGA") %>% 
    tab_footnote(locations = cells_body(columns = plot_name, row = which(player_data$plot_name == 'Off. Rating')),
                 footnote = "Offensive and defensive ratings represent points scored/allowed per 100 possessions") %>% 
    tab_footnote(locations = cells_body(columns = plot_name, row = which(player_data$plot_name == 'PORPAG')),
                 footnote = "PORPAG represents points above replacement player at that usage") %>% 
    tab_header(title = html(title_header)) %>% 
    tab_source_note(source_note = md('Data by cbbdata<br>Viz. + Analysis by @andreweatherman')) %>% 
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
    )
  
  return(table)
  
}

gtsave_extra(plot_player_report('Kyle Filipowski', 'Duke'), here('player_report', 'Filipowski_report.png'), zoom = 3)


player_data <- get_player('Kyle Filipowski', 'Duke', min_cutoff = 40, games_cutoff = 5)











