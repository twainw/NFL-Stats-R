library(tidyverse)
library(rvest)
library(janitor)
library(gt)
library(gtExtras)
library(nflreadr)
library(prismatic)

# draft values data
draft_value <- nflreadr::csv_from_url("https://github.com/nflverse/nfldata/raw/master/data/draft_values.csv")

# function to pull picks data from 2017 from PFR
get_data <- function(s) {
  
  url <- paste0("https://www.pro-football-reference.com/years/", s, "/draft.htm")
  
  df <- url |> 
    read_html() |> 
    html_element(css = "#all_drafts") |> 
    html_table() |> 
    row_to_names(row_number = 1) |> 
    clean_names() |> 
    select(rnd:age) |> 
    filter(rnd != "Rnd") |> 
    mutate(draft_season = s) |> 
    mutate(age = parse_number(age),
           rnd = parse_number(rnd),
           pick = parse_number(pick))
  
  return(df)
  
}

# pull data
picks <- map_df(seq(2017, 2022, 1), get_data) |> 
  mutate(team = clean_team_abbrs(tm)) |> 
  select(-tm)

# sanity checks
table(picks$draft_season) # number of picks by season matches what I see on PFR
sum(is.na(picks)) # no NAs
table(picks$pos)

# calculate number of selections by position for each team
picks_df <- picks |> 
  mutate(position_category = case_when(
    pos %in% c("DB", "CB") ~ "CB/DB",
    pos %in% c("FB", "RB") ~ "RB/FB", 
    pos %in% c("C", "G", "OL", "OT", "T") ~ "OL",
    pos %in% c("ILB", "LB", "OLB") ~ "LB",
    pos %in% c("DL", "DT", "NT") ~ "DL",
    TRUE ~ pos
  )) |> 
  mutate(position_side = case_when(
    position_category %in% c("DE", "DL", "LB", "S", "CB/DB") ~ "DEF",
    position_category %in% c("RB/FB", "QB", "OL", "TE", "WR") ~ "OFF"
  )) |> 
  mutate(day = case_when(
    rnd == 1 ~ "day_1",
    rnd %in% c(2, 3) ~ "day_2", 
    TRUE ~ "day_3"
  )) |>
  filter(!position_category %in% c("K", "LS", "P", "SAF")) |> 
  left_join(draft_value |> select(pick, otc, pff), 
            by = "pick")

sum(is.na(picks_df$otc))
sum(is.na(picks_df$pff))

# count position categories by team and side of the ball
df <- picks_df |> 
  group_by(day, team, position_side, position_category) |> 
  summarize(otc_total_value = sum(otc), 
            n = n()) |> 
  ungroup()

sum(is.na(df))
names(df)

# create data frames for offense and defensive positons
off_df <- df |> 
  filter(position_side == "OFF") |> 
  select(-position_side) |> 
  pivot_wider(names_from = position_category, values_from = n:otc_total_value) |> 
  mutate_at(vars(3:last_col()), ~replace_na(.,0)) |>
  mutate(total_value = rowSums(across(c(`otc_total_value_QB`:`otc_total_value_RB/FB`)))) |> 
  left_join(nflfastR::teams_colors_logos |> select(team_abbr, team_logo_espn), 
            by = c("team" = "team_abbr"))

def_df <- df |> 
  filter(position_side == "DEF") |> 
  select(-position_side) |> 
  pivot_wider(names_from = position_category, values_from = n:otc_total_value) |> 
  mutate_at(vars(3:last_col()), ~replace_na(.,0)) |>
  mutate(total_value = rowSums(across(c(`otc_total_value_LB`:`otc_total_value_S`)))) |> 
  left_join(nflfastR::teams_colors_logos |> select(team_abbr, team_logo_espn), 
            by = c("team" = "team_abbr"))

# GT function
gt_fn <- function(x) {
  
  x |>
    fmt_number(
      columns = 3:last_col(), 
      decimals = 0, 
      sep_mark = ","
    ) |> 
    tab_style(
      style = list(
        cell_text(weight = 'bold')
      ),
      locations = cells_body(
        columns = c("row", "total_value")
      )) |> 
    cols_align(align = 'center', columns = c("team_logo_espn")) |> 
    cols_align(align = 'right', columns = 'row') |> 
    cols_label(
      total_value = md("Total<br>Value"),
      team_logo_espn = html("<span style='color:blue'>POSITION<br />GROUP &#8594;</span>"),
      row = ""
    ) |> 
    gt_img_rows(team_logo_espn, height = 35) |> 
    gt_hulk_col_numeric(total_value, domain = c(1100, 12000)) |> 
    data_color(
      columns = 3:7,
      colors = scales::col_numeric(
        palette = paletteer::paletteer_c(
          palette = "ggthemes::Orange-Blue Light Diverging",
          n = 10,
          direction = 1
        ) |>  as.character(),
        domain = c(0, 5500), 
        na.color = "#FFFFFF"
      )
    ) |> 
    tab_style(
      style = list(
        cell_borders(
          side = c('right', 'left'),
          color = 'black', 
          weight = px(2)
        )),
      locations = cells_body(columns = total_value)) |> 
    cols_width(
      3:7 ~ px(50)
    ) |> 
    gt_theme_538()
  
}

# offense gt table function
build_off_gt_table <- function(x) {
  
  gt(x) |> 
    gt_merge_stack(col1 = "otc_total_value_RB/FB", col2 = "n_RB/FB", 
                   font_weight = c("normal", "normal"),
                   palette = c("black", "black")) |> 
    gt_merge_stack(col1 = "otc_total_value_QB", col2 = "n_QB", 
                   font_weight = c("normal", "normal"),
                   palette = c("black", "black")) |> 
    gt_merge_stack(col1 = "otc_total_value_OL", col2 = "n_OL", 
                   font_weight = c("normal", "normal"),
                   palette = c("black", "black")) |> 
    gt_merge_stack(col1 = "otc_total_value_TE", col2 = "n_TE", 
                   font_weight = c("normal", "normal"),
                   palette = c("black", "black")) |> 
    gt_merge_stack(col1 = "otc_total_value_WR", col2 = "n_WR", 
                   font_weight = c("normal", "normal"),
                   palette = c("black", "black")) |> 
    tab_spanner(label = md("**OFFENSE**"), 
                columns = 3:7) |> 
    cols_label("otc_total_value_RB/FB" = "RB/FB", 
               "otc_total_value_QB" = "QB", 
               "otc_total_value_OL" = "OL",
               "otc_total_value_TE" = "TE",
               "otc_total_value_WR" = "WR"
    ) |> 
    gt_fn()
  
}

# defense gt table function
build_def_gt_table <- function(x) {
  
  gt(x) |> 
    gt_merge_stack(col1 = "otc_total_value_DE", col2 = "n_DE", 
                   font_weight = c("normal", "normal"),
                   palette = c("black", "black")) |> 
    gt_merge_stack(col1 = "otc_total_value_DL", col2 = "n_DL", 
                   font_weight = c("normal", "normal"),
                   palette = c("black", "black")) |> 
    gt_merge_stack(col1 = "otc_total_value_LB", col2 = "n_LB", 
                   font_weight = c("normal", "normal"),
                   palette = c("black", "black")) |> 
    gt_merge_stack(col1 = "otc_total_value_S", col2 = "n_S", 
                   font_weight = c("normal", "normal"),
                   palette = c("black", "black")) |> 
    gt_merge_stack(col1 = "otc_total_value_CB/DB", col2 = "n_CB/DB", 
                   font_weight = c("normal", "normal"),
                   palette = c("black", "black")) |> 
    tab_spanner(label = md("**DEFENSE**"), 
                columns = 3:7) |> 
    cols_label("otc_total_value_DE" = "DE", 
               "otc_total_value_DL" = "DL", 
               "otc_total_value_LB" = "LB",
               "otc_total_value_S" = "S",
               "otc_total_value_CB/DB" = "CB/DB"
    ) |> 
    gt_fn()
  
}

# function to build all 4 tables (two images) for any day's picks
build_a_days_table <- function(off_df, def_df, day_picks, day_title) {
  
  ## local df for offense
  func_off_df <- off_df |> 
    filter(day == day_picks) |> 
    select(-day) |> 
    arrange(-total_value) |> 
    mutate(row = row_number()) |> 
    select(row, 
           team_logo_espn,
           "otc_total_value_RB/FB", "otc_total_value_QB", 
           "otc_total_value_OL", "otc_total_value_TE", "otc_total_value_WR",
           "n_RB/FB", "n_QB", "n_OL", "n_TE", "n_WR",
           total_value)
  
  ## 1st offensive table - first 16 teams
  tab1 <- func_off_df |> 
    slice(1:16) |> 
    build_off_gt_table() |> 
    tab_source_note(
      source_note = md("Table: @twain_w | Data: PFR")
    ) |> 
    tab_footnote(
      footnote = md("OL = C, G, OL, OT, T"),
      locations = cells_column_labels(columns = "otc_total_value_OL")
    ) |> 
    tab_footnote(
      footnote = md("From Over The Cap's Draft Value Chart"),
      locations = cells_column_labels(columns = "total_value")
    )
  
  ## 2nd offensive table - bottom 16 teams
  tab2 <- func_off_df |> 
    slice(17:32) |> 
    build_off_gt_table()
  
  ## create an html object with both tables
  obj_off <- htmltools::div(html(paste0("<span style='font-size:15pt; font-weight:bold'><center>Positional ", day_title, " Draft Value Spent, Offense<center></span>")),
                            html("<span style='font-size:9.5pt; font-weight:normal'><center>Over The Cap Draft Value of picks each team spent on an offensive position groups. 2017-2022 NFL Drafts<br />Subscripted Number: # of picks used on a group<center></span>"),
                            gt_two_column_layout(list(tab1, tab2)))
  
  ## save 
  gtsave_extra(obj_off, filename = paste0("off_table_", day_picks, ".png"), vheight = 1800, vwidth = 850)
  
  ## local df for defense 
  func_def_df <- def_df |> 
    filter(day == day_picks) |> 
    select(-day) |> 
    arrange(-total_value) |> 
    mutate(row = row_number()) |> 
    select(row, 
           team_logo_espn,
           "otc_total_value_DE", "otc_total_value_DL", "otc_total_value_LB", 
           "otc_total_value_S", "otc_total_value_CB/DB",
           "n_DE", "n_DL", "n_LB", "n_S", "n_CB/DB",
           total_value)
  
  ## 1st defensive table - first 16 teams
  tab3 <- func_def_df |> 
    slice(1:16) |> 
    build_def_gt_table() |> 
    tab_source_note(
      source_note = md("Table: @twain_w | Data: PFR")
    ) |> 
    tab_footnote(
      footnote = md("LB = ILB, LB, OLB"),
      locations = cells_column_labels(columns = "otc_total_value_LB")
    ) |> 
    tab_footnote(
      footnote = md("DL = DL, DT, NT"),
      locations = cells_column_labels(columns = "otc_total_value_DL")
    ) |> 
    tab_footnote(
      footnote = md("From Over The Cap's Draft Value Chart"),
      locations = cells_column_labels(columns = "total_value")
    )
  
  ## 2nd defensive table - bottom 16 teams
  tab4 <- func_def_df |> 
    slice(17:32) |> 
    build_def_gt_table()
  
  ## create an html object with both tables
  obj_def <- htmltools::div(html(paste0("<span style='font-size:15pt; font-weight:bold'><center>Positional ", day_title, " Draft Value Spent, Defense<center></span>")),
                            html("<span style='font-size:9.5pt; font-weight:normal'><center>Over The Cap Draft Value of picks each team spent on an defensive position groups. 2017-2022 NFL Drafts<br />Subscripted Number: # of picks used on a group<center></span>"),
                            gt_two_column_layout(list(tab3, tab4)))
  
  ## save
  gtsave_extra(obj_def, filename = paste0("def_table_", day_picks, ".png"), vheight = 1800, vwidth = 850)  
}

# day 1 picks
build_a_days_table(off_df, def_df, "day_1", "Day-1")

# day 2 picks
build_a_days_table(off_df, def_df, "day_2", "Day-2")

# day 3 picks
build_a_days_table(off_df, def_df, "day_3", "Day-3")

