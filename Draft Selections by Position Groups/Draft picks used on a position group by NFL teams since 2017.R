library(tidyverse)
library(rvest)
library(janitor)
library(gt)
library(gtExtras)
library(nflreadr)
library(prismatic)

# custom theme
theme_owen <- function () {
  theme_bw(base_size=11, base_family="Consolas") %+replace%
    theme(
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = 'floralwhite', color = "floralwhite")
    )
}

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
  filter(!position_category %in% c("K", "LS", "P", "SAF"))

# count position categories by team and side of the ball
df <- picks_df |> count(team, position_side, position_category)
sum(is.na(df))
names(df)

# overall frequency by position group
picks_df |> 
  group_by(draft_season, position_category) |> 
  summarize(count = n()) |> 
  ungroup() |> 
  mutate(draft_season = as.factor(draft_season),
         position_category = tidytext::reorder_within(position_category, -count, draft_season)) |> 
  ggplot(aes(x = position_category, y = count)) +
  geom_col(aes(fill = draft_season, color = after_scale(clr_darken(fill, 0.3)))) +
  scale_fill_manual(
    values = c("#004C54", "#002244", "#E31837", "#A71930", "#003594", "#E31837")
  ) + 
  tidytext::scale_x_reordered() +
  geom_text(aes(label = count), vjust = -0.5, family = "Consolas") +
  labs(x = "", y = "",
       title = "How often did NFL teams draft a position group?",
       subtitle = "2017-2022 NFL Drafts. Number of selections on top of each bar",
       caption = "Chart: @twain_w | Data: PFR\nOL = C, G, OL, OT, T | LB = ILB, LB, OLB | DL = DL, DT, NT") + 
  theme_bw(base_size=11, base_family="Consolas") +
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'none',
        panel.grid.major.x = element_blank(),
        plot.title.position = 'plot',
        plot.title = element_text(face = 'bold', size = 12)) +
  facet_wrap(draft_season ~., scales  = "free") +
  ylim(c(0, 52))

ggsave("positions_drafted_by_nfl.png", height = 7, width = 11, dpi = "retina") 
  
# create data frames for offense and defensive positons
off_df <- df |> 
  filter(position_side == "OFF") |> 
  select(-position_side) |> 
  pivot_wider(names_from = position_category, values_from = n) |> 
  mutate_at(-1, ~replace_na(.,0)) |> 
  mutate(total = rowSums(across(where(is.numeric)), na.rm=TRUE)) |> 
  mutate_at(vars(OL:WR), funs(p = ./total)) |> 
  arrange(-total) |> 
  mutate(row = row_number()) |> 
  left_join(nflfastR::teams_colors_logos |> select(team_abbr, team_logo_espn), 
            by = c("team" = "team_abbr")) |> 
  select(row, 
         team_logo_espn,
         "RB/FB_p", "QB_p", "OL_p", "TE_p", "WR_p",
         "RB/FB", "QB", "OL", "TE", "WR",
         total)

def_df <- df |> 
  filter(position_side == "DEF") |> 
  select(-position_side) |> 
  pivot_wider(names_from = position_category, values_from = n) |> 
  mutate_at(-1, ~replace_na(.,0)) |> 
  mutate(total = rowSums(across(where(is.numeric)), na.rm=TRUE)) |> 
  mutate_at(vars(`CB/DB`:S), funs(p = ./total)) |> 
  arrange(-total) |> 
  mutate(row = row_number()) |> 
  left_join(nflfastR::teams_colors_logos |> select(team_abbr, team_logo_espn), 
            by = c("team" = "team_abbr")) |> 
  select(row, 
         team_logo_espn,
         "DE_p", "DL_p", "LB_p", "S_p", "CB/DB_p",
         "DE", "DL", "LB", "S", "CB/DB",
         total)

# build a GT table
gt_fn <- function(x) {
  
  x |> 
    fmt_percent(columns = 3:7, 
                decimals = 0) |> 
    tab_style(
      style = list(
        cell_text(weight = 'bold')
      ),
      locations = cells_body(
        columns = c("row", "total")
      )) |> 
    cols_align(align = 'center', columns = c("team_logo_espn")) |> 
    cols_align(align = 'right', columns = 'row') |> 
    cols_label(
      total = md("Total<br>Picks"),
      team_logo_espn = html("<span style='color:blue'>POSITION<br />GROUPS &#8594;</span>"),
      row = ""
    ) |> 
    gt_img_rows(team_logo_espn, height = 35) |> 
    gt_hulk_col_numeric(total, domain = c(14, 33)) |> 
    data_color(
      columns = 3:7,
      colors = scales::col_numeric(
        palette = paletteer::paletteer_c(
          palette = "ggthemes::Orange-Blue Light Diverging",
          n = 10,
          direction = 1
        ) |>  as.character(),
        domain = c(0, 0.5), 
        na.color = "#00441BFF"
      )
    ) |> 
    tab_style(
      style = list(
        cell_borders(
          side = c('right', 'left'),
          color = 'black', 
          weight = px(2)
        )),
      locations = cells_body(columns = total)) |> 
    cols_width(
      3:7 ~ px(50)
    ) |> 
    gt_theme_538()
  
}

# offense gt table
build_off_gt_table <- function(x) {
  
  gt(x) |> 
    gt_merge_stack(col1 = "RB/FB_p", col2 = "RB/FB", 
                   font_weight = c("normal", "normal"),
                   palette = c("black", "black")) |> 
    gt_merge_stack(col1 = "QB_p", col2 = "QB", 
                   font_weight = c("normal", "normal"),
                   palette = c("black", "black")) |> 
    gt_merge_stack(col1 = "OL_p", col2 = "OL", 
                   font_weight = c("normal", "normal"),
                   palette = c("black", "black")) |> 
    gt_merge_stack(col1 = "TE_p", col2 = "TE", 
                   font_weight = c("normal", "normal"),
                   palette = c("black", "black")) |> 
    gt_merge_stack(col1 = "WR_p", col2 = "WR", 
                   font_weight = c("normal", "normal"),
                   palette = c("black", "black")) |> 
    tab_spanner(label = md("**OFFENSE**"), 
                columns = 3:7) |> 
    cols_label("RB/FB_p" = "RB/FB", 
               "QB_p" = "QB", 
               "OL_p" = "OL",
               "TE_p" = "TE",
               "WR_p" = "WR"
    ) |> 
    gt_fn()
  
}

(tab1 <- off_df |> 
    slice(1:16) |> 
    build_off_gt_table() |> 
    tab_source_note(
      source_note = md("Table: @twain_w | Data: PFR")
    ) |> 
    tab_footnote(
      footnote = md("OL = C, G, OL, OT, T"),
      locations = cells_column_labels(columns = "OL_p")
    )
)

(tab2 <- off_df |> 
    slice(17:32) |> 
    build_off_gt_table()
)

obj <- htmltools::div(html("<span style='font-size:15pt; font-weight:bold'><center>Positional Draft Capital Spent, Offense<center></span>"),
                      html("<span style='font-size:9.5pt; font-weight:normal'><center>How often did each team draft an offensive position group? 2017-2022 NFL Drafts<br />Subscripted Number: # of picks used on a group<center></span>"),
                      gt_two_column_layout(list(tab1, tab2)))

gtsave_extra(obj, filename = "off_table.png", vheight = 1800, vwidth = 850)

# defense gt table
build_def_gt_table <- function(x) {
  
  gt(x) |> 
    gt_merge_stack(col1 = "DE_p", col2 = "DE", 
                   font_weight = c("normal", "normal"),
                   palette = c("black", "black")) |> 
    gt_merge_stack(col1 = "DL_p", col2 = "DL", 
                   font_weight = c("normal", "normal"),
                   palette = c("black", "black")) |> 
    gt_merge_stack(col1 = "LB_p", col2 = "LB", 
                   font_weight = c("normal", "normal"),
                   palette = c("black", "black")) |> 
    gt_merge_stack(col1 = "S_p", col2 = "S", 
                   font_weight = c("normal", "normal"),
                   palette = c("black", "black")) |> 
    gt_merge_stack(col1 = "CB/DB_p", col2 = "CB/DB", 
                   font_weight = c("normal", "normal"),
                   palette = c("black", "black")) |> 
    tab_spanner(label = md("**DEFENSE**"), 
                columns = 3:7) |> 
    cols_label("DE_p" = "DE", 
               "DL_p" = "DL", 
               "LB_p" = "LB",
               "S_p" = "S",
               "CB/DB_p" = "CB/DB"
    ) |> 
    gt_fn()
  
}

(tab3 <- def_df |> 
    slice(1:16) |> 
    build_def_gt_table() |> 
    tab_source_note(
      source_note = md("Table: @twain_w | Data: PFR")
    ) |> 
    tab_footnote(
      footnote = md("LB = ILB, LB, OLB"),
      locations = cells_column_labels(columns = "LB_p")
    ) |> 
    tab_footnote(
      footnote = md("DL = DL, DT, NT"),
      locations = cells_column_labels(columns = "DL_p")
    )
)

(tab4 <- def_df |> 
    slice(17:32) |> 
    build_def_gt_table()
)


obj2 <- htmltools::div(html("<span style='font-size:15pt; font-weight:bold'><center>Positional Draft Capital Spent, Defense<center></span>"),
                      html("<span style='font-size:9.5pt; font-weight:normal'><center>How often did each team draft a defensive position group? 2017-2022 NFL Drafts<br />Subscripted Number: # of picks used on a group<center></span>"),
                      gt_two_column_layout(list(tab3, tab4)))

gtsave_extra(obj2, filename = "def_table.png", vheight = 1800, vwidth = 850)




