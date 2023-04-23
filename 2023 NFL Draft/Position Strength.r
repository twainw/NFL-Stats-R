library(tidyverse)
`%nin%` = Negate(`%in%`)
library(paletteer)

# Scrape nflmockdraftdatabase consensus board from
# https://www.nflmockdraftdatabase.com
load_nflmockdraftdatabase_consensus_board <- function(year){
  cli::cli_progress_step("Loading {.val {year}}. Please be patient, the parser takes a while.")
  raw <- glue::glue("https://www.nflmockdraftdatabase.com/big-boards/{year}/consensus-big-board-{year}") |>
    rvest::read_html()
  
  mock_list <- raw |>
    rvest::html_elements(xpath = "//*[@class='mock-list-item']")
  
  pick_no <- mock_list |>
    rvest::html_elements(xpath = "//div[@class='left-container']//div[contains(concat(' ',normalize-space(@class),' '),' pick-number ')]") |>
    rvest::html_text()
  
  peak <- mock_list |>
    rvest::html_elements(xpath = "//div[@class='peak']//span") |>
    rvest::html_text()
  
  player <- mock_list |>
    rvest::html_elements(xpath = "//div[contains(concat(' ',normalize-space(@class),' '),' player-name ')]") |>
    rvest::html_text()
  
  college_details <- mock_list |>
    rvest::html_elements(xpath = "//div[@class='player-details college-details']")|>
    rvest::html_text()
  
  position <- college_details |>
    stringr::str_split_i("\\|", 1) |>
    stringr::str_trim()
  
  college <- college_details |>
    stringr::str_split_i("\\|", 2) |>
    stringr::str_trim() |>
    stringr::str_split_i("#|[:digit:]|--", 1)
  
  projection <- college_details |>
    stringr::str_split_i("\\|", 2) |>
    stringr::str_trim() |>
    stringr::str_split_i("#", 2)
  
  consensus_board <- tibble::tibble(
    current_rank = as.integer(pick_no),
    player = player,
    position = position,
    college = college,
    projected_pick = as.integer(projection),
    highest_rank = as.integer(peak)
  )
  consensus_board
}

# EDA ---------------------------------------------------------------------

# Load the 2023 Consensus Big Board (Board)
big_board_2023 <- load_nflmockdraftdatabase_consensus_board(2023)

# what position groups are in Board? 
big_board_2023 |> group_by(position) |> tally()

# remove special teams
board_df <- big_board_2023 |> 
  filter(position %nin% c("K", "LS", "P"))

board_df |> group_by(position) |> tally() # I wish the LBs were seperated into MLB and OLB but that's ok. 
# I like that IOL and OT's are seperated from OL. 
# gonna assume that DL and Edge are distinct position groups. So DL should include NTs, DTs, or IDL in general. 
# whereas Edge is outside defenders. 

# any missing values in higher or current rank? Or anywhere?
sapply(board_df, function(x) sum(is.na(x)))

## projected pick after round 2 is all NAs. we can workaround that by 
## assuming that each team will draft the best player per the consensus board. 
## so will use current rank as a threshold if we want to look at top 100 or top 250 guys

# What are the strongest position groups in this draft? -------------------

## which group has the most top-ranked top 10-15 prospects?

board_df |> 
  group_by(position) |> 
  tally()

position_strength_df <- board_df |> 
  mutate(current_rank_category = case_when(
    current_rank <= 15 ~ "top1_15",
    current_rank <= 32 ~ "top16_32",
    current_rank <= 64 ~ "top33_64",
    current_rank <= 100 ~ "top65_100",
    current_rank <= 175 ~ "top101_175",
    current_rank <= 259 ~ "top176_259",
    TRUE ~ "top260+")) |> 
  group_by(position, current_rank_category) |> 
  summarize(count = n()) |> 
  ungroup() |> 
  pivot_wider(names_from = current_rank_category, values_from = count) |> 
  mutate(across(2:last_col(), .fns = ~replace_na(.,0))) |>
  mutate(total_players = rowSums(across(where(is.numeric)))) |> 
  mutate(total_top_100_players = top1_15 + top16_32 + top33_64 + top65_100) |>
  mutate(total_top_64_players = top1_15 + top16_32 + top33_64) |> 
  arrange(-total_top_100_players)

## chart
library(ggchicklet)

theme_owen <- function () { 
  theme_minimal(base_size=12) %+replace% 
    theme(
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = 'floralwhite', color = 'floralwhite')
    )
}

position_strength_df |> 
  pivot_longer(-position, names_to = "category", values_to = "value") |> 
  filter(category %in% c("top1_15", "top16_32", "top33_64", "top65_100")) |> 
  ggplot(aes(x = reorder(position, -value, sum), y = value, fill = category, label = value, color = 'black')) +
  geom_chicklet(position = ggplot2::position_stack(reverse = FALSE)) +
  scale_color_identity() +
  scale_fill_manual(values = c("#005B9F", "#00B9BC", "#86D9C2", "#F5F8EA")) +
  scale_y_continuous(breaks = seq(1, 16, 1)) +
  geom_text(aes(label = value, group = category), size = 4, position = position_stack(vjust = 0.5)) + 
  theme_owen() +
  labs(x = "Position Group",
       y = "Number of Players") +
  guides(fill=guide_legend(
    keywidth= .5,
    keyheight= .2,
    default.unit="inch", 
    label.position = 'bottom', 
    nrow = 1) 
  )




















