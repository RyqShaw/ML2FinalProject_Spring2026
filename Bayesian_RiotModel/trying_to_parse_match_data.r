library(dplyr)
library(tidyr)
library(readr)

# Loading data
raw <- read_csv("../Riot/match_data.csv", show_col_types = FALSE)
colnames(raw)[1] <- "player_id"

long_raw <- raw %>%
  pivot_longer(-player_id, names_to = "match_slot", values_to = "match_str") %>%
  filter(!is.na(match_str))

# regex parsing of the features: helpers 
extract_int <- function(s, key) {
  m <- regmatches(s, regexpr(paste0("'", key, "':\\s*(\\d+)"), s))
  if (length(m) == 0 || m == "") return(NA_integer_)
  as.integer(sub(paste0("'", key, "':\\s*"), "", m))
}

extract_bool <- function(s, key) {
  m <- regmatches(s, regexpr(paste0("'", key, "':\\s*(True|False)"), s))
  if (length(m) == 0 || m == "") return(NA)
  grepl("True", m)
}

extract_datetime <- function(s) {
  m <- regmatches(s, regexpr("datetime\\.datetime\\((\\d+),\\s*(\\d+),\\s*(\\d+),\\s*(\\d+),\\s*(\\d+),\\s*(\\d+)", s))
  if (length(m) == 0 || m == "") return(as.POSIXct(NA))
  nums <- as.integer(strsplit(gsub("[^0-9,]", "", m), ",")[[1]])
  as.POSIXct(sprintf("%04d-%02d-%02d %02d:%02d:%02d",
                     nums[1], nums[2], nums[3], nums[4], nums[5], nums[6]), tz = "UTC")
}

# parsing json data grabbing what I need
parse_match <- function(s) {
  tibble(
    time_played = extract_int(s, "timePlayed"),
    win = extract_bool(s, "win"),
    early_surrender = extract_bool(s, "gameEndedInEarlySurrender"),
    game_start = extract_datetime(s)
  )
}

parsed_df <- bind_rows(lapply(long_raw$match_str, parse_match))

matches <- bind_cols(
  long_raw %>% select(player_id, match_slot),
  parsed_df
)

# make a quality to find if they are just rage quitting
matches <- matches %>%
  mutate(is_remake = time_played < 180)

# Player Summary to work with
match_data <- matches %>% arrange(player_id, game_start)

player_summary <- match_data %>%
  filter(!is_remake) %>%
  group_by(player_id) %>%
  summarise(
    n_games = n(),
    mean_time_played = mean(time_played, na.rm = TRUE),
    win_rate = mean(win, na.rm = TRUE),
    .groups = "drop"
  )

# Output to work with
write_csv(player_summary, "player_engagement_summary.csv")