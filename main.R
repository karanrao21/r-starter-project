# main.R

# --- 1. Load Data ---
stats_path <- "data/stats.csv"
stats <- read.csv(stats_path, header = TRUE, stringsAsFactors = FALSE)
cat("Loaded data from", stats_path, "\n")
print(head(stats))

# --- 2. Data Cleaning: Split Name Column ---
# The first column is "last_name, first_name"
if ("last_name..first_name." %in% names(stats)) {
  stats$last_name <- sapply(strsplit(stats$last_name..first_name., ", "), `[`, 1)
  stats$first_name <- sapply(strsplit(stats$last_name..first_name., ", "), `[`, 2)
  stats$last_name..first_name. <- NULL
}

# --- 3. Convert Numeric Columns ---
numeric_cols <- c("player_id", "year", "pa", "k_percent", "bb_percent", "woba", 
                  "xwoba", "sweet_spot_percent", "barrel_batted_rate", 
                  "hard_hit_percent", "avg_best_speed", "avg_hyper_speed", 
                  "whiff_percent", "swing_percent")
for (col in numeric_cols) {
  if (col %in% names(stats)) {
    # Remove quotes and convert to numeric
    stats[[col]] <- as.numeric(gsub('"', '', stats[[col]]))
  }
}

cat("\nSummary of stats data:\n")
print(summary(stats))

# --- 4. Define the Filtering Function ---
get_player_ids_whiff <- function(data, whiff_percent_min, whiff_percent_max) {
  filtered_data <- data[data$whiff_percent >= whiff_percent_min & 
                          data$whiff_percent <= whiff_percent_max, ]
  return(filtered_data$player_id)
}

# --- 5. Set Whiff Percent Range and Get Player IDs ---
whiff_percent_min <- 10
whiff_percent_max <- 17.8
player_ids_whiff <- get_player_ids_whiff(stats, whiff_percent_min, whiff_percent_max)
cat("\nPlayer IDs with whiff_percent between", whiff_percent_min, "and", 
    whiff_percent_max, ":\n")
print(player_ids_whiff)

# --- 6. Match IDs to Last Names and Sort by Whiff Percent ---
matching_players <- stats[stats$player_id %in% player_ids_whiff, ]
sorted_players <- matching_players[order(matching_players$whiff_percent), ]

cat("\nSorted players (least to most whiff):\n")
#print(colnames(sorted_players))
print(sorted_players[, c("player_id", "last_name..first_name", "whiff_percent", 
                         "woba")])
