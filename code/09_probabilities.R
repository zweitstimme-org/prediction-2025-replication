### ----------------------------------------------------------
### Calculate Election Outcome Probabilities
### Authors: Lukas Stoetzer & Cornelius Erfort
### ----------------------------------------------------------


### 1. Configuration and Data Loading ----------------------

nsim <- 10000  # Number of simulations

# Load district forecast data
prediction_data_districts <- readRDS("output/prediction_data_districts.RDS")
district_reg_predictions <- readRDS("output/district_reg_predictions.RDS")

### 2. Load Latest Forecast -------------------------------

# Get most recent forecast file
forecast_files <- list.files("output", full.names = TRUE) %>% 
  str_subset("forecast_draws_")
latest_date <- max(ymd(str_extract(forecast_files, ".{10}(?=\\.rds)")))
forecast_files <- forecast_files[ymd(str_extract(forecast_files, ".{10}(?=\\.rds)")) == latest_date]
draws <- readRDS(forecast_files)

### 3. Process CDU/CSU Split -----------------------------

# Calculate CDU/CSU split factors based on 2021 results
csu_votes <- 7571313
cdu_votes <- 28743222
csu_factor <- csu_votes / (csu_votes + cdu_votes)
cdu_factor <- cdu_votes / (csu_votes + cdu_votes)

# Split CDU column into CDU and CSU
draws <- cbind(draws, csu = round(draws[, "cdu"] * csu_factor, 4))
draws[, "cdu"] <- round(draws[, "cdu"] * cdu_factor, 4)

### 4. Initialize Results Storage ------------------------

pred_probabilities <- data.frame()
grundmandat_counter <- c()

### 5. Calculate Probabilities ---------------------------

for (j in 1:nsim) {
  print(j)
  
  # Process district predictions
  district_draw <- cbind(pred = district_reg_predictions[, j], prediction_data_districts) %>%
    mutate(
      party = if_else(party == "cdu" & land == "BY", "csu", party),
      draw_winner = pred == max(pred),
      .by = wkr
    )
  
  # Get parties with at least 3 direct mandates
  grundmandat <- district_draw %>%
    filter(draw_winner) %>%
    dplyr::count(party) %>%
    filter(n >= 3) %>%
    pull(party)
  
  grundmandat_counter <- c(grundmandat_counter, grundmandat)
  
  # Get parties with at least 5% or 3 districts
  full_draw <- draws[j, ]
  draw <- full_draw[full_draw >= 0.05 | names(full_draw) %in% grundmandat]
  
  # Remove others category and normalize
  draw <- draw[names(draw) != "oth"]
  draw <- draw/sum(draw)
  
  # Store results
  pred_probabilities <- bind_rows(pred_probabilities, draw)
}

### 6. Process Results ----------------------------------

# Replace NA values with 0
pred_probabilities[is.na(pred_probabilities)] <- 0

# Create merged CDU/CSU column
pred_probabilities_merged <- pred_probabilities
pred_probabilities_merged$cdu_csu <- pred_probabilities$cdu + pred_probabilities$csu

### 7. Calculate Final Probabilities --------------------

pred_probabilities <- data.frame(
  # Hurdle probabilities (>5% or grundmandat)
  hurdle_lin = mean(pred_probabilities$lin > 0),
  hurdle_bsw = mean(pred_probabilities$bsw > 0),
  hurdle_fdp = mean(pred_probabilities$fdp > 0),
  hurdle_spd = mean(pred_probabilities$spd > 0),
  hurdle_cdu = mean(pred_probabilities$cdu > 0),
  hurdle_csu = mean(pred_probabilities$csu > 0),
  hurdle_gru = mean(pred_probabilities$gru > 0),
  hurdle_afd = mean(pred_probabilities$afd > 0),
  
  # Coalition probabilities
  block_min_afd_bsw = mean((pred_probabilities$bsw + pred_probabilities$afd) > 0.33),
  maj_cdu_csu_gru = mean((pred_probabilities$cdu + pred_probabilities$csu + pred_probabilities$gru) > 0.5),
  maj_cdu_csu_spd = mean((pred_probabilities$cdu + pred_probabilities$csu + pred_probabilities$spd) > 0.5),
  maj_cdu_csu_gru_spd = mean((pred_probabilities$cdu + pred_probabilities$csu + pred_probabilities$gru + pred_probabilities$spd) > 0.5),
  maj_cdu_csu_afd = mean((pred_probabilities$cdu + pred_probabilities$csu + pred_probabilities$afd) > 0.5),
  
  # Largest party probabilities
  prob_lin_largest = mean(pred_probabilities_merged$lin > apply(pred_probabilities_merged[, -which(names(pred_probabilities_merged) == "lin")], 1, max)),
  prob_bsw_largest = mean(pred_probabilities_merged$bsw > apply(pred_probabilities_merged[, -which(names(pred_probabilities_merged) == "bsw")], 1, max)),
  prob_fdp_largest = mean(pred_probabilities_merged$fdp > apply(pred_probabilities_merged[, -which(names(pred_probabilities_merged) == "fdp")], 1, max)),
  prob_spd_largest = mean(pred_probabilities_merged$spd > apply(pred_probabilities_merged[, -which(names(pred_probabilities_merged) == "spd")], 1, max)),
  prob_cdu_csu_largest = mean(pred_probabilities_merged$cdu_csu > apply(pred_probabilities_merged[, -which(names(pred_probabilities_merged) == "cdu_csu")], 1, max)),
  prob_gru_largest = mean(pred_probabilities_merged$gru > apply(pred_probabilities_merged[, -which(names(pred_probabilities_merged) == "gru")], 1, max)),
  prob_afd_largest = mean(pred_probabilities_merged$afd > apply(pred_probabilities_merged[, -which(names(pred_probabilities_merged) == "afd")], 1, max)),
  
  # Grundmandat probabilities
  grundmandat_lin = sum(grundmandat_counter == "lin")/nsim,
  grundmandat_bsw = sum(grundmandat_counter == "bsw")/nsim,
  grundmandat_fdp = sum(grundmandat_counter == "fdp")/nsim,
  grundmandat_spd = sum(grundmandat_counter == "spd")/nsim,
  grundmandat_cdu = sum(grundmandat_counter == "cdu")/nsim,
  grundmandat_csu = sum(grundmandat_counter == "csu")/nsim,
  grundmandat_gru = sum(grundmandat_counter == "gru")/nsim,
  grundmandat_afd = sum(grundmandat_counter == "afd")/nsim
)

### 8. Save Results ------------------------------------

saveRDS(pred_probabilities, "output/pred_probabilities.rds")


# Create a data frame with coalition names and rounded probabilities
coalition_probs <- data.frame(
  Coalition = c("CDU/CSU + Greens", "CDU/CSU + SPD", "CDU/CSU + AfD", "CDU/CSU + Greens + SPD"),
  Probability = round(100 * c(
    pred_probabilities$maj_cdu_csu_gru, 
    pred_probabilities$maj_cdu_csu_spd, 
    pred_probabilities$maj_cdu_csu_afd, 
    pred_probabilities$maj_cdu_csu_gru_spd
  ), 0)  %>% str_c("%") # Convert to percentages and round
)

# Use stargazer to create a LaTeX table
stargazer(coalition_probs, summary = FALSE, rownames = FALSE, 
          title = "Coalition Majority Probabilities", 
          align = TRUE, digits = 0, out = "output/tables/coalition_majority_probabilities.tex")
