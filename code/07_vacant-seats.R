### ----------------------------------------------------------
### Calculate Vacant Seats in German Parliament
### Authors: Lukas Stoetzer & Cornelius Erfort
### ----------------------------------------------------------


### 1. Configuration and Data Loading ------------------------

nsim <- 10000  # Number of simulations

# Load required data
btw_candidates_1983_2025 <- read.csv2("data/btw_candidates_1983-2025.csv", stringsAsFactors = FALSE)
prediction_data_districts <- readRDS("output/prediction_data_districts.rds")
district_reg_predictions <- readRDS("output/district_reg_predictions.rds")

# Get votes from last election
land_Z <- btw_candidates_1983_2025 %>% 
  filter(incumbent_party == 1 & election %in% c(2021)) %>% 
  group_by(election, land) %>% 
  summarise(valid_Z_l1 = sum(valid_Z_l1), 
            valid_Z = sum(valid_Z)) %>% 
  ungroup() %>% 
  arrange(election)

### 2. Load Latest Forecast --------------------------------

# Get most recent forecast file
forecast_files <- list.files("output", full.names = TRUE) %>% 
  str_subset("forecast_draws_")
latest_date <- max(ymd(str_extract(forecast_files, ".{10}(?=\\.rds)")))
forecast_files <- forecast_files[ymd(str_extract(forecast_files, ".{10}(?=\\.rds)")) == latest_date]
draws <- readRDS(forecast_files)

### 3. Process CDU/CSU Split -------------------------------

# Calculate CDU/CSU split factors based on 2021 results
csu_votes <- 7571313
cdu_votes <- 28743222
csu_factor <- csu_votes / (csu_votes + cdu_votes)
cdu_factor <- cdu_votes / (csu_votes + cdu_votes)

# Split CDU column into CDU and CSU
draws <- cbind(draws, csu = round(draws[, "cdu"] * csu_factor, 4))
draws[, "cdu"] <- round(draws[, "cdu"] * cdu_factor, 4)

### 4. Process Vote Estimates -----------------------------

# Get shares per party and wkr
votes_est <- btw_candidates_1983_2025 %>% 
  filter(election %in% c(2021)) %>% 
  mutate(
    resp_Z = resp_Z %>% str_replace(",", ".") %>% as.numeric,
    party = substr(partei, 1, 3) %>% tolower(),
    party = if_else(party == "and", "oth", party)
  ) %>% 
  dplyr::select(land, wkr, party, valid_Z, resp_Z) %>% 
  filter(!is.na(valid_Z))

# Split Left party votes between Linke and BSW
lin_est <- votes_est %>% 
  filter(party == "lin") %>% 
  mutate(resp_Z = resp_Z/2)
bsw_est <- lin_est %>% 
  mutate(party = "bsw")

votes_est <- bind_rows(
  filter(votes_est, party != "lin"),
  lin_est,
  bsw_est
) %>% 
  as.data.frame()

### 5. Calculate Proportional Swing -----------------------

# Past election results at federal level
res_el <- c(
  cdu = 0.1890, spd = 0.2574, afd = 0.1034, 
  fdp = 0.1146, lin = 0.0489/2, gru = 0.1475, 
  csu = 0.0517, bsw = 0.0489/2, oth = 0.0875
)

# Reorder like draws
res_el <- res_el[colnames(draws)]

# Calculate swing
sim.swing <- -sweep(-draws, 2, -res_el)
sim.prop.swing <- t(apply(sim.swing, 1, function(x) x / res_el))

### 6. Calculate Predicted Votes --------------------------

# Initialize prediction matrix
zs_pred <- matrix(0, nrow = nrow(votes_est), ncol = nrow(sim.swing))

# Calculate predictions using vectorized operations
zs_pred <- t(apply(votes_est, 1, function(row) {
  party <- row["party"]
  resp_Z <- as.numeric(row["resp_Z"])
  if (party %in% colnames(sim.prop.swing)) {
    return(resp_Z + sim.prop.swing[, party] * resp_Z)
  } else {
    return(rep(0, ncol(sim.prop.swing)))
  }
}))

### 7. Process Vacant Seats ------------------------------

vacant_seats <- data.frame()

for (j in 1:nsim) {
  print(j)
  
  # Find parties to take into account
  
  # Draw from the district forecast
  district_draw <- cbind(pred = district_reg_predictions[, j], prediction_data_districts)
  district_draw$party[district_draw$party == "cdu" & district_draw$land == "BY"] <- "csu"
  district_draw <- district_draw %>% group_by(wkr) %>% mutate(draw_winner = (pred == max(pred)))
  
  # Get parties which win at least 4 districts
  (grundmandat <- table((district_draw %>% filter(draw_winner))$party) %>% as.data.frame %>% filter(Freq >= 3) %>% pull(Var1) %>% as.character())
  
  
  # Rows are rows from votes_est, cols are per draw
  votes_est$zs_pred <- zs_pred[, j]
  votes_est$pred_Z_abs <- round(votes_est$valid_Z * votes_est$zs_pred, 0) # For now take only second column
  
  # Get parties which win at least 4 districts
  # Get parties which win at least 4 districts
  
  # Get parties with at least 5% (or 3 districts? Take CDU?)
  
  full_draw <- draws[j, ]
  draw <- full_draw[(full_draw >= 0.05) | colnames(draws) %in% grundmandat]
  
  # Remove oth
  draw <- draw[names(draw) != "oth"]
  
  # Make percentages sum to 1
  draw <- draw/sum(draw)
  
  # Estimated list vote per party
  (listvote_party <- round(draw*sum(land_Z$valid_Z), 0))
  
  # Starting divisor
  (divisor <- sum(land_Z$valid_Z)/630) # 73717.5
  # (divisor <- sum(votes_est$pred_Z_abs)/630) # 90453))
  
  # Estimated seats per party
  (seats <- round(listvote_party/divisor, 0))
  
  (seats_sum <- round(seats %>% sum))
  
  # If sum is > 630 -> increase divisor, if sum is < 630 -> reduce divisor
  
  
  while(seats_sum != 630) {
    
    if (seats_sum > 630) {
      
      (divisor_cand <- c(listvote_party / (seats - .5), listvote_party / (seats - 1.5)))
      
      # Only positive divisors
      divisor_cand <- divisor_cand[divisor_cand > 0]
      
      # Choose new divisor between smallest and second smallest candidate
      (divisor_min <- sort(divisor_cand)[1])
      (divisor_max <- sort(divisor_cand)[2])
      
      
    } else {
      
      (divisor_cand <- c(listvote_party / (seats + .5), listvote_party / (seats + 1.5)))
      
      # Only positive divisors
      divisor_cand <- divisor_cand[divisor_cand > 0]
      
      # Choose new divisor between smallest and second smallest candidate
      (divisor_min <- sort(divisor_cand, decreasing = T)[1])
      (divisor_max <- sort(divisor_cand, decreasing = T)[2])
      
    }
    
    # New divisor
    (divisor <- (divisor_min + divisor_max)/2)
    (seats <- round(listvote_party/divisor, 0))
    (seats_sum <- round(seats %>% sum))
    
  }
  
  
  ##############################################
  # List votes per party and state (proportional swing?)
  ##############################################
  
  prediction_subset <- votes_est %>% group_by(party) %>% dplyr::select(party, pred_Z_abs) %>%
    summarise(pred_Z_abs = sum(pred_Z_abs)) %>% pivot_wider(names_from = "party", values_from = "pred_Z_abs") %>% as.matrix
  
  
  # For each party in
  
  seats_states <- data.frame()
  
  for (seats_party in names(seats)) {
    
    print(seats_party)
    
    # if(seats_party == "fdp") next
    
    # Number of votes
    prediction_subset[colnames(prediction_subset) == seats_party]
    
    # Number of seats
    seats[seats_party]
    
    # Divisor
    (this_divisor <- prediction_subset[colnames(prediction_subset) == seats_party] / seats[seats_party])
    
    party_est <- votes_est %>% group_by(land, party) %>% 
      summarise(pred_Z_abs = sum(pred_Z_abs)) %>% filter(!is.na(pred_Z_abs)) %>% 
      filter(party == seats_party) %>% 
      mutate(seats = pred_Z_abs / this_divisor,
             seats_round = round(pred_Z_abs / this_divisor, 0)) %>% suppressMessages()
    
    sum(party_est$seats_round)
    
    counter <- 0 # Stop if there is no solution
    
    while(sum(party_est$seats_round) != seats[seats_party] & counter < 10) {
      if(sum(party_est$seats_round) > seats[seats_party]) {
        
        (divisor_cand <- c(party_est$pred_Z_abs / (party_est$seats_round - .5), party_est$pred_Z_abs / (party_est$seats_round - 1.5)) %>% unique)
        
        # Only positive divisors
        (divisor_cand <- divisor_cand[divisor_cand > 0])
        
        # Choose new divisor between smallest and second smallest candidate
        (divisor_min <- sort(divisor_cand)[1])
        (divisor_max <- sort(divisor_cand)[2])
        
        
      } else {
        
        (divisor_cand <- c(party_est$pred_Z_abs / (party_est$seats_round + .5), party_est$pred_Z_abs / (party_est$seats_round + 1.5)) %>% unique)
        
        # Only positive divisors
        divisor_cand <- divisor_cand[divisor_cand > 0]
        
        # Choose new divisor between smallest and second smallest candidate
        (divisor_min <- sort(divisor_cand, decreasing = T)[1])
        (divisor_max <- sort(divisor_cand, decreasing = T)[2])
        
      }
      
      # New divisor
      (this_divisor <- (divisor_min + divisor_max)/2)
      (party_est$seats_round <- round(party_est$pred_Z_abs/this_divisor, 0))
      
      counter <- counter + 1
    }
    
    party_est$zs_draw <- draw[seats_party]
    party_est$divisor <- this_divisor
    
    party_est$seats[party_est$party == "csu"]
    
    # this_divisor
    sum(party_est$seats_round)
    
    seats_states <- bind_rows(seats_states, party_est)
    
  }
  
  # seats_allocated
  
  seats_states <- district_draw %>% # filter(winner == 1) %>% 
    group_by(land, party) %>%
    # Sort decreasing by value
    arrange(desc(draw_winner), desc(value)) %>% 
    # Create a rank var
    mutate(rank = row_number()) %>% 
    mutate(rank = case_when(!draw_winner ~ NA,
                            T ~ rank)) %>% 
    merge(., seats_states, by = c("land", "party"), all.x = T) %>%
    mutate(seats_round = case_when(is.na(seats_round) ~ 0,
                                   T ~ seats_round)) %>% 
    mutate(abandoned = (draw_winner) & (rank > seats_round)) %>% 
    arrange(party, land, rank) %>%
    dplyr::select(land, party, wkr, wkr_name, pred, rank, seats_round, abandoned, zs_draw, divisor, draw_winner)
  
  seats_states$iteration <- j
  
  
  vacant_seats <- bind_rows(vacant_seats, seats_states)
  
}





### 8. Save Results -------------------------------------

saveRDS(vacant_seats, "output/vacant_seats.rds")

