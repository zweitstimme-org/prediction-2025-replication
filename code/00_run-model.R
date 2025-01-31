start_time <- Sys.time()

# Load required packages and functions
source("auxiliary/packages.r")  # Load required packages
source("auxiliary/functions.r") # Load additional functions


# Specifications
upcoming_election <- 2025
cutoff <- as.Date("2025-01-29") # Date of the last poll
election_date <- as.Date("2025-02-23")
past_election_date <- as.Date("2021-09-26")
days_in_model <- 365*2


# Sampler Parameters
nIter <- 1000
nChains <- 20


# Get polls
load(str_c("output/wahlrecht_polls_", cutoff+1,".RData")) 

  # Only run once
  source("code/01_prepare-data.R")
  source("code/02_ger_structural_pre_train_stan.R")
  
  # Run the model file
  source("code/03_ger_combined_model_stan.R")
  
  # Run the data and plots
  source("code/04_party-vote-data.R")
  source("code/05_party-vote-figures.R")
  source("code/06_district-model.R")
  source("code/07_vacant-seats.R")
  source("code/08_district-figures.R")
  source("code/09_probabilities.R")



# Calculate time needed
end_time <- Sys.time()
message("Total time needed in hours:")
difftime(end_time, start_time, unit = "hours") %>% round(1) %>% message

# rstudio version number
rstudioapi::versionInfo() %>% message
