library(targets)
source("R/functions.R")
tar_option_set(packages = c("tidyverse", "jsonlite", "lme4", "igraph"))
list(
  # Reading in sentiment analysis results, see `run-vader.py`
  tar_target(file, "vader-results.csv", format = "file"),
  tar_target(vader, read_csv(file, col_names = FALSE, col_types = list(
    X1 = col_character(),
    X2 = col_double()
  ))),

  # These two steps read in raw JSON files returned from the Twitter API,
  # selects relevant variables and filters tweets to establish the study sample.
  # In addition, it joins results of the VADER sentiment analysis.
  tar_target(d_tweets_raw, get_data()),
  tar_target(d_clean, clean_and_combine(d_tweets_raw, vader)),

  # These two steps annotate each tweet with their respective community
  # and remove any duplicate tweets
  tar_target(d_split_data, split_data(d_clean)),
  tar_target(d_analysis, recombine_splits(d_split_data))
)
