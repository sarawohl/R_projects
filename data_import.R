# Package import
library(readr)
library(here)
library(dplyr)

# List of csv-files
csv_names <- list.files(path = here('data'),
                        pattern = '*.csv',
                        full.names = T)

filenames <- list.files(path=here('data'),
                        pattern='.*csv')


# Data reading and transformation

col_selection <- c('condition', 'corr_resp', 'marker', 'trials.thisN', 'iti', 'key_resp_trial.corr', 'key_resp_trial.rt')

file_reader <- function(x) {
  read_csv(
    x,
    col_select=col_selection, 
    col_types=c(
      col_character(), 
      col_character(), 
      col_integer(), 
      col_integer(),
      col_integer(), 
      col_integer(), 
      col_integer()
    )
  )
}

data <- lapply(
  csv_names,
  file_reader
  ) %>%
  bind_rows(.id = "id")

data <- data %>% 
  mutate( id = as.numeric(id)) %>% 
  arrange(id)

# Remove other objects in environment
rm(list=setdiff(ls(), 'data'))
