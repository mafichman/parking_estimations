# estimate parking occupancy using Sf Park data
# Code by Michael Fichman - mfichman@upenn.edu

# This is a really basic mock-up of how I estimated Pittsburgh parking occupancies to
# the 15 minute interval. I did that project before tidyverse, so it's not code that you
# would find particularly useful.

# There is 100% a better way to do this than to create a bunch of stuff in a mutate
# statement - this is prime for a looped or more algorithmic type of code. It's also very "blunt force"
# in how long it assumes people are parking - there is a lot of rounding.

# This is a panel data setup

# The basic idea is you buy, say, an hour worth of parking, and you "get" 4
# 15 minute "tokens" that can be "dropped" in the occupancy counter of each of the 
# upcoming slots in the panel.

# THis is code just for a few days - but you would want to be doing this for many meters.
# This code assumes you can buy a max of 3 hours of parking (e.g. 12 tokens) - you might need to adjust that.

library(tidyverse)
library(lubridate)
library(RSocrata)

#dat <- read.socrata("https://data.sfgov.org/resource/imvp-dq3v.json?post_id=591-25480")
dat <- read.socrata("https://data.sfgov.org/resource/imvp-dq3v.json?$where=session_start_dt between '2022-01-10T12:00:00' and '2022-01-15T12:00:00'")

# Divide transaction lengths into 15 minute intervals
# THis is done very roughly, you might want to make this more exact
# If a transaction is, say 45 minutes, drop tokens in tokens_00, 01, 02

dat2 <- dat %>%
  mutate(end_interval15 = floor_date(ymd_hms(session_end_dt), unit = "15 mins"),
         start_interval15 = floor_date(ymd_hms(session_start_dt), unit = "15 mins"),
         length = end_interval15 - start_interval15,
         tokens = as.numeric(length )/ 900,
         tokens_00 = ifelse(tokens >= 1, 1, 0),
         tokens_01 = ifelse(tokens >= 2, 1, 0),
         tokens_02 = ifelse(tokens >= 3, 1, 0),
         tokens_03 = ifelse(tokens >= 4, 1, 0),
         tokens_04 = ifelse(tokens >= 5, 1, 0),
         tokens_05 = ifelse(tokens >= 6, 1, 0),
         tokens_06 = ifelse(tokens >= 7, 1, 0),
         tokens_07 = ifelse(tokens >= 8, 1, 0),
         tokens_08 = ifelse(tokens >= 9, 1, 0),
         tokens_09 = ifelse(tokens >= 10, 1, 0),
         tokens_10 = ifelse(tokens >= 11, 1, 0),
         tokens_11 = ifelse(tokens >= 12, 1, 0))

# Summarize this data by start time and meter

dat3 <- dat2 %>%
  group_by(start_interval15, post_id) %>%
  summarize(tokens_00 = sum(tokens_00),
            tokens_01 = sum(tokens_01),
            tokens_02 = sum(tokens_02),
            tokens_03 = sum(tokens_03),
            tokens_04 = sum(tokens_04),
            tokens_05 = sum(tokens_05),
            tokens_06 = sum(tokens_06),
            tokens_07 = sum(tokens_07),
            tokens_08 = sum(tokens_08),
            tokens_09 = sum(tokens_09),
            tokens_10 = sum(tokens_10),
            tokens_11 = sum(tokens_11))

# Create a panel consisting of all the time/meter observations in the set
# Add a day of the year to each observation, join it to the transaction data
# This might need to be tinkered with to make sure every time period for every meter is included
# There are some weird one-off transactions off hours that might need to be cleaned out

study.panel <- 
  expand.grid(start_interval15=unique(dat3$start_interval15), 
              post_id = unique(dat3$post_id)) %>%
  mutate(doty = yday(start_interval15)) %>%
  left_join(., dat3)

# Estimate occupancy but compiling the current tokens and the previous tokens
# that carry forward - i think (i think) the observations at 15:00 hours are the people who start
# the day parking - not every place has the same metered hours

transaction_panel <- study.panel %>%
  replace(is.na(.), 0) %>%
  arrange(start_interval15) %>%
  group_by(post_id, doty) %>%
  mutate(lag01 = ifelse(is.na(lag(tokens_01)) == FALSE, lag(tokens_01), 0),
         lag02 = ifelse(is.na(lag(tokens_02)) == FALSE, lag(tokens_02), 0),
         lag03 = ifelse(is.na(lag(tokens_03)) == FALSE, lag(tokens_03), 0),
         lag04 = ifelse(is.na(lag(tokens_04)) == FALSE, lag(tokens_04), 0),
         lag05 = ifelse(is.na(lag(tokens_05)) == FALSE, lag(tokens_05), 0),
         lag06 = ifelse(is.na(lag(tokens_06)) == FALSE, lag(tokens_06), 0),
         lag07 = ifelse(is.na(lag(tokens_07)) == FALSE, lag(tokens_07), 0),
         lag08 = ifelse(is.na(lag(tokens_08)) == FALSE, lag(tokens_08), 0),
         lag09 = ifelse(is.na(lag(tokens_08)) == FALSE, lag(tokens_09), 0),
         lag10 = ifelse(is.na(lag(tokens_10)) == FALSE, lag(tokens_10), 0),
         lag11 = ifelse(is.na(lag(tokens_11)) == FALSE, lag(tokens_11), 0)) %>%
  mutate(occupancy = tokens_00 + lag01 + lag02 + lag03+ lag04 + lag05 +
           lag06 + lag07 + lag08+ lag09 + lag10 + lag11) %>%
 # filter(is.na(occupancy) == FALSE) %>%
  select(start_interval15, post_id, occupancy)

# join everything

transaction_panel <- left_join(transaction_panel, dat %>% 
                           select(post_id, street_block) %>%
              unique())
