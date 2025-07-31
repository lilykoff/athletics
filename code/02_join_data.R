library(tidyverse)


results = read_csv(here::here("data", "clean", "results_clean.csv"))
startlists = read_csv(here::here("data", "clean", "startlists_clean.csv"))


## join by event, year round, label

results =
  results %>%
  select(-contains("url"), -label)

startlists = startlists %>%
  select(-contains("url"), -label)


joined_data =
  results %>%
  left_join(startlists,
            by = join_by(competition, event, year, round, athlete, country, sex, bib))
hms(joined_data$mark[1])

## do some parsing of marks

para =
  joined_data %>%
  filter(str_detect(event, regex("wheelchair|impaired|blind|cereb\\. palsy|amputee|T5", ignore_case = TRUE)))

relay =
  joined_data %>%
  filter(str_detect(event, "Relay"))


individ_only =
  joined_data %>%
  anti_join(para) %>%
  anti_join(relay) %>%
  filter(!str_detect(event, "Masters"))

unique(individ_only$event)

long_events = c("20 Kilometres Race Walk",
                "Marathon",
                "35 Kilometres Race Walk",
                "50 Kilometres Race Walk")
medium_events = c("10,000 Metres",
                  "5000 Metres",
                  "800 Metres",
                  "1500 Metres",
                  "3000 Metres Steeplechase",
                  "10,000 Metres Race Walk",
                  "10 Kilometres Race Walk",
                  "3000 Metres")

short_events = c("100 Metres",
                 "200 Metres",
                 "400 Metres",
                 "400 EMtres Hurdles",
                 "110 Metres Hurdles",
                 "100 Metres Hurdles")
field_events = c("High Jump",
          "Triple Jump",
          "Long Jump",
          "Pole Vault",
          "Hammer Throw",
          "Shot Put",
          "Discus Throw",
          "Javelin Throw")

individ_only =
  individ_only %>%
  mutate(across(c(contains("best"), mark),
         ~if_else(str_detect(.x, "i"), sub("i.*", "", .x), .x))) %>% # if "indoor" just use as regular
  mutate(across(c(contains("best"), mark),
                ~case_when(
                  event %in% long_events ~ as.numeric(lubridate::hms(.x)), # time in seconds
                  event %in% medium_events ~ as.numeric(lubridate::ms(.x)), # time in seconds
                  event %in% c(short_events, field_events) ~ as.numeric(.x), # in units
                  .default = NA_real_),
         .names = "{col}_numeric"))

track = individ_only %>%
  filter(event %in% c(long_events, short_events, medium_events)) %>%
  select(-contains("numeric")) %>%
  mutate(across(c(contains("best"), mark),
                ~case_when(
                  event %in% long_events ~ lubridate::hms(.x), # time in seconds
                  event %in% medium_events ~ lubridate::ms(.x), # time in seconds
                  event %in% short_events ~ lubridate::ms(paste0("00:", .x))),
                .names = "{col}_duration"))

field = individ_only %>%
  filter(event %in% field_events) %>%
  mutate(across(c(contains("best"), mark), as.numeric, .names = "{col}_numeric"))

write_rds(track, here::here("data", "clean", "track_events_clean.rds"))
write_rds(field, here::here("data", "clean", "field_events_clean.rds"))

# could also do seconds(x)
