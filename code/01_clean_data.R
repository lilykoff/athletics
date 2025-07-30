library(tidyverse)

results_raw_data = list.files(here::here("data", "raw"), pattern = "results", full.names = TRUE)
start_raw_data = list.files(here::here("data", "raw"), pattern = "start", full.names = TRUE)

olympic = read_csv(
  results_raw_data[1],
  col_select = c(
    "sex",
    "event",
    "round",
    "label",
    "url",
    "games_url",
    "POS",
    "BIB",
    "ATHLETE",
    "COUNTRY",
    "MARK"
  ),
  col_types = cols(
    sex = col_character(),
    event = col_character(),
    round = col_character(),
    label = col_character(),
    url = col_character(),
    games_url = col_character(),
    POS = col_integer(),
    BIB = col_integer(),
    ATHLETE = col_character(),
    COUNTRY = col_character(),
    MARK = col_character()
  )
) %>%
  separate_wider_delim(
    cols = "MARK",
    delim = "  ",
    names = c("MARK", "DETAIL"),
    too_few = "align_start"
  )

olympic_clean =
  olympic %>%
  mutate(year = sub(".*olympic\\-games\\/(.+)\\/.*", "\\1", games_url) %>% as.numeric) %>%
  select(year, sex, event, round, label, POS, ATHLETE, COUNTRY, BIB, MARK, DETAIL, url, games_url) %>%
  mutate(type = "Olympics") %>%
  janitor::clean_names()

## do same for startlists

results =
  map_dfr(.x = results_raw_data, .f = \(x) read_csv(x, col_select = c("sex", "event", "round",
                                                             "label", "url", "games_url",
                                                             "POS", "BIB", "ATHLETE", "COUNTRY",
                                                             "MARK"),
                                                col_types = cols(sex = col_character(),
                                                              event = col_character(),
                                                              round = col_character(),
                                                              label = col_character(),
                                                              url = col_character(),
                                                              games_url = col_character(),
                                                              POS = col_integer(),
                                                              BIB = col_integer(),
                                                              ATHLETE = col_character(),
                                                              COUNTRY = col_character(),
                                                              MARK = col_character())))

results_clean =
  results %>%
  separate_wider_delim(cols = "MARK", delim = "  ", names = c("MARK", "DETAIL"),
                       too_few = "align_start") # too_many = "debug")

results_clean =
  results %>%

dat = read_csv(results_raw_data[1], col_select = c("sex", "event", "round",
                                             "label", "url", "games_url",
                                             "POS", "BIB", "ATHLETE", "COUNTRY",
                                             "MARK"),
         col_types = cols(sex = col_character(),
                          event = col_character(),
                          round = col_character(),
                          label = col_character(),
                          url = col_character(),
                          games_url = col_character(),
                          POS = col_integer(),
                          BIB = col_integer(),
                          ATHLETE = col_character(),
                          COUNTRY = col_character(),
                          MARK = col_character())) # mark and detail not separated


dat = read_csv(results_raw_data[3], col_select = c("sex", "event", "round",
                                                   "label", "url", "games_url",
                                                   "POS", "BIB", "ATHLETE", "COUNTRY",
                                                   "MARK", "DETAIL"),
               col_types = cols(sex = col_character(),
                                event = col_character(),
                                round = col_character(),
                                label = col_character(),
                                url = col_character(),
                                games_url = col_character(),
                                POS = col_integer(),
                                BIB = col_integer(),
                                ATHLETE = col_character(),
                                COUNTRY = col_character(),
                                MARK = col_character(),
                                DETAIL = col_character()))


dat = read_csv(results_raw_data[4], col_select = c("sex", "event", "round",
                                                   "label", "url", "games_url",
                                                   "POS", "BIB", "ATHLETE", "COUNTRY",
                                                   "MARK", "DETAIL"),
               col_types = cols(sex = col_character(),
                                event = col_character(),
                                round = col_character(),
                                label = col_character(),
                                url = col_character(),
                                games_url = col_character(),
                                POS = col_integer(),
                                BIB = col_integer(),
                                ATHLETE = col_character(),
                                COUNTRY = col_character(),
                                MARK = col_character(),
                                DETAIL = col_character()))
