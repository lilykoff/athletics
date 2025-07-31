library(tidyverse)

results_fnames = list.files(here::here("data", "raw"), pattern = "results", full.names = TRUE)
start_fnames = list.files(here::here("data", "raw"), pattern = "start", full.names = TRUE)



results_raw =
  map_dfr(results_fnames,
          \(x) read_csv(x, col_select = c("sex", "event", "round", "label",
                                          "url", "games_url", "POS", "BIB",
                                          "ATHLETE", "COUNTRY", "MARK"),
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
                          MARK = col_character()
                        )))


startlists_raw =
  map_dfr(start_fnames, \(x) read_csv(x, col_types = cols(
    Bib = col_character(), 'Order / Lane' = col_number()
  )))

results_clean =
  results_raw %>%
  separate_wider_delim(
    cols = "MARK",
    delim = "  ",
    names = c("MARK", "DETAIL"),
    too_few = "align_start"
  ) %>%
  mutate(competition = if_else(str_detect(games_url, "championships"), "world_champs", "olympics"),
         year = if_else(competition == "olympics",
                        as.numeric(sub(".*olympic\\-games\\/(.+)\\/.*", "\\1", games_url)),
                        as.numeric(sub(".*championships\\/(.+)\\/.*", "\\1", games_url)))) %>%
  select(competition, year, event, sex, round, label, POS, ATHLETE, COUNTRY, BIB, MARK, DETAIL, everything()) %>%
  janitor::clean_names()



startlists_clean =
  startlists_raw %>%
  unite("SB", starts_with("SB"), na.rm = TRUE, remove = TRUE) %>%
  mutate(competition = if_else(str_detect(games_url, "championships"), "world_champs", "olympics"),
         year = if_else(competition == "olympics",
                        as.numeric(sub(".*olympic\\-games\\/(.+)\\/.*", "\\1", games_url)),
                        as.numeric(sub(".*championships\\/(.+)\\/.*", "\\1", games_url))),
         BIB = as.numeric(Bib)) %>% ## okay to create some NAs here
  select(competition, year, event, sex, round, label, ATHLETE, COUNTRY, BIB, personal_best = PB, season_best = SB,
         contains("url")) %>%
  janitor::clean_names()

if(!dir.exists(here::here("data", "clean"))){
  dir.create(here::here("data", "clean"), recursive = TRUE)
}

write_csv(results_clean, here::here("data", "clean", "results_clean.csv"))
write_csv(startlists_clean, here::here("data", "clean", "startlists_clean.csv"))
