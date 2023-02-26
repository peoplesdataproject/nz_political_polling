library(tidyverse)
library(rvest)
library(janitor)
library(lubridate)

election_years <- c(2017,2020,2023)

data <- map_df(election_years, function(yr){
  
  html <- paste0('https://en.wikipedia.org/wiki/Opinion_polling_for_the_',yr,'_New_Zealand_general_election')
  
  webpage <- read_html(html)
  
  tbls <- html_nodes(webpage, "table")
  
  table <- tibble(
    tbl = map(tbls, ~html_table(.x))
  ) %>% 
    mutate(
      correct_headings = map_lgl(tbl, ~all(c('LAB', 'NAT') %in% (names(.x)))),
      nrows = map_dbl(tbl, ~nrow(.x))
    ) %>% filter(correct_headings) %>% filter(nrows == max(nrows)) %>% 
    pull(tbl) %>% .[[1]]
  
  
  data <- clean_names(table) %>%
    rename(date = 1, poll = 2) %>% 
    filter(str_detect(nat, '^(\\d|\\.)+$')) %>% 
    mutate_at(vars(3:last_col()), ~str_remove(.x, ',') %>% as.numeric) %>% 
    # mutate_at(vars(sample_size:last_col()), as.numeric) %>% 
    mutate_at(vars(lab:last_col()), ~replace_na(.x, 0)) %>% 
    mutate() %>% 
    mutate(
      poll = str_remove(poll, '\\[\\w+\\]') %>% str_remove(., '\\[\\d+\\](\\[\\d+\\])?') %>% str_replace_all(., '-|–', ' '),
      poll = recode(poll, 'One News Colmar Brunton' = '1 News Colmar Brunton', 'Roy Morgan' = 'Roy Morgan Research'),
      year = str_extract(date, '\\d{4}'),
      date = str_remove(date, '\\s\\d{4}'),
    ) %>% separate(date, into = c('d1','d2'), sep = '\\s\\–\\s') %>% 
    mutate(
      d1 = ifelse(str_detect(d1, '^\\d'), d1, paste0('15 ', d1)),
      days = ifelse(str_detect(d1, '\\–'), str_extract_all(d1, '\\d+') %>% map(as.numeric), as.numeric(str_extract(d1, '\\d+'))),
      day = map_dbl(days, ~ifelse(length(.x) > 1, round(.x[1] + (.x[2] - .x[1])/2), .x[1])),
      d1 = str_replace(d1, '\\d+\\–\\d+', as.character(day))
    ) %>% 
    mutate_at(vars(d1, d2), ~ifelse(!is.na(.x), paste(.x, year), NA) %>% as.Date(., format = '%d %b %Y')) %>% 
    mutate(date = map2(d1, d2, ~mean(c(.x, .y), na.rm = T))) %>% 
    unnest('date') %>% relocate(date) %>% 
    select(-c(d1, d2, year, days, day), -one_of("lead"))
})

events <- map_df(
  election_years,
  function(yr){
    html <- paste0('https://en.wikipedia.org/wiki/Opinion_polling_for_the_',yr,'_New_Zealand_general_election')
    
    webpage <- read_html(html)
    
    tbls <- html_nodes(webpage, "table")
    
    table <- tibble(
      tbl = map(tbls, ~html_table(.x))
    ) %>% 
      mutate(
        correct_headings = map_lgl(tbl, ~all(c('LAB', 'NAT') %in% (names(.x)))),
        nrows = map_dbl(tbl, ~nrow(.x))
      ) %>% filter(correct_headings) %>% filter(nrows == max(nrows)) %>% 
      pull(tbl) %>% .[[1]]
    
    clean_names(table) %>%
      rename(date = 1, event = 2) %>%
      filter(!str_detect(nat, '^(\\d|\\.)+$'), str_detect(date, '^\\d')) %>%
      select(date, event) %>%
      mutate(
        date = as.Date(date, format = '%d %b %Y'),
        rating = 55 - (9 - row_number() + 1)/9 * (55-43)
      )
  }
)

party_colour <- function(pty){
  recode(pty,
         lab = '#D82A20',
         nat = '#00529F',
         grn = '#098137',
         act = '#FDE401',
         mri = '#B2001A',
         nzf = '#000000',
         top = '#32DAC3',
         ncp = '#00AEEF',
         unf = '#501557',
         con = '#00AEEF',
         mna = '#770808',
         anz = '#AADDCC'
  )
}

plot_data <- pivot_longer(data, nat:last_col(), names_to = 'party', values_to = 'rating') %>% 
  filter(!str_detect(poll, 'result'), party != 'sample_size') %>% 
  mutate(
    party = recode(party, 'tpm' = 'mri'),
    colour = party_colour(party)
  ) 