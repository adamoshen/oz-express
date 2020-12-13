# This code differs slightly from the Rmd file since we're not printing out any tables.

library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(tidytext)
library(gutenbergr)
library(lexRankr)
library(knitr)
library(kableExtra)

##### The Wizard of Oz #####

##### Data prep #####
oz_original <- gutenberg_download(55)

chapter_titles <- oz_original %>%
  slice(19:42) %>%
  pull(text) %>%
  # There's a space out front in the TOC
  str_remove_all(pattern=" Chapter [IVX]+\\. ") %>%
  # There's a space in front of Chapter 6's title
  str_remove_all(pattern="^ ") %>%
  paste0("(", ., ")", collapse="|")

oz_original <- oz_original %>%
  mutate(text=str_remove_all(text, pattern=chapter_titles)) %>%
  slice(-c(1:80))

oz_prep <- oz_original %>%
  select(-gutenberg_id) %>%
  mutate(chapter=cumsum(str_detect(text, pattern="^Chapter"))) %>%
  mutate(text=str_remove_all(text, pattern="^Chapter.*$")) %>%
  filter(chapter != 0) %>%
  unnest_tokens(output=sentence, input=text, token="sentences", to_lower=FALSE) %>%
  mutate(sentnum=1:n()) %>%
  mutate(across(chapter, as.factor)) %>%
  group_by(chapter) %>%
  group_split()


##### Do the LexRank #####
oz_express <- oz_prep %>%
  map_dfr(
    bind_lexrank, text=sentence, doc_id=chapter, level="sentences",
    usePageRank=TRUE, continuous=TRUE
  ) %>%
  drop_na(lexrank) %>%
  group_by(chapter) %>%
  slice_max(order_by=lexrank, prop=0.2) %>%
  arrange(sentnum)


##### Even shorter version #####
oz_speedrun <- oz_express %>%
  group_by(chapter) %>%
  slice_max(order_by=lexrank, n=1) %>%
  select(chapter, sentence, lexrank)
