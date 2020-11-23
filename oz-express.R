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

# The Wizard of Oz

oz_original <- gutenberg_download(55)

chapter_titles <- oz_original %>%
  slice(19:42) %>%
  pull(text) %>%
  # There's a space out front in the TOC
  str_remove_all(pattern=" Chapter [IVX]+\\. ") %>%
  paste0("(", ., ")", collapse="|")

oz_original <- oz_original %>%
  mutate(text=str_remove_all(text, pattern=chapter_titles)) %>%
  slice(-c(1:80))


## Do the LexRank

oz_express <- oz_original %>%
  select(-gutenberg_id) %>%
  mutate(chapter=cumsum(str_detect(text, pattern="^Chapter"))) %>%
  mutate(text=str_remove_all(text, pattern="^Chapter.*$")) %>%
  filter(chapter != 0) %>%
  unnest_tokens(output=sentence, input=text, token="sentences", to_lower=FALSE) %>%
  # Cloning a dummy chapter column so I can have one inside and one outside after nesting
  mutate(sentnum=1:n(), chnum=chapter) %>%
  mutate(across(chapter, as.factor)) %>%
  group_by(chnum) %>%
  nest(documents = c(sentence, sentnum, chapter)) %>%
  pluck("documents") %>%
  map_dfr(bind_lexrank, text=sentence, doc_id=chapter, level="sentences",
          usePageRank=TRUE, continuous=TRUE) %>%
  drop_na(lexrank) %>%
  group_by(chapter) %>%
  slice_max(order_by=lexrank, prop=0.2) %>%
  arrange(sentnum) %>%
  select(chapter, sentence, lexrank)


## Even shorter version

oz_speedrun <- oz_express %>%
  group_by(chapter) %>%
  slice_max(order_by=lexrank, n=1) %>%
  select(chapter, sentence, lexrank)