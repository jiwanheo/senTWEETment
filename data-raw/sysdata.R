## code to prepare `DATASET` dataset goes here

stop_words <- tidytext::stop_words
# stop_words <- data.frame(word = "Hi", lexicon = "asd")

usethis::use_data(stop_words, overwrite = TRUE, internal = TRUE)
rm(stop_words)
