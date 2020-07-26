library(tidyverse)
library(bib2df)
library(stringi)

# make as a function!
lib <- read_tsv("./data/features/ref_tables/standard_of_comparison.csv")  # file from the drive ./data/biblib.csv

# add eng_transl to title
lib %>%
  mutate(title = gsub(" \\[NA\\]", "", paste(title, " [", gsub("\\{|\\}", "", title_translation), "]",sep = ''))) %>%
  mutate(booktitle = gsub(" \\[NA\\]|NA", "", paste(gsub("\\{|\\}", "", booktitle), " [", gsub("\\{|\\}", "", booktitle_translation), "]",sep = ''))) %>%
  select(-c(comment, contributor, booktitle_translation, title_translation))-> lib

lib %>%
  mutate(author = gsub(' и ', ' and ', author)) %>% 
  mutate(editor = gsub(' и ', ' and ', editor))-> lib  # see references.Rmd

# translit (method in progress)
# a = c()
# t = c()
# for (i in lib$author)
# {
#   i <- gsub(" и ", " and ", i)
#   a <- c(a, stri_trans_general(i, "cyrillic-latin"))
# }
# for (i in lib$title)
# {
#   i <- gsub(" и ", " and ", i)
#   t <- c(t, stri_trans_general(i, "cyrillic-latin"))
# }
# lib %>% 
#   mutate(author = a) %>% 
#   mutate(title = t)-> lib
 

df2bib(lib, "./data/features/ref_bibs/standard_of_comparison.bib")  # ./data/bibliography.bib
write_tsv(lib, "./data/features/ref_tables/standard_of_comparison.csv")  # temporal solution ./data/biblib.csv
