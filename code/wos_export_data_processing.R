#### set-up ####

# clear environment
rm(list = ls())

# load packages
library(tidyverse)
library(janitor)
library(readxl)


#### import data ####

# extract file names
wos_export_files <- list.files(path = "data/", pattern = "wos_export_20220203")

# empty list
wos_export_list <- vector(mode = "list", length = length(wos_export_files))

# import files
for(w in 1:length(wos_export_files)){
  
  # import data
  wos_export_list[[w]] <- read_excel(paste("data/", wos_export_files[w], sep = ""), guess_max = 10000)
  
}


#### combine data ####

# revise column types
wos_export_list[[5]][ , "Meeting Abstract"] <- as.character(wos_export_list[[5]][ , "Meeting Abstract"])
wos_export_list[[8]][ , "Supplement"] <- as.character(wos_export_list[[8]][ , "Supplement"])

# convert list to data frame
wos_export <- wos_export_list %>%
  bind_rows()

# examine last column
unique(wos_export[ , 58])

# revise column names
# remove last column (no info)
wos_export2 <- wos_export %>%
  rename_with(~ str_replace_all(.x, " - ", "_")) %>%
  rename_with(~ str_replace_all(.x, " ", "_")) %>%
  rename_with(~ str_replace_all(.x, c("...11", "...36"), "")) %>%
  rename_with(~ str_replace_all(.x, "\\,", "")) %>%
  rename_with(~ str_replace_all(.x, "\\(Unique_ID\\)", "Unique_ID")) %>%
  rename_with(~ str_replace_all(.x, c("...12", "...38"), "2")) %>%
  rename_with(~ str_replace_all(.x, "180_Day", "half_year")) %>%
  rename_with(~ str_replace_all(.x, "...58", "omit")) %>%
  rename_with(tolower) %>%
  select(-omit)


#### examine data ####

# list column names
colnames(wos_export2)

# publication types
unique(wos_export2$publication_type)
# type codes: https://images.webofknowledge.com/images/help/WOS/hs_wos_fieldtags.html
# B = book
# J = journal
# P = patent
# S = book in a series

# missing essential data?
wos_export2 %>%
  filter(is.na(authors) & is.na(group_authors))
# to replace NA in authors, use: group_authors, book_editors, and author_arabic

wos_export2 %>%
  filter(is.na(article_title))
# none missing

wos_export2 %>%
  filter(is.na(publication_date) & is.na(early_access_date)) %>%
  data.frame()
# these articles were published in 2019 and 2020, but not seeing those years anywhere
# to replace NA publication_date, use early_access_date

wos_export2 %>%
  filter(is.na(source_title))
# none missing

wos_export2 %>%
  filter(is.na(doi) & is.na(book_doi))


#### revise data ####

# remove patents
# select author, year, source info
wos_export3 <- wos_export2 %>%
  filter(publication_type != "P") %>% # removes 7 records
  transmute(author = case_when(!is.na(authors) ~ authors,
                               !is.na(group_authors) ~ group_authors,
                               !is.na(author_arabic) ~ author_arabic,
                               !is.na(book_editors) ~ book_editors),
            # date = case_when(!is.na(publication_date) ~ publication_date,
            #                  !is.na(early_access_date) ~ early_access_date), # lots of different formats make this difficult
            title = article_title,
            source = source_title,
            type = publication_type,
            doi = case_when(!is.na(doi) ~ paste("https://doi.org/", doi, sep = ""),
                            !is.na(book_doi) ~ paste("https://doi.org/", book_doi, sep = "")))

# check for duplicates
(wos_dups <- get_dupes(wos_export3, author, title, source)) # 9 duplicates

unique(wos_dups)

# remove duplicates
wos_export4 <- wos_export3 %>%
  anti_join(wos_dups) %>%
  full_join(wos_dups %>%
              group_by(author, title, source, type) %>%
              summarize(doi = doi[1]) %>% # non-unique doi's
              ungroup())


#### output ####

# save data
write_csv(wos_export4, "output/wos_export_20220203.csv")
# rename it with _ed so that edited version doesn't get overwritten