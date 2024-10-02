library(rvest)
library(dplyr)
library(readr)
library(stringr)
library(httr)
library(lubridate)
library(purrr)

source("utils.R")

# Pull all library locations ----
locations_link <- "https://www.bklynlibrary.org/browse-the-branches/about"
library_locations <- read_html(locations_link) %>% 
  html_nodes("table") %>% 
  html_nodes("p[class='lead']") %>% 
  html_text() %>% 
  str_replace_all(., "\\*", "")

# Cleaning the locations from the scraped data
name_cleaner <- c("YA", "Lrg", "Fic", "Biog","AFR", "Shrt", "Literacy", "Non-Fic", "Sci-Fic", "Audiobk","Audiobook", "Sci","- Lit" ," - " , "1st", "2nd", "3rd", "Mystery")

# Load goodreads data ----
books <- read_csv("~/Downloads/goodreads_library_export.csv")

# Clean book data, find locations at BPL where book is available ----
available_books <- books %>% 
  group_by(Title) %>% 
  filter(all(`Exclusive Shelf` == "to-read")) %>% 
  mutate(ISBN = str_extract(ISBN, "[0-9]+(X)?"),
         pub_year = pmin(`Original Publication Year`, `Year Published`, na.rm = T)) %>% 
  filter(!is.na(ISBN)) %>% 
  mutate(ISBN = glue::glue("k:({ISBN})")) %>% 
  # group_by(Title, Author) %>% 
  # filter(`Date Added` == min(`Date Added`)) %>% 
  # ungroup() %>% 
  group_by(Title, `Author l-f`) %>% 
  mutate(search = paste(unique(ISBN), collapse = " OR "),
         pub_year = min(pub_year, na.rm = T)) %>%
  ungroup() %>% 
  # Just look for books I've wanted to read in the last year and a half
  filter(`Date Added` >= today() - months(18)) %>% 
  mutate(search = glue::glue("adv:({search})")) %>% 
  distinct(Title, Author, `Date Added`, `Average Rating`, search)

# Search for all books and get BPL Book IDs
if(!"bpl_ids.Rds" %in% list.files()){
  search_links <- paste0("https://www.bklynlibrary.org/search?search=", available_books$search)
  
  bpl_id <- search_links %>% 
    map(GetBPLBookID) %>% 
    set_names(available_books$Title)
  
  bpl_id[!is.na(bpl_id)] %>% 
    saveRDS("bpl_ids.Rds")
} else {
  bpl_id <- readRDS("bpl_ids.Rds")
}

bpl_ids <- tibble(
  Title = names(bpl_id),
  bpl_id = bpl_id
) %>% 
  tidyr::unnest(bpl_id) %>% 
  mutate(book_link = paste0("https://www.bklynlibrary.org", bpl_id),
         book_link = ifelse(is.na(bpl_id), NA, book_link))

locations <- bpl_id %>% 
  map(~paste0("https://www.bklynlibrary.org", .x) %>% 
        GetAvailableLocations(., name_cleaner))

locations_table <- tibble(
  Title = names(locations),
  available_locations = locations
) %>% 
  tidyr::unnest(available_locations) %>% 
  tidyr::unnest(available_locations) %>% 
  distinct()

# Unnest and save data
available_books %>% 
  group_by(Title) %>% 
  filter(`Date Added` == min(`Date Added`)) %>% 
  ungroup() %>% 
  inner_join(bpl_ids, by = "Title") %>% 
  inner_join(locations_table, by = "Title") %>% dplyr::select(Title, Author, book_link, `Date Added`, `Average Rating`, available_locations) %>% 
  write_csv(., file = "branch_availability.csv")
