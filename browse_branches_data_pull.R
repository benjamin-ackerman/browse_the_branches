library(rvest)
library(dplyr)
library(readr)
library(stringr)
library(httr)
library(lubridate)

source("utils.R")
# <td style="bgcolor:#ffffff;width:175px;"><img src="https://static.bklynlibrary.org/prod/public/images/marketing/Browse%20the%20Branches/9233_SK%20Branch%20Stickers%20for%20production_adams.svg" alt="Adams Street Library" width="150" height="150"></td>

# Pull all library locations ----
locations_link <- "https://www.bklynlibrary.org/browse-the-branches/about"
library_locations <- read_html(locations_link) %>% 
  html_nodes("table") %>% 
  html_nodes("p[class='lead']") %>% 
  html_text() %>% 
  str_replace_all(., "\\*", "")

images <- read_html(locations_link)  %>% 
  html_nodes("table") %>% 
  html_nodes("img") %>% 
  html_attr("src")

locations_df <- tibble(
  branch = library_locations,
  sticker = images
)

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
         pub_year = min(pub_year, na.rm = T),
         `Date Added` = max(`Date Added`, na.rm = T)) %>%
  ungroup() %>% 
  # Just look for books I've wanted to read in the last year and a half
  filter(`Date Added` >= today() - months(12)) %>% 
  mutate(search = glue::glue("adv:({search})")) %>% 
  distinct(Title, Author, `Date Added`, `Average Rating`, search) %>% 
  rowwise() %>% 
  mutate(
    search_link = paste0("https://www.bklynlibrary.org/search?search=", search),
    bpl_id = GetBPLBookID(search_link),
    book_link = paste0("https://www.bklynlibrary.org", bpl_id),
    book_link = ifelse(is.na(bpl_id), NA, book_link),
    available_locations = GetAvailableLocations(book_link, branchname_cleaner = name_cleaner)
  )

# Unnest and save data
available_books %>% 
  dplyr::select(Title, Author, book_link, `Date Added`, `Average Rating`, available_locations) %>% 
  tidyr::unnest(available_locations) %>% 
  write_csv(., file = "branch_availability.csv")
