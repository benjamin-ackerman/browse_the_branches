library(rvest)
library(tidyr)
library(readr)
library(stringr)

setwd("~/Documents/R_projects/browse_branches/")

# Functions for scraping data ----
GetBPLBookID <- function(link){
  # Get the ID of a book from the searched link
  id = read_html(link) %>% 
    html_nodes(xpath = "/html/body/div[2]/section/article/div/div[3]/div/div[2]/div[1]/div[2]/a") %>% 
    html_attr("href")
  
  if(length(id) == 0){
    id = NA
  }
  
  # Sys.sleep(1)
  
  return(id)
}

GetAvailableLocations <- function(link, branchname_cleaner){
  # Get branches where particular book is currently available
  if(is.na(link)){
    locations = NA
  } else {
    link_data <- read_html(link)
    status <- link_data %>% 
      html_nodes("div[class='item-location-data status']") %>% 
      html_text()
    
    locations <- link_data %>% 
      html_nodes("div[class='item-location-data branch notranslate']") %>% 
      html_text() %>% 
      .[which(status == " CHECK SHELVES")] %>% 
      str_replace_all(., paste0("(", paste(branchname_cleaner, collapse = "|"), ").*"), "") %>% 
      trimws() %>% 
      unique() %>% 
      list()
    
  }
  
  return(locations)
}

# Pull all library locations ----
locations_link <- "https://www.bklynlibrary.org/browse-branches/about"
library_locations <- read_html(locations_link) %>% 
  html_nodes("table") %>% 
  html_nodes("p[class='lead']") %>% 
  html_text() %>% 
  str_replace_all(., "\\*", "")

# Cleaning the locations from the scraped data
name_cleaner <- c("YA", "Lrg", "Fic", "Biog","AFR", "Shrt", "Literacy", "Non-Fic", "Sci-Fic", "Audiobk","Audiobook", "Sci","- Lit" ," - " , "1st", "2nd", "3rd", "Mystery")

# Load goodreads data ----
books <- read_csv("goodreads_library_export.csv")

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
  filter(`Date Added` >= as.Date("2023-01-01")) %>% 
  mutate(search = glue::glue("adv:({search})")) %>% 
  distinct(Title, Author, pub_year, `Average Rating`, search) %>% 
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
  dplyr::select(Title, Author, pub_year, `Average Rating`, available_locations) %>% 
  unnest(available_locations) %>% 
  write_csv(., file = "~/Documents/R_projects/browse_branches/branch_availability.csv")
