library(tidyr)
library(shiny)
library(readr)
library(gt)
library(gtExtras)

book_df <- read_csv("~/Documents/R_projects/browse_branches/branch_availability.csv")
source("~/Documents/R_projects/browse_branches/gt_star_function.R")

all_locations <- book_df %>% 
    arrange(available_locations) %>% 
    distinct(available_locations) %>% 
    pull()

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    filtered_book_df <- reactive({
        filtered_df <- book_df %>% 
            filter(available_locations %in% input$locations) %>% 
            rename(`Year Published` = pub_year,
                   `Average Goodreads Rating` = `Average Rating`)
        
        if(length(input$locations) == 1){
            filtered_df <- filtered_df %>% 
                dplyr::select(-available_locations)
        } else {
            filtered_df <- filtered_df %>% 
                group_by(across(c(-available_locations))) %>% 
                summarise(available_locations = paste(available_locations, collapse = ", "), .groups = 'drop') %>% 
                dplyr::rename(`Available Locations` = available_locations)
        }
    })
    
    output$booktab <- render_gt({
        
        filtered_book_df() %>% 
            arrange(desc(`Average Goodreads Rating`)) %>% 
            gt() %>% 
            rating_modified(`Average Goodreads Rating`, color = "gold", icon = "star") %>% 
            opt_interactive(use_search = T, page_size_default = 10)
        
    })
    
})

