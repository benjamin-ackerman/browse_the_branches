library(dplyr)
library(shiny)
library(readr)
library(gt)
library(gtExtras)

book_df <- read_csv("~/Documents/R_projects/browse_branches/branch_availability.csv")

all_locations <- book_df %>% 
    arrange(available_locations) %>% 
    distinct(available_locations) %>% 
    pull()

# Define UI for application that draws a histogram
shinyUI(fluidPage(
    
    # Application title
    titlePanel("BPL Browse the Branches"),
    
    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            selectInput("locations",
                        "BPL Locations:",
                        choices = all_locations,
                        multiple = T)
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            gt_output("booktab")
        )
    )
))
