
library(shiny)
library(fullPage)
library(shinyWidgets)
library(reactable)
library(reactablefmtr)
library(prompter)
library(shinyBS)
library(tidyverse)
library(readxl)
library(plotly)



## Load the database

DATAPATH <- "data/last/Refined data_RR_MH_20230608_version 5.3.xlsx"
DATAPATH_CONVERSION <- "data/last/Risk value to score conversion table.xlsx"

FULL_DATA <- excel_sheets(DATAPATH) %>%
  set_names(., .) %>%
  map(.,
      ~ read_excel(DATAPATH, sheet = .)
      )

CONVERSION_TABLE <- excel_sheets(DATAPATH_CONVERSION) %>%
  set_names(., .) %>%
  map(.,
      ~ read_excel(DATAPATH_CONVERSION, sheet = .)
  )

## Food categories

FOOD_CATEGORIES <- FULL_DATA$table_properties$Food_main_category %>%
  unique() %>%
  set_names(., .) %>%
  map(.,
      ~ filter(FULL_DATA$table_properties, Food_main_category == .)
      ) %>% 
  map(.,
      ~ pull(., Food_item)
      )

for (each_cat in names(FOOD_CATEGORIES)) {  # Add something empty for those with just 1 element
  
  if (length(FOOD_CATEGORIES[[each_cat]]) == 1) {
    FOOD_CATEGORIES[[each_cat]] <- c(FOOD_CATEGORIES[[each_cat]], "")
  }
  
}

## Formatting

BLUE <- "#4C74C5"
ORANGE <- "#ED7D3C"
WHITE <- "#FFFFFF"

## Help pages

FACTORY_PARS_DESCRIPTION <- read_excel("data/last/factory parameter description.xlsx",
                                       sheet = "Sheet1")


 































