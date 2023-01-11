library(tidyverse)
library(rvest)
library(chromote)

get_page_table <- function(b) {
    html <- b$Runtime$evaluate('document.documentElement.outerHTML')$result$value
    table <- read_html(html) %>% 
        html_nodes('table') %>%
        html_table(header = TRUE, trim = TRUE)
    return(table)
}

# Open browser

b <- ChromoteSession$new()
b$view()

# Navigate to main page 

b$Page$navigate('https://carsheet.io/')

table <- get_page_table(b)
names(table[[2]])
