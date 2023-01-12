library(tidyverse)
library(rvest)

get_page_table <- function(path) {
    table <- read_html(path) %>%
        html_nodes('table') %>%
        html_table(header = TRUE, trim = TRUE)
    return(table)
}

root <- '/Users/jhelvy/Desktop/html/'
files <- list.files(root)
pages <- paste0(root, files)

data <- list()
for (i in 1:length(pages)) {
    cat(i, '\n')
    data[[i]] <- get_page_table(pages[i])[[2]] %>%
        janitor::clean_names()
}

df <- do.call(rbind, data)

arrow::write_parquet(df, file.path('data-raw', 'data.parquet'))
