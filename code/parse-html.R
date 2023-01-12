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

# Fix formatting

df <- df %>%
    mutate(
        msrp                      = parse_number(msrp),
        invoice_price             = parse_number(invoice_price),
        used_new_price            = parse_number(used_new_price),
        vehicle_length_in         = parse_number(vehicle_length),
        vehicle_width_in          = parse_number(vehicle_width),
        vehicle_height_in         = parse_number(vehicle_height),
        range_mi                  = parse_number(range),
        electric_range_mi         = parse_number(electric_range),
        wheelbase_in              = parse_number(wheelbase),
        ground_clearance_in       = parse_number(ground_clearance),
        power_to_weight_ratio_in  = parse_number(power_to_weight_ratio),
        cargo_capacity_in         = parse_number(cargo_capacity),
        front_headroom_in         = parse_number(front_legroom),
        front_legroom_in          = parse_number(front_legroom),
        front_shoulder_room_in    = parse_number(front_shoulder_room),
        front_hip_room_in         = parse_number(front_hip_room),
        rear_headroom_in          = parse_number(rear_headroom),
        rear_legroom_in           = parse_number(rear_legroom),
        highway_fuel_economy_mpg  = parse_number(highway_fuel_economy),
        city_fuel_economy_mpg     = parse_number(city_fuel_economy),
        combined_fuel_economy_mpg = parse_number(combined_fuel_economy),
        fuel_capacity_mpg         = parse_number(fuel_capacity)
    ) %>%
    select(-c(
        'msrp', 'invoice_price', 'used_new_price', 'vehicle_length',
        'vehicle_width', 'vehicle_height', 'range_mi',
        'electric_range_mi', 'wheelbase', 'ground_clearance',
        'power_to_weight_ratio', 'cargo_capacity', 'front_headroom',
        'front_legroom', 'front_shoulder_room', 'front_hip_room',
        'rear_headroom', 'rear_legroom', 'highway_fuel_economy',
        'city_fuel_economy', 'combined_fuel_economy', 'fuel_capacity',
    ))

arrow::write_parquet(df, file.path('data-raw', 'data.parquet'))
