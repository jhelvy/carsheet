library(tidyverse)

df <- arrow::read_parquet(file.path('data-raw', 'data.parquet'))

df %>%
    filter(body_style == 'Pickup Truck') %>%
    group_by(year) %>%
    summarise(median(vehicle_height_in, na.rm = TRUE)) %>%
    data.frame()

df %>%
    filter(body_style == 'Pickup Truck') %>%
    filter(vehicle_height_in < 90) %>%
    group_by(year) %>%
    ggplot() +
    geom_boxplot(
        aes(x = year, y = vehicle_height_in, group = year)
    ) +
    scale_x_continuous(breaks = seq(1990, 2023, 3)) +
    theme_bw() +
    labs(
        x = "Year",
        y = "Vehicle height (inches)"
    )

ggsave('pickup_height.png', width = 7, height = 4)
