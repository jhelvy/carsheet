library(tidyverse)
library(rvest)
library(data.table)

# Function to extract the main table from a page

get_page_table <- function(path) {
  table <- read_html(path) %>%
    html_nodes("table") %>%
    html_table(header = TRUE, trim = TRUE)
  table <- table[[2]] %>%
    janitor::clean_names()
  table$model <- as.character(table$model)
  return(table)
}

# Function that converts columns encoded with "Yes" / "No" to T/F

fix_binary <- function(data, var) {
  data <- data %>%
    mutate(
      .,
      {{ var }} := ({{ var }} == "Yes")
      # {{ var }} := ifelse(is.na({{ var }}), NA, ifelse({{ var }}, 1, 0)
    )
  return(data)
}
root <- "/Users/jhelvy/Desktop/carsheet-01-26/"
pages <- list.files(root, full.names = TRUE)

data <- list()
for (i in 1:length(pages)) {
  if (i %% 10 == 0) {
    cat(i, "\n")
  }
  data[[i]] <- get_page_table(pages[i])
}
df <- rbindlist(data)

df <- df %>%
  separate(
    horsepower,
    into = c("horsepower", "horsepower_rpm"),
    sep = "@"
  ) %>%
  separate(
    torque,
    into = c("torque", "torque_rpm"),
    sep = "@"
  ) %>%
  mutate(
    horsepower = parse_number(horsepower),
    horsepower_rpm = parse_number(horsepower_rpm),
    torque_ft_lbs = parse_number(torque),
    torque_rpm = parse_number(torque_rpm),
    towing_capacity_lbs = parse_number(towing_capacity),
    msrp = parse_number(msrp),
    invoice_price = parse_number(invoice_price),
    used_new_price = parse_number(used_new_price),
    vehicle_length_in = parse_number(vehicle_length),
    vehicle_width_in = parse_number(vehicle_width),
    vehicle_height_in = parse_number(vehicle_height),
    range_mi = parse_number(range),
    electric_range_mi = parse_number(electric_range),
    full_recharge_time_hours = parse_number(full_recharge_time),
    wheelbase_in = parse_number(wheelbase),
    curb_weight_lbs = parse_number(curb_weight),
    ground_clearance_in = parse_number(ground_clearance),
    power_to_weight_ratio_in = parse_number(power_to_weight_ratio),
    highway_fuel_economy_mpg = parse_number(highway_fuel_economy),
    city_fuel_economy_mpg = parse_number(city_fuel_economy),
    combined_fuel_economy_mpg = parse_number(combined_fuel_economy),
    fuel_capacity_gal = parse_number(fuel_capacity),
    warranty_ev_battery_years = parse_number(ev_battery_years),
    warranty_ev_battery_miles = parse_number(ev_battery_miles),
    warranty_drivetrain_years = parse_number(drivetrain_years),
    warranty_drivetrain_miles = parse_number(drivetrain_miles),
    warranty_basic_years = parse_number(basic_years),
    warranty_basic_miles = parse_number(basic_miles),
  ) %>%
  fix_binary(lane_departure_warning) %>%
  fix_binary(automatic_emergency_breaking) %>%
  fix_binary(collision_avoidance_assist) %>%
  select(
    make,
    model,
    year,
    trim,
    msrp,
    invoice_price,
    used_new_price,
    body_size,
    body_style,
    curb_weight_lbs,
    vehicle_length_in,
    vehicle_width_in,
    vehicle_height_in,
    wheelbase_in,
    ground_clearance_in,
    engine_aspiration,
    cylinders,
    transmission,
    drivetrain,
    horsepower,
    horsepower_rpm,
    torque_ft_lbs,
    torque_rpm,
    power_to_weight_ratio_in,
    towing_capacity_lbs,
    fuel_type,
    gasoline_fuel_grade,
    highway_fuel_economy_mpg,
    city_fuel_economy_mpg,
    combined_fuel_economy_mpg,
    fuel_capacity_gal,
    range_mi,
    electric_range_mi,
    full_recharge_time_hours,
    automatic_emergency_breaking,
    lane_departure_warning,
    collision_avoidance_assist,
    warranty_ev_battery_years,
    warranty_ev_battery_miles,
    warranty_drivetrain_years,
    warranty_drivetrain_miles,
    warranty_basic_years,
    warranty_basic_miles
  )

# Identify powertrain

# "Gasoline"    ""            "Hybrid"      "Electric"
# "Flex Fuel"   "Diesel"      "Fuel Cell"   "Natural Gas"

df <- df %>%
  mutate(
    powertrain = case_when(
      (fuel_type == 'Hybrid') & (electric_range_mi > 0) ~ 'phev',
      (fuel_type == 'Hybrid') & (electric_range_mi == 0) ~ 'hev',
      (fuel_type == 'Hybrid') & (is.na(electric_range_mi)) ~ 'hev',
      (fuel_type == 'Natural Gas') ~ 'cng',
      (fuel_type == 'Diesel') ~ 'diesel',
      (fuel_type == 'Fuel Cell') ~ 'fcev',
      (fuel_type == 'Electric') ~ 'bev',
      (fuel_type %in% c('Gasoline', 'Flex Fuel')) ~ 'cv',
      TRUE ~ NA
    )
  )

df %>%
  filter(powertrain == 'phev') %>%
  # filter(powertrain == 'hev') %>%
  distinct(model, electric_range_mi) %>%
  arrange(electric_range_mi)

arrow::write_parquet(df, file.path("data-raw", "data.parquet"))
