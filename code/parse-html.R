library(tidyverse)
library(rvest)

get_page_table <- function(path) {
  table <- read_html(path) %>%
    html_nodes("table") %>%
    html_table(header = TRUE, trim = TRUE)
  return(table)
}

root <- "/Users/jhelvy/Desktop/html/"
files <- list.files(root)
pages <- paste0(root, files)

data <- list()
for (i in 1:length(pages)) {
  cat(i, "\n")
  data[[i]] <- get_page_table(pages[i])[[2]] %>%
    janitor::clean_names()
}

df <- do.call(rbind, data)

# Fix formatting


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

temp <- df %>%
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
    payload_capacity_lbs = parse_number(payload_capacity),
    max_cargo_capacity_cubic_ft = parse_number(max_cargo_capacity),
    msrp = parse_number(msrp),
    invoice_price = parse_number(invoice_price),
    used_new_price = parse_number(used_new_price),
    used_price = parse_number(used_price),
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
    cargo_capacity_in = parse_number(cargo_capacity),
    front_headroom_in = parse_number(front_headroom),
    front_legroom_in = parse_number(front_legroom),
    front_shoulder_room_in = parse_number(front_shoulder_room),
    front_hip_room_in = parse_number(front_hip_room),
    rear_headroom_in = parse_number(rear_headroom),
    rear_legroom_in = parse_number(rear_legroom),
    highway_fuel_economy_mpg = parse_number(highway_fuel_economy),
    city_fuel_economy_mpg = parse_number(city_fuel_economy),
    combined_fuel_economy_mpg = parse_number(combined_fuel_economy),
    fuel_capacity_gal = parse_number(fuel_capacity),
    warranty_rust_miles = parse_number(rust_miles),
    warranty_rust_years = parse_number(rust_years),
    warranty_free_maintenance_miles = parse_number(free_maintenance_miles),
    warranty_free_maintenance_years = parse_number(free_maintenance_years),
    warranty_ev_battery_years = parse_number(ev_battery_years),
    warranty_ev_battery_miles = parse_number(ev_battery_miles),
    warranty_roadside_assistance_years = parse_number(roadside_assistance_years),
    warranty_roadside_assistance_miles = parse_number(roadside_assistance_miles),
    warranty_drivetrain_years = parse_number(drivetrain_years),
    warranty_drivetrain_miles = parse_number(drivetrain_miles),
    warranty_basic_years = parse_number(basic_years),
    warranty_basic_miles = parse_number(basic_miles),
  ) %>%
  fix_binary(paddle_shifters) %>%
  fix_binary(tiptronic) %>%
  fix_binary(limited_slip_differential) %>%
  fix_binary(cvt) %>%
  fix_binary(engine_immobilizer) %>%
  fix_binary(alarm_system) %>%
  fix_binary(android_auto) %>%
  fix_binary(rear_wipers) %>%
  fix_binary(heated_windshield) %>%
  fix_binary(rain_sensing_wipers) %>%
  fix_binary(hands_free_entry) %>%
  fix_binary(parking_assist) %>%
  fix_binary(rear_tow_hooks) %>%
  fix_binary(spare_tire) %>%
  fix_binary(convertible_roof) %>%
  fix_binary(front_tow_hooks) %>%
  fix_binary(roof_rack) %>%
  fix_binary(rear_spoiler) %>%
  fix_binary(descent_control) %>%
  fix_binary(brake_drying) %>%
  fix_binary(tilting_steering_wheel) %>%
  fix_binary(hill_start_assist) %>%
  fix_binary(heated_steering_wheel) %>%
  fix_binary(telescopic_steering_wheel) %>%
  fix_binary(adjustable_pedals) %>%
  fix_binary(boost_gauge) %>%
  fix_binary(navigation_system) %>%
  fix_binary(compass) %>%
  fix_binary(nav_w_real_time_traffic) %>%
  fix_binary(power_outlets) %>%
  fix_binary(air_conditioning_dual_zones) %>%
  fix_binary(stability_control) %>%
  fix_binary(traction_control) %>%
  fix_binary(air_conditioning_sun_sensor) %>%
  fix_binary(side_view_camera) %>%
  fix_binary(front_view_camera) %>%
  fix_binary(wireless_charging) %>%
  fix_binary(rear_view_camera) %>%
  fix_binary(automatic_parking_assist) %>%
  fix_binary(power_windows) %>%
  fix_binary(sunroof) %>%
  fix_binary(rear_cupholders) %>%
  fix_binary(tire_pressure_monitoring) %>%
  fix_binary(trip_computer) %>%
  fix_binary(heads_up_display) %>%
  fix_binary(outdoor_temperature_gauge) %>%
  fix_binary(cruise_control) %>%
  fix_binary(transmission_temperature_gauge) %>%
  fix_binary(adaptive_cruise_control) %>%
  fix_binary(remote_door_unlock) %>%
  fix_binary(remote_engine_start) %>%
  fix_binary(keyless_ignition) %>%
  fix_binary(video_system) %>%
  fix_binary(sca_rollover_sensor) %>%
  fix_binary(breaking_assist) %>%
  fix_binary(automatic_emergency_breaking) %>%
  fix_binary(post_collision_braking) %>%
  fix_binary(lane_departure_warning) %>%
  fix_binary(blind_spot_warning) %>%
  fix_binary(collision_avoidance_assist) %>%
  fix_binary(front_curtain_airbags) %>%
  fix_binary(front_side_airbags) %>%
  fix_binary(rear_curtain_airbags) %>%
  fix_binary(rear_side_airbags) %>%
  fix_binary(leather_seats) %>%
  fix_binary(heated_driver_seat) %>%
  fix_binary(heated_front_passenger_seat) %>%
  fix_binary(ventilated_driver_seat) %>%
  fix_binary(ventilated_front_passenger_seat) %>%
  fix_binary(power_driver_seat) %>%
  fix_binary(power_front_passenger_seat) %>%
  fix_binary(ride_adjustable_suspension) %>%
  fix_binary(bluetooth) %>%
  fix_binary(apple_car_play) %>%
  fix_binary(speed_sensitive_wipers) %>%
  fix_binary(rear_defogger) %>%
  fix_binary(x2_stage_unlocking) %>%
  fix_binary(emergency_trunk_release) %>%
  select(
    make,
    model,
    year,
    trim,
    msrp,
    invoice_price,
    used_new_price,
    used_price,
    body_size,
    body_style,
    number_of_doors,
    curb_weight_lbs,
    vehicle_length_in,
    vehicle_width_in,
    vehicle_height_in,
    wheelbase_in,
    ground_clearance_in,
    passenger_capacity,
    cargo_capacity_in,
    max_cargo_capacity_cubic_ft,
    front_headroom_in,
    front_legroom_in,
    front_shoulder_room_in,
    front_hip_room_in,
    rear_headroom_in,
    rear_legroom_in,
    engine_aspiration,
    cylinders,
    transmission,
    cvt,
    drivetrain,
    horsepower,
    horsepower_rpm,
    torque_ft_lbs,
    torque_rpm,
    power_to_weight_ratio_in,
    towing_capacity_lbs,
    payload_capacity_lbs,
    paddle_shifters,
    tiptronic,
    limited_slip_differential,
    fuel_type,
    gasoline_fuel_grade,
    highway_fuel_economy_mpg,
    city_fuel_economy_mpg,
    combined_fuel_economy_mpg,
    fuel_capacity_gal,
    range_mi,
    electric_range_mi,
    full_recharge_time_hours,
    leather_seats,
    heated_driver_seat,
    heated_front_passenger_seat,
    ventilated_driver_seat,
    ventilated_front_passenger_seat,
    power_driver_seat,
    power_front_passenger_seat,
    driver_seat_adjustability,
    passenger_seat_adjustability,
    ride_adjustable_suspension,
    bluetooth,
    apple_car_play,
    speed_sensitive_wipers,
    rear_defogger,
    x2_stage_unlocking,
    engine_immobilizer,
    alarm_system,
    android_auto,
    rear_wipers,
    heated_windshield,
    rain_sensing_wipers,
    hands_free_entry,
    parking_assist,
    rear_tow_hooks,
    spare_tire,
    convertible_roof,
    front_tow_hooks,
    roof_rack,
    rear_spoiler,
    descent_control,
    brake_drying,
    tilting_steering_wheel,
    hill_start_assist,
    heated_steering_wheel,
    telescopic_steering_wheel,
    adjustable_pedals,
    boost_gauge,
    navigation_system,
    compass,
    nav_w_real_time_traffic,
    power_outlets,
    air_conditioning_dual_zones,
    air_conditioning_sun_sensor,
    stability_control,
    traction_control,
    side_view_camera,
    front_view_camera,
    wireless_charging,
    rear_view_camera,
    automatic_parking_assist,
    power_windows,
    sunroof,
    rear_cupholders,
    tire_pressure_monitoring,
    trip_computer,
    heads_up_display,
    outdoor_temperature_gauge,
    cruise_control,
    adaptive_cruise_control,
    transmission_temperature_gauge,
    remote_door_unlock,
    remote_engine_start,
    keyless_ignition,
    video_system,
    sca_rollover_sensor,
    breaking_assist,
    automatic_emergency_breaking,
    post_collision_braking,
    lane_departure_warning,
    blind_spot_warning,
    head_restraints_rear_crash,
    small_overlap_passenger_side,
    roof_strength,
    moderate_overlap_driver_side,
    side_crash,
    small_overlap_driver_side,
    emergency_trunk_release,
    collision_avoidance_assist,
    front_curtain_airbags,
    front_side_airbags,
    rear_curtain_airbags,
    rear_side_airbags,
    warranty_rust_miles,
    warranty_rust_years,
    warranty_free_maintenance_miles,
    warranty_free_maintenance_years,
    warranty_ev_battery_years,
    warranty_ev_battery_miles,
    warranty_roadside_assistance_years,
    warranty_roadside_assistance_miles,
    warranty_drivetrain_years,
    warranty_drivetrain_miles,
    warranty_basic_years,
    warranty_basic_miles,
    hybrid_electric_components_years,
    hybrid_electric_components_miles
  )

arrow::write_parquet(temp, file.path("data-raw", "data.parquet"))
