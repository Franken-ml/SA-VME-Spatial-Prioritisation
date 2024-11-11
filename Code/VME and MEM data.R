################################################################################
# Modified Spatial Conservation Prioritization with VME and MEM data
################################################################################

# Previous package loading code remains the same...

################################################################################
# Date: 02/11/2024
# Spatial Conservation Prioritization with VME and MEM data
################################################################################

# Load required packages
pacman::p_load(tidyverse, sf, future.apply, prioritizr, gurobi)

library(sf)
library(dplyr)
library(tidyr)

#########################STEP 1: SETTING UP PLANNING UNIT#########################
# Read EEZ file
EEZ_SA <- st_read("Data/EEZ/eez.shp")

# Produce a grid
grid_PUs <- st_make_grid(EEZ_SA,
                         cellsize = c(0.1, 0.1)) %>%
  st_sf()

# Intersect grid and EEZ
intersection <- st_intersects(grid_PUs, EEZ_SA) %>%
  lengths > 0

# Filter only PUs that intersect the EEZ
PUs <- grid_PUs %>%
  filter(intersection)

# Save PUs
dir.create("Outputs/RDS/PUs/", recursive = TRUE)
saveRDS(PUs, "Outputs/RDS/PUs/PUs.rds")

#########################STEP 2: PROCESS ALL FEATURES############################

# Function to process point data
process_point_data <- function(file_path, value_col = "VME_Type", suffix = "") {
  data <- st_read(file_path)

  processed <- PUs %>%
    st_join(data) %>%
    dplyr::select(all_of(value_col)) %>%
    filter(!is.na(!!sym(value_col))) %>%
    mutate(values = 1) %>%
    pivot_wider(names_from = value_col,
                values_from = "values",
                values_fn = sum)

  if (suffix != "") {
    processed <- processed %>%
      rename_with(~paste0(., "_", suffix), -geometry)
  }

  return(processed)
}

# Process Marine Ecosystem Map (polygon data)
process_MEM <- function() {
  MEM <- st_read("Data/VMEs/MEM.shp")

  processed <- PUs %>%
    st_intersection(MEM) %>%
    mutate(
      area_proportion = as.numeric(st_area(geometry) / st_area(PUs[1,])),
      values = area_proportion
    ) %>%
    dplyr::select(VME_Type) %>%  # Adjust column name if different
    pivot_wider(names_from = "VME_Type",
                values_from = "values",
                values_fn = sum) %>%
    rename_with(~paste0(., "_MEM"), -geometry)

  return(processed)
}

# Process all point datasets
PUs_VME_catch <- process_point_data("Data/VMEs/VME_Catch_Record.shp", suffix = "catch")
PUs_VME_visual <- process_point_data("Data/VMEs/VME_Visual_Record.shp", suffix = "visual")
PUs_VME_indicator <- process_point_data("Data/VMEs/VME_Indicator_Record.shp", suffix = "indicator")
PUs_MEM <- process_MEM()

# Function to process point data with proper CRS handling
process_point_data <- function(file_path, suffix) {
  # Read the point data
  points <- st_read(file_path) %>%
    # Ensure CRS is WGS84
    st_transform(4326)

  # Create a unique ID for each point
  points$point_id <- paste0(suffix, "_", seq_len(nrow(points)))

  return(points)
}

# Process all datasets with proper CRS handling
PUs_VME_catch <- process_point_data("Data/VMEs/VME_Catch_Record.shp", "catch")
PUs_VME_visual <- process_point_data("Data/VMEs/VME_Visual_Record.shp", "visual")
PUs_VME_indicator <- process_point_data("Data/VMEs/VME_Indicator_Record.shp", "indicator")

# Process MEM data
PUs_MEM <- st_read("Data/VMEs/MEM.shp") %>%
  st_transform(4326)  # Transform to WGS84
################################################################################
# Modified Data Processing with proper CRS handling
################################################################################

# Modified process_MEM function
process_MEM <- function(PUs) {
  message("Processing MEM data...")

  # Read MEM data and transform to WGS84
  MEM <- st_read("Data/VMEs/MEM.shp") %>%
    st_make_valid() %>%  # Fix any invalid geometries
    st_transform(4326)   # Transform to WGS84

  # Ensure PUs is in WGS84
  PUs_wgs84 <- PUs %>%
    st_transform(4326)

  # Process with proper error handling
  processed <- tryCatch({
    PUs_wgs84 %>%
      st_intersection(MEM) %>%
      mutate(
        area_proportion = as.numeric(st_area(geometry) / st_area(PUs_wgs84[1,])),
        values = area_proportion
      ) %>%
      dplyr::select(VME_Type) %>%
      group_by(geometry) %>%
      summarize(
        across(everything(), sum),
        .groups = "drop"
      ) %>%
      rename_with(~paste0(., "_MEM"), -geometry)
  }, error = function(e) {
    message("Error in MEM processing: ", e$message)
    return(NULL)
  })

  return(processed)
}

# Modified point data processing function
process_point_data <- function(file_path, value_col = "VME_Type", suffix = "") {
  message(paste("Processing", suffix, "data..."))

  # Read and transform point data
  data <- st_read(file_path) %>%
    st_make_valid() %>%
    st_transform(4326)  # Ensure WGS84

  # Create buffer around points (e.g., 1km buffer)
  data_buffered <- st_buffer(data, dist = 0.01)  # approximately 1km at equator

  # Process with proper error handling
  processed <- tryCatch({
    PUs %>%
      st_transform(4326) %>%  # Transform PUs to WGS84
      st_join(data_buffered) %>%
      dplyr::select(all_of(value_col)) %>%
      filter(!is.na(!!sym(value_col))) %>%
      mutate(values = 1) %>%
      group_by(geometry) %>%
      summarize(
        across(everything(), sum),
        .groups = "drop"
      ) %>%
      pivot_wider(
        names_from = value_col,
        values_from = "values",
        values_fill = 0
      )

    if (suffix != "") {
      processed <- processed %>%
        rename_with(~paste0(., "_", suffix), -geometry)
    }

    return(processed)
  }, error = function(e) {
    message("Error in point data processing: ", e$message)
    return(NULL)
  })

  return(processed)
}

# Main processing workflow
main_processing <- function() {
  # Create output directories
  dir.create("Outputs/RDS/PUs/", recursive = TRUE, showWarnings = FALSE)
  dir.create("Outputs/logs/", recursive = TRUE, showWarnings = FALSE)

  # Start logging
  log_file <- file("Outputs/logs/processing_log.txt", "w")
  sink(log_file, append = TRUE)
  sink(log_file, append = TRUE, type = "message")

  # Process EEZ and create PUs
  message("Processing EEZ and creating planning units...")
  EEZ_SA <- st_read("Data/EEZ/eez.shp") %>%
    st_make_valid() %>%
    st_transform(4326)  # Transform to WGS84

  # Create grid in WGS84
  grid_PUs <- st_make_grid(EEZ_SA,
                           cellsize = c(0.1, 0.1),
                           crs = 4326) %>%
    st_sf()

  # Intersect grid and EEZ
  intersection <- st_intersects(grid_PUs, EEZ_SA) %>%
    lengths > 0

  # Filter PUs
  PUs <- grid_PUs %>%
    filter(intersection) %>%
    st_make_valid()

  # Process all datasets
  PUs_VME_catch <- process_point_data("Data/VMEs/VME_Catch_Record.shp",
                                      suffix = "catch")
  PUs_VME_visual <- process_point_data("Data/VMEs/VME_Visual_Record.shp",
                                       suffix = "visual")
  PUs_VME_indicator <- process_point_data("Data/VMEs/VME_Indicator_Record.shp",
                                          suffix = "indicator")
  PUs_MEM <- process_MEM(PUs)

  # Combine features with error handling
  message("Combining all features...")
  PUs_all_features <- PUs %>%
    st_transform(4326) %>%
    {if(!is.null(PUs_VME_catch)) left_join(., PUs_VME_catch, by = "geometry") else .} %>%
    {if(!is.null(PUs_VME_visual)) left_join(., PUs_VME_visual, by = "geometry") else .} %>%
    {if(!is.null(PUs_VME_indicator)) left_join(., PUs_VME_indicator, by = "geometry") else .} %>%
    {if(!is.null(PUs_MEM)) left_join(., PUs_MEM, by = "geometry") else .} %>%
    mutate(across(where(is.numeric), ~replace_na(.x, 0)))

  # Validate final output
  if(nrow(PUs_all_features) > 0) {
    message("Successfully created combined feature dataset")
    saveRDS(PUs_all_features, "Outputs/RDS/PUs/PUs_all_features.rds")
  } else {
    stop("Error: No features were successfully combined")
  }

  # Close logging
  sink(type = "message")
  sink()
  close(log_file)

  return(PUs_all_features)
}

# Run the main processing
PUs_all_features <- main_processing()


################################################################################
# Optimized Spatial Joining Process
################################################################################

# First, optimize the PUs and create spatial index
PUs_optimized <- PUs %>%
  st_transform(4326) %>%
  st_make_valid()

# Function for optimized point data processing
process_point_data_fast <- function(file_path, PUs, value_col = "VME_Type", suffix = "") {
  message(paste("Processing", suffix, "data..."))

  # Read points and create binary indicator
  points <- st_read(file_path) %>%
    st_transform(4326) %>%
    select(all_of(value_col)) %>%
    mutate(presence = 1)

  # Use faster st_intersects instead of st_join
  intersections <- st_intersects(PUs, points)

  # Convert to presence/absence matrix
  presence_matrix <- lapply(intersections, function(x) {
    if(length(x) > 0) {
      unique_types <- unique(points[[value_col]][x])
      setNames(rep(1, length(unique_types)), unique_types)
    } else {
      NULL
    }
  })

  # Convert to data frame efficiently
  result_df <- data.frame(
    geometry = st_geometry(PUs)
  )

  # Get unique VME types
  all_types <- unique(points[[value_col]])

  # Initialize columns with zeros
  for(type in all_types) {
    col_name <- if(suffix != "") paste0(type, "_", suffix) else type
    result_df[[col_name]] <- 0
  }

  # Fill in presence values
  for(i in seq_along(presence_matrix)) {
    if(!is.null(presence_matrix[[i]])) {
      for(type in names(presence_matrix[[i]])) {
        col_name <- if(suffix != "") paste0(type, "_", suffix) else type
        result_df[i, col_name] <- 1
      }
    }
  }

  # Convert back to sf object
  st_sf(result_df)
}

# Function for optimized MEM processing
process_MEM_fast <- function(PUs, mem_file = "Data/VMEs/MEM.shp") {
  message("Processing MEM data...")

  # Read and prepare MEM data
  MEM <- st_read(mem_file) %>%
    st_transform(4326) %>%
    st_make_valid() %>%
    select(VME_Type)

  # Use st_intersects for initial filtering
  intersections <- st_intersects(PUs, MEM)

  # Create result dataframe
  result_df <- data.frame(
    geometry = st_geometry(PUs)
  )

  # Get unique MEM types
  mem_types <- unique(MEM$VME_Type)

  # Initialize columns
  for(type in mem_types) {
    result_df[[paste0(type, "_MEM")]] <- 0
  }

  # Calculate areas only for intersecting features
  for(i in seq_along(intersections)) {
    if(length(intersections[[i]]) > 0) {
      # Get intersecting MEM polygons
      mem_subset <- MEM[intersections[[i]], ]

      # Calculate intersection
      intersection <- st_intersection(PUs[i,], mem_subset)

      if(nrow(intersection) > 0) {
        # Calculate area proportions
        areas <- st_area(intersection)
        pu_area <- st_area(PUs[i,])

        # Update result for each type
        for(type in unique(intersection$VME_Type)) {
          type_areas <- areas[intersection$VME_Type == type]
          result_df[i, paste0(type, "_MEM")] <- sum(type_areas) / pu_area
        }
      }
    }
  }

  # Convert back to sf object
  st_sf(result_df)
}

# Main processing function
process_all_features_fast <- function() {
  message("Starting optimized processing...")
  start_time <- Sys.time()

  # Process point data
  PUs_catch <- process_point_data_fast("Data/VMEs/VME_Catch_Record.shp",
                                       PUs_optimized,
                                       suffix = "catch")
  message("Catch data processed...")

  PUs_visual <- process_point_data_fast("Data/VMEs/VME_Visual_Record.shp",
                                        PUs_optimized,
                                        suffix = "visual")
  message("Visual data processed...")

  PUs_indicator <- process_point_data_fast("Data/VMEs/VME_Indicator_Record.shp",
                                           PUs_optimized,
                                           suffix = "indicator")
  message("Indicator data processed...")

  # Process MEM data
  PUs_mem <- process_MEM_fast(PUs_optimized)
  message("MEM data processed...")

  # Combine all features efficiently
  PUs_all_features <- PUs_optimized %>%
    # Join all features using base R column binding
    bind_cols(
      st_drop_geometry(PUs_catch),
      st_drop_geometry(PUs_visual),
      st_drop_geometry(PUs_indicator),
      st_drop_geometry(PUs_mem)
    ) %>%
    # Remove duplicate geometry columns
    select(-contains("geometry")) %>%
    # Add back the geometry
    st_sf(geometry = st_geometry(PUs_optimized))

  end_time <- Sys.time()
  message("Total processing time: ", difftime(end_time, start_time, units = "mins"), " minutes")

  return(PUs_all_features)
}

# Run the optimized processing
PUs_all_features <- process_all_features_fast()


# Combine all features
PUs_all_features <- PUs %>%
  left_join(PUs_VME_catch, by = "geometry") %>%
  left_join(PUs_VME_visual, by = "geometry") %>%
  left_join(PUs_VME_indicator, by = "geometry") %>%
  left_join(PUs_MEM, by = "geometry") %>%
  mutate(across(everything(), ~replace_na(.x, 0)))

#########################STEP 3: ADD COST LAYER#################################
# Read cost layer
cost <- st_read("Data/cost/iAtl_Cost_Kerry.shp")

# Calculate total cost per PU
intersection_cost <- PUs_all_features %>%
  st_transform(st_crs(cost)) %>%
  st_intersects(cost)

total_cost_per_PU <- future_lapply(1:length(intersection_cost), function(i) {
  sum(cost$iA3_xP_CST[intersection_cost[[i]]])
}) %>%
  unlist()

# Add cost to features
PUs_all_features_cost <- PUs_all_features %>%
  mutate(cost = total_cost_per_PU) %>%
  filter(!is.na(cost))

# Save processed data
saveRDS(PUs_all_features_cost, "Outputs/RDS/PUs_features/PUs_all_features_cost.rds")

#########################STEP 4: MODIFIED TARGETS#############################
# Get feature names
names_features <- PUs_all_features_cost %>%
  names() %>%
  setdiff(c("geometry", "cost"))

# Create more nuanced targets dataframe with different scenarios
targets_data <- tibble(
  feature = names_features,
  # Scenario 1: Differentiated targets based on data reliability and ecological significance
  target_conservative = case_when(
    str_detect(feature, "_MEM$") ~ 0.2,  # Include some MEM representation
    str_detect(feature, "_visual$") ~ 0.7, # High confidence in visual data
    str_detect(feature, "_catch$") ~ 0.4,  # Moderate confidence
    str_detect(feature, "_indicator$") ~ 0.2, # Lower confidence
    TRUE ~ 0.3
  ),
  # Scenario 2: Higher protection targets
  target_ambitious = case_when(
    str_detect(feature, "_MEM$") ~ 0.3,
    str_detect(feature, "_visual$") ~ 0.9,
    str_detect(feature, "_catch$") ~ 0.6,
    str_detect(feature, "_indicator$") ~ 0.4,
    TRUE ~ 0.5
  )
)

#########################STEP 5: MODIFIED PRIORITIZR RUNS#####################
# Basic problem setup with cost threshold
problem_base <- problem(PUs_all_features_cost,
                        features = names_features,
                        cost_column = "cost")

# Solution 1: Conservative targets with cost threshold
problem_01 <- problem_base %>%
  add_relative_targets(targets_data$target_conservative) %>%
  add_min_set_objective() %>%
  add_max_utility_objective() %>%
  add_cost_ceiling(ceiling = quantile(PUs_all_features_cost$cost, 0.75)) %>%
  add_gurobi_solver(gap = 0.1, threads = 4)

# Solution 2: Conservative targets with connectivity
problem_02 <- problem_01 %>%
  add_connectivity_penalties(penalty = 0.005) %>%
  add_boundary_penalties(penalty = 0.01, edge_factor = 0.5)

# Solution 3: Ambitious targets with connectivity
problem_03 <- problem_base %>%
  add_relative_targets(targets_data$target_ambitious) %>%
  add_min_set_objective() %>%
  add_connectivity_penalties(penalty = 0.01) %>%
  add_boundary_penalties(penalty = 0.02, edge_factor = 0.7)

# Solution 4: Multi-zone approach
problem_04 <- problem_base %>%
  add_relative_targets(targets_data$target_conservative) %>%
  add_min_shortfall_objective() %>%
  add_bounded_constraints(
    lower = 0.2,  # Minimum 20% of planning units
    upper = 0.4   # Maximum 40% of planning units
  ) %>%
  add_connectivity_penalties(penalty = 0.008)

# Solve all problems
solution_01 <- solve(problem_01, force = TRUE)
solution_02 <- solve(problem_02, force = TRUE)
solution_03 <- solve(problem_03, force = TRUE)
solution_04 <- solve(problem_04, force = TRUE)

# Compare solutions
solutions_comparison <- bind_cols(
  "conservative" = eval_cost_summary(problem_01, solution_01[, "solution_1"]),
  "connected" = eval_cost_summary(problem_02, solution_02[, "solution_1"]),
  "ambitious" = eval_cost_summary(problem_03, solution_03[, "solution_1"]),
  "multi_zone" = eval_cost_summary(problem_04, solution_04[, "solution_1"])
)

# Save solutions and comparison
saveRDS(solutions_comparison, "Outputs/RDS/Solutions/solutions_comparison.rds")
