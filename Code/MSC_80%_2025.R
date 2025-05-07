#Date: 01/02/2025
#Author: Dabalà Alvise (additions by M Franken)
################################################################################
pacman::p_load(tidyverse, sf)
setwd("C:/Users/SANBI/OneDrive - University of Cape Town/Documents/2-25/VME_SCP")

#MSC TARGET#
######################### STEP 1: SETTING UP PLANNING UNITS ###############################

# Read EEZ file
EEZ_SA <- st_read("Data/EEZ/South_African_EEZ.shp")

# Check the CRS of the EEZ_SA object
print(st_crs(EEZ_SA))

# Transform EEZ_SA to a consistent projection
EEZ_SA_transformed <- EEZ_SA %>% 
  st_transform(crs = 4326)  # WGS84 standard geographic coordinate system

# Crop the extent to the EEZ boundary to avoid unnecessary grid creation
bbox <- st_bbox(EEZ_SA_transformed)

# Create grid with consistent CRS within the bounding box of the EEZ
grid_PUs <- st_make_grid(st_as_sfc(bbox), 
                         cellsize = c(0.1, 0.1),  # ~11 km at equator in degrees
                         what = "polygons") %>% 
  st_sf(crs = 4326)

# Filter planning units that are completely within the EEZ
within_EEZ <- st_within(grid_PUs, EEZ_SA_transformed, sparse = FALSE)

# Retain only planning units entirely within the EEZ
PUs <- grid_PUs[apply(within_EEZ, 1, any), ]

# Plot to verify
plot(st_geometry(EEZ_SA_transformed), col = "lightblue", main = "Planning Units within EEZ")
plot(st_geometry(PUs), add = TRUE, border = "red")

# Save final PUs
dir.create("Outputs/RDS/PUs/", recursive = TRUE, showWarnings = FALSE)
saveRDS(PUs, "Outputs/RDS/PUs/PUs.rds")

#########################STEP 2: ADD DISTRIBUTION DATA###########################

#########################ADD VME INDICATOR RECORDS###############################
#Read the file
VMEs_Indicator_taxa <- st_read("Data/VMEs/VME_Indicator_Record.shp")

#Read PUs
PUs <- readRDS("Outputs/RDS/PUs/PUs.rds")

#Join the VMEs indicator taxa
PUs_VMEs_Indicator_taxa <- PUs %>%
  st_join(VMEs_Indicator_taxa) %>% #join based on geometry
  dplyr::select(VME_Type) %>% #select the column of interest
  filter(!is.na(VME_Type)) #filter all the values that are not NAs

#Create a column of presence-absence
PUs_VMEs_Indicator_taxa <- PUs_VMEs_Indicator_taxa %>%
  mutate(values = 1) #make a column of 1

#Report that in wider tibble
PUs_VMEs_Indicator_taxa <- PUs_VMEs_Indicator_taxa %>%
  pivot_wider(names_from = "VME_Type", #names of the columns
              values_from = "values",
              values_fn = sum) #change to wide format
#All the columns that are not geometry

print(PUs_VMEs_Indicator_taxa, n = "all")

dir.create("Outputs/RDS/PUs_features", recursive = TRUE)
saveRDS(PUs_VMEs_Indicator_taxa,
        "Outputs/RDS/PUs_features/PUs_VMEs_Indicator_taxa.rds")

plot(st_geometry(PUs_VMEs_Indicator_taxa), main="PUs with VME Indicator Taxa")  # Visualization
# Plot all Planning Units (PUs) with a light fill color
ggplot() +
  geom_sf(data = PUs, fill = "white", color = "lightgrey",, size = 0.2) +
  geom_sf(data = PUs_VMEs_Indicator_taxa, fill = "red", color = "black", size = 0.2) +
  labs(title = "Planning Units with VME Indicator Taxa") +
  theme_minimal()

#Set targets
targets_VMEs_Indicator_taxa <- tibble(
  feature = (colnames(PUs_VMEs_Indicator_taxa) %>%
               setdiff("geometry")),
  target = 0.30
)

dir.create("Outputs/RDS/Targets/", recursive = TRUE)
saveRDS(targets_VMEs_Indicator_taxa,
        "Outputs/RDS/Targets/targets_VMEs_Indicator_taxa.rds")

#########################ADD VME VISUAL RECORDS###############################
VMEs_visual_survey <- st_read("Data/VMEs/VME_Visual_Record.shp")

# Rename VME_Type values to avoid duplicates
VMEs_visual_survey <- VMEs_visual_survey %>%
  mutate(VME_Type = recode(VME_Type, "Desmophyllum_VME_Indicator" = "Desmophyllum_VME_Visual"))

#Read PUs
PUs <- readRDS("Outputs/RDS/PUs/PUs.rds")

PUs_VMEs_visual_survey <- PUs %>% 
  st_join(VMEs_visual_survey) %>% 
  dplyr::select(VME_Type) %>% 
  filter(!is.na(VME_Type))

#Create a column of presence-absence
PUs_VMEs_visual_survey <- PUs_VMEs_visual_survey %>% 
  mutate(values = 1)

#Report that in wider tibble
PUs_VMEs_visual_survey <- PUs_VMEs_visual_survey %>% 
  pivot_wider(names_from = "VME_Type",
              values_from = "values", values_fn = sum)

saveRDS(PUs_VMEs_visual_survey, "Outputs/RDS/PUs_features/PUs_VMEs_visual_survey.rds")

plot(st_geometry(PUs_VMEs_visual_survey), main="PUs with VME Visual Survey Records")  # Visualization
# Plot all Planning Units (PUs) with a light fill color
ggplot() +
  geom_sf(data = PUs, fill = "white", color = "lightgrey", size = 0.2) +  # All PUs in grey
  geom_sf(data = PUs_VMEs_visual_survey, fill = "red", color = "black", size = 0.2) +  # Selected grids in red
  labs(title = "Planning Units with VME Visual Survey Records") +
  theme_minimal()

#Set targets
targets_VMEs_visual_survey <- tibble(
  feature = (colnames(PUs_VMEs_visual_survey) %>%
               setdiff("geometry")),
  target = 0.80
)

saveRDS(targets_VMEs_visual_survey,
        "Outputs/RDS/Targets/targets_VMEs_visual_survey.rds")

#########################ADD POTENTIAL VME VISUAL RECORDS###############################
PotentialVME_visual_survey <- st_read("Data/VMEs/PotentialVME_Visual_Record.shp")

# Rename VME_Type values to avoid duplicates
PotentialVME_visual_survey <- PotentialVME_visual_survey %>%
  mutate(VME_Type = recode(VME_Type, "Mixed_VME" = "Mixed_PotVME"))

#Read PUs
PUs <- readRDS("Outputs/RDS/PUs/PUs.rds")

PUs_PotentialVME_visual_survey <- PUs %>%
  st_join(PotentialVME_visual_survey) %>%
  dplyr::select(VME_Type) %>%
  filter(!is.na(VME_Type))

#Create a column of presence-absence
PUs_PotentialVME_visual_survey <- PUs_PotentialVME_visual_survey %>%
  mutate(values = 1)

#Report that in wider tibble
PUs_PotentialVME_visual_survey <- PUs_PotentialVME_visual_survey %>%
  pivot_wider(names_from = "VME_Type",
              values_from = "values", values_fn = sum)

saveRDS(PUs_PotentialVME_visual_survey, "Outputs/RDS/PUs_features/PUs_PotentialVME_visual_survey.rds")

plot(st_geometry(PUs_PotentialVME_visual_survey), main="PUs with Potential VME visual survey")  # Visualization
# Plot all Planning Units (PUs) with a light fill color
ggplot() +
  geom_sf(data = PUs, fill = "white", color = "lightgrey", size = 0.2) +  # All PUs in grey
  geom_sf(data = PUs_PotentialVME_visual_survey, fill = "red", color = "black", size = 0.2) +  # Selected grids in red
  labs(title = "Planning Units with Potential VME Visual Survey Records") +
  theme_minimal()

#Set targets
targets_PotentialVME_visual_survey <- tibble(
  feature = (colnames(PUs_PotentialVME_visual_survey) %>%
               setdiff("geometry")),
  target = 0.30
)

saveRDS(targets_PotentialVME_visual_survey,
        "Outputs/RDS/Targets/targets_PotentialVME_visual_survey.rds")
#########################ADD VME CATCH RECORDS###############################
VMEs_catch <- st_read("Data/VMEs/VME_Catch_Record.shp")

#Read PUs
PUs <- readRDS("Outputs/RDS/PUs/PUs.rds")

#Intersect with the PUs
PUs_VMEs_catch <- PUs %>%
  st_join(VMEs_catch) %>%
  dplyr::select(VME_Type) %>%
  filter(!is.na(VME_Type))

#Create a column of presence-absence
PUs_VMEs_catch <- PUs_VMEs_catch %>%
  mutate(values = 1)

#Report that in wider tibble
PUs_VMEs_catch <- PUs_VMEs_catch %>%
  pivot_wider(names_from = "VME_Type",
              values_from = "values", values_fn = sum)

saveRDS(PUs_VMEs_catch, "Outputs/RDS/PUs_features/PUs_VMEs_catch.rds")

plot(st_geometry(PUs_VMEs_catch), main="PUs with VME Catch")  # Visualization
# Plot all Planning Units (PUs) with a light fill color
ggplot() +
  geom_sf(data = PUs, fill = "white", color = "lightgrey", size = 0.2) +  # All PUs in grey
  geom_sf(data = PUs_VMEs_catch, fill = "red", color = "black", size = 0.2) +  # Selected grids in red
  labs(title = "Planning Units with VME Catch Records") +
  theme_minimal()

#Set targets
targets_VMEs_catch <- tibble(
  feature = (colnames(PUs_VMEs_catch) %>%
               setdiff("geometry")),
  target = 0.80)

saveRDS(targets_VMEs_catch,
        "Outputs/RDS/Targets/targets_VMEs_catch.rds")

#########################ADD VME SONAR RECORDS###############################
VMEs_sonar <- st_read("Data/VMEs/Coral_Mounds.shp")

#Read PUs
PUs <- readRDS("Outputs/RDS/PUs/PUs.rds")

#Intersect with the PUs
PUs_VMEs_sonar <- PUs %>%
  st_join(VMEs_sonar) %>%
  dplyr::select(VME_Type) %>%
  filter(!is.na(VME_Type))

#Create a column of presence-absence
PUs_VMEs_sonar <- PUs_VMEs_sonar %>%
  mutate(values = 1)

#Report that in wider tibble
PUs_VMEs_sonar <- PUs_VMEs_sonar %>%
  pivot_wider(names_from = "VME_Type",
              values_from = "values", values_fn = sum)

saveRDS(PUs_VMEs_sonar, "Outputs/RDS/PUs_features/PUs_VMEs_sonar.rds")

plot(st_geometry(PUs_VMEs_sonar), main="PUs with VME Sonar")  # Visualization

# Plot all Planning Units (PUs) with a light fill color
ggplot() +
  geom_sf(data = PUs, fill = "white", color = "lightgrey", size = 0.2) +  # All PUs in grey
  geom_sf(data = PUs_VMEs_sonar, fill = "red", color = "black", size = 0.2) +  # Selected grids in red
  labs(title = "Planning Units with VME Sonar") +
  theme_minimal()

#Set targets
targets_VMEs_sonar <- tibble(
  feature = (colnames(PUs_VMEs_sonar) %>%
               setdiff("geometry")),
  target = 0.80)
saveRDS(targets_VMEs_sonar, "Outputs/RDS/Targets/targets_VMEs_sonar.rds")

#########################ADD LOPHELIA#########################################
library(terra)
library(ggplot2)
library(rnaturalearth)
library(dplyr)
library(exactextractr)

# Load raster and spatial data
Desmophyllum <- terra::rast("Data/HSM/Predictive Modelling1.tif")
SA_sf <- rnaturalearth::ne_countries(country = "South Africa", returnclass = "sf")

# Convert raster to a data frame for plotting
Desmophyllum_df <- as.data.frame(Desmophyllum, xy = TRUE, na.rm = TRUE)
colnames(Desmophyllum_df)

ggplot() +
  geom_sf(data = SA_sf, fill = "transparent", color = "grey25", size = 0.2) +
  geom_raster(data = Desmophyllum_df, aes(x = x, y = y, fill = `Predictive Modelling1`)) +  # Use backticks for special characters
  scale_fill_viridis_c(na.value = NA) +
  labs(title = "Planning Units with Desmophyllum predictive modelling", fill = "Value") +
  theme_bw()


# Extract values to the PUs
PUs <- readRDS("Outputs/RDS/PUs/PUs.rds")

PUs_Desmophyllum <- PUs %>%
  mutate(prob_Desmophyllum = exact_extract(Desmophyllum, 
                                           PUs, 
                                           include_area = FALSE, 
                                           fun = 'max' #maximum value that intersect the PU
  )) %>% 
  mutate(presence_Desmophyllum = case_when(
    prob_Desmophyllum > 0.4 ~ 1, #Threshold to define presence or absence (at the moment 0.5)
    .default = 0
  )) %>% 
  dplyr::select(presence_Desmophyllum)

plot(PUs_Desmophyllum[, "presence_Desmophyllum"])

# Save
saveRDS(PUs_Desmophyllum, "Outputs/RDS/PUs_features/PUs_Desmophyllum.rds")

# Set targets
targets_Desmophyllum <- tibble(
  feature = (colnames(PUs_Desmophyllum) %>%
               setdiff("geometry")),
  target = 0.30)

saveRDS(targets_Desmophyllum, "Outputs/RDS/Targets/targets_Desmophyllum.rds")


#########################STEP 3: GROUP FEATURES###############################
pacman::p_load(sf, tidyverse, future.apply)

# Read PUs
PUs <- readRDS("Outputs/RDS/PUs/PUs.rds")

# Read intersection VMEs
name_files <- list.files("Outputs/RDS/PUs_features/", pattern = ".rds$", full.names = TRUE)

# Join all the features in the folder to the PUs
PUs_all_features <- future_lapply(name_files, function(name_file) {
  PUs_features_tibble <- readRDS(name_file)  # Read the file
  
  # Ensure the spatial extent matches PUs
  PUs_features_tibble <- st_join(PUs, PUs_features_tibble)
  
  return(PUs_features_tibble)
}) %>%
  dplyr::bind_rows()  # Use dplyr::bind_rows to concatenate the results

# Identify numeric and character columns
numeric_cols <- sapply(PUs_all_features, is.numeric)
character_cols <- sapply(PUs_all_features, is.character)

#Transform all the NAs to 0
PUs_all_features <- PUs_all_features %>% 
  mutate(
    across(everything(), ~replace_na(.x, 0)) #replace all the NAs with zero
  )

# Save the updated dataset
dir.create("Outputs/RDS/PUs_all_features", recursive = TRUE)
saveRDS(PUs_all_features, "Outputs/RDS/PUs_all_features/PUs_all_features.rds")

#########################STEP 4: COST LAYER###############################
#Read PUs
PUs_all_features <- readRDS("Outputs/RDS/PUs_all_features/PUs_all_features.rds")

#read cost
cost <- st_read("Data/cost/iAtl_Cost_Kerry.shp")

#intersect the cost with the PUs
intersection <- PUs_all_features %>%
  st_transform(st_crs(cost)) %>% #project to the cost CRS
  st_intersects(cost)

#calculate the total cost in each PU
total_cost_per_PU <- future_lapply(1:length(intersection), function(i) {
  sum(cost$iA3_xP_CST[intersection[[i]]]) #total value of cost in each PU
}) %>%
  unlist()

#Mutate to cost
PUs_all_features_cost <- PUs_all_features %>%
  mutate(cost = total_cost_per_PU) %>%
  filter(!is.na(cost)) #remove PUs that do not intersect the cost layer

plot(PUs_all_features_cost[, "cost"])

#Save
saveRDS(PUs_all_features_cost, "Outputs/RDS/PUs_all_features/PUs_all_features_cost.rds")

dir.create("Outputs/shp/PUs", recursive = TRUE)
st_write(PUs_all_features_cost, "Outputs/shp/PUs/PUs_all_features_cost.shp",
         append = TRUE)

#########################STEP 5: RUN PrioritizeR###############################
# Load required packages
pacman::p_load(gurobi, prioritizr, tidyverse, sf, future.apply)

# Read PUs
PUs_all_features_cost <- readRDS("Outputs/RDS/PUs_all_features/PUs_all_features_cost.rds")


# Get feature names
names_features <- PUs_all_features_cost %>%
  names() %>%
  setdiff(c("geometry", "cost"))

# Load targets
targets_files_names <- list.files("Outputs/RDS/Targets/", full.names = TRUE) %>%
  lapply(readRDS) %>%
  bind_rows()

# Diagnostic prints
print(paste("Number of features:", length(names_features)))
print(paste("Number of filtered targets:", nrow(targets_files_names)))


#Minimum set objective problem
problem_01 <- problem(PUs_all_features_cost, 
                      features = names_features, cost_column = "cost") %>% 
  add_relative_targets(targets_files_names$target) %>% 
  add_min_set_objective() %>% 
  add_gurobi_solver(gap = 0.1, threads = 4)

solution_80 <- solve(problem_01, force = TRUE)

plot(solution_80[, "solution_1"], main = "Solution 80")

# Create visualization plots
ggplot() +
  geom_sf(data = solution_80, aes(fill = factor(solution_1))) +
  geom_sf(data = EEZ_SA_transformed, fill = "lightblue", alpha = 0.3) +
  scale_fill_manual(values = c("white", "darkgreen"),
                    labels = c("Not Selected", "Selected")) +
  labs(title = "Conservation Solution for Scenario 2(MSC)",
       fill = "Status") +
  theme_minimal()

####################### EXPORT SOLUTION 80 RESULTS #######################

# Calculate solution statistics
solution_stats <- tibble(
  metric = c(
    "Total Area Selected (km²)",
    "Number of Planning Units",
    "Total Cost"
  ),
  value = c(
    as.numeric(sum(st_area(solution_80[solution_80$solution_1 == 1,]))) / 1000000,
    sum(solution_80$solution_1),
    sum(solution_80$cost[solution_80$solution_1 == 1])
  )
)

# Calculate feature representation
feature_stats <- data.frame(
  feature = names_features,
  total_area = sapply(names_features, function(f) sum(PUs_all_features_cost[[f]] > 0)),
  protected_area = sapply(names_features, function(f) 
    sum(PUs_all_features_cost[[f]][solution_80$solution_1 == 1] > 0)),
  protection_percentage = sapply(names_features, function(f) 
    sum(PUs_all_features_cost[[f]][solution_80$solution_1 == 1] > 0) / 
      sum(PUs_all_features_cost[[f]] > 0) * 100)
)

# Evaluate target achievement
target_achievement <- problem_01 %>%
  eval_target_coverage_summary(data.frame(solution_1 = solution_80$solution_1))

# Save results
dir.create("Outputs/Results", recursive = TRUE, showWarnings = FALSE)
write.csv(solution_stats, "Outputs/Results/solution80_statistics.csv")
write.csv(feature_stats, "Outputs/Results/solution80_feature_representation.csv")
write.csv(target_achievement, "Outputs/Results/solution80_target_achievement.csv")

