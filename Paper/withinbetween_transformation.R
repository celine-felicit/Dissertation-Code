#PREPARATION#
#Loading relevant packages
library(readxl) # For reading Excel files
library(plyr)  # For mapvalues function
library(dplyr) # For data manipulation
library(tidyr) # For data tidying
library(car)  # For VIF function
library(ggplot2) # For data visualization
library(ggpattern) # For patterned bar plots
library(gt) # For creating tables
library(magick) # For image manipulation
library(kableExtra) # For creating tables
library(modelsummary) # For creating summary tables
library(panelr) # For panel data analysis

#Load datasets into R
#1. UCDP Dyadic Dataset
ucdp_dyadic <- read.csv("~/Studium/Dissertation/Dissertation Code/Datasets/ucdp-dyadic-181.csv")

#2. UCDP ESD Dataset
# Read the first sheet from the Excel file
ucdp_esd <- read_excel("~/Studium/Dissertation/Dissertation Code/Datasets/ucdp-esd-dy-181.xlsx")

# COMBINATION OF DATSETS#
# Step 1. Merge UCDP ESD and UCDP dyadic datasets based on conflict dyad ID
# Check whether rows are unique in ESD dataset
distinct(ucdp_esd) #Yes, all unique

#check number of unique combinations
#checking for a "primary key"
nrow(ucdp_dyadic %>% distinct(dyad_id, year))
nrow(ucdp_esd %>% distinct(dyad_id,year))

ucdp_esd %>% distinct(dyad_id, year) #all unique -> can merge the two datasets based on these two variables

# Merge UCDP dyadic and UCDP ESD datasets on 'dyad_id' and 'year'
merged_ucdp <- ucdp_dyadic %>%
  inner_join(ucdp_esd, by = c("dyad_id", "year"))

#Check whether rows are unique
nrow(distinct(merged_ucdp)) #Yes

#before removing duplicate columns, check with summary statistics if the 
#values are the same in the columns for example the code below gives you the
#name of the columns that are duplicates the code give all those that have 
#.y at the end which means there is another column with the same name but 
#with .x at the end

merged_ucdp %>% 
  dplyr::select(ends_with(".y")) %>%
  names()

#Check which variables are the same in both datasets
intersect(names(ucdp_dyadic), names (ucdp_esd))

#check which columns have matching names and values in both datasets
common_columns <-intersect(colnames(ucdp_dyadic), colnames(ucdp_esd))

for (col in common_columns) {
  if (length(intersect(ucdp_dyadic[[col]], ucdp_esd[[col]])) > 0) {
    print(paste("Column", col, "has matching values"))
  } else {
    print(paste("Column", col, "no matching values"))
  }
}

#Compare values for common columns in both datasets
# Define the common columns you want to compare
common_columns <- c("conflict_id", "location", "side_a", "side_a_id", 
                    "side_b", "side_b_id", "gwno_a", "gwno_b")

# Loop through each common column to compare pairs
for (col_name in common_columns) {
  col_x <- paste0(col_name, ".x")
  col_y <- paste0(col_name, ".y")
  
  # Check if the columns are identical
  if (all(merged_ucdp[[col_x]] == merged_ucdp[[col_y]], na.rm = TRUE)) {
    # If identical, remove one of the columns
    merged_ucdp <- merged_ucdp %>% 
      dplyr::select(-all_of(col_y))
    cat("Columns", col_x, "and", col_y, "are identical. Keeping", col_x, "and removing", col_y, "\n")
  } else {
    cat("Columns", col_x, "and", col_y, "are not identical. Keeping both.\n")
  }
}

#conflict_id is identical -> removing conflict_id.y 
#location is not identical -> Keeping both.
#side_a is identical -> removing side_a.y 
#side_a_id is identical -> removing side_a_id.y 
#side_b is not identical -> Keeping both.
#side_b_id is not identical -> Keeping both.
#gwno_a is not identical -> Keeping both.
#gwno_b is not identical -> Keeping both.

# Check the final dataset to confirm the changes
glimpse(merged_ucdp)

#Checking for missing values
sum(is.na(merged_ucdp)) #5321

#COMBINATION OF FORMS OF SUPPORT#
#Combining the separate forms of support variables into one
#categorical variable ranking different forms of support (from the lowest to the most extreme form of support/non-direct to direct support)
# Add a new column for the categorical variable
merged_ucdp <- merged_ucdp %>%
  mutate(
    ext_category = case_when(
      ext_sum == 0 ~ "no support",                                    # No support provided
      ext_sum > 1 ~ "several forms of support",                       # More than one type of support provided
      ext_u == 1 ~ "unknown support",                                 # Unknown support
      ext_o == 1 ~ "other support",                                   # Other support
      ext_l == 1 ~ "access to territory",                             # Access to territory
      ext_i == 1 ~ "intelligence",                                    # Intelligence
      ext_f == 1 ~ "funding",                                         # Funding
      ext_t == 1 ~ "training and expertise",                          # Training and expertise
      ext_m == 1 ~ "materiel and statistics",                         # Materiel and statistics
      ext_w == 1 ~ "weapons",                                         # Weapons
      ext_y == 1 ~ "access to infrastructure/joint operations",       # Access to infrastructure/joint operations
      ext_p == 1 ~ "foreign troop presence",                          # Foreign troop presence
      ext_x == 1 ~ "troop support",                                   # Troop support
      TRUE ~ "no support"                                             # Default to "no support"
    ),
    ext_category = factor(
      ext_category,
      levels = c(
        "no support",
        "unknown support",
        "other support",
        "access to territory",
        "intelligence",
        "funding",
        "training and expertise",
        "materiel and statistics",
        "weapons",
        "access to infrastructure/joint operations",
        "foreign troop presence",
        "troop support",
        "several forms of support"
      ),
    )
  )

#Creation of a variable to differentiate between indirect and direct support
merged_ucdp <- merged_ucdp %>%
  mutate(
    ext_type = case_when(
      ext_sum == 0 ~ "no support",                               # No support provided
      ext_sum > 1 & (ext_x == 1 | ext_p == 1) &                  # Mixed direct and indirect
        (ext_l == 1 | ext_i == 1 | ext_f == 1 | 
           ext_t == 1 | ext_m == 1 | ext_w == 1 | ext_y == 1) ~ "direct and indirect",
      ext_sum > 1 & (ext_x + ext_p == ext_sum) ~ "direct",       # All support is direct
      ext_sum > 1 & (ext_l + ext_i + ext_f + ext_t + 
                       ext_m + ext_w + ext_y == ext_sum) ~ "indirect", # All support is indirect
      ext_x == 1 | ext_p == 1 ~ "direct",                        # Single direct support
      ext_l == 1 | ext_i == 1 | ext_f == 1 | ext_t == 1 | 
        ext_m == 1 | ext_w == 1 | ext_y == 1 ~ "indirect",       # Single indirect support
      ext_u == 1 ~ "unknown"                                     # Unknown support
    ),
    ext_type = factor(
      ext_type,
      levels = c("no support", "indirect", "direct", "direct and indirect", "unknown") # Set the order
    )
  )

#Creation of a duration variable#
# Ensure the dataset is sorted by dyad_id and year
merged_ucdp <- merged_ucdp %>% arrange(dyad_id, year)

#Creation of a cumulative measure of duration
merged_ucdp <- merged_ucdp %>%
  group_by(dyad_id) %>%  # Group by dyad_id to calculate for each conflict-dyad
  mutate(
    start_year = min(year, na.rm = TRUE),  # Find the first year for each dyad_id
    cumulative_duration = year - start_year + 1  # Calculate duration as the difference from the start year
  ) %>%
  ungroup()  # Ungroup to return the dataset to its original structure

# Reorder columns to place the new ones after 'year'
merged_ucdp <- merged_ucdp %>%
  relocate(cumulative_duration, .after = year)

# Creation of a variable indicating whether the observation is before or after 9/11#
merged_ucdp <- merged_ucdp %>%
  mutate(
    nine_eleven = ifelse(year > 2001, "After 9/11", "Before 9/11")
  )

# Create a variable indicating whether the observation is during or after the Cold War#
merged_ucdp <- merged_ucdp %>%
  mutate(
    cold_war = case_when(
      year >= 1947 & year <= 1991 ~ "Cold War",
      year > 1991 ~ "Post-Cold War",
      TRUE ~ NA_character_ # Handles missing or out-of-range values
    )
  )

# Ensure the new variables are factors for better usability
merged_ucdp <- merged_ucdp %>%
  mutate(
    nine_eleven = factor(nine_eleven, levels = c("Before 9/11", "After 9/11")),
    cold_war = factor(cold_war, levels = c("Cold War", "Post-Cold War"))
  )

#Creation of a binary variable based on 'incompatibility' that indicates whether a conflict is about territory (1) or not (0)
merged_ucdp <- merged_ucdp %>%
  mutate(territorial = ifelse(incompatibility == 1 | incompatibility == 3, 1, 0))

#Factorising variables
#intensity
merged_ucdp$intensity <- factor(merged_ucdp$intensity,
                                levels = c(1, 2),
                                labels = c("Minor armed conflict", "War"))

#ext_coalition
merged_ucdp$ext_coalition <- factor(merged_ucdp$ext_coalition,
                                    levels = c(0, 1),
                                    labels = c("Bilateral Support", "Coalition Support"),
                                    exclude = NULL)

#incompatibility
merged_ucdp$incompatibility <- factor(merged_ucdp$incompatibility,
                                      levels = c(1, 2, 3),
                                      labels = c("territory", "government", "territory and government"))

#type of conflict
merged_ucdp$type <- factor(merged_ucdp$type,
                           levels = c(1, 2, 3, 4),
                           labels = c("extrasystemic", "interstate", "intrastate", "internationalised intrastate"))

#Relevel type so that intrastate becomes the reference group in the regression analysis
merged_ucdp$type <- factor(merged_ucdp$type,
                           levels = c("intrastate", "extrasystemic", "interstate", "internationalised intrastate"))

#ext_nonstate
merged_ucdp$ext_nonstate <- factor(merged_ucdp$ext_nonstate,
                                   levels = c(0, 1),
                                   labels = c("state supporter", "non-state supporter"),
                                   exclude = NULL)

#region
merged_ucdp$region <- factor(merged_ucdp$region,
                             levels = c(1, 2, 3, 4, 5),
                             labels = c("Europe", "Middle East", "Asia", "Africa", "Americas"))

#Remove unnecessary objects
#Remove datasets that are no longer needed
rm(ucdp_dyadic, ucdp_esd)
#Remove objects that are no longer needed
rm(common_columns, col_name, col, col_x, col_y)

# Within-between models #
# Turn dataset into panel data
ucdp_panel <- merged_ucdp %>%
  mutate(wave = year - 1975) %>% # Create a wave variable starting from 1975
  panel_data(id = dyad_id,
             wave = wave) 

# Fit the within-between models
#0. Overall provision of external support
wbm_0 <- wbm(ext_sup ~ intensity + cumulative_duration + nine_eleven + cold_war + ext_coalition # all variables that change within a conflict (which variables were included in FE)
             | type + territorial # all variables that change between conflicts
             | (1 | dyad_id), 
             data = ucdp_panel, 
             family = "poisson", #use poisson as model didn't converge with binomial
             control = glmerControl(optimizer = "bobyqa", # Use a more robust optimizer
                                    optCtrl = list(maxfun = 200000)  # Increase from the default 10000
             )
)
summary(wbm_0)

#1. Troop support
wbm_x <- wbm(ext_x ~ intensity + cumulative_duration + nine_eleven + cold_war + ext_coalition # all variables that change within a conflict (which variables were included in FE)
    | type + territorial # all variables that change between conflicts
    | (1 | dyad_id), 
    data = ucdp_panel, 
    family = "poisson", # Use poisson as model didn't converge with binomial
    control = glmerControl(optimizer = "bobyqa", # Use a more robust optimizer
                           optCtrl = list(maxfun = 200000)  # Increase from the default 10000
    )
)
summary(wbm_x)

#2. Foreign troop presence
wbm_p <- wbm(ext_p ~ intensity + cumulative_duration + nine_eleven + cold_war + ext_coalition # all variables that change within a conflict (which variables were included in FE)
    | type + territorial # all variables that change between conflicts
    | (1 | dyad_id), 
    data = ucdp_panel, 
    family = "poisson", # Use poisson as model didn't converge with binomial
    control = glmerControl(optimizer = "bobyqa", # Use a more robust optimizer
                           optCtrl = list(maxfun = 200000)  # Increase from the default 10000
    )
)
summary(wbm_p)

#3. Access to infrastructure/joint operations
wbm_y <- wbm(ext_y ~ intensity + cumulative_duration + nine_eleven + cold_war + ext_coalition # all variables that change within a conflict (which variables were included in FE)
    | type + territorial # all variables that change between conflicts
    | (1 | dyad_id), 
    data = ucdp_panel, 
    family = "binomial",
    control = glmerControl(optimizer = "bobyqa", # Use a more robust optimizer
                           optCtrl = list(maxfun = 200000)  # Increase from the default 10000
    )
)
summary(wbm_y)

#4. Weapons
wbm_w <- wbm(ext_w ~ intensity + cumulative_duration + nine_eleven + cold_war + ext_coalition # all variables that change within a conflict (which variables were included in FE)
    | type + territorial # all variables that change between conflicts
    | (1 | dyad_id), 
    data = ucdp_panel, 
    family = "binomial",
    control = glmerControl(optimizer = "bobyqa", # Use a more robust optimizer
                           optCtrl = list(maxfun = 200000)  # Increase from the default 10000
    )
)
summary(wbm_w)

#5. Materiel and statistics
wbm_m <- wbm(ext_m ~ intensity + cumulative_duration + nine_eleven + cold_war + ext_coalition # all variables that change within a conflict (which variables were included in FE)
    | type + territorial # all variables that change between conflicts
    | (1 | dyad_id), 
    data = ucdp_panel, 
    family = "binomial",
    control = glmerControl(optimizer = "bobyqa", # Use a more robust optimizer
                           optCtrl = list(maxfun = 200000)  # Increase from the default 10000
    )
)
summary(wbm_m)

#6. Training and expertise
wbm_t <- wbm(ext_t ~ intensity + cumulative_duration + nine_eleven + cold_war + ext_coalition # all variables that change within a conflict (which variables were included in FE)
    | type + territorial # all variables that change between conflicts
    | (1 | dyad_id), 
    data = ucdp_panel, 
    family = "poisson",
    control = glmerControl(optimizer = "bobyqa", # Use a more robust optimizer
                           optCtrl = list(maxfun = 200000)  # Increase from the default 10000
    )
)
summary(wbm_t)

#7. Funding
wbm_f <- wbm(ext_f ~ intensity + cumulative_duration + nine_eleven + cold_war + ext_coalition # all variables that change within a conflict (which variables were included in FE)
    | type + territorial # all variables that change between conflicts
    | (1 | dyad_id), 
    data = ucdp_panel, 
    family = "binomial",
    control = glmerControl(optimizer = "bobyqa", # Use a more robust optimizer
                           optCtrl = list(maxfun = 200000)  # Increase from the default 10000
    )
)
summary(wbm_f)

#8. Intelligence
wbm_i <- wbm(ext_i ~ intensity + cumulative_duration + nine_eleven + cold_war + ext_coalition # all variables that change within a conflict (which variables were included in FE)
    | type + territorial # all variables that change between conflicts
    | (1 | dyad_id), 
    data = ucdp_panel, 
    family = "binomial",
    control = glmerControl(optimizer = "bobyqa", # Use a more robust optimizer
                           optCtrl = list(maxfun = 200000)  # Increase from the default 10000
    )
)
summary(wbm_i)

#9. Access to territory
wbm_l <- wbm(ext_l ~ intensity + cumulative_duration + nine_eleven + cold_war + ext_coalition # all variables that change within a conflict (which variables were included in FE)
    | type + territorial # all variables that change between conflicts
    | (1 | dyad_id), 
    data = ucdp_panel, 
    family = "binomial",
    control = glmerControl(optimizer = "bobyqa", # Use a more robust optimizer
                           optCtrl = list(maxfun = 200000)  # Increase from the default 10000
    )
)
summary(wbm_l)

table(ucdp_panel$dyad_id, ucdp_panel$ext_l) %>% rowSums() %>% summary()

# Potential fix if not converging as binomial
# Scaling cumulative_duration
ucdp_panel$cumulative_duration_z <- scale(ucdp_panel$cumulative_duration)
# Then rerun the model
wbm_yi <- wbm(
  ext_y ~ intensity + cumulative_duration_z + nine_eleven + cold_war + ext_coalition
  | type + territorial
  | (1 | dyad_id),
  data = ucdp_panel,
  family = "binomial",
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 200000))
)
# no warning messages now

