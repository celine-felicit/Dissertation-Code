##DISSERTATION##
#Descriptive statistics#

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

#Load datasets into R
#1. UCDP Dyadic Dataset
ucdp_dyadic <- read.csv("~/Studium/Dissertation/Datasets/ucdp-dyadic-181.csv")

#2. UCDP ESD Dataset
# Read the first sheet from the Excel file
ucdp_esd <- read_excel("~/Studium/Dissertation/Datasets/ucdp-esd-dy-181.xlsx")

#COMBINATION OF DATSETS#
#Step 1. Merge UCDP ESD and UCDP dyadic datasets based on conflict dyad ID
#Check whether rows are unique in ESD dataset
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

#Dependent variables - Forms of external support
sum(is.na(merged_ucdp$ext_sum)) #0
#Troops
sum(is.na(merged_ucdp$ext_x))
#Troop presence
sum(is.na(merged_ucdp$ext_p))
#Access to infrastructure
sum(is.na(merged_ucdp$ext_y))
#Weapons
sum(is.na(merged_ucdp$ext_w))
#Materiel and statistics
sum(is.na(merged_ucdp$ext_m))
#Training and expertise
sum(is.na(merged_ucdp$ext_t))
#Funding
sum(is.na(merged_ucdp$ext_f))
#Intelligence
sum(is.na(merged_ucdp$ext_i))
#Access to territory
sum(is.na(merged_ucdp$ext_l))
#Other support
sum(is.na(merged_ucdp$ext_o))
#Unknown support
sum(is.na(merged_ucdp$ext_u))
  ##all 0

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

#Identify most common combinations of support: Create a new column with the most common combinations of support
merged_ucdp <- merged_ucdp %>%
  mutate(
    ext_combination = case_when(
      ext_sum == 0 ~ "no support",                                    # No support provided
      ext_sum > 2 ~ "several forms of support",                       # More than two types of support provided
      
      # Explicitly include unknown and other support combinations
      ext_u == 1 & ext_o == 1 ~ "unknown and other support",
      ext_u == 1 & ext_l == 1 ~ "unknown support and access to territory",
      ext_u == 1 & ext_i == 1 ~ "unknown support and intelligence",
      ext_u == 1 & ext_f == 1 ~ "unknown support and funding",
      ext_u == 1 & ext_t == 1 ~ "unknown support and training",
      ext_u == 1 & ext_m == 1 ~ "unknown support and materiel",
      ext_u == 1 & ext_w == 1 ~ "unknown support and weapons",
      ext_u == 1 & ext_y == 1 ~ "unknown support and infrastructure",
      ext_u == 1 & ext_p == 1 ~ "unknown support and foreign troop presence",
      ext_u == 1 & ext_x == 1 ~ "unknown support and troop support",
      
      ext_o == 1 & ext_l == 1 ~ "other support and access to territory",
      ext_o == 1 & ext_i == 1 ~ "other support and intelligence",
      ext_o == 1 & ext_f == 1 ~ "other support and funding",
      ext_o == 1 & ext_t == 1 ~ "other support and training",
      ext_o == 1 & ext_m == 1 ~ "other support and materiel",
      ext_o == 1 & ext_w == 1 ~ "other support and weapons",
      ext_o == 1 & ext_y == 1 ~ "other support and infrastructure",
      ext_o == 1 & ext_p == 1 ~ "other support and foreign troop presence",
      ext_o == 1 & ext_x == 1 ~ "other support and troop support",
      
      # Two types of support - explicit labeling
      ext_l == 1 & ext_i == 1 ~ "access to territory and intelligence",
      ext_l == 1 & ext_f == 1 ~ "access to territory and funding",
      ext_l == 1 & ext_t == 1 ~ "access to territory and training",
      ext_l == 1 & ext_m == 1 ~ "access to territory and materiel",
      ext_l == 1 & ext_w == 1 ~ "access to territory and weapons",
      ext_l == 1 & ext_y == 1 ~ "access to territory and infrastructure",
      ext_l == 1 & ext_p == 1 ~ "access to territory and foreign troop presence",
      ext_l == 1 & ext_x == 1 ~ "access to territory and troop support",
      
      ext_i == 1 & ext_f == 1 ~ "intelligence and funding",
      ext_i == 1 & ext_t == 1 ~ "intelligence and training",
      ext_i == 1 & ext_m == 1 ~ "intelligence and materiel",
      ext_i == 1 & ext_w == 1 ~ "intelligence and weapons",
      ext_i == 1 & ext_y == 1 ~ "intelligence and infrastructure",
      ext_i == 1 & ext_p == 1 ~ "intelligence and foreign troop presence",
      ext_i == 1 & ext_x == 1 ~ "intelligence and troop support",
      
      ext_f == 1 & ext_t == 1 ~ "funding and training",
      ext_f == 1 & ext_m == 1 ~ "funding and materiel",
      ext_f == 1 & ext_w == 1 ~ "funding and weapons",
      ext_f == 1 & ext_y == 1 ~ "funding and infrastructure",
      ext_f == 1 & ext_p == 1 ~ "funding and foreign troop presence",
      ext_f == 1 & ext_x == 1 ~ "funding and troop support",
      
      ext_t == 1 & ext_m == 1 ~ "training and materiel",
      ext_t == 1 & ext_w == 1 ~ "training and weapons",
      ext_t == 1 & ext_y == 1 ~ "training and infrastructure",
      ext_t == 1 & ext_p == 1 ~ "training and foreign troop presence",
      ext_t == 1 & ext_x == 1 ~ "training and troop support",
      
      ext_m == 1 & ext_w == 1 ~ "materiel and weapons",
      ext_m == 1 & ext_y == 1 ~ "materiel and infrastructure",
      ext_m == 1 & ext_p == 1 ~ "materiel and foreign troop presence",
      ext_m == 1 & ext_x == 1 ~ "materiel and troop support",
      
      ext_w == 1 & ext_y == 1 ~ "weapons and infrastructure",
      ext_w == 1 & ext_p == 1 ~ "weapons and foreign troop presence",
      ext_w == 1 & ext_x == 1 ~ "weapons and troop support",
      
      ext_y == 1 & ext_p == 1 ~ "infrastructure and foreign troop presence",
      ext_y == 1 & ext_x == 1 ~ "infrastructure and troop support",
      
      ext_p == 1 & ext_x == 1 ~ "foreign troop presence and troop support",
      
      # Single forms of support
      ext_u == 1 ~ "unknown support",
      ext_o == 1 ~ "other support",
      ext_l == 1 ~ "access to territory",
      ext_i == 1 ~ "intelligence",
      ext_f == 1 ~ "funding",
      ext_t == 1 ~ "training",
      ext_m == 1 ~ "materiel",
      ext_w == 1 ~ "weapons",
      ext_y == 1 ~ "infrastructure",
      ext_p == 1 ~ "foreign troop presence",
      ext_x == 1 ~ "troop support",
      
      TRUE ~ "no support"  # Default case to ensure all cases are covered
    )
  ) %>%
  # Convert to factor with proper levels
  mutate(
    ext_combination = factor(
      ext_combination,
      levels = c(
        "no support",
        "unknown support",
        "other support",
        "access to territory", "intelligence", "funding", "training",
        "materiel", "weapons", "infrastructure", "foreign troop presence", "troop support",
        "unknown and other support",
        "unknown support and access to territory", "unknown support and intelligence", 
        "unknown support and funding", "unknown support and training", 
        "unknown support and materiel", "unknown support and weapons", 
        "unknown support and infrastructure", "unknown support and foreign troop presence", 
        "unknown support and troop support",
        "other support and access to territory", "other support and intelligence", 
        "other support and funding", "other support and training", 
        "other support and materiel", "other support and weapons", 
        "other support and infrastructure", "other support and foreign troop presence", 
        "other support and troop support",
        "access to territory and intelligence", "funding and weapons", 
        "intelligence and training", "materiel and weapons", 
        "training and infrastructure", "foreign troop presence and troop support",
        "several forms of support"
      )
    )
  )

# Ensure that ext_combination is correctly set as a factor
class(merged_ucdp$ext_combination) #factor

#check that there's no NAs
sum(is.na(merged_ucdp$ext_combination))

# Verify the output
table(merged_ucdp$ext_combination)

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

#Check NAs for independent variables - conflict characteristics
sum(is.na(merged_ucdp$intensity)) #0
sum(is.na(merged_ucdp$type)) #0
sum(is.na(merged_ucdp$incompatibility)) #0
sum(is.na(merged_ucdp$region)) #0

sum(is.na(merged_ucdp$ext_coalition)) #783
sum(is.na(merged_ucdp$ext_sup)) #0
sum(is.na(merged_ucdp$ext_nonstate)) #411

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
#ext_sup
merged_ucdp$ext_sup <- factor(merged_ucdp$ext_sup,
                              levels = c(0, 1),
                              labels = c("No external support", "External support"))

#ext_nonstate
merged_ucdp$ext_nonstate <- factor(merged_ucdp$ext_nonstate,
                                   levels = c(0, 1),
                                   labels = c("state supporter", "non-state supporter"),
                                   exclude = NULL)

#region
merged_ucdp$region <- factor(merged_ucdp$region,
                             levels = c(1, 2, 3, 4, 5),
                             labels = c("Europe", "Middle East", "Asia", "Africa", "Americas"))

#DESCRIPTIVE ANALYSIS#
# Summarize total conflict counts per year for visualisations
total_summary <- merged_ucdp %>%
  group_by(year) %>%
  summarise(conflict_count = n(), ext_type = "Total", .groups = "drop")  # Add "Total" as a type

#Number of NAs
sum(is.na(merged_ucdp)) #5813
#Number of dyads
nrow(merged_ucdp %>% distinct(dyad_id)) #472

#1. Conflict intensity
#1.1.Distribution of conflict intensity
#Bar chart
ggplot(merged_ucdp, aes(x=intensity)) +
  geom_bar(width = 0.2, fill = "#8c510a", color = "black")+
  labs(title = "Distribution of conflict intensity levels",
       x = "Intensity level",
       y = "Count of conflicts") +
  theme_minimal()

#1.2. Trend analysis conflict intensity
#Create a summary dataset by year and intensity_level
intensity_summary <- merged_ucdp %>%
  filter(!is.na(intensity) & year >= 1975 & year <= 2017) %>% # Filter the dataset to the desired year range
  group_by(year, intensity) %>%
  summarise(conflict_count = n(), .groups = "drop")

#Create a stacked bar plot
ggplot(intensity_summary, aes(x = year, y = conflict_count, fill = intensity)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "World armed conflicts by intensity level",
       x = "Year",
       y = "Number of conflicts",
       fill = "Conflict intensity") +
  scale_fill_manual(values = c("grey", "black")) +
  scale_x_continuous(breaks = seq(min(intensity_summary$year), max(intensity_summary$year), by = 5)) + #Label years every 5 years
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), #Rotate x-axis labels for better readability
        legend.position = "bottom") 

#2. Descriptive statistics for ext_coalition
ext_coalition_stats <- merged_ucdp |>
  group_by(ext_coalition) |>
  summarise(count = n()) |>
  mutate(percentage = (count / sum(count)) * 100)
  ##35.05% missing values

#2.1. Distribution of ext_coalition
#Create a bar plot
ggplot(merged_ucdp, aes(x = ext_coalition, fill = ext_coalition)) +
  geom_bar() +
  labs(title = "Distribution of external support: bilateral vs coalition",
       x = "Type of external support",
       y = "Count",
       fill = "Support type") +
  scale_fill_manual(
    values = c(
      "Bilateral Support" = "#8c510a",   
      "Coalition Support" = "#01665e"
    )
  ) +
  theme_minimal()

#2.2. Trend analysis coalition support
# Summarize the data by year and ext_coalition
coalition_summary <- merged_ucdp %>%
  group_by(year, ext_coalition) %>%
  summarise(conflict_count = n(), .groups = "drop")

#Create the line chart
ggplot(coalition_summary, aes(x = year, y = conflict_count, color = ext_coalition, linetype = ext_coalition)) +
  geom_line(linewidth = 1) +
  labs(
    title = "Evolution of external support: bilateral vs coalition",
    x = "Year",
    y = "Number of conflicts",
    color = "External support",
    linetype = "External support"
  ) +
  scale_color_manual(
    values = c(
      "Bilateral Support" = "#8c510a",   
      "Coalition Support" = "#01665e"
    )
  ) +
  scale_linetype_manual(
    values = c(
      "Bilateral Support" = "solid",
      "Coalition Support" = "dashed"
    )
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )
  ###Why is NA not being portrayed???

#3. Incompatibility
#3.1. Distribution of incompatibility
# Create a bar plot
ggplot(merged_ucdp, aes(x = incompatibility, fill = incompatibility)) +
  geom_bar() +
  labs(
    title = "Distribution of incompatibility",
    x = "Reason for conflict",
    y = "Count",
    fill = "Reason for conflict"
  ) +
  scale_fill_manual(
    values = c(
      "territory" = "#8c510a",   
      "government" = "#01665e",
      "territory and government" = "#80cdc1"
    )
  ) +
  theme_minimal()

#3.2. Trend analysis incompatibility
# Summarize the data by year and incompatibility
incompatibility_summary <- merged_ucdp %>%
  filter(year >= 1975 & year <= 2017) %>%  # Filter data for 1975-2017
  group_by(year, incompatibility) %>%
  summarise(conflict_count = n(), .groups = "drop")

# Create the line chart
ggplot(incompatibility_summary, aes(x = year, y = conflict_count, color = incompatibility, group = incompatibility)) +
  geom_line(size = 1) +
  labs(
    title = "Evolution of incompatibility over time",
    x = "Year",
    y = "Number of conflicts",
    color = "Incompatibility"
  ) +
  scale_color_manual(
    values = c(
      "territory" = "#8c510a",   
      "government" = "#01665e",
      "territory and government" = "#80cdc1"
    )
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )

#OR

# Create the stacked area chart
ggplot(incompatibility_summary, aes(x = year, y = conflict_count, fill = incompatibility)) +
  geom_area(alpha = 0.6, size = 1, color = "white") +  # Stacked area with transparent fill
  labs(
    title = "State-based conflicts by type of incompatibility (1975–2017)",
    x = "Year",
    y = "Number of conflicts",
    fill = "Incompatibility"
  ) +
  scale_fill_manual(
    values = c(
      "territory" = "#8c510a",   # Orange for territory
      "government" = "#01665e",   # Dark green for government
      "territory and government" = "#80cdc1"  # Light blue for territory and government
    )
  ) +
  scale_x_continuous(
    breaks = seq(1975, 2017, by = 5),  # Breaks for every 5 years
    limits = c(1975, 2017)             # Explicit limits for the x-axis
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    plot.caption = element_text(hjust = 0),
    plot.margin = margin(20, 20, 50, 20)
  ) +
  annotate("text", x = 2017,  # Set x to the last year
           y = -5,    # Push the label below the plot area
           label = "Based on UCDP 18.1 data", 
           size = 3, 
           hjust = 1,  # Right-align text
           vjust = 1,  # Align text at the bottom
           color = "black")  # Optional, adjust color if needed

#OR with patterns
# Ensure `year` is numeric
incompatibility_summary <- incompatibility_summary %>%
  mutate(year = as.numeric(year)) %>%
  arrange(year, incompatibility)

# Remove any missing values
incompatibility_summary <- na.omit(incompatibility_summary)
incompatibility_summary <- incompatibility_summary %>% filter(!is.na(year))

# Define colors and patterns
pattern_mapping <- c(
  "territory" = "stripe", 
  "government" = "crosshatch", 
  "territory and government" = "circle"
)

fill_colors <- c(
  "territory" = "#8c510a",   # Orange for territory
  "government" = "#01665e",   # Dark green for government
  "territory and government" = "#80cdc1"  # Light blue for both
)

# Check year range
year_range <- range(incompatibility_summary$year, na.rm = TRUE)

# Adjust x-axis limits dynamically
ggplot(incompatibility_summary, aes(x = year, y = conflict_count, group = incompatibility)) +
  
  # Use ribbon to mimic area plot with pattern
  geom_ribbon_pattern(
    aes(ymin = 0, ymax = conflict_count, fill = incompatibility, pattern = incompatibility),
    alpha = 0.6, 
    size = 0.8, 
    color = "white",
    pattern_density = 0.1,  
    pattern_spacing = 0.02,  
    pattern_fill = "gray50"
  ) +
  
  # Add outline using geom_line()
  geom_line(aes(color = incompatibility), size = 1.2) +
  
  # Customize labels
  labs(
    title = "State-based conflicts by type of incompatibility (1975–2017)",
    x = "Year",
    y = "Number of Conflicts",
    fill = "Incompatibility",
    pattern = "Pattern"
  ) +
  
  # Apply color and pattern scales
  scale_fill_manual(values = fill_colors) +
  scale_pattern_manual(values = pattern_mapping) +
  scale_color_manual(values = fill_colors) +  
  
  # Fix x-axis breaks and limits dynamically
  scale_x_continuous(
    breaks = 5,  # Use manually checked sequence
    limits = c(year_range[1], year_range[2])
  )
  
  # Adjust theme
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    plot.caption = element_text(hjust = 0),
    plot.margin = margin(20, 20, 50, 20)
  ) +
  
  # Add annotation for data source
  annotate("text", x = year_range[2],  
           y = -5,    
           label = "Based on UCDP 18.1 data", 
           size = 3, 
           hjust = 1,  
           vjust = 1,  
           color = "black")  
  ###This looks different to the ones without the patterns, why is that?
  
#4. Conflict type
#4.1. Distribution of conflict type
#Create a bar plot
ggplot(merged_ucdp, aes(x = type, fill = type)) +
  geom_bar() +
  labs(title = "Distribution of conflict types",
       x = "Type of conflict",
       y = "Count",
       fill = "Conflict type") +
  scale_fill_manual(
    values = c(
      "extrasystemic" = "#8c510a",
      "interstate" = "#01665e",
      "intrastate" = "#dfc27d",
      "internationalised intrastate" = "#80cdc1"
    )
  ) +
  theme_minimal()

#4.2. Evolution of conflict type
# Prepare the data for visualization
type_summary <- merged_ucdp %>%
  filter(year >= 1975 & year <= 2017) %>%  # Filter data for 1975-2017
  group_by(year, type) %>%
  summarise(conflict_count = n(), .groups = "drop")  # Count conflicts for each type and year

# Create the grouped line chart
ggplot(type_summary, aes(x = year, y = conflict_count, color = type)) +
  geom_line(size = 1) +  # Add lines for each conflict type
  labs(
    title = "Evolution of conflict types over time",
    x = "Year",
    y = "Number of conflicts",
    color = "Conflict type"
  ) +
  scale_color_manual(
    values = c(
      "extrasystemic" = "#8c510a",
      "interstate" = "#01665e",
      "intrastate" = "#dfc27d",
      "internationalised intrastate" = "#80cdc1"
    )
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    legend.position = "top"
  )

# Create the stacked area chart
ggplot(type_summary, aes(x = year, y = conflict_count, fill = type)) +
  geom_area(alpha = 0.6, size = 1, color = "white") +  # Stacked area with transparent fill
  labs(
    title = "State-based conflicts by type of conflict (1975–2017)",
    x = "Year",
    y = "Number of conflicts",
    fill = "Conflict type"
  ) +
  scale_fill_manual(
    values = c(
      "extrasystemic" = "#8c510a",
      "interstate" = "#01665e",
      "intrastate" = "#dfc27d",
      "internationalised intrastate" = "#80cdc1"
    )
  ) +
  scale_x_continuous(
    breaks = seq(1975, 2017, by = 5),  # Breaks for every 5 years
    limits = c(1975, 2017)             # Explicit limits for the x-axis
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    plot.caption = element_text(hjust = 0),
    plot.margin = margin(20, 20, 50, 20)
  ) +
  annotate("text", x = 2017,  # Set x to the last year
           y = -5,    # Push the label below the plot area
           label = "Based on UCDP 18.1 data", 
           size = 3, 
           hjust = 1,  # Right-align text
           vjust = 1,  # Align text at the bottom
           color = "black")  # Optional, adjust color if needed
  ###Would like to add patterns but receive Error in seq.default(from, to, by) : invalid '(to - from)/by'

#5. External support offered
#5.1. Distribution of external support offered
#Create a bar plot
ggplot(merged_ucdp, aes(x = ext_sup, fill = ext_sup)) +
  geom_bar() +
  labs(title = "Distribution of external support",
       x = "External support",
       y = "Count",
       fill = "Support offered") +
  scale_fill_manual(
    values = c(
      "No external support" = "#8c510a",
      "External support" = "#01665e"
    )
  ) +
  theme_minimal()

#5.2. Trend analysis external support
# Summarize the data by year and ext_sup
support_summary <- merged_ucdp %>%
  group_by(year, ext_sup) %>%
  summarise(conflict_count = n(), .groups = "drop")

# Create the line chart
ggplot(support_summary, aes(x = year, y = conflict_count, color = ext_sup, linetype = ext_sup)) +
  geom_line(size = 1) +
  labs(
    title = "Evolution of external support over time",
    x = "Year",
    y = "Number of conflicts",
    color = "External support",
    linetype = "External support"
  ) +
  scale_color_manual(
    values = c(
      "No external support" = "#8c510a",
      "External support" = "#01665e"
    )
  ) +
  scale_linetype_manual(
    values = c(
      "No external support" = "solid",
      "External support" = "dashed"
    )
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )

#6. Forms of support offered
summary(merged_ucdp$ext_sum)
#Median: 4; Mean: 3.289 -> usually between 3-4 different forms of support are provided
###Would be interesting to see whether there's any prevalent combinations of support

#6.1. Distribution of forms of support
# Bar plot for the distribution of support categories
ggplot(merged_ucdp, aes(x = ext_category, fill = ext_category)) +
  geom_bar() +
  labs(
    title = "Distribution of forms of external support",
    x = "Form of support",
    y = "Count",
    fill = "Form of support"
  ) +
  scale_fill_manual(values = c(
    "no support" = "#E69F00",
    "unknown support" = "#56B4E9",
    "other support" = "#009E73",
    "access to territory" = "#F0E442",
    "intelligence" = "#0072B2",
    "funding" = "#D55E00",
    "training and expertise" = "#CC79A7",
    "materiel and statistics" = "#999999",
    "weapons" = "#9E77A8",
    "access to infrastructure/joint operations" = "#FF9DA7",
    "foreign troop presence" = "#7C7C7C",
    "troop support" = "#F4A582",
    "several forms of support" = "#66C2A5"
  )) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rotate x-axis labels for better readability
      
#6.2. Evolution of forms of support
# Grouped summary data for trend analysis
category_summary <- merged_ucdp %>%
  filter(year >= 1975 & year <= 2017) %>%  # Filter data for 1975-2017
  group_by(year, ext_category) %>%
  summarise(conflict_count = n(), .groups = "drop")

# Line chart for evolution of support categories over time
ggplot(category_summary, aes(x = year, y = conflict_count, color = ext_category, group = ext_category)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(
    title = "Evolution of forms of external support over time",
    x = "Year",
    y = "Count",
    color = "Form of support"
  ) +
  scale_color_manual(values = c(
    "no support" = "#E69F00",
    "unknown support" = "#56B4E9",
    "other support" = "#009E73",
    "access to territory" = "#F0E442",
    "intelligence" = "#0072B2",
    "funding" = "#D55E00",
    "training and expertise" = "#CC79A7",
    "materiel and statistics" = "#999999",
    "weapons" = "#9E77A8",
    "access to infrastructure/joint operations" = "#FF9DA7",
    "foreign troop presence" = "#7C7C7C",
    "troop support" = "#F4A582",
    "several forms of support" = "#66C2A5"
  )) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rotate x-axis labels for better readability

#OR

# Create the stacked area chart
ggplot(category_summary, aes(x = year, y = conflict_count, fill = ext_category)) +
  geom_area(alpha = 0.6, size = 1, color = "white") +  # Stacked area with transparent fill
  labs(
    title = "State-based conflicts by forms of external support (1975–2017)",
    x = "Year",
    y = "Number of conflicts",
    fill = "Category of external support"
  ) +
  scale_fill_manual(
    values = c(
      "no support" = "#E69F00",
      "unknown support" = "#56B4E9",
      "other support" = "#009E73",
      "access to territory" = "#F0E442",
      "intelligence" = "#0072B2",
      "funding" = "#D55E00",
      "training and expertise" = "#CC79A7",
      "materiel and statistics" = "#999999",
      "weapons" = "#9E77A8",
      "access to infrastructure/joint operations" = "#FF9DA7",
      "foreign troop presence" = "#7C7C7C",
      "troop support" = "#F4A582",
      "several forms of support" = "#66C2A5"
    )
  ) +
  scale_x_continuous(
    breaks = seq(1975, 2017, by = 5),  # Breaks for every 5 years
    limits = c(1975, 2017)             # Explicit limits for the x-axis
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    plot.caption = element_text(hjust = 0),
    plot.margin = margin(20, 20, 50, 20)
  ) +
  annotate("text", x = 2017,  # Set x to the last year
           y = -5,    # Push the label below the plot area
           label = "Based on UCDP 18.1 data", 
           size = 3, 
           hjust = 1,  # Right-align text
           vjust = 1,  # Align text at the bottom
           color = "black")  # Optional, adjust color if needed
  ###Would like to add patterns but receive Error in seq.default(from, to, by) : invalid '(to - from)/by'

#6.3. Prevalent combinations of support
# Calculate the average number of forms of external support provided
mean(merged_ucdp$ext_sum, na.rm = TRUE) #3.289
median(merged_ucdp$ext_sum, na.rm = TRUE) #4

# Create a summary dataset for the most common combinations of support
support_combinations <- merged_ucdp %>%
  group_by(ext_combination) %>%
  summarise(conflict_count = n(), .groups = "drop_last") %>%  # Fix `.groups` placement
  arrange(desc(conflict_count))  # Sort by descending count

# Print top combinations
print(head(support_combinations, 10))  # Display the 10 most common combinations
  ##several forms of support (1126), no support (425), NA (185), training+materiel+weapons (82), training (48)

# Get proportion of each category
support_combinations <- support_combinations %>%
  mutate(percentage = (conflict_count / sum(conflict_count)) * 100)

#Bar chart - Plot the top 10 most frequent support combinations in a colour-blind friendly palette
ggplot(head(support_combinations, 10), aes(x = reorder(ext_combination, conflict_count), y = conflict_count, fill = ext_combination)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Top 10 most frequent combinations of external support",
    x = "Combination of support",
    y = "Count",
    fill = "Combination of support"
  ) +
  scale_fill_viridis_d(option = "magma") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rotate x-axis labels for better readability

#6.4. Evolution of prevalent combinations of support
# Prepare the data for visualization
combination_summary <- merged_ucdp %>%
  filter(year >= 1975 & year <= 2017) %>%  # Filter data for 1975-2017
  group_by(year, ext_combination) %>%
  summarise(conflict_count = n(), .groups = "drop")  # Count conflicts for each combination and year

# Create the grouped line chart for the 10 most frequent support combinations
# Identify the 10 most frequent support combinations
top_10_combinations <- combination_summary %>%
  group_by(ext_combination) %>%
  summarise(total_count = sum(conflict_count)) %>%
  arrange(desc(total_count)) %>%
  slice_head(n = 10) %>%
  pull(ext_combination)

# Filter dataset to include only the top 10 combinations
top_combination_summary <- combination_summary %>%
  filter(ext_combination %in% top_10_combinations)

# Plot grouped line chart
ggplot(top_combination_summary, aes(x = year, y = conflict_count, color = ext_combination, linetype = ext_combination)) +
  geom_line(size = 1) +  # Line for each support type
  labs(
    title = "Evolution of the 10 Most Common External Support Combinations",
    x = "Year",
    y = "Number of Conflict-Dyads",
    color = "Support Combination",
    linetype = "Support Combination"
  ) +
  scale_color_viridis_d(option = "magma") + # Colour-blind friendly palette
  scale_linetype_manual(values = c("solid", "dashed", "dotted", "dotdash", "twodash",
                                   "longdash", "solid", "dotted", "dashed", "twodash")) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    legend.position = "bottom"
  )


#7. Type of support
#7.1. Distribution of support type
#Create a bar plot
ggplot(merged_ucdp, aes(x = ext_type, fill = ext_type)) +
  geom_bar() +
  labs(title = "Distribution of support types",
       x = "Type of support",
       y = "Count",
       fill = "Type of support") +
  scale_fill_manual(
    values = c(
      "no support" = "#8c510a",
      "direct" = "#01665e",
      "indirect" = "#dfc27d",
      "direct and indirect" = "#80cdc1"
    )
  ) +
  theme_minimal()

#7.2. Evolution of support type
# Prepare the data for visualization
ext_type_summary <- merged_ucdp %>%
  group_by(year, ext_type) %>%  # Group by year and support type
  summarise(conflict_count = n(), .groups = "drop")  # Count conflicts for each type and year

# Create the grouped line chart
ggplot(ext_type_summary, aes(x = year, y = conflict_count, color = ext_type)) +
  geom_line(size = 1) +  # Add lines for each support type
  labs(
    title = "Evolution of support types over time",
    x = "Year",
    y = "Number of conflicts",
    color = "Support type"
  ) +
  scale_color_manual(
    values = c(
      "no support" = "#8c510a",
      "direct" = "#01665e",
      "indirect" = "#dfc27d",
      "direct and indirect" = "#80cdc1"
    )
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    legend.position = "top"
  )

#OR

# Combine datasets for ext_type and total conflicts
combined_type <- bind_rows(ext_type_summary, total_summary)

# Create the grouped line chart
ggplot(combined_type, aes(x = year, y = conflict_count, linetype = ext_type)) +
  geom_line(size = 1) +  # Add lines for each support type
  geom_line(data = total_summary, aes(x = year, y = conflict_count, linetype = "Total"), color = "black", size = 1) +  # Add total line
  labs(
    title = "Direct and Indirect Support, 1975–2017",
    x = "Year",
    y = "Number of conflict-dyads",
    linetype = "Support type"
  ) +
  scale_linetype_manual(
    values = c(
      "direct" = "dotted",               # Direct support: dotted line
      "indirect" = "dashed",             # Indirect support: dashed line
      "direct and indirect" = "dotdash", # Direct and indirect: dot-dash line
      "Total" = "solid"                  # Total: solid line
    )
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    legend.position = "bottom"
  )
  ###NA is not being portrayed, so how do I get rid of it in the legend?

#8. Region
#8.1. Distribution of conflicts by region
#Create a bar plot
ggplot(merged_ucdp, aes(x = region, fill = region)) +
  geom_bar() +
  labs(title = "Distribution of conflicts by region",
       x = "Region",
       y = "Count",
       fill = "Region") +
  scale_fill_manual(
    values = c(
      "Europe" = "#8c510a",
      "Middle East" = "#01665e",
      "Asia" = "#dfc27d",
      "Africa" = "#80cdc1",
      "Americas" = "#a6d854"
    )
  ) +
  theme_minimal()

#8.2. Trend analysis of conflicts by region
# Prepare the data for visualization
region_summary <- merged_ucdp %>%
  filter(year >= 1975 & year <= 2017) %>%  # Filter data for 1975-2017
  group_by(year, region) %>%
  summarise(conflict_count = n(), .groups = "drop")  # Count conflicts for each region and year

# Create the grouped line chart
ggplot(region_summary, aes(x = year, y = conflict_count, color = region, linetype = region)) +
  geom_line(size = 1) +  # Add lines for each region
  labs(
    title = "Evolution of conflicts by region over time",
    x = "Year",
    y = "Number of conflicts",
    color = "Region",
    linetype = "Region"
  ) +
  scale_color_manual(
    values = c(
      "Europe" = "#8c510a",
      "Middle East" = "#01665e",
      "Asia" = "#dfc27d",
      "Africa" = "#80cdc1",
      "Americas" = "#a6d854"
    )
  ) +
  scale_linetype_manual(
    values = c(
      "Europe" = "solid",
      "Middle East" = "dashed",
      "Asia" = "dotted",
      "Africa" = "dotdash",
      "Americas" = "longdash"
    )
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    legend.position = "bottom"
  )

#Stacked area chart
ggplot(region_summary, aes(x = year, y = conflict_count, fill = region)) +
  geom_area(alpha = 0.6, size = 1, color = "white") +  # Stacked area with transparent fill
  labs(
    title = "State-based conflicts by region (1975–2017)",
    x = "Year",
    y = "Number of conflicts",
    fill = "Region"
  ) +
  scale_fill_manual(
    values = c(
      "Europe" = "#8c510a",
      "Middle East" = "#01665e",
      "Asia" = "#dfc27d",
      "Africa" = "#80cdc1",
      "Americas" = "#a6d854"
    )
  ) +
  scale_x_continuous(
    breaks = seq(1975, 2017, by = 5),  # Breaks for every 5 years
    limits = c(1975, 2017)             # Explicit limits for the x-axis
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    plot.caption = element_text(hjust = 0),
    plot.margin = margin(20, 20, 50, 20)
  ) +
  annotate("text", x = 2017,  # Set x to the last year
           y = -5,    # Push the label below the plot area
           label = "Based on UCDP 18.1 data", 
           size = 3, 
           hjust = 1,  # Right-align text
           vjust = 1,  # Align text at the bottom
           color = "black")  # Optional, adjust color if needed
  ###Would like to add patterns

#9. Conflict duration
#9.1. Distribution of cumulative conflict duration
#Visualization - Histogram overlaid with kernel density curve and median
ggplot(data = merged_ucdp, aes(x = cumulative_duration)) + 
  geom_histogram(aes(y=..density..), #Histogram with density instead of count on y-axis
                 binwidth= , colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666") + #Overlay with transparent density plot
  geom_vline(aes(xintercept=median(cumulative_duration, na.rm=T)), #Ignore NA values for median
             color="red", linetype="dashed", size=1) +
  labs(title = "Duration Distribution",
       x = "Duration",
       y = "Density of duration") +
  theme_minimal()

#9.2. Trend analysis conflict duration (are conflicts getting shorter/longer)
# Prepare the data for visualization  
duration_summary <- merged_ucdp %>%
  filter(year >= 1975 & year <= 2017) %>%  # Filter data for 1975-2017
  group_by(year) %>%
  summarise(mean_duration = mean(cumulative_duration, na.rm = TRUE),  # Calculate the mean duration
            median_duration = median(cumulative_duration, na.rm = TRUE),  # Calculate the median duration
            .groups = "drop")

# Create the line chart
ggplot(duration_summary, aes(x = year, y = mean_duration, linetype = "Mean")) +
  geom_line(size = 1) +  # Add line for mean duration
  geom_line(aes(x = year, y = median_duration, linetype = "Median"), size = 1) +  # Add line for median duration
  labs(
    title = "Mean and Median Conflict Duration over Time",
    x = "Year",
    y = "Duration",
    linetype = "Statistic"
  ) +
  scale_linetype_manual(values = c("Mean" = "dashed", "Median" = "dotted")) +  # Set linetypes
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    legend.position = "bottom"
  )

#9.3. Conflict duration by region
# Prepare the data for visualization
duration_region <- merged_ucdp %>%
  filter(year >= 1975 & year <= 2017) %>%  # Filter data for 1975-2017
  filter(!is.na(cumulative_duration)) %>%  # Filter out NA values
  group_by(region) %>%
  summarise(mean_duration = mean(cumulative_duration, na.rm = TRUE),  # Calculate the mean duration
            median_duration = median(cumulative_duration, na.rm = TRUE),  # Calculate the median duration
            .groups = "drop")

# Create the bar chart
ggplot(duration_region, aes(x = region)) +
  geom_bar(aes(y = mean_duration, fill = "Mean Duration"), 
           stat = "identity", show.legend = TRUE) +  # Bar chart for mean duration
  geom_point(aes(y = median_duration, color = "Median Duration"), 
             size = 3, show.legend = TRUE) +  # Points for median duration
  labs(
    title = "Mean and Median Conflict Duration by Region",
    x = "Region",
    y = "Duration",
    fill = "Statistic",  # Legend title for bar (mean)
    color = "Statistic"  # Legend title for points (median)
  ) +
  scale_fill_manual(values = c("Mean Duration" = "#FF6666")) +  # Custom color for bars
  scale_color_manual(values = c("Median Duration" = "blue")) +  # Custom color for points
  scale_y_continuous(breaks = seq(0, 10, by = 1)) +  # Set breaks for y-axis
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top"
  )

#10. Identity of external supporters
#10.1. Distribution of state and non-state external supporters
#Create a bar plot
ggplot(merged_ucdp, aes(x = ext_nonstate, fill = ext_nonstate)) +
  geom_bar() +
  labs(title = "Distribution of state and non-state supporters",
       x = "Identity of external supporter",
       y = "Count",
       fill = "identity of external supporter") +
  scale_fill_manual(
    values = c(
      "state supporter" = "#8c510a",
      "non-state supporter" = "#01665e"
    )
  ) +
  theme_minimal()

#10.2. Trend analysis of state and non-state external supporters
# Prepare the data for visualization
supporter_summary <- merged_ucdp %>%
  filter(year >= 1975 & year <= 2017) %>%  # Filter data for 1975-2017
  group_by(year, ext_nonstate) %>%
  summarise(conflict_count = n(), .groups = "drop")  # Count conflicts for each type and year

# Create the line chart
ggplot(supporter_summary, aes(x = year, y = conflict_count, color = ext_nonstate, linetype = ext_nonstate)) +
  geom_line(size = 1) +  # Add lines for each support type
  labs(
    title = "Evolution of state and non-state external supporters over time",
    x = "Year",
    y = "Number of conflicts",
    color = "Identity of external supporter",
    linetype = "Identity of external supporter"
  ) +
  scale_color_manual(
    values = c(
      "state supporter" = "#8c510a",
      "non-state supporter" = "#01665e"
    )
  ) +
  scale_linetype_manual(
    values = c(
      "state supporter" = "solid",
      "non-state supporter" = "dashed"
    )
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    legend.position = "bottom"
  )
  ###Why is NA not being portrayed?

#BIVARIATE ANALYSIS#
#1. ext_coalition and ext_category 
#Contingency table
contingency_1 <- table(merged_ucdp$ext_coalition, merged_ucdp$ext_category)
print(contingency_1)

#Chi-square
chi_1 <- chisq.test(contingency_1)
print(chi_1)
#The chi-square test checks if there is a significant association between the variables.
#If p < 0.05, there is evidence of an association.
#If p >= 0.05, no significant association is found.
###p-value = NA -> the chi-square test cannot be computed because the contingency table contains cells with expected frequencies less than 5.

#If the contingency table contains cells with low expected frequencies (e.g., less than 5), a chi-square test might not be appropriate. 
#In such cases, you may use Fisher’s Exact Test:
fisher_1 <- fisher.test(contingency_1, simulate.p.value=TRUE)
print(fisher_1)

#Visualisation
#Stacked bar chart
ggplot(merged_ucdp, aes(x = ext_coalition, fill = ext_category)) +
  geom_bar(position = "stack") +  # Stack bars
  labs(
    title = "Forms of Support by coalition status",
    x = "Coalition Support",
    y = "Count",
    fill = "Form of Support"
  ) +
  scale_fill_manual(values = c(
    "no support" = "#999999",
    "unknown support" = "#E69F00",
    "other support" = "#56B4E9",
    "access to territory" = "#009E73",
    "intelligence" = "#F0E442",
    "funding" = "#0072B2",
    "training and expertise" = "#D55E00",
    "materiel and statistics" = "#CC79A7",
    "weapons" = "#E5D8BD",
    "access to infrastructure/joint operations" = "#F4A582",
    "foreign troop presence" = "#92C5DE",
    "troop support" = "#B2182B",
    "several forms of support" = "#2166AC"
  )) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for better readability
    legend.position = "right"  # Adjust legend position
  )

#1.1. ext_coalition and ext_type 
# Create a contingency table
contingency_1.1 <- table(merged_ucdp$ext_coalition, merged_ucdp$ext_type)
print(contingency_1.1)

#Chi-square
chi_1.1 <- chisq.test(contingency_1.1)
print(chi_1.1)

#Fisher test
fisher_1.1 <- fisher.test(contingency_1.1, simulate.p.value=TRUE)
print(fisher_1.1)

#Visualisation: Stacked bar chart
ggplot(merged_ucdp, aes(x = ext_coalition, fill = ext_type)) +
  geom_bar(position = "stack") +  # Stack bars
  labs(
    title = "Type of support by coalition support",
    x = "Coalition support",
    y = "Count",
    fill = "Type of support"
  ) +
  scale_fill_manual(values = c(
    "no support" = "#8c510a",
    "direct" = "#01665e",
    "indirect" = "#dfc27d",
    "direct and indirect" = "#80cdc1"
  )) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for better readability
    legend.position = "right"  # Adjust legend position
  )

#3. type and ext_category 
contingency_3 <- table(merged_ucdp$type, merged_ucdp$ext_category)
print(contingency_3)

#Chi-square
chi_3 <- chisq.test(contingency_3)
print(chi_3)
#p-value = NA

#Fisher test
fisher_3 <- fisher.test(contingency_3, simulate.p.value=TRUE)
print(fisher_3)

#Visualisation: Stacked bar chart
ggplot(merged_ucdp, aes(x = type, fill = ext_category)) +
  geom_bar(position = "stack") +  # Stack bars
  labs(
    title = "Forms of support by conflict type",
    x = "Type of conflict",
    y = "Count",
    fill = "Form of support"
  ) +
  scale_fill_manual(values = c(
    "no support" = "#999999",
    "unknown support" = "#E69F00",
    "other support" = "#56B4E9",
    "access to territory" = "#009E73",
    "intelligence" = "#F0E442",
    "funding" = "#0072B2",
    "training and expertise" = "#D55E00",
    "materiel and statistics" = "#CC79A7",
    "weapons" = "#E5D8BD",
    "access to infrastructure/joint operations" = "#F4A582",
    "foreign troop presence" = "#92C5DE",
    "troop support" = "#B2182B",
    "several forms of support" = "#2166AC"
  )) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for better readability
    legend.position = "right"  # Adjust legend position
  )

#3.1. type and ext_type
#Contingency table
contingency_3.1 <- table(merged_ucdp$type, merged_ucdp$ext_type)
print(contingency_3.1)

#Chi-square
chi_3.1 <- chisq.test(contingency_3.1)
print(chi_3.1)
#p-value = NA

#Fisher test
fisher_3.1 <- fisher.test(contingency_3.1, simulate.p.value=TRUE)
print(fisher_3.1)

#Visualisation: Stacked bar chart
ggplot(merged_ucdp, aes(x = type, fill = ext_type)) +
  geom_bar(position = "stack") +  # Stack bars
  labs(
    title = "Type of support by type of conflict",
    x = "Conflict type",
    y = "Count",
    fill = "Type of support"
  ) +
  scale_fill_manual(values = c(
    "no support" = "#8c510a",
    "direct" = "#01665e",
    "indirect" = "#dfc27d",
    "direct and indirect" = "#80cdc1"
  )) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for better readability
    legend.position = "right"  # Adjust legend position
  )

#4.intensity and ext_category 
contingency_4 <- table(merged_ucdp$intensity, merged_ucdp$ext_category)
print(contingency_4)

#Chi-square
chi_4 <- chisq.test(contingency_4)
print(chi_4)
#p-value = 2.745e-10

#Visualisation: Stacked bar chart
ggplot(merged_ucdp, aes(x = intensity, fill = ext_category)) +
  geom_bar(position = "stack") +  # Stack bars
  labs(
    title = "Forms of support by conflict intensity",
    x = "Conflict intensity",
    y = "Count",
    fill = "Form of support"
  ) +
  scale_fill_manual(values = c(
    "no support" = "#999999",
    "unknown support" = "#E69F00",
    "other support" = "#56B4E9",
    "access to territory" = "#009E73",
    "intelligence" = "#F0E442",
    "funding" = "#0072B2",
    "training and expertise" = "#D55E00",
    "materiel and statistics" = "#CC79A7",
    "weapons" = "#E5D8BD",
    "access to infrastructure/joint operations" = "#F4A582",
    "foreign troop presence" = "#92C5DE",
    "troop support" = "#B2182B",
    "several forms of support" = "#2166AC"
  )) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for better readability
    legend.position = "right"  # Adjust legend position
  )

#4.1. intensity and ext_type
#Contingency table
contingency_4.1 <- table(merged_ucdp$intensity, merged_ucdp$ext_type)
print(contingency_4.1)

#Chi-square
chi_4.1 <- chisq.test(contingency_4.1)
print(chi_4.1)
#p-value = < 2.2e-16

#Visualisation: Stacked bar chart
ggplot(merged_ucdp, aes(x = intensity, fill = ext_type)) +
  geom_bar(position = "stack") +  # Stack bars
  labs(
    title = "Type of support by conflict intensity",
    x = "Conflict intensity",
    y = "Count",
    fill = "Type of support"
  ) +
  scale_fill_manual(values = c(
    "no support" = "#8c510a",
    "direct" = "#01665e",
    "indirect" = "#dfc27d",
    "direct and indirect" = "#80cdc1"
  )) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for better readability
    legend.position = "right"  # Adjust legend position
  )

#5. incompatibility and ext_category 
contingency_5 <- table(merged_ucdp$incompatibility, merged_ucdp$ext_category)
print(contingency_5)

#Chi-square
chi_5 <- chisq.test(contingency_5)
print(chi_5)
#p-value = 0.001356

#Visualisation: Stacked bar chart
ggplot(merged_ucdp, aes(x = incompatibility, fill = ext_category)) +
  geom_bar(position = "stack") +  # Stack bars
  labs(
    title = "Forms of Support by incompatibility",
    x = "Incompatibility",
    y = "Count",
    fill = "Form of Support"
  ) +
  scale_fill_manual(values = c(
    "no support" = "#999999",
    "unknown support" = "#E69F00",
    "other support" = "#56B4E9",
    "access to territory" = "#009E73",
    "intelligence" = "#F0E442",
    "funding" = "#0072B2",
    "training and expertise" = "#D55E00",
    "materiel and statistics" = "#CC79A7",
    "weapons" = "#E5D8BD",
    "access to infrastructure/joint operations" = "#F4A582",
    "foreign troop presence" = "#92C5DE",
    "troop support" = "#B2182B",
    "several forms of support" = "#2166AC"
  )) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for better readability
    legend.position = "right"  # Adjust legend position
  )

#5.1. incompatibility and ext_type
#Contingency table
contingency_5.1 <- table(merged_ucdp$incompatibility, merged_ucdp$ext_type)
print(contingency_5.1)

#Visualisation: Stacked bar chart
ggplot(merged_ucdp, aes(x = incompatibility, fill = ext_type)) +
  geom_bar(position = "stack") +  # Stack bars
  labs(
    title = "Type of support by incompatibility",
    x = "Incompatibility",
    y = "Count",
    fill = "Type of support"
  ) +
  scale_fill_manual(values = c(
    "no support" = "#8c510a",
    "direct" = "#01665e",
    "indirect" = "#dfc27d",
    "direct and indirect" = "#80cdc1"
  )) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for better readability
    legend.position = "right"  # Adjust legend position
  )

#Chi-square
chi_5.1 <- chisq.test(contingency_5.1)
print(chi_5.1)
#p-value < 2.2e-16

###6. Conflict party and ext_category
###6.1. Conflict party and ext_type

#Variable table
# Create a dataframe with variable details
variable_table <- data.frame(
  Category = c(
    "Dependent Variables", rep("", 15),
    "Independent Variables", rep("", 5),
    "Control Variables", rep("", 3)
  ),
  Variable = c(
    "", "ext_sup", "ext_x", "ext_p", "ext_y", "ext_w", "ext_m", "ext_t", "ext_f", "ext_i", "ext_l", "ext_o", "ext_u",
    "ext_category", "ext_combination", "ext_type",
    "", "type", "intensity", "incompatibility", "cumulative_duration", "region",
    "", "cold_war", "nine_eleven", "ext_coalition"
  ),
  Description = c(
    "", 
    "External support provided (0 = No, 1 = Yes)",
    "Troop support (1 = Yes)", "Foreign troop presence (1 = Yes)", 
    "Access to infrastructure/joint operations (1 = Yes)", "Weapons support (1 = Yes)", 
    "Materiel and logistics support (1 = Yes)", "Training and expertise support (1 = Yes)", 
    "Funding support (1 = Yes)", "Intelligence support (1 = Yes)", 
    "Access to territory (1 = Yes)", "Other support (1 = Yes)", "Unknown support (1 = Yes)",
    "Categorized external support (e.g., no support, direct, indirect, troop support, etc.)",
    "Most common combinations of external support (up to three explicitly listed)",
    "Categorization of external support as direct, indirect, or mixed",
    
    "", 
    "Type of conflict (1 = Extrasystemic, 2 = Interstate, 3 = Intrastate, 4 = Internationalized Intrastate)",
    "Conflict intensity (1 = Minor conflict, 2 = War)", 
    "Conflict incompatibility (1 = Territory, 2 = Government, 3 = Both)",
    "Cumulative years of conflict (Years since first observed conflict year)",
    "Region of conflict (1 = Europe, 2 = Middle East, 3 = Asia, 4 = Africa, 5 = Americas)",
    
    "", 
    "Cold War status (0 = Cold War, 1 = Post-Cold War)",
    "Period relative to 9/11 (0 = Before 9/11, 1 = After 9/11)",
    "Coalition support (0 = Bilateral support, 1 = Coalition support)"
  ),
  Measurement_Scale = c(
    "", 
    rep("Nominal", 12), "Ordinal", "Nominal", "Nominal",
    "", 
    "Nominal", "Nominal", "Nominal", "Ratio", "Nominal",
    "", 
    "Nominal", "Nominal", "Nominal"
  ),
  stringsAsFactors = FALSE
)

# Create a formatted table using gt
variable_table %>%
  gt() %>%
  tab_header(title = "Variable Table: Conflict Characteristics and External Support") %>%
  cols_label(
    Category = "Category",
    Variable = "Variable Name",
    Description = "Description",
    Measurement_Scale = "Measurement Scale"
  ) %>%
  tab_options(table.font.size = "small") %>%
  tab_style(
    style = cell_text(weight = "bold", size = px(14)),
    locations = cells_column_labels()
  ) %>%
  tab_style(
    style = cell_fill(color = "lightgray"),
    locations = cells_body(rows = Variable == "")
  ) %>%
  tab_style(
    style = cell_text(weight = "bold", align = "left", size = px(13)),
    locations = cells_body(rows = Variable == "")
  )
