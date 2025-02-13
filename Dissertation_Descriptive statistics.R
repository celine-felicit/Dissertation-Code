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
library(dagitty) # For DAGs
library(ggdag) # For DAG plots

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
sum(is.na(merged_ucdp)) #5325

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
# Identify the support variables
support_vars <- c("ext_l", "ext_i", "ext_f", "ext_t", "ext_m", "ext_w", "ext_y", "ext_p", "ext_x")

# Define readable labels for support types
support_labels <- c(
  "ext_l" = "access to territory",
  "ext_i" = "intelligence",
  "ext_f" = "funding",
  "ext_t" = "training",
  "ext_m" = "materiel",
  "ext_w" = "weapons",
  "ext_y" = "infrastructure",
  "ext_p" = "foreign troop presence",
  "ext_x" = "troop support"
)

# Create the ext_combination column
merged_ucdp <- merged_ucdp %>%
  mutate(
    ext_combination = apply(select(., all_of(support_vars)), 1, function(row) {
      active_support <- names(row)[row == 1]  # Extract names of active supports
      
      # Convert to readable labels using mapvalues
      active_support <- mapvalues(active_support, from = names(support_labels), to = support_labels, warn_missing = FALSE)
      
      # Assign categories based on count of support types
      if (length(active_support) == 0) {
        return("no support")
      } else if (length(active_support) == 1) {
        return(active_support)
      } else if (length(active_support) == 2) {
        return(paste(active_support, collapse = " and "))  # Two types
      } else if (length(active_support) == 3) {
        return(paste(active_support, collapse = ", "))  # Three types
      } else {
        return("several forms of support")  # More than 3
      }
    }),
    
    # Convert to factor with proper levels
    ext_combination = factor(
      ext_combination,
      levels = c(
        "no support", "unknown support", "other support",
        support_labels,  # Single support types
        paste(support_labels, collapse = " and "),  # Two support types
        paste(support_labels, collapse = ", "),  # Three support types
        "several forms of support"
      )
    )
  )

# Ensure that ext_combination is correctly set as a factor
class(merged_ucdp$ext_combination) #factor

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

# Calculate the new variables
merged_ucdp <- merged_ucdp %>%
  group_by(dyad_id) %>%
  mutate(
    # Calculate the first and last observed years for the dyad
    start_year = min(year, na.rm = TRUE),  # First year in the dataset for the dyad
    end_year = max(year, na.rm = TRUE),    # Last year in the dataset for the dyad
    
    # Calculate the duration including missing years
    duration_cease = end_year - start_year + 1,
    
    # Calculate the duration excluding missing years
    duration_peace = n()  # Count the actual number of rows (years) for the dyad
  ) %>%
  ungroup()

# Reorder columns to place the new ones after 'year'
merged_ucdp <- merged_ucdp %>%
  relocate(start_year, end_year, duration_cease, duration_peace, .after = year)

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

#Factorising variables
#intensity
merged_ucdp$intensity <- factor(merged_ucdp$intensity,
                                levels = c(1, 2),
                                labels = c("Minor armed conflict", "War"))
#ext_coalition
merged_ucdp$ext_coalition <- factor(merged_ucdp$ext_coalition,
                                    levels = c(0, 1),
                                    labels = c("Bilateral Support", "Coalition Support"))
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
                                   labels = c("state supporter", "non-state supporter"))

#region
merged_ucdp$region <- factor(merged_ucdp$region,
                             levels = c(1, 2, 3, 4, 5),
                             labels = c("Europe", "Middle East", "Asia", "Africa", "Americas"))

#DESCRIPTIVE ANALYSIS#
# Summarize total conflict counts per year for visualisations
total_summary <- merged_ucdp %>%
  group_by(year) %>%
  summarise(conflict_count = n(), ext_type = "Total", .groups = "drop")  # Add "Total" as a type

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
  geom_line(size = 1) +
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
    breaks = breaks_seq,  # Use manually checked sequence
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
ggplot(top_combination_summary, aes(x = year, y = conflict_count, linetype = ext_combination)) +
  geom_line(size = 1) +  # Line for each support type
  labs(
    title = "Evolution of the 10 Most Common External Support Combinations",
    x = "Year",
    y = "Number of Conflict-Dyads",
    linetype = "Support Combination"
  ) +
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
#9.1. Differences between duration_cease and duration_peace
merged_ucdp <- merged_ucdp %>%
  mutate(diff_duration = duration_cease - duration_peace)

# Reorder columns to place the new ones after 'year'
merged_ucdp <- merged_ucdp %>%
  relocate(diff_duration, .after = duration_peace)

# Compute the mean and median of the differences
mean_diff <- mean(merged_ucdp$diff_duration, na.rm = TRUE)
median_diff <- median(merged_ucdp$diff_duration, na.rm = TRUE)

# Output the results
mean_diff #2.846
median_diff #0

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
ggplot(duration_region, aes(x = region, y = mean_duration)) +
  geom_bar(stat = "identity", fill = "#FF6666") +  # Bar chart for mean duration
  geom_point(aes(y = median_duration), color = "blue", size = 3) +  # Add points for median duration
  labs(
    title = "Mean and Median Conflict Duration by Region",
    x = "Region",
    y = "Duration",
    color = "Statistic"
  ) +
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

#DIRECTED ACYCLIC GRAPH#
# Define the DAG structure (Variable names modified to avoid spaces)
dag <- dagitty("
  dag {
    incompatibility -> type_of_conflict
    incompatibility -> intensity
    incompatibility -> cumulative_duration
    type_of_conflict -> intensity 
    type_of_conflict -> cumulative_duration
    type_of_conflict -> provision_of_external_support
    intensity -> supporters_GDP
    intensity -> supporters_total_defence_spending
    intensity <-> cumulative_duration
    intensity <-> provision_of_external_support
    intensity <-> type_of_support
    region -> geographic_proximity
    region -> incompatibility
    cumulative_duration <-> provision_of_external_support
    
    nine_eleven -> shared_cultural_or_ideological_ties
    Cold_war -> shared_cultural_or_ideological_ties
    identity_of_recipient -> shared_cultural_or_ideological_ties
    
    colonial_ties -> shared_cultural_or_ideological_ties
    democracy -> shared_cultural_or_ideological_ties
    common_enemy <-> shared_cultural_or_ideological_ties
    common_enemy -> provision_of_external_support
    shared_cultural_or_ideological_ties -> provision_of_external_support
    shared_cultural_or_ideological_ties -> type_of_support
    
    geographic_proximity -> trade_connection
    geographic_proximity -> provision_of_external_support
    geographic_proximity -> type_of_support
    chance_of_spillover -> provision_of_external_support
    chance_of_spillover -> type_of_support
    multi_actor_dimension -> provision_of_external_support
    multi_actor_dimension -> intensity
    multi_actor_dimension -> type_of_conflict
    multi_actor_dimension -> cumulative_duration
    trade_connection -> provision_of_external_support
    supporters_GDP -> supporters_total_defence_spending
    supporters_total_defence_spending -> provision_of_external_support
    
    provision_of_external_support -> coalition_support
    provision_of_external_support -> type_of_support
    coalition_support -> type_of_support
    type_of_support -> indirect_support
    type_of_support -> direct_support
    direct_support -> troop_presence
    indirect_support -> access_to_infrastructure
    indirect_support -> weapons
    indirect_support -> materiel_and_logistics
    indirect_support -> training_and_expertise
    indirect_support -> intelligence
    indirect_support -> access_to_territory
  }
")

# Convert DAG to a tidy format
tidy_dag <- tidy_dagitty(dag)

# Extract unique node names
node_names <- unique(tidy_dag$data$name)

# Define x and y positions for clear layout using mutate()
node_positions <- data.frame(
  name = node_names  
) %>%
  mutate(
    x = case_when(
      name %in% c("incompatibility", "type_of_conflict", "intensity", "cumulative_duration", 
                  "region", "Cold_war", "nine_eleven", "identity_of_recipient") ~ -6,
      name %in% c("provision_of_external_support", "type_of_support", "direct_support", 
                  "indirect_support", "troop_presence", "access_to_infrastructure", "weapons", 
                  "materiel_and_logistics", "training_and_expertise", "intelligence", "access_to_territory") ~ 6,
      TRUE ~ 0
    ),
    y = seq(from = 10, to = -10, length.out = length(node_names)) # Spread out nodes vertically
  )

# Check the structure of node_positions
print(head(node_positions))

# Merge positions into DAG data (ensure tidy_dag$data has 'name' column)
tidy_dag$data <- tidy_dag$data %>%
  left_join(node_positions, by = "name")

# Rename the x.x, y.x columns to x and y
tidy_dag$data <- tidy_dag$data %>%
  rename(x = x.y, y = y.y)

# Check the structure of the resulting merged data
print(head(tidy_dag$data))

# Ensure x and y columns are now present
if (!all(c("x", "y") %in% colnames(tidy_dag$data))) {
  stop("Error: x and y columns do not exist in tidy_dag$data after merging!")
}

# Extract edges and positions
edges <- as.data.frame(edges(dag)) %>%
  rename(from = v, to = w) %>%
  left_join(tidy_dag$data %>% select(name, x, y), by = c("from" = "name"), suffix = c("_from", "")) %>%
  left_join(tidy_dag$data %>% select(name, x, y), by = c("to" = "name"), suffix = c("", "_end")) %>%
  rename(xend = x_end, yend = y_end)

# Ensure edges have valid positions
if (any(is.na(edges$x) | is.na(edges$y) | is.na(edges$xend) | is.na(edges$yend))) {
  stop("Error: Some edges have missing x or y positions!")
}

# Plot the improved DAG
ggplot() +
  geom_segment(data = edges, aes(x = x, y = y, xend = xend, yend = yend), 
               arrow = arrow(length = unit(0.2, "cm")), color = "black") +
  geom_text(data = tidy_dag$data, aes(x = x, y = y, label = name), size = 4, hjust = 0.5, vjust = 0.5) +
  theme_minimal() +
  ggtitle("DAG on the Effect of Conflict Characteristics on External Support") +
  theme(
    plot.title = element_text(hjust = 0.5),  # Center the title
    axis.title = element_blank(),            # Remove axis labels
    axis.text = element_blank(),             # Remove axis text
    panel.grid = element_blank()             # Remove grid
  )
  ###completely unreadable...
  ## Any suggestions on how to fix this? I have attempted multiple times but just end in a circle of errors