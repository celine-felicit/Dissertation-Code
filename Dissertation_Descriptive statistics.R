##DISSERTATION##
#Descriptive statistics#

#PREPARATION#
#Loading relevant packages
library(dplyr)
library(readxl)
library(tidyr)
library(purrr)
library(ggplot2)
library(ggpattern)

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
#We now have 117 variables so seems to have worked
###add further tests to see whether it worked

###Think about how to operationalise this variable
#Which forms of support are you specifically interested in: funding, weapons, troop

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
##118 variables now so seems to have worked
###add further checks

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

#Factorise region variable
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
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) #Rotate x-axis labels for better readability

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

# Create the line chart
ggplot(coalition_summary, aes(x = year, y = conflict_count, color = ext_coalition, group = ext_coalition)) +
  geom_line(size = 1) +
  labs(
    title = "Evolution of support types over time",
    x = "Year",
    y = "Number of conflicts",
    color = "Support type"
  ) +
  scale_color_manual(
    values = c(
      "Bilateral Support" = "#8c510a",
      "Coalition Support" = "#01665e"
    )
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )

#OR

# Combine datasets for coalition and total conflicts
combined_coalition <- bind_rows(coalition_summary, total_summary)

# Create the grouped line chart
ggplot(combined_coalition, aes(x = year, y = conflict_count, linetype = ext_coalition)) +
  geom_line(size = 1) +  # Add lines for each support type
  geom_line(data = total_summary, aes(x = year, y = conflict_count, linetype = "Total"), color = "black", size = 1) +  # Add total line
  labs(
    title = "Coalition support, 1975–2017",
    x = "Year",
    y = "Number of conflict-dyads",
    linetype = "Support type"
  ) +
  scale_linetype_manual(
    values = c(
      "Bilateral Support" = "dotted",   
      "Coalition Support" = "dashed",
      "Total armed conflicts" = "solid"
    )
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
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
###Would like to add patterns but receive Error in seq.default(from, to, by) : invalid '(to - from)/by'

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
ggplot(support_summary, aes(x = year, y = conflict_count, color = ext_sup, group = ext_sup)) +
  geom_line(size = 1) +
  labs(
    title = "Evolution of external support over time",
    x = "Year",
    y = "Number of conflicts",
    color = "External support"
  ) +
  scale_color_manual(
    values = c(
      "No external support" = "#8c510a",
      "External support" = "#01665e"
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
###Would be good to seperate 'several forms of support' into the most prevalent combinations

#6.2. Evolution of forms of support
# Grouped summary data for trend analysis
category_summary <- merged_ucdp %>%
  filter(year >= 1975 & year <= 2017) %>%  # Filter data for 1975-2017
  group_by(year, ext_category) %>%
  summarise(conflict_count = n(), .groups = "drop")

# Line chart for evolution of support categories over time
ggplot(category_summary, aes(x = year, y = count, color = ext_category, group = ext_category)) +
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
ggplot(region_summary, aes(x = year, y = conflict_count, color = region)) +
  geom_line(size = 1) +  # Add lines for each region
  labs(
    title = "Evolution of conflicts by region over time",
    x = "Year",
    y = "Number of conflicts",
    color = "Region"
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
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    legend.position = "top"
  )

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

#9.1. Distribution conflict duration
#9.1.1. Distribution of duration_cease
#Visualization - Histogram overlaid with kernel density curve and median
ggplot(data = merged_ucdp, aes(x = duration_cease)) + 
  geom_histogram(aes(y=..density..), #Histogram with density instead of count on y-axis
                 binwidth= , colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666") + #Overlay with transparent density plot
  geom_vline(aes(xintercept=median(duration_cease, na.rm=T)), #Ignore NA values for median
             color="red", linetype="dashed", size=1) +
  labs(title = "Duration Distribution",
       x = "Duration",
       y = "Density of duration") +
  theme_minimal()

#9.1.2. Distribution of duration_peace
#Visualization - Histogram overlaid with kernel density curve and median
ggplot(data = merged_ucdp, aes(x = duration_peace)) + 
  geom_histogram(aes(y=..density..), #Histogram with density instead of count on y-axis
                 binwidth= , colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666") + #Overlay with transparent density plot
  geom_vline(aes(xintercept=median(duration_peace, na.rm=T)), #Ignore NA values for median
             color="red", linetype="dashed", size=1) +
  labs(title = "Duration Distribution",
       x = "Duration",
       y = "Density of duration") +
  theme_minimal()

#9.2. Trend analysis conflict duration (are conflicts getting shorter/longer)
#9.2.1. Duration_cease over time
# Prepare the data for visualization  
duration_summary <- merged_ucdp %>%
  filter(year >= 1975 & year <= 2017) %>%  # Filter data for 1975-2017
  group_by(year) %>%
  summarise(mean_duration = mean(duration_cease, na.rm = TRUE),  # Calculate the mean duration
            median_duration = median(duration_cease, na.rm = TRUE),  # Calculate the median duration
            .groups = "drop")

# Create the line chart
ggplot(duration_summary, aes(x = year)) +
  geom_line(aes(y = mean_duration), color = "blue", size = 1) +  # Add line for mean duration
  geom_line(aes(y = median_duration), color = "red", size = 1) +  # Add line for median duration
  labs(
    title = "Trend in conflict duration over time",
    x = "Year",
    y = "Duration",
    color = "Statistic"
  ) +
  scale_y_continuous(breaks = seq(0, 10, by = 1)) +  # Set breaks for y-axis
  scale_color_manual(values = c("blue", "red")) +  # Set colors for lines
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    legend.position = "top"
  )

#9.2.2. Duration_peace over time
# Prepare the data for visualization
duration_summary_peace <- merged_ucdp %>%
  filter(year >= 1975 & year <= 2017) %>%  # Filter data for 1975-2017
  group_by(year) %>%
  summarise(mean_duration = mean(duration_peace, na.rm = TRUE),  # Calculate the mean duration
            median_duration = median(duration_peace, na.rm = TRUE),  # Calculate the median duration
            .groups = "drop")

# Create the line chart
ggplot(duration_summary_peace, aes(x = year)) +
  geom_line(aes(y = mean_duration), color = "blue", size = 1) +  # Add line for mean duration
  geom_line(aes(y = median_duration), color = "red", size = 1) +  # Add line for median duration
  labs(
    title = "Trend in peace duration over time",
    x = "Year",
    y = "Duration",
    color = "Statistic"
  ) +
  scale_y_continuous(breaks = seq(0, 10, by = 1)) +  # Set breaks for y-axis
  scale_color_manual(values = c("blue", "red")) +  # Set colors for lines
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    legend.position = "top"
  )

#9.3. Conflict duration by region
#9.3.1. Duration_cease by region
# Prepare the data for visualization
duration_region <- merged_ucdp %>%
  filter(year >= 1975 & year <= 2017) %>%  # Filter data for 1975-2017
  filter(!is.na(duration_cease)) %>%  # Filter out NA values
  group_by(region) %>%
  summarise(mean_duration = mean(duration_cease, na.rm = TRUE),  # Calculate the mean duration
            median_duration = median(duration_cease, na.rm = TRUE),  # Calculate the median duration
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

###10. Conflict parties
#10.1.Distribution state vs. non-state
#10.2. side_a/b_2nd (troop support): Always certain countries that provide troop support?


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
