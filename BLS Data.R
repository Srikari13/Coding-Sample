library(tidyverse)
library(vroom)
library(tidyr)
library(rlang)

# Create variables to store the names of the files
months <- c("jan", "feb", "mar", "apr", "may", "jun",
            "jul", "aug", "sep", "oct", "nov", "dec")
filenames <- paste0(months, "20pub.csv")

# Create variables for the occupation codes and labels
occupation_codes <- c("300", "1106", "1240", "1460", "1650", 
                      "2100", "2205", "2825", "2865")

occupation_labels <- c("AEM", "CNA", "Stat", "MechEng", "MedSci", 
                       "Lawyer&Judge", "PostSec", "PubRel", "OtherMedia")

# Combines all data files into one single variable
all_data <- map_dfr(filenames, ~ vroom(.x, delim = ",", col_types = cols(.default = col_character())))

# Filter once outside the loop
all_data <- all_data %>%
  filter(pemlr %in% c("1", "2"), !is.na(pwcmpwgt))

all_data <- all_data %>%
  mutate(pwcmpwgt = as.numeric(pwcmpwgt))

summary_tables_list <- list()

# Loop through each occupation
for (i in seq_along(occupation_codes)) {
  code <- occupation_codes[i]
  label <- occupation_labels[i]
  
  # Sanitize label to use as variable name
  safe_label <- str_replace_all(label, "[^[:alnum:]_]", "_")
  
  # Define a processing function for this occupation
  process_file <- function(file) {
    df <- all_data %>%
      filter(pemlr %in% c("1", "2")) %>%
      filter(ptio1ocd == code)
    
    # Calculates total value for one occupation
    total <- (sum(df$pwcmpwgt) / 10000) / 12
    
    # Calculates number of white non-hispanics for one occupation
    white_nhp <- df %>%
      filter(ptdtrace == "1", pehspnon == "2") %>%
      summarise(white_total = (sum(pwcmpwgt) / 10000)/ 12) %>%
      pull(white_total)
    
    # Calculates number of black non-hispanics for one occupation
    black_nhp <- df %>%
      filter(ptdtrace == "2", pehspnon == "2") %>%
      summarise(black_total = (sum(pwcmpwgt) / 10000) / 12) %>%
      pull(black_total)
    
    # Calculates number of asian non-hispanics for one occupation
    asian_nhp <- df %>%
      filter(ptdtrace == "4", pehspnon == "2") %>%
      summarise(asian_total = (sum(pwcmpwgt) / 10000) / 12) %>%
      pull(asian_total)
    
    # Calculates number of other race hispanic for one occupation
    hispanic_other <- df %>%
      filter(pehspnon == "1", !ptdtrace %in% c("1", "2", "4")) %>%
      summarise(hispanic_total = (sum(pwcmpwgt) / 10000) / 12) %>%
      pull(hispanic_total)
    
    tibble(total = total,
           white_nhp = white_nhp,
           black_nhp = black_nhp,
           asian_nhp = asian_nhp, 
           hispanic_other = hispanic_other)
  }
  
  # Process all files
  results <- map_dfr(filenames, process_file)
  
  # Compute averages and percentages
  avg_total <- round(mean(results$total), digits = 0)
  avg_white_nhp <- round(mean(results$white_nhp), digits = 0)
  avg_black_nhp <- round(mean(results$black_nhp), digits = 0)
  avg_asian_nhp <- round(mean(results$asian_nhp), digits = 0)
  avg_hispanic_other <- round(mean(results$hispanic_other), digits = 0)

  per_white_nhp <- round(((avg_white_nhp / avg_total) * 100), digits = 2)
  per_black_nhp <- round(((avg_black_nhp / avg_total) * 100), digits = 2)
  per_asian_nhp <- round(((avg_asian_nhp / avg_total) * 100), digits = 2)
  per_hispanic_other <- round(((avg_hispanic_other / avg_total) * 100), digits = 2)

  
  summary_table <- tibble(
    Occupation = label,
    Group = c("Total", "White Non-Hispanic", "Black Non-Hispanic", 
              "Asian Non-Hispanic", "Hispanic Other races"), 
    `Average` = c(avg_total, avg_white_nhp, avg_black_nhp, 
                  avg_asian_nhp, avg_hispanic_other), 
    `Percentage of Total (%)` = c(100, per_white_nhp, per_black_nhp, 
                                  per_asian_nhp, per_hispanic_other) 
  )
  
  # Add to list
  summary_tables_list[[label]] <- summary_table
}

# Combine all summary tables into one data frame
combined_summary <- bind_rows(summary_tables_list)

# Print or export
print(combined_summary)

# Optional: write to CSV
write_csv(combined_summary, "combined_occupation_summary_final3.csv")


