library(tidyverse)
library(readxl)


path <- "data/older lemr evictions data - contains organization/Evictions, 2010 to 2020, 20210804/L applications"

file_list <- list.files(path, full.names = TRUE, pattern = "\\.xlsx$")



# Define a function to rename columns if they exist
rename_columns <- function(df) {
  rename_map <- c(
    "Notice" = "Notice Type",
    "Notice Form" = "Notice Type",
    "File Number" = "Case Number",
    "Reason for Closure" = "Disposition",
    "Application Type" = "Category",
    "Applications" = "Case Type"
    
  )
  
  for (original_name in names(rename_map)) {
    if (original_name %in% names(df)) {
      names(df)[names(df) == original_name] <- rename_map[[original_name]]
    }
  }
  return(df)
}


# Read and bind all the files together
combined_organization_data <- file_list |>
  lapply(function(file) {
    df <- read_excel(file)
    df <- rename_columns(df)
    df$file_source <- file
    return(df)
  }) |>
  bind_rows()

combined_organization_data <- combined_organization_data |>
  janitor::clean_names() |>
  select(any_of(c("case_number", "organization_name", "filing_date"))) |>
  filter(!is.na(organization_name))

# Save the combined data to a file
saveRDS(combined_organization_data, "data/correspondence_files/compiled_organization_data.rds")


