library(tidyverse)
library(tidygeocoder)
library(stringr)

apps_data <- readRDS("data/cleaned/cleaned_application_data.rds")
notice_data <- readRDS("data/cleaned/cleaned_notice_data.rds")

addresses <- c(apps_data$rental_unit_address, notice_data$rental_unit_address)
case_numbers <- c(apps_data$case_number, notice_data$case_number)
file_source <- c(apps_data$file_source, notice_data$file_source)

address_table <- tibble(original_address = addresses, 
                        formatted_address = paste(addresses, "Ontario, Canada"), 
                        case_number = case_numbers, 
                        file_source = file_source) %>%
  distinct() |>
  filter(!is.na(original_address) & original_address != "")


unique_addresses <- address_table |>
  group_by(formatted_address) |>
  slice(1) |>
  ungroup()



#######  Format Addresses for Geocoding

# I will do some minor cleaning of addresses before geocoding. 

# There are a lot of strangely formatted addresses. They include descriptors like "upper floor unit", "garage unit".. but Bing search returns most of these accurately.
# I will geocode first and then try to flag the errors and clean them up.

### Remove "(No Street Number)" 

# There are a lot of "No street Number" indicators. Most actually have street number included with the street name field. 
# Applicants must have not adhered to splitting out address components in the specified fields.


### Remove unit indicators: 

# Most "No street Number" indicators actually have street number included with the street name field. 
# Applicants must have not adhered to splitting out address components in the specified fields
# I will remove these strings


### Remove PO Box locations

# Some entries contain PO Box and rental unit address in the same string. I will remove PO Box to ensure the wrong location is not geocoded.



# I will however remove PO Box entries and unit indicators. Some entries contain PO Box and rental unit address in the same string. I will remove PO Box to ensure the wrong location is not geocoded.

# unique_addresses <- unique_addresses |> 
#   mutate(formatted_address = gsub("PO BOX[^ ]* [^ ]* ", "", formatted_address, ignore.case = TRUE))


remove_unit_number <- function(address) {
  if (!is.character(address)) {
    address <- as.character(address)
  }
  
  # Rules to address the variety of formats for unit indicators
  
  # Rule 1: Remove "Unit" followed by a number and a space
  address <- str_replace_all(address, "(?i)Unit\\s*\\d+\\s", "")
  
  # Rule 2: Remove the whole pattern when there's a fraction "1/2"
  address <- str_replace_all(address, "\\d+/\\d+", "")
  
  # Rule 2: Remove the whole pattern when there's a fraction "1/2"
  address <- str_replace_all(address, "\\d+\\\\\\d+", "")
  
  # Rule 3: Remove part before "-" for patterns like "1-122"
  address <- str_replace(address, "\\d+-(\\d+)", "\\1")
  
  # Rule 3: Remove part before "-" for patterns like "1-122" or "1 - 122"
  address <- str_replace(address, "\\d+\\s*-\\s*(\\d+)", "\\1")
  
  # Rule 3: Remove ".5" part for patterns like "122.5"
  address <- str_replace_all(address, "\\.\\d+", "")
  
  
  return(address)
}


unique_addresses_no_unit <- unique_addresses |> 
  mutate(formatted_address = remove_unit_number(formatted_address))



# Remove descriptors from string
strings_to_remove <- c("\\(No Street Number\\)", "Basement Unit", "Back Unit", "Unit ", "Basement ", 
                       "Room ", "apt ", "Garage Unit", "Garage", "Ground Floor", "Ground ", 
                       "REAR GROUND FLOOR UNIT", "Rear Unit", "Rear ")

# Create a case-insensitive regex pattern
pattern <- str_c("(?i)", str_c(strings_to_remove, collapse = "|"))

remove_strings <- function(address) {
  if (!is.character(address)) {
    address <- as.character(address)
  }
  
  # Remove all specified strings case-insensitively
  address <- str_replace_all(address, pattern, "")
  
  return(address)
}

# Apply the function to our data
unique_addresses_removed_strings <- unique_addresses_no_unit |> 
  mutate(formatted_address = remove_strings(formatted_address))


# Function to clean the string
clean_address <- function(address) {
  # Remove leading special characters (e.g., commas, hyphens, slashes)
  address <- str_replace(address, "^[[:punct:][:space:]]+", "")
  
  address <- str_replace(address, "^[[:punct:]]+", "")
  
  
  # Trim leading and trailing whitespace
  address <- str_trim(address, side = "both")
  
  return(address)
}

# Apply clean_address function to data
cleaned_addresses <- unique_addresses_removed_strings |>
  mutate(formatted_address = clean_address(formatted_address),
         formatted_address = str_replace(formatted_address, "  ", " "),
         formatted_address = gsub("\\^|`", "", formatted_address)) 


unique_cleaned_addresses <- cleaned_addresses |>
  group_by(formatted_address) |>
  slice(1) |>
  ungroup()


# Save the cleaned addresses
#saveRDS(unique_cleaned_addresses, "data/cleaned/cleaned_addresses.rds")



