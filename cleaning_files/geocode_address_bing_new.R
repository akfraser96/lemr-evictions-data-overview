null_geocode_return <- function(){
  dplyr::tibble(status_code =  NA_character_, address = NA_character_,
                locality = NA_character_, admin_district = NA_character_,
                admin_district_2 = NA_character_,
                formatted_address = NA_character_, postal_code = NA_character_,
                method = NA_character_, confidence = NA_character_,
                latitude = NA_real_, longitude = NA_real_,
                cached_date = NA_character_)
}


# TODO: reafactor and use this?
# the first log_and_stop doesn't make sense since I want to support both - this
# should be the responsibility of the cleaner file to choose the appropriate one
# for the dataset, not this section of the pipeline.
municipality_rename <- function(df, clean_address, region){
  rlog::log_info(paste("Region is ", region))
  if("address.locality" %in% names(df) & "address.adminDistrict2" %in% names(df)){
    log_and_stop("Both address.locality and address.adminiDistrict2 in return. Figure this out. Put in some per-city logic  ", clean_address)
  } else if("address.locality" %in% names(df) ){

    return(df |>
           rename(municipality = address.locality))

  } else if("address.adminDistrict2" %in% names(df)){
    return(df |>
           rename(municipality = address.adminDistrict2))

  } else{
    log_and_stop("Neither address.locality nor address.adminDistrict2 in return. Figure this out. Put in some per-city logic  ", clean_address)
  }
}


#' Replaced by geocode_address for external use.
geocode_address_bing <- function(clean_address, region, base = "http://dev.virtualearth.net/REST/v1/Locations/", token = Sys.getenv("BING_TOKEN"), quiet = FALSE) {
  geocode_address(
    clean_address = clean_address,
    base = base,
    token = token,
    quiet = quiet)
}


global_addresses <<- data.frame()
#' For external use. geocode_address_bing curries the unused region parameter
#' of geocode_address_bing.
#'@export
geocode_address <- function(clean_address, base = "http://dev.virtualearth.net/REST/v1/Locations/", token = Sys.getenv("BING_TOKEN"), quiet = FALSE) {
  # Error if no token
  if (is.null(token) | identical(token, "")) { stop("No Bing token provided", call. = FALSE) }

  call <- glue::glue("{base}{clean_address}?maxResults=1&key={token}") # Full call URL

  
  status_code <- NULL
  tryCatch({
    # Get geocoding
    geocode_result <- httr::GET(call)
    status_code <- geocode_result[["status_code"]]
  }, error = function(e) {
    rlog::log_warn(e)
    rlog::log_warn(glue::glue("URL is: {call}"))
  })

  ## A single retry
  if (status_code != 200 || is.null(status_code)) {
    Sys.sleep(.25)
    tryCatch({
      # Get geocoding
      geocode_result <- httr::GET(call)
      status_code <- geocode_result[["status_code"]]
    }, error = function(e) {
      rlog::log_warn(e)
      rlog::log_warn(glue::glue("URL failed on 2 subsequent calls. Skipping."))
    })
  }


  if (status_code != 200 || is.null(status_code)) {
    # Display progress if quiet = FALSE
    if (!quiet) { rlog::log_info(glue::glue("Fetching {clean_address} - Status: {status_code}")) 
      global_addresses <<- rbind(global_addresses, list(class_name=global_cleaner_class_name, clean_address=clean_address, call=call, status_code=status_code))
    }

    # Return all NAs with status code
    res <-  null_geocode_return()
    res$status_code <- paste("Single:", status_code)


  } else if (status_code == 200) { # If successful (status code 200), extract address

    geocode_json <- httr::content(geocode_result, "text")
    geocode_json_tidied <- geocode_json |>
      jsonlite::fromJSON(flatten = TRUE) %>%
      purrr::pluck("resourceSets") %>%
      # Extract the element named "resourceSets"
      dplyr::pull(.data$resources) %>%
      # Column named "resources"
      purrr::pluck(1) # First element

    res <- tibble(geocode_json_tidied)
    res$status_code <- paste0("Single: ", status_code)
    res <- res |>
      select(any_of(
        c("status_code",
          "address.addressLine",
          "address.locality",
          "address.adminDistrict",
          "address.adminDistrict2",
          "address.formattedAddress",
          "address.postalCode",
          "geocodePoints")))

    # If the address is NULL, return NAs for everything - there may still be results for latitude, longitude, etc, but in the case of Toronto, when it can't find it, it just returns the lat / long for city hall! Eek! Better to return NAs to make it clear that the geocoding failed.

    # Also change the status code to 404, "not found" - this is probably the closest option and I'd rather also flag issues this way, rather than returning 200 (= all good)
    if((!"address.addressLine" %in% names(res))){
      if(length(res$address.addressLine) == 0) {
      # if(is.na(res$address.addressLine)) {
        res <-  null_geocode_return() |>   mutate(status_code = "Single: Bing returned city hall location.")
      }

      names(res) <- glue::glue("bing_{names(res)}")

      Sys.sleep(0.25) # Sleep for 0.25 seconds to comply with API, which only allows for 5 calls per second

      return(res)
    }

    if (!quiet) rlog::log_info(glue::glue("Fetching {clean_address} - Status: 200"))

    geocode_points_tidied <-  res$geocodePoints |>
      purrr::pluck(1) %>%
      dplyr::slice(1) %>%
      dplyr::filter(.data$usageTypes == "Display")

    # Method of determining geocoding
    method <- geocode_points_tidied[["calculationMethod"]]

    # Confidence of geocoding
    confidence <- geocode_json_tidied[["confidence"]]

    # Latitude and longitude
    latitude_longitude <- geocode_points_tidied %>%
      dplyr::pull(.data$coordinates) %>% # Pull coordinates
      purrr::pluck(1) # In a list, so first element

    latitude <- latitude_longitude[[1]]
    longitude <- latitude_longitude[[2]]

    res <-  mutate(res,
              method = method,
              confidence = confidence,
              latitude = latitude,
              longitude = longitude) |>
                select(-(geocodePoints)) |>
                  set_blank_if_missing_else_rename(
                    old_key = "address.addressLine", new_key = "address") |>
                  set_blank_if_missing_else_rename(
                    old_key = "address.locality", new_key = "locality") |>
                  set_blank_if_missing_else_rename(
                    old_key = "address.adminDistrict", new_key = "admin_district") |>
                  set_blank_if_missing_else_rename(
                    old_key = "address.adminDistrict2", new_key = "admin_district2") |>
                  set_blank_if_missing_else_rename(
                    old_key = "address.formattedAddress", new_key = "formatted_address") |>
                  set_blank_if_missing_else_rename(
                    old_key = "address.postalCode", new_key = "postal_code")
  }

  all_cols <- null_geocode_return() |>    rename_all(function(x) paste0("bing_", x)  ) |>
    head(0) |>
    mutate(across(everything(), as.character)) |>
    mutate(across(all_of(c("bing_latitude", "bing_longitude")), as.numeric))

  res <- res |> rename_all(function(x) paste0("bing_", x)  )

  res <- bind_rows(all_cols, res)
  Sys.sleep(.25)
  return(res)
}
