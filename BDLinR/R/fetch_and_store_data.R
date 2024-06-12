#' A function to store data directly from BDL
#'
#' With this function user can download data directly from BDL directly into their project. We recommend using our complementary Shiny app to see the Variable IDs.
#'
#' @author Mateusz Pliszka Łukasz Janisiów Dominika Lach
#' @param variableID ID of the data set.
#' @param level Level of aggregation as a number. If left blank function downloads all available years.
#' @param year Year of the variable. If left blank - package downloads all available data.
#' @param name Name of the data set.
#' @param output_dir Directory of desired location.
#' @import httr
#' @import jsonlite
#' @import dplyr
#' @import tidyr
#' @return Data set from BDL. Stored in the specified location as RDS file.
#' @examples
#' fetch_and_store_data(variableID = "3507",level = "5",year = "2020", name = "xdxd")
#' @export

fetch_and_store_data <- function(variableID = "", level ="", year = "", name = "", output_dir = getwd()) {
  library(httr)
  library(jsonlite)
  library(dplyr)
  library(tidyr)
  valid_levels <- c("0","1", "2", "3", "4", "5", "6")
  if (!(level %in% valid_levels)) {
    stop("Error: Level must be one of 1, 2, 3, 4, 5, or 6.")
  }

  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package 'dplyr' is required but not installed.")
  }
  if (!requireNamespace("httr", quietly = TRUE)) {
    stop("Package 'httr' is required but not installed.")
  }
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Package 'jsonlite' is required but not installed.")
  }
  if (!requireNamespace("tidyr", quietly = TRUE)) {
    stop("Package 'tidyr' is required but not installed.")
  }

  multiplequeries <- function(url, client_id = NULL) {
    all_data <- data.frame()  # Initialize an empty data frame to store all responses
    response <- httr::GET(url, add_headers("X-ClientId" = client_id))

    if (httr::status_code(response) == 404) {
      return("Error: Resource not found (404)")
    }

    data <- jsonlite::fromJSON(rawToChar(response$content))

    if (data$totalRecords[1] == 0) {
      return("Error: Wrong level!")
    }

    if (!is.null(data$errors)) {
      print(data$errors)
      return (NULL)
    }
    all_data <- data$results

    while (!is.null(data$links$`next`)) {
      url <- data$links$`next`
      response <- httr::GET(url, add_headers("X-ClientId" = client_id))

      if (httr::status_code(response) == 404) {
        return("Error: Resource not found (404)")
      }

      data <- jsonlite::fromJSON(rawToChar(response$content))
      if (!is.null(data$errors)) {
        print(data$errors)
        return (all_data)
      }
      all_data <- rbind(all_data, data$results)
    }
    return(all_data)
  }

  # Fetch available data based on category
  fetch_available_data <- function(category) {
    if (is.null(year)) {
      url <- paste0("https://bdl.stat.gov.pl/api/v1/subjects?lang=pl&parent-id=", category, "&format=json&page-size=100")
    } else {
      url <- paste0("https://bdl.stat.gov.pl/api/v1/subjects?lang=pl&parent-id=", category, "&year=", year, "&format=json&page-size=100")
    }
    response <- httr::GET(url)

    if (httr::status_code(response) == 404) {
      return("Error: Resource not found (404)")
    }

    data <- jsonlite::fromJSON(rawToChar(response$content))
    return(data$results[c("id", "name")])
  }

  # Fetch data by variable ID and level
  fetch_data_by_variable <- function(variableID, level) {
    if (is.null(year)) {
      url <- paste0("https://bdl.stat.gov.pl/api/v1/data/by-variable/", variableID, "?unit-level=", level, "&format=json&page-size=100")
    } else {
      url <- paste0("https://bdl.stat.gov.pl/api/v1/data/by-variable/", variableID, "?unit-level=", level, "&year=", year, "&format=json&page-size=100")
    }
    data <- multiplequeries(url, NULL)
    if (!is.null(data) && is.character(data) && data == "Error: Resource not found (404)" ) {
      stop(data)
    }
    if (!is.null(data) && is.character(data) && data == "Error: Wrong level!" ) {
      stop(data)
    }
    if (!is.null(data)) {
      unnested_data <- tidyr::unnest(data, values)
      return(unnested_data)
    }
    return(NULL)
  }

  # Get data for the specific variable and level
  final_data <- fetch_data_by_variable(variableID, level)
  if (is.null(final_data)) {
    stop("No data available for the given parameters.")
  }
  print("Data fetched successfully.")

  # Save the data to the specified directory
  saveRDS(final_data, file = file.path(output_dir, paste0("data_", name, ".rds")))
  print(paste0("Data saved to ", file.path(output_dir, paste0("data_", name, ".rds"))))

  return(final_data)
}

