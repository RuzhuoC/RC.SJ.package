#' @title Search Dryad repository for datasets containing a keyword
#' @description
#' Given a keyword or phrase, the `search_datasets()` function queries the Dryad API and returns metadata for matching datasets in the form of a tibble. Each row in the result includes the dataset title, authors, publication year, DOI, and methods section (if available).
#' @param keyword A character vector of keywords or phrases to search for.
#' @param max_results Maximum number of results to return
#' @return A tibble with dataset title, authors, publication year, and DOI link
#' @examples
#' search_datasets(c("climate", "butterfly"), 50)
#' @export

search_datasets <- function(keyword, max_results = 100) {

  # construct the API URL by inserting key word(s) for search
  url <- httr::modify_url(
    "https://datadryad.org/api/v2/search",
    query = list(q = paste(keyword, collapse = " "), # collapse multiple keywords into one string
                 per_page = max_results) # number of results to return on one page
  ) # note that the API can only handle a single string value for the q parameter

  # send an HTTP GET request to the URL we built, store response
  response <- httr::GET(url)

  # check if the response is an error, if so return empty table and a warning
  if (httr::http_error(response)) {
    warning("Failed to connect to Dryad API.")
    return(tibble::tibble())
  }

  json <- httr::content(response, as = "text", encoding = "UTF-8") # extract content in response into JSON-format string
  parsed <- jsonlite::fromJSON(json, flatten = TRUE) # convert JSON into a nested list/dataframe

  # get the actual dataset entries from nested JSON object (parsed)
  data <- parsed$`_embedded`$`stash:datasets`

  if (length(data) == 0) {
    message("No datasets found.")
    return(tibble::tibble())
  } # if no result, return empty tibble and message

  # build a tibble with the extracted content
  search_result <-
    tibble::tibble(
      title = data$title,
      authors = data$author,
      year = data$publicationDate,
      doi = data$identifier,
      methods = data$methods,
      abstract = data$abstract
    )[1:min(max_results, nrow(data)), ]

  return(search_result)
}


#' @title Search for datasets that use a specific method
#' @description
#' Given a search result tibble from `search_datasets()`, `search_method()` filters datasets whose methods OR abstract contain one or more target keywords.
#' @param result A tibble returned from `search_datasets()`.
#' @param keywords Character vector of search keywords.
#' @param match_all Logical. If TRUE, all keywords must appear (AND logic). If FALSE (default), any keyword may appear (OR logic).
#' @return A filtered tibble of matching datasets.
#' @examples
#' search_datasets("climate", 50) |> search_method(c("experiment", "heatwave"))
#' @export

search_method <- function(result, keywords, match_all = FALSE) {

  # filter for rows where either methods or abstract is not NA
  filtered <- dplyr::filter(result, !is.na(methods) | !is.na(abstract))

  # combine methods & abstract into one string
  combined_content <- paste(filtered$methods, filtered$abstract, sep = " ")

  # create a regex string for searching
  search_pattern <- paste(keywords, collapse = if (match_all) ".*" else "|")

  # check whether the regex pattern is found in each string, and filter on "filtered" tibble
  method_result <-
    filtered[stringr::str_detect(combined_content,
                                 stringr::regex(search_pattern,
                                       ignore_case = TRUE)), # ignore difference between upper and lowercase
             ]

  return(method_result)
}


#' @title Download datasets from Dryad
#' @description
#' Downloads all datasets (ZIP files) from a tibble returned by `search_datasets()` or `search_method()` to the current working directory.
#' @param result A tibble with a 'doi' column, from `search_datasets()` or `search_method()`.
#' @return A tibble with columns: title, doi, and the local file path of each successfully downloaded dataset.
#' @examples
#' search_datasets(c("climate", "butterfly"), max_results = 10) |>
#'   search_method(c("experiment", "heatwave"), match_all = FALSE) |>
#'   download_dataset()
#' @export

download_dataset <- function(result) {
  # stop if invalid input (input not a data frame)
  if (!is.data.frame(result)) {
    stop("Input must be a data frame")
  }
  if (nrow(result) == 0) {
    message("No datasets to download")
    return(invisible(NULL))
  }


  # set download destination to current working directory
  dir <- "."

  # create empty character vector to store the file paths of downloaded files, with length equal to # rows in result
  downloaded <- character(nrow(result))

  # loop over each row in result
  for (i in seq_len(nrow(result))) {
    doi_raw <- result$doi[i] # extract doi string
    doi_encoded <- utils::URLencode(doi_raw, reserved = TRUE) # convert special characters to safe URL symbols
    download_url <- paste0("https://datadryad.org/api/v2/datasets/", doi_encoded, "/download") # build the full Dryad API url

    safe_name <- gsub("[:/]", "_", doi_raw)
    file_path <- file.path(dir, paste0(safe_name, ".zip")) # build full path where the dataset will be saved

    # try to download the dataset
    success <- tryCatch({
      utils::download.file(download_url, file_path, mode = "wb", quiet = TRUE)
      TRUE
    }, error = function(e) {
      warning(paste("Failed to download:", doi_raw))
      FALSE # if download fail, return warning
    })

    # stores the file path in the downloaded vector if the download succeeded
    downloaded[i] <- if (success) file_path else NA_character_
  }

  # identify which downloads succeeded (non-NA entries)
  success_indices <- which(!is.na(downloaded)) # identify successful downloads

  # display message and return a tibble of successfully downloaded datasets
  if (length(success_indices) > 0) {
    message("✅ Successfully downloaded ", length(success_indices), " dataset(s):")
  } else {
    message("❌ No datasets were successfully downloaded.")
  }

  tibble::tibble(
    title = result$title[success_indices],
    doi   = result$doi[success_indices],
    file  = downloaded[success_indices]
  )

}
