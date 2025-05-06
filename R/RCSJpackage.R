#' @title Search Dryad repository for datasets containing a keyword
#' @description
#' Given a keyword or phrase, the `search_datasets()` function queries the Dryad API and returns metadata for matching datasets in the form of a tibble. Each row in the result includes the dataset title, authors, publication year, DOI, and methods section (if available).
#' @param keyword A string to search for in dataset metadata.
#' @param max_results Maximum number of results to return
#' @importFrom jsonlite read_json (or substitute with appropriate package and function)
#' @return A tibble with dataset title, authors, publication year, and DOI link
#' @examples
#' search_datasets(c("climate", "butterfly"), 50)
#' @export

search_datasets <- function(keyword, max_results = 100) {

  # construct the API URL by inserting key word(s) for search
  url <- httr::modify_url(
    "https://datadryad.org/api/v2/search",
    query = list(q = paste(keyword, collapse = " "), # collapse multiple keywords into one string separated by space
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
  tibble::tibble(
    title = data$title,
    authors = data$author,
    year = data$publicationDate,
    doi = data$identifier,
    methods = data$methods
  )[1:min(max_results, nrow(data)), ]
}
