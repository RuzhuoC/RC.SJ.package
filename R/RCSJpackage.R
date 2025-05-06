#' @title Search Dryad repository for datasets containing a keyword
#' @description
#' Given a keyword or phrase, the `search_datasets()` function queries the Dryad API and returns metadata for matching datasets in the form of a tibble. Each row in the result includes the dataset title, authors, publication year, DOI, and methods section (if available).
#' @param keyword A string to search for in dataset metadata.
#' @param max_results Maximum number of results to return
#' @importFrom jsonlite read_json (or substitute with appropriate package and function)
#' @return A tibble with dataset title, authors, publication year, and DOI link
#' @examples
#' search_datasets(c("butterfly", "temperature"), max_results = 10)
#' @export

search_datasets <- function(keyword, max_results = 50) {
  url <- httr::modify_url(
    "https://datadryad.org/api/v2/search",
    query = list(q = keyword)
  )
  response <- httr::GET(url)

  if (httr::http_error(response)) {
    warning("Failed to connect to Dryad API.")
    return(tibble::tibble())
  }

  json <- httr::content(response, as = "text", encoding = "UTF-8")
  parsed <- jsonlite::fromJSON(json, flatten = TRUE)

  # correct nested location
  data <- parsed$`_embedded`$`stash:datasets`

  if (length(data) == 0) {
    message("No datasets found.")
    return(tibble::tibble())
  }

  tibble::tibble(
    title = data$title,
    authors = purrr::map_chr(data$authors, function(author_df) {
      if (is.null(author_df) || is.atomic(author_df) || is.character(author_df)) {
        return(NA_character_)
      }
      if (!"fullName" %in% names(author_df)) {
        return(NA_character_)
      }
      paste(author_df$fullName, collapse = ", ")
    }),
    year = data$publicationDate,
    doi = data$identifier,
    methods = data$methods
  )[1:min(max_results, nrow(data)), ]
}
