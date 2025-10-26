#' Fetch political advertising transparency data
#'
#' @description
#' Retrieve political advertising transparency data from Dutch transparency APIs.
#' Supports both PolitiekeReclame.nl and DPG Media APIs with unified interface.
#'
#' @param source Character string: "politiekereclame" or "dpgmedia". Default: "politiekereclame".
#' @param query Character string for free-text search. Use \code{NULL} to omit.
#' @param theme Character string to filter by theme. Use \code{NULL} to omit.
#' @param country_code Character string for country code (DPG Media only). Default: "NL".
#' @param period Character string for time period (DPG Media only). Options: "PAST_7_DAYS",
#'   "PAST_30_DAYS", "PAST_YEAR". Default: "PAST_7_DAYS".
#' @param channel Character string for channel type (DPG Media only). Options: "PRINT", "DIGITAL",
#'   or \code{NULL} for all channels. Default: \code{NULL}.
#' @param page Integer page number (>= 1). Ignored when \code{all = TRUE}.
#' @param limit Integer number of results per page (1-1000).
#' @param all Logical. If \code{TRUE}, automatically fetches all pages.
#' @param max_pages Integer maximum pages to fetch when \code{all = TRUE}.
#' @param sleep Numeric seconds to pause between requests during auto-pagination.
#' @param extra_params Named list of additional query parameters.
#' @param headers Named list of HTTP headers to include.
#' @param return Character string: "data" (tibble), "list" (parsed JSON), or "response" (raw).
#' @param verbose Logical. Print progress during auto-pagination.
#'
#' @details
#' This function provides a unified interface to Dutch political advertising
#' transparency APIs. It automatically handles the differences between APIs
#' and provides consistent output formatting.
#'
#' @return
#' Depends on \code{return} parameter:
#' \itemize{
#'   \item \code{"data"} (default): A tibble with one row per statement
#'   \item \code{"list"}: Parsed JSON as R list
#'   \item \code{"response"}: Raw httr2 response object(s)
#' }
#'
#' @examples
#' \dontrun{
#' # PolitiekeReclame.nl API
#' fetch_ads(query = "election")
#' fetch_ads(theme = "Tweede Kamerverkiezingen 2025")
#' fetch_ads(query = "campaign", all = TRUE, verbose = TRUE)
#' 
#' # DPG Media API
#' fetch_ads(source = "dpgmedia", period = "PAST_30_DAYS")
#' fetch_ads(source = "dpgmedia", channel = "PRINT", period = "PAST_YEAR")
#' }
#'
#' @importFrom httr2 request req_url_query req_headers req_user_agent req_perform
#'   req_error req_retry resp_body_json resp_status
#' @importFrom tibble tibble
#' @importFrom dplyr bind_rows
#' @export
fetch_ads <- function(
    source = c("politiekereclame", "dpgmedia"),
    query = NULL,
    theme = NULL,
    country_code = "NL",
    period = c("PAST_7_DAYS", "PAST_30_DAYS", "PAST_YEAR"),
    channel = NULL,
    page = 1L,
    limit = 25L,
    all = FALSE,
    max_pages = 50L,
    sleep = 0.2,
    extra_params = NULL,
    headers = NULL,
    return = c("data", "list", "response"),
    verbose = FALSE
) {
  # Input validation and parameter matching
  source <- match.arg(source)
  period <- match.arg(period)
  return <- match.arg(return)
  
  # Validate inputs based on source
  if (source == "politiekereclame") {
    validate_politiekereclame_inputs(query, theme, page, limit, all, max_pages, sleep, extra_params, headers)
  } else {
    validate_dpgmedia_inputs(country_code, period, channel, headers)
  }
  
  # Route to appropriate API handler
  if (source == "politiekereclame") {
    return(handle_politiekereclame(query, theme, page, limit, all, max_pages, sleep, extra_params, headers, return, verbose))
  } else {
    return(handle_dpgmedia(country_code, period, channel, headers, return, verbose))
  }
}

# Input validation for PolitiekeReclame API
validate_politiekereclame_inputs <- function(query, theme, page, limit, all, max_pages, sleep, extra_params, headers) {
  stopifnot(
    "query must be NULL or single character string" = 
      is.null(query) || (is.character(query) && length(query) == 1),
    "theme must be NULL or single character string" = 
      is.null(theme) || (is.character(theme) && length(theme) == 1),
    "page must be single integer >= 1" = 
      is.numeric(page) && length(page) == 1 && page >= 1,
    "limit must be single integer between 1 and 1000" = 
      is.numeric(limit) && length(limit) == 1 && limit >= 1 && limit <= 1000,
    "all must be single logical value" = 
      is.logical(all) && length(all) == 1,
    "max_pages must be single integer >= 1" = 
      is.numeric(max_pages) && length(max_pages) == 1 && max_pages >= 1,
    "sleep must be single non-negative number" = 
      is.numeric(sleep) && length(sleep) == 1 && sleep >= 0,
    "extra_params must be NULL or named list" = 
      is.null(extra_params) || (is.list(extra_params) && !is.null(names(extra_params))),
    "headers must be NULL or named list" = 
      is.null(headers) || (is.list(headers) && !is.null(names(headers)))
  )
}

# Input validation for DPG Media API
validate_dpgmedia_inputs <- function(country_code, period, channel, headers) {
  stopifnot(
    "country_code must be single character string" = 
      is.character(country_code) && length(country_code) == 1,
    "channel must be NULL or single character string" = 
      is.null(channel) || (is.character(channel) && length(channel) == 1),
    "headers must be NULL or named list" = 
      is.null(headers) || (is.list(headers) && !is.null(names(headers)))
  )
}

# Handle PolitiekeReclame API
handle_politiekereclame <- function(query, theme, page, limit, all, max_pages, sleep, extra_params, headers, return, verbose) {
  # Build request
  request_obj <- build_politiekereclame_request(extra_params, headers)
  
  # Execute based on mode
  if (!all) {
    return(execute_single_request(request_obj, query, theme, page, limit, extra_params, return))
  } else {
    return(execute_pagination(request_obj, query, theme, limit, extra_params, max_pages, sleep, verbose, return))
  }
}

# Handle DPG Media API
handle_dpgmedia <- function(country_code, period, channel, headers, return, verbose) {
  # Build request
  request_obj <- build_dpgmedia_request(headers)
  
  # Execute request
  response <- perform_dpgmedia_request(request_obj, country_code, period, channel, verbose)
  
  # Return based on type
  switch(return,
    "response" = response$response,
    "list" = response$parsed,
    "data" = convert_dpgmedia_to_tibble(response$parsed)
  )
}

# Build PolitiekeReclame HTTP request
build_politiekereclame_request <- function(extra_params, headers) {
  base_url <- "https://politiekereclame.nl/api/public/statements"
  
  # Create base request with error handling and retries
  request_obj <- httr2::request(base_url) |>
    httr2::req_user_agent(create_user_agent()) |>
    httr2::req_error(is_error = function(resp) httr2::resp_status(resp) >= 400) |>
    httr2::req_retry(max_tries = 3, backoff = ~ .x + runif(1, 0, 0.25)) |>
    httr2::req_headers(!!!create_headers(headers))
  
  return(request_obj)
}

# Build DPG Media HTTP request
build_dpgmedia_request <- function(headers) {
  base_url <- "https://privacy.dpgmedia.nl/api/campaign-items"
  
  # Create base request with error handling and retries
  request_obj <- httr2::request(base_url) |>
    httr2::req_user_agent(create_user_agent()) |>
    httr2::req_error(is_error = function(resp) httr2::resp_status(resp) >= 400) |>
    httr2::req_retry(max_tries = 3, backoff = ~ .x + runif(1, 0, 0.25)) |>
    httr2::req_headers(!!!create_dpgmedia_headers(headers))
  
  return(request_obj)
}

# Create user agent string
create_user_agent <- function() {
  sprintf("fetch_ads/1.0.0 (R/%s; %s)", 
          getRversion(), R.version$platform)
}

# Create HTTP headers
create_headers <- function(custom_headers) {
  default_headers <- list(
    "Accept" = "application/json, text/plain, */*",
    "Referer" = "https://politiekereclame.nl/transparantie-verklaringen",
    "Accept-Language" = "en-US,en;q=0.9,nl;q=0.7"
  )
  
  if (is.null(custom_headers)) {
    return(default_headers)
  }
  
  modifyList(default_headers, custom_headers)
}

# Execute single page request
execute_single_request <- function(request_obj, query, theme, page, limit, extra_params, return) {
  query_params <- create_query_params(query, theme, page, limit, extra_params)
  response <- perform_request(request_obj, query_params)
  
  switch(return,
    "response" = response$response,
    "list" = response$parsed,
    "data" = convert_to_tibble(response$parsed)
  )
}

# Execute pagination across multiple pages
execute_pagination <- function(request_obj, query, theme, limit, extra_params, max_pages, sleep, verbose, return) {
  page <- 1L
  all_responses <- list()
  all_parsed <- list()
  
  while (page <= max_pages) {
    if (verbose) {
      message(sprintf("Fetching page %d...", page))
    }
    
    query_params <- create_query_params(query, theme, page, limit, extra_params)
    response <- perform_request(request_obj, query_params)
    
    all_responses[[page]] <- response$response
    all_parsed[[page]] <- response$parsed
    
    # Check if we've reached the last page
    if (count_items(response$parsed) < limit) {
      break
    }
    
    page <- page + 1L
    if (sleep > 0) {
      Sys.sleep(sleep)
    }
  }
  
  switch(return,
    "response" = all_responses,
    "list" = all_parsed,
    "data" = combine_tibbles(all_parsed)
  )
}

# Create query parameters
create_query_params <- function(query, theme, page, limit, extra_params) {
  params <- list(
    q = query,
    page = as.integer(page),
    limit = as.integer(limit),
    theme = theme
  )
  
  # Add extra parameters if provided
  if (!is.null(extra_params)) {
    params <- modifyList(params, extra_params, keep.null = TRUE)
  }
  
  # Remove NULL values to avoid empty parameters
  params[!sapply(params, is.null)]
}

# Perform HTTP request
perform_request <- function(request_obj, query_params) {
  request_with_params <- httr2::req_url_query(request_obj, !!!query_params)
  response <- httr2::req_perform(request_with_params)
  
  parsed <- tryCatch(
    httr2::resp_body_json(response, simplifyVector = FALSE),
    error = function(e) {
      stop("Failed to parse JSON response: ", e$message, call. = FALSE)
    }
  )
  
  list(response = response, parsed = parsed)
}

# Count items in parsed response
count_items <- function(parsed) {
  if (is.list(parsed) && !is.null(parsed$items)) {
    return(length(parsed$items))
  }
  if (is.list(parsed)) {
    return(length(parsed))
  }
  return(length(parsed))
}

# Convert parsed response to tibble
convert_to_tibble <- function(parsed) {
  items <- extract_items(parsed)
  
  if (length(items) == 0) {
    return(tibble::tibble())
  }
  
  # Process items to handle nested structures
  processed_items <- process_nested_fields(items)
  
  # Convert to tibble
  dplyr::bind_rows(processed_items)
}

# Extract items from parsed response
extract_items <- function(parsed) {
  if (is.list(parsed) && !is.null(parsed$items)) {
    return(parsed$items)
  }
  return(parsed)
}

# Process nested fields to prevent duplication
process_nested_fields <- function(items) {
  lapply(items, function(item) {
    for (field_name in names(item)) {
      field_value <- item[[field_name]]
      if (is.list(field_value) && length(field_value) > 1) {
        # Wrap in list to preserve as list column
        item[[field_name]] <- list(field_value)
      }
    }
    item
  })
}

# Combine multiple tibbles from pagination
combine_tibbles <- function(parsed_list) {
  tibbles <- lapply(parsed_list, convert_to_tibble)
  dplyr::bind_rows(tibbles)
}

# DPG Media specific functions
create_dpgmedia_headers <- function(custom_headers) {
  default_headers <- list(
    "accept" = "*/*",
    "accept-language" = "en-US,en;q=0.9,de-DE;q=0.8,de;q=0.7,nl;q=0.6,it;q=0.5,sv;q=0.4,is;q=0.3",
    "priority" = "u=1, i",
    "sec-ch-ua" = '"Google Chrome";v="141", "Not?A_Brand";v="8", "Chromium";v="141"',
    "sec-ch-ua-mobile" = "?0",
    "sec-ch-ua-platform" = '"macOS"',
    "sec-fetch-dest" = "empty",
    "sec-fetch-mode" = "cors",
    "sec-fetch-site" = "same-origin",
    "Referer" = "https://privacy.dpgmedia.nl/nl/political-ads"
  )
  
  if (is.null(custom_headers)) {
    return(default_headers)
  }
  
  modifyList(default_headers, custom_headers)
}

perform_dpgmedia_request <- function(request_obj, country_code, period, channel, verbose) {
  # Create query parameters
  query_params <- create_dpgmedia_query_params(country_code, period, channel)
  
  # Add query parameters to request
  request_with_params <- httr2::req_url_query(request_obj, !!!query_params)
  
  if (verbose) {
    message("Fetching campaign data from DPG Media...")
  }
  
  # Execute request
  response <- httr2::req_perform(request_with_params)
  
  # Parse JSON response
  parsed <- tryCatch(
    httr2::resp_body_json(response, simplifyVector = FALSE),
    error = function(e) {
      stop("Failed to parse JSON response: ", e$message, call. = FALSE)
    }
  )
  
  list(response = response, parsed = parsed)
}

create_dpgmedia_query_params <- function(country_code, period, channel) {
  params <- list(
    countryCode = country_code,
    period = period
  )
  
  # Add channel if specified
  if (!is.null(channel)) {
    params$channel <- channel
  }
  
  # Remove NULL values
  params[!sapply(params, is.null)]
}

convert_dpgmedia_to_tibble <- function(parsed) {
  if (is.null(parsed) || length(parsed) == 0) {
    return(tibble::tibble())
  }
  
  # Handle different response structures
  if (is.list(parsed) && !is.null(parsed$items)) {
    items <- parsed$items
  } else if (is.list(parsed)) {
    items <- parsed
  } else {
    return(tibble::tibble())
  }
  
  if (length(items) == 0) {
    return(tibble::tibble())
  }
  
  # Process items to handle nested structures
  processed_items <- process_dpgmedia_nested_fields(items)
  
  # Convert to tibble
  dplyr::bind_rows(processed_items)
}

process_dpgmedia_nested_fields <- function(items) {
  lapply(items, function(item) {
    for (field_name in names(item)) {
      field_value <- item[[field_name]]
      if (is.list(field_value) && length(field_value) > 1) {
        # Wrap in list to preserve as list column
        item[[field_name]] <- list(field_value)
      }
    }
    item
  })
}

# Utility function for NULL coalescing
`%||%` <- function(x, y) if (is.null(x)) y else x