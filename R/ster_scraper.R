#' Scrape and parse Ster political advertising transparency data
#'
#' @description
#' Scrapes the Ster website for political advertising transparency statements,
#' downloads all PDFs, and parses them into a unified data format. Automatically
#' handles version detection, multiple formats per party, and comprehensive
#' data extraction from PDF documents.
#'
#' @param base_url Character string. Base URL of the Ster transparency page.
#'   Default: "https://ster.nl/politiek"
#' @param download_dir Character string. Directory to save PDFs. Default: "ster_pdfs".
#' @param parse_pdfs Logical. Whether to parse PDF content. Default: TRUE.
#' @param extract_tables Logical. Whether to extract tables from PDFs. Default: TRUE.
#' @param extract_text Logical. Whether to extract text content from PDFs. Default: TRUE.
#' @param verbose Logical. Print progress information. Default: TRUE.
#' @param return_list Logical. If TRUE, returns full list with metadata. If FALSE, returns only unified_data. Default: FALSE.
#'
#' @details
#' This function performs comprehensive data collection from the Ster website:
#' \itemize{
#'   \item Scrapes the transparency statements page
#'   \item Identifies all available PDFs with version detection
#'   \item Downloads PDFs with proper naming and organization
#'   \item Parses PDF content using multiple extraction methods
#'   \item Unifies data into a comprehensive tibble format
#' }
#'
#' The function automatically handles:
#' - Version detection (prefers version 2+ over version 1)
#' - Multiple formats per party (TV, radio, online, etc.)
#' - PDF parsing with table and text extraction
#' - Data cleaning and standardization
#'
#' @return
#' If \code{return_list = FALSE} (default): A tibble containing the unified transparency data.
#' If \code{return_list = TRUE}: A list containing:
#' \itemize{
#'   \item \code{metadata}: Scraping metadata and file information
#'   \item \code{raw_data}: Raw extracted data from all PDFs
#'   \item \code{unified_data}: Clean, unified tibble with all transparency data
#'   \item \code{download_info}: Information about downloaded files
#' }
#'
#' @examples
#' \dontrun{
#' # Basic scraping and parsing (returns tibble by default)
#' ster_data <- scrape_ster_transparency()
#'
#' # Return full list with metadata
#' ster_data <- scrape_ster_transparency(return_list = TRUE)
#'
#' # Just download without parsing
#' ster_data <- scrape_ster_transparency(parse_pdfs = FALSE)
#'
#' # Custom download directory
#' ster_data <- scrape_ster_transparency(download_dir = "my_ster_data")
#' }
#'
#' @importFrom httr2 request req_perform resp_body_html
#' @importFrom rvest html_nodes html_attr html_text
#' @importFrom tibble tibble
#' @importFrom dplyr bind_rows mutate filter arrange
#' @importFrom stringr str_detect str_extract str_remove_all
#' @importFrom pdftools pdf_text pdf_info
#' @export
scrape_ster_transparency <- function(
    base_url = "https://ster.nl/politiek",
    download_dir = "ster_pdfs",
    parse_pdfs = TRUE,
    extract_tables = TRUE,
    extract_text = TRUE,
    verbose = TRUE,
    return_list = FALSE
) {
  if (verbose) {
    cat("\n🚀 Starting Ster transparency data scraping...\n")
    cat(paste(rep("=", 50), collapse = ""), "\n")
  }

  # Create download directory
  if (!dir.exists(download_dir)) {
    dir.create(download_dir, recursive = TRUE)
  }

  # Step 1: Scrape the main page
  if (verbose) cat("\n📋 Step 1: Scraping transparency statements page...\n")
  page_data <- scrape_ster_page(base_url, verbose)

  # Step 2: Process and filter PDF links
  if (verbose) cat("\n🔗 Step 2: Processing PDF links and version detection...\n")
  pdf_links <- process_pdf_links(page_data, verbose)

  # Step 3: Download PDFs
  download_info <- download_ster_pdfs(pdf_links, download_dir, verbose, page_data$last_updated)

  # Step 4: Parse PDFs (if requested)
  raw_data <- NULL
  unified_data <- NULL

  if (parse_pdfs) {
    raw_data <- parse_ster_pdfs(download_info$file_paths, download_info$pdf_urls, extract_tables, extract_text, verbose)

    if (verbose) cat("\n📊 Step 5: Creating unified dataset...\n")
    unified_data <- create_unified_ster_data(raw_data, verbose)
  }

  if (verbose) {
    cat("\n✅ Scraping complete!\n")
    cat(paste(rep("=", 50), collapse = ""), "\n")
    cat(sprintf("📁 Downloaded %d PDFs to %s\n", length(download_info$file_paths), download_dir))
    if (!is.null(unified_data)) {
      cat(sprintf("📊 Created unified dataset with %d rows\n", nrow(unified_data)))
    }
    cat("\n")
  }

  # Return format based on return_list argument
  if (return_list) {
    result <- list(
      metadata = list(
        scrape_date = Sys.time(),
        base_url = base_url,
        total_links_found = length(page_data$links),
        total_pdfs_downloaded = length(download_info$file_paths),
        download_dir = download_dir
      ),
      raw_data = raw_data,
      unified_data = unified_data,
      download_info = download_info
    )
    return(result)
  } else {
    # Return just the unified data by default
    return(unified_data)
  }
}

# Scrape the main Ster transparency page
scrape_ster_page <- function(url, verbose) {
  tryCatch({
    response <- httr2::request(url) |>
      httr2::req_user_agent("ster_scraper/1.0.0") |>
      httr2::req_perform()

    html_content <- httr2::resp_body_html(response)

    # Extract last updated date from website
    last_updated <- extract_last_updated_date(html_content, verbose)

    # Extract all download links with their context
    link_elements <- rvest::html_nodes(html_content, "a[href*='.pdf']")
    links <- rvest::html_attr(link_elements, "href")
    link_texts <- rvest::html_text(link_elements)

    # Extract table rows to get party names and link context
    table_rows <- rvest::html_nodes(html_content, "tr")

    # Process each table row to extract structured data
    structured_data <- list()
    current_party <- NULL

    for (row in table_rows) {
      cells <- rvest::html_nodes(row, "td")

      # Check if this row has a party name in the first cell
      if (length(cells) >= 2) {
        party_name <- rvest::html_text(cells[1])
        party_name <- stringr::str_trim(party_name)

        if (party_name != "" && party_name != "Politieke partij" && party_name != "Overig" && party_name != "FunX") {
          current_party <- party_name
        }
      }

      # Process links in this row (either from current party or continuation of previous party due to rowspan)
      if (!is.null(current_party)) {
        # Look for PDF links in any cell of this row
        row_links <- rvest::html_nodes(row, "a[href*='.pdf']")
        for (link in row_links) {
          href <- rvest::html_attr(link, "href")
          link_text <- rvest::html_text(link)

          structured_data[[length(structured_data) + 1]] <- list(
            party = current_party,
            link = href,
            link_text = link_text,
            full_url = ifelse(stringr::str_detect(href, "^http"), href, paste0("https://ster.nl", href))
          )
        }
      }
    }

    list(
      structured_data = structured_data,
      raw_links = links,
      link_texts = link_texts,
      html_content = html_content,
      last_updated = last_updated
    )
  }, error = function(e) {
    stop("Failed to scrape Ster page: ", e$message, call. = FALSE)
  })
}

# Process PDF links with version detection
process_pdf_links <- function(page_data, verbose) {
  structured_data <- page_data$structured_data

  if (length(structured_data) == 0) {
    warning("No PDF links found on the page")
    return(tibble::tibble())
  }

  # Convert structured data to tibble
  processed_links <- tibble::tibble(
    party = sapply(structured_data, function(x) x$party),
    link = sapply(structured_data, function(x) x$link),
    link_text = sapply(structured_data, function(x) x$link_text),
    full_url = sapply(structured_data, function(x) x$full_url),
    filename = basename(sapply(structured_data, function(x) x$link)),
    version = NA_character_,
    format = NA_character_,
    priority = 1L
  )

  # Extract version and format information from link text and URLs
  for (i in seq_len(nrow(processed_links))) {
    link_text <- processed_links$link_text[i]

    # Extract version information (Dutch terms)
    if (stringr::str_detect(link_text, "versie 2|version 2|v2|Download versie 2")) {
      processed_links$version[i] <- "2"
      processed_links$priority[i] <- 1L
    } else if (stringr::str_detect(link_text, "versie 1|version 1|v1|Download versie 1")) {
      processed_links$version[i] <- "1"
      processed_links$priority[i] <- 2L
    } else if (stringr::str_detect(link_text, "Download$")) {
      # If it's just "Download" without version, assume it's the main/latest version
      processed_links$version[i] <- "main"
      processed_links$priority[i] <- 1L
    } else {
      processed_links$version[i] <- "unknown"
      processed_links$priority[i] <- 3L
    }

    # Extract format information (Dutch terms)
    if (stringr::str_detect(link_text, "alleen televisie|televisie en radio|televisie")) {
      processed_links$format[i] <- "TV"
    } else if (stringr::str_detect(link_text, "alleen radio|radio")) {
      processed_links$format[i] <- "Radio"
    } else if (stringr::str_detect(link_text, "alleen online|online directe inkoop|online intermediair|online")) {
      processed_links$format[i] <- "Online"
    } else {
      processed_links$format[i] <- "General"
    }
  }

  # Filter to prefer version 2+ over version 1, but keep all versions when they exist
  # Group by party and format, but if there are multiple versions, keep all of them
  filtered_links <- processed_links |>
    dplyr::arrange(.data$party, .data$format, .data$priority) |>
    dplyr::group_by(.data$party, .data$format) |>
    dplyr::mutate(
      # If there are multiple versions for the same party-format combination, keep all
      # Otherwise, keep only the highest priority (version 2 > version 1 > main > unknown)
      group_size = dplyr::n(),
      keep_row = ifelse(.data$group_size > 1, TRUE, dplyr::row_number() == 1)
    ) |>
    dplyr::filter(.data$keep_row) |>
    dplyr::select(-"keep_row", -"group_size") |>
    dplyr::ungroup()

  if (verbose) {
    cat(sprintf("✅ Processed %d PDF links, filtered to %d unique entries\n",
                nrow(processed_links), nrow(filtered_links)))
    cat("Parties found: ", paste(unique(filtered_links$party), collapse = ", "), "\n")
  }

  return(filtered_links)
}

# Download PDFs from processed links
download_ster_pdfs <- function(links_df, download_dir, verbose, last_updated = NULL) {
  if (nrow(links_df) == 0) {
    return(list(file_paths = character(0), success = logical(0), pdf_urls = character(0)))
  }

  file_paths <- character(nrow(links_df))
  pdf_urls <- character(nrow(links_df))
  success <- logical(nrow(links_df))

  # Create progress bar
  if (verbose) {
    cat("\n📥 Downloading PDFs...\n")
    pb <- txtProgressBar(min = 0, max = nrow(links_df), style = 3, char = "█")
  }

  for (i in seq_len(nrow(links_df))) {
    url <- links_df$full_url[i]
    party <- links_df$party[i]
    version <- links_df$version[i]
    format <- links_df$format[i]

    # Create filename with better logic
    safe_party <- stringr::str_remove_all(party, "[^A-Za-z0-9]")
    safe_party <- stringr::str_replace_all(safe_party, "\\s+", "_")

    # Create filename based on version
    if (version == "unknown") {
      # Use original filename if available
      original_filename <- basename(links_df$link[i])
      if (stringr::str_detect(original_filename, "\\.pdf$")) {
        filename <- original_filename
      } else {
        filename <- sprintf("%s_%s.pdf", safe_party, format)
      }
    } else if (version == "main") {
      # For main version, don't include version in filename
      filename <- sprintf("%s_%s.pdf", safe_party, format)
    } else {
      # For specific versions, include version number
      filename <- sprintf("%s_v%s_%s.pdf", safe_party, version, format)
    }

    file_path <- file.path(download_dir, filename)

    # Check if we should force re-download based on website update
    force_redownload <- should_force_redownload(download_dir, last_updated, FALSE)  # No verbose during progress

    # Check if file already exists
    if (file.exists(file_path) && !force_redownload) {
      file_paths[i] <- file_path
      pdf_urls[i] <- url
      success[i] <- TRUE
      if (verbose) {
        setTxtProgressBar(pb, i)
      }
      next
    }

    tryCatch({
      # Download file
      response <- httr2::request(url) |>
        httr2::req_user_agent("ster_scraper/1.0.0") |>
        httr2::req_perform()

      # Write to file
      writeBin(httr2::resp_body_raw(response), file_path)

      file_paths[i] <- file_path
      pdf_urls[i] <- url
      success[i] <- TRUE

    }, error = function(e) {
      success[i] <- FALSE
    })

    if (verbose) {
      setTxtProgressBar(pb, i)
    }
  }

  if (verbose) {
    close(pb)
    cat("\n")
  }

  # Save metadata about this download session
  save_download_metadata(download_dir, last_updated, verbose)

  return(list(
    file_paths = file_paths[success],
    pdf_urls = pdf_urls[success],
    success = success,
    links_df = links_df
  ))
}

# Parse downloaded PDFs
parse_ster_pdfs <- function(file_paths, pdf_urls, extract_tables, extract_text, verbose) {
  if (length(file_paths) == 0) {
    return(tibble::tibble())
  }

  parsed_data <- list()

  # Create progress bar
  if (verbose) {
    cat("\n📄 Parsing PDFs...\n")
    pb <- txtProgressBar(min = 0, max = length(file_paths), style = 3, char = "█")
  }

  for (i in seq_along(file_paths)) {
    file_path <- file_paths[i]
    
    # Get the PDF URL directly from the parallel vector
    pdf_url <- if (i <= length(pdf_urls)) pdf_urls[i] else NA_character_

    tryCatch({
      # Extract text using pdftools
      text_content <- NULL
      if (extract_text) {
        text_content <- pdftools::pdf_text(file_path)
      }

      # Extract tables using tabulapdf (if available)
      table_content <- NULL
      if (extract_tables) {
        # Check if tabulapdf is available
        if (require(tabulapdf, quietly = TRUE)) {
          tryCatch({
            # Use tabulapdf for table extraction
            table_content <- tabulapdf::extract_tables(file_path)
          }, error = function(e) {
            # Silently fail for table extraction
          })
        }
      }

      # Get PDF info
      pdf_info <- pdftools::pdf_info(file_path)

      # Extract structured data from text
      structured_data <- extract_structured_ster_data(text_content, table_content)

      # Extract party info from filename for efficiency
      party <- extract_party_from_filename(basename(file_path))
      version <- extract_version_from_filename(basename(file_path))
      format <- extract_format_from_filename(basename(file_path))

      parsed_data[[i]] <- list(
        file_path = file_path,
        filename = basename(file_path),
        party = party,
        version = version,
        format = format,
        pdf_url = pdf_url,
        text_content = text_content,
        table_content = table_content,
        pdf_info = pdf_info,
        structured_data = structured_data,
        parse_success = TRUE
      )

    }, error = function(e) {
      parsed_data[[i]] <- list(
        file_path = file_path,
        filename = basename(file_path),
        pdf_url = pdf_url,
        parse_success = FALSE,
        error = e$message
      )
    })

    if (verbose) {
      setTxtProgressBar(pb, i)
    }
  }

  if (verbose) {
    close(pb)
    cat("\n")
  }

  return(parsed_data)
}

# Create unified dataset from parsed PDFs
create_unified_ster_data <- function(parsed_data, verbose) {
  if (is.null(parsed_data) || length(parsed_data) == 0) {
    return(tibble::tibble())
  }

  unified_rows <- list()

  for (i in seq_along(parsed_data)) {
    pdf_data <- parsed_data[[i]]

    if (!pdf_data$parse_success) {
      next
    }

    # Use pre-extracted information (more efficient)
    filename <- pdf_data$filename
    party <- pdf_data$party
    version <- pdf_data$version
    format <- pdf_data$format

    # Use already extracted structured data from parsing
    text_data <- pdf_data$structured_data

    # Create clean, flat row
    unified_rows[[i]] <- tibble::tibble(
      # File information
      filename = filename,
      party = party,
      version = version,
      format = format,
      file_path = pdf_data$file_path,
      pdf_url = if (!is.null(pdf_data$pdf_url)) pdf_data$pdf_url else NA_character_,

      # Basic stats
      word_count = if (!is.null(text_data$word_count)) text_data$word_count else NA_integer_,
      page_count = if (!is.null(text_data$page_count)) text_data$page_count else NA_integer_,

      # Financial information
      total_spending = if (!is.null(text_data$total_spending)) text_data$total_spending else NA_real_,

      # Channel-specific financial data
      tv_total_amount = if (!is.null(text_data$channel_financials$tv_total)) text_data$channel_financials$tv_total else NA_real_,
      tv_ster_amount = if (!is.null(text_data$channel_financials$tv_ster)) text_data$channel_financials$tv_ster else NA_real_,
      radio_total_amount = if (!is.null(text_data$channel_financials$radio_total)) text_data$channel_financials$radio_total else NA_real_,
      radio_ster_amount = if (!is.null(text_data$channel_financials$radio_ster)) text_data$channel_financials$radio_ster else NA_real_,
      online_display_total_amount = if (!is.null(text_data$channel_financials$online_display_total)) text_data$channel_financials$online_display_total else NA_real_,
      online_display_ster_amount = if (!is.null(text_data$channel_financials$online_display_ster)) text_data$channel_financials$online_display_ster else NA_real_,
      online_video_total_amount = if (!is.null(text_data$channel_financials$online_video_total)) text_data$channel_financials$online_video_total else NA_real_,
      online_video_ster_amount = if (!is.null(text_data$channel_financials$online_video_ster)) text_data$channel_financials$online_video_ster else NA_real_,

      # Organization information
      organization_name = if (!is.null(text_data$organization_name) && !is.na(text_data$organization_name)) {
        text_data$organization_name
      } else {
        party  # Use party name as fallback
      },

      # Media information
      media_types = if (!is.null(text_data$media_breakdown) && length(text_data$media_breakdown) > 0) {
        paste(names(text_data$media_breakdown), collapse = ", ")
      } else {
        format  # Use format as fallback
      },

      # Campaign information
      campaign_start = if (!is.null(text_data$campaign_period) && length(text_data$campaign_period) > 0) {
        text_data$campaign_period[1]
      } else {
        NA_character_
      },
      campaign_end = if (!is.null(text_data$campaign_period) && length(text_data$campaign_period) > 1) {
        text_data$campaign_period[2]
      } else {
        NA_character_
      },

      # Contact information
      contact_emails = if (!is.null(text_data$contact_info$emails) && length(text_data$contact_info$emails) > 0) {
        paste(text_data$contact_info$emails, collapse = "; ")
      } else {
        NA_character_
      },

      # Financing information
      funding_source = if (!is.null(text_data$funding_source)) text_data$funding_source else NA_character_,
      funding_geography = if (!is.null(text_data$funding_geography)) text_data$funding_geography else NA_character_,
      online_targeting = if (!is.null(text_data$online_targeting)) text_data$online_targeting else NA_character_,

      # Technical information
      tables_found = if (!is.null(text_data$tables_found)) text_data$tables_found else 0L,

      # Raw text (truncated for readability)
      raw_text_preview = if (!is.null(text_data$raw_text)) {
        text <- text_data$raw_text
        if (nchar(text) > 200) {
          paste0(substr(text, 1, 200), "...")
        } else {
          text
        }
      } else {
        NA_character_
      },

      # Parse metadata
      parse_date = Sys.time()
    )
  }

  # Combine all rows
  unified_data <- suppressWarnings(dplyr::bind_rows(unified_rows))

  if (verbose) {
    cat(sprintf("✅ Created unified dataset with %d entries\n", nrow(unified_data)))
    cat("Columns: ", paste(names(unified_data), collapse = ", "), "\n")
  }

  return(unified_data)
}

# Smart re-downloading functions
should_force_redownload <- function(download_dir, last_updated, verbose) {
  if (is.null(last_updated) || is.na(last_updated)) {
    return(FALSE)
  }

  metadata_file <- file.path(download_dir, ".ster_metadata.json")

  if (!file.exists(metadata_file)) {
    if (verbose) {
      cat("📥 No previous metadata found, will download all files\n")
    }
    return(TRUE)
  }

  tryCatch({
    # Read previous metadata
    metadata <- jsonlite::fromJSON(metadata_file)
    previous_update <- metadata$last_updated

    if (is.null(previous_update) || is.na(previous_update)) {
      if (verbose) {
        cat("📥 No previous update date found, will download all files\n")
      }
      return(TRUE)
    }

    # Compare dates
    if (last_updated != previous_update) {
      if (verbose) {
        cat(sprintf("🔄 Website updated: %s -> %s, will re-download\n", previous_update, last_updated))
      }
      return(TRUE)
    } else {
      if (verbose) {
        cat(sprintf("⏭️  Website not updated since %s, skipping downloads\n", last_updated))
      }
      return(FALSE)
    }

  }, error = function(e) {
    if (verbose) {
      cat("⚠️  Error reading metadata, will download all files: ", e$message, "\n")
    }
    return(TRUE)
  })
}

save_download_metadata <- function(download_dir, last_updated, verbose) {
  if (is.null(last_updated) || is.na(last_updated)) {
    return()
  }

  metadata_file <- file.path(download_dir, ".ster_metadata.json")

  metadata <- list(
    last_updated = last_updated,
    download_date = Sys.time(),
    version = "1.0"
  )

  tryCatch({
    jsonlite::write_json(metadata, metadata_file, pretty = TRUE)
    if (verbose) {
      cat(sprintf("💾 Saved metadata: website updated %s\n", last_updated))
    }
  }, error = function(e) {
    if (verbose) {
      cat("⚠️  Error saving metadata: ", e$message, "\n")
    }
  })
}

# Extract last updated date from website
extract_last_updated_date <- function(html_content, verbose) {
  tryCatch({
    # Look for "Laatst bijgewerkt op" pattern
    last_updated_text <- rvest::html_text(rvest::html_nodes(html_content, "p, div, span"))

    # Find the line containing "Laatst bijgewerkt op"
    for (text in last_updated_text) {
      if (stringr::str_detect(text, "Laatst bijgewerkt op")) {
        # Extract the date part
        date_match <- stringr::str_extract(text, "Laatst bijgewerkt op\\s+(\\d+\\s+\\w+\\s+\\d+)")
        if (!is.na(date_match)) {
          date_str <- stringr::str_remove(date_match, "Laatst bijgewerkt op\\s+")
          # Convert Dutch date to standard format
          parsed_date <- parse_dutch_date(date_str)
  if (verbose) {
    cat(sprintf("📅 Website last updated: %s\n", parsed_date))
  }
          return(parsed_date)
        }
      }
    }

    if (verbose) {
      cat("⚠️  Could not find last updated date on website\n")
    }
    return(NA_character_)

  }, error = function(e) {
    if (verbose) {
      cat("⚠️  Error extracting last updated date: ", e$message, "\n")
    }
    return(NA_character_)
  })
}

# Parse Dutch date format
parse_dutch_date <- function(date_str) {
  # Dutch month names
  dutch_months <- c("januari", "februari", "maart", "april", "mei", "juni",
                    "juli", "augustus", "september", "oktober", "november", "december")

  # Extract day, month, year
  parts <- stringr::str_split(date_str, "\\s+")[[1]]
  if (length(parts) >= 3) {
    day <- parts[1]
    month_name <- tolower(parts[2])
    year <- parts[3]

    # Convert Dutch month to number
    month_num <- which(dutch_months == month_name)
    if (length(month_num) > 0) {
      # Create date string in YYYY-MM-DD format
      date_formatted <- sprintf("%s-%02d-%02d", year, month_num, as.numeric(day))
      return(date_formatted)
    }
  }

  return(NA_character_)
}

# Helper functions for data extraction
extract_party_from_filename <- function(filename) {
  name_lower <- tolower(stringr::str_trim(filename))
  if (stringr::str_detect(name_lower, "partijvoordedieren|pvdd|partij voor de dieren")) return("PvdD")
  if (stringr::str_detect(name_lower, "vvd|volkspartij")) return("VVD")
  if (stringr::str_detect(name_lower, "pvda|partij van de arbeid|groenlinks.*pvda|groenlinks")) return("PvdA/GroenLinks")
  if (stringr::str_detect(name_lower, "d66|democraten")) return("D66")
  if (stringr::str_detect(name_lower, "cda|christen.*democratisch")) return("CDA")
  if (stringr::str_detect(name_lower, "sp_|socialistische partij")) return("SP")
  if (stringr::str_detect(name_lower, "fvd|forum voor democratie|forumvoordemocratie")) return("FvD")
  if (stringr::str_detect(name_lower, "nsc")) return("NSC")
  if (stringr::str_detect(name_lower, "christenunie")) return("ChristenUnie")
  if (stringr::str_detect(name_lower, "sgp|staatkundig gereformeerde")) return("SGP")
  if (stringr::str_detect(name_lower, "denk")) return("DENK")
  if (stringr::str_detect(name_lower, "ja21")) return("JA21")
  if (stringr::str_detect(name_lower, "volt")) return("Volt")
  if (stringr::str_detect(name_lower, "bbb|boerburgerbeweging")) return("BBB")
  if (stringr::str_detect(name_lower, "bij1")) return("BIJ1")
  if (stringr::str_detect(name_lower, "50plus|50\\+")) return("50PLUS")
  return("Unknown")
}

extract_version_from_filename <- function(filename) {
  version_match <- stringr::str_extract(filename, "v\\d+")
  if (!is.na(version_match)) {
    return(stringr::str_remove(version_match, "v"))
  }
  return("unknown")
}

extract_format_from_filename <- function(filename) {
  if (stringr::str_detect(filename, "_TV_")) return("TV")
  if (stringr::str_detect(filename, "_Radio_")) return("Radio")
  if (stringr::str_detect(filename, "_Online_")) return("Online")
  return("General")
}

# Extract channel-specific financial data from Ster PDFs
extract_channel_financials <- function(text) {
  # Initialize result structure
  channel_financials <- list(
    tv_total = NA_real_,
    tv_ster = NA_real_,
    radio_total = NA_real_,
    radio_ster = NA_real_,
    online_display_total = NA_real_,
    online_display_ster = NA_real_,
    online_video_total = NA_real_,
    online_video_ster = NA_real_
  )

  # Look for the "WAARDE" section which contains the financial breakdown
  # This section typically has the format shown in the image
  waarde_section <- stringr::str_extract(text, "7\\.\\s*WAARDE[\\s\\S]*?(?=8\\.\\s*FINANCIERING|$)")

  if (!is.na(waarde_section)) {
    # Extract data for each channel using more specific patterns
    channel_financials$tv_total <- extract_channel_amount_specific(waarde_section, "Televisie", "Totaalbedrag")
    channel_financials$tv_ster <- extract_channel_amount_specific(waarde_section, "Televisie", "Waarvan bij Ster")

    channel_financials$radio_total <- extract_channel_amount_specific(waarde_section, "Radio", "Totaalbedrag")
    channel_financials$radio_ster <- extract_channel_amount_specific(waarde_section, "Radio", "Waarvan bij Ster")

    channel_financials$online_display_total <- extract_channel_amount_specific(waarde_section, "Online Display", "Totaalbedrag")
    channel_financials$online_display_ster <- extract_channel_amount_specific(waarde_section, "Online Display", "Waarvan bij Ster")

    channel_financials$online_video_total <- extract_channel_amount_specific(waarde_section, "Online Video", "Totaalbedrag")
    channel_financials$online_video_ster <- extract_channel_amount_specific(waarde_section, "Online Video", "Waarvan bij Ster")
  }

  # If WAARDE section not found, try alternative patterns
  if (all(is.na(unlist(channel_financials)))) {
    # Look for individual channel mentions with amounts using generic method
    channel_financials$tv_total <- extract_channel_amount(text, "televisie", "totaal", method = "generic")
    channel_financials$tv_ster <- extract_channel_amount(text, "televisie", "ster", method = "generic")

    channel_financials$radio_total <- extract_channel_amount(text, "radio", "totaal", method = "generic")
    channel_financials$radio_ster <- extract_channel_amount(text, "radio", "ster", method = "generic")

    channel_financials$online_display_total <- extract_channel_amount(text, "online display", "totaal", method = "generic")
    channel_financials$online_display_ster <- extract_channel_amount(text, "online display", "ster", method = "generic")

    channel_financials$online_video_total <- extract_channel_amount(text, "online video", "totaal", method = "generic")
    channel_financials$online_video_ster <- extract_channel_amount(text, "online video", "ster", method = "generic")
  }

  return(channel_financials)
}

# Consolidated channel amount extractor with mode switch
extract_channel_amount <- function(text, channel, amount_type, method = "waarde") {
  if (method == "waarde") {
    return(extract_channel_amount_specific(text, channel, amount_type))
  } else {
    return(extract_amount_by_channel(text, channel, amount_type))
  }
}

# More specific extraction function for WAARDE section table structure
extract_channel_amount_specific <- function(text, channel, amount_type) {
  # First check if this channel is marked as "n.v.t." (not applicable)
  # Extract just this channel's section to avoid cross-channel contamination
  channel_section <- stringr::str_extract(text, paste0("\\b", channel, "\\b[\\s\\S]*?(?=\\n\\n|\\b(?:Televisie|Radio|Online Display|Online Video)\\b|$)"))

  if (!is.na(channel_section)) {
    nvt_pattern <- paste0(
      amount_type, ".*?(n\\.v\\.t\\.|niet van toepassing)"
    )

    if (stringr::str_detect(channel_section, stringr::regex(nvt_pattern, ignore_case = TRUE))) {
      return(NA_real_)  # Channel not used
    }
  }

  # Look for the specific pattern in the WAARDE section
  # The pattern should match: Channel name, checkbox, amount type, amount
  # More specific patterns to avoid cross-contamination between total and ster amounts

  if (amount_type == "Totaalbedrag") {
    # Look for "Totaalbedrag in €" pattern
    pattern <- paste0(
      "\\b", channel, "\\b[\\s\\S]*?",
      "Totaalbedrag\\s+in\\s+€\\s*([0-9.,]+)"
    )

    match <- stringr::str_extract(text, stringr::regex(pattern, ignore_case = TRUE))

    if (!is.na(match)) {
      amount_str <- stringr::str_extract(match, "€\\s*([0-9.,]+)")
      if (!is.na(amount_str)) {
        return(clean_euro_nl(amount_str))
      }
    }
  } else if (amount_type == "Waarvan bij Ster") {
    # Look for "Waarvan bij Ster in €" pattern - this appears on a separate line
    # First try the direct pattern
    pattern <- paste0(
      "\\b", channel, "\\b[\\s\\S]*?",
      "Waarvan\\s+bij\\s+Ster\\s+in\\s+€\\s*([0-9.,]+)"
    )

    match <- stringr::str_extract(text, stringr::regex(pattern, ignore_case = TRUE))

    if (!is.na(match)) {
      # For Waarvan bij Ster, extract the amount that comes after "Waarvan bij Ster"
      amount_str <- stringr::str_extract(match, "Waarvan\\s+bij\\s+Ster\\s+in\\s+€\\s*([0-9.,]+)")
      if (!is.na(amount_str)) {
        # Extract just the amount part
        amount_str <- stringr::str_extract(amount_str, "€\\s*([0-9.,]+)")
      }

      if (!is.na(amount_str)) {
        return(clean_euro_nl(amount_str))
      }
    }

    # If the above pattern doesn't work, try a simpler approach
    # Look for "Waarvan bij Ster" followed by the amount, but only after the channel name
    channel_section <- stringr::str_extract(text, paste0("\\b", channel, "\\b[\\s\\S]*?(?=\\n\\n|$)"))
    if (!is.na(channel_section)) {
      ster_pattern <- "Waarvan\\s+bij\\s+Ster\\s+in\\s+€\\s*([0-9.,]+)"
      ster_match <- stringr::str_extract(channel_section, stringr::regex(ster_pattern, ignore_case = TRUE))
      if (!is.na(ster_match)) {
        amount_str <- stringr::str_extract(ster_match, "€\\s*([0-9.,]+)")
        if (!is.na(amount_str)) {
          return(clean_euro_nl(amount_str))
        }
      }
    }
  } else {
    # Fallback to general pattern
    pattern <- paste0(
      "\\b", channel, "\\b[\\s\\S]*?",
      amount_type, ".*?€\\s*([0-9,]+(?:\\.[0-9]+)?)"
    )
  }

  match <- stringr::str_extract(text, stringr::regex(pattern, ignore_case = TRUE))

  if (!is.na(match)) {
    # Extract the amount from the match
    amount_str <- stringr::str_extract(match, "€\\s*([0-9,]+(?:\\.[0-9]+)?)")
    if (!is.na(amount_str)) {
      return(clean_euro_nl(amount_str))
    }
  }

  return(NA_real_)
}

# Alternative extraction method for when structured section is not found
extract_amount_by_channel <- function(text, channel, amount_type) {
  # Look for patterns like "Televisie Totaalbedrag in € 694.832"
  patterns <- c(
    paste0("\\b", channel, "\\b.*?", amount_type, ".*?€\\s*([0-9,]+(?:\\.[0-9]+)?)"),
    paste0("\\b", channel, "\\b.*?€\\s*([0-9,]+(?:\\.[0-9]+)?).*?", amount_type),
    paste0(amount_type, ".*?\\b", channel, "\\b.*?€\\s*([0-9,]+(?:\\.[0-9]+)?)")
  )

  for (pattern in patterns) {
    match <- stringr::str_extract(text, stringr::regex(pattern, ignore_case = TRUE))
    if (!is.na(match)) {
      amount_str <- stringr::str_extract(match, "€\\s*([0-9,]+(?:\\.[0-9]+)?)")
      if (!is.na(amount_str)) {
        return(clean_euro_nl(amount_str))
      }
    }
  }

  return(NA_real_)
}

# Extract structured data from Ster PDF content
extract_structured_ster_data <- function(text_content, table_content) {
  if (is.null(text_content)) return(list())

  # Combine all text
  full_text <- paste(text_content, collapse = " ")

    # Extract key information using pattern matching
    structured_data <- list(
      # Basic text info
      raw_text = full_text,
      word_count = length(unlist(strsplit(full_text, "\\s+"))),
      page_count = length(text_content),

      # Extract spending information
      total_spending = extract_spending_amount(full_text),
      media_breakdown = extract_media_breakdown(full_text),
      channel_financials = extract_channel_financials(full_text),

      # Extract campaign details
      campaign_period = extract_campaign_period(full_text),
      target_audience = extract_target_audience(full_text),

      # Extract organization details
      organization_name = extract_organization_name(full_text),
      contact_info = extract_contact_info(full_text),

      # Extract financing information
      funding_source = extract_funding_source(full_text),
      funding_geography = extract_funding_geography(full_text),
      online_targeting = extract_online_targeting(full_text),

      # Table data
      tables_found = if (!is.null(table_content)) length(table_content) else 0,
      table_summary = if (!is.null(table_content)) summarize_tables(table_content) else list()
    )

  return(structured_data)
}

# Centralized amount cleaning function for Dutch formatting
clean_euro_nl <- function(amount_str) {
  if (is.na(amount_str) || amount_str == "") {
    return(NA_real_)
  }
  
  # Clean the amount string - remove euro symbol
  amount_clean <- stringr::str_remove_all(amount_str, "[€]")
  
  # Handle Dutch decimal formatting: commas are decimals, dots are thousands
  # First remove thousands separators (dots)
  amount_clean <- stringr::str_replace_all(amount_clean, "\\.", "")
  # Then convert decimal comma to decimal point
  amount_clean <- stringr::str_replace(amount_clean, ",", ".")
  
  return(suppressWarnings(as.numeric(amount_clean)))
}

# Helper functions for data extraction
extract_spending_amount <- function(text) {
  # Look for actual total spending amounts, not individual channel amounts
  # Priority patterns for total spending
  total_patterns <- c(
    "totale.*?uitgaven.*?€[[:space:]]*([0-9.,]+)",
    "totaal.*?bedrag.*?€[[:space:]]*([0-9.,]+)",
    "som.*?van.*?alle.*?€[[:space:]]*([0-9.,]+)",
    "grand.*?total.*?€[[:space:]]*([0-9.,]+)"
  )

  amounts <- c()
  for (pattern in total_patterns) {
    matches <- stringr::str_extract_all(text, stringr::regex(pattern, ignore_case = TRUE))
    amounts <- c(amounts, unlist(matches))
  }

  # If we found explicit total amounts, use the highest one
  if (length(amounts) > 0) {
    amounts_numeric <- sapply(amounts, clean_euro_nl)
    valid_amounts <- amounts_numeric[!is.na(amounts_numeric)]
    if (length(valid_amounts) > 0) {
      return(max(valid_amounts))
    }
  }

  # If no explicit total found, calculate from channel amounts
  # This is the correct approach when no overall total is specified
  channel_financials <- extract_channel_financials(text)
  channel_totals <- c(
    channel_financials$tv_total,
    channel_financials$radio_total,
    channel_financials$online_display_total,
    channel_financials$online_video_total
  )
  
  # Sum all non-NA channel totals
  valid_totals <- channel_totals[!is.na(channel_totals)]
  if (length(valid_totals) > 0) {
    return(sum(valid_totals))
  }

  return(NA_real_)
}

extract_media_breakdown <- function(text) {
  # Look for media type mentions (Dutch terms)
  media_types <- c("televisie", "radio", "online", "print", "social media", "digitale media",
                   "directe inkoop", "intermediair", "alleen televisie", "alleen radio", "alleen online")
  breakdown <- list()

  for (media in media_types) {
    if (stringr::str_detect(text, stringr::regex(media, ignore_case = TRUE))) {
      # Try to extract amount for this media type
      pattern <- paste0(media, ".*?€\\s*([0-9,]+(?:\\.[0-9]+)?)")
      amount <- stringr::str_extract(text, stringr::regex(pattern, ignore_case = TRUE))
      if (!is.na(amount)) {
        breakdown[[media]] <- amount
      }
    }
  }

  return(breakdown)
}

extract_campaign_period <- function(text) {
  # Look for date patterns
  date_patterns <- c(
    "\\d{1,2}[\\-/]\\d{1,2}[\\-/]\\d{4}",
    "\\d{1,2}\\s+(januari|februari|maart|april|mei|juni|juli|augustus|september|oktober|november|december)\\s+\\d{4}",
    "van\\s+\\d{1,2}[\\-/]\\d{1,2}[\\-/]\\d{4}\\s+tot\\s+\\d{1,2}[\\-/]\\d{1,2}[\\-/]\\d{4}"
  )

  dates <- c()
  for (pattern in date_patterns) {
    matches <- stringr::str_extract_all(text, stringr::regex(pattern, ignore_case = TRUE))
    dates <- c(dates, unlist(matches))
  }

  return(unique(dates))
}

extract_target_audience <- function(text) {
  # Look for audience descriptions (Dutch terms)
  audience_keywords <- c(
    "doelgroep", "target", "publiek", "kijkers", "luisteraars",
    "leeftijd", "demografisch", "geografisch", "bereik", "campagne",
    "reclameboodschap", "stemgedrag", "verkiezing", "kiesgedrag"
  )

  audience_info <- c()
  for (keyword in audience_keywords) {
    if (stringr::str_detect(text, stringr::regex(keyword, ignore_case = TRUE))) {
      # Extract sentence containing the keyword
      sentences <- stringr::str_split(text, "[.!?]")[[1]]
      relevant_sentences <- sentences[stringr::str_detect(sentences, stringr::regex(keyword, ignore_case = TRUE))]
      audience_info <- c(audience_info, relevant_sentences)
    }
  }

  return(unique(audience_info))
}

extract_organization_name <- function(text) {
  # Look for organization name patterns (Dutch terms)
  patterns <- c(
    "organisatie:\\s*([^\\n]+)",
    "adverteerder:\\s*([^\\n]+)",
    "opdrachtgever:\\s*([^\\n]+)",
    "partij:\\s*([^\\n]+)",
    "politieke partij:\\s*([^\\n]+)",
    "transparantieverklaring.*?([A-Za-z\\s]+)",
    "politieke uiting.*?([A-Za-z\\s]+)"
  )

  for (pattern in patterns) {
    match <- stringr::str_extract(text, stringr::regex(pattern, ignore_case = TRUE))
    if (!is.na(match)) {
      # Clean up the match
      cleaned <- stringr::str_remove(match, "organisatie:|adverteerder:|opdrachtgever:|partij:|politieke partij:")
      cleaned <- stringr::str_trim(cleaned)
      if (nchar(cleaned) > 2) {
        return(cleaned)
      }
    }
  }

  return(NA_character_)
}

extract_contact_info <- function(text) {
  # Look for contact information
  email_pattern <- "[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}"
  phone_pattern <- "\\+?[0-9\\s\\-\\(\\)]{10,}"

  emails <- stringr::str_extract_all(text, email_pattern)
  phones <- stringr::str_extract_all(text, phone_pattern)

  return(list(
    emails = unlist(emails),
    phones = unlist(phones)
  ))
}

summarize_tables <- function(table_content) {
  if (is.null(table_content) || length(table_content) == 0) {
    return(list())
  }

  summary <- list()
  for (i in seq_along(table_content)) {
    table <- table_content[[i]]
    if (!is.null(table) && nrow(table) > 0) {
      summary[[paste0("table_", i)]] <- list(
        rows = nrow(table),
        cols = ncol(table),
        headers = if (nrow(table) > 0) as.character(table[1, ]) else character(0)
      )
    }
  }

  return(summary)
}

# Extract financing information
extract_funding_source <- function(text) {
  # Look for funding source patterns
  if (stringr::str_detect(text, "☒.*Publieke herkomst")) {
    return("Publieke herkomst")
  } else if (stringr::str_detect(text, "☒.*Private herkomst")) {
    return("Private herkomst")
  } else if (stringr::str_detect(text, "☒.*Gemengde herkomst")) {
    return("Gemengde herkomst")
  } else {
    return(NA_character_)
  }
}

extract_funding_geography <- function(text) {
  # Look for geographic origin patterns
  if (stringr::str_detect(text, "☒.*Europese Unie.*☐.*Buiten Europese Unie.*☐.*Gemengde herkomst")) {
    return("Europese Unie")
  } else if (stringr::str_detect(text, "☐.*Europese Unie.*☒.*Buiten Europese Unie.*☐.*Gemengde herkomst")) {
    return("Buiten Europese Unie")
  } else if (stringr::str_detect(text, "☐.*Europese Unie.*☐.*Buiten Europese Unie.*☒.*Gemengde herkomst")) {
    return("Gemengde herkomst")
  } else {
    return(NA_character_)
  }
}

extract_online_targeting <- function(text) {
  # Look for online targeting techniques section
  targeting_pattern <- "ONLINE TARGETINGTECHNIEKEN[\\s\\S]*?(?=10\\.|$)"
  targeting_match <- stringr::str_extract(text, stringr::regex(targeting_pattern, ignore_case = TRUE))

  if (!is.na(targeting_match)) {
    # Clean up the match
    targeting_text <- stringr::str_trim(targeting_match)
    targeting_text <- stringr::str_remove(targeting_text, "ONLINE TARGETINGTECHNIEKEN")
    targeting_text <- stringr::str_trim(targeting_text)

    if (targeting_text == "N.v.t." || targeting_text == "n.v.t.") {
      return("N.v.t.")
    } else if (nchar(targeting_text) > 0) {
      return(targeting_text)
    }
  }

  return(NA_character_)
}

extract_table_data <- function(table_content) {
  if (is.null(table_content)) return(list())

  # Process tables
  processed_tables <- list()
  for (i in seq_along(table_content)) {
    if (!is.null(table_content[[i]]) && nrow(table_content[[i]]) > 0) {
      processed_tables[[i]] <- as.data.frame(table_content[[i]], stringsAsFactors = FALSE)
    }
  }

  return(processed_tables)
}
