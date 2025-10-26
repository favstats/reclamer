
<!-- README.md is generated from README.Rmd. Please edit that file -->

# reclamer

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/favstats/reclamer/workflows/R-CMD-check/badge.svg)](https://github.com/favstats/reclamer/actions)
<!-- badges: end -->

**Comprehensive Dutch Political Advertising Transparency Data
Collection**

The `reclamer` package provides unified access to Dutch political
advertising transparency data from multiple sources, including
PolitiekeReclame.nl, DPG Media, and Ster public broadcaster. It
automatically handles data collection, version detection, PDF parsing,
and provides clean, structured outputs for analysis.

## 🎯 Key Features

- **Unified API Access**: Single function for multiple transparency
  sources
- **Intelligent Version Detection**: Automatically prefers latest
  versions
- **Comprehensive PDF Processing**: Downloads and parses transparency
  statements
- **Multi-Format Support**: Handles TV, Radio, Online, and Print
  advertising
- **Auto-Pagination**: Collects complete datasets automatically
- **Clean Data Output**: Structured tibbles ready for analysis

## 📦 Installation

You can install the development version of reclamer from GitHub:

``` r
# install.packages("devtools")
devtools::install_github("favstats/reclamer")
```

## 🚀 Quick Start

### Basic Usage

``` r
library(reclamer)

# PolitiekeReclame.nl API (default)
ads <- fetch_ads(query = "election", limit = 10)
print(ads)

# DPG Media API
dpg_ads <- fetch_ads(source = "dpgmedia", period = "PAST_7_DAYS")
print(dpg_ads)

# Ster transparency scraper
ster_data <- scrape_ster_transparency()
print(ster_data$unified_data)
```

### Advanced Data Collection

``` r
# Auto-pagination for complete datasets
all_ads <- fetch_ads(query = "campaign", all = TRUE, verbose = TRUE)

# Multiple time periods from DPG Media
recent_ads <- fetch_ads(source = "dpgmedia", period = "PAST_7_DAYS")
monthly_ads <- fetch_ads(source = "dpgmedia", period = "PAST_30_DAYS")
yearly_ads <- fetch_ads(source = "dpgmedia", period = "PAST_YEAR")

# Channel-specific data
print_ads <- fetch_ads(source = "dpgmedia", channel = "PRINT")
digital_ads <- fetch_ads(source = "dpgmedia", channel = "DIGITAL")
```

## 📊 Data Sources

### 1. PolitiekeReclame.nl API

Official transparency statements from the Dutch political advertising
registry.

``` r
# Search for specific terms
election_ads <- fetch_ads(query = "verkiezingen")

# Filter by theme
tk2025_ads <- fetch_ads(theme = "Tweede Kamerverkiezingen 2025")

# Get all results with auto-pagination
all_statements <- fetch_ads(all = TRUE, max_pages = 10, verbose = TRUE)
```

### 2. DPG Media API

Political advertising data from major Dutch media company (AD, Het
Parool, etc.).

``` r
# Recent campaigns
recent_campaigns <- fetch_ads(
  source = "dpgmedia", 
  period = "PAST_7_DAYS",
  verbose = TRUE
)

# Print advertising only
print_campaigns <- fetch_ads(
  source = "dpgmedia",
  channel = "PRINT", 
  period = "PAST_30_DAYS"
)

# Digital campaigns
digital_campaigns <- fetch_ads(
  source = "dpgmedia",
  channel = "DIGITAL",
  period = "PAST_YEAR"
)
```

### 3. Ster Transparency Scraper

Comprehensive scraping of public broadcaster transparency statements.

``` r
# Scrape and download all transparency PDFs
ster_data <- scrape_ster_transparency(
  download_dir = "ster_pdfs",
  parse_pdfs = TRUE,
  verbose = TRUE
)

# Access downloaded files
downloaded_files <- ster_data$download_info$file_paths
print(paste("Downloaded", length(downloaded_files), "PDFs"))

# View unified dataset
unified_data <- ster_data$unified_data
print(unified_data)
```

## 🔍 Data Analysis Examples

### Combine Data from Multiple Sources

``` r
# Collect data from all sources
pr_data <- fetch_ads(limit = 50)
dpg_data <- fetch_ads(source = "dpgmedia", period = "PAST_30_DAYS")
ster_data <- scrape_ster_transparency(parse_pdfs = FALSE)

# Analyze party coverage
pr_parties <- unique(pr_data$name)
dpg_parties <- unique(dpg_data$advertiser_name)
ster_parties <- unique(ster_data$download_info$links_df$party)

print(paste("PolitiekeReclame parties:", length(pr_parties)))
print(paste("DPG Media advertisers:", length(dpg_parties)))
print(paste("Ster transparency statements:", length(ster_parties)))
```

### Time Series Analysis

``` r
# Collect data over different time periods
periods <- c("PAST_7_DAYS", "PAST_30_DAYS", "PAST_YEAR")
campaign_counts <- sapply(periods, function(period) {
  data <- fetch_ads(source = "dpgmedia", period = period)
  nrow(data)
})

print("Campaign counts by period:")
print(campaign_counts)
```

## 📋 Function Reference

### `fetch_ads()`

Unified function for accessing political advertising data.

**Parameters:** - `source`: “politiekereclame” or “dpgmedia” - `query`:
Search term (PolitiekeReclame only) - `theme`: Theme filter
(PolitiekeReclame only) - `country_code`: Country code (DPG Media
only) - `period`: Time period (DPG Media only) - `channel`: Channel type
(DPG Media only) - `all`: Auto-pagination (PolitiekeReclame only) -
`return`: “data”, “list”, or “response”

### `scrape_ster_transparency()`

Comprehensive Ster website scraper.

**Parameters:** - `base_url`: Ster transparency page URL -
`download_dir`: Directory for PDF downloads - `parse_pdfs`: Whether to
parse PDF content - `extract_tables`: Extract tables from PDFs -
`extract_text`: Extract text from PDFs - `verbose`: Progress information

## 🛠️ Advanced Features

### Version Detection

Automatically detects and prefers version 2+ over version 1 for Ster
transparency statements.

### Multi-Format Support

Collects all available formats per party: - TV advertising - Radio
advertising  
- Online advertising - Print advertising

### Intelligent Filtering

- Removes duplicate entries
- Prioritizes latest versions
- Handles multiple formats per party
- Preserves data integrity

### Error Handling

- Automatic retries with exponential backoff
- Graceful failure handling
- Comprehensive error messages
- Progress reporting

## 📈 Use Cases

### Research Applications

- Political advertising spending analysis
- Transparency compliance monitoring
- Cross-platform advertising comparison
- Temporal trend analysis

### Journalistic Investigations

- Campaign finance transparency
- Media coverage analysis
- Political messaging tracking
- Regulatory compliance verification

### Academic Studies

- Political communication research
- Media effects studies
- Campaign strategy analysis
- Democratic transparency assessment

## 🔧 Dependencies

The package requires the following R packages: - `httr2` - HTTP
requests - `rvest` - Web scraping - `tibble` - Data structures -
`dplyr` - Data manipulation - `stringr` - String processing -
`pdftools` - PDF processing (optional) - `tabulizer` - Table extraction
(optional)

## 📝 Contributing

Contributions are welcome! Please feel free to submit issues, feature
requests, or pull requests.

## 📄 License

This project is licensed under the MIT License - see the LICENSE file
for details.

## 🙏 Acknowledgments

- **PolitiekeReclame.nl** for providing the transparency API
- **DPG Media** for their advertising transparency data
- **Ster** for public broadcaster transparency statements
- The R community for excellent packages that make this possible

------------------------------------------------------------------------

**Built with ❤️ for Dutch political transparency**
