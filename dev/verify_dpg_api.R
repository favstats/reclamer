#!/usr/bin/env Rscript

# Quick verification script for DPG Media API
# Run this anytime to verify the API is working correctly

cat("Verifying DPG Media API...\n\n")

# Load required libraries
suppressPackageStartupMessages({
  library(httr2)
  library(tibble)
  library(dplyr)
})

# Source the functions
source("R/reclame.R")

# Test the API (default period = NULL gets all data)
tryCatch({
  result <- fetch_ads(
    source = "dpgmedia",
    verbose = FALSE
  )
  
  cat("✅ API working correctly!\n")
  cat(sprintf("   Retrieved %d campaign items\n", nrow(result)))
  cat(sprintf("   From %d unique sponsors\n", length(unique(result$sponsorName))))
  cat(sprintf("   Channels: %s\n", paste(unique(result$channel), collapse=", ")))
  cat(sprintf("   Date range: %s to %s\n", 
              min(result$publicationStartDate), 
              max(result$publicationStartDate)))
}, error = function(e) {
  cat("❌ Error:", e$message, "\n")
})

