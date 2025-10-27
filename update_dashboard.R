#!/usr/bin/env Rscript

# Dashboard Update and Git Push Script
# This script renders the dashboard and pushes changes to git

cat("\n========================================\n")
cat("Dashboard Update Script\n")
cat(paste("Started at:", Sys.time()), "\n")
cat("========================================\n\n")

# Set working directory
setwd("/Users/favstats/Dropbox/reclamer")
cat("Working directory:", getwd(), "\n\n")

# Load required packages
cat("Loading packages...\n")
suppressPackageStartupMessages({
  library(gert)
})

# Render the dashboard
cat("\n--- Rendering Dashboard ---\n")
quarto_path <- "/Applications/RStudio.app/Contents/Resources/app/quarto/bin/quarto"
tryCatch({
  result <- system2(quarto_path, args = c("render", "_site/index.qmd"), 
                    stdout = TRUE, stderr = TRUE)
  if (!is.null(attr(result, "status")) && attr(result, "status") != 0) {
    cat("Quarto output:\n", paste(result, collapse = "\n"), "\n")
    stop("Quarto rendering failed")
  }
  cat("✅ Dashboard rendered successfully!\n")
}, error = function(e) {
  cat("❌ Error rendering dashboard:", e$message, "\n")
  quit(status = 1)
})

# Git operations
cat("\n--- Git Operations ---\n")

# Check git status
status <- git_status()
cat("Files changed:", nrow(status), "\n")

if (nrow(status) > 0) {
  # Add all changes
  cat("Adding files to git...\n")
  git_add(".")
  cat("✅ Files added\n")
  
  # Commit changes
  cat("Committing changes...\n")
  commit_msg <- paste("Auto-update dashboard:", Sys.Date())
  git_commit(commit_msg)
  cat("✅ Committed:", commit_msg, "\n")
  
  # Push to remote
  cat("Pushing to remote...\n")
  tryCatch({
    git_push()
    cat("✅ Pushed successfully!\n")
  }, error = function(e) {
    cat("❌ Error pushing to remote:", e$message, "\n")
    cat("Note: Make sure credentials are configured\n")
    quit(status = 1)
  })
} else {
  cat("ℹ️  No changes to commit\n")
}

cat("\n========================================\n")
cat("Script completed successfully!\n")
cat(paste("Finished at:", Sys.time()), "\n")
cat("========================================\n")

