#!/bin/bash

# Setup script for dashboard auto-update cron job

echo "========================================="
echo "Dashboard Auto-Update Cron Job Setup"
echo "========================================="
echo ""

# Define the cron job
RSCRIPT_PATH=$(which Rscript)
SCRIPT_PATH="/Users/favstats/Dropbox/reclamer/update_dashboard.R"
LOG_PATH="/Users/favstats/Dropbox/reclamer/update_dashboard.log"

# Cron job runs daily at 8 PM
CRON_TIME="0 20 * * *"
# Set LANG environment variable for proper UTF-8 encoding
CRON_JOB="$CRON_TIME export LANG=en_US.UTF-8; $RSCRIPT_PATH $SCRIPT_PATH >> $LOG_PATH 2>&1"

echo "Rscript path: $RSCRIPT_PATH"
echo "Script path: $SCRIPT_PATH"
echo "Log path: $LOG_PATH"
echo ""
echo "Cron job to be added:"
echo "$CRON_JOB"
echo ""

# Check if cron job already exists
if crontab -l 2>/dev/null | grep -F "$SCRIPT_PATH" > /dev/null; then
    echo "⚠️  Cron job already exists!"
    echo ""
    echo "Current crontab:"
    crontab -l | grep -F "$SCRIPT_PATH"
    echo ""
    read -p "Do you want to remove and re-add it? (y/n) " -n 1 -r
    echo ""
    if [[ $REPLY =~ ^[Yy]$ ]]; then
        # Remove existing entry
        crontab -l | grep -v -F "$SCRIPT_PATH" | crontab -
        echo "✅ Removed existing cron job"
    else
        echo "❌ Cancelled"
        exit 0
    fi
fi

# Add the cron job
(crontab -l 2>/dev/null; echo "$CRON_JOB") | crontab -

echo ""
echo "✅ Cron job added successfully!"
echo ""
echo "The dashboard will be updated daily at 8:00 PM"
echo "Logs will be saved to: $LOG_PATH"
echo ""
echo "To view current crontab:"
echo "  crontab -l"
echo ""
echo "To remove the cron job:"
echo "  crontab -e"
echo "  (then delete the line containing update_dashboard.R)"
echo ""
echo "========================================="

