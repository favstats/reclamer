#!/usr/bin/env Rscript

# dashboard.R — Generate political ad spend tracker dashboard using dashboardr
#
# Recreates the Quarto-based reclamer dashboard as a dashboardr site.
# Fetches data from all three sources (PolitiekeReclame, Ster, DPG Media),
# then builds an interactive multi-page dashboard.

library(dashboardr)
library(dplyr)
library(tidyr)
library(stringr)
library(reactable)
library(htmltools)

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || all(is.na(x))) y else x

# ===========================================================================
# DATA LOADING
# ===========================================================================

cat("\n========================================\n")
cat("Reclamer Dashboard (dashboardr)\n")
cat(paste("Started at:", Sys.time()), "\n")
cat("========================================\n\n")

source("R/reclame.R")
source("R/ster_scraper.R")

# Fetch or load cached data
update <- TRUE

if (update) {
  cat("Fetching Ster data...\n")
  ster_data <- scrape_ster_transparency(
    download_dir = "ster_pdfs",
    parse_pdfs = TRUE
  )
  saveRDS(ster_data, "data/ster_data.rds")
  unlink("ster_pdfs/", recursive = TRUE)

  cat("Fetching PolitiekeReclame data...\n")
  api_data_politiekereclame_default <- fetch_ads(
    source = "politiekereclame",
    all = TRUE,
    verbose = FALSE
  )
  # Also fetch archived themes by ID (not returned by default)
  archived_themes <- c(6L)  # 6 = Tweede Kamerverkiezingen 2025
  api_data_archived <- lapply(archived_themes, function(tid) {
    tryCatch(
      fetch_ads(source = "politiekereclame", all = TRUE, verbose = FALSE,
                extra_params = list(theme = tid)),
      error = function(e) { cat("Theme", tid, "fetch failed:", e$message, "\n"); NULL }
    )
  })
  api_data_archived <- dplyr::bind_rows(Filter(Negate(is.null), api_data_archived))
  api_data_politiekereclame_new <- dplyr::bind_rows(api_data_politiekereclame_default, api_data_archived)
  api_data_politiekereclame_new <- api_data_politiekereclame_new[!duplicated(api_data_politiekereclame_new$public_id), ]
  # Merge with existing data to preserve campaigns the API may have dropped
  if (file.exists("data/api_data_politiekereclame.rds")) {
    api_data_politiekereclame_old <- readRDS("data/api_data_politiekereclame.rds")
    dropped <- api_data_politiekereclame_old %>%
      dplyr::filter(!public_id %in% api_data_politiekereclame_new$public_id)
    api_data_politiekereclame <- dplyr::bind_rows(api_data_politiekereclame_new, dropped)
  } else {
    api_data_politiekereclame <- api_data_politiekereclame_new
  }
  saveRDS(api_data_politiekereclame, "data/api_data_politiekereclame.rds")

  cat("Fetching DPG Media data...\n")
  api_data_dpgmedia <- fetch_ads(
    source = "dpgmedia",
    verbose = FALSE
  )
  saveRDS(api_data_dpgmedia, "data/api_data_dpgmedia.rds")
} else {
  api_data_dpgmedia <- readRDS("data/api_data_dpgmedia.rds")
  api_data_politiekereclame <- readRDS("data/api_data_politiekereclame.rds")
  ster_data <- readRDS("data/ster_data.rds")
}

# ===========================================================================
# PARTY AGGREGATION
# ===========================================================================

aggregate_parties <- function(party_names) {
  sapply(party_names, function(party_name) {
    if (is.na(party_name) || party_name == "Unknown") return("Unknown")
    name_lower <- tolower(stringr::str_trim(party_name))
    if (stringr::str_detect(name_lower, "pvv|partij voor de vrijheid")) return("PVV")
    if (stringr::str_detect(name_lower, "pink|partijvoordedieren|pvdd|partij voor de dieren")) return("PvdD")
    if (stringr::str_detect(name_lower, "vvd|volkspartij voor vrijheid")) return("VVD")
    if (stringr::str_detect(name_lower, "pvda|partij van de arbeid|groenlinks.*pvda|groenlinks")) return("PvdA/GroenLinks")
    if (stringr::str_detect(name_lower, "d66|democraten 66")) return("D66")
    if (stringr::str_detect(name_lower, "cda|christen.*democratisch")) return("CDA")
    if (stringr::str_detect(name_lower, "\\bsp\\b|socialistische partij")) return("SP")
    if (stringr::str_detect(name_lower, "vrede voor dieren")) return("Vrede voor Dieren")
    if (stringr::str_detect(name_lower, "fvd|forum voor democratie|forumvoordemocratie")) return("FvD")
    if (stringr::str_detect(name_lower, "nsc|nieuw sociaal contract")) return("NSC")
    if (stringr::str_detect(name_lower, "christenunie")) return("ChristenUnie")
    if (stringr::str_detect(name_lower, "sgp|staatkundig gereformeerde")) return("SGP")
    if (stringr::str_detect(name_lower, "denk")) return("DENK")
    if (stringr::str_detect(name_lower, "ja21")) return("JA21")
    if (stringr::str_detect(name_lower, "volt")) return("Volt")
    if (stringr::str_detect(name_lower, "bbb|boerburgerbeweging")) return("BBB")
    if (stringr::str_detect(name_lower, "bij1")) return("BIJ1")
    if (stringr::str_detect(name_lower, "50plus|50\\+")) return("50PLUS")
    return(party_name)
  })
}

# Party colors
party_colors <- c(
  "VVD" = "#e85a0f",
  "PVV" = "#154273",
  "CDA" = "#007c5e",
  "D66" = "#01ac48",
  "GroenLinks-PvdA" = "#e10600",
  "PvdA/GroenLinks" = "#e10600",
  "GroenLinks" = "#00842b",
  "PvdA" = "#e10600",
  "SP" = "#f00000",
  "ChristenUnie" = "#00a7eb",
  "PvdD" = "#006b2c",
  "Vrede voor Dieren" = "#506b2c",
  "50PLUS" = "#672c7a",
  "SGP" = "#006e98",
  "DENK" = "#00bcb3",
  "FvD" = "#8b1a1a",
  "JA21" = "#12487c",
  "Volt" = "#52267d",
  "BIJ1" = "#fff200",
  "BBB" = "#93bf1f",
  "NSC" = "#0a3d8f",
  "Unknown" = "#94a3b8"
)

# ===========================================================================
# DATA TRANSFORMATION
# ===========================================================================

# --- Ster data ---
ster_per_row <- ster_data %>%
  group_by(party) %>%
  arrange(desc(version)) %>%
  filter(version == max(as.numeric(version), na.rm = TRUE) | version == "unknown") %>%
  ungroup() %>%
  arrange(desc(total_spending)) %>%
  mutate(type = ifelse(party == "Unknown", "others", "party")) %>%
  mutate(
    actor = if_else(
      type == "others",
      stringr::word(filename, 1, sep = fixed("_")),
      party
    )
  ) %>%
  select(type, actor, everything())

cols_to_sum <- c(
  "total_spending",
  "tv_total_amount", "tv_ster_amount",
  "radio_total_amount", "radio_ster_amount",
  "online_display_total_amount", "online_display_ster_amount",
  "online_video_total_amount", "online_video_ster_amount"
)

ster_by_actor_type <- ster_per_row %>%
  group_by(type, actor, pdf_url, campaign_start, campaign_end) %>%
  summarise(across(all_of(cols_to_sum), ~ sum(.x, na.rm = TRUE)), .groups = "drop")

ster_df <- ster_by_actor_type %>%
  arrange(desc(total_spending)) %>%
  mutate(
    online_spending_total = online_display_total_amount + online_video_total_amount,
    online_spending = online_display_ster_amount + online_video_ster_amount
  ) %>%
  rename(
    radio_total = radio_total_amount,
    radio_spending = radio_ster_amount,
    tv_total = tv_total_amount,
    tv_spending = tv_ster_amount,
    advertiser = actor
  ) %>%
  mutate(data_source = "Ster PDFs")

# --- PolitiekeReclame data ---
api_df_politiekereclame <- api_data_politiekereclame %>%
  unnest_wider(theme_link, names_sep = "_") %>%
  mutate(
    advertiser_raw = opdrachtgever_naam_organisatie,
    advertiser = aggregate_parties(advertiser_raw),
    total_spending = readr::parse_number(betaalde_media_waarde_euro),
    total_value = readr::parse_number(totale_waarde_campagne_euro),
    media_channel = ingezette_media,
    data_source = "Politiekereclame API",
    theme = coalesce(theme_link_label, "Unknown"),
    campaign_start = as.Date(campagne_periode_startdatum),
    campaign_end = as.Date(campagne_periode_einddatum)
  ) %>%
  select(
    advertiser_raw, organization_created, advertiser, total_spending,
    media_channel, data_source, betaalde_media_waarde_euro,
    campaign_start, campaign_end, total_value, public_id, theme
  )

# --- DPG Media data ---
api_df_dpgmedia <- api_data_dpgmedia %>%
  mutate(
    advertiser_raw = case_when(
      !is.na(sponsorName) ~ sponsorName,
      !is.na(payerName) ~ payerName,
      TRUE ~ "Unknown"
    ),
    advertiser = aggregate_parties(advertiser_raw),
    budgetLowerBound = ifelse(is.na(budgetLowerBound), 1, budgetLowerBound),
    budgetUpperBound = ifelse(is.na(budgetUpperBound), budgetLowerBound, budgetUpperBound),
    total_spending = (budgetLowerBound + budgetUpperBound) / 2,
    media_channel = channel,
    data_source = "DPG Media API"
  ) %>%
  select(advertiser, total_spending, budgetLowerBound, budgetUpperBound, media_channel, data_source)

# ===========================================================================
# SUMMARY STATS
# ===========================================================================

total_pr <- sum(api_df_politiekereclame$total_value, na.rm = TRUE)
total_pr_spending <- sum(api_df_politiekereclame$total_spending, na.rm = TRUE)
total_ster <- sum(ster_df$total_spending, na.rm = TRUE)
total_dpg <- sum(api_df_dpgmedia$total_spending, na.rm = TRUE)

n_pr <- nrow(api_df_politiekereclame)
n_ster <- nrow(ster_df)
n_dpg <- nrow(api_df_dpgmedia)

# Spending by party (all sources combined)
all_spending <- bind_rows(
  api_df_politiekereclame %>%
    filter(!is.na(advertiser) & advertiser != "Unknown") %>%
    transmute(advertiser, spending = total_value, source = "PolitiekeReclame"),
  ster_df %>%
    filter(!is.na(advertiser) & advertiser != "Unknown") %>%
    transmute(advertiser, spending = total_spending, source = "Ster"),
  api_df_dpgmedia %>%
    filter(!is.na(advertiser) & advertiser != "Unknown") %>%
    transmute(advertiser, spending = total_spending, source = "DPG Media")
)

party_spending <- all_spending %>%
  group_by(advertiser) %>%
  summarise(total = sum(spending, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(total))

# Spending by source
source_spending <- all_spending %>%
  group_by(source) %>%
  summarise(total = sum(spending, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(total))

# ===========================================================================
# PAGE 1: Ad Spend Tracker (Overview)
# ===========================================================================

# Row-level data for sparkline cards (one row per campaign)
sparkline_data <- api_df_politiekereclame %>%
  filter(!is.na(campaign_start)) %>%
  transmute(date = campaign_start, theme)

overview_page <- create_page(
  "Ad Spend Tracker",
  data = sparkline_data,
  icon = "line-md:gauge",
  is_landing_page = TRUE
)

# Landing text
overview_page <- overview_page %>%
  add_text(md_text(
    "This dashboard tracks political advertising spending across three major Dutch data sources,",
    "covering the **2025 parliamentary elections and beyond** (including the **2026 municipal elections**).",
    "Following new **EU transparency regulations** and subsequent political ad bans by Meta and Google,",
    "attention has shifted toward traditional media channels.",
    "",
    paste0("*Last updated: ", format(Sys.time(), "%B %d, %Y at %H:%M %Z"), "*")
  ))

# Value box cards for each source
overview_page <- overview_page %>%
  add_value_box_row() %>%
    add_value_box(
      title = "PolitiekeReclame.nl",
      value = paste0("\u20AC", format(round(total_pr), big.mark = ",")),
      bg_color = "#4c5f7a"
    ) %>%
    add_value_box(
      title = "Ster Transparency",
      value = paste0("\u20AC", format(round(total_ster), big.mark = ",")),
      bg_color = "#3d7068"
    ) %>%
    add_value_box(
      title = "DPG Media",
      value = paste0("\u20AC", format(round(total_dpg), big.mark = ",")),
      bg_color = "#5b8db8"
    ) %>%
  end_value_box_row()

# --- Spending by Election Theme (treemap) ---
theme_spending <- api_df_politiekereclame %>%
  group_by(theme) %>%
  summarise(total = sum(total_value, na.rm = TRUE), campaigns = n(), .groups = "drop") %>%
  arrange(desc(total)) %>%
  mutate(.count = total)

theme_pal <- c(
  "Gemeenteraadsverkiezingen 2026" = "#3b82f6",
  "Tweede Kamerverkiezingen 2025" = "#e85a0f",
  "Algemeen - beleid en maatschappelijke thema's" = "#10b981"
)

if (nrow(theme_spending) > 0) {
  theme_content <- create_content(data = theme_spending, type = "treemap") %>%
    add_viz(
      group_var = "theme",
      value_var = ".count",
      title = "PolitiekeReclame: Spending by Election Theme",
      color_palette = theme_pal
    )
  overview_page <- overview_page %>% add_content(theme_content)
}

# --- Spending by Party (treemap) ---
party_treemap_data <- party_spending %>%
  filter(total > 0) %>%
  head(20) %>%
  mutate(.count = total)

if (nrow(party_treemap_data) > 0) {
  party_treemap <- create_content(data = party_treemap_data, type = "treemap") %>%
    add_viz(
      group_var = "advertiser",
      value_var = ".count",
      title = "Total Spending by Party (All Sources)",
      color_palette = party_colors
    )
  overview_page <- overview_page %>% add_content(party_treemap)
}

# --- Spending by Source (bar chart) ---
source_bar_data <- source_spending %>%
  mutate(.count = total)

source_pal <- c(
  "PolitiekeReclame" = "#4c5f7a",
  "Ster" = "#3d7068",
  "DPG Media" = "#5b8db8"
)

if (nrow(source_bar_data) > 0) {
  source_content <- create_content(data = source_bar_data, type = "bar") %>%
    add_viz(
      x_var = "source",
      value_var = ".count",
      title = "Total Spending by Data Source",
      color_palette = source_pal,
      x_order = c("PolitiekeReclame", "Ster", "DPG Media"),
      horizontal = TRUE
    )
  overview_page <- overview_page %>% add_content(source_content)
}

# --- PolitiekeReclame: Spending by Party (bar) ---
pr_party <- api_df_politiekereclame %>%
  filter(!is.na(advertiser) & advertiser != "Unknown") %>%
  group_by(advertiser) %>%
  summarise(total = sum(total_value, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(total)) %>%
  head(20) %>%
  mutate(.count = total)

pr_party_order <- rev(pr_party$advertiser)

if (nrow(pr_party) > 0) {
  pr_content <- create_content(data = pr_party, type = "bar") %>%
    add_viz(
      x_var = "advertiser",
      value_var = ".count",
      title = "PolitiekeReclame: Top Spenders",
      color_palette = party_colors,
      horizontal = TRUE,
      x_order = pr_party_order,
      tabgroup = "by_source/politiekereclame"
    )

  # Ster: Spending by Party (bar)
  ster_party <- ster_df %>%
    filter(!is.na(advertiser) & advertiser != "Unknown") %>%
    group_by(advertiser) %>%
    summarise(total = sum(total_spending, na.rm = TRUE), .groups = "drop") %>%
    arrange(desc(total)) %>%
    head(20) %>%
    mutate(.count = total)

  ster_party_order <- rev(ster_party$advertiser)

  if (nrow(ster_party) > 0) {
    pr_content <- pr_content %>%
      add_viz(
        data = ster_party, type = "bar",
        x_var = "advertiser",
        value_var = ".count",
        title = "Ster: Top Spenders",
        color_palette = party_colors,
        horizontal = TRUE,
        x_order = ster_party_order,
        tabgroup = "by_source/ster"
      )
  }

  # DPG Media: Spending by Party (bar)
  dpg_party <- api_df_dpgmedia %>%
    filter(!is.na(advertiser) & advertiser != "Unknown") %>%
    group_by(advertiser) %>%
    summarise(total = sum(total_spending, na.rm = TRUE), .groups = "drop") %>%
    arrange(desc(total)) %>%
    head(20) %>%
    mutate(.count = total)

  dpg_party_order <- rev(dpg_party$advertiser)

  if (nrow(dpg_party) > 0) {
    pr_content <- pr_content %>%
      add_viz(
        data = dpg_party, type = "bar",
        x_var = "advertiser",
        value_var = ".count",
        title = "DPG Media: Top Spenders",
        color_palette = party_colors,
        horizontal = TRUE,
        x_order = dpg_party_order,
        tabgroup = "by_source/dpgmedia"
      )
  }

  overview_page <- overview_page %>% add_content(pr_content)
}

# --- Media Channel breakdown: Ster (bar chart) ---
ster_channel <- ster_df %>%
  summarise(
    Television = sum(tv_total, na.rm = TRUE),
    Radio = sum(radio_total, na.rm = TRUE),
    Online = sum(online_spending_total, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(everything(), names_to = "channel", values_to = "amount") %>%
  filter(amount > 0) %>%
  mutate(.count = amount)

channel_pal <- c(
  "Television" = "#3b82f6",
  "Radio" = "#10b981",
  "Online" = "#f59e0b",
  "PRINT" = "#8b5cf6",
  "DIGITAL" = "#ec4899"
)

if (nrow(ster_channel) > 0) {
  channel_content <- create_content(data = ster_channel, type = "bar") %>%
    add_viz(
      x_var = "channel",
      value_var = ".count",
      title = "Ster: Spending by Channel",
      color_palette = channel_pal,
      horizontal = TRUE,
      tabgroup = "channels/ster"
    )

  # DPG Media channel breakdown
  dpg_channel <- api_df_dpgmedia %>%
    filter(!is.na(media_channel) & media_channel != "") %>%
    group_by(media_channel) %>%
    summarise(amount = sum(total_spending, na.rm = TRUE), .groups = "drop") %>%
    arrange(desc(amount)) %>%
    mutate(.count = amount)

  if (nrow(dpg_channel) > 0) {
    channel_content <- channel_content %>%
      add_viz(
        data = dpg_channel, type = "bar",
        x_var = "media_channel",
        value_var = ".count",
        title = "DPG Media: Spending by Channel",
        color_palette = channel_pal,
        horizontal = TRUE,
        tabgroup = "channels/dpgmedia"
      )
  }

  # PolitiekeReclame channel breakdown
  pr_channel <- api_df_politiekereclame %>%
    filter(!is.na(media_channel) & media_channel != "") %>%
    mutate(
      channels = strsplit(media_channel, ","),
      channel_count = sapply(channels, length)
    ) %>%
    rowwise() %>%
    mutate(spending_per_channel = total_value / channel_count) %>%
    ungroup() %>%
    tidyr::unnest(channels) %>%
    mutate(channels = str_trim(channels)) %>%
    group_by(channels) %>%
    summarise(amount = sum(spending_per_channel, na.rm = TRUE), .groups = "drop") %>%
    arrange(desc(amount)) %>%
    mutate(.count = amount)

  if (nrow(pr_channel) > 0) {
    channel_content <- channel_content %>%
      add_viz(
        data = pr_channel, type = "bar",
        x_var = "channels",
        value_var = ".count",
        title = "PolitiekeReclame: Spending by Channel",
        horizontal = TRUE,
        tabgroup = "channels/politiekereclame"
      )
  }

  overview_page <- overview_page %>% add_content(channel_content)
}

# --- Timeline: campaign spending over time ---
timeline_agg <- api_df_politiekereclame %>%
  filter(!is.na(campaign_start)) %>%
  mutate(date = campaign_start) %>%
  group_by(date) %>%
  summarise(value = sum(total_value, na.rm = TRUE), .groups = "drop") %>%
  arrange(date)

if (nrow(timeline_agg) > 0) {
  timeline_content <- create_content(data = timeline_agg, type = "timeline") %>%
    add_viz(
      time_var = "date",
      y_var = "value",
      agg = "none",
      title = "PolitiekeReclame: Campaign Start Dates Over Time",
      chart_type = "line",
      y_label = "Spending (\u20AC)"
    )
  overview_page <- overview_page %>% add_content(timeline_content)
}

# Tabgroup labels
overview_page <- set_tabgroup_labels.page_object(
  overview_page,
  by_source = "Top Spenders by Source",
  politiekereclame = "PolitiekeReclame",
  ster = "Ster",
  dpgmedia = "DPG Media",
  channels = "Spending by Channel"
)

# ===========================================================================
# PAGE 2: Data Explorer (Reactable tables)
# ===========================================================================

explorer_page <- create_page("Data Explorer", icon = "line-md:search")

# --- PolitiekeReclame table ---
pr_table_data <- api_df_politiekereclame %>%
  filter(!is.na(advertiser) & advertiser != "Unknown") %>%
  group_by(advertiser, advertiser_raw, public_id, theme, campaign_start, campaign_end) %>%
  summarise(
    total_spending = sum(total_spending, na.rm = TRUE),
    total_value = sum(total_value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(total_value)) %>%
  mutate(
    campaign_period = paste(
      format(campaign_start, "%b %d"),
      "\u2013",
      format(campaign_end, "%b %d, %Y")
    ),
    theme_short = case_when(
      str_detect(theme, "Tweede Kamer") ~ "Elections 2025",
      TRUE ~ str_trunc(theme, 35)
    ),
    transparency_url = paste0("https://politiekereclame.nl/transparantie-verklaringen/", public_id)
  )

pr_reactable <- reactable(
  pr_table_data %>%
    select(advertiser_raw, advertiser, total_value, campaign_period, theme_short, transparency_url),
  filterable = TRUE,
  pagination = TRUE,
  defaultPageSize = 15,
  defaultSorted = "total_value",
  defaultSortOrder = "desc",
  highlight = TRUE,
  compact = TRUE,
  defaultColDef = colDef(headerClass = "header", align = "left"),
  columns = list(
    advertiser_raw = colDef(
      name = "Advertiser",
      minWidth = 200,
      cell = function(value, index) {
        party <- pr_table_data$advertiser[index]
        color <- party_colors[party] %||% "#64748b"
        div(
          style = list(display = "flex", alignItems = "center"),
          div(style = list(
            width = "10px", height = "10px", borderRadius = "50%",
            background = color, marginRight = "10px", flexShrink = 0
          )),
          span(style = list(fontWeight = 600), value)
        )
      },
      html = TRUE
    ),
    advertiser = colDef(show = FALSE),
    total_value = colDef(
      name = "Total Value",
      defaultSortOrder = "desc",
      cell = function(value) {
        formatted <- format(round(value), big.mark = ",", scientific = FALSE)
        div(
          style = list(fontWeight = 600, fontFamily = "monospace"),
          paste0("\u20AC", formatted)
        )
      }
    ),
    campaign_period = colDef(name = "Period", minWidth = 140),
    theme_short = colDef(name = "Theme", filterable = TRUE),
    transparency_url = colDef(
      name = "Link",
      width = 70,
      cell = function(value) {
        tags$a(
          href = value, target = "_blank",
          style = "color: #0d6efd; text-decoration: none; font-weight: 600;",
          "View"
        )
      },
      html = TRUE,
      filterable = FALSE
    )
  ),
  elementId = "tbl-pr"
)

explorer_page <- explorer_page %>%
  add_text(md_text("## PolitiekeReclame.nl Data")) %>%
  add_reactable(reactable_object = pr_reactable)

# --- Ster table ---
ster_table_data <- ster_df %>%
  filter(!is.na(advertiser) & advertiser != "Unknown") %>%
  arrange(desc(total_spending)) %>%
  mutate(
    ster_spending = tv_spending + radio_spending + online_spending,
    ster_pct = round((ster_spending / total_spending) * 100, 1)
  ) %>%
  select(advertiser, total_spending, ster_spending, ster_pct,
         tv_total, radio_total, online_spending_total, pdf_url)

ster_reactable <- reactable(
  ster_table_data,
  filterable = TRUE,
  pagination = TRUE,
  defaultPageSize = 15,
  defaultSorted = "total_spending",
  defaultSortOrder = "desc",
  highlight = TRUE,
  compact = TRUE,
  defaultColDef = colDef(headerClass = "header", align = "left"),
  columns = list(
    advertiser = colDef(
      name = "Advertiser",
      minWidth = 180,
      cell = function(value) {
        color <- party_colors[value] %||% "#64748b"
        div(
          style = list(display = "flex", alignItems = "center"),
          div(style = list(
            width = "10px", height = "10px", borderRadius = "50%",
            background = color, marginRight = "10px", flexShrink = 0
          )),
          span(style = list(fontWeight = 600), value)
        )
      },
      html = TRUE
    ),
    total_spending = colDef(
      name = "Total Spending",
      cell = function(value) {
        div(style = list(fontWeight = 600, fontFamily = "monospace"),
            paste0("\u20AC", format(round(value), big.mark = ",")))
      }
    ),
    ster_spending = colDef(
      name = "Ster Channels",
      cell = function(value) {
        div(style = list(fontFamily = "monospace"),
            paste0("\u20AC", format(round(value), big.mark = ",")))
      }
    ),
    ster_pct = colDef(name = "Ster %", width = 80),
    tv_total = colDef(
      name = "TV",
      cell = function(value) {
        if (value == 0) return("")
        paste0("\u20AC", format(round(value), big.mark = ","))
      }
    ),
    radio_total = colDef(
      name = "Radio",
      cell = function(value) {
        if (value == 0) return("")
        paste0("\u20AC", format(round(value), big.mark = ","))
      }
    ),
    online_spending_total = colDef(
      name = "Online",
      cell = function(value) {
        if (value == 0) return("")
        paste0("\u20AC", format(round(value), big.mark = ","))
      }
    ),
    pdf_url = colDef(
      name = "PDF",
      width = 60,
      cell = function(value) {
        if (is.na(value) || !nzchar(value)) return("")
        tags$a(href = value, target = "_blank",
               style = "color: #0d6efd; text-decoration: none;", "PDF")
      },
      html = TRUE,
      filterable = FALSE
    )
  ),
  elementId = "tbl-ster"
)

explorer_page <- explorer_page %>%
  add_text(md_text("## Ster Transparency Data")) %>%
  add_reactable(reactable_object = ster_reactable)

# --- DPG Media table ---
dpg_table_data <- api_df_dpgmedia %>%
  filter(!is.na(advertiser) & advertiser != "Unknown") %>%
  group_by(advertiser, media_channel) %>%
  summarise(
    total_spending = sum(total_spending, na.rm = TRUE),
    campaigns = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(total_spending))

dpg_reactable <- reactable(
  dpg_table_data,
  filterable = TRUE,
  pagination = TRUE,
  defaultPageSize = 15,
  defaultSorted = "total_spending",
  defaultSortOrder = "desc",
  highlight = TRUE,
  compact = TRUE,
  defaultColDef = colDef(headerClass = "header", align = "left"),
  columns = list(
    advertiser = colDef(
      name = "Advertiser",
      minWidth = 180,
      cell = function(value) {
        color <- party_colors[value] %||% "#64748b"
        div(
          style = list(display = "flex", alignItems = "center"),
          div(style = list(
            width = "10px", height = "10px", borderRadius = "50%",
            background = color, marginRight = "10px", flexShrink = 0
          )),
          span(style = list(fontWeight = 600), value)
        )
      },
      html = TRUE
    ),
    total_spending = colDef(
      name = "Est. Spending",
      cell = function(value) {
        div(style = list(fontWeight = 600, fontFamily = "monospace"),
            paste0("\u20AC", format(round(value), big.mark = ",")))
      }
    ),
    media_channel = colDef(name = "Channel", filterable = TRUE),
    campaigns = colDef(name = "Items", width = 70)
  ),
  elementId = "tbl-dpg"
)

explorer_page <- explorer_page %>%
  add_text(md_text("## DPG Media Data")) %>%
  add_reactable(reactable_object = dpg_reactable)

# ===========================================================================
# PAGE 3: Info
# ===========================================================================

info_page <- create_page("Info", icon = "line-md:alert-circle")

info_page <- info_page %>%
  add_text(md_text(
    "## About This Dashboard",
    "",
    "This dashboard tracks political advertising spending across three major Dutch data sources",
    "leading up to the **2025 parliamentary elections**.",
    "",
    "### Data Sources",
    "",
    "**PolitiekeReclame.nl** - The central platform developed by five Dutch media associations",
    "(Audify, MMA, NDP Nieuwsmedia, Outreach, and Screenforce) for EU political advertising",
    "transparency. Political advertisers submit declarations covering TV, radio, newspapers,",
    "magazines, outdoor, and online advertising.",
    "",
    "**Ster** - The advertising sales organization for Dutch public broadcasting (NPO).",
    "Their transparency page lists political parties and organizations that purchased",
    "advertising through television, radio, and online channels.",
    "",
    "**DPG Media** - A major Dutch media company operating newspapers, magazines, and",
    "digital platforms. Their transparency page lists political advertising campaigns on",
    "their print and digital channels. DPG Media reports spending boundaries rather than",
    "exact amounts; we calculate spending by taking the midpoint.",
    "",
    "### Methodology",
    "",
    "- Data is fetched daily from all three APIs/sources",
    "- Party names are aggregated using regex matching to unify different spellings",
    "- For PolitiekeReclame channel analysis, multi-channel campaigns are split equally",
    "- DPG Media spending is estimated as the midpoint of budget brackets",
    "- Ster data is extracted from PDF transparency statements",
    "",
    "### Code",
    "",
    "Powered by the **reclamer** R package: [github.com/favstats/reclamer](https://github.com/favstats/reclamer)",
    "",
    paste0("*Dashboard generated: ", format(Sys.time(), "%B %d, %Y at %H:%M %Z"), "*")
  ))

# ===========================================================================
# GENERATE DASHBOARD
# ===========================================================================

output_dir <- "dashboard_output"

cat("\nGenerating dashboard...\n")

create_dashboard(
  output_dir = output_dir,
  title = "NL Political Ad Spend Tracker",
  theme = "cosmo",
  sidebar = FALSE,
  page_layout = "full",
  mainfont = "Roboto",
  back_to_top = TRUE,
  tabset_theme = "minimal",
  tabset_colors = list(active_bg = "#0028B8", active_text = "#FFFFFF"),
  value_boxes = TRUE,
  search = TRUE,
  backend = "echarts4r"
) %>%
  add_pages(overview_page, explorer_page, info_page) %>%
  generate_dashboard(render = TRUE, open = FALSE)

cat("\n========================================\n")
cat("Dashboard generated successfully!\n")
cat(paste("Output:", file.path(output_dir, "docs", "index.html")), "\n")
cat(paste("Finished at:", Sys.time()), "\n")
cat("========================================\n")
