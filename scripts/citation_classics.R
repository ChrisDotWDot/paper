# scripts/citation_classics.R
# Posts highly-cited speech science papers bi-weekly
library(atrrr)
library(dplyr)
library(readr)
library(glue)
library(lubridate)
library(stringr)

# Initialize logging
log_file <- "citation_classics.log"
log_message <- function(msg) {
  write(paste(now(), "-", msg), file = log_file, append = TRUE)
  message(msg)
}

log_message("=== Starting Citation Classics ===")
log_message("SCRIPT VERSION: grapheme-guard-v2-smart-assemble")

# Authenticate to Bluesky
tryCatch({
  pw <- Sys.getenv("ATR_PW")
  if (pw == "") stop("ATR_PW environment variable not set")
  
  auth(user = "speechpapers.bsky.social",
       password = pw,
       overwrite = TRUE)
  log_message("Authentication successful")
}, error = function(e) {
  log_message(paste("Authentication failed:", toString(e)))
  stop(e)
})

# Load curated classics
classics_file <- "content/classics/citation-classics.csv"
posted_file <- "content/classics/posted-classics.csv"

# Load classics
classics <- read_csv(classics_file, show_col_types = FALSE)
log_message(paste("Loaded", nrow(classics), "classics"))

# Load or create posted tracker
if (file.exists(posted_file)) {
  posted_classics <- read_csv(posted_file, show_col_types = FALSE)
  log_message(paste("Loaded", nrow(posted_classics), "posted classics"))
} else {
  posted_classics <- tibble(id = integer(), date_posted = Date())
  write_csv(posted_classics, posted_file)
  log_message("Created new posted classics tracker")
}

# Get available classics
available <- classics %>% 
  anti_join(posted_classics, by = "id")

# Reset if all classics posted
if (nrow(available) == 0) {
  log_message("All classics posted - resetting tracker")
  posted_classics <- tibble(id = integer(), date_posted = Date())
  available <- classics
  write_csv(posted_classics, posted_file)
}

# Select classic (prioritize highest citations)
today_classic <- available %>%
  arrange(desc(citations_approx)) %>%
  slice(1)

log_message(paste("Selected classic ID:", today_classic$id, "-", today_classic$title))

# --- Robust Truncation Logic ---
# Calculate the fixed parts of the tweet so we NEVER truncate the URL or tags
fixed_author_line <- paste0(today_classic$authors, " (", today_classic$year, ")")
fixed_stats_line <- paste0("Citations: ", format(today_classic$citations_approx, big.mark = ','), "+")
fixed_url_line <- paste0("🔗 ", today_classic$url)
fixed_tags_line <- "#SpeechScience"

# Base length of the absolute mandatory components including newlines
base_components <- paste(
  "📚 Citation Classic",
  "\"\"", # placeholder for title quotes
  fixed_author_line,
  fixed_stats_line,
  fixed_url_line,
  fixed_tags_line,
  sep = "\n"
)

base_len <- nchar(base_components)
max_allowed <- 295 # Bluesky limit is 300, leaving a 5-char buffer just in case
available_space <- max_allowed - base_len

title_text <- today_classic$title
sig_text <- today_classic$significance

# 1. Check if we need to truncate the title itself
if (nchar(title_text) > available_space) {
  # Title + URL is so long we have to truncate the title and remove significance entirely
  allowed_title_len <- max(0, available_space - 3)
  title_text <- paste0(substr(title_text, 1, allowed_title_len), "...")
  sig_text <- ""
} else {
  # 2. We have room for the title. Do we have room for significance?
  space_for_sig <- available_space - nchar(title_text) - 1 # -1 for newline
  
  if (isTRUE(is.na(sig_text))) {
    sig_text <- ""
  } else if (space_for_sig < 10) {
    sig_text <- "" # Not enough room to bother
  } else if (nchar(sig_text) > space_for_sig) {
    sig_text <- paste0(substr(sig_text, 1, space_for_sig - 3), "...")
  }
}

# Assemble final post text safely
lines <- c(
  "📚 Citation Classic",
  paste0("\"", title_text, "\""),
  fixed_author_line,
  fixed_stats_line
)

# Only add significance if it isn't empty
if (sig_text != "") {
  lines <- c(lines, sig_text)
}

lines <- c(lines, fixed_url_line, fixed_tags_line)
post_text <- paste(lines, collapse = "\n")

log_message(paste("Post length:", nchar(post_text), "characters"))

# Final brute-force safety check (should never trigger, but keeps action from failing)
if (nchar(post_text) > 300) {
  log_message("WARNING: Post text still over 300! Applying strict fallback.")
  post_text <- paste0(substr(post_text, 1, 297), "...")
}

# Post to Bluesky
tryCatch({
  log_message("Posting citation classic...")
  
  result <- post_skeet(
    text = post_text,
    created_at = now(),
    preview_card = FALSE
  )
  
  log_message("Post successful!")
  
  # Mark classic as posted
  posted_classics <- bind_rows(
    posted_classics,
    tibble(id = today_classic$id, date_posted = today())
  )
  write_csv(posted_classics, posted_file)
  log_message("Updated tracking file")
  
}, error = function(e) {
  log_message(paste("ERROR posting:", toString(e)))
  stop(e)
})

log_message("=== Citation Classics Complete ===")
