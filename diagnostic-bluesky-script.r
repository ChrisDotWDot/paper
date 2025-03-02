## Diagnostic Script for Bluesky Posting
cat("=== DIAGNOSTIC MODE ACTIVE ===\n")
cat("Script started at:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")

## packages
cat("Loading required packages...\n")
tryCatch({
  library(tidyRSS)
  library(atrrr)
  library(lubridate)
  library(dplyr)
  library(stringr)
  library(glue)
  library(purrr)
  library(xml2)
  cat("All packages loaded successfully\n")
}, error = function(e) {
  cat("ERROR LOADING PACKAGES:", conditionMessage(e), "\n")
  stop("Package loading failed - stopping execution")
})

## Environment check
cat("\n=== ENVIRONMENT CHECK ===\n")
cat("R version:", R.version.string, "\n")
cat("Platform:", Sys.info()["sysname"], "\n")
cat("Working directory:", getwd(), "\n")

# Check if ATR_PW environment variable exists
cat("ATR_PW environment variable check:", 
    ifelse(Sys.getenv("ATR_PW") != "", "EXISTS", "MISSING"), 
    "- Length:", nchar(Sys.getenv("ATR_PW")), "characters\n")

if (nchar(Sys.getenv("ATR_PW")) > 0) {
  # Show first/last character to check for quotes (without revealing the full password)
  cat("First character of ATR_PW:", substr(Sys.getenv("ATR_PW"), 1, 1), "\n")
  cat("Last character of ATR_PW:", substr(Sys.getenv("ATR_PW"), nchar(Sys.getenv("ATR_PW")), nchar(Sys.getenv("ATR_PW"))), "\n")
  
  # Check for unwanted quotes
  if (substr(Sys.getenv("ATR_PW"), 1, 1) == "\"" || substr(Sys.getenv("ATR_PW"), nchar(Sys.getenv("ATR_PW")), nchar(Sys.getenv("ATR_PW"))) == "\"") {
    cat("WARNING: ATR_PW contains quote characters which may cause authentication issues!\n")
  }
}

## Part 1: read RSS feed
cat("\n=== RSS FEED PROCESSING ===\n")

# Vector of Pubmed feeds from search terms:
pubmed_feeds <- c("https://pubmed.ncbi.nlm.nih.gov/rss/search/1H9AR3ZQQCaH18XJmZjvELYBadSZGaorxW0NnaqVNWn8GIEkJ4/?limit=15&utm_campaign=pubmed-2&fc=20250125192025")

# Read all the PubMed feeds
cat("Fetching PubMed feeds...\n")
tryCatch({
  pubmed_df <- map_df(pubmed_feeds, tidyfeed)
  cat("PubMed feeds fetched successfully. Rows:", nrow(pubmed_df), "\n")
}, error = function(e) {
  cat("ERROR FETCHING PUBMED FEEDS:", conditionMessage(e), "\n")
  pubmed_df <- data.frame() # Empty dataframe to continue script
})

# Vector of feeds of possible interest from bioRxiv, yields the last 30 days
brv_feeds <- c("http://connect.biorxiv.org/biorxiv_xml.php?subject=neuroscience")

# Read all the bioRxiv feeds
cat("Fetching bioRxiv feeds...\n")
tryCatch({
  brv <- map_df(brv_feeds, tidyfeed)
  cat("bioRxiv feeds fetched successfully. Rows:", nrow(brv), "\n")
}, error = function(e) {
  cat("ERROR FETCHING BIORXIV FEEDS:", conditionMessage(e), "\n")
  brv <- data.frame() # Empty dataframe to continue script
})

# Filter for biorxiv feed keywords and trim the link
cat("Filtering bioRxiv data...\n")
brv_filt <- brv |>
  filter(str_detect(item_title, "speech")) |>
  mutate(link = str_extract(item_link,"^.*?[^?]*"))
cat("bioRxiv filtered to", nrow(brv_filt), "rows\n")

# Filter for Pubmed feed keywords, recent publications, and trim link
cat("Filtering PubMed data...\n")
pubmed_filt <- pubmed_df |>
  filter(str_detect(item_title, "speech"),
         item_pub_date >= today() - 29) |>
  mutate(link = str_extract(item_link,"^.*?[^?]*"))
cat("PubMed filtered to", nrow(pubmed_filt), "rows\n")

# Filter posts for unique titles
cat("Creating combined post list...\n")
rss_posts <- bind_rows(brv_filt |> select(item_title, item_description, link),
                       pubmed_filt |> select(item_title, item_description, link)) |>
  distinct(item_title, .keep_all = TRUE)
cat("Combined unique posts:", nrow(rss_posts), "\n")

## Part 2: Create posts from feed
cat("\n=== POST PREPARATION ===\n")
posts <- rss_posts |>
  mutate(post_text = glue("{item_title} {link}"), # Needs to be <300 characters
         timestamp = now()) # Add timestamp
cat("Prepared", nrow(posts), "potential posts\n")

if (nrow(posts) > 0) {
  cat("Sample post:", substr(posts$post_text[1], 1, 50), "...\n")
}

## Part 3: Authentication
cat("\n=== AUTHENTICATION ATTEMPT ===\n")
# Load the app password from environment variable
pw <- Sys.getenv("ATR_PW")
cat("Password retrieved from environment, length:", nchar(pw), "characters\n")

# Authenticate to Bluesky
auth_success <- FALSE
tryCatch({
  cat("Attempting authentication to Bluesky...\n")
  auth(user = "speechpapers.bsky.social",
       password = pw,
       overwrite = TRUE)
  cat("Authentication appears successful\n")
  auth_success <- TRUE
}, error = function(e) {
  cat("AUTHENTICATION ERROR:", conditionMessage(e), "\n")
  print(e)
})

if (!auth_success) {
  cat("Authentication failed - cannot continue with posting\n")
  # Don't stop execution so we can complete diagnostics
} else {
  # Check for existing posts to avoid duplicates
  cat("\n=== RETRIEVING EXISTING POSTS ===\n")
  old_posts_success <- FALSE
  tryCatch({
    cat("Fetching existing posts...\n")
    old_posts <- get_skeets_authored_by("speechpapers.bsky.social", limit = 5000L)
    cat("Successfully retrieved", nrow(old_posts), "existing posts\n")
    old_posts_success <- TRUE
  }, error = function(e) {
    cat("ERROR FETCHING EXISTING POSTS:", conditionMessage(e), "\n")
    print(e)
    # Create empty dataframe to continue
    old_posts <- data.frame(text = character(0))
  })

  # Filter to post only new stuff
  if (old_posts_success) {
    cat("\n=== FILTERING FOR NEW POSTS ===\n")
    posts_new <- posts |>
      filter(!post_text %in% old_posts$text)
    cat("Found", nrow(posts_new), "new posts to publish\n")
  } else {
    cat("Skipping filtering due to error fetching existing posts\n")
    posts_new <- posts
  }

  ## Part 4: Post skeets
  if (nrow(posts_new) > 0) {
    cat("\n=== POSTING ATTEMPT ===\n")
    for (i in seq_len(nrow(posts_new))) {
      cat("Attempting to post item", i, "of", nrow(posts_new), "...\n")
      tryCatch({
        cat("Post text:", posts_new$post_text[i], "\n")
        result <- post_skeet(
          text = posts_new$post_text[i],
          created_at = posts_new$timestamp[i],
          preview_card = FALSE
        )
        cat("Post successful! Response:", ifelse(is.null(result), "NULL", "Object received"), "\n")
        # Log successful posts
        message("Successfully posted: ", posts_new$post_text[i])
      }, error = function(e) {
        # Detailed error logging
        cat("POSTING ERROR:", conditionMessage(e), "\n")
        print(e)
        message("Failed to post: ", posts_new$post_text[i])
        message("Full error details: ", toString(e))
        
        # Optional: Log errors to a file
        write(paste("Error posting:", posts_new$post_text[i], "Error:", toString(e)),
              file = "bluesky_post_errors.log", append = TRUE)
      })
    }
  } else {
    cat("No new posts to publish\n")
  }
}

cat("\n=== DIAGNOSTIC COMPLETE ===\n")
cat("Script completed at:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
