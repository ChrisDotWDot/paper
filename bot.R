## packages
library(tidyRSS)
library(atrrr)
library(lubridate)
library(dplyr)
library(stringr)
library(glue)
library(purrr)
library(xml2)

## Part 1: read RSS feed

# Safe wrapper for tidyfeed that won't crash on failed URLs
safe_tidyfeed <- function(url) {
  tryCatch(
    tidyfeed(url),
    error = function(e) {
      message("Feed unavailable, skipping ", url, ": ", conditionMessage(e))
      NULL
    }
  )
}

# Vector of Pubmed feeds from search terms
pubmed_feeds <- c("https://pubmed.ncbi.nlm.nih.gov/rss/search/1H9AR3ZQQCaH18XJmZjvELYBadSZGaorxW0NnaqVNWn8GIEkJ4/?limit=15&utm_campaign=pubmed-2&fc=20250125192025")

# Read all the PubMed feeds with error handling
pubmed_df <- map(pubmed_feeds, safe_tidyfeed) |>
  compact() |>
  bind_rows() |>
  (\(df) if (nrow(df) == 0) tibble(item_title = character(), item_description = character(),
                                    item_link = character(), item_pub_date = as.Date(character())) else df)()

# Vector of feeds of possible interest from bioRxiv, yields the last 30 days
brv_feeds <- c("http://connect.biorxiv.org/biorxiv_xml.php?subject=neuroscience")

# Read all the bioRxiv feeds with error handling
brv <- map(brv_feeds, safe_tidyfeed) |>
  compact() |>
  bind_rows() |>
  (\(df) if (nrow(df) == 0) tibble(item_title = character(), item_description = character(),
                                    item_link = character()) else df)()

# Filter for biorxiv feed keywords and trim the link
brv_filt <- brv |>
  filter(str_detect(item_title, "speech")) |>
  mutate(link = str_extract(item_link, "^.*?[^?]*"))

# Filter for Pubmed feed keywords, recent publications, and trim link
pubmed_filt <- pubmed_df |>
  filter(str_detect(item_title, "speech"),
         item_pub_date >= today() - 29) |>
  mutate(link = str_extract(item_link, "^.*?[^?]*"))

# Filter posts for unique titles
rss_posts <- bind_rows(
  brv_filt    |> select(item_title, item_description, link),
  pubmed_filt |> select(item_title, item_description, link)
) |>
  distinct(item_title, .keep_all = TRUE)

# Exit early if no posts found
if (nrow(rss_posts) == 0) {
  message("No new RSS items found matching filter. Exiting.")
  quit(status = 0)
}

## Part 2: Create posts from feed
posts <- rss_posts |>
  mutate(post_text = glue("{item_title} {link}"), # Needs to be <300 characters
         timestamp = now())

## Part 3: Authentication

# Load the app password from environment variable
pw <- Sys.getenv("ATR_PW")
if (pw == "") stop("ATR_PW environment variable is not set.")

# Authenticate to Bluesky
auth(user = "speechpapers.bsky.social",
     password = pw,
     overwrite = TRUE)

# Check for existing posts to avoid duplicates
old_posts <- tryCatch({
  get_skeets_authored_by("speechpapers.bsky.social", limit = 500L)
}, error = function(e) {
  message("Could not fetch 500 old posts, falling back to 100: ", e$message)
  tryCatch({
    get_skeets_authored_by("speechpapers.bsky.social", limit = 100L)
  }, error = function(e2) {
    message("Could not fetch old posts at all, proceeding without dedup: ", e2$message)
    tibble(text = character())
  })
})

# Filter to post only new stuff
posts_new <- posts |>
  filter(!post_text %in% old_posts$text)

message(nrow(posts_new), " new post(s) to send.")

# Exit cleanly if nothing to post
if (nrow(posts_new) == 0) {
  message("No new posts to send. Exiting.")
  quit(status = 0)
}

## Part 4: Post skeets
for (i in seq_len(nrow(posts_new))) {
  tryCatch({
    post_skeet(
      text = posts_new$post_text[i],
      created_at = posts_new$timestamp[i],
      preview_card = FALSE
    )
    message("Successfully posted: ", posts_new$post_text[i])
  }, error = function(e) {
    message("Failed to post: ", posts_new$post_text[i])
    message("Full error details: ", toString(e))
    write(
      paste("Error posting:", posts_new$post_text[i], "Error:", toString(e)),
      file = "bluesky_post_errors.log",
      append = TRUE
    )
  })
}
