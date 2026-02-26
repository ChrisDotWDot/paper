Could you update this whole script
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
# Vector of Pubmed feeds from search terms: 
pubmed_feeds <- c("https://pubmed.ncbi.nlm.nih.gov/rss/search/1H9AR3ZQQCaH18XJmZjvELYBadSZGaorxW0NnaqVNWn8GIEkJ4/?limit=15&utm_campaign=pubmed-2&fc=20250125192025")
# Read all the PubMed feeds
pubmed_df <- map_df(pubmed_feeds, tidyfeed) 
# Vector of feeds of possible interest from bioRxiv, yields the last 30 days
brv_feeds <- c("http://connect.biorxiv.org/biorxiv_xml.php?subject=neuroscience")
# Read all the bioRxiv feeds with error handling
brv <- tryCatch({
  map_df(brv_feeds, tidyfeed)
}, error = function(e) {
  message("bioRxiv feed unavailable, skipping: ", e$message)
  tibble(item_title = character(), item_description = character(), item_link = character())
})
# Filter for biorxiv feed keywords and trim the link
brv_filt <- brv |> 
  filter(str_detect(item_title, "speech")) |> 
  mutate(link = str_extract(item_link,"^.*?[^?]*"))
# Filter for Pubmed feed keywords, recent publications, and trim link
pubmed_filt <- pubmed_df |> 
  filter(str_detect(item_title, "speech"),
         item_pub_date >= today() - 29) |> 
  mutate(link = str_extract(item_link,"^.*?[^?]*"))
# Filter posts for unique titles
rss_posts <- bind_rows(brv_filt |> select(item_title, item_description, link),
                       pubmed_filt |> select(item_title, item_description, link)) |> 
  distinct(item_title, .keep_all = TRUE)
## Part 2: Create posts from feed
posts <- rss_posts |> 
  mutate(post_text = glue("{item_title} {link}"), # Needs to be <300 characters
         timestamp = now()) # Add timestamp
## Part 3: Authentication
# Load the app password from environment variable
pw <- Sys.getenv("ATR_PW")
# Authenticate to Bluesky
auth(user = "speechpapers.bsky.social",
     password = pw,
     overwrite = TRUE)
# Check for existing posts to avoid duplicates
old_posts <- tryCatch({
  get_skeets_authored_by("speechpapers.bsky.social", limit = 500L)
}, error = function(e) {
  message("Could not fetch old posts, checking last 100")
  get_skeets_authored_by("speechpapers.bsky.social", limit = 100L)
})
# Filter to post only new stuff
posts_new <- posts |>
  filter(!post_text %in% old_posts$text)
## Part 4: Post skeets
for (i in seq_len(nrow(posts_new))) {
  tryCatch({
    result <- post_skeet(
      text = posts_new$post_text[i],
      created_at = posts_new$timestamp[i],
      preview_card = FALSE
    )
    # Log successful posts
    message("Successfully posted: ", posts_new$post_text[i])
  }, error = function(e) {
    # Detailed error logging
    message("Failed to post: ", posts_new$post_text[i])
    message("Full error details: ", toString(e))
    
    # Optional: Log errors to a file
    write(paste("Error posting:", posts_new$post_text[i], "Error:", toString(e)), 
          file = "bluesky_post_errors.log", append = TRUE)
  })
}
