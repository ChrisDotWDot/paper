## packages
library(tidyRSS)
library(atrrr)
library(anytime)
library(lubridate)
library(dplyr)
library(stringr)
library(glue)
library(purrr)
library(xml2)

## Part 1: read RSS feed

# Vector of Pubmed feeds from search terms: 
pubmed_feeds <- c("https://pubmed.ncbi.nlm.nih.gov/rss/search/1Vu-RtW34K22uP7FKBpdRmDjVSeiYxkTVLmNY43ot8d0uhoEAV/?limit=100&utm_campaign=pubmed-2&fc=20240913065033")

# Read all the PubMed feeds
pubmed_df <- map_df(pubmed_feeds, tidyfeed) 

# Vector of feeds of possible interest from bioRxiv, yields the last 30 days
brv_feeds <- c("http://connect.biorxiv.org/biorxiv_xml.php?subject=neuroscience")

# Read all the bioRxiv feeds
brv <- map_df(brv_feeds, tidyfeed)

# Filter for biorxiv feed keywords and trim the link
brv_filt <- brv |> 
  filter(str_detect(item_title, "Speech")) |> 
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
Sys.setenv(BSKY_TOKEN = "papers_token.rds")
pw <- Sys.getenv("BSKY_APP_PASSWORD")

auth(user = "speechpapers.bsky.social",
     password = pw,
     overwrite = TRUE)

# Check for existing posts
old_posts <- get_skeets_authored_by("speechpapers.bsky.social", limit = 5000L)

# Filter to post only new stuff
posts_new <- posts |>
  filter(!post_text %in% old_posts$text)

## Part 4: Post skeets
for (i in seq_len(nrow(posts_new))) {
  tryCatch({
    post_skeet(
      text = posts_new$post_text[i],
      created_at = posts_new$timestamp[i],
      preview_card = FALSE
    )
  }, error = function(e) {
    message("Failed to post: ", posts_new$post_text[i])
    message("Error: ", e$message)
  })
}
