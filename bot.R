## Bot script adapted from https://github.com/JBGruber/r-bloggers-bluesky bot.r
# A.Bailey 2024-09-10
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

# Print package version for debugging
message("atrrr version: ", as.character(packageVersion("atrrr")))

## Part 1: read RSS feed
# Vector of Pubmed feeds from search terms: 
# immunopeptidom*[tiab]
# hdx-ms[tiab]
# immunopeptidom*[tiab] AND neoantigen*[tiab]
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

# Filter for Pubmed feed for keywords and publication of no earlier than last 30 days and trim link
pubmed_filt <- pubmed_df |> 
  filter(str_detect(item_title, "speech"),
         item_pub_date >= today() - 29) |> 
  mutate(link = str_extract(item_link,"^.*?[^?]*"))

# Filter posts for unique titles
rss_posts <- bind_rows(brv_filt |> select(item_title,item_description,link),
                      pubmed_filt |> select(item_title,item_description,link)) |> 
  distinct(item_title, .keep_all = T)  

## Part 2: create posts from feed using paper title and link
posts <- rss_posts |>
  mutate(post_text = glue("{item_title} {link}"), # Needs to be <300 characters
         timestamp = now()) # Add timestamp

## Part 3: get already posted updates and de-duplicate with enhanced error handling
Sys.setenv(BSKY_TOKEN = "papers_token.rds")
pw <- Sys.getenv("ATR_PW")

if (pw == "") {
  stop("ATR_PW environment variable is not set")
}

# Authentication with fallback options
auth_success <- FALSE
tryCatch({
  # First attempt with original domain
  auth(user = "speechpapers.bsky.social",
       password = pw,
       overwrite = TRUE)
  auth_success <- TRUE
}, error = function(e) {
  message("First authentication attempt failed, trying alternative...")
  tryCatch({
    # Second attempt without .bsky.social
    auth(user = "speechpapers",
         password = pw,
         overwrite = TRUE)
    auth_success <- TRUE
  }, error = function(e2) {
    message("Both authentication attempts failed")
    stop(e2)
  })
})

if (!auth_success) {
  stop("All authentication attempts failed")
}

# Check for existing posts with error handling
old_posts <- tryCatch({
  get_skeets_authored_by("speechpapers.bsky.social", limit = 5000L)
}, error = function(e) {
  message("Error getting existing posts: ", e$message)
  # Return empty dataframe with required structure if fetch fails
  data.frame(text = character(0))
})

# Filter to post only new stuff
posts_new <- posts |>
  filter(!post_text %in% old_posts$text)

## Part 4: Post skeets with enhanced error handling
for (i in seq_len(nrow(posts_new))) {
  max_retries <- 3
  retry_count <- 0
  post_success <- FALSE
  
  while (!post_success && retry_count < max_retries) {
    retry_count <- retry_count + 1
    
    tryCatch({
      resp <- post_skeet(
        text = posts_new$post_text[i],
        created_at = posts_new$timestamp[i],
        preview_card = FALSE
      )
      post_success <- TRUE
      message(sprintf("Successfully posted skeet %d of %d", i, nrow(posts_new)))
    }, error = function(e) {
      message(sprintf("Attempt %d failed for post %d: %s", 
                     retry_count, i, e$message))
      if (retry_count == max_retries) {
        warning(sprintf("Failed to post skeet %d after %d attempts", 
                       i, max_retries))
      }
      Sys.sleep(2)  # Wait 2 seconds before retrying
    })
  }
}
