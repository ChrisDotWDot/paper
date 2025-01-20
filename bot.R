## Bot script adapted from https://github.com/JBGruber/r-bloggers-bluesky bot.r
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
pubmed_feeds <- c("https://pubmed.ncbi.nlm.nih.gov/rss/search/1Vu-RtW34K22uP7FKBpdRmDjVSeiYxkTVLmNY43ot8d0uhoEAV/?limit=100&utm_campaign=pubmed-2&fc=20240913065033")
pubmed_df <- map_df(pubmed_feeds, tidyfeed) 

brv_feeds <- c("http://connect.biorxiv.org/biorxiv_xml.php?subject=neuroscience")
brv <- map_df(brv_feeds, tidyfeed)

brv_filt <- brv |> 
  filter(str_detect(item_title, "Speech")) |> 
  mutate(link = str_extract(item_link,"^.*?[^?]*"))

pubmed_filt <- pubmed_df |> 
  filter(str_detect(item_title, "speech"),
         item_pub_date >= today() - 29) |> 
  mutate(link = str_extract(item_link,"^.*?[^?]*"))

rss_posts <- bind_rows(brv_filt |> select(item_title,item_description,link),
                      pubmed_filt |> select(item_title,item_description,link)) |> 
  distinct(item_title, .keep_all = T)  

## Part 2: create posts
posts <- rss_posts |>
  mutate(post_text = glue("{item_title} {link}"),
         timestamp = now())

## Part 3: authenticate and check existing posts
pw <- Sys.getenv("ATR_PW")
if (pw == "") stop("ATR_PW environment variable is not set")

tryCatch({
  atrrr::post_auth(
    handle = "speechpapers.bsky.social",
    password = pw
  )
  message("Authentication successful")
}, error = function(e) {
  message("Authentication error: ", conditionMessage(e))
  stop(e)
})

old_posts <- tryCatch({
  posts_by("speechpapers.bsky.social", limit = 5000L)
}, error = function(e) {
  message("Error getting existing posts: ", e$message)
  data.frame(text = character(0))
})

posts_new <- posts |>
  filter(!post_text %in% old_posts$text)

## Part 4: Post content
for (i in seq_len(nrow(posts_new))) {
  tryCatch({
    post(
      text = posts_new$post_text[i],
      created_at = posts_new$timestamp[i]
    )
    message(sprintf("Successfully posted %d of %d", i, nrow(posts_new)))
    Sys.sleep(2)
  }, error = function(e) {
    message(sprintf("Failed to post %d: %s", i, e$message))
  })
}
