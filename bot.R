name: "Update Bot"
on:
  schedule:
    - cron: '0 1,13 * * *'
  push:
    branches:
      - main
  pull_request:
    branches:
      - main
  workflow_dispatch:  # Added manual trigger option for testing

jobs:
  blog-updates:
    name: bot
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      - uses: r-lib/actions/setup-r@v2
        with:
          r-version: '4.3.2'
      - name: Install system dependencies
        run: |
          sudo apt-get update
          sudo apt-get install -y --no-install-recommends \
            libcurl4-openssl-dev \
            libssl-dev \
            libxml2-dev \
            libgit2-dev \
            build-essential
      - name: Install packages
        env:
          GITHUB_PAT: ${{ secrets.GITHUB_TOKEN }}
          R_REMOTES_NO_ERRORS_FROM_WARNINGS: true
        run: |
          # First set the token in R
          Sys.setenv(GITHUB_PAT = Sys.getenv("GITHUB_PAT"))
          
          # Install pak
          install.packages("pak", repos = "https://cloud.r-project.org")
          
          # Install all packages including GitHub package
          pak::pkg_install(c(
            "Rcpp",
            "jsonlite",
            "dplyr",
            "renv",
            "tidyRSS",
            "lubridate",
            "stringr",
            "glue",
            "purrr",
            "xml2",
            "JBGruber/atrrr"
          ), dependencies = TRUE)
        shell: Rscript {0}
      
      - name: Run Bluesky bot
        env:
          GITHUB_PAT: ${{ secrets.GITHUB_TOKEN }}
          ATR_PW: ${{ secrets.ATR_PW }}
          R_REMOTES_NO_ERRORS_FROM_WARNINGS: true
        run: |
          # Create script file
          cat > feed-bluesky.R << 'EOF'
          ## packages
          library(tidyRSS)
          library(atrrr)
          library(lubridate)
          library(dplyr)
          library(stringr)
          library(glue)
          library(purrr)
          library(xml2)

          # Log start of script
          message("Script started at: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))

          ## Part 1: read RSS feed

          # Vector of Pubmed feeds from search terms: 
          pubmed_feeds <- c("https://pubmed.ncbi.nlm.nih.gov/rss/search/1H9AR3ZQQCaH18XJmZjvELYBadSZGaorxW0NnaqVNWn8GIEkJ4/?limit=15&utm_campaign=pubmed-2&fc=20250125192025")

          # Read all the PubMed feeds
          message("Fetching PubMed feeds...")
          pubmed_df <- map_df(pubmed_feeds, tidyfeed) 
          message("PubMed feeds fetched. Rows: ", nrow(pubmed_df))

          # Vector of feeds of possible interest from bioRxiv, yields the last 30 days
          brv_feeds <- c("http://connect.biorxiv.org/biorxiv_xml.php?subject=neuroscience")

          # Read all the bioRxiv feeds
          message("Fetching bioRxiv feeds...")
          brv <- map_df(brv_feeds, tidyfeed)
          message("bioRxiv feeds fetched. Rows: ", nrow(brv))

          # Filter for biorxiv feed keywords and trim the link
          brv_filt <- brv |> 
            filter(str_detect(item_title, "speech")) |> 
            mutate(link = str_extract(item_link,"^.*?[^?]*"))
          message("bioRxiv filtered to ", nrow(brv_filt), " rows")

          # Filter for Pubmed feed keywords, recent publications, and trim link
          pubmed_filt <- pubmed_df |> 
            filter(str_detect(item_title, "speech"),
                  item_pub_date >= today() - 29) |> 
            mutate(link = str_extract(item_link,"^.*?[^?]*"))
          message("PubMed filtered to ", nrow(pubmed_filt), " rows")

          # Filter posts for unique titles
          rss_posts <- bind_rows(brv_filt |> select(item_title, item_description, link),
                                pubmed_filt |> select(item_title, item_description, link)) |> 
            distinct(item_title, .keep_all = TRUE)
          message("Combined unique posts: ", nrow(rss_posts))

          ## Part 2: Create posts from feed
          posts <- rss_posts |> 
            mutate(post_text = glue("{item_title} {link}"), # Needs to be <300 characters
                  timestamp = now()) # Add timestamp
          message("Prepared ", nrow(posts), " potential posts")

          if (nrow(posts) > 0) {
            message("Sample post: ", substr(posts$post_text[1], 1, 50), "...")
          }

          ## Part 3: Authentication
          # Load the app password from environment variable
          pw <- Sys.getenv("ATR_PW")
          message("Password retrieved, length: ", nchar(pw), " characters")

          # Authenticate to Bluesky
          tryCatch({
            message("Authenticating to Bluesky...")
            auth(user = "speechpapers.bsky.social",
                password = pw,
                overwrite = TRUE)
            message("Authentication successful")
            
            # Check for existing posts to avoid duplicates
            message("Retrieving existing posts...")
            old_posts <- get_skeets_authored_by("speechpapers.bsky.social", limit = 5000L)
            message("Retrieved ", nrow(old_posts), " existing posts")

            # Filter to post only new stuff
            posts_new <- posts |>
              filter(!post_text %in% old_posts$text)
            message("Found ", nrow(posts_new), " new posts to publish")

            ## Part 4: Post skeets
            if (nrow(posts_new) > 0) {
              for (i in seq_len(nrow(posts_new))) {
                message("Posting item ", i, " of ", nrow(posts_new), "...")
                tryCatch({
                  message("Post text: ", posts_new$post_text[i])
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
                  
                  # Log errors to a file
                  write(paste("Error posting:", posts_new$post_text[i], "Error:", toString(e)), 
                        file = "bluesky_post_errors.log", append = TRUE)
                })
                # Small delay between posts to avoid rate limiting
                Sys.sleep(2)
              }
            } else {
              message("No new posts to publish")
            }
          }, error = function(e) {
            message("ERROR: ", conditionMessage(e))
            print(e)
          })

          message("Script completed at: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
          EOF
          
          # Run the script and capture output
          Rscript feed-bluesky.R > bluesky_output.log 2>&1
          
          # Display the log
          cat bluesky_output.log
          
      - name: Upload logs as artifacts
        uses: actions/upload-artifact@v4
        with:
          name: bluesky-logs
          path: |
            bluesky_output.log
            bluesky_post_errors.log
          if-no-files-found: ignore
