# scripts/citation_classics.R
# Posts highly-cited speech science papers bi-weekly

library(atrrr)
library(dplyr)
library(readr)
library(glue)
library(lubridate)
library(stringr)
library(httr)
library(jsonlite)

# Initialize logging
log_file <- "citation_classics.log"
log_message <- function(msg) {
  write(paste(now(), "-", msg), file = log_file, append = TRUE)
  message(msg)
}

log_message("=== Starting Citation Classics ===")

# Check if this is a bi-weekly Thursday (run every other week)
#week_num <- isoweek(today())
#if (week_num %% 2 != 0) {
 # log_message("Not a bi-weekly posting week - skipping")
 # quit(save = "no", status = 0)
#}

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

# Check if files exist
if (!file.exists(classics_file)) {
  log_message("ERROR: Classics file not found. Creating sample file...")
  
  # Create sample content with curated classic papers
  sample_classics <- tibble(
    id = 1:10,
    title = c(
      "Acoustic Theory of Speech Production",
      "Theory of Speech Perception",
      "A Mathematical Theory of Communication",
      "Distinctive Features and Phonological Opposition",
      "The Motor Theory of Speech Perception",
      "Coarticulation and Connected Speech Processes",
      "Source-Filter Theory",
      "Speech Perception by the Human Brain",
      "Hidden Markov Models for Speech Recognition",
      "Articulatory Phonology"
    ),
    authors = c(
      "Gunnar Fant",
      "Alvin Liberman",
      "Claude Shannon",
      "Roman Jakobson",
      "Liberman & Mattingly",
      "Patricia Keating",
      "Gunnar Fant",
      "David Poeppel",
      "Lawrence Rabiner",
      "Catherine Browman & Louis Goldstein"
    ),
    year = c(
      1960,
      1957,
      1948,
      1951,
      1985,
      1990,
      1960,
      2003,
      1989,
      1986
    ),
    citations_approx = c(
      8500,
      5200,
      15000,
      12000,
      3400,
      2100,
      8500,
      1800,
      25000,
      2800
    ),
    significance = c(
      "This paper laid the foundation for how we understand speech production today. The source-filter model explains how voice and vocal tract shape combine to create speech sounds. Every voice synthesis system uses these principles!",
      "Introduced the concept of categorical perception in speech - the idea that we perceive continuous acoustic signals as discrete phonetic categories. This revolutionized our understanding of how humans process speech sounds.",
      "While not speech-specific, Shannon's information theory provided the mathematical foundation for understanding communication systems, including speech transmission and coding. Still fundamental to all digital communication today.",
      "Established distinctive features as the building blocks of phonology. This framework influenced decades of linguistic theory and speech recognition systems by showing how sounds can be decomposed into binary features.",
      "Proposed that speech perception involves motor simulation - we understand speech by mentally simulating how we would produce those sounds. Still debated today, but hugely influential in speech science.",
      "Systematically described how speech sounds overlap and influence each other in connected speech. Essential for understanding the difference between isolated sounds and natural, fluent speech production.",
      "Fant's source-filter theory separated the sound source (vocal folds) from the filter (vocal tract). This elegant model is still the basis of speech synthesis and analysis 60+ years later.",
      "Integrated neuroscience with speech perception, proposing dual processing streams in the brain. Changed how we think about the neural basis of language and speech processing.",
      "Provided the statistical framework that revolutionized speech recognition. HMMs enabled the first practical large-vocabulary speech recognition systems and dominated the field for decades.",
      "Introduced articulatory phonology as a dynamic approach to speech production. Rather than static positions, speech involves coordinated gestural patterns - a paradigm shift in phonology."
    ),
    url = c(
      "https://www.semanticscholar.org/paper/Acoustic-Theory-of-Speech-Production-Fant/",
      "https://www.semanticscholar.org/paper/Speech-Perception-Liberman/",
      "https://ieeexplore.ieee.org/document/6773024",
      "https://www.semanticscholar.org/paper/Distinctive-Features-Jakobson/",
      "https://www.semanticscholar.org/paper/Motor-Theory-Liberman-Mattingly/",
      "https://www.semanticscholar.org/paper/Coarticulation-Keating/",
      "https://www.semanticscholar.org/paper/Source-Filter-Fant/",
      "https://www.semanticscholar.org/paper/Speech-Perception-Poeppel/",
      "https://ieeexplore.ieee.org/document/18626",
      "https://www.semanticscholar.org/paper/Articulatory-Phonology-Browman-Goldstein/"
    ),
    field = c(
      "Production", "Perception", "Information", "Phonology", "Perception",
      "Production", "Production", "Neuroscience", "Technology", "Phonology"
    )
  )
  
  dir.create("content/classics", recursive = TRUE, showWarnings = FALSE)
  write_csv(sample_classics, classics_file)
  log_message("Sample classics file created")
}

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

# Create post content - start with a shorter version
# Calculate available space for significance text
title_part <- glue("\"{today_classic$title}\"")
meta_part <- glue("{today_classic$authors} ({today_classic$year})")
citations_part <- glue("Citations: {format(today_classic$citations_approx, big.mark = ',')}+")
url_part <- glue("🔗 {today_classic$url}")
hashtag_part <- "#SpeechScience"

# Calculate overhead (emojis, line breaks, static text)
overhead <- nchar("📚 Citation Classic\n\n\n\n\n\n\n\n") + 
            nchar(title_part) + 
            nchar(meta_part) + 
            nchar(citations_part) + 
            nchar(url_part) + 
            nchar(hashtag_part)

# Available space for significance (leave buffer for emoji grapheme counting)
available_chars <- 250 - overhead

# Truncate significance if needed
significance_text <- if (nchar(today_classic$significance) > available_chars) {
  str_trunc(today_classic$significance, available_chars, ellipsis = "...")
} else {
  today_classic$significance
}

post_text <- glue("📚 Citation Classic

\"{today_classic$title}\"
{today_classic$authors} ({today_classic$year})
Citations: {format(today_classic$citations_approx, big.mark = ',')}+

{significance_text}

🔗 {today_classic$url}

#SpeechScience")

log_message(paste("Post length:", nchar(post_text), "characters"))

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
