# scripts/sunday_deepdive.R
# Posts educational content about speech science every Sunday

library(atrrr)
library(dplyr)
library(readr)
library(glue)
library(lubridate)
library(stringr)

# Initialize logging
log_file <- "sunday_deepdive.log"
log_message <- function(msg) {
  write(paste(now(), "-", msg), file = log_file, append = TRUE)
  message(msg)
}

log_message("=== Starting Sunday Deep Dive ===")

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

# Load curated content
facts_file <- "content/educational/speech-history-facts.csv"
used_file <- "content/educational/used-facts.csv"

# Check if files exist
if (!file.exists(facts_file)) {
  log_message("ERROR: Facts file not found. Creating sample file...")
  
  # Create sample content
  sample_facts <- tibble(
    id = 1:5,
    title = c(
      "The First Speech Synthesizer",
      "The Discovery of Formants",
      "The Peterson-Barney Study",
      "The Birth of the Spectrogram",
      "McGurk Effect Discovery"
    ),
    content = c(
      "In 1779, Christian Kratzenstein built one of the first speech synthesizers - a set of resonating tubes that could produce vowel sounds when air was blown through them. It was remarkably accurate for its time!",
      "In the 1950s, researchers discovered that vowels could be characterized by their formant frequencies - specific resonances in the vocal tract. This insight revolutionized both speech science and synthesis.",
      "The Peterson & Barney (1952) study measured formant frequencies from 76 speakers. Their vowel charts are STILL used today to teach phonetics, over 70 years later!",
      "During WWII, engineers at Bell Labs developed the sound spectrograph to analyze encrypted speech. After the war, it became the speech scientist's most important tool for visualizing sound.",
      "In 1976, Harry McGurk discovered that what we SEE affects what we HEAR. When watching someone say 'ga' while hearing 'ba', most people perceive 'da'. Vision and hearing integrate automatically!"
    ),
    source = c(
      "🔗 More: en.wikipedia.org/wiki/Speech_synthesis#History",
      "📚 Key paper: Potter et al. (1947) 'Visible Speech'",
      "📊 Original study: Peterson & Barney, JASA 1952",
      "🎙️ Learn more: antiqueradio.org/soundspec.htm",
      "🎥 Watch it yourself: youtube.com/watch?v=G-lN8vWm3m0"
    ),
    category = c("Technology", "Discovery", "Methods", "Technology", "Perception")
  )
  
  dir.create("content/educational", recursive = TRUE, showWarnings = FALSE)
  write_csv(sample_facts, facts_file)
  log_message("Sample facts file created")
}

# Load facts
facts <- read_csv(facts_file, show_col_types = FALSE)
log_message(paste("Loaded", nrow(facts), "facts"))

# Load or create used facts tracker
if (file.exists(used_file)) {
  used_facts <- read_csv(used_file, show_col_types = FALSE)
  log_message(paste("Loaded", nrow(used_facts), "used facts"))
} else {
  used_facts <- tibble(id = integer(), date_posted = Date())
  write_csv(used_facts, used_file)
  log_message("Created new used facts tracker")
}

# Get available facts
available <- facts %>% 
  anti_join(used_facts, by = "id")

# Reset if all facts used
if (nrow(available) == 0) {
  log_message("All facts used - resetting tracker")
  used_facts <- tibble(id = integer(), date_posted = Date())
  available <- facts
  write_csv(used_facts, used_file)
}

# Select random fact
set.seed(as.numeric(Sys.Date()))  # Reproducible for the day
today_fact <- sample_n(available, 1)
log_message(paste("Selected fact ID:", today_fact$id, "-", today_fact$title))

# Create post content
intro_text <- glue("🔬 Speech Science Sunday

{today_fact$title}

A thread 🧵")

detail_text <- glue("{today_fact$content}

{today_fact$source}

#SpeechScience #{today_fact$category}")

# Post to Bluesky
tryCatch({
# Post intro
log_message("Posting intro...")
intro_post <- post_skeet(
  text = intro_text,
  created_at = now(),
  preview_card = FALSE
)
  
  Sys.sleep(2)  # Brief pause between posts
  
# Post details as reply
log_message("Posting details as reply...")
detail_post <- post_skeet(
  text = detail_text,
  in_reply_to = intro_post$uri,
  created_at = now(),
  preview_card = FALSE
)
  
  log_message("Posts successful!")
  
  # Mark fact as used
  used_facts <- bind_rows(
    used_facts,
    tibble(id = today_fact$id, date_posted = today())
  )
  write_csv(used_facts, used_file)
  log_message("Updated tracking file")
  
}, error = function(e) {
  log_message(paste("ERROR posting:", toString(e)))
  stop(e)
})

log_message("=== Sunday Deep Dive Complete ===")
