# modelBuilder.R processes the corpus and builds the model(s) to be used for the predictive Shiny app

# Setup
local <- TRUE
ifelse(local, setwd("~/Documents/DataScience/Capstone"), setwd("./r-projects/Capstone"))

ensurePkg <- function(x) {
    if (!require(x,character.only = TRUE)) {
        install.packages(x,dep=TRUE, repos="http://cran.r-project.org")
        if(!require(x,character.only = TRUE)) stop("Package not found")
    }
}

ensurePkg("tm")
ensurePkg("dplyr")
ensurePkg("stringi")
ensurePkg("slam")
ensurePkg("quanteda")

USnews <- readLines("corpus/final/en_US/en_US.news.txt", skipNul = TRUE)
USblogs <- readLines("corpus/final/en_US/en_US.blogs.txt", skipNul = TRUE)
UStweets <- readLines("corpus/final/en_US/en_US.twitter.txt", skipNul = TRUE)

# Process out hashtags, email addresses and similar items

# Process out numbers, extra whitespaces, punctuation, etc.

# Combine into corpus
corpus <- corpus(c(USnews, USblogs, UStweets))
dfm <- dfm(corpus, concatenator = " ", verbose = F, ngrams = 1:3)
topfeatures(mydfm, 20)  # 20 top words
