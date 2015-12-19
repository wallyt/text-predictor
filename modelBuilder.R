# modelBuilder.R processes the corpus and builds the model(s) to be used for the predictive Shiny app

# Setup
local <- TRUE
ifelse(local, setwd("~/Documents/DataScience/Capstone"), setwd("./r-projects/Capstone")); rm(local)

ensurePkg <- function(x) {
    if (!require(x,character.only = TRUE)) {
        install.packages(x,dep=TRUE, repos="http://cran.r-project.org")
        if(!require(x,character.only = TRUE)) stop("Package not found")
    }
}

# ensurePkg("tm")
# ensurePkg("dplyr")
# ensurePkg("stringi")
# ensurePkg("slam")
ensurePkg("quanteda")

USnews <- readLines("corpus/final/en_US/en_US.news.txt", skipNul = TRUE)
USblogs <- readLines("corpus/final/en_US/en_US.blogs.txt", skipNul = TRUE)
UStweets <- readLines("corpus/final/en_US/en_US.twitter.txt", skipNul = TRUE)


########### Process out numbers, extra whitespaces, punctuation, etc.
baseClean <- function(text) {
    new <- gsub(" @[a-zA-z]+| #[a-zA-z]+", "", text)
    new <- gsub("[[:digit:]]+", "", new)
    new <- gsub("[[:punct:]^']+", "", new)
    return(new)
}
UStweets <- baseClean(UStweets)
USblogs <- baseClean(USblogs)
USnews <- baseClean(USnews)



######### Combine into corpus
# Take random 10,000-line samples from each source and then combine into a corpus
set.seed(42); n <- USnews[sample(length(USnews), 10000)]
set.seed(42); b <- USblogs[sample(length(USblogs), 10000)]
set.seed(42); t <- UStweets[sample(length(UStweets), 10000)]
corpus <- corpus(c(n,b,t))

#tolower, strip whitespace and de-profane
# From Shutterstock list at https://github.com/shutterstock/List-of-Dirty-Naughty-Obscene-and-Otherwise-Bad-Words
curseDict <- read.csv("profanities.csv", stringsAsFactor=F)

dfm1 <- dfm(corpus, concatenator = " ", verbose = F, ngrams = 1)
dfm2 <- dfm(corpus, concatenator = " ", verbose = F, ngrams = 2)
dfm3 <- dfm(corpus, concatenator = " ", verbose = F, ngrams = 3)
topfeatures(dfm, 200)  # 20 top words

#or\


corpus <- Corpus(VectorSource(c(n,b,t)))


corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, removeWords, as.matrix(curseDict))
dtm <- DocumentTermMatrix(corpus, control = list(minWordLength = 2))
