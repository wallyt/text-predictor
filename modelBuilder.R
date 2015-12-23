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
    # Employing negative lookahead so apostrophes not subbed; Perl needed to make it work right
    new <- gsub("(?!')[[:punct:]]+", "", new, perl = T)
    new <- gsub("\\s\\s", " ", new)
    return(new)
}
UStweets <- baseClean(UStweets)
USblogs <- baseClean(USblogs)
USnews <- baseClean(USnews)

# To detect double words, with Perl=T: "\\b(\\S+?)\\1\\S*\\b" and then insert with \1
## TODO: MAKE THE DOUBLE CHECKER WORK

## TODO: MAKE PROFANITY FILTER WORK (try the textfile() function in quanteda
# Remove profanity
# From Shutterstock list at https://github.com/shutterstock/List-of-Dirty-Naughty-Obscene-and-Otherwise-Bad-Words
curseDict <- read.csv("profanities.csv", stringsAsFactor=F, quote = "")
curseDict <- as.vector(curseDict[,1])
curseDict <- paste0(' ', curseDict, ' ')

profanityClean <- function(text) {
    for(i in length(curseDict)) {
        new <- gsub(curseDict[i], "", text)
    }
    return(new)
}
UStweets <- profanityClean(UStweets)
USblogs <- profanityClean(USblogs)
USnews <- profanityClean(USnews)

######### Combine into corpus and generate ngrams
# Take random 10,000-line samples from each source and then combine into a corpus
set.seed(42); n <- USnews[sample(length(USnews), 10000)]
set.seed(42); b <- USblogs[sample(length(USblogs), 10000)]
set.seed(42); t <- UStweets[sample(length(UStweets), 10000)]
corpus <- corpus(c(n,b,t))

# Full corpus
#corpus <- corpus(c(USnews, USblogs, UStweets))


# Keywords in context, 3 words of context
#kwic(corpus, "love", 3)

dfm1 <- dfm(corpus, toLower = T, removePunct = F, removeTwitter = T, stem = F)
dfm2 <- dfm(corpus, toLower = T, removePunct = F, removeTwitter = T, stem = F, concatenator = " ", ngrams = 2)
dfm3 <- dfm(corpus, toLower = T, removePunct = F, removeTwitter = T, stem = F, concatenator = " ", ngrams = 3)
dfm4 <- dfm(corpus, toLower = T, removePunct = F, removeTwitter = T, stem = F, concatenator = " ", ngrams = 4)
topfeatures(dfm3, 20)  # 20 top words

# Another way to look at it is collocations (bigrams, trigrams)
colos <- collocations(corpus, method="all")

## TODO: EXPORT DFMs AS DATA FRAMES
df1 <- topfeatures(dfm1, dim(dfm1)[2])
df2 <- topfeatures(dfm2, dim(dfm2)[2])
df3 <- topfeatures(dfm3, dim(dfm3)[2])
df4 <- topfeatures(dfm4, dim(dfm4)[2])
df1 <- data.frame(word=names(df1), freq=df1, row.names = NULL)
df2 <- data.frame(word=names(df2), freq=df2, row.names = NULL)
df3 <- data.frame(word=names(df3), freq=df3, row.names = NULL)
df4 <- data.frame(word=names(df4), freq=df4, row.names = NULL)

############## Build models

input <- "it would mean the world"
