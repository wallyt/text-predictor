# modelBuilder.R processes the corpus and builds the model(s) to be used for the predictive Shiny app

# Setup
local <- TRUE
ifelse(local, setwd("~/Documents/DataScience/Johns Hopkins/Capstone"), setwd("./r-projects/Capstone")); rm(local)

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
ensurePkg("data.table")

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
    new <- gsub("[^[:print:]]", "", new)
    return(new)
}
UStweets <- baseClean(UStweets)
USblogs <- baseClean(USblogs)
USnews <- baseClean(USnews)

############ Remove profanity
# From Shutterstock list at https://github.com/shutterstock/List-of-Dirty-Naughty-Obscene-and-Otherwise-Bad-Words
curseDict <- read.csv("profanities.csv", stringsAsFactor=F, quote = "")
curseDict <- as.vector(curseDict[,1])
curseDict <- paste0(' ', curseDict, ' ')

profanityClean <- function(text) {
    for(i in 1:length(curseDict)) {
        print(i)
        new <- gsub(curseDict[i], "", text)
    }
    return(new)
}
UStweets <- profanityClean(UStweets)
USblogs <- profanityClean(USblogs)
USnews <- profanityClean(USnews)

## TODO: MAKE THE DOUBLE CHECKER WORK
# To detect double words, with Perl=T: "\\b(\\S+?)\\1\\S*\\b" and then insert with \1

######### Combine into corpus and generate ngrams
# Take random 20,000-line samples from each source and then combine into a corpus
# set.seed(42); n <- USnews[sample(length(USnews), 20000)]
# set.seed(42); b <- USblogs[sample(length(USblogs), 20000)]
# set.seed(42); t <- UStweets[sample(length(UStweets), 20000)]
# corpus <- corpus(c(n,b,t))

# Full corpus
corpus <- corpus(c(USnews, USblogs, UStweets))
rm(c(USnews, USblogs, UStweets))

# Keywords in context, 3 words of context
#kwic(corpus, "love", 3)

dfm1 <- dfm(corpus, toLower = T, removePunct = F, removeTwitter = T, stem = F)
dfm2 <- dfm(corpus, toLower = T, removePunct = F, removeTwitter = T, stem = F, concatenator = " ", ngrams = 2)
dfm3 <- dfm(corpus, toLower = T, removePunct = F, removeTwitter = T, stem = F, concatenator = " ", ngrams = 3)
dfm4 <- dfm(corpus, toLower = T, removePunct = F, removeTwitter = T, stem = F, concatenator = " ", ngrams = 4)
topfeatures(dfm4, 20)  # 20 top words
rm(corpus)

dfm2 <- trim(dfm2, minDoc = 6)

# Save the data
save(dfm1, file="data/dfm1.RData")
save(dfm2, file="data/dfm2.RData")
save(dfm3, file="data/dfm3.RData")
save(dfm4, file="data/dfm4.RData")

# Load the data
load("data/dfm1.RData")
load("data/dfm2.RData")
load("data/dfm3.RData")
load("data/dfm4.RData")

# Extract the features for data table creation
tf1 <- topfeatures(dfm1, dim(dfm1)[2])
tf2 <- topfeatures(dfm2, dim(dfm2)[2])
tf3 <- topfeatures(dfm3, dim(dfm3)[2])
tf4 <- topfeatures(dfm4, dim(dfm4)[2])

# Another way to look at it is collocations (bigrams, trigrams)
colos <- collocations(corpus, method="all")

## Export DFMs as data tables
dt1 <- data.table(word=names(tf1), freq=tf1)
dt2 <- data.table(word=names(tf2), freq=tf2)
dt3 <- data.table(word=names(tf3), freq=tf3)
dt4 <- data.table(word=names(tf4), freq=tf4)


# Goal is something like:
# key              freq     value
# thanks for the   612      support
# thanks for the   380      drink
# thanks for the   215      payment
# thanks for the    27      encouragement

# And to pull out just the top two of each combination:
# answer <- setorder(testTable[ave(-testTable$C, testTable$A, FUN=rank) <=2, ], A, -C)

# Additional ngrams, if needed: http://www.ngrams.info/samples_coca1.asp

# Assuming that myTable starts out as a data table with a single variable containing all 
# individual occurrences of phrases that interest you (e.g. of length 4 words, say) then these 
# 2 lines will give you myTable containing only unique instances of all phrases with a second 
# variable which is the frequency count for each unique phrase.
# myTable <- myTable[ ,.N, by=list(V1)]
# setnames(myTable, 1:2, c("phrase","freq"))

############## Build models


input <- "and a case of"
input <- tokenize(input, ngrams = 1:3, concatenator = " ", simplify = T)
l <- length(input)
dt4[grep(input[l], word)]











