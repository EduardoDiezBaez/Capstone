## Necessary packages

library(tm)
library(qdap)
library(RWeka)

## Functions definitions

ReplaceAbbr <- function(x) {
    # Replaces every occurence of typical abbreviations in english (like "Mr.", "a.m.", "o.k.", ...)
    
    # Returns a corpus which is the result of the previous replacements
    
    x <- gsub("Mr\\.", "Mr", x)
    x <- gsub("Ms\\.", "Ms", x)
    x <- gsub("Mrs\\.", "Mrs", x)
    x <- gsub("i\\.e\\.", "ie", x)
    x <- gsub("A\\.D\\.", "AD", x)
    x <- gsub("B\\.C\\.", "BC", x)
    x <- gsub("A\\.M\\.", "am", x)
    x <- gsub("P\\.M\\.", "pm", x)
    x <- gsub("a\\.m\\.", "am", x)
    x <- gsub("p\\.m\\.", "pm", x)
    x <- gsub("et al\\.", "et al", x)
    x <- gsub("Jr\\.", "Jr", x)
    x <- gsub("Sr\\.", "Sr", x)
    x <- gsub("Dr\\.", "Dr", x)
    x <- gsub("Sen\\.", "Sen", x)
    x <- gsub("PhD\\.", "PhD", x)
    x <- gsub("U\\.S\\.", "US", x)
    x <- gsub("U\\.S\\.A\\.", "USA", x)
    x <- gsub("p\\.s\\.", "ps", x)
    x <- gsub("o\\.k\\.", "ok", x)
    x <- gsub("w\\/", "with", x)
    x <- gsub("\\&", "and", x)
    x <- gsub("\\$", " dollar ", x)
    x <- gsub("m\\.r\\.i\\.", "mri", x)
    x <- gsub("\\/", " ", x)
    
    return(x)
}


RemoveDecimals <- function(x) gsub("([0-9]*)\\.([0-9]+)", "\\1 \\2", x)
# Removes every occurence of dots in decimal numbers in the corpus

# Returns a corpus which is the result of the previous removals


RemoveEmail <- function(x) gsub("[A-z0-9._%+-]+@[A-z0-9.-]+\\.(?:[A-z]{2}|com|org|net|edu|gov|mil|biz|info|mobi|name|aero|asia|jobs|museum)", "", x)
# Removes every occurence of email addresses in the corpus

# Returns a corpus which is the result of the previous removals


RemoveURL <- function(x) sub("((https?|ftp)://)?www\\.[A-z0-9]+\\.[A-z0.9]{2,}", "", x)
# Removes every occurence of URLs addresses in the corpus

# Returns a corpus which is the result of the previous removals


RemoveLRSQuotation <- function(x) gsub("\u2018|\u2019", "\'", x)
# Replaces every occurence of single left and right quotation marks with "\'" in the corpus

# Returns a corpus which is the result of the previous replacements


RemoveLRDQuotation <- function(x) gsub("\u201c|\u201d", "\"", x)
# Replaces every occurence of double left and right quotation marks with "\'" in the corpus

# Returns a corpus which is the result of the previous replacements


#RemoveHashtags <- function(x) gsub("#[A-z0-9]+", "", x)

RemoveDashes <- function(x) gsub("-+|—", " ", x)
# Removes every occurence of of one or more dashes in the corpus

# Returns a corpus which is the result of the previous removals


TransformIs <- function(x) {
    # Transforms every occurence of low case "i'" with the equivalent "I'"
    
    # Returns a corpus which is the result of the previous transformations
        
    x <- gsub("^i ", "I ", x)
    x <- gsub("^i'", "I'", x)
    x <- gsub(" i ", " I ", x)
    x <- gsub(" i'", " I'", x)
    x <- gsub(".i ", ". I ", x)
    x <- gsub(".i'", ". I'", x)
    
    return(x)
}


TransformDots <- function(x) gsub("[:?!|…]+", ".", x)
# Transforms every occurence of sentence termination marks a dot in the corpus

# Returns a corpus which is the result of the previous transformations


RemovePunc <- function(x) {
    # Removes every occurence of punctuation marks in the corpus
    
    # Returns a corpus which is the result of the previous removals
        
    x <- gsub('[])(,;#%$^*\\~{}[&+=@/"`|<>_]+', "", x)
    x <- gsub("\\'", "", x)
    
    return(x)
}

SplitSentence <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 1, delimiters = "\\r\\n\\t.:?!|…"))
# Splits every sentence according to termination marks in the corpus

# Returns a corpus which is the result of the sentence splitting


RemoveTrailingSpaces <- function(x) gsub("^\\s|\\s$", "", x)
# Removes every occurence of trailing spaces in the corpus

# Returns a corpus which is the result of the previous removals


RemoveEmptyStrings <- function(x) {
    # Removes every empty string in the corpus
    
    # Returns a corpus which is the result of the previous removals
    
    return(x[x != ""])
}


CleanCorpus <- function(x) {
    # Clean the corpus by applying the previous functions
    
    # Returns a corpus which is the result of the previous steps
    
    x <- tm_map(x, content_transformer(ReplaceAbbr))
    x <- tm_map(x, content_transformer(RemoveDecimals))
    x <- tm_map(x, content_transformer(removeNumbers))
    x <- tm_map(x, content_transformer(RemoveEmail))
    x <- tm_map(x, content_transformer(RemoveURL))
    x <- tm_map(x, content_transformer(RemoveLRSQuotation))
    x <- tm_map(x, content_transformer(RemoveLRDQuotation))
    x <- tm_map(x, content_transformer(RemoveHashtags))
    x <- tm_map(x, content_transformer(RemoveDashes))
    x <- tm_map(x, content_transformer(tolower))
    x <- tm_map(x, content_transformer(TransformIs))
    x <- tm_map(x, content_transformer(iconv), from = "latin1", to = "ASCII", sub = "")
    x <- tm_map(x, content_transformer(TransformDots))
    x <- tm_map(x, content_transformer(RemovePunc))
    x <- tm_map(x, stripWhitespace)
    x <- tm_map(x, content_transformer(SplitSentence))
    x <- tm_map(x, content_transformer(RemoveTrailingSpaces))
    x <- tm_map(x, content_transformer(RemoveEmptyStrings))
    x <- tm_map(x, content_transformer(unique))
    
    return(x)
}

## Corpus creation (package "tm")

dir <- DirSource("final/en_US/", encoding = "UTF-8")
corpus <- Corpus(dir, readerControl = list(reader = readPlain))

## Extracting a random sample of the corpus

set.seed(1537)
sample1 <- sample(1:length(corpus[[1]]$content), length(corpus[[1]]$content) / 10000)
blogs <- corpus[[1]]$content[sample1]
write(blogs, file = "final/en_US/sample/en_US.blogs.sample.txt")
sample2 <- sample(1:length(corpus[[2]]$content), length(corpus[[2]]$content) / 10000)
news <- corpus[[2]]$content[sample2]
write(news, file = "final/en_US/sample/en_US.news.sample.txt")
sample3 <- sample(1:length(corpus[[3]]$content), length(corpus[[3]]$content) / 10000)
twitter <- corpus[[3]]$content[sample3]
write(twitter, file = "final/en_US/sample/en_US.twitter.sample.txt")
dir.source <- DirSource("final/en_US/sample/", encoding = "UTF-8")
corpus.sample <- Corpus(dir.source, readerControl = list(reader = readPlain))

## Corpus cleaning

clean.corpus <- CleanCorpus(corpus.sample)
