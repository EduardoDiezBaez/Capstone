# Packages loading

library(ggplot2)
library(wordcloud)

# 1. Document Term Matrix for unigrams

preproc.pruned <- tm_map(preproc, removeWords, stopwords("english"))
#sampleTDM.unigrams <- TermDocumentMatrix(preproc.pruned)
sampleTDM.onegrams <- TermDocumentMatrix(preproc.pruned, control = list(tokenize = OnegramTokenizer))
sampleTDM.common <- removeSparseTerms(sampleTDM, 0.6)

# 1.1. Frequent unigrams

#freq.unigrams <- rowSums(as.matrix(sampleTDM.unigrams))
#ord <- order(freq.unigrams)
#freq.unigrams[tail(ord)]
#head(table(freq.unigrams), 20)
freq.onegrams <- sort(rowSums(as.matrix(sampleTDM.onegrams)), decreasing = TRUE)
head(freq.onegrams, 20)
head(table(freq.onegrams), 20)
findFreqTerms(sampleTDM.onegrams, lowfreq = 50)

# 1.1.1. How many unique words do you need in a frequency sorted dictionary to cover 50% of all word instances in the language? 90%?

coverInstance <- function(freq, threshold) {
    tot <- sum(freq)
    for (i in 1:tot) {
        s <- sum(freq[1:i])
        if (s >= (threshold / 100 * tot))
            break
    }
    return(i)
}
coverInstance(freq.onegrams, 50)
coverInstance(freq.onegrams, 90)

# 1.2. Graph of most frequent unigrams

freq.term.onegrams <- subset(freq.onegrams, freq.onegrams >= 600)
df <- data.frame(term = names(freq.term.onegrams), freq = freq.term.onegrams)
ggplot(df, aes(x = term, y = freq)) + geom_bar(stat = "identity") + xlab("Terms") + ylab("Count") + theme(axis.text.x=element_text(angle=45, hjust=1))
df <- data.frame(term = as.numeric(names(table(freq.onegrams))), freq = as.vector(table(freq.onegrams)))
ggplot(df, aes(x = log(term), y = log(freq))) + layer(geom = "point")
plot(log(df$term), log(df$freq))

# 1.3. Associations (not useful ?!)

findAssocs(sampleTDM.onegrams, "into", corlimit = 0.98)

# 1.4. Word cloud of unigrams

set.seed(1)
wordcloud(names(freq.onegrams), freq.onegrams, max.words = 100, colors = brewer.pal(6, "Dark2"))

# 2. Document Term Matrix for bigrams

sampleTDM.twograms <- TermDocumentMatrix(preproc, control = list(tokenize = TwogramTokenizer))

# 2.1. Frequent bigrams

freq.twograms <- sort(rowSums(as.matrix(sampleTDM.twograms)), decreasing = TRUE)
head(table(freq.twograms), 20)
head(freq.twograms, 20)
findFreqTerms(sampleTDM.twograms, lowfreq = 50)

# 2.2. Graph of most frequent bigrams

freq.term.twograms <- subset(freq.twograms, freq.twograms >= 600)
df <- data.frame(term = names(freq.term.twograms), freq = freq.term.twograms)
ggplot(df, aes(x = term, y = freq)) + geom_bar(stat = "identity") + xlab("Terms") + ylab("Count") + theme(axis.text.x=element_text(angle=45, hjust=1))

# 2.3. Word cloud of bigrams

wordcloud(names(freq.twograms), freq.twograms, max.words = 100, colors = brewer.pal(6, "Dark2"))

# 3. Document Term Matrix for trigrams

sampleTDM.threegrams <- TermDocumentMatrix(preproc, control = list(tokenize = ThreegramTokenizer))

# 3.1. Frequent trigrams

freq.threegrams <- sort(rowSums(as.matrix(sampleTDM.threegrams)), decreasing = TRUE)
head(table(freq.threegrams), 20)
head(freq.threegrams, 20)
findFreqTerms(sampleTDM.threegrams, lowfreq = 50)

# 3.2. Graph of most frequent trigrams

freq.term.threegrams <- subset(freq.threegrams, freq.threegrams >= 50)
df <- data.frame(term = names(freq.term.threegrams), freq = freq.term.threegrams)
ggplot(df, aes(x = term, y = freq)) + geom_bar(stat = "identity") + xlab("Terms") + ylab("Count") + theme(axis.text.x=element_text(angle=45, hjust=1))

# 3.3. Word cloud of trigrams

wordcloud(names(freq.threegrams), freq.threegrams, max.words = 39, colors = brewer.pal(6, "Dark2"))
