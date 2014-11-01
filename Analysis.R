# Packages loading

library(ggplot2)
library(wordcloud)

# 1. Document Term Matrix for unigrams

preproc.pruned <- tm_map(preproc, removeWords, stopwords("english"))
#sampleTDM.unigrams <- TermDocumentMatrix(preproc.pruned)
sampleTDM.unigrams <- TermDocumentMatrix(preproc.pruned, control = list(tokenize = UnigramTokenizer))
sampleTDM.common <- removeSparseTerms(sampleTDM, 0.6)

# 1.1. Frequent unigrams

#freq.unigrams <- rowSums(as.matrix(sampleTDM.unigrams))
#ord <- order(freq.unigrams)
#freq.unigrams[tail(ord)]
#head(table(freq.unigrams), 20)
freq.unigrams <- sort(rowSums(as.matrix(sampleTDM.unigrams)), decreasing = TRUE)
head(freq.unigrams, 20)
head(table(freq.unigrams), 20)
findFreqTerms(sampleTDM.unigrams, lowfreq = 50)

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
coverInstance(freq.unigrams, 50)
coverInstance(freq.unigrams, 90)

# 1.2. Graph of most frequent unigrams

freq.term.unigrams <- subset(freq.unigrams, freq.unigrams >= 600)
df <- data.frame(term = names(freq.term.unigrams), freq = freq.term.unigrams)
ggplot(df, aes(x = term, y = freq)) + geom_bar(stat = "identity") + xlab("Terms") + ylab("Count") + theme(axis.text.x=element_text(angle=45, hjust=1))
df <- data.frame(term = as.numeric(names(table(freq.unigrams))), freq = as.vector(table(freq.unigrams)))
ggplot(df, aes(x = log(term), y = log(freq))) + layer(geom = "point")
plot(log(df$term), log(df$freq))

# 1.3. Associations (not useful ?!)

findAssocs(sampleTDM.unigrams, "into", corlimit = 0.98)

# 1.4. Word cloud of unigrams

set.seed(1)
wordcloud(names(freq.unigrams), freq.unigrams, max.words = 100, colors = brewer.pal(6, "Dark2"))

# 2. Document Term Matrix for bigrams

sampleTDM.bigrams <- TermDocumentMatrix(preproc, control = list(tokenize = BigramTokenizer))

# 2.1. Frequent bigrams

freq.bigrams <- sort(rowSums(as.matrix(sampleTDM.bigrams)), decreasing = TRUE)
head(table(freq.bigrams), 20)
head(freq.bigrams, 20)
findFreqTerms(sampleTDM.bigrams, lowfreq = 50)

# 2.2. Graph of most frequent bigrams

freq.term.bigrams <- subset(freq.bigrams, freq.bigrams >= 600)
df <- data.frame(term = names(freq.term.bigrams), freq = freq.term.bigrams)
ggplot(df, aes(x = term, y = freq)) + geom_bar(stat = "identity") + xlab("Terms") + ylab("Count") + theme(axis.text.x=element_text(angle=45, hjust=1))

# 2.3. Word cloud of bigrams

wordcloud(names(freq.bigrams), freq.bigrams, max.words = 100, colors = brewer.pal(6, "Dark2"))

# 3. Document Term Matrix for trigrams

sampleTDM.trigrams <- TermDocumentMatrix(preproc, control = list(tokenize = TrigramTokenizer))

# 3.1. Frequent trigrams

freq.trigrams <- sort(rowSums(as.matrix(sampleTDM.trigrams)), decreasing = TRUE)
head(table(freq.trigrams), 20)
head(freq.trigrams, 20)
findFreqTerms(sampleTDM.trigrams, lowfreq = 50)

# 3.2. Graph of most frequent trigrams

freq.term.trigrams <- subset(freq.trigrams, freq.trigrams >= 50)
df <- data.frame(term = names(freq.term.trigrams), freq = freq.term.trigrams)
ggplot(df, aes(x = term, y = freq)) + geom_bar(stat = "identity") + xlab("Terms") + ylab("Count") + theme(axis.text.x=element_text(angle=45, hjust=1))

# 3.3. Word cloud of trigrams

wordcloud(names(freq.trigrams), freq.trigrams, max.words = 40, colors = brewer.pal(6, "Dark2"))
