# 1. Library loading

library(tm)
library(qdap)
library(RWeka)

# 2. Corpus creation

dir <- DirSource("final/en_US/", encoding = "UTF-8")
corpus <- Corpus(dir, readerControl = list(reader = readPlain))

# 3. Random sampling corpus

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

# 4. Tokenization

# 4.0. Copy of corpus.sample

preproc <- corpus.sample

# 4.1. Abbreviations removing

replaceAbbr <- function(x) {
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
preproc <- tm_map(preproc, content_transformer(replaceAbbr))
#new.abbr <- data.frame(abv = c("Mr.", "Mrs.", "Ms.", "i.e.", "A.D.", "B.C.", "A.M.", "P.M.", "et al.", "Jr.", "Dr.", "Sr.", "Sen.", "U.S.", "U.S.A.", "p.s."), rep = c("Mr", "Mrs", "Ms", "ie", "AD", "BC", "AM", "PM", "et al", "Jr", "Dr", "Sr", "Sen", "USA", "USA", "ps"))
#abv = c("Mr.", "Mrs.", "Ms.", "i.e.", "A.D.", "B.C.", "A.M.", "P.M.", "et al.", "Jr.", "Dr.", "Sr.", "Sen.", "U.S.", "U.S.A.", "p.s.", "w/", "&", "$")
#rep = c("Mr", "Mrs", "Ms", "ie", "AD", "BC", "AM", "PM", "et al", "Jr", "Dr", "Sr", "Sen", "USA", "USA", "ps", "with", "and", "dollar ")
#preproc <- tm_map(preproc, content_transformer(replace_abbreviation), abbreviation = new.abbr)

# 4.2. Numbers removing (with the case of decimals)

removeDecimals <- function(x) gsub("([0-9]*)\\.([0-9]+)", "\\1 \\2", x)
preproc <- tm_map(preproc, content_transformer(removeDecimals))
#preproc <- tm_map(preproc, content_transformer(replace_ordinal), num.paste = FALSE)
preproc <- tm_map(preproc, content_transformer(removeNumbers))

# 4.3. Remove URLs and email addresses

removeEmail <- function(x) gsub("[A-z0-9._%+-]+@[A-z0-9.-]+\\.(?:[A-z]{2}|com|org|net|edu|gov|mil|biz|info|mobi|name|aero|asia|jobs|museum)", "", x)
preproc <- tm_map(preproc, content_transformer(removeEmail))
removeURL <- function(x) sub("((https?|ftp)://)?www\\.[A-z0-9]+\\.[A-z0.9]{2,}", "", x)
preproc <- tm_map(preproc, content_transformer(removeURL))
#removeURL2 <- function(x) gsub("www.[A-z0-9]+.[a-z]{2,}", "", x)
#preproc <- tm_map(preproc, content_transformer(removeURL2))

# 4.4. Transform (single) left and right quotation marks

removeLRSQuotation <- function(x) gsub("\u2018|\u2019", "\'", x)
removeLRDQuotation <- function(x) gsub("\u201c|\u201d", "\"", x)
preproc <- tm_map(preproc, content_transformer(removeLRSQuotation))
preproc <- tm_map(preproc, content_transformer(removeLRDQuotation))

# 4.4. Remove brackets (?!!!)

# First : just remove quotes with one or two words in between (not a sentence)

#removeQuotations <- function(x) gsub("[\"|\u2018|\u201c|\']([A-z0-9]*\\s*[A-z0-9]*\\.*)[\"|\u2019|\u201d|\']", "\\1", x)
#preproc <- tm_map(preproc, content_transformer(removeQuotations))

# Second : remove brackets and quotes with more than two words (a sentence)

#removeBrackets <- function(x) {
#    bra <- genXtract(x, left = c("\'", "(", "\"", "\u201c", "\u2018", "[", "{"), right = c("\'", ")", "\"", "\u201d", "\u2019", "]", "}"))
#    bra <- unlist(bra)
#    text <- genX(x, left = c("\'", "(", "\"", "\u201c", "\u2018", "[", "{"), right = c("\'", ")", "\"", "\u201d", "\u2019", "]", "}"), scrub = FALSE)
#    text <- unlist(text)
#    return(c(text, bra))
#}
#preproc <- tm_map(preproc, content_transformer(removeBrackets))

# 4.5. Remove hashtags

removeHashtags <- function(x) gsub("#[A-z0-9]+", "", x)
preproc <- tm_map(preproc, content_transformer(removeHashtags))

# 4.6. Remove intra word dashes

removeDashes <- function(x) gsub("-+|—", " ", x)
preproc <- tm_map(preproc, content_transformer(removeDashes))
#removeDollar <- function(x) gsub("\\$", "", x)
#preproc <- tm_map(preproc, content_transformer(removeDollar))

# 4.8. Lower casing (and taking care of "I's")

transformIs <- function(x) {
    x <- gsub("^i ", "I ", x)
    x <- gsub("^i'", "I'", x)
    x <- gsub(" i ", " I ", x)
    x <- gsub(" i'", " I'", x)
    x <- gsub(".i ", ". I ", x)
    x <- gsub(".i'", ". I'", x)
    return(x)
}
preproc <- tm_map(preproc, content_transformer(tolower))
preproc <- tm_map(preproc, content_transformer(transformIs))

# 4.9. Non ASCII characters removing

preproc <- tm_map(preproc, content_transformer(iconv), from = "latin1", to = "ASCII", sub = "")

# 4.10. Punctuation removing

transformDots <- function(x) gsub("[:?!|…]+", ".", x)
removePunc <- function(x) gsub('[])(,;#%$^*\\~{}[&+=@/"`|<>_]+', "", x)
#removePunc <- function(x) gsub("\\.|:|!|\\?|\\||_|\\(|\\)|,|;|=|\\*|>|<|\\[|\\]|#|^|$|\\~|@", "", x)
preproc <- tm_map(preproc, content_transformer(transformDots))
preproc <- tm_map(preproc, content_transformer(removePunc))

# 4.12. White space removing

preproc <- tm_map(preproc, stripWhitespace)

# 4.7. Sentence splitting

#preproc1 <- tm_map(preproc, content_transformer(sent_detect), endmarks = c("?", ".", "!", "|", "…", ":", "\n","\r\n"), language = "en")
SentenceTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 1, delimiters = "\\r\\n\\t.:?!|…"))
preproc <- tm_map(preproc, content_transformer(SentenceTokenizer))

# 4.11. Trailing spaces removing

removeTrailingSpaces <- function(x) gsub("^\\s|\\s$", "", x)
preproc <- tm_map(preproc, content_transformer(removeTrailingSpaces))

# 4.13. Empty strings removing

removeEmptyStrings <- function(x) {
    return(x[x != ""])
}
preproc <- tm_map(preproc, content_transformer(removeEmptyStrings))

# 4.14. Remove double entries

preproc <- tm_map(preproc, content_transformer(unique))

# 5. Profanity filtering

profanities <- c("acrotomophilia", "anal", "anilingus", "anus", "arsehole", "ass", "asses", "asshole", "assholes", "assmunch", "auto erotic", "autoerotic", "babeland", "baby batter", "ball gag", "ball gravy", "ball kicking", "ball licking", "ball sack", "ball sucking", "bangbros", "bareback", "barely legal", "barenaked", "bastardo", "bastinado", "bbw", "bdsm", "beaver cleaver", "beaver lips", "bestiality", "bi curious", "big black", "big breasts", "big knockers", "big tits", "bimbos", "birdlock", "bitch", "bitches", "black cock", "blonde action", "blonde on blonde action", "blow j", "blow your l", "blue waffle", "blumpkin", "bollocks", "bondage", "boner", "boob", "boobs", "booty call", "brown showers", "brunette action", "bukkake", "bulldyke", "bullet vibe", "bung hole", "bunghole", "busty", "butt", "butts", "buttcheeks", "butthole", "camel toe", "cameltoe", "cameltoes", "camgirl", "camslut", "camwhore", "carpet muncher", "carpetmuncher", "chocolate rosebuds", "circlejerk", "cleveland steamer", "clit", "clitoris", "clover clamps", "clusterfuck", "cock", "cocks", "coprolagnia", "coprophilia", "cornhole", "cum", "cumming", "cunnilingus", "cunt", "cunts", "darkie", "date rape", "daterape", "deep throat", "deepthroat", "dick", "dicks", "dildo", "dirty pillows", "dirty sanchez", "doggie style", "doggiestyle", "doggy style", "doggystyle", "dog style", "dolcett", "dominatrix", "dommes", "donkey punch", "double dong", "double penetration", "dp action", "eat my ass", "ecchi", "ejaculation", "erotic", "erotism", "escort", "ethical slut", "eunuch", "faggot", "fecal", "felch", "fellatio", "feltch", "female squirting", "femdom", "figging", "fingering", "fisting", "foot fetish", "footjob", "frotting", "fuck", "fuck off", "fuckit", "fucked", "fucker", "fuckers", "fuckin", "fucking", "fuckyou", "futanari", "gang bang", "gay sex", "genitals", "giant cock", "girl on", "girls gone wild", "gokkun", "goodpoop", "goo girl", "grope", "group sex", "g-spot", "guro", "hand job", "handjob", "hard core", "hardcore", "hentai", "hooker", "hot chick", "huge fat", "humping", "incest", "intercourse", "jack off", "jail bait", "jailbait", "jerk off", "jigaboo", "jiggaboo", "jiggerboo", "jizz", "juggs", "kike", "kinbaku", "kinkster", "kinky", "knobbing", "leather restraint", "leather straight jacket", "lemon party", "lolita", "lovemaking", "make me come", "male squirting", "masturbate", "menage a trois", "milf", "missionary position", "motherfucker", "motherfuckers", "mound of venus", "mr hands", "muff diver", "muffdiving", "nambla", "nawashi", "negro", "neonazi", "nigga", "niggas", "nigger", "niggers", "nig nog", "nimphomania", "nipple", "nipples", "nsfw images", "nude", "nudity", "nympho", "nymphomania", "octopussy", "omorashi", "orgasm", "orgy", "orgies", "paedophile", "panties", "panty", "pedobear", "pedophile", "pegging", "penis", "phone sex", "piece of shit", "pissing", "piss pig", "pisspig", "playboy", "pleasure chest", "pole smoker", "ponyplay", "poof", "poopchute", "porn", "porno", "pornography", "pthc", "pubes", "pussy", "queaf", "raghead", "rape", "raping", "rapist", "rectum", "rimjob", "rimming", "rosy palm", "rusty trombone", "sadism", "scat", "schlong", "scissoring", "semen", "sex", "sexo", "sexy", "shaved beaver", "shaved pussy", "shemale", "shibari", "shit", "shota", "shrimping", "slanteye", "slut", "sluts", "s&m", "smut", "snatch", "sodomize", "sodomy", "spic", "spooge", "spread legs", "strap on", "strapon", "strip club", "style doggy", "suck", "sucks", "suicide girls", "sultry women", "swastika", "swinger", "tea bagging", "threesome", "throating", "tied up", "tight white", "tit", "tits", "titties", "titty", "topless", "tosser", "tranny", "tribadism", "tub girl", "tubgirl", "tushy", "twat", "twink", "twinkie", "undressing", "upskirt", "urethra play", "urophilia", "vagina", "venus mound", "vibrator", "violet wand", "vorarephilia", "voyeur", "vulva", "wank", "wetback", "wet dream", "white power", "women rapping", "wrapping men", "xx", "xxx", "yaoi", "yiffy", "zoophilia")
filter <- tm_map(preproc, removeWords, profanities)

# 6. Tokenizing 1-grams

options(mc.cores = 1)
OnegramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 1, delimiters = " \\r\\n\\t.,;:\"()?!|"))
onegrams <- tm_map(preproc, content_transformer(OnegramTokenizer))

# 7. Tokenizing 2-grams

TwogramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2, delimiters = " \\r\\n\\t.,;:\"()?!|"))
twograms <- tm_map(preproc, content_transformer(TwogramTokenizer))

# 8. Tokenizing 3-grams

ThreegramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3, delimiters = " \\r\\n\\t.,;:\"()?!|"))
threegrams <- tm_map(preproc, content_transformer(ThreegramTokenizer))

# 9. Tokenizing 4-grams

FourgramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 4, max = 4, delimiters = " \\r\\n\\t.,;:\"()?!|"))
fourgrams <- tm_map(preproc, content_transformer(FourgramTokenizer))

# 10. Tokenizing 5-grams

FivegramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 5, max = 5, delimiters = " \\r\\n\\t.,;:\"()?!|"))
fivegrams <- tm_map(preproc, content_transformer(FivegramTokenizer))
