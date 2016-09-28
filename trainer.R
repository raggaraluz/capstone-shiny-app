input.folder <- file.path('..', 'en_US')
library(quanteda)
library(dplyr)



# news <- readLines(file(file.path(input.folder, 'en_US.news.txt'), 'rb'), encoding = 'UTF-8')
# blogs <- readLines(file(file.path(input.folder, 'en_US.blogs.txt'), 'rb'), encoding = 'UTF-8')
# twit <- readLines(file(file.path(input.folder, 'en_US.twitter.txt'), 'rb'), encoding = 'UTF-8')
# 
# perc.lines = 10 / 100 # Percentage of lines to be actualy read from the files.
# set.seed(22342)
# news <- sample(news, length(news) * perc.lines)
# blogs <- sample(blogs, length(blogs) * perc.lines)
# twit <- sample(twit, length(twit) * perc.lines)
# sentences <- c(news, blogs, twit)
# 
# writeLines(c(news, blogs, twit), 'reduced_dataset.txt')

sentences <- readLines(file('../data/reduced_dataset.txt', 'rb'), encoding = 'UTF-8')
# sentences <- sample(sentences, length(sentences) * .2)

docs <- corpus(sentences)
rm(sentences)

tokensAll <- tokenize(toLower(docs), removeNumbers = T, removePunct = T, removeSeparators = T,
                      removeTwitter = T, removeSymbols = T, removeURL = T)

dist.1 <- dfm(tokensAll, removeTwitter = T)
top.1 <- topfeatures(dist.1, 1e10)
cdf.1 <- cumsum(top.1) / sum(top.1)
percs <- 0.95
most.used <- names(which(cdf.1 <= percs))
tokensSubset <- selectFeatures(tokensAll, most.used, selection = c('keep'))

rm(dist.1, cdf.1, tokensAll)

#dist.1 <- dfm(docs)
#words <- names(topfeatures(dist.1, 10000))

dist.3 <- dfm(ngrams(tokensSubset, 3), removeTwitter = T)
dist.3 <- topfeatures(dist.3, 1e6)
dist.4 <- dfm(ngrams(tokensSubset, 4), removeTwitter = T)
dist.4 <- topfeatures(dist.4, 1e6)
dist.2 <- dfm(ngrams(tokensSubset, 2), removeTwitter = T)
dist.2 <- topfeatures(dist.2, 1e6)

rm(tokensSubset)

#dist.3.s <- dfm(docs, ignoredFeatures=stopwords('english'), ngrams = 3)

library(dplyr)

toDataFrame <- function(dist.c, n) {
    a <- sapply(names(dist.c), strsplit, '_')
    rows <- which(sapply(a, length) == n)
    a <- a[rows]
    dist.c <- dist.c[rows]
    data <- tbl_df(data.frame(do.call(rbind, a), dist.c))
    
    w.str <- paste0('w', 1:(n-1))
    names(data) <- c(w.str, 'wp', 'count')
    data.s <- data %>% 
        group_by_(.dots = w.str) %>%
        mutate(total = sum(count)) %>%
        top_n(3, count) %>%
        ungroup()

    return (data.s)
}

data.4 <- toDataFrame(dist.4, 4)
data.3 <- toDataFrame(dist.3, 3)
data.2 <- toDataFrame(dist.2, 2)
data.1 <- data.frame(wp = names(top.1[1:3]), count = top.1[1:3], total = sum(top.1) )

#data.3.s <- toDataFrame(dist.3.s, 3)

save(file='data_english.Rdata', data.4, data.3, data.2, data.1, most.used)

# # Q1
# #q1.2 <- data.2 %>% filter(w1 == 'of')
# q1.3 <- data.3 %>% filter(w1 == 'case', w2 == 'of')
# q1.4 <- data.4 %>% filter(w1 == 'a', w2 == 'case', w3 == 'of') # beer
# 
# # Q2
# #q2.3 <- data.3 %>% filter(w1 == 'mean', w2 == 'the')  # World
# 
# 
# # Q3
# #q3.3 <- data.3 %>% filter(w1 == 'me', w2 == 'the')
# q3.4 <- data.4 %>% filter(w1 == 'make', w2 == 'me', w3 == 'the')
# 
# # Q4
# #q4.3 <- data.3 %>% filter(w1 == 'but', w2 == 'the')
# q4.4 <- data.4 %>% filter(w1 == 'struggling', w2 == 'but', w3 == 'the')
# 
# # Q5
# #q5.3 <- data.3 %>% filter(w1 == 'at', w2 == 'the')
# q5.4 <- data.4 %>% filter(w1 == 'date', w2 == 'at', w3 == 'the')
# 
# # Q6
# #q6.3 <- data.3 %>% filter(w1 == 'on', w2 == 'my') # way
# q6.4 <- data.4 %>% filter(w1 == 'be', w2 == 'on', w3 == 'my')
# 
# # Q7
# #q7.3 <- data.3 %>% filter(w1 == 'quite', w2 == 'some')
# q7.4 <- data.4 %>% filter(w1 == 'in', w2 == 'quite', w3 == 'some')
# #q7.2 <- data.2 %>% filter(w1 == 'some')
# 
# # Q8
# #q8.3 <- data.3 %>% filter(w1 == 'his', w2 == 'little')
# q8.4 <- data.4 %>% filter(w1 == 'with', w2 == 'his', w3 == 'little')
# #q8.2 <- data.2 %>% filter(w1 == 'little')
# 
# # Q9
# #q9.3 <- data.3 %>% filter(w1 == 'during', w2 == 'the')
# q9.4 <- data.4 %>% filter(w1 == 'faith', w2 == 'during', w3 == 'the')
# #q9.2 <- data.2 %>% filter(w1 == 'the')
# 
# # Q10
# #q10.3 <- data.3 %>% filter(w1 == 'must', w2 == 'be')
# q10.4 <- data.4 %>% filter(w1 == 'you', w2 == 'must', w3 == 'be')
# #q10.2 <- data.2 %>% filter(w1 == 'be')

evaluate <- function(wi1, wi2 = NA, wi3 = NA)
{
    if (is.na(wi2))
    {
        d <- data.2 %>% filter(w1 == wi1) %>% select(wp.m)
        return (d)
    }
    if (is.na(wi3))
    {
        d <- data.3 %>% filter(w1 == wi1, w2 == wi2) %>% select(wp.m)
        return (d)
    }
    d <- data.4 %>% filter(w1 == wi1, w2 == wi2, w3 == wi3) %>% select(wp.m)
    return (d)
}


evaluate('live', 'and', "i'd")
evaluate('and', "i'd")

evaluate('me', 'about', 'this')
evaluate('about', 'this')
evaluate('this')

evaluate('arctic', 'monkeys', 'this')
evaluate('monkeys', 'this')
evaluate('to', 'see', 'this')
evaluate('see', 'this')

evaluate('helps', 'reduce', 'your')
evaluate('reduce', 'your')
evaluate('your')

evaluate('to', 'take', 'a')

evaluate('to', 'settle', 'the')
evaluate('settle', 'the')

evaluate('in', 'each')
evaluate('groceries', 'in', 'each')

evaluate('bottom', 'to', 'the')
evaluate('to', 'the')

evaluate('bruises', 'from', 'playing')
evaluate('from', 'playing')
evaluate('playing')

evaluate('of', 'Adam', "Sandler's")
evaluate("Sandler's")
evaluate('Adam', "Sandler's")
