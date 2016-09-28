library(quanteda)
library(dplyr)

# Calculate the prediction for the text input
get.prediction <- function(input) {
    tokens <- get.tokens(input)[[1]]
    l <- length(tokens)
    pred <- data.frame(wp = c(), count = c())
    
    if (l >= 3)
    {
        pred <- get.prediction.from.tokens(tokens[l-2], tokens[l-1], tokens[l])
    }
    
    if (nrow(pred) < 3 && l >= 2)
    {
        tmp.pred <- get.prediction.from.tokens(tokens[l-1], tokens[l]) %>%
            filter(!(wp %in% pred$wp)) %>%
            slice(1:(3-nrow(pred)))
        
        pred <- rbind(pred, tmp.pred)
    }
    
    if (nrow(pred) == 0 && l >= 1)
    {
        tmp.pred <- get.prediction.from.tokens(tokens[l]) %>%
            filter(!(wp %in% pred$wp)) %>%
            slice(1:(3-nrow(pred)))
        
        pred <- rbind(tmp.pred, pred)
    }
    
    pred
}

get.prediction.from.tokens <- function(wi1, wi2 = NA, wi3 = NA)
{
    if (is.na(wi2))
    {
        d <- data.2 %>% filter(w1 == wi1)
    }
    else if (is.na(wi3))
    {
        d <- data.3 %>% filter(w1 == wi1, w2 == wi2)
    }
    else {
        d <- data.4 %>% filter(w1 == wi1, w2 == wi2, w3 == wi3)
    }
    d %>% select(wp, count)
}

# Get the tokens to use for the prediction
get.tokens <- function(input) {
    # Tokenize first
    all.tokens <- tokenize(input, removeNumbers = T, removePunct = T,
                             removeSymbols = T, removeSeparators = T,
                             removeTwitter = T, removeHyphens = T, removeURL = T) %>%
                  toLower() %>%
    
    # Then keep only the most used words
                  selectFeatures(most.used, selection = c('keep'))
    
    all.tokens
}

# Load the model
load(file = 'data_2.Rdata')


## Test statements
get.prediction('Some text')

tokens <- get.tokens('This is some text I would like to review')
