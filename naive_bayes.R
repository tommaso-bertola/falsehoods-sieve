# import the required libraries
library(tidyverse)
library(stopwords)
library(tidytext)
library(stringr)
library(purrr)
library(magrittr)
library(parallel)


# a function to clean tokens
# requires as input a vector of tokens (or a dataframe of 1 column)
# returns the same vector but with elements modified accordingly
cleaning_tokens <- function(token.df) {
    # removes final 's' to change to singlular the plural words
    word <- ifelse(endsWith(token.df[, 1], "s"),
                   substr(token.df[, 1], 1, nchar(token.df[, 1]) - 1),
                   token.df[, 1])

    # remove saxon genitives 'Tommaso's' to 'Tommaso'
    word <- ifelse(endsWith(word, "'"), substr(word, 1, nchar(word) - 1), word)

    # equivalence class for numbers: every number is substituted
    # with '--number--', no distinctions are made on the numbers
    word <- ifelse(str_detect(word, "[0-9]+"), "--number--", word)

    # return a dataframe to be more easily handeld with pipe operators
    return(as.data.frame(word))
}


# the first function to count the words in the vocabulary
# takes as input the raw train dataset and tags_numbers
# which is an object helping with the name and number of classes
# train.df.train needs to have a 'Text' column
vocabulary <- function(train.df.train, tags_numbers) {

    myframes <- list() # to help keeping track of
                       # the word coutings depending on the category

    for (i in seq_along(tags_numbers$tags)) {

        tag <- tags_numbers$tags[i] # get the label name
        number <- tags_numbers$numbers[i] # get the label number

        train.df.train %>%
            filter(Labels == number) %>%              # select only a specific label
            select(Text) %>%                          # select the column 'Text'
            unnest_tokens(word, Text) %>%             # use tidytext function to return tokenized message
            anti_join(get_stopwords(),                # remove from the dataframe of token the stop words
                      by = join_by(word)) %>%         #
            cleaning_tokens() %>%                     # perform equivalence class normalization
            arrange(word) %>%                         # sort by word alphabetically
            group_by(word) %>%                        # group by 'word'
            reframe(counts = n()) %>%                 # summarize by returning the counts of each word
            arrange(desc(counts)) %>%                 # arrange by counts in descending order
            setNames(c("word", tag)) -> myframes[[i]] # set column names and save in the list
    }

    # reduce the list of dataset to a single dataframe
    counts <- purrr::reduce(myframes, dplyr::full_join, by = "word")

    # set NA values to 0
    counts %<>% replace(is.na(.), 0)

    # return a dataframe counting each word appearance for each label
    return(counts)
}


####################################
#                                  #
#     RANKERS                      #
#                                  #
####################################

# rank words by class for a given metric 'df_rank'
# requires a metric, the actual counts, the number of classes,
# and the fraction to keep 
ranking_per_class <- function(df_rank, counts, n_classes, frac = 0.5) {

    mywords <- list() # help to keep track of different countings
    
    # for each column (label), retain only the most significant 'frac' % words
    for (i in 2:(n_classes + 1)) {
        df_rank[, c(1, i)] %>%             # get the rank for that label
            arrange(desc(.[[2]])) %>%      # order by metric value
            top_frac(frac, .[[2]]) %>%     # retain only the top_frac
            select(word) -> mywords[[i]]   # get the associatiod words and save them
    }

    # join all the words retained for each category
    words <- purrr::reduce(mywords, rbind)

    # remove duplicated words
    words %<>% unique

    # from original countings, keep only selected words and
    # return the vocabulry and associated countings
    counts %>% right_join(words["word"], by = join_by(word)) -> naive.bayes.vocabulary

    return(naive.bayes.vocabulary)
}

# rank words by overall metric, given by 'df_rank'
# requires a metric, the actual counts, the number of classes,
# and the fraction to keep 
ranking_per_mean <- function(df_rank, counts, n_classes, frac = 0.5) {

    # compute the mean metric score for each word
    df_rank["mean"] <- rowMeans(df_rank[2:(n_classes + 1)])

    df_rank %>%
        arrange(desc(mean)) %>%     # arrange by mean of metric score
        top_frac(frac, mean) %>%    # retain the top fraction
        select(word) -> words       # select the words and save

    # from original countings, keep only selected words and
    # return the vocabulry and associated countings
    counts %>% right_join(words["word"], by = join_by(word)) -> naive.bayes.vocabulary

    return(naive.bayes.vocabulary)
}


####################################
#                                  #
#     FEATURE SELECTION            #
#                                  #
####################################

#####################
#  By frequency     #
#####################

# perform the feature selction with absolute frequencies
# requires initial countings of the words, 
# number of classes, and fraction to retain
feature_selection.frequency_mean <- function(counts, n_classes, frac = 0.5) {

    # sum columns to get the normalization factor
    counts %>%
        select(-word) %>%
        colSums() -> tot_counts_per_class

    # generate metric by computing the fraction of each word
    counts_prob <- cbind(counts["word"], counts[2:(n_classes + 1)] / tot_counts_per_class)

    # compute the dictionary
    return(ranking_per_mean(counts_prob, counts, n_classes, frac))
}

# perform the feature selction with absolute frequencies by class
# requires initial countings of the words,
# number of classes, and fraction to retain
feature_selection.frequency_per_class <- function(counts, n_classes, frac = 0.5) {

    # return the ranking per class
    return(ranking_per_class(counts, counts, n_classes, frac))
}



#####################
#  By Chi squared   #
#####################

# compute the chi squared metric per word for all classes
# requires the word, the train dataset, and the number of classes
word_chi_squared <- function(word, train.df.train, n_classes) {

    # makes the distinction between numbers and words
    if (word != "--number--") { # if it is a proper word
        train.df.train %>%
            group_by(Labels, .drop = FALSE) %>%               # make distinction on the different labels
            filter(grepl(word, Text, ignore.case = TRUE)) %>% # select documents with that word
            summarise(c = n()) -> yw_c                        # count the number of documents with that word and save

        # same as above, but count documents without that word
        train.df.train %>%
            group_by(Labels, .drop = FALSE) %>%
            filter(!grepl(word, Text, ignore.case = TRUE)) %>%
            summarise(c = n()) -> nw_c

    } else {
        # same as before but check the numbers
        train.df.train %>%
            group_by(Labels, .drop = FALSE) %>%
            filter(grepl("[0-9]+", Text, ignore.case = TRUE)) %>%
            summarise(c = n()) -> yw_c

        train.df.train %>%
            group_by(Labels, .drop = FALSE) %>%
            filter(!grepl("[0-9]+", Text, ignore.case = TRUE)) %>%
            summarise(c = n()) -> nw_c
    }

    # keep track of chi squared value per class
    chi <- vector(length = n_classes)

    # count per class the n__ values to compute the chi squared 
    for (i in 1:n_classes) {
        n11 <- yw_c[yw_c$Labels == (i - 1), ]$c
        n10 <- sum(yw_c[yw_c$Labels != (i - 1), ]$c)
        n01 <- nw_c[yw_c$Labels == (i - 1), ]$c
        n00 <- sum(nw_c[nw_c$Labels != (i - 1), ]$c)

        # actually compute the chi squared per class
        chi[i] <- ((n11 + n10 + n01 + n00) * (n11 * n00 - n10 * n01)^2) / ((n11 + n01) *
            (n11 + n10) * (n10 + n00) * (n01 + n00))
    }

    # avoid NA values
    chi[is.na(chi)] <- 0

    # return a vector of chi squared metric per class for the 'word'
    return(chi)
}


# compute the chi squared for each word in the  original vocabulary 'count'
# requires the counts, the whole training dataset, number of classes and number of cores 
# to perform the parallel computation
vocabulary_chi_squared <- function(counts, train.df.train, n_classes, numCores = numCores) {

    # parallel apply of chi squared computation to each word in the vocabulary
    df <- mclapply(
        X = counts$word, FUN = word_chi_squared, train.df.train = train.df.train,
        n_classes = n_classes, mc.cores = numCores
    )

    # generate the chi squared df_metric dataframe with all words and 
    # chi squared values per class
    chi <- cbind(counts[, 1], as.data.frame(do.call(rbind, df)))

    return(chi)
}

# return the vocabulary ranked per mean with chi squared feature selection
# requires the original countings of the words, the whole training dataset, 
# number of classes and fraction to retain
feature_selection.chi_squared_per_mean <- function(counts, train.df.train, n_classes, frac = 0.5) {

    # return the df_metric dataframe
    chi <- vocabulary_chi_squared(counts, train.df.train, n_classes, numCores)

    # compute the final dictionary
    return(ranking_per_mean(chi, counts, n_classes, frac))
}

# return the vocabulary ranked per mean with chi squared feature selection
# requires the original countings of the words, the whole training dataset, 
# number of classes and fraction to retain
feature_selection.chi_squared_per_class <- function(counts, train.df.train, n_classes, frac = 0.5) {

    # return the df_metric dataframe
    chi <- vocabulary_chi_squared(counts, train.df.train, n_classes, numCores)

    # compute the final dictionary, distinguishing by class
    return(ranking_per_class(chi, counts, n_classes, frac))
}



#############################
#  By mutual information    #
#############################

# compute the mutual information metric per word for all classes
# requires the word, the train dataset, and the number of classes
word_mutual_info <- function(word, train.df.train, n_classes) {

    # same structure of chi squared metric
    if (word != "--number--") {
        train.df.train %>%
            group_by(Labels, .drop = FALSE) %>%
            filter(grepl(word, Text, ignore.case = TRUE)) %>%
            summarise(c = n()) -> yw_c

        train.df.train %>%
            group_by(Labels, .drop = FALSE) %>%
            filter(!grepl(word, Text, ignore.case = TRUE)) %>%
            summarise(c = n()) -> nw_c

    } else {

        train.df.train %>%
            group_by(Labels, .drop = FALSE) %>%
            filter(grepl("[0-9]+", Text, ignore.case = TRUE)) %>%
            summarise(c = n()) -> yw_c

        train.df.train %>%
            group_by(Labels, .drop = FALSE) %>%
            filter(!grepl("[0-9]+", Text, ignore.case = TRUE)) %>%
            summarise(c = n()) -> nw_c
    }

    # compute the mutual information metric

    # keeps the mutual information separated by class, for a single word
    MI <- vector(length = n_classes)
    for (i in 1:n_classes) {
        n11 <- yw_c[yw_c$Labels == (i - 1), ]$c
        n10 <- sum(yw_c[yw_c$Labels != (i - 1), ]$c)
        n01 <- nw_c[yw_c$Labels == (i - 1), ]$c
        n00 <- sum(nw_c[nw_c$Labels != (i - 1), ]$c)
        n1_ <- n11 + n10 # marginalize
        n0_ <- n01 + n00 #
        n_1 <- n01 + n11 #
        n_0 <- n10 + n00 #
        n <- n11 + n01 + n10 + n00

        # the actual mutual information
        MI[i] <- n11 / n * log2(n * n11 / (n1_ * n_1)) + n01 / n * log2(n * n01 / (n0_ *
            n_1)) + n10 / n * log2(n * n10 / (n1_ * n_0)) + n00 / n * log2(n * n00 / (n0_ *
            n_0))
    }

    # avoid NA values
    MI[is.na(MI)] <- 0

    # return the mutual information vector for a single word, by class
    return(MI)
}

# compute the muutal information for all the words in the vocabulary
# requires the counts, the whole training dataset, number of classes and number of cores 
# to perform the parallel computation
vocabulary_mutual_info <- function(counts, train.df.train, n_classes, numCores = numCores) {

    # apply the mutual information computation to each word
    df <- mclapply(
        X = counts$word, FUN = word_mutual_info, train.df.train = train.df.train,
        n_classes = n_classes, mc.cores = numCores
    )

    # concatenate mutual informations to a single dataset
    mutual_info <- cbind(counts[, 1], as.data.frame(do.call(rbind, df)))

    # return a df_metric dataframe of mutual information by word and class
    return(mutual_info)
}


# return the vocabulary ranked per mean with mutual information feature selection
# requires the original countings of the words, the whole training dataset, 
# number of classes and fraction to retain
feature_selection.mutual_info_per_class <- function(counts, train.df.train, n_classes, frac = 0.5) {

    # get the mutual information values for each word
    mutual_info <- vocabulary_mutual_info(counts, train.df.train, n_classes, numCores)

    # compute feature selectin by class
    return(ranking_per_class(mutual_info, counts, n_classes, frac))
}



# return the vocabulary ranked per mean with chi squared feature selection
# requires the original countings of the words, the whole training dataset, 
# number of classes and fraction to retain
feature_selection.mutual_info_per_mean <- function(counts, train.df.train, n_classes, frac = 0.5) {

    # compute the df_metric for mutual information
    mutual_info <- vocabulary_mutual_info(counts, train.df.train, n_classes, numCores)

    # perform feature selection by mean on mutual information
    return(ranking_per_mean(mutual_info, counts, n_classes, frac))
}


#################################
#                               #
#    NAIVE BAYES ALGORITHM      #
#                               #
#################################

# compute the denominator per class to normalize countings to probabilities
likelihood.denominator <- function(naive.bayes.vocabulary) {

    naive.bayes.vocabulary %>%
        select(-word) %>%
        colSums() -> tot_counts_per_class # just sum the countings by column

    # add the Laplace smoothing to the denominator
    den <- tot_counts_per_class + length(naive.bayes.vocabulary[[1]])

    # return the vector of denomanators, by class
    return(den)
}


# compute the likelihood of each word in the feature selected dictionary
# to be part of a specific class
# requires the feature selected dictionary and the normalization factor
likelihood <- function(naive.bayes.vocabulary, den) {

    # bind rows to likelihoods, add Laplace smoothing of +1
    likelihood.token <- cbind(naive.bayes.vocabulary["word"],
                             (naive.bayes.vocabulary[2:(n_classes + 1)] + 1) / den)

    # return the likelihood dataframe
    return(likelihood.token)
}

# tokenize a single message
# requires the text of a single message
tweet_tokenize <- function(msg) {

    # transform the message to a dataframe for easier handling
    df <- data.frame(msg)

    # for easier handling by cleaning functions
    colnames(df) <- "tweet"

    # perform the same operation of tokenization and equivalence classes as above
    df %>%
        unnest_tokens(word, tweet) %>%
        anti_join(get_stopwords(), by = join_by(word)) %>%
        cleaning_tokens() -> token.list

    # return the list of tokens of a message
    return(token.list)
}


# compute the posterior probability of a message being of a specific class
# requires the token list of the message, the dataframe of token likelihoods,
# the prior probabilities in being in a specific class,
# the denominator to normalize, and the number of classes
bayes.probability <- function(token.list, likelihood.token, prior.classes, den, n_classes) {

    # the initial prior of the message in being in each class
    # it will be updated with the likelihoods
    # we use the logarithm to deal with smaller numbers and to turn multiplications into sums
    query_out <- log(as.numeric(prior.classes))

    # for each token
    for (i in 1:length(token.list)) {

        # update the posterior with the likelihoods found in the corresponding dataframe
        query <- log(as.numeric(likelihood.token[likelihood.token["word"] == token.list$word[i]][2:(n_classes +
            1)]))

        # if the token is not found in the likelihood dataframe, just use the Laplace smoothing
        if (is.na(query[1])) {
            # because if the word is not found, all NA are returned. if the
            # word is in the vocabulary the proper likelihood is returned
            query <- as.numeric(log(1 / den))
        }

        # actually update the posterior 
        query_out <- query_out + query
    }

    # return a proper posterior
    query_out <- exp(query_out)

    # normalize the posterior for each class
    query_out <- query_out / sum(query_out)
    return(query_out)
}


# return the predicted class of a message
# requires the message, the likelihood dataframe, the prior probabilities, 
# the normalization factor and the number of classes
naive.bayes <- function(msg, likelihood.token, prior.classes, den, n_classes) {

    # tokenize a single message
    token.list <- as.vector(tweet_tokenize(msg))

    # compute the posterior of being in each class
    probability <- bayes.probability(
        token.list, likelihood.token, prior.classes,
        den, n_classes
    )

    # assing the prediction to the class with highest probability
    predicted_class <- which.max(probability) - 1

    # return the predicted class number
    return(predicted_class)
}
