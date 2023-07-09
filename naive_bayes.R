library(tidyverse)
library(ggplot2)
library(stopwords)
library(tidytext)
library(stringr)
library(purrr)
library(magrittr)
library(parallel)


cleaning_tokens <- function(token.df) {
    word <- ifelse(endsWith(token.df[, 1], "s"), substr(token.df[, 1], 1, nchar(token.df[, 1]) - 1), token.df[, 1])
    word <- ifelse(endsWith(word, "'"), substr(word, 1, nchar(word) - 1), word)
    word <- ifelse(str_detect(word, "[0-9]+"), "--number--", word)
    return(as.data.frame(word))
}


vocabulary <- function(train.df.train, tags_numbers) {
    # take in input a df with 'Text' column containing the
    # messages and 'tag_numbers' the names of the labels

    myframes <- list()

    for (i in 1:length(tags_numbers$tags)) {
        tag <- tags_numbers$tags[i]

        train.df.train %>%
            filter(Labels == tags_numbers$numbers[i]) %>%
            select(Text) %>%
            unnest_tokens(word, Text) %>%
            anti_join(get_stopwords(), by = join_by(word)) %>%
            cleaning_tokens() %>%
            arrange(word) %>%
            group_by(word) %>%
            reframe(counts = n()) %>%
            arrange(desc(counts)) %>%
            setNames(c("word", tag)) -> myframes[[i]]
    }
    counts <- purrr::reduce(myframes, dplyr::full_join, by = "word")
    counts %<>% replace(is.na(.), 0)
    return(counts)
}


ranking_per_class <- function(df_rank, counts, n_classes, frac = 0.5) {
    mywords <- list()
    for (i in 2:(n_classes + 1)) {
        df_rank[, c(1, i)] %>%
            arrange(desc(.[[2]])) %>%
            top_frac(frac, .[[2]]) %>%
            select(word) -> mywords[[i]]
    }
    words <- purrr::reduce(mywords, rbind)
    words %<>%
        unique

    counts %>%
        right_join(words["word"], by = join_by(word)) -> naive.bayes.vocabulary

    return(naive.bayes.vocabulary)
}


ranking_per_mean <- function(df_rank, counts, n_classes, frac = 0.5) {
    df_rank["mean"] <- rowMeans(df_rank[2:(n_classes + 1)])

    df_rank %>%
        arrange(desc(mean)) %>%
        top_frac(frac, mean) %>%
        select(word) -> vocabulary.train.features

    counts %>%
        right_join(vocabulary.train.features["word"], by = join_by(word)) -> naive.bayes.vocabulary

    return(naive.bayes.vocabulary)
}


feature_selection.frequency_mean <- function(counts, n_classes, frac = 0.5) {
    counts %>%
        select(-word) %>%
        colSums() -> tot_counts_per_class

    counts_prob <- cbind(counts["word"], counts[2:(n_classes + 1)] / tot_counts_per_class)

    return(ranking_per_mean(counts_prob, counts, n_classes, frac))
}


feature_selection.frequency_per_class <- function(counts, n_classes, frac = 0.5) {
    return(ranking_per_class(counts, counts, n_classes, frac))
}



word_chi_squared <- function(word, train.df.train, n_classes) {
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

    chi <- vector(length = n_classes)
    for (i in 1:n_classes) {
        n11 <- yw_c[yw_c$Labels == (i - 1), ]$c
        n10 <- sum(yw_c[yw_c$Labels != (i - 1), ]$c)
        n01 <- nw_c[yw_c$Labels == (i - 1), ]$c
        n00 <- sum(nw_c[nw_c$Labels != (i - 1), ]$c)

        chi[i] <- ((n11 + n10 + n01 + n00) * (n11 * n00 - n10 * n01)^2) / ((n11 + n01) *
            (n11 + n10) * (n10 + n00) * (n01 + n00))
    }
    chi[is.na(chi)] <- 0

    return(chi)
}



vocabulary_chi_squared <- function(counts, train.df.train, n_classes, numCores = numCores) {
    df <- mclapply(
        X = counts$word, FUN = word_chi_squared, train.df.train = train.df.train,
        n_classes = n_classes, mc.cores = numCores
    )

    chi <- cbind(counts[, 1], as.data.frame(do.call(rbind, df)))

    return(chi)
}



feature_selection.chi_squared_per_mean <- function(
    counts, train.df.train, n_classes,
    frac = 0.5) {
    chi <- vocabulary_chi_squared(counts, train.df.train, n_classes, numCores)

    return(ranking_per_mean(chi, counts, n_classes, frac))
}




feature_selection.chi_squared_per_class <- function(
    counts, train.df.train, n_classes,
    frac = 0.5) {
    chi <- vocabulary_chi_squared(counts, train.df.train, n_classes, numCores)

    return(ranking_per_class(chi, counts, n_classes, frac))
}



word_mutual_info <- function(word, train.df.train, n_classes) {
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

    MI <- vector(length = n_classes)
    for (i in 1:n_classes) {
        n11 <- yw_c[yw_c$Labels == (i - 1), ]$c
        n10 <- sum(yw_c[yw_c$Labels != (i - 1), ]$c)
        n01 <- nw_c[yw_c$Labels == (i - 1), ]$c
        n00 <- sum(nw_c[nw_c$Labels != (i - 1), ]$c)
        n1_ <- n11 + n10
        n0_ <- n01 + n00
        n_1 <- n01 + n11
        n_0 <- n10 + n00
        n <- n11 + n01 + n10 + n00
        MI[i] <- n11 / n * log2(n * n11 / (n1_ * n_1)) + n01 / n * log2(n * n01 / (n0_ *
            n_1)) + n10 / n * log2(n * n10 / (n1_ * n_0)) + n00 / n * log2(n * n00 / (n0_ *
            n_0))
    }
    MI[is.na(MI)] <- 0

    return(MI)
}






vocabulary_mutual_info <- function(counts, train.df.train, n_classes, numCores = numCores) {
    df <- mclapply(
        X = counts$word, FUN = word_mutual_info, train.df.train = train.df.train,
        n_classes = n_classes, mc.cores = numCores
    )

    mutual_info <- cbind(counts[, 1], as.data.frame(do.call(rbind, df)))

    return(mutual_info)
}







feature_selection.mutual_info_per_class <- function(
    counts, train.df.train, n_classes,
    frac = 0.5) {
    mutual_info <- vocabulary_mutual_info(counts, train.df.train, n_classes, numCores)

    return(ranking_per_class(mutual_info, counts, n_classes, frac))
}




feature_selection.mutual_info_per_mean <- function(
    counts, train.df.train, n_classes,
    frac = 0.5) {
    mutual_info <- vocabulary_mutual_info(counts, train.df.train, n_classes, numCores)

    return(ranking_per_mean(mutual_info, counts, n_classes, frac))
}




likelihood.denominator <- function(naive.bayes.vocabulary) {
    naive.bayes.vocabulary %>%
        select(-word) %>%
        colSums() -> tot_counts_per_class

    den <- tot_counts_per_class + length(naive.bayes.vocabulary[[1]])
    return(den)
}




likelihood <- function(naive.bayes.vocabulary, den) {
    likelihood.token <- cbind(naive.bayes.vocabulary["word"], (naive.bayes.vocabulary[2:(n_classes +
        1)] + 1) / den)
    return(likelihood.token)
}




tweet_tokenize <- function(msg) {
    tib <- data.frame(msg)
    colnames(tib) <- "tweet"
    tib %>%
        unnest_tokens(word, tweet) %>%
        anti_join(get_stopwords(), by = join_by(word)) %>%
        cleaning_tokens() -> token.list
    return(token.list)
}




bayes.probability <- function(token.list, likelihood.token, prior.classes, den, n_classes) {
    query_out <- log(as.numeric(prior.classes))
    for (i in 1:length(token.list)) {
        query <- log(as.numeric(likelihood.token[likelihood.token["word"] == token.list$word[i]][2:(n_classes +
            1)]))
        if (is.na(query[1])) {
            # because if the word is not found, all NA are returned. if the
            # word is in the vocabulary the proper likelihood is returned
            query <- as.numeric(log(1 / den))
        }
        query_out <- query_out + query
    }
    query_out <- exp(query_out)
    query_out <- query_out / sum(query_out)
    return(query_out)
}




naive.bayes <- function(msg, likelihood.token, prior.classes, den, n_classes) {
    token.list <- as.vector(tweet_tokenize(msg))
    probability <- bayes.probability(
        token.list, likelihood.token, prior.classes,
        den, n_classes
    )
    predicted_class <- which.max(probability) - 1
    return(predicted_class)
}
