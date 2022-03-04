dtmToDf <- function(DocTermMatrix){
        freq <- colSums(DocTermMatrix)
        terms <- data.frame(term = names(freq), frequency = freq, stringsAsFactors = FALSE)
        terms <- arrange(terms, desc(frequency))
        rownames(terms) <- 1:length(freq)
        terms <- terms[!(is.na(terms$term) | terms$term==""), ]
        return(terms)
}