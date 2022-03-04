SeparateTerms <- function(Gram){
        # Gram <- inputString
        len <- length(strsplit(Gram,split = " ")[[1]])
        data.table(FirstTerms = paste0(strsplit(Gram,split=" ")[[1]][1:len-1],collapse="_"),
                   LastTerm = paste0(strsplit(Gram,split=" ")[[1]][len],collapse="_"))
}

