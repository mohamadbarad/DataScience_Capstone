SeparateTerms <- function(x){
        # x <- inputString
        len <- length(strsplit(x,split = " ")[[1]])
        data.table(FirstTerms = paste0(strsplit(x,split=" ")[[1]][1:len-1],collapse="_"),
                   LastTerm = paste0(strsplit(x,split=" ")[[1]][len],collapse="_"))

}


