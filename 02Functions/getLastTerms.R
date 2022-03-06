getLastTerms = function(inputString,num){
        # inputString <- inputString
        # num <- 3
        

        len <- length(strsplit(inputString,split = " ")[[1]])
        if (len < num){
                stop("Insuffcient number of terms")
        }

        word(inputString,start = -num,end = -1,sep = " ")
}
        