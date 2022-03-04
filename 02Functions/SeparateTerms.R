SeparateTerms <- function(x){
        # Gram <- inputString
        len <- length(strsplit(x,split = " ")[[1]])
        data.table(FirstTerms = paste0(strsplit(x,split=" ")[[1]][1:len-1],collapse="_"),
                   LastTerm = paste0(strsplit(x,split=" ")[[1]][len],collapse="_"))
        
        # _________________________________________________________________
        # 
        # # Pre-allocate
        # firstTerms = character(length(x))
        # lastTerm = character(length(x))
        # 
        # for(i in 1:length(x)){
        #         posOfSpaces = gregexpr("_", x[i])[[1]]
        #         posOfLastSpace = posOfSpaces[length(posOfSpaces)]
        #         firstTerms[i] = substr(x[i], 1, posOfLastSpace-1)
        #         lastTerm[i] = substr(x[i], posOfLastSpace+1, nchar(x[i]))
        # }
        # 
        # list(FirstTerms=firstTerms, LastTerm=lastTerm)
}


