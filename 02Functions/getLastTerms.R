getLastTerms = function(inputString,num){
        # string <- inputString
        # num <- 3
        word(inputString,start = -num,end = -1,sep = " ")
        # _________________________________________________________
        # 
        # # Preprocessing
        # inputString = gsub("[[:space:]]+", " ", str_trim(tolower(inputString)))
        # 
        # # Now, ready!
        # words = unlist(strsplit(inputString, " "))
        # 
        # if (length(words) < num){
        #         stop("Number of Last Terms: Insufficient!")
        # }
        # 
        # from = length(words)-num+1
        # to = length(words)
        # tempWords = words[from:to]
        # 
        # paste(tempWords, collapse="_")
}
        