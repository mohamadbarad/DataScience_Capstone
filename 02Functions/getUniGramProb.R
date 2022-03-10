# Source: https://github.com/ThachNgocTran/KatzBackOffModelImplementationInR
getUniGramProb = function(inputString){
        # Example
        # inputString <- "he started telling me about his horticultural"
        
        # Preprocessing
        # inputString_c <- removeWords(inputString,stopwords('en'))
        # inputString_c <- stripWhitespace(removeNumbers(removePunctuation(tolower(inputString_c),preserve_intra_word_dashes = TRUE)))
        # 
        # lastWords <- strsplit(inputString_c,split=" ")[[1]]
        # lastWords <- tail(lastWords[lastWords != ""],4)
        # length <- length(lastWords)
        # 
        finalProb = -1
        
        
        oneGroupIn2Gram = BiGramTable_ex[FirstTerm == lastWords[length-1]]
        # We only have hope in 1-gram!
        oneGroupIn1Gram = UniGramTable_ex # we don't have "FirstTerms" here!
        oneRecordIn1Gram = UniGramTable_ex[LastTerm == lastWords[length]]
        if(nrow(oneRecordIn1Gram) > 0){
                oneGroupIn1Gram_Remain = oneGroupIn1Gram[!(oneGroupIn1Gram$LastTerm %in% oneGroupIn2Gram$LastTerm)]
                
                all_freq = sum(oneGroupIn1Gram$frequency)
                alpha = beta_leftoverprob / sum((oneGroupIn1Gram_Remain$frequency * oneGroupIn1Gram_Remain$discount) / all_freq)
                finalProb = alpha * ((oneRecordIn1Gram$frequency * oneRecordIn1Gram$discount) / all_freq)
                
                predictionList = oneGroupIn1Gram_Remain[which(discount >= finalProb[1]),"LastTerm"]
        }
        # predictionList[is.numeric(LastTerm)& LastTerm != ""]
        
        ### We're done!
        ifelse(finalProb == -1,"No match", round(finalProb,5))
        
}
