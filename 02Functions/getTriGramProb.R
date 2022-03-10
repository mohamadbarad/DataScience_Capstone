# Source: https://github.com/ThachNgocTran/KatzBackOffModelImplementationInR
getTriGramProb = function(inputString){
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
        oneGroupIn3Gram = TriGramTable_ex[TriGramTable_ex$FirstTerm == lastWords[length-2]
                                          & TriGramTable_ex$Midterm == lastWords[length-1]]
        
        if (nrow(oneGroupIn3Gram) > 0){
                # Algorithm here:
                oneRecordIn3Gram = TriGramTable_ex[TriGramTable_ex$FirstTerm == lastWords[length-2]
                                                   & TriGramTable_ex$Midterm == lastWords[length-1]
                                                   & TriGramTable_ex$LastTerm == lastWords[length]]
                if (nrow(oneRecordIn3Gram) > 0){
                        # We found one in 3-gram
                        all_freq = sum(oneGroupIn3Gram$frequency)
                        finalProb = ((oneRecordIn3Gram$discount * oneRecordIn3Gram$frequency) / all_freq)
                        predictionList =oneGroupIn3Gram[which(discount >= finalProb[1]),"LastTerm"]
                        
                        ### We're done!
                } else{
                        # Nothing found in 3 gram look in 2 gram
                        beta_leftoverprob = TriGramTable_leftOverProb_ex[TriGramTable_leftOverProb_ex$FirstTerm == lastWords[length-2]
                                                                         & TriGramTable_leftOverProb_ex$Midterm == lastWords[length-1]]$leftoverprob
                        
                        oneGroupIn2Gram = BiGramTable_ex[BiGramTable_ex$FirstTerm == lastWords[length-1]]
                        
                        oneRecordIn2Gram = BiGramTable_ex[BiGramTable_ex$FirstTerm == lastWords[length-1]
                                                          & BiGramTable_ex$LastTerm == lastWords[length]]
                        
                        if (nrow(oneRecordIn2Gram) > 0){
                                # We found one in 2-gram
                                # Only consider remaining
                                oneGroupIn2Gram_Remain = oneGroupIn2Gram[!(oneGroupIn2Gram$LastTerm %in% oneGroupIn3Gram$LastTerm)]
                                
                                all_freq = sum(oneGroupIn2Gram$frequency)
                                alpha = beta_leftoverprob / sum((oneGroupIn2Gram_Remain$frequency * oneGroupIn2Gram_Remain$discount) / all_freq)
                                finalProb = alpha * ((oneRecordIn2Gram$frequency * oneRecordIn2Gram$discount ) / all_freq)
                                predictionList =oneGroupIn2Gram[which(discount >= finalProb[1]),"LastTerm"]
                                
                        }else {
                                # We only have hope in 1-gram!
                                oneGroupIn1Gram = UniGramTable_ex # we don't have "FirstTerms" here!
                                oneRecordIn1Gram = UniGramTable_ex[LastTerm == lastWords[length]]
                                # oneRecordIn1Gram = UniGramTable[.(inLastTerm2gram),nomatch = NULL] # what if this returns "zero" row?
                                # oneRecordIn1Gram = UniGramTable[grepl(inLastTerm2gram, LastTerm, useBytes =  TRUE)& LastTerm !="",]
                                # oneRecordIn1Gram = UniGramTable[LastTerm %in% inLastTerm2gram]
                                if(nrow(oneRecordIn1Gram) > 0){
                                        oneGroupIn1Gram_Remain = oneGroupIn1Gram[!(oneGroupIn1Gram$LastTerm %in% oneGroupIn3Gram$LastTerm)]
                                        
                                        all_freq = sum(oneGroupIn1Gram$frequency)
                                        alpha = beta_leftoverprob / sum((oneGroupIn1Gram_Remain$frequency * oneGroupIn1Gram_Remain$discount) / all_freq)
                                        finalProb = alpha * ((oneRecordIn1Gram$frequency * oneRecordIn1Gram$discount) / all_freq)
                                        
                                        predictionList = oneGroupIn1Gram_Remain[which(discount >= finalProb[1]),"LastTerm"]
                                }
                                # predictionList[is.numeric(LastTerm)& LastTerm != ""]
                        }# ends 1 gram condition
                        
                } # ends 2 gram condition
        }# ends 3 gram condition
        else {
                stop(sprintf("[%s] not found in the 3-gram model.", lastWords[length]))
                # The workaround could be:
                # + Write another function in which we primarily use 2-gram with support from 1-gram.
                # + Increase the corpus size so that the 3-gram can capture more diversity of words...
        }
        ifelse(finalProb == -1,"No match", round(finalProb,5))
        
}

