# Source: https://github.com/ThachNgocTran/KatzBackOffModelImplementationInR
getTriGramProb = function(inputString){
        # Preprocessing
        # inputString <- "monkey know how"
        mylist = SeparateTerms(getLastTerms(inputString, num = 3))
        inFirstTerms3gram = mylist$FirstTerms
        inFirstTerms3gram = str_split(inFirstTerms3gram,pattern = "_",n = 2,simplify = TRUE)
        inLastTerm3gram = mylist$LastTerm
        
        finalProb = -1
        oneGroupIn3Gram = TriGramTable[grepl(pattern = paste0(inFirstTerms3gram,collapse = "|"),
                                             x = FirstTerms,useBytes = TRUE),]
        # oneGroupIn3Gram = TriGramTable[.(inFirstTerms3gram),nomatch = NULL]
        # oneGroupIn3Gram = TriGramTable[FirstTerms %in% inFirstTerms3gram]
        if (nrow(oneGroupIn3Gram) > 0){
                # Algorithm here
                
                # oneRecordIn3Gram = TriGramTable[.(inFirstTerms3gram,inLastTerm3gram),nomatch = NULL]
                oneRecordIn3Gram = TriGramTable[grepl(pattern = paste0(inFirstTerms3gram,collapse = "|"),
                                                     x = FirstTerms,useBytes = TRUE) &
                                                grepl(pattern = paste0(inLastTerm3gram,collapse = "|"),
                                                     x = LastTerm,useBytes = TRUE) ,]                
                if (nrow(oneRecordIn3Gram) > 0){
                        # We found one in 3-gram
                        all_freq = sum(oneGroupIn3Gram$frequency)
                        finalProb = ((oneRecordIn3Gram$discount * oneRecordIn3Gram$frequency) / all_freq)
                        predictionList =oneGroupIn3Gram[which(discount >= finalProb[1]),"LastTerm"]
                        return(predictionList)
                        ### We're done!
                } else {
                        # NOT found in 3-gram => check 2-gram & 1-gram
                        mylist = SeparateTerms(getLastTerms(inputString, num = 2))
                        inFirstTerms2gram = mylist$FirstTerms
                        inLastTerm2gram = mylist$LastTerm
                        
                        # Get the left-over probability so that we can distribute it for lower-order grams.
                        # beta_leftoverprob = TriGramTable_leftOverProb[.(inFirstTerms3gram),nomatch = NULL]$leftoverprob
                        beta_leftoverprob = TriGramTable_leftOverProb[grepl(pattern = paste0(inFirstTerms3gram,collapse = "|"),
                                                                            x= FirstTerms,useBytes = TRUE)]$leftoverprob
                        
                        # oneGroupIn2Gram = BiGramTable[.(inFirstTerms2gram),nomatch = NULL]
                        oneGroupIn2Gram = BiGramTable[grepl(pattern = inFirstTerms2gram,
                                                            x= FirstTerms,useBytes = TRUE),]
                        # oneRecordIn2Gram = BiGramTable[.(inFirstTerms2gram,inLastTerm2gram),nomatch = NULL]
                        oneRecordIn2Gram = TriGramTable[grepl(pattern = inFirstTerms2gram,
                                                              x = FirstTerms,useBytes = TRUE) &
                                                                grepl(pattern = inLastTerm2gram,
                                                                      x = LastTerm,useBytes = TRUE),]   
                        
                        if (nrow(oneRecordIn2Gram) > 0){
                                # We found one in 2-gram!
                                # We only consider ones that do not appear in 3-grams...
                                oneGroupIn2Gram_Remain = oneGroupIn2Gram[!(oneGroupIn2Gram$LastTerm %in% oneGroupIn3Gram$LastTerm)]
                                
                                all_freq = sum(oneGroupIn2Gram$frequency)
                                alpha = beta_leftoverprob / sum((oneGroupIn2Gram_Remain$frequency * oneGroupIn2Gram_Remain$discount) / all_freq)
                                
                                finalProb = alpha * ((oneRecordIn2Gram$frequency * oneRecordIn2Gram$discount ) / all_freq)
                                
                                predictionList =oneGroupIn2Gram_Remain[which(discount >= finalProb[1]),"LastTerm"]
                                return(predictionList)
                                ### We're done!
                        } else {
                                # We only have hope in 1-gram!
                        
                                oneGroupIn1Gram = UniGramTable[LastTerm !=""] # we don't have "FirstTerms" here!
                                
                                # oneRecordIn1Gram = UniGramTable[.(inLastTerm2gram),nomatch = NULL] # what if this returns "zero" row?
                                oneRecordIn1Gram = UniGramTable[grepl(inLastTerm2gram, LastTerm, useBytes =  TRUE)& LastTerm !="",]
                                # oneRecordIn1Gram = UniGramTable[LastTerm %in% inLastTerm2gram]
                                
                                oneGroupIn1Gram_Remain = oneGroupIn1Gram[!(oneGroupIn1Gram$LastTerm %in% oneGroupIn3Gram$LastTerm)]
                                all_freq = sum(oneGroupIn1Gram$frequency)
                                
                                alpha = beta_leftoverprob / sum((oneGroupIn1Gram_Remain$frequency * oneGroupIn1Gram_Remain$discount) / all_freq)
                                
                                finalProb = alpha * ((oneRecordIn1Gram$frequency * oneRecordIn1Gram$discount) / all_freq)
                                
                                predictionList = oneGroupIn1Gram_Remain[which(discount >= finalProb[1]),"LastTerm"]
                                predictionList[is.numeric(LastTerm)& LastTerm != ""]
                                return(predictionList[is.numeric(LastTerm)& LastTerm != ""])
                                ### We're done!
                        }
                        return(predictionList)
                }
        } else {
                stop(sprintf("[%s] not found in the 3-gram model.", inFirstTerms3gram))
                # The workaround could be:
                # + Write another function in which we primarily use 2-gram with support from 1-gram.
                # + Increase the corpus size so that the 3-gram can capture more diversity of words...
        }
        return(predictionList)
        
        ifelse(nrow(predictionList[is.numeric(LastTerm)])==0,"No Match",predictionList[!is.numeric(LastTerm)])
        
        # paste0("The next possible words is/are: ","'",
        #        ifelse(is.na(predictionList[1]),"No Match",predictionList[1])
        #        ,"'",". With probabiliy ",
        #        ifelse(is.na(round(finalProb[1],4)),"No Match",round(finalProb[1],4)))

       
}
