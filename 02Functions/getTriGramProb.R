# Source: https://github.com/ThachNgocTran/KatzBackOffModelImplementationInR
getTriGramProb = function(inputString){
        # Preprocessing
        inputString <- "help let us do"
        mylist = SeparateTerms(getLastTerms(inputString, num = 3))
        inFirstTerms3gram = mylist$FirstTerms
        inLastTerm3gram = mylist$LastTerm
        
        setkeyv(TriGramTable,c("FirstTerms","LastTerm"))
        setkeyv(BiGramTable,c("FirstTerms","LastTerm"))
        setkeyv(UniGramTable,"LastTerm")
        
        finalProb = -1
        # oneGroupIn3Gram = TriGramTable %>% filter(str_detect(FirstTerms,inFirstTerms3gram))
        oneGroupIn3Gram = TriGramTable[FirstTerms == inFirstTerms3gram]
        if (nrow(oneGroupIn3Gram) > 0){
                # Algorithm here
                # oneRecordIn3Gram  = TriGramTable %>% 
                #         filter(str_detect(FirstTerms,inFirstTerms3gram)& 
                #                        str_detect(LastTerm,inLastTerm3gram))
                oneRecordIn3Gram = TriGramTable[FirstTerms == inFirstTerms3gram & LastTerm == inLastTerm3gram]
                if (nrow(oneRecordIn3Gram) > 0){
                        # We found one in 3-gram
                        all_freq = sum(oneGroupIn3Gram$frequency)
                        finalProb = ((oneRecordIn3Gram$discount * oneRecordIn3Gram$frequency) / all_freq)
                        predictionList =oneRecordIn3Gram$LastTerm
                        
                        ### We're done!
                } else {
                        # NOT found in 3-gram => check 2-gram & 1-gram
                        mylist = SeparateTerms(getLastTerms(inputString, num = 2))
                        inFirstTerms2gram = mylist$FirstTerms
                        inLastTerm2gram = mylist$LastTerm
                        
                        # Get the left-over probability so that we can distribute it for lower-order grams.
                        # beta_leftoverprob = TriGramTable_leftOverProb %>% 
                        #         filter(str_detect(FirstTerms,inFirstTerms3gram)) %>% 
                        #         select(leftoverprob)
                        beta_leftoverprob = TriGramTable_leftOverProb [FirstTerms == inFirstTerms3gram]$leftoverprob
                        # system.time({
                        # oneGroupIn2Gram = BiGramTable %>%
                        #         filter(str_detect(FirstTerms,inFirstTerms2gram))
                        # })
                        # system.time({
                        # oneGroupIn2Gram = BiGramTable [FirstTerms == inFirstTerms2gram]
                        # }) # 2.18 sek
                        
                        setkeyv(BiGramTable,c("FirstTerms",))
                        
                        # system.time({
                                oneGroupIn2Gram = BiGramTable[inFirstTerms2gram]
                        # }) # 0.03 sek
                        # system.time({
                        #         oneGroupIn2Gram = BiGramTable [FirstTerms %like% inFirstTerms2gram]
                        # }) # 104 sekunder
                        
                        # system.time({
                        #         oneGroupIn2Gram = BiGramTable [grepl(inFirstTerms2gram, 
                        #                                              FirstTerms, 
                        #                                              fixed = TRUE),]
                        # }) # 7 sekunder
                        
                        # oneRecordIn2Gram = BiGramTable %>% 
                        #         filter(str_detect(FirstTerms,inFirstTerms2gram)&
                        #                        str_detect(LastTerm,inLastTerm2gram))
                        oneRecordIn2Gram = BiGramTable[FirstTerms == inFirstTerms2gram & LastTerm == inLastTerm2gram]
                        if (nrow(oneRecordIn2Gram) > 0){
                                # We found one in 2-gram!
                                # We only consider ones that do not appear in 3-grams...
                                oneGroupIn2Gram_Remain = oneGroupIn2Gram[!(oneGroupIn2Gram$LastTerm == oneGroupIn3Gram$LastTerm)]
                                all_freq = sum(oneGroupIn2Gram$frequency)
                                
                                alpha = beta_leftoverprob / sum((oneGroupIn2Gram_Remain$frequency * oneGroupIn2Gram_Remain$discount) / all_freq)
                                
                                finalProb = alpha * ((oneRecordIn2Gram$frequency * oneRecordIn2Gram$discount ) / all_freq)
                                
                                predictionList =oneRecordIn2Gram$LastTerm
                                ### We're done!
                        } else {
                                # We only have hope in 1-gram!
                                oneGroupIn1Gram = UniGramTable # we don't have "FirstTerms" here!
                                # oneRecordIn1Gram = UniGramTable %>% 
                                #         filter(str_detect(LastTerm,inLastTerm2gram))
                                oneRecordIn1Gram = UniGramTable[LastTerm == inLastTerm2gram] # what if this returns "zero" row?
                                
                                oneGroupIn1Gram_Remain = oneGroupIn1Gram[!(oneGroupIn1Gram$LastTerm == oneGroupIn3Gram$LastTerm)]
                                all_freq = sum(oneGroupIn1Gram$frequency)
                                
                                alpha = beta_leftoverprob / sum((oneGroupIn1Gram_Remain$frequency * oneGroupIn1Gram_Remain$discount) / all_freq)
                                
                                finalProb = alpha * ((oneRecordIn1Gram$frequency * oneRecordIn1Gram$discount) / all_freq)
                                
                                predictionList =oneRecordIn1Gram$LastTerm
                                ### We're done!
                        }
                }
        } else {
                stop(sprintf("[%s] not found in the 3-gram model.", inFirstTerms3gram))
                # The workaround could be:
                # + Write another function in which we primarily use 2-gram with support from 1-gram.
                # + Increase the corpus size so that the 3-gram can capture more diversity of words...
        }
        
        paste0("The next possible words is/are: ","'",predictionList[1:5],"'",". With probabiliy ",round(finalProb[1:5],4))
        
}
