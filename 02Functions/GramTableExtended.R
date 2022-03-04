# Source: https://github.com/ThachNgocTran/KatzBackOffModelImplementationInR
GramTableExtended = function(GramTable){
        # Supposed table "threeGramTable" as above, we want to add a "discount" column.
        GramTable$discount = rep(1, nrow(GramTable))
        
        # Calculate the discount coefficient.
        # We only consider n-grams that have 0 < frequency <= k (5). Larger than 5: "Reliable enough".
        for(i in 5:1){
                currRTimes = i
                nextRTimes = currRTimes + 1
                
                currN = nrow(GramTable[frequency == currRTimes])
                nextN = nrow(GramTable[frequency == nextRTimes])
                
                currd = (nextRTimes / currRTimes) * (nextN / currN) # assumption: 0 < d < 1
                
                # the beauty of "data.table"!
                GramTable[frequency == currRTimes, discount := currd]
        }
        if(colnames(GramTable)[1] %in% "FirstTerms"){
                GramTable_leftOverProb = GramTable[, .(leftoverprob=LeftOverProb(LastTerm, frequency, discount)), 
                                                   by=FirstTerms]
                
        }
        return(GramTable)
       
}
# Calculate the remaining probability (thanks to discounting...).

