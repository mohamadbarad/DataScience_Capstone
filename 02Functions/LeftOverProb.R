# Source: https://github.com/ThachNgocTran/KatzBackOffModelImplementationInR
LeftOverProb = function(LastTerm, frequency, discount){
        all_freq = sum(frequency)
        
        return(sort(1-sum((discount*frequency)/all_freq),decreasing = TRUE))
}