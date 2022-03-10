source("02Functions/getQuadGramProb.R")
source("02Functions/getTriGramProb.R")
source("02Functions/getBiGramProb.R")
source("02Functions/getUniGramProb.R")

# Example
inputString <-  "Be grateful for the good times and keep the faith during the hard"

# Preprocessing
inputString_c <- removeWords(inputString,stopwords('en'))
inputString_c <- stripWhitespace(removeNumbers(removePunctuation(tolower(inputString_c),preserve_intra_word_dashes = TRUE)))

lastWords <- strsplit(inputString_c,split=" ")[[1]]
lastWords <- tail(lastWords[lastWords != ""],4)
length <- length(lastWords)

#____________________________________

system.time({
        if(length == 4){
                Pred <- ifelse(!inherits(try(getQuadGramProb(inputString),silent = TRUE),"try-error"),getQuadGramProb(inputString),
                               ifelse(!inherits(try(getTriGramProb(inputString),silent = TRUE),"try-error"),getTriGramProb(inputString),
                                      ifelse(!inherits(try(getBiGramProb(inputString),silent = TRUE),"try-error"),getBiGramProb(inputString),"Error")))
                print(paste0("With ngram = ",length," The probability of getting the word ","'",lastWords[length],"'", " is: ",Pred, ". Found in quadram"))
                
        } else if(length ==3){
                Pred <- ifelse(!inherits(try(getTriGramProb(inputString),silent = TRUE),"try-error"),getTriGramProb(inputString),
                               ifelse(!inherits(try(getBiGramProb(inputString),silent = TRUE),"try-error"),getBiGramProb(inputString),"Error"))
                print(paste0("With ngram = ",length," The probability of getting the word ","'",lastWords[length],"'", " is: ",Pred, ". Found in trigram"))
                
        } else if(length ==2) {
                Pred <-  ifelse(!inherits(try(getBiGramProb(inputString),silent = TRUE),"try-error"),getBiGramProb(inputString),"Error")
                print(paste0("With ngram = ",length," The probability of getting the word ","'",lastWords[length],"'", " is: ",Pred, ". Found in bigram"))
        } else{
                Pred <- ifelse(!inherits(try(getUniGramProb(inputString),silent = TRUE),"try-error"),getUniGramProb(inputString),"Error")
                print(paste0("With ngram = ",length," The probability of getting the word ","'",lastWords[length],"'", " is: ",Pred, ". Found in unigram"))
                
        }
        
})
