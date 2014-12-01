rm(list=ls()) #remove all variables from workspace


###########################################################################
#Given a file name, create a list variable that contains any necessary information
    #Input:  A file
    #Output:  A list
readPaper <- function(file){
  list <- unlist(scan(file, what = list(""), sep = ""))# Read in the text file.
  list <- gsub("[[:punct:]]", "", list) #remove all of the punctuation
  list <- tolower(list) # change all words to lowercase so "As" and "as" clump together as the same thing.
}
#read about the scan function here: 
    #1. http://www.ats.ucla.edu/stat/r/modules/raw_data.htm
    #2. R help
    #3. for gsub:  http://stackoverflow.com/questions/11498157/convert-punctuation-to-space
#########################

#Takes output from readPaper and a word (or a vector of words) and gives the frequency the frequency of the word
    #Input:  A file and a word
    #Output:  The frequency of the number
wordCount <- function(filelist, word){
  sum(filelist == word)  #make a vector of Trues/Falses for every word.  Sum up the matches(trues).
}
#Read about functions here:
  #1.  http://stackoverflow.com/questions/1923273/counting-the-number-of-elements-with-the-values-of-x-in-a-vector
#########################

#Takes a word and output from readPaper and gives the starting character position of that word indexed from the beginning of the paper.
    #Input:  A file and a word
    #Output:  A vector of the index of the beginning of each word placement
wordPlacement <- function(filelist, word){
  which(filelist == word)  #The which function gives the index of a logical object and outputs an array of indices.
}
#read about the which function here: 
    #1. R help
#########################

#Generates a frequency histogram of the 10 most frequent words in a file, can change the number of words most frequent words
    #Input:  Filelist and the top X words, default 10
    #Output:  An image of a histrogram
wordHist <- function(filelist, top = 10){
  pap <- as.data.frame(table(filelist))  #Make a data frame of all words in the list and its count.
  colnames(pap) <- c("word", "freq")  #Change column names to make them more accurate  
  arg <- order(pap$freq, decreasing = TRUE) #Order freq column by most to least abundant.
  pap <- pap[arg, ]  #Order dataframe by the most to least abundant based on freq column
  pap <- head(pap, n=top)  #take the number of rows from "top=?" input
  x <- barplot(pap$freq, names = pap$word, col = "royalblue", space = 1, #Make a barplot of frequency
               xaxt="n",xlab="", ylab = "Frequency", main = "Word Frequencies")
  labels <- pap$word  #Create vector of names
  text(x, x=x-.5, y=-3.5, labels = labels, srt = 45, pos = 1, xpd = TRUE) #Rotates labels so they look pretty.
}
#Sources:
  #1. http://www.dummies.com/how-to/content/how-to-sort-data-frames-in-r.html
  #2. http://haotu.wordpress.com/2013/07/09/angle-axis-x-labels-on-r-plot/
  #3. http://stackoverflow.com/questions/20241388/rotate-x-axis-labels-45-degrees-on-grouped-bar-plot-r
#########################

#Given a word, give the frequency of the words that follow it.
    #Input:  File and a word
    #Output: Vector of counts
nextWord <- function(filelist, word){
  something <- which(filelist == word)  #Make a vector of indices of the occurences of the word of interest
  something2 <- something + 1  #Get the index of the word that follows the word of interest.
  test<-(rep(NA,length(something2)))   #Create a vector with the length of occurrences of the word of interest
    for(i in 1:length(something2)){
        test[i]<-  filelist[something2[i]]  #Make a vector of all of the next words following the word of interest.
    }
  sort(table(test))  #Make a vector with next word and its counts and sort it by increasing abundance
}
#Sources:
  #1.  Help with graduate student: Daniel Katz in SNRE.
#########################

#  Given a word, give the freqency of words that preceed it
    #Input:  File and a word
    #Output:  Vector of Counts
previousWord <- function(filelist, word){
  something <- which(filelist == word)  #Make a vector of indices of the occurences of the word of interest
  something2 <- something - 1  #Get the index of the word that preceeds the word of interest.
  test<-(rep(NA,length(something2)))   #Create a vector with the length of occurrences of the word of interest
  for(i in 1:length(something2)){
    test[i]<-  filelist[something2[i]]  #Make a vector of all of the previous words before the word of interest.
  }
  sort(table(test))  #Make a vector with previous word and its counts and sort it by increasing abundance
}
#Sources:
    #1.  Help from graduate student: Daniel Katz in SNRE.
#########################

# This function takes a readPaper output filelist and outputs a histogram of the frequency of each letter in the alphabet
    #Input:  File processed through readPaper
    #Output: Histogram of letter frequency
surpriseMe <- function(filelist){
  letter_list <- toString(filelist) #Converts the list of words into a list of letters.
  letter_list <- gsub("[[:punct:]]", "", letter_list) #remove all of the punctuation.
  letter_list <- gsub("[[:space:]]", "", letter_list) #remove all spaces.
  letter_list <- gsub("[[:digit:]]", "", letter_list) #remove numerics.
  letter_list <- tolower(letter_list) #Just to make sure all letters are lowercase.
  oop <- strsplit(letter_list, split = "") #make each letter it's own unit.
  toop <- as.data.frame(table(oop)) #count each letter and make it a data frame.
  x <- barplot(toop$Freq, names.arg = toop$oop, col = "violetred", xaxt="n",
               xlab="Letter", ylab = "Frequency", main = "Letter Frequencies in filelist") #Plot the data with frequency on y axis and letter on x axis
  labels <- toop$oop  #Create vector of names
  text(x, x=x, y=-3.5, labels = labels, srt = 0, pos = 1, xpd = TRUE) #letter labels closer to x-axis.
}

#########################
###########################################################################
#END









