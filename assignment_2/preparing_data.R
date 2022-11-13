library(tidyverse)

# Since all files are only containing one entry rather than several (checked using 'grep ER *') we don't check for several entries in a file

# Helper function which makes it easy to append a Vector to a List
lappend <- function (lst, ...){ 
  lst <- c(lst, list(...))
  return(lst)
}

# Load the RIS file given in [fileName] and create a dataframe from it
# Resulting data frame has columns key, value
readLinesToDataFrame<-function(fileName) {
  lines <- readLines(fileName) # read lines of file to a list
  listOfLines<-list() # create a new list where we can append the results of our processing to
  for(line in lines) {
    output <- str_split(line,"-",n=2) # str_split returns a list of character vectors 'TY  - CHAP' becomes c('TY ',' CHAP')
    key<-output[[1]][1]
    data<-output[[1]][2]
    key<-str_trim(key, side = "both") # str_trim removes whitespaces from both sides of the string 'TY ' becomes 'TY'
    data<-str_trim(data, side = "both") # str_trim removes whitespaces from both sides of the string ' CHAP' becomes 'CHAP'
    listOfLines<-lappend(listOfLines,c(key,data)) # construct a new list containig vectors with two values by appending vctor format=c('TY','CHAP')
  }
  df<-as.data.frame(do.call(rbind, listOfLines)) # create a dataframe out of the list we constrcuted before
  colnames(df)<-c('key','value') # rename the columns in the new dataframe to 'key' and 'value'
  # take the rows which have the same 'key' and concatenate their values by using ';' as a separator
  df <-df %>%
    group_by(key) %>%
    mutate(value = paste0(value, collapse = ";")) 
  
  df <-df[!duplicated(df$key), ] # remove all rows where 'key' is the same value except one
  return(df)
}

# Provide a column and a key. Returns true if key is present in column, otherwise return false
isPresentInColumn<-function(column,key){return(sum(str_detect(column, key)) > 0)}

# Append value of Row with key=key to appendToString if key is present in column
appendIfPresent<-function(appendToString,df,column,key){
  if(isPresentInColumn(column,key)){
    toAppend<-df[column==key,2]
    if(appendToString==''){
      appendToString<-toAppend
    }else {
      appendToString<-paste(appendToString,toAppend,sep = ';')
    }
  }
  return(appendToString)
}

# Combine all author fields to single String
consolidateAuthorsForSingleWork<-function(singleWorkDf) {
  allAuthors<-''
  allAuthors<-appendIfPresent(allAuthors,singleWorkDf,singleWorkDf$key,'AU')
  allAuthors<-appendIfPresent(allAuthors,singleWorkDf,singleWorkDf$key,'A1')
  allAuthors<-appendIfPresent(allAuthors,singleWorkDf,singleWorkDf$key,'A2')
  allAuthors<-appendIfPresent(allAuthors,singleWorkDf,singleWorkDf$key,'A3')
  allAuthors<-appendIfPresent(allAuthors,singleWorkDf,singleWorkDf$key,'A4')
  return(allAuthors)
}

# Convert a dataframe of one file to a row for the dataframe of all files
singleWorkToRow<-function(df) {
  TY<-df[df$key=='TY',2]
  TI<-df[df$key=='TI',2]
  # Check if PY is present, otherwise set NaN
  PY<-NaN
  if(isPresentInColumn(df$key,'PY')){
    PY<-df[df$key=='PY',2]
  }
  KW<-''
  if(isPresentInColumn(df$key,'KW')){
    KW<-df[df$key=='KW',2]
  }
  allAuthors<-consolidateAuthorsForSingleWork(df)
  authorCount<-length(str_split(allAuthors,";",simplify = TRUE))
  AB<-''
  if(isPresentInColumn(df$key,'AB')){
    AB<-df[df$key=='AB',2]
  }
  return(c(TY,PY,TI,KW,allAuthors,authorCount,AB))
}

# create empty dataframe to hold data
all_works_df<- data.frame(TY=character(0),
                          PY=numeric(0),
                          TI=character(0),
                          KW=character(0),
                          AU=character(0),
                          AuthorCount=numeric(0),
                          AB=character(0))

# Load all 50 files and create one dataframe from them
library(glue)
for(i in seq(from=1,to=50)) {
  df<-readLinesToDataFrame(glue('bib/work{i}.ris'))
  all_works_df[nrow(all_works_df) + 1,] = singleWorkToRow(df)
}
# Save our dataframe to an RDS file and load it again
outputFileName<-'allWorks.rds'
saveRDS(all_works_df, file = outputFileName)
# Restore the object
allWorksDf<-readRDS(file = outputFileName)











