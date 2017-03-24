library(shiny);
library(rdrop2) #Dropbox connection
library(memoise) #cache Function
library(dplyr);
library(mice) # imputation
library(randomForest) # classification algorithm
library(ggplot2)
library(DT)
library(scales)
library(ggthemes)
library(rpart)
library(data.table)
library(lubridate)

#getwd()
#token <- drop_auth()
#saveRDS(token, "droptoken.rds")
# Upload droptoken to your server
# ******** WARNING ********
# Losing this file will give anyone 
# complete control of your Dropbox account
# You can then revoke the rdrop2 app from your
# dropbox account and start over.
# ******** WARNING ********
# read it back with readRDS
token <- readRDS("droptoken.rds")
# Then pass the token to each drop_ function
drop_acc(dtoken = token)

domainData <<- list('titanic' = 'titanic', 'animal' = 'animal')

cachedResult <- memoise(function(data) {
  
  if (!(data %in% domainData))
    stop("Unknown data")
  
  #Dropbox 폴더 이름으로 설정
  switch(data, 
         titanic={
           outputDir <<- "shinyDropbox/titanic"
         },
         animal={
           outputDir <<- "shinyDropbox/animal"
         },
         {
           outputDir <<- NULL
           print('Choice domain data!')
         }
  )#End Switch
  
  if(!is.null(outputDir)){
      a<-loadData()
      answer <<- a[[1]]
      test <<- a[[2]]
      train <<- a[[3]]
      outputDir<-NULL
  }
  
})

saveData <- function(data) {
  data <- t(data)
  # Create a unique file name
  fileName <- sprintf("%s_%s.csv", as.integer(Sys.time()), digest::digest(data))
  # Write the data to a temporary file locally
  filePath <- file.path(tempdir(), fileName)
  write.csv(data, filePath, row.names = FALSE, quote = TRUE)
  # Upload the file to Dropbox
  drop_upload(filePath, dest = outputDir)
}

loadData <- function() {
  # Read all the files into a list
  filesInfo <- drop_dir(outputDir)
  filePaths <- filesInfo$path
  data <- lapply(filePaths, drop_read_csv, stringsAsFactors = FALSE)
  # Concatenate all data together into one data.frame
  #data <- do.call(rbind, data)
  data
}
