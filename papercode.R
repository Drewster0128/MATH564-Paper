library(jsonlite)
library(readr)
library(stringr)
#Load Scarcasm dataset
sarcasm_json_v1 <- read_file("datasets/Sarcasm_Headlines_Dataset.json")
sarcasm_json_v2 <- read_file("datasets/Sarcasm_Headlines_Dataset_v2.json")

#Combine into 1 large file
test <- str_flatten(c(sarcasm_json_v1, sarcasm_json_v2))
full_sarcasm_json <- write_file(str_flatten(c(sarcasm_json_v1, sarcasm_json_v2), collapse = "\n"), "datasets/full_sarcasm.json")


sarcasm <- read_file("datasets/full_sarcasm.json")
sarcasm <- strsplit(sarcasm, "\n")

sarcasm <- sapply(1:length(sarcasm[[1]]), function(i) {
  return(fromJSON(sarcasm[[1]][i]))
})

sarcasm <- t(sarcasm)
sarcasm <- data.frame(sarcasm)

#get list of all unique words in headlines
unique_words <- c()
for(headline in sarcasm$headline)
{
  words <- strsplit(headline, " ")[[1]]
  words <- sapply(words, function(word) {
    return(gsub("\\", "", word, fixed = TRUE))
  })
  for(word in words)
  {
    if(!word %in% unique_words)
    {
      unique_words <- append(unique_words, word)
    }
  }
}

one_hot_encodings <- matrix(nrow = nrow(sarcasm), ncol= length(unique_words))
colnames(one_hot_encodings) <- unique_words
for(i in 1:length(sarcasm$headline))
{
  headline_words <- strsplit(sarcasm$headline[[i]][1], " ")[[1]]
  for(j in 1:length(unique_words))
  {
    one_hot_encodings[i,j] <- sum(headline_words == unique_words[j])
  }
}
sarcasm <- cbind(sarcasm, one_hot_encodings)

#save this dataset to storage
write_csv(sarcasm, "datasets/clean_sarcasm.csv")