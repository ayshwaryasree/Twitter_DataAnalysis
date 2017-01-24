dt = read.csv("demonetization-tweets.csv")#read the csv file
dt_unique = data.frame(unique(dt$text))#getting unique tweets from text colummn
write.csv(dt_unique, "tweet2.csv")#creating csv file for unique tweets
dt = read.csv("tweet2.csv")#reading the unique tweets csv file
names(dt)#getting column names
dt_text = dt$text#getting tweets column
dt_text = gsub("(RT|via)((?:\\b\\W*@\\w+)+:+)", "",dt_text)#cleaning data
dt_text = gsub("[[:punct:]]", "", dt_text )#cleaning puntuations
dt_text = gsub("[[:digit:]]", "", dt_text)#cleaning digits
dt_text=gsub("http.*", "", (dt_text))#removing links
dt_text = gsub("[ \t]{2,}", "", dt_text)#cleaning data
dt_text = gsub("^\\s+|\\s+$", "", dt_text)#removing extra spaces
write.csv(dt_text, "tweet3.csv")#creating a csv file for cleaned tweets
df = read.csv("tweet3.csv")#reading csv file
names(df)#column in csv file
dt2_text = df$x#reading the tweets
try.error = function(x)#ifinding errr
{
  y = NA
  try_error = tryCatch(tolower(x), error=function(e) e)#to find errors
  if (!inherits(try_error, "error"))#if error not found
    y = tolower(x)#toconvert to lower case
  return(y)
}
dt2_text = sapply(dt2_text, try.error)#check allthe tweets if errors are there
dt2_text = dt2_text [!is.na(dt2_text)]#removing all na
dt2_text
library(syuzhet)#imprting library
dt2_word = get_tokens(dt2_text, pattern = ("\\W"))#dividing all tweets into words
dt2_word
sent_score <- get_sentiment((dt2_text), method="syuzhet")#sentiment score
sent_score
library(ggplot2)# importing library
qplot(sent_score, bins = 50, xlab = "sentiment score", ylab = "Number of Users",
      color = 1 : length(sent_score))#plot between sentiment score and users
ggplot(y = seq(1:length(sent_score)),x = sent_score)
qplot( sent_score,   type="l",   main="Example Plot Trajectory",   xlab = "Narrative Time", 
  ylab= "Emotional Valence"
)
dt2_word
sent_data = get_nrc_sentiment(dt2_word)#data frame of all sentiments
sent_data
barplot(
  sort(colSums(prop.table(sent_data[, 1:8]))),
  horiz = TRUE,
  cex.names = 0.7,
  las = 1,
  main = 'Emotions', xlab="percentage"
)#plot for sentimental analysis


