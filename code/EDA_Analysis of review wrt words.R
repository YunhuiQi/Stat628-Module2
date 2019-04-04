library(wordcloud)

################# Read data cleaned ###############
#data_train_clean = read.csv("/Users/yunhui/Dropbox/628 module2/上传用/data/review_train_clean.csv")
data_train_clean = read.csv("../data/review_train_clean.csv")

## Due to different format of python and R, do some basic data cleaning(remove punctuation)
data_train_clean[,5]=as.character(data_train_clean[,5])
words_clean=list()
for (i in 1:length(data_train_clean[,5])) {
  data_train_clean[i,5]= gsub("\\[\\'","",data_train_clean[i,5])
  data_train_clean[i,5]
  data_train_clean[i,5]= gsub("\\'\\, \\'"," ",data_train_clean[i,5])
  data_train_clean[i,5]
  data_train_clean[i,5]= gsub("\\'\\]","",data_train_clean[i,5])
  data_train_clean[i,5]
  data_train_clean[i,5]= gsub("\\\"","",data_train_clean[i,5])
  data_train_clean[i,5]
  words_clean[[i]]= unlist(strsplit(data_train_clean[i,5], " "))
}


################# Do frequcney analysis for all data ################

toptenword_freq_all=sort(table(unlist(words_clean)),decreasing=T)[1:200]
plot(toptenword_freq_all,main="Top 10 frequent words in overall reviews")
wordcloud(words=names(toptenword_freq_all),freq=toptenword_freq_all,scale=c(3,.5),col=rainbow(length(toptenword_freq_all)))

## Explore top 10 words in reviews of different stars 
# one star review
data_clean_one = data_train_clean[data_train_clean[,3] == 1,]
words_clean_one = words_clean[which(data_train_clean[,3] == 1)]
(toptenword_freq_one=sort(table(unlist(words_clean_one)),decreasing=T)[1:10])
plot(toptenword_freq_one,main="Top 10 frequent words in * reviews")
# two star review
data_train_two = data_train_clean[data_train_clean[,3] == 2,]
words_clean_two = words_clean[which(data_train_clean[,3] == 2)]
(toptenword_freq_two = sort(table(unlist(words_clean_two)),decreasing=T)[1:10])
plot(toptenword_freq_two,main="Top 10 frequent words in ** reviews")

# three star review
data_train_three = data_train_clean[data_train_clean[,3] == 3,]
words_clean_three = words_clean[which(data_train_clean[,3] == 3)]
(toptenword_freq_three = sort(table(unlist(words_clean_three)),decreasing=T)[1:10])
plot(toptenword_freq_three,main="Top 10 frequent words in *** reviews")

# four star review
data_train_four = data_train_clean[data_train_clean[,3] == 4,]
words_clean_four = words_clean[which(data_train_clean[,3] == 4)]
(toptenword_freq_four = sort(table(unlist(words_clean_four)),decreasing=T)[1:10])
plot(toptenword_freq_four,main="Top 10 frequent words in **** reviews")

# five star review
data_train_five = data_train_clean[data_train_clean[,3] == 5,]
words_clean_five = words_clean[which(data_train_clean[,3] == 5)]
(toptenword_freq_five = sort(table(unlist(words_clean_five)),decreasing=T)[1:10])
plot(toptenword_freq_five,main="Top 10 frequent words in ***** reviews")



####################### TF_IDF values analysis #####################
# for each review, we compute TF_IDF value for all word, and pick the top 1,
# then combine all of chosen  ones and pick first 10 as our keywords.

##compute how many reviews in which this key appears
f_appear_num<-function(word,wordslistofvec){
  num=0
  for(i in 1:length(wordslistofvec)){
    d = wordslistofvec[[i]]
    if(word %in% d == TRUE){
      num = num+1
    }
  }
  return(num) 
}
 
##Compute tfidf vector for one review
tfidf <- function(revw,wordslistofvec,topnum){
  ##input is an element of words_clean and words_clean
  freq = vector()
  tf = vector()
  idf = vector()
  tfidfvalue = vector()
  t = table(revw)
  name = vector()
  for(key in 1:length(t)){
    freq[key] = t[key]
    num = f_appear_num(names(t[key]),wordslistofvec)
    tf[key] = freq[key]/length(revw)
    idf[key] = log(length(wordslistofvec)/(1+num)) 
    tfidfvalue[key]= tf[key]*idf[key]
    name[key] = names(t)[key]
  }
  tfidf=sort(tfidfvalue,decreasing = T)[1:topnum]
  word=name[order(tfidfvalue,decreasing = T)[1:topnum]]
  return(c(word,tfidf))
}
 

##pick top 10 keywords from all review
get_keywords<-function(topnum_per_review,topnum_all,dat){
  keywords<-vector()
  tfidf_all<-vector()
  for (i in 1:length(dat)) {
    print(i)
    x = tfidf(dat[[i]],dat,topnum_per_review)
    tfidf_all[i]<-as.numeric(x[2])
    keywords[i]<-as.character(x[1])
  }
  return(keywords[order(tfidf_all,decreasing = T)][1:topnum_all])
}

##find keywords for all review
get_keywords(1,10,words_clean)
##find keywords for * review
get_keywords(1,10,words_clean_one)
##find keywords for ** review
get_keywords(1,10,words_clean_two)
##find keywords for *** review
get_keywords(1,10,words_clean_three)
##find keywords for **** review
get_keywords(1,10,words_clean_four)
##find keywords for ***** review
get_keywords(1,10,words_clean_five)
