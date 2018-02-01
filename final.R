library(rvest)
library(stringr)
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)                                  ##내용 표현에 자주 쓰이는 태그:a,b,p,th,td,h1,h2,h3,div,span,li,strong,pre,dd,dt,label
                                                #URL 후에 완성후 모든 파일을 처리하도록 반복 
rm(list=ls())  #clean mem
src_dir <- c("R/Webpage Dataset/Webpage Dataset/")
a=NA
src_file <- list.files(src_dir) #workspace 속 web data set의 파일들을 list로 
length(src_file) # 11000개 데이터셋   --> 작동 중지 . 횟수를 나눠 실행해보


for(i in 1:11000){
 url<-paste("R/Webpage Dataset/Webpage Dataset/",src_file[i],sep="") #문자열 or 변수(문자열) 붙이기 sep을 "" 설정하여 공백없앰
 tag.title<-url %>%
   read_html(options="HUGE")%>%
   html_node("title")%>%       ##html_node는 하나의 요소 반환 / html_nodes는 해당되는 모든 요소 반환
   html_text()
 #tag.title
 data.before=tag.title
 
 
 tag.a<-url %>%
   read_html(options="HUGE")%>%
   html_nodes("a")%>%   
   html_text()
 #tag.a
 data.before=c(data.before,tag.a)
 
 
 tag.b<-url %>%
   read_html(options="HUGE")%>%
   html_nodes("b")%>%   
   html_text()
 #tag.b
 data.before=c(data.before,tag.b)
 
 
 tag.p<-url %>%
   read_html(options="HUGE")%>%
   html_nodes("p")%>%   
   html_text()
 #tag.p
 data.before=c(data.before,tag.p)
 
 tag.th<-url %>%
   read_html(options="HUGE")%>%
   html_nodes("th")%>%   
   html_text()
 #tag.th
 data.before=c(data.before,tag.th)
 
 
 tag.td<-url %>%
   read_html(options="HUGE")%>%
   html_nodes("td")%>%   
   html_text()
 #tag.td
 data.before=c(data.before,tag.td)
 
 tag.h1<-url %>%
   read_html(options="HUGE")%>%
   html_nodes("h1")%>%   
   html_text()
 #tag.h1
 data.before=c(data.before,tag.h1)
 
 tag.h2<-url %>%
   read_html(options="HUGE")%>%
   html_nodes("h2")%>%   
   html_text()
 #tag.h2
 data.before=c(data.before,tag.h2)
 
 tag.h3<-url %>%
   read_html(options="HUGE")%>%
   html_nodes("h3")%>%   
   html_text()
 #tag.h3
 data.before=c(data.before,tag.h3)
 
 tag.div<-url %>%
   read_html(options="HUGE")%>%
   html_nodes("div")%>%   
   html_text()
 #tag.div
 data.before=c(data.before,tag.div)
 
 tag.span<-url %>%
   read_html(options="HUGE")%>%
   html_nodes("span")%>%   
   html_text()
 #tag.span
 data.before=c(data.before,tag.span)
 
 tag.li<-url %>%
   read_html(options="HUGE")%>%
   html_nodes("li")%>%   
   html_text()
 #tag.li
 data.before=c(data.before,tag.li)
 
 tag.strong<-url %>%
   read_html(options="HUGE")%>%
   html_nodes("strong")%>%   
   html_text()
 #tag.strong
 data.before=c(data.before,tag.strong)
 
 
 tag.pre<-url %>%
   read_html(options="HUGE")%>%
   html_nodes("pre")%>%   
   html_text()
 #tag.pre
 data.before=c(data.before,tag.pre)
 
 tag.dd<-url %>%
   read_html(options="HUGE")%>%
   html_nodes("dd")%>%   
   html_text()
 #tag.dd
 data.before=c(data.before,tag.dd)
 
 tag.dt<-url %>%
   read_html(options="HUGE")%>%
   html_nodes("dt")%>%   
   html_text()
 #tag.dt
 data.before=c(data.before,tag.dt)
 
 tag.label<-url %>%
   read_html(options="HUGE")%>%
   html_nodes("label")%>%   
   html_text()
 #tag.label
 data.before=c(data.before,tag.label)
 
 data.before%>%
   str_replace_all(pattern = "\r\n", replacement = " ") %>%
   str_replace_all(pattern = "[\\^]", replacement = " ") %>%   #클리니
   str_replace_all(pattern = "\"", replacement = " ") %>%
   str_replace_all(pattern = "\\s+", replacement = " ") %>%
   str_replace_all(pattern = "\t", replacement = "") %>%
   str_trim(side = "both")
 data<-data.before

 ############################################################################################################################

 
 
 text.refine <- Corpus(VectorSource(data))
 #inspect(text.refine)
 
 
 toSpace <- content_transformer(function (x , pattern) gsub(pattern, " ", x))
 text.refine <- tm_map(text.refine, toSpace, "/")
 text.refine <- tm_map(text.refine, toSpace, "@")   #클리닝 2
 text.refine <- tm_map(text.refine, toSpace, "\\|")
 
 #text.refine
 #inspect(text.refine)
 # 소문자 통일 
 text.refine <- tm_map(text.refine, content_transformer(tolower))
 # 숫자 제거 
 text.refine <- tm_map(text.refine, removeNumbers)
 # stopwords 설정 
 text.refine <- tm_map(text.refine, removeWords, stopwords("english"))
 text.refine <- tm_map(text.refine, removeWords, c("will","may","can","use","give","just","make","must","every","also","take","is","href","div","else")) 
 # 구두점 제거 
 text.refine <- tm_map(text.refine, removePunctuation)
 # 불필요한 공백 제거 
 text.refine <- tm_map(text.refine, stripWhitespace)
 # 형태소 어근 추출하기
 text.refine <- tm_map(text.refine, stemDocument)
 
 
 dtm <- TermDocumentMatrix(text.refine)
 
 #m <- as.matrix(dtm)
 #v <- sort(rowSums(m),decreasing=TRUE)
 #d <- data.frame(word = names(v),freq=v)
 #head(d, 100)  #단어 카운팅 100개 값 보이기
 
 
 a=c(a,head(findFreqTerms(dtm, lowfreq = 7),50)) #각 파일에서 많이 나오는 단어 순 앞에 30개가 가장 적당 하네 

 class.refine<- Corpus(VectorSource(a)) #각 파일 최다빈도수 30개 리스트들을 내에서 한번더 카운팅 
 class<-TermDocumentMatrix(class.refine)
 
 m <- as.matrix(class)
 v <- sort(rowSums(m),decreasing=TRUE)
 d <- data.frame(word = names(v),freq=v)
 #head(d, 100)  #단어 카운팅 100개 값 보이기
 if(i==1000){
 A_class<-head(d,100)
 a=NA
 }else if(i==2000){
   B_class<-head(d,100)
   a=NA
 }else if(i==3000){
   C_class<-head(d,100)
   a=NA
 }else if(i==4000){
   D_class<-head(d,100)
   a=NA
 }else if(i==5000){
   E_class<-head(d,100)
   a=NA
 }else if(i==6000){
   F_class<-head(d,100)
   a=NA
 }else if(i==7000){
   G_class<-head(d,100)
   a=NA
 }else if(i==8000){
   H_class<-head(d,100)
   a=NA
 }else if(i==9000){
   I_class<-head(d,100)
   a=NA
 }else if(i==10000){
   J_class<-head(d,100)
   a=NA
 }else if(i==11000){
   X_class<-head(d,100)
   a=NA
 }
 print(i)
}


#추가 end
###### word cloud와 함께 그 결과를 pdf output하기 #############################
# set.seed(12)
 #path<- paste("R/pdf/",src_file[i],".pdf",sep="")
 #pdf(path)
 
 #wordcloud(words = d$word, freq = d$freq, min.freq = 7,max.words=200, random.order=FALSE, rot.per=0.35, 
  #         colors=brewer.pal(8, "Dark2"))   #word cloud 
 
# dev.off()
##############################################################################
#}#기존 end



counting<-function(arg,classes,counts){
  if(arg==classes){
    counts=counts+1
    return(counts)
  }
  return(counts)
}

src_dir <- c("R/Webpage Dataset/Webpage Dataset/")
a=NA
src_file <- list.files(src_dir) 
length(src_file)

pages = matrix(nrow=11000, ncol=11)
labels = matrix(nrow=11000, ncol=1)
for(i in 1:11000){
  url<-paste("R/Webpage Dataset/Webpage Dataset/",src_file[i],sep="") 
  tag.title<-url %>%
    read_html(options="HUGE")%>%
    html_node("title")%>%   
    html_text()
  #tag.title
  data.before=tag.title
  
  
  tag.a<-url %>%
    read_html(options="HUGE")%>%
    html_nodes("a")%>%   
    html_text()
  #tag.a
  data.before=c(data.before,tag.a)
  
  
  tag.b<-url %>%
    read_html(options="HUGE")%>%
    html_nodes("b")%>%   
    html_text()
  #tag.b
  data.before=c(data.before,tag.b)
  
  
  tag.p<-url %>%
    read_html(options="HUGE")%>%
    html_nodes("p")%>%   
    html_text()
  #tag.p
  data.before=c(data.before,tag.p)
  
  tag.th<-url %>%
    read_html(options="HUGE")%>%
    html_nodes("th")%>%   
    html_text()
  #tag.th
  data.before=c(data.before,tag.th)
  
  
  tag.td<-url %>%
    read_html(options="HUGE")%>%
    html_nodes("td")%>%   
    html_text()
  #tag.td
  data.before=c(data.before,tag.td)
  
  tag.h1<-url %>%
    read_html(options="HUGE")%>%
    html_nodes("h1")%>%   
    html_text()
  #tag.h1
  data.before=c(data.before,tag.h1)
  
  tag.h2<-url %>%
    read_html(options="HUGE")%>%
    html_nodes("h2")%>%   
    html_text()
  #tag.h2
  data.before=c(data.before,tag.h2)
  
  tag.h3<-url %>%
    read_html(options="HUGE")%>%
    html_nodes("h3")%>%   
    html_text()
  #tag.h3
  data.before=c(data.before,tag.h3)
  
  tag.div<-url %>%
    read_html(options="HUGE")%>%
    html_nodes("div")%>%   
    html_text()
  #tag.div
  data.before=c(data.before,tag.div)
  
  tag.span<-url %>%
    read_html(options="HUGE")%>%
    html_nodes("span")%>%   
    html_text()
  #tag.span
  data.before=c(data.before,tag.span)
  
  tag.li<-url %>%
    read_html(options="HUGE")%>%
    html_nodes("li")%>%   
    html_text()
  #tag.li
  data.before=c(data.before,tag.li)
  
  tag.strong<-url %>%
    read_html(options="HUGE")%>%
    html_nodes("strong")%>%   
    html_text()
  #tag.strong
  data.before=c(data.before,tag.strong)
  
  
  tag.pre<-url %>%
    read_html(options="HUGE")%>%
    html_nodes("pre")%>%   
    html_text()
  #tag.pre
  data.before=c(data.before,tag.pre)
  
  tag.dd<-url %>%
    read_html(options="HUGE")%>%
    html_nodes("dd")%>%   
    html_text()
  #tag.dd
  data.before=c(data.before,tag.dd)
  
  tag.dt<-url %>%
    read_html(options="HUGE")%>%
    html_nodes("dt")%>%   
    html_text()
  #tag.dt
  data.before=c(data.before,tag.dt)
  
  tag.label<-url %>%
    read_html(options="HUGE")%>%
    html_nodes("label")%>%   
    html_text()
  #tag.label
  data.before=c(data.before,tag.label)
  
  data.before%>%
    str_replace_all(pattern = "\r\n", replacement = " ") %>%
    str_replace_all(pattern = "[\\^]", replacement = " ") %>%   #클리닝 
    str_replace_all(pattern = "\"", replacement = " ") %>%
    str_replace_all(pattern = "\\s+", replacement = " ") %>%
    str_replace_all(pattern = "\t", replacement = "") %>%
    str_trim(side = "both")
  data<-data.before
  
  ############################################################################################################################
  
  
  
  text.refine <- Corpus(VectorSource(data))
  #inspect(text.refine)
  
  
  toSpace <- content_transformer(function (x , pattern) gsub(pattern, " ", x))
  text.refine <- tm_map(text.refine, toSpace, "/")
  text.refine <- tm_map(text.refine, toSpace, "@")   #클리닝 2
  text.refine <- tm_map(text.refine, toSpace, "\\|")
  
  #text.refine
  #inspect(text.refine)
  # 소문자 통일 
  text.refine <- tm_map(text.refine, content_transformer(tolower))
  # 숫자 제거 
  text.refine <- tm_map(text.refine, removeNumbers)
  # stopwords 설정 
  text.refine <- tm_map(text.refine, removeWords, stopwords("english"))
  text.refine <- tm_map(text.refine, removeWords, c("will","may","can","use","give","just","make","must","every","also","take","is","href","div","else")) 
  # 구두점 제거 
  text.refine <- tm_map(text.refine, removePunctuation)
  # 불필요한 공백 제거 
  text.refine <- tm_map(text.refine, stripWhitespace)
  # 형태소 어근 추출하기
  text.refine <- tm_map(text.refine, stemDocument)
  
  
  dtm <- TermDocumentMatrix(text.refine)
  
  #m <- as.matrix(dtm)
  #v <- sort(rowSums(m),decreasing=TRUE)
  #d <- data.frame(word = names(v),freq=v)
  #head(d, 100)  #단어 카운팅 100개 값 보이기
  
  
  a=head(findFreqTerms(dtm, lowfreq = 7),50) #각 파일에서 많이 나오는 단어 순 앞에 30개가 가장 적당 
  
  print(i)


a_count=0
b_count=0
c_count=0
d_count=0
e_count=0
f_count=0
g_count=0
h_count=0
i_count=0
j_count=0
x_count=0
if(length(a)==0)
  next

for(q in 1:length(a)){
  
  for(j in 1:100){
    a_count<-counting(a[q],A_class[j,1],a_count)
    b_count<-counting(a[q],B_class[j,1],b_count)
    c_count<-counting(a[q],C_class[j,1],c_count)
    d_count<-counting(a[q],D_class[j,1],d_count)
    e_count<-counting(a[q],E_class[j,1],e_count)
    f_count<-counting(a[q],F_class[j,1],f_count)
    g_count<-counting(a[q],G_class[j,1],g_count)
    h_count<-counting(a[q],H_class[j,1],h_count)
    i_count<-counting(a[q],I_class[j,1],i_count)
    j_count<-counting(a[q],J_class[j,1],j_count)
    x_count<-counting(a[q],X_class[j,1],x_count)
    
   
  }
  
}
if(i<=1000){
  label="A"
}else if(1000<i&i<=2000){
  label="B"
}else if(2000<i&i<=3000){
  label="C"
}else if(3000<i&i<=4000){
  label="D"
}else if(4000<i&i<=5000){
  label="E"
}else if(5000<i&i<=6000){
  label="F"
}else if(6000<i&i<=7000){
  label="G"
}else if(7000<i&i<=8000){
  label="H"
}else if(8000<i&i<=9000){
  label="I"
}else if(9000<i&i<=10000){
  label="J"
}else if(10000<i&i<=11000){
  label="X"
}
labels[i]=label

pages[i,]<-c(a_count,b_count,c_count,d_count,e_count,f_count,g_count,h_count,i_count,j_count,x_count)
a_count=0
b_count=0
c_count=0
d_count=0
e_count=0
f_count=0
g_count=0
h_count=0
i_count=0
j_count=0
x_count=0

}
#######data frame 으로 생성하지 않았어서 다시 생성......#####.
df<-data.frame(classA= numeric(0),classB= numeric(0),classC= numeric(0),classD= numeric(0),classE= numeric(0),classF= numeric(0),classG= numeric(0)
               ,classH= numeric(0),classI= numeric(0),classJ= numeric(0),classX= numeric(0),label=numeric(0))
for(i in 1:11000){
  df[i,]<-c(pages[i,1],pages[i,2],pages[i,3],pages[i,4],pages[i,5],pages[i,6],pages[i,7],
            pages[i,8],pages[i,9],pages[i,10],pages[i,11],NA)
  df[i,12]=labels[i]
  
}

df$label <- factor(df$label)
str(df)
df_NA=na.omit(df)
str(df_NA)
df_NA
library(e1071)
library(party)
library(nnet)
select.col <- c("classA","classB","classC","classD","classE","classF","classG","classH","classI","classJ","classX","label")

obj.view <- subset(df_NA, select = select.col)
obj.view <- obj.view[complete.cases(obj.view),]

index <- sample(2, nrow(obj.view), replace = TRUE, prob = c(0.7, 0.3))
data.train <- obj.view
data.test <- obj.view

data.train<-df_NA[index,]
data.test<-df_NA[-index,]
#나이브 베이지안
nb <- naiveBayes(label ~ ., data = data.train)
pred <- predict(nb, data.test)
conf.mat <- table(pred, data.test$label)
conf.mat
(accuracy <- sum(diag(conf.mat))/sum(conf.mat) * 100)

#의사 결정 트리
tree <- ctree(label~., data = data.train)
pred <- predict(tree, data.test)
plot(tree)
conf.mat <- table(pred, data.test$label)
conf.mat
(accuracy <- sum(diag(conf.mat))/sum(conf.mat) * 100)


#뉴럴 네트워크
nn <- nnet(label~ ., data = data.train, size = 43,decay=5e-04, maxit=100000)  #size =2 히든 레이어,decay=5e-04가 가장 정확, maxit 반복 횟수  
pred <- predict(nn, newdata = data.test, type = "class")
conf.mat <- table(pred, data.test$label)
conf.mat
(accuracy <- sum(diag(conf.mat))/sum(conf.mat) * 100) 

###################################################################

