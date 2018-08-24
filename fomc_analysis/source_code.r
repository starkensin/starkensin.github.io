files.list <- list.files()

texts.all<-vector()
texts.spring<-vector()
texts.summer<-vector()
texts.fall<-vector()
texts.winter<-vector()
texts.pre<-vector()
texts.post<-vector()

texts.all.list<-grep("^...03|^...04|^...06|^...07|^...09|^...11|^...12|^...01",files.list,value=T)
texts.spring.list<-grep("^...03|^...04",files.list,value=T)
texts.summer.list<-grep("^...06|^...07",files.list,value=T)
texts.fall.list<-grep("^...09|^...11",files.list,value=T)
texts.winter.list<-grep("^...12|^...01",files.list,value=T)
texts.pre.list<-grep("^11...|^12...|^13...",files.list,value=T)
texts.post.list<-grep("^14...|^15...|^16...",files.list,value=T)

for(i in texts.all.list){
file <- scan(file=i,what="char",quote=NULL)
texts.all<-c(texts.all,file)
}
for(i in texts.spring.list){
file <- scan(file=i,what="char",quote=NULL)
texts.spring<-c(texts.spring,file)
}
for(i in texts.summer.list){
file <- scan(file=i,what="char",quote=NULL)
texts.summer<-c(texts.summer,file)
}
for(i in texts.fall.list){
file <- scan(file=i,what="char",quote=NULL)
texts.fall<-c(texts.fall,file)
}
for(i in texts.winter.list){
file <- scan(file=i,what="char",quote=NULL)
texts.winter<-c(texts.winter,file)
}
for(i in texts.pre.list){
file <- scan(file=i,what="char",quote=NULL)
texts.pre<-c(texts.pre,file)
}
for(i in texts.post.list){
file <- scan(file=i,what="char",quote=NULL)
texts.post<-c(texts.post,file)
}

cat(texts.all,file=file.choose(),sep="\n")
cat(texts.spring,file=file.choose(),sep="\n")
cat(texts.summer,file=file.choose(),sep="\n")
cat(texts.fall,file=file.choose(),sep="\n")
cat(texts.winter,file=file.choose(),sep="\n")
cat(texts.pre,file=file.choose(),sep="\n")
cat(texts.post,file=file.choose(),sep="\n")

texts.all<-tolower(texts.all)
texts.spring<-tolower(texts.spring)
texts.summer<-tolower(texts.summer)
texts.fall<-tolower(texts.fall)
texts.winter<-tolower(texts.winter)
texts.pre<-tolower(texts.pre)
texts.post<-tolower(texts.post)

texts.all<-gsub("^[[:punct:]]|[[:punct:]]$","",texts.all)
texts.spring<-gsub("^[[:punct:]]|[[:punct:]]$","",texts.spring)
texts.summer<-gsub("^[[:punct:]]|[[:punct:]]$","",texts.summer)
texts.fall<-gsub("^[[:punct:]]|[[:punct:]]$","",texts.fall)
texts.winter<-gsub("^[[:punct:]]|[[:punct:]]$","",texts.winter)
texts.pre<-gsub("^[[:punct:]]|[[:punct:]]$","",texts.pre)
texts.post<-gsub("^[[:punct:]]|[[:punct:]]$","",texts.post)

all.freq<-data.frame(sort(table(texts.all),decreasing=T))
spring.freq<-data.frame(sort(table(texts.spring),decreasing=T))
summer.freq<-data.frame(sort(table(texts.summer),decreasing=T))
fall.freq<-data.frame(sort(table(texts.fall),decreasing=T))
winter.freq<-data.frame(sort(table(texts.winter),decreasing=T))
pre.freq<-data.frame(sort(table(texts.pre),decreasing=T))
post.freq<-data.frame(sort(table(texts.post),decreasing=T))

#texts.all 등 기본 텍스트는 남겨두고 ppc(pre-processing) 생성
texts.all.ppc<-texts.all[!grepl("^the$|^and$|^of$|^to$|^in$|^its$|^a$|^that$|^at$|^is$|^with$|^as$|^has$|^on$|^have$|^be$|^it$|^in$|^are$|^was$|^but$|^however$|^while$|^were$",texts.all)]
texts.spring.ppc<-texts.spring[!grepl("^the$|^and$|^of$|^to$|^in$|^its$|^a$|^that$|^at$|^is$|^with$|^as$|^has$|^on$|^have$|^be$|^it$|^in$|^are$|^was$|^but$|^however$|^while$|^were$",texts.spring)]
texts.summer.ppc<-texts.summer[!grepl("^the$|^and$|^of$|^to$|^in$|^its$|^a$|^that$|^at$|^is$|^with$|^as$|^has$|^on$|^have$|^be$|^it$|^in$|^are$|^was$|^but$|^however$|^while$|^were$",texts.summer)]
texts.fall.ppc<-texts.fall[!grepl("^the$|^and$|^of$|^to$|^in$|^its$|^a$|^that$|^at$|^is$|^with$|^as$|^has$|^on$|^have$|^be$|^it$|^in$|^are$|^was$|^but$|^however$|^while$|^were$",texts.fall)]
texts.winter.ppc<-texts.winter[!grepl("^the$|^and$|^of$|^to$|^in$|^its$|^a$|^that$|^at$|^is$|^with$|^as$|^has$|^on$|^have$|^be$|^it$|^in$|^are$|^was$|^but$|^however$|^while$|^were$",texts.winter)]
texts.pre.ppc<-texts.pre[!grepl("^the$|^and$|^of$|^to$|^in$|^its$|^a$|^that$|^at$|^is$|^with$|^as$|^has$|^on$|^have$|^be$|^it$|^in$|^are$|^was$|^but$|^however$|^while$|^were$",texts.pre)]
texts.post.ppc<-texts.post[!grepl("^the$|^and$|^of$|^to$|^in$|^its$|^a$|^that$|^at$|^is$|^with$|^as$|^has$|^on$|^have$|^be$|^it$|^in$|^are$|^was$|^but$|^however$|^while$|^were$",texts.post)]

all.ppc.freq<-data.frame(sort(table(texts.all.ppc),decreasing=T))
spring.ppc.freq<-data.frame(sort(table(texts.spring.ppc),decreasing=T))
summer.ppc.freq<-data.frame(sort(table(texts.summer.ppc),decreasing=T))
fall.ppc.freq<-data.frame(sort(table(texts.fall.ppc),decreasing=T))
winter.ppc.freq<-data.frame(sort(table(texts.winter.ppc),decreasing=T))
pre.ppc.freq<-data.frame(sort(table(texts.pre.ppc),decreasing=T))
post.ppc.freq<-data.frame(sort(table(texts.post.ppc),decreasing=T))

#빈도수 2이하 제거
all.ppc.freq2<-all.ppc.freq[all.ppc.freq$Freq>2,]
spring.ppc.freq2<-spring.ppc.freq[spring.ppc.freq$Freq>2,]
summer.ppc.freq2<-summer.ppc.freq[summer.ppc.freq$Freq>2,]
fall.ppc.freq2<-fall.ppc.freq[fall.ppc.freq$Freq>2,]
winter.ppc.freq2<-winter.ppc.freq[winter.ppc.freq$Freq>2,]
pre.ppc.freq2<-pre.ppc.freq[pre.ppc.freq$Freq>2,]
post.ppc.freq2<-post.ppc.freq[post.ppc.freq$Freq>2,]

#연어분석
Index <- which(texts.all.ppc == "inflation")
spanlength = 6
Conc <- vector()
for( i in Index)
{
span <- (i - spanlength):(i+spanlength)
span <- span[span>0&span<=length(texts.all.ppc)]

Conc <- c(texts.all.ppc[span],Conc)
}

Freq.span <- sort(table(Conc),decreasing=T)
Freq.co <- data.frame(W1 = vector(),W2 = vector(),W1W2 =vector(),N = vector())
n=1
for( i in (2:length(Freq.span))){
Freq.co[n,] <- c(length(Index),
length(texts.all.ppc[texts.all.ppc ==names(Freq.span)[i]]),
Freq.span[i],
length(texts.all.ppc))
n=n+1
}

rownames(Freq.co) <- names(Freq.span)[2:length(Freq.span)]

score.frame <- data.frame(Freq.co,
t.score=(Freq.co$W1W2 - ((Freq.co$W1*Freq.co$W2)/Freq.co$N))/sqrt(Freq.co$W1W2),
MI.score=log2((Freq.co$W1W2*Freq.co$N)/(Freq.co$W1*Freq.co$W2)))

t.score.sort <- score.frame[order(score.frame$t.score,decreasing=T),]

##N그렘 및 빈도표 생성
all.ppc.bigram<-paste(texts.all.ppc[1:(length(texts.all.ppc)-1)], texts.all.ppc[2:length(texts.all.ppc)], sep=" ")
pre.ppc.bigram<-paste(texts.pre.ppc[1:(length(texts.pre.ppc)-1)], texts.pre.ppc[2:length(texts.pre.ppc)], sep=" ")
post.ppc.bigram<-paste(texts.post.ppc[1:(length(texts.post.ppc)-1)], texts.post.ppc[2:length(texts.post.ppc)], sep=" ")

all.ppc.trigram<-paste(texts.all.ppc[1:(length(texts.all.ppc)-2)], texts.all.ppc[2:(length(texts.all.ppc)-1)],texts.all.ppc[3:length(texts.all.ppc)], sep=" ")
all.ppc.trigram.freq <- data.frame(sort(table(all.ppc.trigram),decreasing = TRUE))

all.ppc.bigram.freq <- data.frame(sort(table(all.ppc.bigram),decreasing = TRUE))
pre.ppc.bigram.freq <- data.frame(sort(table(pre.ppc.bigram),decreasing = TRUE))
post.ppc.bigram.freq <- data.frame(sort(table(post.ppc.bigram),decreasing = TRUE))

## 비교 클라우드
colnames(pre.ppc.bigram.freq)[1]<-"word"
colnames(post.ppc.bigram.freq)[1]<-"word"
Union <- merge(pre.ppc.bigram.freq, post.ppc.bigram.freq, by="word", all=T) 
colnames(Union)[2:3] <- c("PRE", "POST") 
Union[is.na(Union)] <- 0 
Union.Freq <- data.frame(row.names=Union$word, Union[,2:3])

comparison.cloud(Union.Freq, scale=c(3, 0.9), max.words=200, random.order=F, 
                 rot.per=0.4, title.size=2, colors = c("darkgreen", "darkorange"))
