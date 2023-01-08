dataHeart<-read.csv("heart.csv",header=T,sep=",")
attach(dataHeart)



str(dataHeart)
head(dataHeart)
summary(dataHeart)



dataHeart$caa <- as.factor(dataHeart$caa)
dataHeart$cp <- as.factor(dataHeart$cp)
dataHeart$restecg <- as.factor(dataHeart$restecg)
dataHeart$slp  <- as.factor(dataHeart$slp)


dataHeart$exng <- as.logical(dataHeart$exng)
dataHeart$fbs <- as.logical(dataHeart$fbs)
dataHeart$sex <- as.logical(dataHeart$sex)
dataHeart$output <- as.logical(dataHeart$output)
dataHeart$thall  <- as.logical(dataHeart$thall)


table(dataHeart$exng)
table(dataHeart$fbs)
table(dataHeart$sex)
table(dataHeart$output)
table(dataHeart$thall)


table(dataHeart$caa)
table(dataHeart$cp)
table(dataHeart$restecg)
table(dataHeart$slp)


if(!require(ggplot2)){
  install.packages('ggplot2', repos='http://cran.us.r-project.org')
  library(ggplot2)
}
if(!require(ggpubr)){
  install.packages('ggpubr', repos='http://cran.us.r-project.org')
  library(ggpubr)
}
if(!require(grid)){
  install.packages('grid', repos='http://cran.us.r-project.org')
  library(grid)
}
if(!require(gridExtra)){
  install.packages('gridExtra', repos='http://cran.us.r-project.org')
  library(gridExtra)
}
if(!require(C50)){
  install.packages('C50', repos='http://cran.us.r-project.org')
  library(C50)
}
if (!require('ggplot2')) install.packages('ggplot2'); library('ggplot2')
if(!require('Rmisc')) install.packages('Rmisc'); library('Rmisc')
if(!require('dplyr')) install.packages('dplyr'); library('dplyr')
if(!require('xfun')) install.packages('xfun'); library('xfun')
if(!require("corrplot")) install.packages("corrplot"); library("corrplot")

plotbyexng <- barplot(table(dataHeart$exng), main = "Bar Plot exng")
plotbyfbs <- barplot(table(dataHeart$fbs), main = "Bar Plot fbs")
plotbysex <- barplot(table(dataHeart$sex), main = "Bar Plot sex")
plotbyoutput <- barplot(table(dataHeart$output), main = "Bar Plot output")
plotbythall <- barplot(table(dataHeart$thall), main = "Bar Plot thall")
plotbycaa <- barplot(table(dataHeart$caa), main = "Bar Plot caa")
plotbycp <- barplot(table(dataHeart$cp), main = "Bar Plot cp")
plotbyrestecg <- barplot(table(dataHeart$restecg), main = "Bar Plot restecg")
plotbyslp <- barplot(table(dataHeart$slp), main = "Bar Plot slp")

grid.newpage()

df <- data.frame(
  "names" = levels(dataHeart$cp),
  "freqs" = unname(table(dataHeart$cp))
)
ggp1 <- ggplot(df,aes(x=names,y=freqs.Freq)) + 
  geom_bar(stat="identity",fill="lightblue") +
  coord_flip() +
  ylab("count") + xlab("cp")+
  ggtitle("Count of cp")

df <- data.frame(
  "names" = levels(dataHeart$caa),
  "freqs" = unname(table(dataHeart$caa))
)
ggp2 <- ggplot(df,aes(x=names,y=freqs.Freq)) + 
  geom_bar(stat="identity",fill="lightblue") +
  coord_flip() +
  ylab("count") + xlab("caa")+
  ggtitle("Count of caa") 

df <- data.frame(
  "names" = levels(dataHeart$restecg),
  "freqs" = unname(table(dataHeart$restecg))
)
ggp3 <- ggplot(df,aes(x=names,y=freqs.Freq)) + 
  geom_bar(stat="identity",fill="lightblue") +
  coord_flip() +
  ylab("count") + xlab("restecg")+
  ggtitle("Count of restecg") 

df <- data.frame(
  "names" = levels(dataHeart$slp),
  "freqs" = unname(table(dataHeart$slp))
)
ggp4 <- ggplot(df,aes(x=names,y=freqs.Freq)) + 
  geom_bar(stat="identity",fill="lightblue") +
  coord_flip() +
  ylab("count") + xlab("slp")+
  ggtitle("Count of slp") 

grid.arrange(ggp1,ggp2,ggp3,ggp4,ncol=2)

pie( table(dataHeart$output), labels = c("FALSE", "TRUE"), main="Pie Chart of Total Heart Disease Attacks", col=c('red','green'))



n = c("age","trtbps","chol","thalachh","oldpeak")
factores= dataHeart %>% select(all_of(n))
res<-cor(factores)
title <- "DataHeart correlation"
corrplot(res,method="color",tl.col="black", tl.srt=30, order = "AOE",title = title,
         number.cex=0.75,sig.level = 0.01, addCoef.col = "black",mar=c(0,0,1,0))

boxplot(x = dataHeart$age,main='Variable age')
boxplot(x = dataHeart$trtbps,main='Variable trtbps')
boxplot(x = dataHeart$thalachh,main='Variable thalachh')
boxplot(x = dataHeart$oldpeak,main='Variable oldpeak')


boxplot(x = dataHeart$chol,main='Variable chol')
out_hsize <- boxplot.stats(dataHeart$chol)$out
out_ind_hsize <- which(dataHeart$chol %in% c(out_hsize))

head(out_hsize)



x<-dataHeart 
x<- x[-which(x$chol %in% out_hsize),]
boxplot(x = x$chol,main='Variable chol')
summary(x$chol)
dataHeart <- x


hist(dataHeart$chol[dataHeart$output==TRUE], col=rgb(1,0,0,0.2),main="Cholesterol vs Heart Attack")
hist(dataHeart$chol[dataHeart$output==FALSE], col=rgb(0,0,1,0.2), add=TRUE)
legend('topright', c('Output TRUE', 'Output FALSE'),
       fill=c(rgb(1,0,0,0.2),rgb(0,0,1,0.2)), xpd=TRUE, cex=0.7,)



test_grafica<-dataHeart %>%
  group_by(exng) %>%
  count(sex)%>%
  mutate(porcentaje=scales::percent(n/sum(n)))

ggplot(test_grafica,aes(x=exng , y=n, fill=sex))+
  geom_bar(stat="identity", position="dodge")+
  geom_text(aes(label=porcentaje),color="black", vjust=1.5, position = position_dodge(0.9))


test_grafica<-dataHeart %>%
  group_by(fbs) %>%
  count(sex)%>%
  mutate(porcentaje=scales::percent(n/sum(n)))

ggplot(test_grafica,aes(x=fbs , y=n, fill=sex))+
  geom_bar(stat="identity", position="dodge")+
  geom_text(aes(label=porcentaje),color="black", vjust=1.5, position = position_dodge(0.9))



hist(dataHeart$thalach [dataHeart$sex==TRUE], col=rgb(1,0,0,0.2))
hist(dataHeart$thalach [dataHeart$sex==FALSE], col=rgb(0,0,1,0.2), add=TRUE)
legend('topright', c('Sex TRUE', 'Sex FALSE'),
       fill=c(rgb(1,0,0,0.2),rgb(0,0,1,0.2)), xpd=TRUE, cex=0.7,)




library(nortest)
alpha = 0.05
col.names = colnames(dataHeart)
for (i in 1:ncol(dataHeart)) {
  if (i == 1) cat("Variables que no siguen una distribución normal:\n")
  if (is.integer(dataHeart[,i]) | is.numeric(dataHeart[,i])) {
    p_val = ad.test(dataHeart[,i])$p.value
    if (p_val < alpha) {
      cat(col.names[i])
      # Format output
      if (i < ncol(dataHeart) - 1) cat(", ")
      if (i %% 3 == 0) cat("\n")
    }
  }
}

##Correlaciones 
if(!require("corrplot")) install.packages("corrplot"); library("corrplot")
n = c("age","sex","cp","trtbps","chol","fbs","restecg","thalachh","exng","oldpeak","slp","caa","thall","output")
factores= dataHeart %>% select(all_of(n))
res<-cor(factores)
corrplot(res,method="color",tl.col="black", tl.srt=30, order = "AOE",
         number.cex=0.75,sig.level = 0.01, addCoef.col = "black")




library(car)
ModelF <- lm(output ~ sex+cp+fbs+restecg+exng+slp+caa+thall, data = dataHeart)
car::vif(ModelF)



ModlgF <- glm(output ~ age+sex+cp+trtbps+chol+fbs+restecg+thalachh+exng+oldpeak+slp+caa+thall+output
              , data = dataHeart, family = "binomial")
summary(ModlgF)

vif(ModlgF)

