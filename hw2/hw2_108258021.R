# trailingOnly 如果是TRUE的話，會只編輯command-line出現args的值args <- 
args <- commandArgs(trailingOnly=TRUE)
if (length(args)==0) {
  stop("USAGE: Rscript hw2_yourID.R --target male|female --input file1 file2 ... filen --output out.csv", call.=FALSE)
}
# parse parameters
#multiple files
i<-1 
while(i < length(args))
{
  if(args[i] == "--target"){
    query_m<-args[i+1]
    i<-i+1
  }else if(args[i] == "--input"){
    j<-grep("-", c(args[(i+1):length(args)], "-"))[1]
    files<-args[(i+1):(i+j-1)]
    i<-i+j-1
  }else if(args[i] == "--output"){
    out_f<-args[i+1]
    i<-i+1
  }else if(args[i] == "--badthre"){
    badthre<-args[i+1]
    i<-i+1
  }else{
    stop(paste("Unknown flag", args[i]), call.=FALSE)
  }
  i<-i+1
}

print("PROCESS")
print(paste("query mode :", query_m))
print(paste("output file:", out_f))
print(paste("files      :", files))

#getwd()#相對路徑
#data path
a <- file.path(getwd(),"data")

#讀取檔案
h <- list()
data <- list()
for (i in files) {
  h[[i]]<-gsub(".csv", "", basename(i)) #for index name
  data[[i]] <- read.csv(paste0(a,"/",i))
  }

#pred.score轉換成0,1
#data[[1]]$pred.score
data_new <- data
for (i in files) {
  data_new[[i]]$pred.score <- ifelse(data[[i]]$pred.score >= badthre,1,0)
}

#confusion matrix
cf <- list()
for (i in files) {
  cf[[i]] <- table(data_new[[i]][,c('pred.score','reference')])
}
#cm <- table(data[[1]][,c('pred.score','reference')])
tp <- c()
tn <- c()
fp <- c()
fn <- c()
for (i in 1:length(data_new)) {
  tp[[i]] <- cf[[i]][2,1]
  tn[[i]] <- cf[[i]][1,2]
  fp[[i]] <- cf[[i]][1,1]
  fn[[i]] <- cf[[i]][2,2]
}
#accuracy,sensitivity(recall),specificity,precision
accuracy <- c()
for (i in 1:length(data_new)) {
  accuracy[[i]] <- (tp[[i]] + tn[[i]])/(tp[[i]]+tn[[i]]+fp[[i]]+fn[[i]])
}
sensitivity <- c()
for (i in 1:length(data_new)) {
  sensitivity[[i]] <- tp[[i]]/(tp[[i]] + fn[[i]])
}
specificity <- c()
for (i in 1:length(data_new)) {
  specificity[[i]] <- tn[[i]]/(tn[[i]] + fp[[i]])
}
precision <- c()
for (i in 1:length(data_new)) {
  precision[[i]] <- tp[[i]]/(tp[[i]] + fp[[i]])
}
#F1-score
f1_score <- c()
for (i in 1:length(data_new)) {
  f1_score[[i]] <- 2 * precision[[i]] * sensitivity[[i]] / (precision[[i]] + sensitivity[[i]])
}

#log_likelihood, null model's log_likelihood
#pseudo_R2
log_likelihood <- c()
pnull <- c()
log_likelihood_null <- c()
deviance <- c()
deviance_null <- c() 
pseudo_r2 <- c()
for (i in 1:length(data)) {
  log_likelihood[[i]] <- sum(ifelse(data_new[[i]]$pred.score == 1,
                               log(data[[i]]$pred.score),
                               log(1-data[[i]]$pred.score)))
  pnull[[i]] <- sum(ifelse(data_new[[i]]$pred.score == 1,1,0))/dim(data[[i]])[[1]]
  deviance[[i]] <- -2*(log_likelihood[[i]])
  log_likelihood_null[[i]] <- sum(ifelse(data_new[[i]] == 1,1,0))*log(pnull[[i]])
                              +sum(ifelse(data_new[[i]] == 1,0,1))*log(1-pnull[[i]])
  deviance_null[[i]] <- -2*(log_likelihood_null[[i]])
  pseudo_r2[[i]] <- 1- deviance[[i]]/deviance_null[[i]]
}

#data summary
data_final <- list()
df <- data.frame()
for (i in 1:length(files)) {
  data_final[[i]] <- data.frame(h[[i]],round(sensitivity[[i]],2),round(specificity[[i]],2),round(f1_score[[i]],2),round(log_likelihood[[i]],2),round(pseudo_r2[[i]],2))
  #rename
  names(data_final[[i]]) <- c('method','sensitivity','specificity','F1','Loglikelihood','pseudoR2')
  df <- rbind(df,data_final[[i]])
}

#compare each column which is better method
max <- list()
for (i in 2:length(df)) {
  max[[i]] <- df[which.max(df[[i]]),][1]
}
k <- data.frame(c("max",max[[2]],max[[3]],max[[4]],max[[5]],max[[6]]))
names(k) <- c("method","sensitivity","specificity","F1","Loglikelihood","pseudoR2")
df <- rbind(df,k)

#output path
final_files <- file.path(getwd(),"eval","/")
#write output file
write.csv(df,file=paste(final_files,out_f,sep=""),row.names=FALSE)
#show result
read.csv(paste(final_files,out_f, sep=""), header=T)





