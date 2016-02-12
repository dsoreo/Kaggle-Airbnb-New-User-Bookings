library(readr)
library(sqldf)

cat("Ensemble.R - Read output from model m1\n")
m11 <- read_csv('m1_s_f_123.csv')
m12 <- read_csv('m1_s_f_1.csv')
m13 <- read_csv('m1_s_f_2.csv')
m14 <- read_csv('m1_s_f_3.csv')
m15 <- read_csv('m1_s_f_4.csv')

m1 <- as.matrix((m11+m12+m13+m14+m15)/5)

cat("Ensemble.R - Read output from model m1\n")
m21 <- read_csv('m2_s_f_123.csv')
m22 <- read_csv('m2_s_f_1.csv')
m23 <- read_csv('m2_s_f_2.csv')
m24 <- read_csv('m2_s_f_3.csv')
m25 <- read_csv('m2_s_f_4.csv')

m2 <- as.matrix((m21+m22+m23+m24+m25)/5)

cat("Ensemble.R - Read output from model m1\n")
m31 <- read_csv('m3_s_f_123.csv')
m32 <- read_csv('m3_s_f_1.csv')
m33 <- read_csv('m3_s_f_2.csv')
m34 <- read_csv('m3_s_f_3.csv')
m35 <- read_csv('m3_s_f_4.csv')

m3 <- as.matrix((m31+m32+m33+m34+m35)/5)

cat("Ensemble.R - Read output from model m1\n")
m41 <- read_csv('m4_s_f_123.csv')
m42 <- read_csv('m4_s_f_1.csv')
m43 <- read_csv('m4_s_f_2.csv')
m44 <- read_csv('m4_s_f_3.csv')
m45 <- read_csv('m4_s_f_4.csv')

m4 <- as.matrix((m41+m42+m43+m44+m45)/5)

cat("Ensemble.R - Read output from model m1\n")
m51 <- read_csv('m5_s_f_123.csv')
m52 <- read_csv('m5_s_f_1.csv')
m53 <- read_csv('m5_s_f_2.csv')
m54 <- read_csv('m5_s_f_3.csv')
m55 <- read_csv('m5_s_f_4.csv')

m5 <- as.matrix((m51+m52+m53+m54+m55)/5)

cat("Ensemble.R - Read output from model m1\n")
m61 <- read_csv('m6_s_f_123.csv')
m62 <- read_csv('m6_s_f_1.csv')
m63 <- read_csv('m6_s_f_2.csv')
m64 <- read_csv('m6_s_f_3.csv')
m65 <- read_csv('m6_s_f_4.csv')

m6 <- as.matrix((m61+m62+m63+m64+m65)/5)


en1 <- m2*.8+m3*.05+m4*.05+m5*.05+m6*.05
en2 <- m1*.1+m2*.7+m3*.04+m4*.04+m5*.06+m6*.06

combi_sub <- en2

test_data <- read_csv('test_formatted_v3.csv')
train_data <- read_csv('train_formatted_v3.csv')
cat("Create factor variables for country\n")
mapping <- data.frame(country=unique(train_data$country_destination),
                      map = as.integer(factor(unique(train_data$country_destination))))
mapping$map <- mapping$map-1
tmp <- sqldf('select * from train_data left join mapping ON mapping.country=train_data.country_destination')
train_data$book <- tmp$map
rm(tmp)
mapping$country <- as.character(mapping$country)
test_id <- test_data$id

cat("Ensemble.R - Create submission\n")
dest_sub <- data.frame(id=test_id,p1=NA,p2=NA,p3=NA,p4=NA,p5=NA)
for(i in 1:nrow(dest_sub)){
  dest_sub$p1[i] <- mapping$country[which(combi_sub[i,]==max(combi_sub[i,]))[1]-1 == mapping$map]
  combi_sub[i,which(combi_sub[i,]==max(combi_sub[i,]))[1]] <- -1
  dest_sub$p2[i] <- mapping$country[which(combi_sub[i,]==max(combi_sub[i,]))[1]-1 == mapping$map]
  combi_sub[i,which(combi_sub[i,]==max(combi_sub[i,]))[1]] <- -1
  dest_sub$p3[i] <- mapping$country[which(combi_sub[i,]==max(combi_sub[i,]))[1]-1 == mapping$map]
  combi_sub[i,which(combi_sub[i,]==max(combi_sub[i,]))[1]] <- -1
  dest_sub$p4[i] <- mapping$country[which(combi_sub[i,]==max(combi_sub[i,]))[1]-1 == mapping$map]
  combi_sub[i,which(combi_sub[i,]==max(combi_sub[i,]))[1]] <- -1
  dest_sub$p5[i] <- mapping$country[which(combi_sub[i,]==max(combi_sub[i,]))[1]-1 == mapping$map]
  combi_sub[i,which(combi_sub[i,]==max(combi_sub[i,]))[1]] <- -1
  
  if(i %% 1000 == 0) {print(paste("Done",i,"of",nrow(dest_sub)))}
}

submission <- data.frame(id=rep(dest_sub$id,5))
submission$country <- c(dest_sub$p1,dest_sub$p2,dest_sub$p3,dest_sub$p4,dest_sub$p5)
write.csv(submission,"~/Documents/Kaggle/Kaggle_Airbnb/submission_file.csv",row.names=FALSE)