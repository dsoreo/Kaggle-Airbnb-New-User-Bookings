#Principle  component analysis code adapted from http://www.sthda.com/english/wiki/principal-component-analysis-in-r-prcomp-vs-princomp-r-software-and-data-mining#use-the-r-function-prcomp-for-pca


start_time <- Sys.time()

cat("multi_prob.R - Reading data\n")
train_data <- read_csv('train_formatted.csv')
test_data <- read_csv('test_formatted.csv')

uid <- read_csv("sessions_id.csv")
train_data$session_available <- train_data$id %in% uid$x
test_data$session_available <- test_data$id %in% uid$x
rm(uid)

if(NEW_IMPUTE_AGE == TRUE){
  cat("multi_prob.R - Impute age with xgb model\n")
  source("impute_age.R")
}

if(DT_2014 == TRUE){
  cat("multi_prob.R - subset 2014 data for training\n")
  train_data <- train_data[train_data$session_available==1,]
}

if(DT_3M == TRUE) {
  cat("multi_prob.R - subset last three months data for training\n")
  tr <- read.csv('train_users_2.csv',colClasses = c(NULL,NULL,'character'),stringsAsFactors = FALSE)
  tr$tsfa_t <- as.Date(as.POSIXct(strtrim(tr$timestamp_first_active,8),format="%Y%m%d"),format="%Y%m%d")
  train_data <- train_data[tr$tsfa_t >= "2014-04-01",]
}

cat("multi_prob.R - Create factor variables for country\n")
mapping <- data.frame(country=unique(train_data$country_destination),
                      map = as.integer(factor(unique(train_data$country_destination))))
mapping$map <- mapping$map-1
tmp <- sqldf('select * from train_data left join mapping ON mapping.country=train_data.country_destination')
train_data$book <- tmp$map
rm(tmp)
mapping$country <- as.character(mapping$country)

cat("multi_prob.R - Get zero variance variables\n")
zero.var <- nearZeroVar(train_data[,-match(c("book","country_destination","id"),colnames(train_data))],saveMetrics = TRUE)
zero.var.variables <- rownames(zero.var[zero.var$zeroVar==TRUE,])

cat("multi_prob.R - Do principle component analysis to remove low variance variables\n")
temp_train <- train_data[,-match(zero.var.variables,colnames(train_data))]
temp_train <- temp_train[,-match(c("id","country_destination","book"),colnames(temp_train))]
res.pca <- prcomp(temp_train,scale=TRUE)
eig <- (res.pca$sdev)^2
variance <- eig*100/sum(eig)
cumvar <- cumsum(variance)
eig.decathlon2.active <- data.frame(eig = eig, variance = variance,
                                    cumvariance = cumvar)
eig.decathlon2.active$name <- rownames(res.pca$rotation)
useless_var <- eig.decathlon2.active$name[PCA_INDEX_1:PCA_INDEX_2]
rm(temp_train)

cat("multi_prob.R - Sample Data\n")
set.seed(USE_SEED)
s <- sample(SAMPLE_SIZE*nrow(train_data))
train_train <- train_data[s,]
train_validation <- train_data[-s,]
rm(s)

tt_country <- train_train$country_destination
train_train <- train_train[,-match(c("id","country_destination"),colnames(train_train))]
tv_id <- train_validation$id
tv_dest <- train_validation$country_destination
tv_book <- train_validation$book
tv_country <- train_validation$country_destination
train_validation<- train_validation[,-match(c("id","country_destination","book"),
                                            colnames(train_validation))]

train_train <- train_train[,-match(zero.var.variables,colnames(train_train))]
train_validation<- train_validation[,-match(zero.var.variables,colnames(train_validation))]
train_train <- train_train[-match(useless_var,colnames(train_train))]
train_validation <- train_validation[-match(useless_var,colnames(train_validation))]

cat("multi_prob.R - Create xgboost model\n")
dtrain<-xgb.DMatrix(data=data.matrix(train_train[,-match(c("book"),colnames(train_train))]),label=train_train$book)
dval<-xgb.DMatrix(data=data.matrix(train_validation),label=tv_book)
watchlist<-list(val=dval,train=dtrain)
param <- list(  objective           = "multi:softprob", 
                booster             = "gbtree",
                eta                 = 0.01,
                max_depth           = XGB_DEPTH, 
                subsample           = 0.9, # 0.7
                num_class           = 12,
                colsample_bytree    = 0.7
)

set.seed(123)
xgb_model <- xgb.train(params=param,
                       data=dtrain,
                       nrounds=2000,
                       watchlist=watchlist,
                       early.stop.round=200,
                       maximize=FALSE)

xgb_predictions <- predict(xgb_model,dval)
xgb_predictions <- matrix(xgb_predictions,nrow=nrow(train_validation),ncol=12,byrow = TRUE)

current_pred <- xgb_predictions
cat("multi_prob.R - Create validation predictions\n")
dest_pred <- data.frame(id=tv_id,p1=NA,p2=NA,p3=NA,p4=NA,p5=NA)
for(i in 1:nrow(dest_pred)){
  dest_pred$p1[i] <- mapping$country[which(current_pred[i,]==max(current_pred[i,]))[1]-1 == 
                                       mapping$map]
  current_pred[i,which(current_pred[i,]==max(current_pred[i,]))[1]] <- -1
  dest_pred$p2[i] <- mapping$country[which(current_pred[i,]==max(current_pred[i,]))[1]-1 == 
                                       mapping$map]
  current_pred[i,which(current_pred[i,]==max(current_pred[i,]))[1]] <- -1
  dest_pred$p3[i] <- mapping$country[which(current_pred[i,]==max(current_pred[i,]))[1]-1 == 
                                       mapping$map]
  current_pred[i,which(current_pred[i,]==max(current_pred[i,]))[1]] <- -1
  dest_pred$p4[i] <- mapping$country[which(current_pred[i,]==max(current_pred[i,]))[1]-1 == 
                                       mapping$map]
  current_pred[i,which(current_pred[i,]==max(current_pred[i,]))[1]] <- -1
  dest_pred$p5[i] <- mapping$country[which(current_pred[i,]==max(current_pred[i,]))[1]-1 == 
                                       mapping$map]
  current_pred[i,which(current_pred[i,]==max(current_pred[i,]))[1]] <- -1
  
  if(i %% 1000 == 0) {print(paste("Done",i,"of",nrow(dest_pred)))}
}

cat("multi_prob.R - Validation score\n")
print(mean(score_predictions(dest_pred[,-c(1)],tv_dest)))


cat("multi_prob.R - Create submission probability matrix\n")
test_id <- test_data$id
test_data<- test_data[,-match(c("id"),colnames(test_data))]

test_data <- test_data[,-match(zero.var.variables,colnames(test_data))]
test_data <- test_data[-match(useless_var,colnames(test_data))]

xgb_sub <- predict(xgb_model,data.matrix(test_data))
xgb_sub <- matrix(xgb_sub,nrow=nrow(test_data),ncol=12,byrow = TRUE)

filename <- paste(USE_MODEL,"_s_f_",USE_SEED,".csv",sep="")
write.csv(xgb_sub,filename,row.names=FALSE)

gc(reset=TRUE)
print(Sys.time()-start_time)