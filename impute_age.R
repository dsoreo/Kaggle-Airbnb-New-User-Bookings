library(sqldf)
library(xgboost)

cat("impute_age.R - Reading data\n")
ts <- read.csv('test_users.csv',colClasses = c(NULL,NULL,'character'),stringsAsFactors = FALSE)
tr <- read.csv('train_users_2.csv',colClasses = c(NULL,NULL,'character'),stringsAsFactors = FALSE)

tr$age <- as.numeric(tr$age)
ts$age <- as.numeric(ts$age)
cat("Create missing age variable and impute age\n")
tr$age_flag <- ifelse(is.na(tr$age),0,1)
ts$age_flag <- ifelse(is.na(ts$age),0,1)

train_cd <- tr$country_destination
tr <- tr[,-match(c("country_destination"),colnames(tr)),]
tr$amid <- seq(from = 1, to = nrow(tr),by=1)
ts$amid <- seq(from=(nrow(tr)+1), to = (nrow(tr)+nrow(ts)))
c_d <- rbind(tr,ts)

cat("impute_age.R - Fixing values before one hot encoding\n")
tr$signup_flow <- ifelse((tr$signup_flow=="0"|
                                    tr$signup_flow=='12'|
                                    tr$signup_flow=='23'|
                                    tr$signup_flow=='25'), ts$signup_flow,"other")
ts$signup_flow <- ifelse((ts$signup_flow=="0"|
                                   ts$signup_flow=='12'|
                                   ts$signup_flow=='23'|
                                   ts$signup_flow=='25'), ts$signup_flow,"other")

cat("impute_age.R - One hot encoding for imputing age\n")
factor_variables <- c("gender","signup_method","language","affiliate_channel","affiliate_provider","first_affiliate_tracked","signup_app","first_device_type","first_browser","signup_flow")

for(i in 1:length(factor_variables)){
  v <- as.data.frame(c_d[,match(factor_variables[i],colnames(c_d))])
  names(v) <- c(paste(factor_variables[i],"_",sep=""))
  df <- data.frame(model.matrix(~.-1,v))
  c_d <- cbind(c_d,df)
}
c_d <- c_d[,-match(factor_variables,colnames(c_d))]

rm(df,v)

cat("impute_age.R - Fix age\n")
c_d$age <- ifelse(c_d$age==2014,NA,c_d$age)
c_d$age <- ifelse(c_d$age==2013,NA,c_d$age)
c_d$age <- ifelse(c_d$age>1000,2014-c_d$age,c_d$age)
c_d$age <- ifelse(c_d$age <=5, NA, c_d$age)
c_d$age <- ifelse(c_d$age >100, NA, c_d$age)

c_d <- c_d[,-match(c("id","date_account_created","timestamp_first_active","date_first_booking"),colnames(c_d))]
atrain <- c_d[!is.na(c_d$age),]
atest <- c_d[is.na(c_d$age),]


age_pred_1 <- NA
age_pred_2 <- NA
age_pred_3 <- NA
age_pred_4 <- NA
age_pred_5 <- NA

for(i in 1:5) {
  cat("impute_age.R - Running xgb model ",i," of 5 to impute age\n")
  #5CV seeds - 1,2,3,4,5
  # 0.85 in part 1
  set.seed(i)
  s <- sample(0.85*nrow(atrain))
  atrain_t <- atrain[s,]
  atrain_v <- atrain[-s,]
  rm(s)
  
  dtrain<-xgb.DMatrix(data=data.matrix(atrain_t[,-match(c("age","amid"),colnames(atrain_t))]),label=atrain_t$age)
  dval<-xgb.DMatrix(data=data.matrix(atrain_v[,-match(c("amid","age"),colnames(atrain_v))]),label=atrain_v$age)
  watchlist<-list(val=dval,train=dtrain)
  param <- list(  objective           = "reg:linear", 
                  booster             = "gbtree",
                  eta                 = 0.01, 
                  max_depth           = 8, 
                  subsample           = 0.9, 
                  colsample_bytree    = 0.7,
                  silent              = 1
  )
  
  set.seed(123)
  xgb_model <- xgb.train(params=param,
                         data=dtrain,
                         nrounds=2000,
                         watchlist=watchlist,
                         early.stop.round=200,
                         maximize=FALSE,
                         verbose = 0)
  
  if (i==1) {age_pred_1 <- predict(xgb_model,as.matrix(atest[,-match(c("age"),colnames(atest))]))}
  if (i==2) {age_pred_2 <- predict(xgb_model,as.matrix(atest[,-match(c("age"),colnames(atest))]))}
  if (i==3) {age_pred_3 <- predict(xgb_model,as.matrix(atest[,-match(c("age"),colnames(atest))]))}
  if (i==4) {age_pred_4 <- predict(xgb_model,as.matrix(atest[,-match(c("age"),colnames(atest))]))}
  if (i==5) {age_pred_5 <- predict(xgb_model,as.matrix(atest[,-match(c("age"),colnames(atest))]))}
  
  gc(reset=TRUE)
}

cat("impute_age.R - predict age\n")
age_pred <- (age_pred_1+age_pred_2+age_pred_3+age_pred_4+age_pred_5)/5
rm(age_pred_1,age_pred_2,age_pred_3,age_pred_4,age_pred_5)

a1 <- data.frame(mid=atest$amid,nwage=age_pred)
a2 <- data.frame(mid=atrain$amid,nwage=atrain$age)
a <- rbind(a1,a2)
tr <- merge(tr,a,by.x="amid",by.y="mid",all.x=TRUE,all.y = FALSE)
ts <- merge(ts,a,by.x="amid",by.y="mid",all.x=TRUE,all.y = FALSE)

rm(c_d,atrain,atest,a,a1,a2,atrain_t,atrain_v,age_pred,dtrain,dval,train_cd,watchlist,xgb_model,param,factor_variables,i)

cat("impute_age.R - Impute age in train and test data\n")
train_data$age <- tr$nwage
test_data$age <- ts$nwage
rm(tr,ts)
gc(reset=TRUE)
cat("impute_age.R - Imputation complete\n")