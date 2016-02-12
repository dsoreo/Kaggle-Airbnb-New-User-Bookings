cat("data_structure.R - Read data\n")
test_data <- read.csv('test_users.csv',colClasses = c(NULL,NULL,'character'),stringsAsFactors = FALSE)
train_data <- read.csv('train_users_2.csv',colClasses = c(NULL,NULL,'character'),stringsAsFactors = FALSE)

train_data$age <- as.numeric(train_data$age)
test_data$age <- as.numeric(test_data$age)
cat("data_structure.R - Create variable flag for missing age\n")
train_data$age_flag <- ifelse(is.na(train_data$age),0,1)
test_data$age_flag <- ifelse(is.na(test_data$age),0,1)

train_cd <- train_data$country_destination
train_data <- train_data[,-match(c("country_destination"),colnames(train_data)),]
c_d <- rbind(train_data,test_data)

cat("data_structure.R - Fix age variables entered as years\n")
c_d$age <- ifelse(c_d$age==2014,NA,c_d$age)
c_d$age <- ifelse(c_d$age==2013,NA,c_d$age)
c_d$age <- ifelse(c_d$age>1000,2014-c_d$age,c_d$age)
c_d$age <- ifelse(c_d$age <=5, NA, c_d$age)
c_d$age <- ifelse(c_d$age >100, NA, c_d$age)

cat("data_structure.R - Impute missing ages\n")
median_ages <- sqldf('select median(age) as m_age, gender, signup_method, signup_flow, language, affiliate_channel, affiliate_provider, first_affiliate_tracked, signup_app, first_device_type, first_browser from c_d group by gender, signup_method, signup_flow, language, affiliate_channel, affiliate_provider, first_affiliate_tracked, signup_app, first_device_type, first_browser')
median_ages$m_age <- as.numeric(median_ages$m_age)

fake_age <- sqldf("select * from c_d left join median_ages on median_ages.gender == c_d.gender AND
                    median_ages.signup_method == c_d.signup_method AND
                  median_ages.signup_flow == c_d.signup_flow AND
                  median_ages.language == c_d.language AND
                  median_ages.affiliate_channel == c_d.affiliate_channel AND
                  median_ages.affiliate_provider == c_d.affiliate_provider AND
                  median_ages.first_affiliate_tracked == c_d.first_affiliate_tracked AND
                  median_ages.signup_app == c_d.signup_app AND
                  median_ages.first_device_type == c_d.first_device_type AND
                  median_ages.first_browser == c_d.first_browser")
c_d$age <- ifelse(is.na(c_d$age),fake_age$m_age,c_d$age)

m_age <- median(c_d$age,na.rm=TRUE)
c_d$age <- ifelse(is.na(c_d$age),m_age,c_d$age)
rm(fake_age,median_ages)
gc(reset=TRUE)

cat("data_structure.R - Group signup flow values before 1 hot encoding\n")
train_data$signup_flow <- ifelse((train_data$signup_flow=="0"|
                                   train_data$signup_flow=='12'|
                                   train_data$signup_flow=='23'|
                                   train_data$signup_flow=='25'), test_data$signup_flow,"other")
test_data$signup_flow <- ifelse((test_data$signup_flow=="0"|
                                   test_data$signup_flow=='12'|
                                   test_data$signup_flow=='23'|
                                   test_data$signup_flow=='25'), test_data$signup_flow,"other")

cat("data_structure.R - One hot encoding\n")
factor_variables <- c("gender","signup_method","language","affiliate_channel","affiliate_provider","first_affiliate_tracked","signup_app","first_device_type","first_browser","signup_flow")

for(i in 1:length(factor_variables)){
  v <- as.data.frame(c_d[,match(factor_variables[i],colnames(c_d))])
  names(v) <- c(paste(factor_variables[i],"_",sep=""))
  df <- data.frame(model.matrix(~.-1,v))
  #df <- df[,-1] #Remove redundant column from encoding
  c_d <- cbind(c_d,df)
}
c_d <- c_d[,-match(factor_variables,colnames(c_d))]

train_data <- c_d[1:nrow(train_data),]
train_data$country_destination <- train_cd
test_data <- c_d[(nrow(train_data)+1):nrow(c_d),]

rm(c_d,train_cd,df,i,v,factor_variables)