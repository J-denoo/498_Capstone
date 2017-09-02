
library(data.table)
library(dplyr)
library(randomForest)

#Read file
data = fread('data.csv', select = c("Semana",'Cliente_ID', 'Producto_ID', 'Agencia_ID', 'Ruta_SAK','Canal_ID','Dev_uni_proxima','Dev_proxima', 'Demanda_uni_equil'))
# dimemsion
dim(data)     #[1] 36631    12
#display summary statistics
summary(data)

#Data has 7 weeks of record, set aside weeks 8 and 9 for testing and
#weeks 3 through 7 for training
test = filter(data, Semana > 7 )
train = filter(data, Semana < 8)
dim(train)  # [1] 26165     8
dim(test)  #  10466     8
# check summary statistics and quantiles of the target variable in the train data
summary(train$Demanda_uni_equil)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
# 0.0    16.0    95.0   140.3   200.0  2538.0
quantile(train$Demanda_uni_equil, c(.90, .95, .98, 0.99))
#  90%    95%    98%    99%
#   333.00 448.00 626.00 777.36




train_group =train %>%  group_by( Cliente_ID, Producto_ID,Ruta_SAK,Agencia_ID,Canal_ID) %>%
    summarize( meanDemand = mean(Demanda_uni_equil ),
               meanReturn = mean(Dev_uni_proxima ) )
hist(train_group$meanDemand)
hist(train_group$meanReturn)

train_group$meanDemand = log(train_group$meanDemand +1 )
train_group$meanReturn = log(train_group$meanReturn +1)
train = merge(train, train_group, by = c('Cliente_ID', 'Producto_ID', 'Ruta_SAK', 'Agencia_ID', 'Canal_ID'))

# do same to to test data
test = merge(test, train_group, by = c('Cliente_ID', 'Producto_ID', 'Ruta_SAK', 'Agencia_ID', 'Canal_ID'))

#ZeromeanDemand = filter(train, meanDemand == 0)
#plot(ZeromeanDemand$Dev_uni_proxima )

# create frequency count of some variables in train
freq_client <- train %>%  group_by(Semana, Cliente_ID) %>%
    summarize(Count = n()) %>% group_by(Cliente_ID) %>% summarize( Client.freq = mean(Count))
freq_Agents <- train  %>% group_by(Semana, Agencia_ID) %>%
    summarize(Count = n()) %>% group_by(Agencia_ID) %>% summarize( Agent.freq = mean(Count))
freq_Prod <- train %>%  group_by(Semana, Producto_ID) %>%
    summarize(Count = n()) %>% group_by(Producto_ID) %>% summarize( Prod.freq = mean(Count))
freq_Ruta <- train %>% group_by(Semana, Ruta_SAK) %>%
    summarize(Count = n()) %>% group_by(Ruta_SAK) %>% summarize( Ruta.freq = mean(Count))

train = merge(train, freq_client, by = 'Cliente_ID')
train = merge(train, freq_Prod, by =  'Producto_ID')
train = merge(train, freq_Ruta, by =  'Ruta_SAK')
train = merge(train, freq_Agents, by =  'Agencia_ID')

#Do same for test
test = merge(test, freq_client, by = 'Cliente_ID')
test = merge(test, freq_Prod, by =  'Producto_ID')
test = merge(test, freq_Ruta, by =  'Ruta_SAK')
test = merge(test, freq_Agents, by =  'Agencia_ID')



lag = train
lag$Semana = lag$Semana + 2
dim(lag)
head(lag)

# Rename lag columns and select variables to merge, avoid duplication
colnames(lag)[9] <- "lagDemand"
colnames(lag)[7] <- "lagReturn"
lag = select(lag, Semana,Cliente_ID, Agencia_ID, Ruta_SAK,Producto_ID, Canal_ID,lagReturn, lagDemand)

hist(lag$lagDemand)
hist(lag$lagReturn)

hist(log(lag$lagDemand + 1))
hist(log(lag$lagReturn +1))

lag$lagDemand = log(lag$lagDemand + 1)
lag$lagReturn= log(lag$lagReturn +1)


lag_train = filter(lag, Semana <8)
lag_test = filter(lag, Semana >7)
train = filter(train, Semana >4)

train = merge(train, lag_train, by = c('Semana','Cliente_ID', 'Agencia_ID', 'Ruta_SAK','Producto_ID', 'Canal_ID'))
test = merge(test, lag_test, by = c('Semana','Cliente_ID', 'Agencia_ID', 'Ruta_SAK','Producto_ID', 'Canal_ID'))


train$Target = train$Demanda_uni_equil
train$Demanda_uni_equil = NULL
train$Dev_uni_proxima = NULL
train$Dev_proxima = NULL

# hist(train$Target)
# train$Target = log(train$Target+1)


test$Target = test$Demanda_uni_equil
test$Demanda_uni_equil = NULL
#copy test for anlysis after model



train800 = filter(train, Target > 800)
a = unique(train800$Cliente_ID)
#notIn
`%!in%` <- function(a,b) ! a %in% b
train1 = filter(train,Cliente_ID %!in% a )
test1 = filter(test,Cliente_ID %!in% a )
dim(train1)
dim(test1)

train2 = filter(train,Cliente_ID %in% a )
test2 = filter(test,Cliente_ID %in% a )
dim(train2)
dim(test2)

summary(train1$Target)
quantile(train1$Targetl, c(.90, .95, .98, 0.99))
summary(train2$Target)
quantile(train2$Target, c(.90, .95, .98, 0.99))


test_compute1 = test1
test1$Dev_uni_proxima = NULL
test1$Dev_proxima = NULL
actual_1 = test1$Target
test1$Target = 0

test_compute2 = test2
test2$Dev_uni_proxima = NULL
test2$Dev_proxima = NULL
actual_2 = test2$Target
test2$Target = 0

test$Dev_uni_proxima = NULL
test$Dev_proxima = NULL
actual = test$Target
test$Target = 0


# c actual = test_pData$Target
# c test_pData$Target = 0
# bulid model for  class 1 clients
set.seed(2)
rf.model1=randomForest(Target~., data=train1, importance =TRUE)  #947.2273
yhat.rf1 = predict(rf.model1 ,newdata=test1)

yhat.rf1 = round(yhat.rf1, 0)
yhat.rf1[yhat.rf1 < 1 ] = 0
mean((actual_1 -yhat.rf1)^2)                  # [1] 947.2273

varImpPlot(rf.model1, col="navy")


sqrt(1/length(actual_1)*(sum((log(yhat.rf1 +1)-log(actual_1 +1))^2)))    #[1] 0.7690447


##########
##########
##########
########## For customer with bigger demand, lag returns does not seem to help determine demand.
set.seed(2)
rf.model2=randomForest(Target~. -Cliente_ID -Agencia_ID -Client.freq -lagReturn -meanReturn , data=train2, importance =TRUE)   # 5870.828
yhat.rf2 = predict(rf.model2 ,newdata=test2)

yhat.rf2 = round(yhat.rf2, 0)
yhat.rf2[yhat.rf2 < 1 ] = 0

mean((actual_2 -yhat.rf2)^2)  #  [1] 5870.828
varImpPlot(rf.model2, col="navy")

plot(actual_2 ,yhat.rf2)

sqrt(1/length(actual_2)*(sum((log(yhat.rf2 +1)-log(actual_2 +1))^2)))


#mean(abs(actual -yhat.rf)) # [1] 30.68422

## House cleaning and applying heuristic findings
test_compute1$Predict =yhat.rf1
test_compute2$Predict =yhat.rf2

## combine the 2 models
test_compute = rbind(test_compute1, test_compute2)

mean((test_compute$Predict- test_compute$Target)^2) #[1] 3277.776

sqrt(1/length(test_compute$Target)*(sum((log(test_compute$Predict +1)-log(test_compute$Target +1))^2)))
#0.863598
mean(abs(test_compute$Target - test_compute$Predict)) #30.65154

test_compute$unitPrice = test_compute$Dev_proxima/test_compute$Dev_uni_proxima

test_compute$PredDiff = test_compute$Target - test_compute$Predict

test_compute$DiffCost = test_compute$unitPrice*test_compute$PredDiff



# current return cost for the 2 weeks
sum(test_compute$Dev_proxima)
#[1] 2364995

#Return after produced by the model
sum(test_compute$DiffCost[test_compute$DiffCost > 0])
#costR [1] 1748850


#Savings =
#2364995 - 1748850


# Lost of Sales revenue
sum(test_compute$DiffCost[test_compute$DiffCost < 0])


#Saving 616,145

#Breakeven if profit margin =
#616145     (( It cost us this much))

 #   1845119  ( cost profit margin)
#  = [1] 0.3339324

# SO If profit margin is bigger than 0.333 then model will not yield saving
#but we believe we can increase the break even by alot if we have more lag demand records included in the mode

set.seed(2)
rf.model=randomForest(Target~. -Cliente_ID -Agencia_ID -Client.freq -lagReturn -meanReturn -Canal_ID, data=train, mtry = 4, importance =TRUE)   # 5870.828
yhat.rf = predict(rf.model ,newdata=test)
yhat.rf = round(yhat.rf, 0)
yhat.rf[yhat.rf < 1 ] = 0
mean((actual -yhat.rf)^2)



ac$Predict[ac$meanDemand == 0 ] <- 0
test_compute$Prediction[test_compute$meanDemand == 650 ] <- test_compute$meanDemand

#read prod name, town and client information
client = fread("client.csv")
town = fread("town_state2.csv")
 prod = fread("prod.csv")
head(prod)

#combine prediction data with prod name, town and client information
df = merge(test_compute, client, by = 'Cliente_ID')
df = merge(df, town, by = 'Agencia_ID')
df = merge(df, prod, by = 'Producto_ID'


fwrite(df, ‘df.csv')