rm(list=ls())
getwd()
setwd("/cloud/project")
MAPE = function(ycap,y){
  cat('MAPE',mean(abs((y - ycap)/y)))
}
# Data importing
X=read.csv("dataset_Facebook.csv",sep=";",header=TRUE,na.string = '?' )
attach(X)
colnames(X)
colnames(X)[8:15]=c('Total.Reach','Total.Impressions','Lifetime.Engaged.Users','Consumers','Consumptions','Impressions.liked','Reach.liked','People.engaged.liked')
str(X)
library(ggplot2)
library(RColorBrewer)
library(skimr)
colnames(X)
skim(X)
#see that Paid contains a missing value
which(is.na(X$Paid)==T) #500
#deleting last observation due to missing value
X=X[-500,]
dim(X)  #499  19

X$Type=as.factor(X$Type)
X$Category=as.factor(X$Category)
X$Paid=as.factor(X$Paid)
X$Post.Month=as.factor(X$Post.Month)
X$Post.Hour=as.factor(X$Post.Hour)
X$Post.Weekday=as.factor(X$Post.Weekday)

#splitting into train and test
set.seed(197)
ind=sample(499,499*.8) # selecting randomly approx. 80% of observation from 499 observation
X_train=X[ind,]
X_test=X[-ind,]
dim(X_train)  #399 19
dim(X_test)   #100 19

#train set
y=X_train[,which(colnames(X)=='Lifetime.Engaged.Users')]
X=X_train[,-c(which(colnames(X)=='Lifetime.Engaged.Users'),which(colnames(X)=='like'),which(colnames(X)=='share'),which(colnames(X)=='comment'))]

#test set
y_test=X_test[,which(colnames(X_test)=='Lifetime.Engaged.Users')]
X_test=X_test[,-c(which(colnames(X_test)=='Lifetime.Engaged.Users'),which(colnames(X_test)=='like'),which(colnames(X_test)=='share'),which(colnames(X_test)=='comment'))]



cat_col=c(2,3,4,5,6,7) # a Variable containing the column indices of categorical variable
##Data summary

skim(X)
#box plot and frequency tables for categorical variable
data=cbind.data.frame(X,y)
colnames(data)[16]="Lifetime.Engaged.Users"
#box plot and frequency tables for categorical variable
ggplot(data,aes(x=Type,y=Lifetime.Engaged.Users,fill=Type))+
  geom_boxplot()+scale_fill_brewer(palette = "Dark2")+theme_classic()
table(X[,2])
ggplot(data,aes(x=Category,y=Lifetime.Engaged.Users,fill=Category))+
  geom_boxplot()+scale_fill_brewer(palette = "Dark2")+theme_classic()
table(X[,3])
ggplot(data,aes(x=Paid,y=Lifetime.Engaged.Users,fill=Paid))+
  geom_boxplot()+scale_fill_brewer(palette = "Dark2")+theme_classic()
table(X[,7])
ggplot(data,aes(x=Post.Month,y=Lifetime.Engaged.Users,fill=Post.Month))+
  geom_boxplot()+scale_fill_brewer(palette = "Paired")+theme_classic()
table(X[,4])
ggplot(data,aes(x=Post.Hour,y=Lifetime.Engaged.Users,fill=Post.Hour))+
  geom_boxplot()+theme_classic()
table(X[,6])
ggplot(data,aes(x=Post.Weekday,y=Lifetime.Engaged.Users,fill=Post.Weekday))+
  geom_boxplot()+scale_fill_brewer(palette = "Dark2")+theme_classic()
table(X[,5])
#corelation among numeric variables
pairs(X[,-cat_col])
cor(X[,-cat_col])
cor(X[,-cat_col],y)
# indicator Variable
#install.packages("fastDummies")
library(fastDummies)
length(cat_col)
X=dummy_cols(X,select_columns =colnames(X)[ cat_col],remove_first_dummy = T,remove_selected_columns = T)
X_test=dummy_cols(X_test,select_columns =colnames(X_test)[ cat_col],remove_first_dummy = T,remove_selected_columns = T)


p=(dim(X))[2]   
p     #no. of predictors   53
n=(dim(X))[1]
n     #no. of observation  399

library(olsrr)
my_data=cbind(y,X)
which(colSums(my_data)==0)  #53
my_data=my_data[,-53]
X.test=X_test[,-52]
model=lm(y~.,data=my_data)
summary(model)
dim(my_data)  #399  53
predictions <- predict(model, X_test)
MAPE(predictions,y_test)  #MAPE 0.04609676  

#leverage points
hat_value=hatvalues(model)
par(mfrow=c(1,1))
plot(hat_value,xlab ="observation",ylab="hat_values",col=(hat_value>0.6)+1)
abline(h=0.6,col="red")
lev_point=which(hat_value>0.6)      # it provides row no of data corresponding to which high hat values
length(lev_point)  #5
my_data1=my_data[-lev_point,]
c=which(colSums(my_data1)==0)
c
my_data1=my_data1[,-c]
X.test=X.test[,-(c-1)]
row.names(my_data1)=as.character(seq(1,dim(my_data1)[1]))   # assigning row names to each row of data
model1=lm(y~.,my_data1)
summary(model1)
dim(my_data1)
predictions <- predict(model1, X_test)
MAPE(predictions,y_test) ##MAPE 0.0477752  

# cooks distance
cd=cooks.distance(model1)
plot(cd,xlab="observation",ylab="cooks distance")
abline(h=0.1,col="red")
inf_cd=which(cd>0.1)
length(inf_cd) #5
ols_plot_cooksd_bar(model1)

# DFFITS
df_fit=dffits(model1)
plot(1:length(df_fit),df_fit,xlab="observation",ylab="DFFIT")
abline(h=c(2.5,-2.5),col="red")
ols_plot_dffits(model)
influence_DFFIT=which(abs(df_fit)>2.5)
length(influence_DFFIT)  #4

####DFBETA######
DFBETA_ij=dfbetas(model1)
dim(my_data1)  #394 49
influence_DFBETA=c()
for(j in 2:10)
  influence_DFBETA=c(influence_DFBETA ,which(abs(DFBETA_ij[,j])>2/sqrt(394)) )
influence_DFBETA = as.numeric(names(which( table(influence_DFBETA)>=7) ) )
length(influence_DFBETA) #13

#covratio
COVRATIO=covratio(model1)
par(mfrow=c(1,1))
plot(COVRATIO,main="Fig 3:COVRATIO for training data Red:influential points and Black :Non",
     col=(abs(COVRATIO-1)>1)+1,cex.main=0.85, pch=20)
abline(h=c( 2, 0) , col="red", lwd=2, lty="dotted")#check threshold
influence_COVRATIO=which(abs(COVRATIO-1)>1)
length(influence_COVRATIO) # 1


#####Final Influential#####
influential = sort(( c(inf_cd,influence_DFBETA,influence_DFFIT,influence_COVRATIO ) ))
final=as.numeric(names(which(table(influential)>=2)))
length(final)  #5
my_data2=my_data1[-final,]
which(colSums(my_data2)==0) #0
row.names(my_data2)=as.character(seq(1,dim(my_data2)[1]))
dim(my_data2)  #389  49
model2=lm(y~.,data=my_data2)
summary(model2)
predictions <- predict(model2, X_test)
MAPE(predictions,y_test) # MAPE 0.05115962  
my_data22=my_data2


##normality checking
y=my_data2[,1]
library(ggpubr)
ggdensity(y,fill='blue')+ggtitle('Density Curve')+labs(x='y')+theme_dark()
ggqqplot(y)+ggtitle('Normal Q-Q Plot')+theme_light()
shapiro.test(y)  #p-value < 2.2e-16  so, it is non normal

#transformation
z=log(y)
ggqqplot(z)+ggtitle('Normal Q-Q Plot')+theme_light()
shapiro.test(z)  #p-value = 1.28e-10  so, it is non normal

#excluding first 6 points
z1=sort(z)[7:389]
ggqqplot(z1)+ggtitle('Normal Q-Q Plot')+theme_light()
ggdensity(z1,fill='blue')+ggtitle('Density Curve')+theme_dark()
shapiro.test(z1) #p-value = 0.1075  so, it is normal
del_point=which(z<=sort(z)[6])

#new data with transformed response
my_data2[,1]=z
my_data2=my_data2[-del_point,]
colnames(my_data2)[1]='z'
dim(my_data2)   #382  49
model3=lm(z~.,data=my_data2)   #Our new or transformed model
summary(model3)
predictions <- predict(model3, X_test)
MAPE(exp(predictions),y_test) # MAPE 8.761486



#heteroscedasticity
library(lmtest)
bptest(model3)  #p-value =3.147e-05 implies hetero
z_cap=model3$fitted.values
e=model3$residuals
#we check through graph

plot(z_cap,e,pch=20,col='green',xlab = "Z_cap",ylab="residuals")
par(mfrow=c(3,3))
for(i in 2:10)
  plot(my_data2[,i],e,pch=20,col='red',main = sub('^','resd(y axis) vs x',i-1))


#we need to transform x
par(mfrow=c(1,1))
my_data4=my_data2
my_data4[,2:10]=log(my_data4[,2:10])
model4=lm(z~.,data=my_data4)
z_fit=model4$fitted.values
resd=model4$residuals
plot(z_fit,resd,pch=20,col='green')
for(i in 2:10)
  plot(my_data4[,i],resd,pch=20,col='red',main = sub('^','resd(y axis) vs x',i-1))
bptest(model4)  #p_value=0.02235  
X_test[,1:9]=log(X_test[,1:9])
X.test[,1:9]=log(X.test[,1:9])
predictions <- predict(model4, X_test)
MAPE(exp(predictions),y_test)  # MAPE 0.05810667
summary(model4)

#multicollinearity
# checking Multicollinearity
X_num<-my_data4[,2:10]   # containing only numerical vectors
X_num1=X_num
X_num1=scale(X_num1)
cor(X_num1)  # from this it is clear that X2,X3,X6,X7 are highly correlate also 
#X4 and X8 are highly correlated.(indication of Multicollinearity)

#Computation of X'X
X_num1=as.numeric(unlist(X_num1))
X_num1=matrix(X_num1,ncol=9,byrow = FALSE)
C=t(X_num1)%*%(X_num1)  # it is [9,9] matrix
eigen_C=(eigen(C))$values
k=max(eigen_C)/min(eigen_C)   
# here k is condition number and k=972.91 which is near to 1000 and it is
# strong evidence of Multicollinearity

library(GGally)
ggpairs(X_num)
car::vif(model4)      
# many of VIF is more than 10 means model4 has poor estimates 
#of coefficient due to multicollinearity
x_new1=my_data4[,c(3,4,7,8)]
pcr1=prcomp(x_new1,center=T,scale=T)
plot(pcr1,type='l')
summary(pcr1)
PC11=pcr1$x[,1]
PC12=pcr1$x[,2]

x_new2=my_data4[,c(5,6,9)]
pcr2=prcomp(x_new2,center=T,scale=T)
plot(pcr2,type='l')
summary(pcr2)
PC21=pcr2$x[,1]
PC22=pcr2$x[,2]
cor(my_data4$z,my_data4$Page.total.likes)
my_data5=cbind(z=my_data4[,1],PC11,PC12,PC21,PC22,my_data4[,10:49]) # first variable is excluded due to large vif and low correlation with response

model5=lm(z~.,data=my_data5)
summary(model5)
car::vif(model5)
summary(model5)

#loadings used for prediction of test data
loading1=pcr1$rotation
loading2=pcr2$rotation
X_test_pc1=as.matrix(X_test[,c(2,3,6,7)])%*%loading1
X_test_pc2=as.matrix(X_test[,c(4,5,8)])%*%loading2
X_test_pc=(as.data.frame(cbind(X_test_pc1[,1:2],X_test_pc2[,1:2],X_test[,9:53])))
colnames(X_test_pc)[1:4]=c("PC11","PC12","PC21","PC22")
predictions <- predict(model5, X_test_pc)
MAPE(exp(predictions),y_test) #

step=step(model5,direction="both")
summary(step)
model6=lm(formula = z ~ PC12 + PC21 + PC22 + Total.Interactions + Type_Status + 
            Post.Month_2 + Post.Month_3 + Post.Month_4 + Post.Month_5 + 
            Post.Month_6 + Post.Month_7 + Post.Month_8 + Post.Month_9 + 
            Post.Month_10 + Post.Month_11 + Post.Month_12 + Post.Weekday_5 + 
            Post.Hour_2 + Post.Hour_5 + Post.Hour_6 + Post.Hour_8 + Post.Hour_11 + 
            Post.Hour_14 + Post.Hour_15 + Post.Hour_12, data = my_data5)
summary(model6)
predictions <- predict(model6, X_test_pc)
MAPE(exp(predictions),y_test) #

#pcr
pcr.fit=pcr(z~.,data=my_data4,scale=T,validation='CV')
summary(pcr.fit)
validationplot(pcr.fit,val.type = 'MSEP')
pcr.pred=predict(pcr.fit,X.test,ncomp = 38 )   # change ncomp accordingly
MAPE(exp(pcr.pred),y_test)

library(pls)
# pls
pls.fit=plsr(z~.,data=my_data4,scale=T,validation='CV')
summary(pls.fit)
validationplot(pls.fit,val.type = 'MSEP')
pls.pred=predict(pls.fit,X.test,ncomp = 10 ) # change ncomp accordingly
MAPE(exp(pls.pred),y_test)

#Lasso to remove multicolinearity + variable selection
set.seed(197)
library(glmnet)
xtrain=as.matrix(my_data4[,-1])
ytrain=as.vector(my_data4[,1])
cv.out=cv.glmnet(xtrain,ytrain,type.measure = "mse",
                 alpha=1, family="gaussian")
a=coef(cv.out)[,1]
length(a[a!=0]) #17 non zero coefficients
selected_variable=names(a[a!=0])
selected_variable
plot(cv.out,xlab="log(lambda)",ylab="mean squared error")
bestlam=cv.out$lambda.min
bestlam
lasso.pred=predict(cv.out,s=bestlam,newx = as.matrix(X.test))
MAPE(exp(lasso.pred),y_test) #0.0571

#first Category
first=function(dat){
  Type_Link=rep(0,nrow(dat))
  Category_1=rep(0,nrow(dat))
  Post.Month_1=rep(0,nrow(dat))
  Post.Weekday_1=rep(0,nrow(dat))
  Post.Hour_1=rep(0,nrow(dat))
  s=rep(0,nrow(dat))
  a=which(colnames(dat)=='Type_Photo')
  for (i in a:a+2) 
    s=s+dat[,i]
  for(i in 1:length(s)){
    if (s[i]==0)
      Type_Link[i]=1
  }
  s=rep(0,nrow(dat))
  a=which(colnames(dat)=='Category_2')
  for (i in a:a+1) 
    s=s+dat[,i]
  for(i in 1:length(s)){
    if (s[i]==0)
      Category_1[i]=1
  }
  s=rep(0,nrow(dat))
  a=which(colnames(dat)=='Post.Month_2')
  for (i in a:a+10) 
    s=s+dat[,i]
  for(i in 1:length(s)){
    if (s[i]==0)
      Post.Month_1[i]=1
  }
  s=rep(0,nrow(dat))
  a=which(colnames(dat)=='Post.Weekday_2')
  for (i in a:a+6) 
    s=s+dat[,i]
  for(i in 1:length(s)){
    if (s[i]==0)
      Post.Weekday_1[i]=1
  }
  s=rep(0,nrow(dat))
  a=which(colnames(dat)=='Post.Hour_2')
  b=which(colnames(dat)=='Paid_1')
  for (i in a:b-1) 
    s=s+dat[,i]
  for(i in 1:length(s)){
    if (s[i]==0)
      Post.Hour_1[i]=1
  }
  a=which(colnames(dat)=='Paid_1')
  Paid_0=abs(dat[,a]-1)
  one=cbind(Type_Link,Category_1,Post.Month_1,Post.Weekday_1,Post.Hour_1,Paid_0)
  assign('one',one,envir = .GlobalEnv)
}
#model
one=0
first(my_data)
data=cbind(my_data[,-c(11:53)],one) #399 16
mod=lm(y~.,data=data)
summary(mod)$coefficients[,4] 
#model1
first(my_data1)
data=cbind(my_data1[,-c(11:49)],one) #394 16
mod=lm(y~.,data=data)
summary(mod)$coefficients[,4] 
#model2
first(my_data22)
data=cbind(my_data22[,-c(11:49)],one) #389 16
mod=lm(y~.,data=data)
summary(mod)$coefficients[,4] 
#model3
first(my_data2)
data=cbind(my_data2[,-c(11:49)],one) #382 16
mod=lm(z~.,data=data)
summary(mod)$coefficients[,4] 
#model4
first(my_data4)
data=cbind(my_data4[,-c(11:49)],one) #382 16
mod=lm(z~.,data=data)
summary(mod)$coefficients[,4] 
#model5
first(my_data5)
data=cbind(my_data5[,-c(7:45)],one) #382 12
mod=lm(z~.,data=data)
summary(mod)$coefficients[,4] 
#Stepwise
first(my_data5)
data=cbind(my_data5[,1:6],one,my_data5[,-c(1:7,10,13,23,29,45)])
mod=lm(z~.,data=data)
mod1=lm(z~1,data=data)
step1=step(mod1,direction="both",scope=formula(mod))
summary(step1)$call
#lasso
set.seed(197)
first(my_data4)
xtrain1=as.matrix(cbind(my_data4[,c(2:10)],one,my_data4[,-c(1:11,14,16,27,33,49)]))
ytrain1=as.vector(my_data4[,1])
cv.out1=cv.glmnet(xtrain1,ytrain1,type.measure = "mse",
                  alpha=1, family="gaussian")
a1=coef(cv.out1)[,1]
names(a1[a1!=0])


################## FINISH ####################################
