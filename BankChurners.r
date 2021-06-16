# ENVIRONMENT SETUP -----------------------
## load raw data
rm(list=ls()) # remove all existing objects in the environment
gc() #garbage collection 
setwd('C:/Users/spatel/Projects/BankChurn') # set working directory
# sink("./console_output.txt", append =T) # uncomment to write output to file
## setup packages
packages = c( "class","caret","cowplot", "DataExplorer", "dplyr",
              "DT", "fastDummies", "ggplot2", "gridExtra",
              "h2o", "highcharter","htmlwidgets","purrr", "ROCR", 
              "stringr", "tidyr", "modelr","plyr",
              "skimr","stringr","tidyverse", "ROSE", "rpart", "rpart.plot", "spplot") # required packages

installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
} # install packages not yet installed on the machine

invisible(lapply(packages, library, character.only = TRUE)) # loading required packages

# LOAD DATA -----------------------

dat0 = read.csv('BankChurners.csv', stringsAsFactors=T, head=T) # load data file
dim(dat0) # get dimensions of the data (# of rows : 10127; # of columns :23)
skim_without_charts(dat0) #  broad overview of a data frame

# DATA CLEANING ------------------------

## check missing data
matrix.na = is.na(dat0)
pmiss = colMeans(matrix.na) # proportion of missing for each column
nmiss = rowMeans(matrix.na) # proportion of missing for each row
plot(pmiss)

# There is no missing data in this dataset
#RESULT: no missing value  

dat <- dat0[,-c(1,22,23)] # take out the columns that are not required
view(dat)
# DATA PRE-PROCESSING ------------------------
## Converting factors to numeric
dat2 <- dat 
# converting columns with factors to numeric
dat2$Attrition_Flag <- as.numeric(dat$Attrition_Flag) # Attrited Customer=1; Existing Customer=2
dat2$Gender <- as.numeric(dat$Gender) # M=2; F=1
dat2$Education_Level <- as.numeric(dat$Education_Level) #	College=1; Doctorate=2; Graduate=3; High School=4; Post-Graduate=5; Uneducated=6; Unknown=7
dat2$Marital_Status <- as.numeric(dat$Marital_Status) # Divorced=1; Married=2; Single=3; Unknown=4
dat2$Income_Category <- as.numeric(dat$Income_Category) # $120K +=1; $40K - $60K=2; $60K - $80K=3; $80K - $120K =4; Less than $40K=5; Unknown=6
dat2$Card_Category <- as.numeric(dat$Card_Category) # Blue=1; Gold=2; Platinum=3; Silver =4;
View(dat2) # for use in spearman's correlation coefficeints.

## converting dummy variables and take out columns that are not required
#Creating dummy variables
dat3 <- fastDummies::dummy_columns(dat, select_columns = c('Gender', 'Education_Level', 'Marital_Status','Income_Category', 
                                                           'Card_Category'))
#changing column names
dat3$Education_Level_High_School = dat3$`Education_Level_High School`
dat3$Education_Level_Post_Graduate = dat3$`Education_Level_Post-Graduate`
dat3$Income_Category_40K_60K = dat3$`Income_Category_$40K - $60K`
dat3$Income_Category_60_80K = dat3$`Income_Category_$60K - $80K`
dat3$Income_Category_80K_120K = dat3$`Income_Category_$80K - $120K`
dat3$Income_Category_120K_Plus= dat3$`Income_Category_$120K +`
dat3$Income_Category_Less_than_40K = dat3$`Income_Category_Less than $40K`
#Dropping Columns
dat3$Gender<-NULL
dat3$Education_Level<-NULL
dat3$Marital_Status<-NULL
dat3$Income_Category<-NULL
dat3$Card_Category<-NULL
dat3$`Education_Level_High School` <- NULL
dat3$`Education_Level_Post-Graduate` <- NULL
dat3$`Income_Category_$40K - $60K` <- NULL
dat3$`Income_Category_$60K - $80K` <- NULL
dat3$`Income_Category_$80K - $120K`<- NULL
dat3$`Income_Category_$120K +` <- NULL
dat3$`Income_Category_Less than $40K` <- NULL

# take out the columns that are not required
dat3 <- dat3[,-c(17,18)]
View(dat3)
# ***************** NOTE *****************    #
# dat: original data cleaned ;                #
# dat2: factors converted to numeric ;        #
# dat3: factors converted to dummy variables  #

# DATA EXPLORATION-----------------------------------

splitList = split(dat, dat$Attrition_Flag) # Split dat into a list of dataframes, one for existing and one for attrited customer

attrited = splitList[[1]] # View Structure of split
existing = splitList[[2]] # View Structure of split

summary(attrited) # Summary of attrited customers
summary(existing) # Summary of existing customers

## Exploratory Data Analysis using DataExplorer (keep only one out of the three)
DataExplorer::create_report(dat, output_file = "./output/report_clean_data_dat.html", report_title = "Data Profiling Report for dataset with original data cleaned") # report for dataset with original data cleaned
# DataExplorer::create_report(dat2, output_file = "report_factor_to_numeric_dat2.html", report_title = "Data Profiling Report for dataset with factors converted to numeric") # report for dataset with factors converted to numeric
# DataExplorer::create_report(dat3, output_file = "report_factor_to_dummy_dat3.html", report_title = "Data Profiling Report for dataset with factors converted to dummy variable") # report for dataset with factors converted to dummy variable


## frequency tables for categorical variables
table(dat$Attrition_Flag) 
table(dat$Gender)
table(dat$Education_Level)
table(dat$Marital_Status)
table(dat$Income_Category)
table(dat$Card_Category)
## distributions of the quantitative variables
quant <- dat %>%
          keep(is.numeric) %>% 
          gather() %>% 
          ggplot(aes(value)) +
          facet_wrap(~ key, scales = "free") +
          geom_histogram(fill = "#f26b38") +
          sp_theme()
brand_plot(quant)

## Boxplots for detecting outliers

bp1 <- ggplot(dat, aes(x = Attrition_Flag, y = Customer_Age, fill = Attrition_Flag)) + 
  geom_boxplot() + 
  scale_fill_manual(values = c("#f26b38", "#38bff2")) + 
  labs(y= 'Customer Age') + 
  sp_theme() + 
  theme(legend.position = "none")
brand_plot(bp1)

bp2 <- ggplot(dat, aes(x = Attrition_Flag, y = Dependent_count, fill = Attrition_Flag)) + 
  geom_boxplot() + 
  scale_fill_manual(values = c("#f26b38", "#38bff2")) + 
  labs(y= 'Dependent Count') + 
  sp_theme() + 
  theme(legend.position = "none")
brand_plot(bp2)

bp3 <- ggplot(dat, aes(x = Attrition_Flag, y = Months_on_book, fill = Attrition_Flag)) + 
  geom_boxplot() + 
  scale_fill_manual(values = c("#f26b38", "#38bff2")) + 
  labs(y= 'Months on Book') + 
  sp_theme() + 
  theme(legend.position = "none")
brand_plot(bp3)

bp4 <- ggplot(dat, aes(x = Attrition_Flag, y = Total_Relationship_Count, fill = Attrition_Flag)) + 
  geom_boxplot() + 
  scale_fill_manual(values = c("#f26b38", "#38bff2")) + 
  labs(y= 'Total Relationship Count') + 
  sp_theme() + 
  theme(legend.position = "none")
brand_plot(bp4)

bp5 <- ggplot(dat, aes(x = Attrition_Flag, y = Months_Inactive_12_mon, fill = Attrition_Flag)) + 
  geom_boxplot() + 
  scale_fill_manual(values = c("#f26b38", "#38bff2")) + 
  labs(y= 'Months Inactive (12 Month Period)') + 
  sp_theme() + 
  theme(legend.position = "none")
brand_plot(bp5)

bp6 <- ggplot(dat, aes(x = Attrition_Flag, y = Contacts_Count_12_mon, fill = Attrition_Flag)) + 
  geom_boxplot() + 
  scale_fill_manual(values = c("#f26b38", "#38bff2")) + 
  labs(y= 'Contract Count (12 Month Period)') + 
  sp_theme() + 
  theme(legend.position = "none")
brand_plot(bp6)

bp7 <- ggplot(dat, aes(x = Attrition_Flag, y = Credit_Limit, fill = Attrition_Flag)) + 
  geom_boxplot() + 
  scale_fill_manual(values = c("#f26b38", "#38bff2")) + 
  labs(y= 'Credit Limit') + 
  sp_theme() + 
  theme(legend.position = "none") 
brand_plot(bp7)

bp8 <- ggplot(dat, aes(x = Attrition_Flag, y = Avg_Open_To_Buy, fill = Attrition_Flag)) + 
  geom_boxplot() + 
  scale_fill_manual(values = c("#f26b38", "#38bff2"))  + 
  labs(y= 'Average Open to Buy') + 
  sp_theme() + 
  theme(legend.position = "none")
brand_plot(bp8)

## CATEGORICAL FREQUENCY

p1 <- ggplot(dat, aes(x = Gender, fill = Gender)) +
  geom_bar() +
  geom_text(aes(y = ..count.. -200, 
                label = paste0(round(prop.table(..count..),4) * 100, '%')), 
            stat = 'count', 
            position = position_dodge(.1), 
            size = 3) + 
  sp_theme() + 
  theme(legend.position = "none")
p1 <- brand_plot(p1)

p2 <- ggplot(dat, aes(x = Dependent_count, fill = as.factor(Dependent_count))) +
  geom_bar() +
  geom_text(aes(y = ..count.. -200, 
                label = paste0(round(prop.table(..count..),4) * 100, '%')), 
            stat = 'count', 
            position = position_dodge(.1), 
            size = 3) + 
  sp_theme() + 
  theme(legend.position = "none")
p2 <- brand_plot(p2)

p3 <- ggplot(dat, aes(x = Total_Relationship_Count,fill = as.factor(Total_Relationship_Count))) +
  geom_bar() +
  geom_text(aes(y = ..count.. -200, 
                label = paste0(round(prop.table(..count..),4) * 100, '%')), 
            stat = 'count', 
            position = position_dodge(.1), 
            size = 3) + 
  sp_theme() + 
  theme(legend.position = "none")
p3 <- brand_plot(p3)

p4 <- ggplot(dat, aes(x = Marital_Status, fill = Marital_Status)) +
  geom_bar() +
  geom_text(aes(y = ..count.. -200, 
                label = paste0(round(prop.table(..count..),4) * 100, '%')), 
            stat = 'count', 
            position = position_dodge(.1), 
            size = 3) + 
  sp_theme() + 
  theme(legend.position = "none")
p4 <- brand_plot(p4)

p5 <- ggplot(dat, aes(x = Income_Category,fill = Income_Category)) +
  geom_bar() +
  geom_text(aes(y = ..count.. -200, 
                label = paste0(round(prop.table(..count..),4) * 100, '%')), 
            stat = 'count', 
            position = position_dodge(.1), 
            size = 3) + 
  sp_theme() + 
  theme(legend.position = "none")
p5 <- brand_plot(p5)

p6 <- ggplot(dat, aes(x = Card_Category, fill = Card_Category)) +
  geom_bar() +
  geom_text(aes(y = ..count.. -200, 
                label = paste0(round(prop.table(..count..),4) * 100, '%')), 
            stat = 'count', 
            position = position_dodge(.1), 
            size = 3) + 
  sp_theme() + 
  theme(legend.position = "none")
p6 <- brand_plot(p6)

p7 <- ggplot(dat, aes(x = Months_on_book, fill = asfactor(Months_on_book))) +
  geom_bar() +
  geom_text(aes(y = ..count.. -200, 
                label = paste0(round(prop.table(..count..),4) * 100, '%')), 
            stat = 'count', 
            position = position_dodge(.1), 
            size = 3) + 
  sp_theme() + 
  theme(legend.position = "none")
p7 <- brand_plot(p7)

p8 <- ggplot(dat, aes(x = Months_Inactive_12_mon, fill = as.factor(Months_Inactive_12_mon))) +
  geom_bar() +
  geom_text(aes(y = ..count.. -200, 
                label = paste0(round(prop.table(..count..),4) * 100, '%')), 
            stat = 'count', 
            position = position_dodge(.1), 
            size = 3) + 
  sp_theme() + 
  theme(legend.position = "none")
p8 <- brand_plot(p8)

grid.arrange(p1, p2, p3, p4, ncol=2)
grid.arrange(p5,p6,p7,p8, ncol=2)

p9 <- ggplot(dat, aes(x = Attrition_Flag, fill = Attrition_Flag)) +
  geom_bar() +
  geom_text(aes(y = ..count.. -200, 
                label = paste0(round(prop.table(..count..),4) * 100, '%')), 
            stat = 'count', 
            position = position_dodge(.1), 
            size = 3) + 
  sp_theme() + 
  theme(legend.position = "none")
p9 <- brand_plot(p9)
# Attrition flag bar charts 
plot1 <- ggplot(data = dat, aes(x = Gender, fill = Attrition_Flag)) +
  geom_bar(position = "fill") + 
  sp_theme()+ 
  scale_fill_manual(values = c("#f26b38", "#38bff2"))
plot1 <-brand_plot(plot1)

plot2 <- ggplot(data = dat, aes(x = Dependent_count, fill = Attrition_Flag)) +
  geom_bar(position = "fill") + 
  sp_theme()+ 
  scale_fill_manual(values = c("#f26b38", "#38bff2"))
plot2 <-brand_plot(plot2)

plot3 <- ggplot(data = dat, aes(x = Education_Level, fill = Attrition_Flag)) +
  geom_bar(position = "fill") + 
  sp_theme()+ 
  scale_fill_manual(values = c("#f26b38", "#38bff2"))
plot3 <-brand_plot(plot3)

plot4 <- ggplot(data = dat, aes(x = Marital_Status, fill = Attrition_Flag)) +
  geom_bar(position = "fill")+ 
  sp_theme()+ 
  scale_fill_manual(values = c("#f26b38", "#38bff2"))
plot4 <-brand_plot(plot4)

plot5 <- ggplot(data = dat, aes(x = Income_Category, fill = Attrition_Flag)) +
  geom_bar(position = "fill") + 
  sp_theme()+ 
  scale_fill_manual(values = c("#f26b38", "#38bff2"))
plot5 <-brand_plot(plot5)

plot6 <- ggplot(data = dat, aes(x = Card_Category, fill = Attrition_Flag)) +
  geom_bar(position = "fill") + 
  sp_theme()+ 
  scale_fill_manual(values = c("#f26b38", "#38bff2"))
plot6 <-brand_plot(plot6)

plot7 <- ggplot(data = dat, aes(x = Total_Relationship_Count, fill = Attrition_Flag)) +
  geom_bar(position = "fill") + 
  sp_theme()+ 
  scale_fill_manual(values = c("#f26b38", "#38bff2"))
plot7 <-brand_plot(plot7)

plot8 <- ggplot(data = dat, aes(x = Months_Inactive_12_mon, fill = Attrition_Flag)) +
  geom_bar(position = "fill")+ 
  sp_theme()+ 
  scale_fill_manual(values = c("#f26b38", "#38bff2"))
plot8 <-brand_plot(plot8)

plot9 <- ggplot(data = dat, aes(x = Contacts_Count_12_mon, fill = Attrition_Flag)) +
  geom_bar(position = "fill")+ 
  sp_theme()+ 
  scale_fill_manual(values = c("#f26b38", "#38bff2"))
plot9 <-brand_plot(plot9)

grid.arrange(plot1, plot2, plot3, plot4, plot5, plot6, plot7, plot8, plot9, nrow=3, ncol=3)

# COLLINEARITY -----------------------------------

## Co-relation plot (using dat2) with Spearman method
cor_spearman <- cor(dat2[, sapply(dat2, is.numeric)], method = 'spearman')
View(cor_spearman)
# Visualizing with a heatmap the correlation matrix with the pearson method
hc <- as.matrix(data.frame(cor_spearman)) %>% 
  round(3) %>% #round
  hchart() %>% 
  hc_add_theme(hc_theme_smpl()) %>%
  hc_title(text = "Spearman's correlation coefficients", align = "center") %>% 
  hc_legend(align = "center") %>% 
  hc_colorAxis(stops = color_stops(colors = viridis::inferno(10))) %>%
  hc_plotOptions(
    series = list(
      boderWidth = 0,
      dataLabels = list(enabled = TRUE)))
saveWidget(hc, file="./spearman_coreelation_coefficients.html")
# REMOVE OUTLIERS + HIGH CORRELATION ----------------------------------------
dat4 = dat[dat$Total_Amt_Chng_Q4_Q1 < 3, ]
dat5 = dat2[dat2$Total_Amt_Chng_Q4_Q1 < 3, ]
dat6 = dat3[dat3$Total_Amt_Chng_Q4_Q1 < 3, ]

dat7 = subset(dat4, select = -c(Total_Trans_Amt, Avg_Open_To_Buy)) #original cleaned data w/o outliers and columns w/ collinearity >.8
dat8 = subset(dat5, select = -c(Total_Trans_Amt, Avg_Open_To_Buy)) # as. numeric
dat9 = subset(dat6, select = -c(Total_Trans_Amt, Avg_Open_To_Buy)) ##DUMMY (FOR KNN)

# SAMPLING ----------------------------------------

#over-sampling for dummy
dat9_over<- ovun.sample(formula = Attrition_Flag ~ ., data = dat9, method = "over")$data
table(dat9_over$Attrition_Flag)
dim(dat9_over)
View(dat9_over)

# NORMALIZATION ----------------------------------------

# split datset into numeric and dummy
dat_9_over_numeric <- dat9_over[ , c(1:13)] 
dat_9_over_dummy <- dat9_over[ , c(14:34)]

# pre-process and normalize numerics
preproc3 <- preProcess(dat_9_over_numeric, method=c("center", "scale"))
normalized_dat9_over_numeric <- predict(preproc3, dat_9_over_numeric)

# combine normalized numeric with dummy
normalized_dat9_over <- cbind(normalized_dat9_over_numeric, dat_9_over_dummy)
summary(normalized_dat9_over)
view(normalized_dat9_over)
# KNN ALGORITHM IMPLEMENTATION ----------------------------------------
train <- normalized_dat9_over[,-c(26,28,16,21,22)]

set.seed(1)
n.train = floor( nrow(train)*0.80 )
ind.train = sample(1:nrow(train), n.train)
ind.test = setdiff(1:nrow(train), ind.train)

require(class)
Xtrain = train[ind.train, 2:29]
Xtest = train[ind.test,2:29]
ytrain = train[ind.train,1]
ytest = train[ind.test,1]

get.prob = function(x) {
  prob = attr(x, 'prob')
  if ( sum(x==0)==length(x) ) {
    prob = 1 - prob
  } else if ( sum(x==1)!=length(x) ) {
    cl = as.numeric(x)
    ind = which(cl == 1)
    prob[ind] = 1 - prob[ind]
  }
  return(prob)
}

get.prob = function(x) {
  prob = attr(x, 'prob')
  if ( sum(x==0)==length(x) ) {
    prob = 1 - prob
  } else if ( sum(x==1)!=length(x) ) {
    cl = as.numeric(x)
    ind = which(cl == 1)
    prob[ind] = 1 - prob[ind]
  }
  return(prob)
}

knn.bestK = function(train, test, y.train, y.test, k.grid = 1:20, ct = .5) {
  # browser()
  fun.tmp = function(x) {
    y.tmp = knn(train, test, y.train, k = x, prob=T) # run knn for each k in k.grid
    prob = get.prob(y.tmp)
    y.hat = as.numeric( prob > ct )
    return( sum(y.hat != as.numeric(y.test)) )
  }
  ## create a temporary function (fun.tmp) that we want to apply to each value in k.grid
  error = unlist(lapply(k.grid, fun.tmp))
  names(error) = paste0('k=', k.grid)
  ## it will return a list so I need to unlist it to make it to be a vector
  out = list(k.optimal = k.grid[which.min(error)], 
             error.min = min(error)/length(y.test),
             error.all = error/length(y.test))
  return(out)
}

obj1 = knn.bestK(Xtrain, Xtest, ytrain, ytest, seq(1, 18, 2), .5)
obj1

ypred1 = knn(Xtrain, Xtest, ytrain, k=obj1$k.optimal, prob=T)
table(ytest, ypred1)

# LOGISTIC REGRESSION ----------------------------------------

# take a look at how many acceptance and rejection first
min.model = glm(Attrition_Flag ~ 1, data = normalized_dat9_over, family = 'binomial')
max.model = glm(Attrition_Flag ~ ., data = normalized_dat9_over, family = 'binomial')
max.formula = formula(max.model)

obj = step(min.model, direction='forward', scope=max.formula) # it will print out models in each step
summary(obj)


get.or = function(sobj, alpha=.05) {
  b = sobj$coef[-1, 'Estimate']
  se.b = sobj$coef[-1, 'Std. Error']
  pval = sobj$coef[-1, 'Pr(>|z|)']
  or = exp(b); se.or = exp(b)*se.b
  lb = b + qnorm(alpha/2)*se.b; lb.or = exp(lb)
  ub = b + qnorm(1-alpha/2)*se.b; ub.or = exp(ub)
  out = cbind(or, se.or, lb.or, ub.or, pval)
  colnames(out) = c('OR', 'SE', paste0((1-alpha)*100, '% CI, lower'),
                    paste0((1-alpha)*100, '% CI, upper'), 'p value')
  return(out)
}
get.or(summary(obj))

## what if we want to partition the data and evaluate model ##
sample <- sample.int(n = nrow(normalized_dat9_over), size = floor(.80*nrow(normalized_dat9_over)), replace = F)
train <- normalized_dat9_over[sample, ]
test  <- normalized_dat9_over[-sample, ]
nrow(train)
nrow(test)
View(test)


min.model = glm(Attrition_Flag ~ 1, data = train, family = 'binomial')
max.model = glm(Attrition_Flag ~ ., data = train, family = 'binomial')
max.formula = formula(max.model)

#Forward Selection	
obj1 = step(min.model, direction='forward', scope=max.formula) # it will print out models in each step	
summary(obj1) # it will give you the final model	
get.or(summary(obj1)) # 19 variables	

#backward selection	
obj2 = step(max.model, direction='backward', scope=max.model) # it will print out models in each step	
summary(obj2) # it will give you the final model	
get.or(summary(obj2)) #22 variables	

#Stepwise prediction	
obj3 = step(min.model, direction='both', scope= list(lower=min.model, upper=max.model)) # it will print out models in each step	
summary(obj3) # it will give you the final model	
get.or(summary(obj3)) #variables

yhat = predict(obj, newdata = test, type='response')
hist(yhat)

dichotomize = function(yhat, cutoff=.5) {
  out = rep(0, length(yhat))
  out[yhat > cutoff] = 1
  out
}

yhat.class = dichotomize(yhat, .5)
unique(yhat.class)
ytestl = as.numeric(test$Attrition_Flag)- 1
unique(ytestl)
err = mean(yhat.class != ytestl) # misclassification error rate
err
table(yhat.class, ytestl)

sen = function(ytrue, yhat) {
  ind.true1 = which(ytrue == 1)
  mean( ytrue[ind.true1] == yhat[ind.true1] )
}

spe = function(ytrue, yhat) {
  ind.true0 = which(ytrue == 0)
  mean( ytrue[ind.true0] == yhat[ind.true0] )
}

sen(ytestl, yhat.class)# 83.00117
spe(ytestl, yhat.class)#81.35991

# CART ----------------------------------------

# Classification Tree with rpart
K = 10 # number of cross-validations
fit = rpart(Attrition_Flag ~ ., method="class", data=normalized_dat9_over, cp = 1e-2, minsplit=5, xval=K) # same as using all other variables as predictors

# Minimum Error Tree
pfit.me = prune(fit, cp = fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])
rpart.plot(pfit.me, main = 'Min Error Tree')

# Best Pruned Tree
ind = which.min(fit$cptable[,"xerror"]) # xerror: cross-validation error
se1 = fit$cptable[ind,"xstd"]/sqrt(K) # 1 standard error
xer1 = min(fit$cptable[,"xerror"]) + se1 # targeted error: min + 1 SE
ind0 = which.min(abs(fit$cptable[1:ind,"xerror"] - xer1)) # select the tree giving closest xerror to xer1
pfit.bp = prune(fit, cp = fit$cptable[ind0,"CP"])
rpart.plot(pfit.bp, main = 'Best Pruned Tree')


yhat = predict(pfit.bp, normalized_dat9_over, type = "class") 
err.bp = 1 - mean(yhat == normalized_dat9_over$Attrition_Flag)

# if you want to use a cutoff not equal to 0.5
prob1 = predict(pfit.bp, normalized_dat9_over, type = "prob")[,2]
pred.class = as.numeric(prob1 > .5) # change to .3 or .4,.5
ytest = as.numeric(normalized_dat9_over$Attrition_Flag)-1
unique(as.numeric(ytest))
unique(normalized_dat9_over$Attrition_Flag)
err.bp.newCut = 1 - mean(pred.class == ytest)
err.bp.newCut

table(pred.class, ytest)
#Confusion matrix and Stats
confusionMatrix(yhat, normalized_dat9_over$Attrition_Flag) 

sen = function(ytrue, yhat) {
  ind.true1 = which(ytrue == 1)
  mean( ytrue[ind.true1] == yhat[ind.true1] )
}

spe = function(ytrue, yhat) {
  ind.true0 = which(ytrue == 0)
  mean( ytrue[ind.true0] == yhat[ind.true0] )
}

sen(ytest, pred.class)
spe(ytest, pred.class)

# CAPTURE GENERATED PLOTS -------------------------------------------------
plots.dir.path <- list.files(tempdir(), pattern="rs-graphics", full.names = TRUE); 
plots.png.paths <- list.files(plots.dir.path, pattern=".png", full.names = TRUE)
file.copy(from=plots.png.paths, to="./")