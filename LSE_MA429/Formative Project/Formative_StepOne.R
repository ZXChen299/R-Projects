# MA 429
# Mock Project
# for adult income prediction

# Loading the data --------------------------------------------------------

rm(list = ls())

# setwd("C:/Users/zxmaz/OneDrive/Desktop/R files/MA429/Formative_Projects/data")
# X.adult.train = read.table('adult.data', sep = ',', fill = F, strip.white = T)
# X.adult.test = read.table('adult.test', sep = ',', fill = F, strip.white = T,skip = 1)

X.adult.train = read.table('https://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data', 
                           sep = ',', fill = F, strip.white = T)
colnames(X.adult.train) = c("age", "workclass", "fnlwgt", "education", "education_num", 
                            "marital_status", "occupation", "relationship", "race", "sex",
                            "capital_gain", "capital_loss", "hours_per_week", "native_country", 
                            "income_class")

X.adult.test = read.table('https://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.test', 
                          sep = ',', fill = F, strip.white = T,skip = 1)
colnames(X.adult.test) = c("age", "workclass", "fnlwgt", "education", "education_num", 
                           "marital_status", "occupation", "relationship", "race", "sex",
                           "capital_gain", "capital_loss", "hours_per_week", "native_country", 
                           "income_class")

is.na(X.adult.train) = X.adult.train=='?'
is.na(X.adult.train) = X.adult.train==' ?'
X.adult.train = na.omit(X.adult.train) # total number: 30162

is.na(X.adult.test) = X.adult.test=='?'
is.na(X.adult.test) = X.adult.test==' ?'
X.adult.test = na.omit(X.adult.test)  # total number: 15060

X.adult = rbind(X.adult.train, X.adult.test)

X.adult$workclass = as.character(X.adult$workclass)
X.adult$occupation = as.character(X.adult$occupation)
X.adult$native_country = as.character(X.adult$native_country)

X.adult$native_country = factor(X.adult$native_country)
X.adult$workclass = factor(X.adult$workclass)
X.adult$occupation = factor(X.adult$occupation)

X.adult.train = X.adult[1:30162, ]
X.adult.test = X.adult[30163:45222, ]
dim(X.adult.train)[1] + dim(X.adult.test)[1] == dim(X.adult)[1]

# Predictor Analysis ------------------------------------------------------
data <- X.adult.train
str(data)

# Association Rule
library(arules)
library(arulesViz)
factor.data <- cbind(data$workclass,data$education,data[,6:10],data$native_country, data$income_class)
colnames(factor.data) = c("workclass", "education", "marital_status", "occupation", 
                          "relationship", "race", "sex","native_country", "income_class")
rules0 = apriori(factor.data, parameter=list(support=0.001,confidence=0.5))
summary(rules0)
inspect(rules0[1:10])
plot(rules0, method="graph")

# for income_class only
rules1<-apriori(factor.data,parameter=list(maxlen=2,supp=0.001,conf=0.1),appearance=list(rhs="income_class=<=50K",default="lhs"))
rules1.sorted_sup = sort ( rules1, by="confidence" )
inspect(rules1.sorted_sup)
plot(rules1, method="graph")
plot(rules1, method="paracoord")
plot(rules1, method="grouped")
plot(rules1.sorted_sup, method = "graph", measure = "confidence", control = list(measureLabels = TRUE))

# confidence between 0.45 - 0.55: {occupation=Prof-specialty}, {marital_status=Married-civ-spouse}, 
#                                 {relationship=Husband}, {occupation=Exec-managerial}, {relationship=Wife},
#                                 {workclass=Self-emp-inc}

rules2<-apriori(factor.data,parameter=list(maxlen=2,supp=0.001,conf=0.1),appearance=list(rhs="income_class=>50K",default="lhs"))
rules2.sorted_sup = sort ( rules2, by="confidence" )
inspect(rules2.sorted_sup)
plot(rules2, method="graph")

# confidence between 0.45 - 0.55: {workclass=Self-emp-inc}, {relationship=Wife}, {occupation=Exec-managerial},
#                                 {relationship=Husband}, {marital_status=Married-civ-spouse}


# suggestion: clean up the relationship into "parents" 

# Logistic Regression
library(fastDummies)
glm.fit = glm(income_class ~ .,data=X.adult.train, family=binomial)
summary (glm.fit)
levels(X.adult.train$workclass)
results <- fastDummies::dummy_cols(X.adult.train)
results <- cbind(results$age,results$fnlwgt,results$education_num, results$capital_gain, 
                 results$capital_loss, results$hours_per_week,results[,15:117])

glm.fit = glm(income_class ~ .,data=results, family=binomial)
summary (glm.fit)

# suggestion: clean up the native_country, marital_status, education;
#             combine education level and education months
levels(data$workclass)
data$workclass <- as.character(data$workclass)
data$workclass <- as.factor(data$workclass)
which(data=="?")

# Visulazattion

# for number predictors
par(mfrow = c(1,2))
boxplot(X.adult$age, main="boxplot age")
boxplot(X.adult$hours_per_week, main="boxplot hours_per_week")
boxplot(X.adult$capital_gain, main="boxplot capital_gain")                            # delete or clean up
boxplot(X.adult$capital_loss, main="boxplot capital_loss")                            # delete or clean up


# for factor predictors
par(mfrow = c(1,1))
barplot(prop.table(table(X.adult$workclass)), main = "barplot workclass")             # clean up 
barplot(prop.table(table(X.adult$marital_status)), main = "barplot marital_status")   # clean up
barplot(prop.table(table(X.adult$education)), main = "barplot education")             # clean up
barplot(prop.table(table(X.adult$occupation)), main = "barplot occupation")           # clean up
barplot(prop.table(table(X.adult$relationship)), main = "barplot relationship")       # clean up as suggesting before
barplot(prop.table(table(X.adult$race)), main = "barplot race")                       # clean up
barplot(prop.table(table(X.adult$sex)), main = "barplot sex")                         # need nothing to do
barplot(prop.table(table(X.adult$native_country)), main = "barplot native_country")   # clean up as suggesting before


