# Bayesian project

# Bayesian Methods for Data Science (DATS 6450 - 11, Fall 2019)
# Data Science @ George Washington University
# Author: Hung Chun-Lin, Chen Chen, Liu siyang

# Reference:
# Some of the code is from the book by Professor John K. Kruschke
# Please find the reference to and website of the book below:
# Kruschke, J. K. (2014). Doing Bayesian Data Analysis: A Tutorial with R, JAGS, and Stan. 2nd Edition. Academic Press / Elsevier
# https://sites.google.com/site/doingbayesiandataanalysis/

source("CodeSource.R")

#load packages
loadPkg("sjmisc")
loadPkg("ggplot2")
loadPkg("plotly")
loadPkg("dplyr")
loadPkg("dummies")
loadPkg("kableExtra")
loadPkg("bestglm")
loadPkg("glmnet")
loadPkg("stats")
loadPkg("caTools")
loadPkg("ROSE")

#Load the data
insurance_data = loadData('travel_insurance.csv')

#Summarize dataset
summary(insurance_data)

#Rename the column
names(insurance_data)[names(insurance_data) == "Commision..in.value."] <- "Commision"

#Preprocessing
##Drop NA
Gender_empty_list = checkEmpty(insurance_data$Gender)
print(length(Gender_empty_list))
###To many empty value in Gender, drop it directly
drop_columns = c("Gender") 
insurance_data = insurance_data[, !(names(insurance_data) %in% drop_columns)]

##Drop outlier
###Age
####make boxplot
age_data = insurance_data["Age"]
age_plot = box_plot(age_data, 'red')
####drop outlier
insurance_data = insurance_data[!(insurance_data$Age < 0 & insurance_data$Age >100),]

###Duration
####make boxplot
duration_data = insurance_data["Duration"]
duration_plot = box_plot(duration_data, 'red')
####drop outlier
insurance_data = insurance_data[!(insurance_data$Duration <= 0 & insurance_data$Duration > 4000),]

###Commission
commision_data = insurance_data["Commision"]
####make boxplot
commission_plot = box_plot(commision_data, 'green')

###Net.Sales
sales_data = insurance_data["Net.Sales"]
####make boxplot
sales_plot = box_plot(sales_data, 'yellow')
####drop outliers
insurance_data<-insurance_data[!(insurance_data$Net.Sales <= 0),]

summary(insurance_data)

#Descriptive Statistics analysis
n_Agency = length(unique(insurance_data$Agency)) ##15
n_Agency
n_Agency_type = length(unique(insurance_data$Agency.Type)) ##2
n_Agency_type
n_Destination = length(unique(insurance_data$Destination)) ## 147
n_Destination
n_Distribution_type = length(unique(insurance_data$Distribution.Channel)) ##2
n_Distribution_type
n_Product_name = length(unique(insurance_data$Product.Name)) ##26
n_Product_name

Agency_freq = insurance_data %>% 
  count(Agency, Claim, sort = TRUE) %>% 
  top_n(15)
Agency_type_freq = insurance_data %>% 
  count(Agency.Type, Claim, sort = TRUE) %>% 
  top_n(10)
Destination_type_freq = insurance_data %>% 
  count(Destination, Claim, sort = TRUE) %>% 
  top_n(21) 
Product_name_freq = insurance_data %>% 
  count(Product.Name, Claim, sort = TRUE) %>% 
  top_n(22)
Distribution_type_freq = insurance_data %>% 
  count(Distribution.Channel, Claim, sort = TRUE) %>% 
  top_n(4)

ggplot(data = Destination_type_freq, aes(x = reorder(Destination, -n), y = n)) + 
  geom_bar(stat="identity", aes(fill = Claim))  +
  ggtitle("Claim or not among the Top 20 Destination") + 
  theme(axis.text.x = element_text(angle = 0, hjust = 1), 
        axis.title = element_blank()) +
  coord_flip()

ggplot(data = Agency_type_freq, aes(x = reorder(Agency.Type, -n), y = n)) + 
  geom_bar(stat="identity", aes(fill = Claim))  +
  ggtitle("Claim or not given the Agency type") + 
  theme(axis.text.x = element_text(angle = 0, hjust = 1),
        axis.title = element_blank())

ggplot(data = Distribution_type_freq, aes(x = reorder(Distribution.Channel, -n), y = n)) + 
  geom_bar(stat="identity", aes(fill = Claim))  +
  ggtitle("Claim or not given the Distribution type") + 
  theme(axis.text.x = element_text(angle = 0, hjust = 1),
        axis.title = element_blank())

ggplot(data = Product_name_freq, aes(x = reorder(Product.Name, -n), y = n)) + 
  geom_bar(stat="identity", aes(fill = Claim))  +
  ggtitle("Claim or not among the Product") + 
  theme(axis.text.x = element_text(angle = 0, hjust = 1),
        axis.title = element_blank())+
  coord_flip()

ggplot(data = Agency_freq, aes(x = reorder(Agency, -n), y = n)) + 
  geom_bar(stat="identity", aes(fill = Claim))  +
  ggtitle("Claim or not among the Agency") + 
  theme(axis.text.x = element_text(angle = 30, hjust = 1),
        axis.title = element_blank())

#Feature selection
##Too many types of Destination, almost 147 types, so We only keep 10 contries and 1 new dummy called 'others'.
##All the top 10 destinations are larger than 2000
df_Destination_freq = insurance_data %>% 
  group_by(Destination) %>%
  summarise(no_rows = length(Destination))

df_Destination_freq = df_Destination_freq  %>% 
  arrange(desc(no_rows)) ## descending order
top_10_Destination = df_Destination_freq[1:10,1] ##select the top 10
top_10_Destination = as.character(unlist(top_10_Destination))
length(top_10_Destination)
all_Destination = as.character(unique(unlist(insurance_data["Destination"])))
length(all_Destination)
none_top_Destination = all_Destination[!all_Destination %in% top_10_Destination] ##filter the top destination
length(none_top_Destination)

insurance_data$Destination = as.character(insurance_data$Destination)
insurance_data$Destination = ifelse(insurance_data$Destination %in% none_top_Destination, "Others", insurance_data$Destination)
insurance_data$Destination = as.factor(insurance_data$Destination)

#Product name
#There are 26 types in product name, some have fewer records, so we only keep top 10 product, and add 1 new dummy call "Others"
df_Product_freq = insurance_data %>% 
  group_by(Product.Name) %>%
  summarise(no_rows = length(Product.Name))

df_Product_freq = df_Product_freq  %>% 
  arrange(desc(no_rows)) ## descending order

top_10_Product = df_Product_freq[1:10,1] ##select the top 10
top_10_Product = as.character(unlist(top_10_Product))
length(top_10_Product)
all_Prouct = as.character(unique(unlist(insurance_data["Product.Name"])))
length(all_Prouct)
none_top_Prouct = all_Prouct[!all_Prouct %in% top_10_Product] ##filter the top destination
length(none_top_Prouct)

insurance_data$Product.Name = as.character(insurance_data$Product.Name)
insurance_data$Product.Name = ifelse(insurance_data$Product.Name %in% none_top_Prouct, "Others", insurance_data$Product.Name)
insurance_data$Product.Name = as.factor(insurance_data$Product.Name)

##Drop product.Name column due to multicorrelation with Agency
drop_column_agency = c("Product.Name") 
insurance_data = insurance_data[, !(names(insurance_data) %in% drop_column_agency)]

##Label encoder the target
insurance_data$Claim = as.factor(insurance_data$Claim) 
levels(insurance_data$Claim) <- c(0,1)

##Lasso feature selection
###Set x and y in lasso regression
x=model.matrix(Claim~.,insurance_data)[,-1]
y=insurance_data$Claim

###Make the lasso coefficient plot
set.seed(100)
lasso_mod=glmnet(x,y,alpha=1,family="binomial")
lasso_plot1 = plot(lasso_mod, xvar = "lambda", pch=19)
lasso_plot1


###Check the cross validation 
cv_lasso=cv.glmnet(x,y,alpha=1,family="binomial")
###Check the best lambda and 1 standard error lambda
bestlam=cv_lasso$lambda.1se
bestlam1=cv_lasso$lambda.min
lasso_plot2 = plot(cv_lasso)
lasso_plot2

###Check the coefficient of the variables at 1 standard error point
lasso_coef = coef(cv_lasso, s = "lambda.min")
###Select the top variables
order_data = abs(as.data.frame(as.matrix(lasso_coef)))
order_data$column = row.names(order_data)
row.names(order_data) <- NULL
order_data <- order_data[-1,]
selected_variable = order_data[order(-order_data[1]),][1:15,]
###List the column name of the selected variables
variable_name = selected_variable["column"][1:15,1]

#MCMC
#For the nominal variable, we do one-hot encoding
dummy_data = dummy.data.frame(insurance_data, name=c("Destination","Agency","Distribution.Channel"), sep="")
dummy_data = as.data.frame(apply(dummy_data, 2, as.numeric))
bys_data = dummy_data[variable_name]
bys_data$Claim = as.numeric(insurance_data$Claim)-1
str(bys_data)

#use caTools function to split, SplitRatio for 90%:10% splitting
dt= sample.split(bys_data,SplitRatio = 0.9)
#subsetting into Train data
train =subset(bys_data,dt==TRUE)
#subsetting into Test data
test =subset(bys_data,dt==FALSE)

#Plot the correlation matrix between variables
cor_matrix = round(cor(x=train, method = c("pearson")),2)
kable(cor_matrix)
heatmap(cor_matrix)

#Get the column names for all features
xName = colnames(subset(train, select = -c(Claim)))
yName = "Claim"
#Convert factor to numeric before shipping to JAGS
train = train[,c(xName, yName)]


#Generate the robust MCMC Logistic
mcmcCoda = genMCMC( data = train, xName = xName , yName = yName, 
                    numSavedSteps=15000 , thinSteps=1 , saveName=NULL ,
                    runjagsMethod=runjagsMethodDefault , 
                    nChains=nChainsDefault)

#Display diagnostics of chain, for specified parameters:
parameterNames = varnames(mcmcCoda) # get all parameter names
for ( parName in parameterNames ) {
  diagMCMC( codaObject=mcmcCoda , parName=parName , 
            saveName="auto" , saveType="png" )
}

#Get summary statistics of chain:
summaryInfo = smryMCMC( mcmcCoda , 
                        saveName="Summary_Info" )
show(summaryInfo)

#Display posterior information:
plotMCMC( mcmcCoda , data=train, xName=xName , yName=yName , 
          pairsPlot=TRUE , showCurve=FALSE ,
          saveName="weight"  , saveType="png" )
#test the model accuracy
train <- ovun.sample(Claim~., data=train,
                            seed=1, method="over")$data
summary(glm0<- glm(Claim ~ ., family="binomial", data=train))
pred0 <- predict(glm0, newdata = test, type="response")
glm.pred0 <- ifelse(pred0 > 0.5, "Up", "Down")

confus.matrix <- table(real=model1_test$Claim, predict=glm.pred0)
accuracy <- sum(diag(confus.matrix))/sum(confus.matrix)
accuracy


model1_train = train[c("Claim","AgencyC2B","Distribution.ChannelOnline",
                     "AgencyLWC","AgencyTST","AgencyKML","AgencyCWT",
                     "DestinationSINGAPORE","DestinationMALAYSIA")]
model1_train <- ovun.sample(Claim~., data=model1_train,
                                seed=1, method="over")$data
model1_test = test[c("Claim","AgencyC2B","Distribution.ChannelOnline",
                     "AgencyLWC","AgencyTST","AgencyKML","AgencyCWT",
                     "DestinationSINGAPORE","DestinationMALAYSIA")]
summary(glm1<- glm(Claim ~ ., family="binomial", data=model1_train))
pred1 <- predict(glm1, newdata = model1_test, type="response")
pred1 <- pred1*(1- 0.01) + 0.01/2
glm.pred1 <- ifelse(pred1 > 0.5, "Up", "Down")

confus.matrix <- table(real=model1_test$Claim, predict=glm.pred1)
accuracy <- sum(diag(confus.matrix))/sum(confus.matrix)
accuracy

model2_train = train[c("Claim","Distribution.ChannelOnline","AgencyTST","AgencyKML",
                         "DestinationSINGAPORE","DestinationMALAYSIA")]
model2_train <- ovun.sample(Claim~., data=model2_train,
                            seed=1, method="over")$data
model2_test = test[c("Claim","Distribution.ChannelOnline","AgencyTST","AgencyKML",
                       "DestinationSINGAPORE","DestinationMALAYSIA")]
glm2<- glm(Claim ~ ., family="binomial", data=model2_train)
pred2 <- predict(glm2, newdata = model2_test)
glm.pred2 <- ifelse(pred2 > 0.5, "Up", "Down")

confus.matrix <- table(real=model2_test$Claim, predict=glm.pred2)
accuracy <- sum(diag(confus.matrix))/sum(confus.matrix)
accuracy
