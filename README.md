# Big-Mart-Prediction
Predicting the Sales


############################ Big Mart Project ###################################


library(party)
library(ggplot2)
library(psych)



train <- read.csv("train.csv")
View(train)

# Summary of data
summary(train)

# structure of data
str(train)

# To find NA in data
sum(is.na(train))

# To find NA in Item_Identifier
sum(is.na(train$Item_Identifier))

# To find Na in Item_weight
sum(is.na(train$Item_Weight))

# To find NA in Item type
sum(is.na(train$Item_Type))

# To find NA in Item MRP
sum(is.na(train$Item_MRP))

summary(train)
sapply(train, function(x) sum(is.na(x)))

sapply(train, function(x) sum(is.na(x)))


##### Visualization ####

summary(train$Item_Weight)
hist(train$Item_Weight)


#Plot
colnames(train)

ggplot(train, aes(x = Item_MRP, y = Item_Outlet_Sales, colour = Item_Type)) + geom_point() 



ggplot(train,aes(x=Item_Type,y=Item_Outlet_Sales,fill=Item_Type)) + geom_bar(stat = "identity")



#plot Fat Level
ggplot(train,aes(x=Item_Fat_Content,y=Item_Outlet_Sales,fill=Item_Fat_Content)) + geom_bar(stat = "identity") +guides(fill=guide_legend(title = "Item_Fat_Content"))



# Plot for Item_Outlet_Sales and Outlet_Location_Type
ggplot(train,aes(x=Outlet_Location_Type, y = Item_Outlet_Sales,fill=Outlet_Location_Type)) + geom_bar(stat = "identity") + ylim(0,7700000) + guides(fill=guide_legend(title = "Location Type"))


#plot for Outlet Type and Sales
ggplot(train,aes(x=Outlet_Type,y=Item_Outlet_Sales,fill=Outlet_Type)) + geom_bar(stat = "identity") +guides(fill=guide_legend(title = "Outlet Type"))


#plot Visibility
ggplot(train,aes(x=Item_Visibility,y=Item_Outlet_Sales,color=train$Item_Type)) + geom_point() + guides(fill=guide_legend(title = "Product Name"))


## Checking the normality with  QQplot

#QQPlot for SALES
qqnorm(train$Item_Outlet_Sales)
qqline(train$Item_Outlet_Sales)

qqnorm(log(train$Item_Outlet_Sales))
qqline(log(train$Item_Outlet_Sales))

qqnorm(sqrt(train$Item_Outlet_Sales))
qqline(sqrt(train$Item_Outlet_Sales))


#QQPlot for MRP

qqnorm(train$Item_MRP)
qqline(train$Item_MRP)

qqnorm(sqrt(train$Item_MRP))
qqline(sqrt(train$Item_MRP))


qqnorm(log(train$Item_MRP))
qqline(log(train$Item_MRP))



## Checking the normality with histogram and density graph


##Item_Outlet_Sales
ggplot(train,aes(Item_Outlet_Sales))+geom_histogram(binwidth = 10)

ggplot(train,aes(Item_Outlet_Sales))+geom_density()
ggplot(train,aes(sqrt(Item_Outlet_Sales)))+geom_density()
ggplot(train,aes(log(Item_Outlet_Sales)))+geom_density()


##Item_MRP
ggplot(train,aes(Item_MRP))+geom_histogram(binwidth = 30)

ggplot(train,aes(Item_MRP))+geom_density()
ggplot(train,aes(sqrt(Item_MRP)))+geom_density()
ggplot(train,aes(log(Item_MRP)))+geom_density()


describeBy(train)

# by density and qqplot we will take a log on Item_Outlet_Sales and Item_MRP to make them normal distribution


# Correlation

cor(x=train$Item_MRP,y=train$Item_Outlet_Sales)
# High correlation compared to other variables



########################## Linear Regression ##################################


model <- lm(log(Item_Outlet_Sales) ~log(Item_MRP)+Outlet_Identifier,data = train)

model
summary(model)
par(mfrow = c(2,2))
plot(model)
text(model)

#Visualized
ggplot(train, aes((log(Item_MRP)),(log(Item_Outlet_Sales)),fill=Outlet_Identifier)) + geom_point() +geom_abline(model)


## read a test file
test <- read.csv("test.csv")



table(test$Outlet_Size,test$Outlet_Type,test$Outlet_Location_Type)


test$Item_Outlet_Sales1 <- predict(model,test,interval = "confidence",level = 0.99999992)

# We will take interval as confidence and levels as 0.99999992

write.csv(test,"salepredict23.csv")


#################################################################################
#################################################################################
