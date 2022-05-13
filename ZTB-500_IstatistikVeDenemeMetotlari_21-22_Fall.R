# Veri setinin yukleme
install.packages("readxl")
library("readxl")

setwd("H:/dersler/Istatistik_ve_Deneme_Metodlari/R_uygulamalari/DataSets")

setwd("C:/Users/sebah/Downloads")

#setwd("~/")
#getwd()

dataSet <- read_xlsx("AirQualityUCI.xlsx")
dataSet2 <- read_excel("Raisin_Dataset/Raisin_Dataset.xlsx")


###########Descriptive Statistics###########
#mean
mean(dataSet$`NO2(GT)`, na.rm =TRUE)
#median
median(dataSet$`NO2(GT)`, na.rm =TRUE)
#std.dev
sd(dataSet$`NO2(GT)`, na.rm = TRUE)

for (variable in colnames(dataSet)) {
  print(paste(variable , sd(dataSet[[variable]], na.rm = TRUE), sep = " "))
}

#quantiles/percentiles
quantile(dataSet$`NO2(GT)`, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)

#summary of all dataset of a column
summary(dataSet)
summary(dataSet$`NO2(GT)`)
summary(dataSet2)


########### Plotting the Data ############
#scatter plot
plot(dataSet$Time, dataSet$`NO2(GT)`)
plot(dataSet$Date, dataSet$`NO2(GT)`, xlab = "Date", 
     ylab = "Carbon Monoxide", main = "CO level in the air vs. Date")

plot(dataSet2[0:7])

#boxplot
boxplot(dataSet$`NO2(GT)`)
boxplot(dataSet$`NOx(GT)`)
boxplot(dataSet$`NO2(GT)`, dataSet$`NOx(GT)`)
boxplot(dataSet$`NO2(GT)`, dataSet$`NOx(GT)`, names = c("ilkveri","ikinciveri"))

boxplot(dataSet2$Perimeter ~ dataSet2$Class, ylab = "YariCap", xlab="Uzum", 
        ylim=c(500,3000), las = 0)

boxplot(dataSet2$Perimeter ~ dataSet2$Class, ylab = "YariCap", xlab="Uzum", 
        ylim=c(500,3000), las = 1)
        

#barplot and pie chart
barplot(dataSet2$Area)
barplot(table(dataSet2$Class))
barplot(table(dataSet2$Class)/900*100, ylab = "Yüzde frekans(%)")
barplot(table(dataSet2$Class)/900*100, xlab = "Yüzde frekans(%)", horiz = TRUE)

pie(table(dataSet2$Class))


# line and various plots
plot(dataSet$Time, dataSet$`PT08.S5(O3)`, type="l", lwd=2, col="red" )
plot(dataSet$Time[0:10], dataSet$`PT08.S5(O3)`[0:10], type="l", lwd=2, col="red" )
plot(dataSet$Time[0:10], dataSet$`PT08.S5(O3)`[0:10], type="b", lwd=2, col="red" )
plot(dataSet$Time[0:10], dataSet$`PT08.S5(O3)`[0:10], type="s", lwd=2, col="red" )
plot(dataSet$Time[0:10], dataSet$`PT08.S5(O3)`[0:10], type="h", lwd=2, col="red" )

hist(dataSet2$Extent)
hist(dataSet2$Extent, freq = FALSE)
hist(dataSet2$Extent, probability = F)
hist(dataSet2$Extent, probability = F, breaks = 5)
hist(dataSet2$Extent, probability = F, breaks = 15)
hist(dataSet2$Extent, probability = F, breaks = c(0,.2,.4,.6,.8,1))
hist(dataSet2$Extent, probability = T, breaks = seq(from=0, to=1, by=0.05), ylim = c(0,10))

lines(density(dataSet2$Extent), type="o", col="pink")
lines(density(dataSet2$Extent), col="blue", lwd=3)




####### Correlation############
cor(dataSet)
cor(dataSet2[,0:7])
cor(dataSet2[,0:7], method = "kendall")
cov(dataSet2[,0:7], method = "kendall")
var(dataSet2$Area)
var(dataSet2[,0:5])

cor.test(dataSet2$Area, dataSet2$Perimeter)
cor.test(dataSet2$Area, dataSet2$Perimeter, method = "spearman")
cor.test(dataSet2$Area, dataSet2$Perimeter, conf.level = 0.9)
cor.test(dataSet2$Area, dataSet2$Perimeter, conf.level = .95, alternative = "less")
cor.test(dataSet2$Area, dataSet2$Perimeter, conf.level = .95, alternative = "greater")
cor.test(dataSet2$Area, dataSet2$Perimeter, conf.level = .95, alternative = "two.sided")

install.packages("corrgram")
library("corrgram")
corrgram(dataSet2[,0:7])
corrgram(dataSet2[,0:7], order=TRUE, lower.panel=panel.shade, upper.panel=panel.pie, text.panel=panel.txt, main="correlogram1")
corrgram(dataSet2[,0:7], order=TRUE, lower.panel=panel.ellipse, upper.panel=panel.pts, text.panel=panel.txt, diag.panel=panel.minmax, main="correlogram2") 
corrgram(dataSet2[,0:7], order=NULL, lower.panel=panel.shade, upper.panel=NULL, text.panel=panel.txt, main="correlogram3")


######### t test ################
# independent 2-group t-test
t.test(dataSet2$Eccentricity ~ dataSet2$Class) # where y is numeric and x is a binary factor
t.test(dataSet2$Eccentricity ~ dataSet2$Class, conf.level = 0.90)
t.test(dataSet2$Area ~ dataSet2$Class, conf.level = 0.90)
t.test(dataSet2$Area ~ dataSet2$Class, conf.level = 0.90, alternative="less")

# independent 2-group t-test
t.test(dataSet$`NOx(GT)` , dataSet$`CO(GT)`) # where y1 and y2 are numeric

# paired t-test
t.test(dataSet$`NOx(GT)` , dataSet$`NO2(GT)` ,paired=TRUE) # where y1 & y2 are numeric
x=c(1,2,3,4,5,6)
y=c(10,20,30,40,50,60)
t.test(x,y ,paired=TRUE) # where y1 & y2 are numeric


# one sample t-test
t.test(dataSet2$Extent, mu=0.9)
t.test(dataSet2$Extent, mu=0.70)
t.test(dataSet2$Extent, mu=0.6995079)






########### Multiple Lineer Regerssion#########
# Multiple Linear Regression Example
fit <- lm(ConvexArea ~ Area + Perimeter + Extent, data=dataSet2)
summary(fit) # show results

fit1 <- lm(dataSet2$ConvexArea ~ dataSet2$Area + dataSet2$Perimeter + dataSet2$Extent)
summary(fit) # show results

fit2 <- lm(ConvexArea ~ ., data=dataSet2[0:7])
summary(fit2) # show results

fit3 <- lm(ConvexArea ~ Area + Perimeter + Extent + Class, data=dataSet2)
summary(fit3) # show results
fit3$coefficients #coefficients
fit3 <- lm(ConvexArea ~ Class, data=dataSet2)
contrasts(factor(dataSet2$Class))

# Other useful functions
coefficients(fit2) # model coefficients
confint(fit2, level=0.95) # CIs for model parameters
fitted(fit2) # predicted values
residuals(fit2) # residuals
anova(fit2) # anova table

# diagnostic plots
plot(fit)
plot(fit,5)
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page
plot(fit)
layout(1,1)

# compare models
anova(fit, fit2)

# Stepwise Regression
install.packages("MASS")
library("MASS")
step <- stepAIC(fit2, direction="both")
step$anova # display results

#plot regression line
plot(dataSet2$ConvexArea , dataSet2$Area, main="regression plot")
abline(fit2, col=2, lwd=3)
abline(step, col="blue", lwd=3)





###### ANOVA/MANOVA ############
# One Way Anova (Completely Randomized Design)
fit <- aov(ConvexArea ~ Area + Perimeter , data=dataSet2)
summary(fit)

# Two Way Factorial Design
fit1 <- aov(ConvexArea ~ Area + Perimeter + Area:Perimeter, data=dataSet2)
summary(fit1)
fit1 <- aov(ConvexArea ~ Area * Perimeter, data=dataSet2) # same thing
summary(fit1)
plot(fit1)

drop1(fit1,~.,test="F") # type III SS and F Tests

# 2x2 Factorial MANOVA with 3 Dependent Variables.
Y <- cbind(dataSet2$Area , dataSet2$Perimeter , dataSet2$ConvexArea )
fit <- manova(Y ~ dataSet2$MajorAxisLength*dataSet2$MinorAxisLength)
summary(fit)
