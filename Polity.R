#ADVANCED STATISTICS 2019 MIDTERM

#Download your assigned dataset and read the accompanying codebook. 

#Which dataset are you working with?
#Polity IV: Regime Authority Characteristics and Transitions Dataset, Polity IV Annual Time-Series, 1800-2017

#Install and/or load the necessary packages for the commands you use (you can come back and add these as you need them):
require(psych)
require(ggplot2)
install.packages("pscl")
require(pscl)

#Load your data into R:
df<-read.csv(file.choose())

#Summarize your data in some way and check that it loaded properly and is what you and the codebook expect:
#Polity IV Project, Political Regime Characteristics and Transitions, 1800-2017, annual, cross-national, time-series and polity-case formats coding democratic and autocratic "patterns of authority" and regime changes in all independent countries with total population greater than 500,000 in 2017. These polity variables are divided into the following: Country and Case Identifier Code, Indicators of Democracy and Autocracy (Composite Indicators), Authority Characteristics (Component and Concept Variables), and Polity Regime Transitions.
View(df)
summary(df) 
df[,2:length(df)]


#Choose two variables from the dataset that you think may have an interesting and potentially causal relationship. Which variables did you choose?
#PARCOMP, XCONST

#In at least a paragraph, explain why you chose these variables, what relationship you think they could have, and why you think they have that relationship.
#The competitiveness of participation (parcomp) refers to the extent to which alternative preferences for policy and leadership can be pursued in the political arena. While the variable executive constraint (xconst) refers to the extent of institutionalized constraints on the decision- making powers of chief executives, whether individuals or collectivities. For example, in Western democracies these are usually legislatures. I chose these variables because initially I hypothesized that the executive constraint (xconst) is the most important component of the democracy, followed by the competitiveness of executive recruitment and the competitiveness of participation thereby making the relationship linear. 

#Describe and visualize each variable separately:
hist(df$parcomp)
hist(df$parcomp,main="Histogram for Competitiveness of Participation (parcomp)", xlab="PARCOMP", border="blue",col="green") 
summary(df$parcomp)
plot(df$parcomp, col = "blue", main="Plot for Competitiveness of Participation (parcomp) ")
boxplot(df$parcomp, main="parcomp", sub=paste("Outlier rows: ", boxplot.stats(df$parcomp)$out))  

hist(df$xconst)
hist(df$xconst,main="Histogram for Executive Constraint (xconst)", xlab="XCONST", border="blue",col="red")
summary(df$xconst)
plot(df$xconst, col = "blue", main="Histogram for Executive Constraint (xconst)”)
boxplot(df$xconst, main="xconst", sub=paste("Outlier rows: ", boxplot.stats(df$xconst)$out))  
     
     
#What did you learn from these descriptive statistics?
#Describe and visualize the two variables together:
#Correlate them:
     
cor(df$parcomp, df$xconst)
linearMod <- lm(parcomp ~ xconst, data=df)
print(linearMod)
summary(linearMod)
     
#Plot them:
scatter.smooth(x=df$parcomp, y=df$xconst, main="paracomp ~ xconst", col="red")
     
dfs <- table(df$parcomp, df$xconst)
print(dfs)
barplot(dfs, main="parcomp and xconst", xlab="Scores", col=c("darkblue","darkgreen"), legend = rownames(dfs))
     
#What did you learn from this? How are the variables related (or not)?
#Being as a correlation of 1.0 indicates a perfect positive correlation, a correlation of 0.995 indicates a near to perfect positive correlation between variables parcomp and xconst. This signifies that both variables move in the same direction and/or are correlated. There is a strong linear relationship between these two variables. 
     
#Regress one of the variables on the other. 
#1) Which is your independent variable (or x)? xconst
#2) Which is your dependent variable (or y)? parcomp
#3) In at least a paragraph, explain which estimator you used and why. Why is that estimator appropriate? What are some potential problems with that estimator given your variables?
#The term general linear model (GLM) usually refers to conventional linear regression models for a continuous response variable given continuous and/or categorical predictors. It includes multiple linear regression, as well as ANOVA I choose GLM as an estimator because the models are fitted via Maximum Likelihood estimation; therefore optimal properties of the estimators also there is great flexibility in the capacity to model.

logit1 <- glm(df$parcomp~ df$xconst, data = df)
summary(logit1)
exp(coef(logit1))
     
     
#4) In at least a paragraph, explain your bivariate regression results. What do your results say? What do your results mean?
#Bivariate Regression Analysis involves analysing two variables to establish the strength of the relationship between them. The two variables denoted as x,y are parcomp and xconst respectively with one being an independent variable (or explanatory variable), while the other is a dependent variable (or outcome variable).
In order to determine the relationship, Bivariate Regression Analysis uses a linear regression line to help measure how the two variables change together, simultaneously. The result took the form of a line of best fit embedded on the scatter chart through the plotted values of the independent variable, against the dependent variable. In this case, the y-intercept yielded -1.0809 and the coefficient of xconst yielded 0.9759 indicating that this is a positive relationship.
     
     
#Choose a third variable and run a multivariate regression.
#1) Which variable did you choose and why? 
#Institutionalized Democracy (democ) is conceived as three essential, interdependent elements. Moreover, the second element of democ focuses on the existence of institutionalized constraints on the exercise of power by the executive while the third is the guarantee of civil liberties to all citizens in their daily lives and in acts of political participation. I believe these two elements touched of the democ variable touched on the variables xconst and parcomp which fortified my interest in conducting a multivariate regression. In addition to democ, I also looked at the variable autoc as Autocracies sharply restrict or suppress competitive political participation therefore this may sharply affect parcomp resulting in higher competitiveness of participation in autocratic entities and thus providing a relationship. 
     
logit2 <- glm(parcomp~ xconst + democ, data=df)
summary(logit2) 
plot(logit2)
     
logit3 <- glm(parcomp~ xconst + autoc, data=df)
summary(logit3)
plot(logit3)
     
anova(logit2, logit3)
     
cor(df$democ, df$autoc)
     
#2)In at least a paragraph, explain your multivariate regression results. What do your results say? What do your results mean? What changed when you added a third variable? What is that third variable doing? Why do you think that change (or no change) happened? T is simply the calculated difference represented in units of standard error. The greater the magnitude of T, the greater the evidence against the null hypothesis. This means there is greater evidence that there is a significant difference. The closer T is to 0, the more likely there isn't a significant difference.; however, the magnitude of democ is 20 and magnitude of democ is 109, therefore democ has lesser significance than autoc. The coefficient of the variable xconst increased when the third variable democ was added from 2.6 to 3.04 but decreased to 2.2 for autoc. The third variable was influencing the prior two variables. 
     
     
#Finally, in at least a paragraph, discuss how you would change either model if you had more data and time, and why you would make those changes.
#Given additional time for this project, I would modify the model by utilizing lm is used to fit linear models and can also be used to carry out regression. Initial analysis of the data indicated a strong linear relationship, it may have been more efficient to utilize lm.
     
     