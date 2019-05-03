#FINAL PAPER CODE 


#install the Zelig package for ordered logit
install.packages("Zelig")
require(Zelig)
#install the Zelig Choice package for ordered logit
install.packages("ZeligChoice")
require(ZeligChoice)
#install the plm package for panel analysis
install.packages("plm")
require(plm)
require(psych)
require(ggplot2)
install.packages("pscl")
require(pscl)


#Read 

df<-read.csv(file.choose())

#Summarize

View(df)
summary(df)

#Variables: v2mecenefm, v2xel_frefair, v2x_jucon

#Visualize 

hist(df$v2mecenefm)
summary(df$v2mecenefm)
describe(df$v2mecenefm)
plot(df$v2mecenefm, col = "red", main="Plot for v2mecenefm")

hist(df$v2xel_frefair)
summary(df$v2xel_frefair)
describe(df$v2xel_frefair)
plot(df$v2xel_frefair, col = "red", main="Plot for v2xel_frefair")

hist(df$v2x_jucon)
summary(df$v2x_jucon)
describe(df$v2x_jucon)
plot(df$v2x_jucon, col = "red", main="Plot for v2x_jucon")

#Correlation 

cor(df$v2mecenefm, df$v2xel_frefair, use="complete.obs")

scatter.smooth(x=df$v2mecenefm, y=df$v2xel_frefair, main="v2mecenefm ~ v2xel_frefair", col="red")
fit <- lm(df$v2mecenefm ~ df$v2xel_frefair, data=df)
print(fit)

scatter.smooth(x=df$v2mecenefm, y=df$v2x_jucon, main="v2mecenefm ~ v2x_jucon", col="red")
fit <- lm(df$v2xel_frefair ~ df$v2x_jucon, data=df)
print(fit)

scatter.smooth(x=df$v2x_jucon, y= main="v2xel_frefair ~ v2x_jucon", col="red")
fit <- lm(df$v2xel_frefair ~ df$v2x_jucon, data=df)
print(fit)

corr<-head(df)
print(corr)


dfs <- table(df$v2mecenefm, df$v2xel_frefair)
view(dfs)

#Control
f<- lm(df$v2x_jucon~df$v2mecenefm*df$v2xel_frefair+df$e_migdppc)
print(f)
g<- lm(df$v2x_jucon~df$v2mecenefm*df$v2xel_frefair+df$e_miurbani)
print(g)
h<- lm(df$v2x_jucon~df$v2mecenefm*df$v2xel_frefair+df$e_peginiwi)
print(h)

#OLS and Panel Analysis

pd <- pdata.frame(df, index=c("country_id", "year"))
pwtest(v2mecenefm ~ v2xel_frefair + v2x_jucon, data=pd)

#Fixed Effects Model

fe <- plm(v2mecenefm ~ v2xel_frefair + v2x_jucon, data=pd, model = "within", effect = "twoways")
summary(fe)

