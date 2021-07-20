install.packages("nlme")
library(nlme)

MathAchieve
dim(MathAchieve)
head(MathAchieve,10)
summary(MathAchieve)

MathAchSchool
dim(MathAchSchool)
head(MathAchSchool, 10)
summary(MathAchSchool)

attach(MathAchSchool)
attach(MathAchieve)

is.factor(Sector)
Math <- as.data.frame(MathAchieve[, c("School", "SES", "MathAch")]) 
set.seed(3)
sample1 <- sort(sample(nrow(Math), 10))
Math[sample1, ]

sector <- MathAchSchool$Sector
names(sector) <- row.names(MathAchSchool) 
mses <- with(MathAchieve, tapply(SES, School, mean))
Math <- within(Math,{ Meanses <- as.vector(mses[as.character(School)]) 
                       Cses <- SES - Meanses 
                       Sector <- sector[as.character(School)] }) 
Math[sample1,]

cato <- with(Math, sample(unique(Math$School[Sector == "Catholic"]), 20))
cato2 <- Math[is.element(Math$School, cato), ]
public <- with(Math, sample(unique(Math$School[Sector == "Public"]), 20))
public2 <- Math[is.element(Math$School, public), ]

install.packages("lattice")
library(lattice)
trellis.device(color=TRUE) 
xyplot(MathAch ~ Cses | School, data=cato2, main="Catholic", type=c("p", "r", "smooth"), span=1)
xyplot(MathAch ~ Cses | School, data=public2, main="Public", type=c("p", "r", "smooth"), span=1)

# koriscenje funkcije lmList

cat.list <- lmList(MathAch ~ Cses | School,subset =Sector == "Catholic",data = Math )
pub.list <- lmList(MathAch ~ Cses | School,subset =Sector == "Public",data = Math )

plot(intervals(cat.list),main="Catholic")
plot(intervals(pub.list),main="Public")

cat.coef <- coef(cat.list)
head(cat.coef,6)
pub.coef <- coef(pub.list)
head(pub.coef,6)

par(mfrow=c(1,2))
boxplot(cat.coef[,1],pub.coef[,1],main="Intercepts",names = c("Catholic","Public"))
boxplot(cat.coef[,2],pub.coef[,2],main="Slopes",names = c("Catholic","Public"))


# Hijerarhijski linearni model sa funkcijom lme

Math$Sector <- factor(Math$Sector,levels = c("Public","Catholic"))
contrasts(Math$Sector)
Mathlme1 <- lme(MathAch ~ Meanses*Cses + Sector*Cses, random = ~ Cses | School, data=Math)
summary(Mathlme1)

Mathlme2 <- update(Mathlme1, random = ~ 1 | School)
anova(Mathlme1, Mathlme2)

Mathlme3<- update(Mathlme1, random = ~ Cses - 1 | School)
anova(Mathlme1, Mathlme3)

summary(Mathlme2)

library(effects)
plot(allEffects(Mathlme2,response="Cses",x.var="Cses"),rug=FALSE)

library(lme4)
Mathlmer1 <- lmer(MathAch ~ Meanses*Cses + Sector*Cses + (Cses | School),data=Math)
summary(Mathlmer1)

library(ggplot2) 
Math2<-Math[1:785,] 
Math2lmer<-lmer(MathAch ~ Cses + (1 + Cses | School), data=Math2) 
intercepts <- coef(Math2lmer)$School[,1] 
slopes <- coef(Math2lmer)$School[,2]
ggplot(Math2, aes(x=Cses, y=MathAch, color=as.factor(School))) + geom_point(shape=100) + geom_abline(slope=slopes, intercept=intercepts)

Mathlm<- lm(MathAch ~ Cses + Sector, Math2) 
Math2$Predictions <- fitted(Mathlm)
Math2$MLPredictions <- fitted(Math2lmer) 

ggplot(Math2, aes(x=Cses, y=MathAch, group=School)) + geom_line(aes(y = Math2$Predictions), color = "darkgrey") + geom_line(aes(y = Math2$MLPredictions), color = "red") + geom_point(alpha = 0.3, size = 3) + facet_wrap(~School) + theme_bw()

test<- Math[c(1:9, 48:56, 73:81,121:129,141:149,189:197,219:227,247:255,282:290,326:334),]
train<- Math[ -c(1:9, 48:56, 73:81,121:129,141:149,189:197,219:227,247:255,282:290,326:334,358:7185),]
Mlmer<-lmer(MathAch ~ Meanses*Cses + Sector*Cses + (Cses | School), data=train)

Mlm<- lm(MathAch ~ Cses + Sector, train) 
global_pred<- predict(Mlm, newdata=test) 
global_MSE <- mean((test$MathAch - global_pred)^2)
global_MSE

mlm_pred <-predict(Mlmer, newdata=test, allow.new.levels = TRUE)
mlm_MSE<- mean((test$MathAch - mlm_pred)^2)
mlm_MSE