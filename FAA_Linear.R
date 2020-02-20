
library(MASS)
library(readxl)
library(dplyr)
library(schoolmath)
#step1

FAA1_1 <- read_excel("FAA1-1.xls")
FAA2_1 <- read_excel("FAA2-1.xls")
dim(FAA1_1)

#Step 2
str(FAA1_1)
str(FAA2_1)

#Step 3
FAA <- merge(FAA1_1,FAA2_1,all.x=TRUE)
dim(FAA)
#Removing Duplicate rows
FAA_uniq <- FAA %>% distinct(distance,.keep_all = TRUE)
dim(FAA_uniq)
#Hence No duplicates

#NA values
colSums(is.na(FAA_uniq))

#Step 4
str(FAA_uniq)
summary(FAA_uniq)

#Step 5
sum(FAA_uniq$aircraft=="airbus")
sum(FAA_uniq$aircraft=="boeing")


##Data Cleaning and further exploartion

#Step 6
FAA_uniq <- FAA_uniq[!(FAA_uniq$duration <= 40), ]
FAA_uniq <- FAA_uniq[!(FAA_uniq$speed_ground < 30 | FAA_uniq$speed_ground > 140), ]
FAA_uniq <- FAA_uniq[!(FAA_uniq$height < 6), ]
FAA_uniq <- FAA_uniq[!(FAA_uniq$distance > 6000), ]

#Step 7
str(FAA_uniq)
summary(FAA_uniq)
sum(FAA_uniq$aircraft=="airbus")
sum(FAA_uniq$aircraft=="boeing")

#Step 8
par(mfrow = c(3, 3))
hist(FAA_uniq$no_pasg)
hist(FAA_uniq$speed_ground)
hist(FAA_uniq$speed_air)
hist(FAA_uniq$height)
hist(FAA_uniq$pitch)
hist(FAA_uniq$distance)
hist(FAA_uniq$duration)

#Step 9
#Data reduced to 781 observations after removing abnormal values
#Speed_air still has 586 rows having no value
#Airbus and Boeing records are 394 and 387  respectively after cleaning

#Step 10
FAA_uniq$aircraft <- ifelse(FAA_uniq$aircraft == "airbus", 0, 1)
#FAA_uniq$aircraft <- as.factor(FAA_uniq$aircraft)
#faa_num <- FAA_uniq[, sapply(FAA_uniq, is.numeric)]
cor_faa <- cor(FAA_uniq)
round(cor_faa, 2)
tab <- cor_faa[,7,drop=FALSE]
#dat <- data.frame(tab)
Table1 <- data.frame("Name of the Variable" = c("aircraft","no_pasg","speed_ground","speed_air","height","pitch","Distance","duration"),
                     "Corr_Coefficient"=c(tab[1:8]),
                     "Direction of corr_coefficient" = c(ifelse(is.positive(tab[1])=="TRUE","Positive","Negative"),
                                                    ifelse(is.positive(tab[2])=="TRUE","Positive","Negative"),
                                                    ifelse(is.positive(tab[3])=="TRUE","Positive","Negative"),
                                                    ifelse(tab[4]=="","",""),
                                                    ifelse(is.positive(tab[5])=="TRUE","Positive","Negative"),
                                                    ifelse(is.positive(tab[6])=="TRUE","Positive","Negative"),
                                                    ifelse(is.positive(tab[7])=="TRUE","Positive","Negative"),
                                                    ifelse(is.positive(tab[8])=="TRUE","Positive","Negative")))
Table1 <- Table1[order(-abs(Table1$Corr_Coefficient)),]

#Step 11
par(mfrow = c(2, 3))
plot(FAA_uniq$distance ~ FAA_uniq$no_pasg)
plot(FAA_uniq$distance ~ FAA_uniq$speed_ground)
plot(FAA_uniq$distance ~ FAA_uniq$speed_air)
plot(FAA_uniq$distance ~ FAA_uniq$height)
plot(FAA_uniq$distance ~ FAA_uniq$pitch)
plot(FAA_uniq$distance ~ FAA_uniq$duration)

#Step 12
FAA_uniq$aircraft <- ifelse(FAA_uniq$aircraft == "airbus", 0, 1)
plot(FAA_uniq$distance ~ FAA_uniq$aircraft)

#Step 13
model1 <- lm(FAA_uniq$distance ~ FAA_uniq$aircraft)
model2 <- lm(FAA_uniq$distance ~ FAA_uniq$no_pasg)
model3 <- lm(FAA_uniq$distance ~ FAA_uniq$speed_ground)
model4 <- lm(FAA_uniq$distance ~ FAA_uniq$speed_air)
model5 <- lm(FAA_uniq$distance ~ FAA_uniq$height)
model6 <- lm(FAA_uniq$distance ~ FAA_uniq$pitch)
model7 <- lm(FAA_uniq$distance ~ FAA_uniq$duration)

Table2 <- data.frame("Name of the Variable" = c("aircraft","no_pasg","speed_ground","speed_air","height","pitch","duration"), "p-value" = 
                       c(summary(model1)$coefficients[2,4],summary(model2)$coefficients[2,4],summary(model3)$coefficients[2,4],
                         summary(model4)$coefficients[2,4],summary(model5)$coefficients[2,4],summary(model6)$coefficients[2,4],summary(model7)$coefficients[2,4]), 
                     "Direction of coefficient" = c(ifelse(is.positive(summary(model1)$coefficients[2,1])=="TRUE","Positive","Negative"),
                                                    ifelse(is.positive(summary(model2)$coefficients[2,1])=="TRUE","Positive","Negative"),
                                                    ifelse(is.positive(summary(model3)$coefficients[2,1])=="TRUE","Positive","Negative"),
                                                    ifelse(is.positive(summary(model4)$coefficients[2,1])=="TRUE","Positive","Negative"),
                                                    ifelse(is.positive(summary(model5)$coefficients[2,1])=="TRUE","Positive","Negative"),
                                                    ifelse(is.positive(summary(model6)$coefficients[2,1])=="TRUE","Positive","Negative"),
                                                    ifelse(is.positive(summary(model7)$coefficients[2,1])=="TRUE","Positive","Negative")))
                                                    
#Step 14
FAA_stan <- FAA_uniq
FAA_stan$aircraftsd <- (FAA_uniq$aircraft-mean(FAA_uniq$aircraft))/sd(FAA_uniq$aircraft)
FAA_stan$no_pasgsd <- (FAA_uniq$no_pasg-mean(FAA_uniq$no_pasg))/sd(FAA_uniq$no_pasg)
FAA_stan$speed_groundsd <- (FAA_uniq$speed_ground-mean(FAA_uniq$speed_ground))/sd(FAA_uniq$speed_ground)
FAA_stan$heightsd <- (FAA_uniq$height-mean(FAA_uniq$height))/sd(FAA_uniq$height)
FAA_stan$pitchsd <- (FAA_uniq$pitch-mean(FAA_uniq$pitch))/sd(FAA_uniq$pitch)
FAA_stan$durationsd <- (FAA_uniq$duration-mean(FAA_uniq$duration))/sd(FAA_uniq$duration)

model11 <- lm(FAA_stan$distance ~ FAA_stan$aircraftsd)
model21 <- lm(FAA_stan$distance ~ FAA_stan$no_pasgsd)
model31 <- lm(FAA_stan$distance ~ FAA_stan$speed_groundsd)
model41 <- lm(FAA_stan$distance ~ FAA_stan$speed_air)
model51 <- lm(FAA_stan$distance ~ FAA_stan$heightsd)
model61 <- lm(FAA_stan$distance ~ FAA_stan$pitchsd)
model71 <- lm(FAA_stan$distance ~ FAA_stan$durationsd)

Table3 <- data.frame("Name of the Variable" = c("aircraft","no_pasg","speed_ground","speed_air","height","pitch","duration"), "Standardized_coeff_value" = 
                       c(abs(summary(model11)$coefficients[2,1]),abs(summary(model21)$coefficients[2,1]),abs(summary(model31)$coefficients[2,1]),
                         abs(summary(model41)$coefficients[2,1]),abs(summary(model51)$coefficients[2,1]),abs(summary(model61)$coefficients[2,1]),abs(summary(model71)$coefficients[2,1])), 
                     "Standardized_coeff_direction" = c(ifelse(is.positive(summary(model11)$coefficients[2,1])=="TRUE","Positive","Negative"),
                                                    ifelse(is.positive(summary(model21)$coefficients[2,1])=="TRUE","Positive","Negative"),
                                                    ifelse(is.positive(summary(model31)$coefficients[2,1])=="TRUE","Positive","Negative"),
                                                    ifelse(is.positive(summary(model41)$coefficients[2,1])=="TRUE","Positive","Negative"),
                                                    ifelse(is.positive(summary(model51)$coefficients[2,1])=="TRUE","Positive","Negative"),
                                                    ifelse(is.positive(summary(model61)$coefficients[2,1])=="TRUE","Positive","Negative"),
                                                    ifelse(is.positive(summary(model71)$coefficients[2,1])=="TRUE","Positive","Negative")))


Table3 <- Table3[order(-abs(Table3$Standardized_coeff_value)),]

#Step 15
Table0 <- Reduce(function(x,y) merge(x = x, y = y), 
       list(Table1, Table2, Table3))

Table0 <- Table0[order(-abs(Table0$Corr_Coefficient),Table0$p.value,-Table0$Standardized_coeff_value),]

#Step 16
model.sg <- lm(FAA_uniq$distance ~ FAA_uniq$speed_ground)
model.sa <- lm(FAA_uniq$distance ~ FAA_uniq$speed_air)
model.comb <- lm(FAA_uniq$distance ~ FAA_uniq$speed_ground+FAA_uniq$speed_air)
summary(model.comb)
summary(model.sg)
summary(model.sa)

FAA_ns <- na.omit(FAA_uniq)
cor(FAA_ns)
#98.8% correlation in speed_ground and speed_air
#Take speed_ground as more number of observations

#Step 17
model.x1 <- lm(FAA_uniq$distance ~ FAA_uniq$speed_ground)
model.x2 <- lm(FAA_uniq$distance ~ FAA_uniq$speed_ground+FAA_uniq$aircraft)
model.x3 <- lm(FAA_uniq$distance ~ FAA_uniq$speed_ground+FAA_uniq$aircraft+FAA_uniq$height)
model.x4 <- lm(FAA_uniq$distance ~ FAA_uniq$speed_ground+FAA_uniq$aircraft+FAA_uniq$height+FAA_uniq$pitch)
model.x5 <- lm(FAA_uniq$distance ~ FAA_uniq$speed_ground+FAA_uniq$aircraft+FAA_uniq$height+FAA_uniq$pitch+FAA_uniq$duration)
model.x6 <- lm(FAA_uniq$distance ~ FAA_uniq$speed_ground+FAA_uniq$aircraft+FAA_uniq$height+FAA_uniq$pitch+FAA_uniq$duration+FAA_uniq$no_pasg)
rsq <- data.frame("Value"=1:6,"rsq"=c(summary(model.x1)$r.squared,summary(model.x2)$r.squared,summary(model.x3)$r.squared,
        summary(model.x4)$r.squared,summary(model.x5)$r.squared,summary(model.x6)$r.squared))
plot(rsq$Value,rsq$rsq)

#Step 18
arsq <- data.frame("Value"=1:6,"arsq"=c(summary(model.x1)$adj.r.squared,summary(model.x2)$adj.r.squared,summary(model.x3)$adj.r.squared,
                                      summary(model.x4)$adj.r.squared,summary(model.x5)$adj.r.squared,summary(model.x6)$adj.r.squared))
plot(arsq$Value,arsq$arsq)

#Step 19
AIC <- data.frame("Value"=1:6,"AIC"=c(AIC(model.x1),AIC(model.x2),AIC(model.x3),
                                      AIC(model.x4),AIC(model.x5),AIC(model.x6)))
plot(AIC$Value,AIC$AIC)

#Step20
#Will use Speed_ground and aircraft

#Step 21
Model_LM <- lm(FAA_uniq$distance ~FAA_uniq$speed_ground+FAA_uniq$aircraft+FAA_uniq$height+FAA_uniq$pitch+FAA_uniq$duration+FAA_uniq$no_pasg ,data = FAA_uniq)
fit1_LM <- stepAIC(Model_LM, direction = 'backward',trace=TRUE)

Model_lm1 <- lm(FAA_uniq$distance ~ FAA_uniq$speed_ground)
fit2_LM <- stepAIC(Model_LM, direction = 'forward',trace=TRUE)





