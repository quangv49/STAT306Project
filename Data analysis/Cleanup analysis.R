

weather <- read.table("YVR_feb_weather_data.csv", sep = ",", header=TRUE)
weatherAbove0 <- weather[which(weather$Temp..Ã..C.>0),]
weatherBelow0 <- weather[which(weather$Temp..Ã..C.<=0),]

weather$isAbove0 <- seq(1,648)
for (i in 1:648) {
  if (weather$Temp..Â.C.[i] > 0) {weather$isAbove0[i] = 1}
  if (weather$Temp..Â.C.[i] <= 0) {weather$isAbove0[i] = 0}
}
weather$isAbove0 <- as.factor(weather$isAbove0)

#Analysis for above 0 oC dataset starts here

#Plots

#Temp vs dew point
dp_temp <- plot(weather$Temp..Ã‚.C., weather$Dew.Point.Temp..Ã‚.C.,
                xlab = "Temperature (deg Celsius)",
                ylab = "Dew point temperature (deg Celsius)",
                main = "Scatterplot of dew point temperature against celsius")

#Relative humidity vs dew point
dp_rh <- plot(weather$Rel.Hum...., weather$Dew.Point.Temp..Ã‚.C.,
              xlab = "Relative humidity (percent)",
              ylab = "Dew point temperature (deg Celsius)",
              main = "Scatterplot of dew point temperature against relative humidity")

#Wind speed vs dew point
dp_wind <- plot(weather$Wind.Spd..km.h., weather$Dew.Point.Temp..Ã‚.C.,
                xlab = "Wind speed (km/h)",
                ylab = "Dew point temperature (deg Celsius)",
                main = "Scatterplot of dew point temperature against windspeed")

#Station pressure vs dew point
dp_pres <- plot(weather$Stn.Press..kPa., weather$Dew.Point.Temp..Ã‚.C.,
                xlab = "Station pressure (kPa)",
                ylab = "Dew point temperature (deg Celsius)",
                main = "Scatterplot of dew point temperature against station pressure")

#Models

#Full model
lin_full <- lm(weather$Dew.Point.Temp..Ã‚.C. ~
                 weather$Temp..Ã‚.C. +
                 weather$Rel.Hum.... +
                 weather$Wind.Spd..km.h. +
                 weather$Stn.Press..kPa.)
summary(lin_full)

#Residual plot of full model
res_lin_full <- plot(lin_full$fitted.values, lin_full$residuals,
                     xlab = "Fitted values",
                     ylab = "Residuals",
                     main = "Residual plot for full linear model")

#Satisfactory model found as reference

quad_full <- lm(weather$Dew.Point.Temp..Ã‚.C.~
                    weather$Temp..Ã‚.C.*weather$isAbove0 + 
                    I(weather$Temp..Ã‚.C.^2)*weather$isAbove0 + 
                    weather$Rel.Hum....*weather$isAbove0 + 
                    I(weather$Rel.Hum....^2)*weather$isAbove0 + 
                    weather$Wind.Spd..km.h. + 
                    weather$Stn.Press..kPa.)
summary(quad_full)
res_quad_model <- plot(quad_full$fitted.values, quad_full$residuals,
                       xlab = "Fitted values",
                       ylab = "Residuals",
                       main = "Residual plot for quadratic model")

#Below 0 is very likely cubic

cubic_model <- lm(weather$Dew.Point.Temp..Â.C.~
                    weather$Temp..Â.C.*weather$isAbove0 + 
                    I(weather$Temp..Â.C.^2)*weather$isAbove0 + 
                    I(weather$Temp..Â.C.^3)*weather$isAbove0 +
                    weather$Rel.Hum....*weather$isAbove0 + 
                    I(weather$Rel.Hum....^2)*weather$isAbove0 +
                    I(weather$Rel.Hum....^3)*weather$isAbove0 +
                    weather$Wind.Spd..km.h. + 
                    weather$Stn.Press..kPa.)
summary(cubic_model)
res_cubic_model <- plot(cubic_model$fitted.values, cubic_model$residuals,
                        xlab = "Fitted values",
                        ylab = "Residuals",
                        main = "Residual plot for cubic model")
plot(weather$Stn.Press..kPa., (cubic_model$residuals - mean(cubic_model$residuals))/sd(cubic_model$residuals))

#Model selection on cubic model
library(leaps)
cubic_model_formula <- weather$Dew.Point.Temp..Â.C.~
    weather$Temp..Â.C.*weather$isAbove0 + 
    I(weather$Temp..Â.C.^2)*weather$isAbove0 + 
    I(weather$Temp..Â.C.^3)*weather$isAbove0 +
    weather$Rel.Hum....*weather$isAbove0 + 
    I(weather$Rel.Hum....^2)*weather$isAbove0 +
    I(weather$Rel.Hum....^3)*weather$isAbove0 +
    weather$Wind.Spd..km.h. + 
    weather$Stn.Press..kPa.
selection <- regsubsets(cubic_model_formula, data=weather)
summary(selection)$which
plot(seq(2,9), summary(selection)$cp, ylim=c(10,20))
abline(0,1)
summary(selection)$cp

library(boot)

cubic_model_glm <- glm(weather$Dew.Point.Temp..Â.C.~
                        weather$Temp..Â.C.*weather$isAbove0 + 
                        I(weather$Temp..Â.C.^2)*weather$isAbove0 + 
                        I(weather$Temp..Â.C.^3)*weather$isAbove0 +
                        weather$Rel.Hum....*weather$isAbove0 + 
                        I(weather$Rel.Hum....^2)*weather$isAbove0 +
                        I(weather$Rel.Hum....^3)*weather$isAbove0 +
                        weather$Wind.Spd..km.h. + 
                        weather$Stn.Press..kPa.)
cv <- cv.glm(weather,cubic_model_glm, K=3)
cv$delta
