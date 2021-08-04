weather <- read.table("YVR_feb_weather_data.csv", sep = ",", header=TRUE)
View(weather)
test_model <- lm(weather$Visibility..km.~weather$Temp..Â.C.+weather$Dew.Point.Temp..Â.C.+weather$Rel.Hum....+weather$Wind.Spd..km.h.+weather$Stn.Press..kPa.)
summary(test_model)
res_plot<-plot(test_model$fitted.values, test_model$residuals)
qq_plot <- qqnorm(y=test_model$residuals)
vis_temp_plot <- plot(weather$Temp..Â.C.,weather$Visibility..km.)
logvis_temp_plot <- plot(weather$Temp..Â.C.,log(weather$Visibility..km.))
test_model2 <- lm(weather$Dew.Point.Temp..Â.C.~weather$Temp..Â.C.+weather$Rel.Hum....+weather$Wind.Spd..km.h.+weather$Stn.Press..kPa.)                
res_plot2 <- plot(test_model2$fitted.values, test_model2$residuals)

dp_temp_plot <- plot(weather$Temp..Â.C., weather$Dew.Point.Temp..Â.C., xlab="Temp", ylab="Dew point temp")
dp_windspd_plot <- plot(weather$Wind.Spd..km.h., weather$Dew.Point.Temp..Â.C., xlab="Wind speed", ylab="Dew point temp")
dp_pres_plot <- plot(weather$Stn.Press..kPa., weather$Dew.Point.Temp..Â.C., xlab="Pressure", ylab="Dew point temp")
dp_lnwindspd_plot <- plot(log(weather$Wind.Spd..km.h.), weather$Dew.Point.Temp..Â.C., xlab="ln Wind speed", ylab="Dew point temp")
dp_lnpres_plot <- plot(log(weather$Stn.Press..kPa.), weather$Dew.Point.Temp..Â.C., xlab="ln Pressure", ylab="Dew point temp")
dp_rh_plot <- plot(weather$Rel.Hum...., weather$Dew.Point.Temp..Â.C., xlab = "Relative humidity", ylab= "Dew point temp")
test_model2 <- lm(weather$Dew.Point.Temp..Â.C.~weather$Temp..Â.C.+weather$Rel.Hum....+weather$Wind.Spd..km.h.+weather$Stn.Press..kPa.)             
summary(test_model2)
test_model2_temprh <- lm(weather$Dew.Point.Temp..Â.C.~weather$Temp..Â.C.*weather$Rel.Hum....+weather$Wind.Spd..km.h.+weather$Stn.Press..kPa.)
summary(test_model2_temprh)
res_plot2_temprh <- plot(test_model2_temprh$fitted.values, test_model2_temprh$residuals)
test_model2_presrh <- lm(weather$Dew.Point.Temp..Â.C.~weather$Temp..Â.C.+weather$Wind.Spd..km.h.+weather$Stn.Press..kPa.*weather$Rel.Hum....)
summary(test_model2_presrh)
res_plot2_presrh <- plot(test_model2_presrh$fitted.values, test_model2_presrh$residuals)
test_model3 <- lm(weather$Dew.Point.Temp..Â.C.~weather$Temp..Â.C.+weather$Rel.Hum....+weather$Stn.Press..kPa.)
summary(test_model3)
res_plot3 <- plot(test_model3$fitted.values, test_model3$residuals)
test_model4 <- lm(weather$Dew.Point.Temp..Â.C.~weather$Temp..Â.C.+weather$Rel.Hum....)
summary(test_model4)
res_plot4 <- plot(test_model4$fitted.values, test_model4$residuals)

pres_temp_plot <- plot(weather$Temp..Â.C., weather$Stn.Press..kPa.)

test_model5 <- lm(weather$Dew.Point.Temp..Â.C.~weather$Temp..Â.C.+I(weather$Temp..Â.C.^2)+weather$Rel.Hum....+I(weather$Rel.Hum....^2))
summary(test_model5)
res_plot5 <- plot(test_model5$fitted.values, test_model5$residuals)
weather$isAbove0 <- seq(1,648)
for (i in 1:648) {
  if (weather$Temp..Â.C.[i] > 0) {weather$isAbove0[i] = 1}
  if (weather$Temp..Â.C.[i] <= 0) {weather$isAbove0[i] = 0}
}
weather$isAbove0 <- as.factor(weather$isAbove0)

test_model6 <- lm(weather$Dew.Point.Temp..Â.C.~weather$Temp..Â.C.*weather$isAbove0+weather$Rel.Hum....*weather$isAbove0)
summary(test_model6)
res_plot6 <- plot(test_model6$fitted.values, test_model6$residuals)

weatherAbove0 <- subset(weather, weather$isAbove0==1)
test_model7 <- lm(weatherAbove0$Dew.Point.Temp..Â.C.~weatherAbove0$Temp..Â.C.+weatherAbove0$Rel.Hum....)
res_plot7 <- plot(test_model7$fitted.values, test_model7$residuals)

test_model8 <- lm(weather$Dew.Point.Temp..Â.C.~weather$Temp..Â.C.*weather$isAbove0+I(weather$Temp..Â.C.^2)*weather$isAbove0+weather$Rel.Hum....*weather$isAbove0 + I(weather$Rel.Hum....^2)*weather$isAbove0)
res_plot8 <- plot(test_model8$fitted.values, test_model8$residuals)

min_dew_point = min(weather$Dew.Point.Temp..Â.C.)
test_model9 <- lm(log(weather$Dew.Point.Temp..Â.C.-min_dew_point+1)~weather$Temp..Â.C.*weather$isAbove0+I(weather$Temp..Â.C.^2)*weather$isAbove0+weather$Rel.Hum....*weather$isAbove0 + I(weather$Rel.Hum....^2)*weather$isAbove0)
res_plot9 <- plot(test_model9$fitted.values, test_model9$residuals)

test_model10 <- lm(weather$Dew.Point.Temp..Â.C.~weather$isAbove0*(weather$Temp..Â.C.+I(weather$Temp..Â.C.^2)+weather$Rel.Hum....*weather$isAbove0 + I(weather$Rel.Hum....^2)+weather$Stn.Press..kPa.+weather$Wind.Spd..km.h.))
res_plot10 <- plot(test_model10$fitted.values, test_model10$residuals)
