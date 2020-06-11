# COURSE:EC410: MID TERM EXAM
# EXXON MOBIL CASE STUDY
# INSTALL AND LOAD PACKAGES 
library(datasets)  # LOAD BASE PACKAGES MANUALLY
# Installs pacman ("package manager") if needed
if (!require("pacman")) install.packages("pacman")
install.packages("car")
library(car)
install.packages("olsrr")
library(olsrr)
install.packages("ggfortify")
library(ggfortify)
install.packages("ggcorrplot")
library(ggcorrplot)
install.packages("zoo")
library(zoo)
#FOR GRID ARRANGE 
install.packages("gridExtra")
library(gridExtra)


# IMPORT FILE 
Readfile <- read.csv("D:/Econometrics_business_Frct/Project/CleanedStockPriceData.csv")

# REMOVING THE DATE FIELD 
Test_File <-  Readfile[,-1] 

# READING THE FILE 
Test_File[1:3,]

# SUMMARY OF THE FILE 
summary(Test_File)

#CREATING XOM FILE 
XOM_File <- Test_File[,-7]
head(XOM_File)

#CREATING RDSA FILE
RDSA_File <- Test_File[,-1]
head(RDSA_File)

#PLOTTING CONTINUOUS VARIABLES 
grid.arrange(
  ggplot(XOM_File,aes(x = Oil_Futures, color = XOM))+ 
    geom_freqpoly(size=2)+
    theme_minimal(),
  ggplot(XOM_File,aes(x = Oil_Prices, color = XOM))+ 
    geom_freqpoly(size=2)+
    theme_minimal(),
  ggplot(XOM_File,aes(x = S_P, color = XOM))+ 
    geom_freqpoly(size=2)+
    theme_minimal(),
  ggplot(XOM_File,aes(x = VIX, color = XOM))+ 
    geom_freqpoly(size=2)+
    theme_minimal(),
  ggplot(XOM_File,aes(x = TNX, color = XOM))+ 
    geom_freqpoly(size=2)+
    theme_minimal()  
)

#SCATTER PLOTS OF INDEPENDENT AND DEPENDENT VARIABLE
plot(XOM_File,col="blue")  # Entire data frame
plot(XOM_File$Oil_Prices,XOM_File$XOM,col="blue")  # Price_of_oil x XOM_Stock_Price
plot(XOM_File$TNX, XOM_File$XOM ,col="blue")  # Interest_Rates x XOM_Stock_Price
plot(XOM_File$VIX, XOM_File$XOM ,col="blue")  # Volatility_Index x XOM_Stock_Price
plot(XOM_File$Oil_Futures, XOM_File$XOM ,col="blue")  # Price_of_oil_futures x XOM_Stock_Price
plot(XOM_File$S_P, XOM_File$XOM, col="blue")  # GSPC_Index x XOM_Stock_Price
plot(XOM_File$Oil_Prices, XOM_File$Oil_Prices, col="blue")  # Price_of_oil x Price_of_oil


# REGRESSION OF ENTIRE DATA
model_1 <- lm(XOM ~.,data=XOM_File)
summary(model_1)
vif(model_1) #VARIANCE INTERFACE FACTOR 
ols_plot_added_variable(model_1) # PLOTS FOR COLLINEARITY
coef(model_1)             # COEFFICIENTS (SAME AS MODEL_1)
confint(model_1)          # CI FOR COEFFICIENTS
resid(model_1)            # RESIDUALS CASE-BY-CASE
hist(residuals(model_1))  # HISTOGRAM OF RESIDUALS


#CORRELATION PLOT 
options(repr.plot.width =6, repr.plot.height = 6)
XOM_cor <- round(cor(XOM_File[,c("Oil_Futures","Oil_Prices","S_P","VIX","TNX")]), 1)
ggcorrplot(XOM_cor, hc.order = TRUE, type = "upper",
           lab = TRUE)


# REGRESSION AFTER ELIMINATING COLUMNS WHICH ARE NOT STATISTICALLY SIGNIFICANT
model_2 <- lm(XOM ~ S_P + VIX + TNX,data = XOM_File)
summary(model_2)
vif(model_2)
coef(model_2)             # COEFFICIENTS 
confint(model_2)          # CI FOR COEFFICIENTS
resid(model_2)            # RESIDUALS CASE-BY-CASE 
hist(residuals(model_2))     # HISTOGRAM OF RESIDUALS



# REGRESSION AFTER LOG TRANSFORMATION
vix = log(XOM_File$VIX)
sp=log(XOM_File$S_P)
plot(sp)
model_3 <- lm(XOM ~ sp + vix + TNX,data = XOM_File)
summary(model_3)


#PREDICTION FOR RDSA STOCK PRICE
model_4 <- lm(RDSA ~ S_P + VIX + TNX,data = RDSA_File)
summary(model_4)


# DECLARING THE DATA AS TIME SERIES
XOM_tseries <- ts(XOM_File$XOM, start=c(2020,2), freq=6)
plot(XOM_tseries)
