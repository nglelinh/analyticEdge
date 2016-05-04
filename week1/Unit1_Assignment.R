# Get the current directory
  getwd()

#################### AN ANALYTICAL DETECTIVE motor vehicle theft ###############################
# Read the csv file
  mvt = read.csv("mvtWeek1.csv")
# Structure of the dataset
  str(mvt)
# Statistical summary
  summary(mvt)
# Number of rows
  nrow(mvt)

# Max of ID
  max(mvt$ID)

# How many observations have value TRUE in the Arrest variable 
  table(mvt$Arrest)

# converts the variable "Date" into a Date object in R
  DateConvert = as.Date(strptime(mvt$Date, "%m/%d/%y %H:%M"))

# extract the month and the day of the week, and add these variables to our data frame mvt
  mvt$Month = months(DateConvert)
  mvt$Weekday = weekdays(DateConvert)

# replace the old Date variable with DateConvert 
  mvt$Date = DateConvert

# which month did the fewest motor vehicle thefts occur
  table(mvt$Month)

# Which month has the largest number of motor vehicle thefts for which an arrest was made
  mvtArrested = subset(mvt, mvt$Arrest == TRUE)
  table(mvtArrested$Month)

# histogram of the variable Date
  hist(mvt$Date, breaks=100)

# Create a boxplot of the variable "Date", sorted by the variable "Arrest" 
  boxplot(mvt$Date ~ mvt$Arrest)

# proportion of motor vehicle thefts in 2001 was an arrest made
  table(mvtArrested$Year)
  table(mvt$Year)

# Which locations are the top five locations for motor vehicle thefts, excluding the "Other" category?
  sort(table(mvt$LocationDescription))

# Create a subset of your data, only taking observations for which the theft happened in one of these five locations
  Top5 = subset(mvt, mvt$LocationDescription == "STREET" | mvt$LocationDescription == "PARKING LOT/GARAGE(NON.RESID.)" | mvt$LocationDescription == "ALLEY" | mvt$LocationDescription == "GAS STATION" | mvt$LocationDescription == "DRIVEWAY - RESIDENTIAL")

# refactor for new subset
  Top5$LocationDescription = factor(Top5$LocationDescription)

# highiest arrest rate
  Top5Arrested = subset(Top5, Top5$Arrest == 'TRUE')
  table(Top5$LocationDescription)
  table(Top5Arrested$LocationDescription)

###############STOCK DYNAMICS###############################################################
  IBM = read.csv("IBMStock.csv")
  GE = read.csv("GEStock.csv")
  ProcterGamble = read.csv("ProcterGambleStock.csv")
  Boeing = read.csv("BoeingStock.csv")
  CocaCola = read.csv("CocaColaStock.csv")


  IBM$Date = as.Date(IBM$Date, "%m/%d/%y")
  GE$Date = as.Date(GE$Date, "%m/%d/%y")
  CocaCola$Date = as.Date(CocaCola$Date, "%m/%d/%y")
  ProcterGamble$Date = as.Date(ProcterGamble$Date, "%m/%d/%y")
  Boeing$Date = as.Date(Boeing$Date, "%m/%d/%y")  

  # Visualizing Stock Dynamics
  plot(CocaCola$Date, CocaCola$StockPrice, type="l", col="red")
  lines(ProcterGamble$Date, ProcterGamble$StockPrice, col="blue")

  # Vertical line
  abline(v=as.Date(c("2000-03-01")), lwd=2)
  abline(v=as.Date(c("1983-01-01")), lwd=2)

  # how the stock prices changed from 1995-2005 for all five companies
  plot(CocaCola$Date[301:432], CocaCola$StockPrice[301:432], type="l", col="red", ylim=c(0,210))
  lines(IBM$Date[301:432], IBM$StockPrice[301:432], col="blue")
  lines(GE$Date[301:432], GE$StockPrice[301:432], col="orange")
  lines(ProcterGamble$Date[301:432], ProcterGamble$StockPrice[301:432], col="green")
  lines(Boeing$Date[301:432], Boeing$StockPrice[301:432], col="black")
  abline(v=as.Date(c("2000-03-01")), lwd=2)
  abline(v=as.Date(c("1997-10-01")), lwd=2)

  # Use the tapply command to calculate the mean stock price of IBM, sorted by months
  tapply(IBM$StockPrice, months(IBM$Date), mean, na.rm=TRUE)

##################DEMOGRAPHICS AND EMPLOYMENT IN THE UNITED STATES############################
