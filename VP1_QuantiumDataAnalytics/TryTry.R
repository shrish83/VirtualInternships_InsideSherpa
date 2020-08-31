df <- as.data.table(data.frame(id = c(201801,201903,201801,201903),store=c(1,1,2,2),sales = c(244.34,101.55,193.00,151.89)))
df1 <- as.data.table(data.frame(id = c(201801,201903),store=c(77,77),sales = c(105.22,155.90)))

#df$store <- replace(df$store,values = rep(1:272))

st <- 77
calCorr <- function(df,df1,st){
  calTable = data.table(Store1 = numeric(), Store2 = numeric(), YEARMONTH = numeric(),mag_measure = numeric())

  stN <- df %>% select(store)

  for(i in stN){

    contSt <- df %>% filter(store==i)
    contSt <- contSt %>% select(sales)

    calMeasure = data.table("Store1" = st, "Store2" = i, "YEARMONTH" = df$id ,"mag_measure" = abs(df1$sales - contSt$sales))
    min_measure <- min(calMeasure$mag_measure)
    max_measure <- max(calMeasure$mag_measure)
    calMeasure <- unique(calMeasure)
    calMeasure <- calMeasure %>% group_by(Store1,YEARMONTH) %>% mutate(minDist = min_measure, maxDist = max_measure)

    calTable <- rbind(calTable, calMeasure)

    }
   distTable <- calTable %>% mutate(magnitudeMeasure = 1 - (mag_measure - minDist)/(maxDist - minDist))
   finalDistTable <- distTable %>% group_by(Store1, Store2) %>% mutate(mean_mag_measure  = mean(magnitudeMeasure))

   return(finalDistTable)
}

nSales <- calCorr(df,df1,st)


# calculateMagnitudeDistance <- function(inputTable, metricCol, storeComparison)
# { 
#   calcDistTable = data.table(Store1 = numeric(), Store2 = numeric(), YEARMONTH = numeric(), measure = numeric())
#   
#   storeNumbers <- inputTable %>% .$STORE_NBR 
#   
#   for (i in storeNumbers) {
#     
#     calculatedMeasure = data.table("Store1" = storeComparison , "Store2" = i , "YEARMONTH" = inputTable[STORE_NBR == storeComparison, YEARMONTH] , "measure" = abs(inputTable[inputTable$STORE_NBR == storeComparison, eval(metricCol)] - inputTable[inputTable$STORE_NBR == i, eval(metricCol)]))
#     
#     calcDistTable <- rbind(calcDistTable, calculatedMeasure)
#   }
#   
#   #### Standardise the magnitude distance so that the measure ranges from 0 to 1
#   minMaxDist <‐ calcDistTable[, .(minDist = min(measure), maxDist = max(measure)), by = c("Store1", "YEARMONTH")]
#   
#   distTable <‐ merge(calcDistTable, minMaxDist, by = c("Store1", "YEARMONTH"))
#   
#   distTable[, magnitudeMeasure := 1 ‐ (measure ‐ minDist)/(maxDist ‐ minDist)]
#   
#   finalDistTable <‐ distTable[, .(mag_measure  = mean(magnitudeMeasure)), by = .(Store1, Store2)]
#   
#   return(finalDistTable)
# }


# Serving:
#   melted butter = 3 tsp
# fine chopped cashew 1 tsp
# fine chopped almonds 1tsp
# dhaniya 1 tsp
# chat masala i/2 tsp
# cream - 2 tbsp
# chilli flakes 1/2 tsp
# chese NA
# lemon juice few drops
# mix altogether

data[, YEARMONTH := year(DATE)*100 + month(DATE)]

measureOverTime <- data[, .(totSales = sum(TOT_SALES),
                            nCustomers = uniqueN(LYLTY_CARD_NBR),
                            nTxnPerCust = uniqueN(TXN_ID)/uniqueN(LYLTY_CARD_NBR),
                            nChipsPerTxn = sum(PROD_QTY)/uniqueN(TXN_ID),
                            avgPricePerUnit = sum(TOT_SALES)/sum(PROD_QTY)), by = c("STORE_NBR", "YEARMONTH")][order(STORE_NBR,YEARMONTH)]




storesWithFullObs <- unique(measureOverTime[, .N, STORE_NBR][N == 12,STORE_NBR])
preTrialMeasures <- measureOverTime[YEARMONTH < 201902 & STORE_NBR %in% storesWithFullObs, ]



calculateCorrelation <- function(inputTable, metricCol, storeComparison) {
  calcCorrTable = data.table(Store1 = numeric(), Store2 = numeric(), corr_measure = numeric())
  storeNumbers <- unique(inputTable[, inputTable$STORE_NBR])
  for (i in storeNumbers) {
    calculatedMeasure = data.table("Store1" = storeComparison, "Store2" = i,
                                   "corr_measure" = cor(inputTable[STORE_NBR == storeComparison,
                                                                   eval(metricCol)], inputTable[STORE_NBR == i, eval(metricCol)]))    
    calcCorrTable <- rbind(calcCorrTable, calculatedMeasure)
  }
  return(calcCorrTable)
}

calculateMagnitudeDistance <- function(inputTable, metricCol, storeComparison)
  {
    calcDistTable = data.table(Store1 = numeric(), Store2 = numeric(), YEARMONTH = numeric(), measure = numeric())
    storeNumbers <- unique(inputTable[, STORE_NBR])
    for (i in storeNumbers) 
      {
      calculatedMeasure = data.table("Store1" = storeComparison, "Store2" = i, "YEARMONTH" = inputTable[STORE_NBR == storeComparison, YEARMONTH], "measure" = abs(inputTable[STORE_NBR == storeComparison, eval(metricCol)] - inputTable[STORE_NBR == i,eval(metricCol)]))
      
      calcDistTable <- rbind(calcDistTable, calculatedMeasure)
    }
}

calculateMagnitudeDistance <- function(inputTable, metricCol, storeComparison)
  {
    calcDistTable = data.table(Store1 = numeric(), Store2 = numeric(), YEARMONTH = numeric(), measure = numeric())
    storeNumbers <- unique(inputTable[, STORE_NBR])
    for (i in storeNumbers) { 
      calculatedMeasure = data.table("Store1" = storeComparison
                                     , "Store2" = i, "YEARMONTH" = inputTable[STORE_NBR ==storeComparison, YEARMONTH], "measure" = abs(inputTable[STORE_NBR == storeComparison, eval(metricCol)]- inputTable[STORE_NBR == i,eval(metricCol)]) )
      calcDistTable <- rbind(calcDistTable, calculatedMeasure)
    }
    minMaxDist <- calcDistTable[, .(minDist = min(measure), maxDist = max(measure)), by = c("Store1", "YEARMONTH")]
    distTable <- merge(calcDistTable, minMaxDist, by = c("Store1", "YEARMONTH"))
    distTable[, magnitudeMeasure := 1 - (measure - minDist)/(maxDist - minDist)]
    finalDistTable <- distTable[, .(mag_measure = mean(magnitudeMeasure)), by = .(Store1, Store2)]
    
    return(finalDistTable)
}

trial_store <- 77
corr_nSales <- calculateCorrelation(preTrialMeasures, quote(totSales),trial_store)
corr_nCustomers <- calculateCorrelation(preTrialMeasures, quote(nCustomers),trial_store)

magnitude_nSales <- calculateMagnitudeDistance(preTrialMeasures,quote(totSales), trial_store)
magnitude_nCustomers <- calculateMagnitudeDistance(preTrialMeasures,quote(nCustomers), trial_store)


##### *******************

standMag <- function(magnitude_nSales) {
  minMaxDist <- magnitude_nSales[, .(minDist = min( magnitude_nSales$mag_measure), maxDist = max(magnitude_nSales$mag_measure)), by = c("Store1", "YEARMONTH")]
  distTable <- merge(magnitude_nSales, minMaxDist, by = c("Store1", "YEARMONTH"))
  distTable[, magnitudeMeasure := 1 - (mag_measure - minDist)/(maxDist - minDist)]
  finalDistTable <- distTable[, .(magN_measure = mean(magnitudeMeasure)), by = .(Store1, Store2)]
  return(finalDistTable)
}

standMag2 <- function(magnitude_nCustomers) {
  minMaxDist <- magnitude_nCustomers[, .(minDist = min( magnitude_nCustomers$mag_measure), maxDist = max(magnitude_nCustomers$mag_measure)), by = c("Store1", "YEARMONTH")]
  distTable <- merge(magnitude_nCustomers, minMaxDist, by = c("Store1", "YEARMONTH"))
  distTable[, magnitudeMeasure := 1 - (mag_measure - minDist)/(maxDist - minDist)]
  finalDistTable <- distTable[, .(magN_measure = mean(magnitudeMeasure)), by = .(Store1, Store2)]
  return(finalDistTable)
}


magnitude_nCustomers <- standMag2(magnitude_nCustomers)
magN <- standMag(magnitude_nSales)









##### *******************
min_measure <-min(calTable$mag_measure)
max_measure <-max(calTable$mag_measure)
#calMeasure <- unique(calMeasure)
calTable <- calTable %>% group_by(Store1,YEARMONTH) %>% mutate(minDist = min_measure, maxDist = max_measure)

distTable <- calTable %>% mutate(magnitudeMeasure = 1 - (mag_measure - minDist)/(maxDist - minDist))
finalDistTable <- distTable %>% group_by(Store1, Store2) %>% mutate(mean_mag_measure  = mean(magnitudeMeasure))

##############################################333

measureOverTimeSales <- as.data.table(measureOverTime)
pastSales <- measureOverTimeSales[, Store_type := ifelse(STORE_NBR == trial_store, "Trial",ifelse(STORE_NBR == control_store,"Control", "Other stores"))][, totSales := mean(totSales), by = c("YEARMONTH","Store_type")][, TransactionMonth := as.Date(paste(YEARMONTH %/%100, YEARMONTH %% 100, 1, sep = "‐"), "%Y‐%m‐%d")][YEARMONTH < 201903 , ]

################################


df1 <- data.frame(A = numeric(),B=numeric(),C=numeric())
df2 <- data.frame(A = numeric(),B=numeric(),C=numeric())
mdf <- rbind(df1$A,df2$C)
 mdf
rm(df1,df2,mdf) 

a <- list(c(1,"2","3"))
mode(b)
class(b)
typeof(b)

b <- c(1,1.23)

d <- length(10)
d <- d[2*1:5]  ##length reduced to 5
d

e <- seq(1,36)
dim(e) <- c(3,4,3)
e[1,,]
 class(e)
typeof(e) 

nm <- matrix(1:9,3,3)

e <- seq(1,6)
dim(e) <- c(3,2,1)
e

####OUTER PRODUCT
e %o% e ## or outer(e,e, "*")  
e * e

###MATRIX MULTIPLICATION
nm %*% nm

###linear EQUATION
 # b <- A %*% X or solve(A,b) 

a
a[[1]][1]

g <- data.frame(A = c(1,2), B = c(3,4))
g$A
attach(g)
A
detach(g)
A  #Error: object not found
search()

#Conventionally, a p-value of less than 0.05 indicates that 
#the variables are likely not independent whereas a p-value 
#exceeding 0.05 fails to provide any such evidence.


tf <- c(1,3,2,5,4)
mean(tf > 3)
