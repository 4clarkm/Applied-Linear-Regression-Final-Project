# Stat 333 Final Project  -------------------------------------------------

#Initialization 
rm(list = ls())
setwd("C:/Users/mason/OneDrive/Desktop/2025-2026/Stats 333/Final Project")
getwd()

install.packages("corrplot")
library(corrplot)

Airline_Data <- read.csv("AirlineData.csv")
df <- data.frame(Airline_Data)
df_flights <- readRDS("df_flights.rds")
df_cancelled <- readRDS("df_cancelled.rds")

# CST Time Fixer ----------------------------------------------------------

### Converts all time into CST 
# timevar: local time from original BTS data
# state: U.S. state in which the airport is located.
# city: city in which the airport is located.
# output: outputs the time in CST

CSTTimeFixer <- function(timevar,state,city) {
  ## Oddballs ##
  if(state == "AK") return(timevar + 300)
  if(state == "HI") return(timevar + 400)
  if(state == "PR") return(timevar - 200)

  if(state %in% c("CT","DE","DC","GA","IN","ME","MD","MA","MI","NH","NJ","NY","NC","OH",
                  "PA","RI","SC","VT","VA","WV")) return(timevar - 100)

  if(state == "FL") {
    return(ifelse(city != "Pensacola, FL",timevar-100,timevar))
  }

  if(state == "KY") {
    return(ifelse(city != "Paducah, KY",timevar-100,timevar))
  }
  # TN (half east coast) needs to be separated
  if(state == "TN"){
    return(ifelse(city == "Chattanooga, TN" | city == "Knoxville,TN",timevar-100,timevar))
  }
  
  
  if(state == "SD") {
    return(ifelse(city == "Rapid City, SD",timevar+100,timevar))
  }

  if(state == "TX") {
    return(ifelse(city == "El Paso, TX",timevar+100,timevar))
  }
  
  if(state %in% c("AZ","CO","ID","KS","MT","NM")) return(timevar + 100)
  if(state %in% c("CA","NV","OR","WA")) return(timevar+200)
  return(timevar)
}

# Data Cleaning  ----------------------------------------------------------

df$On_Time <- as.integer(df$Arr_Delay <= 0)

df$Dep_Time_CST <- mapply(CSTTimeFixer, df$Dep_Time_Scheduled, df$Dep_State, df$Dep_City)
df$Arr_Time_CST <- mapply(CSTTimeFixer, df$Arr_Time_Scheduled, df$Arr_State, df$Arr_City)
df$Dep_Time_Actual_CST <- mapply(CSTTimeFixer, df$Dep_Time_Actual, df$Dep_State, df$Dep_City)
df$Arr_Time_Actual_CST <- mapply(CSTTimeFixer, df$Arr_Time_Actual, df$Arr_State, df$Arr_City)


df_flights <- df[df$Cancelled == 0, ]
df_cancelled <- df[df$Cancelled == 1, ]
df_cancelled$Partial_Move <- !is.na(df_cancelled$Wheels_Off)

df_flights$Dep_Hour <- df_flights$Dep_Time_CST %/% 100

delay_cols <- c("Carrier_Delay_Time","Weather_Delay_Time","NAS_Delay_Time",
                "Security_Delay_Time","Late_Aircraft_Delay_Time")

df_flights[delay_cols] <- lapply(df_flights[delay_cols], function(x) ifelse(is.na(x),0,x))

saveRDS(df_flights, "df_flights.rds")
saveRDS(df_cancelled, "df_cancelled.rds")

# Correlation---------------------------------------------------------------

num_cols <- sapply(df_flights, is.numeric)
numeric_data <- df_flights[, num_cols]

numeric_data_nonzero <- numeric_data[, sapply(numeric_data, function(x) sd(x, na.rm = TRUE) != 0)]
cor_matrix <- cor(numeric_data_nonzero, use = "pairwise.complete.obs")
cor_matrix


corrplot(cor_matrix, 
         method = "color",     
         type = "upper",        
         tl.col = "black",      
         tl.cex = 0.7,          
         addCoef.col = "black",
         number.cex = 0.4,      
         order = "hclust") 

plot(df_flights$Dep_Hour, df_flights$Arr_Delay)
    

