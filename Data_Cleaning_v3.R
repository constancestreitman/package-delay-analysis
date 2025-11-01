df = logistics_shipments_dataset

###PART 1: REPLACING TRANSIT DAY N/A VALUES W THE MEDIAN VAL

df$Shipment_Date <- as.Date(df$Shipment_Date)
df$Delivery_Date <- as.Date(df$Delivery_Date)

error_rows <- which(df$Shipment_Date > df$Delivery_Date)
temp_ship_date <- df$Shipment_Date[error_rows]
df$Shipment_Date[error_rows] <- df$Delivery_Date[error_rows]
df$Delivery_Date[error_rows] <- temp_ship_date

df$Transit_Days <- as.numeric(df$Delivery_Date - df$Shipment_Date)
df$Transit_Days[is.na(df$Transit_Days)] <- median(df$Transit_Days, na.rm = TRUE)


###PART 2: PRUNING, VARIABLE SETTING

df = subset(df, select = -Shipment_ID) #deleting shipment_id
df = subset(df, tolower(Status) != "in transit") #deleting "in transit" samples
df$D_or_L <- ifelse(tolower(df$Status) %in% c("delayed", "lost"), 1, 0)
# ^ makes the binary
df <- subset(df, select = -Status)

#making everything below a factor bc idk ur supposed to
df$Origin_Warehouse = as.factor(df$Origin_Warehouse)
df$Destination = as.factor(df$Destination)
df$Carrier = as.factor(df$Carrier)
df$D_or_L = as.factor(df$D_or_L)


###PART 3: FIXING NA DELIVERY DATE VALUES, NA COST

df$Cost[is.na(df$Cost)] = median(df$Cost, na.rm = TRUE)
# i ran a shapiro-wilk test - Cost is not normal, pval small - so, median
df$Delivery_Date = ifelse(
  is.na(df$Delivery_Date) & !is.na(df$Transit_Days),
  df$Shipment_Date + df$Transit_Days,
  df$Delivery_Date
) #filling in delivery date values where possible, given in transit var exists
df$Delivery_Date <- as.Date(df$Delivery_Date, origin = "1970-01-01")


###PART 4: adding weekday value
df$ShipWeekday <- factor(
  weekdays(df$Shipment_Date, abbreviate = FALSE),
  levels = c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"),
  ordered = FALSE
)
df <- df[, c("D_or_L", "Origin_Warehouse", "Destination", 
             "Carrier", "ShipWeekday", "Shipment_Date", "Delivery_Date",
             "Transit_Days", "Distance_miles", "Weight_kg", "Cost")]
#making it in a better order

###PART 4: SAVING

write.csv(df, "C:/Users/[your name here]/Downloads/df.csv", row.names = FALSE)

