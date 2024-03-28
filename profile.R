Rprof()
KUB_22_5min <- KUB_22_5min %>% 
  mutate(WATER_FLOW = ifelse(WATER_FLOW == 0, NA, WATER_FLOW))
KUB_23_5min <- KUB_23_5min %>% 
  mutate(WATER_FLOW = ifelse(WATER_FLOW == 0, NA, WATER_FLOW))

KUB_22_5min[which.min(KUB_22_5min$WATER_FLOW),]
KUB_23_5min[which.min(KUB_23_5min$WATER_FLOW),]

sum(KUB_22_5min$WATER_FLOW==0)
sum(KUB_23_5min$WATER_FLOW==0)

for (i in seq(1, nrow(KUB_22_5min)-2, by=3)) {
  avg_value <- mean(KUB_22_5min$WATER_FLOW[i:(i+2)])
  KUB_22_15min$WATER_FLOW[(i+2)/3] <- avg_value
}

for (i in seq(1, nrow(KUB_23_5min)-2, by=3)) {
  avg_value <- mean(KUB_23_5min$WATER_FLOW[i:(i+2)])
  KUB_23_15min$WATER_FLOW[(i+2)/3] <- avg_value
}

for (i in seq(1, nrow(KUB_22_hourly)-1)) {
  avg_value <- mean(KUB_22_hourly$Raw_Temp[i:(i+1)])
  # Update the new 15-minute interval column with the averaged values
  KUB_22_15min$Raw_Temp[i*4 - 3] <- avg_value
  KUB_22_15min$Raw_Temp[i*4 - 2] <- avg_value
  KUB_22_15min$Raw_Temp[i*4 - 1] <- avg_value
  KUB_22_15min$Raw_Temp[i*4] <- avg_value
}

for (i in seq(1, nrow(KUB_23_hourly)-1)) {
  avg_value <- mean(KUB_23_hourly$Raw_Temp[i:(i+1)])
  # Update the new 15-minute interval column with the averaged values
  KUB_23_15min$Raw_Temp[i*4 - 3] <- avg_value
  KUB_23_15min$Raw_Temp[i*4 - 2] <- avg_value
  KUB_23_15min$Raw_Temp[i*4 - 1] <- avg_value
  KUB_23_15min$Raw_Temp[i*4] <- avg_value
}

KUB_22_15min <- KUB_22_15min %>%
  mutate(CDG1_FEED_RATE_LBS_HR = ifelse(ChlorineDioxide1_RATE_LBS_HR == 1, 0, ChlorineDioxide1_RATE_LBS_HR),
         CDG2_FEED_RATE_LBS_HR = ifelse(ChlorineDioxide2_RATE_LBS_HR == 1, 0, ChlorineDioxide2_RATE_LBS_HR),
         CDG_FEED_RATE_LBS_HR_TOTAL = CDG1_FEED_RATE_LBS_HR + CDG2_FEED_RATE_LBS_HR) 

# Polymer feed and avg dose are zero until 2022-11-01 in KUB_22_daily
# head(KUB_22_daily)
KUB_22_daily <- KUB_22_daily %>%
  mutate(Polymer_Feed_Gals = ifelse(Polymer_Feed_Gals == 0, NA, Polymer_Feed_Gals),
         Polymer_Feed_Avg_Dose = ifelse(Polymer_Feed_Avg_Dose == 0, NA, Polymer_Feed_Avg_Dose))

KUB_23_15min <- KUB_23_15min %>%
  mutate(ChlorineDioxide1_RATE_LBS_HR = ifelse(ChlorineDioxide1_RATE_LBS_HR == 1, 0, ChlorineDioxide1_RATE_LBS_HR),
         ChlorineDioxide2_RATE_LBS_HR = ifelse(ChlorineDioxide2_RATE_LBS_HR == 1, 0, ChlorineDioxide2_RATE_LBS_HR),
         ChlorineDioxide_RATE_LBS_HR_TOTAL = ChlorineDioxide1_RATE_LBS_HR + ChlorineDioxide2_RATE_LBS_HR)

KUB_22_15min <- KUB_22_15min %>%
  mutate(ChlorineDioxide1_RATE_LBS_HR = ifelse(ChlorineDioxide1_RATE_LBS_HR == 1, 0, ChlorineDioxide1_RATE_LBS_HR),
         ChlorineDioxide2_RATE_LBS_HR = ifelse(ChlorineDioxide2_RATE_LBS_HR == 1, 0, ChlorineDioxide2_RATE_LBS_HR),
         ChlorineDioxide_RATE_LBS_HR_TOTAL = ChlorineDioxide1_RATE_LBS_HR + ChlorineDioxide2_RATE_LBS_HR)

Rprof(NULL)
summaryRprof()
