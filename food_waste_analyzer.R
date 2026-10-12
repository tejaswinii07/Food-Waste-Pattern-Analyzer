```r
# ============================================================
# PROJECT: FOOD WASTE PATTERN ANALYZER USING R
# Concepts: Data Frames, Lists, Functions, Loops, Conditions
# ============================================================


# STEP 1: LOAD THE DATASET

food_data <- read.csv("food_waste_data.csv",
                      stringsAsFactors = FALSE)

cat("\n========================================\n")
cat("       FOOD WASTE PATTERN ANALYZER\n")
cat("========================================\n")

cat("\nOriginal Food Waste Dataset:\n")
print(food_data)


# STEP 2: CHECK THE DATASET

cat("\nDataset Structure:\n")
str(food_data)

cat("\nNumber of Rows and Columns:\n")
print(dim(food_data))

cat("\nColumn Names:\n")
print(colnames(food_data))


# STEP 3: CALCULATE FOOD WASTE

# Unserved food
food_data$Unserved_kg <-
  food_data$Prepared_kg - food_data$Served_kg

# Food left on plates
food_data$Plate_Waste_kg <-
  food_data$Served_kg - food_data$Consumed_kg

# Total food waste
food_data$Total_Waste_kg <-
  food_data$Prepared_kg - food_data$Consumed_kg

# Waste percentage
food_data$Waste_Percentage <-
  (food_data$Total_Waste_kg /
     food_data$Prepared_kg) * 100

cat("\n========================================\n")
cat("         FOOD WASTE CALCULATIONS\n")
cat("========================================\n")

print(food_data)


# STEP 4: OVERALL FOOD WASTE SUMMARY

total_prepared <- sum(food_data$Prepared_kg)
total_served <- sum(food_data$Served_kg)
total_consumed <- sum(food_data$Consumed_kg)
total_unserved <- sum(food_data$Unserved_kg)
total_plate_waste <- sum(food_data$Plate_Waste_kg)
total_waste <- sum(food_data$Total_Waste_kg)

overall_waste_percentage <-
  (total_waste / total_prepared) * 100

cat("\n========================================\n")
cat("          OVERALL WASTE SUMMARY\n")
cat("========================================\n")

cat("Total Food Prepared:", total_prepared, "kg\n")
cat("Total Food Served:", total_served, "kg\n")
cat("Total Food Consumed:", total_consumed, "kg\n")
cat("Total Unserved Food:", total_unserved, "kg\n")
cat("Total Plate Waste:", total_plate_waste, "kg\n")
cat("Total Food Waste:", total_waste, "kg\n")

cat("Overall Waste Percentage:",
    round(overall_waste_percentage, 2), "%\n")


# STEP 5: FOOD-WISE ANALYSIS USING A DATA FRAME

food_summary <- aggregate(
  cbind(Prepared_kg,
        Served_kg,
        Consumed_kg,
        Unserved_kg,
        Plate_Waste_kg,
        Total_Waste_kg) ~ Food_Item,
  data = food_data,
  FUN = sum
)

# Calculate food-wise waste percentage
food_summary$Waste_Percentage <-
  (food_summary$Total_Waste_kg /
     food_summary$Prepared_kg) * 100

# Sort foods by total waste, highest first
food_summary <- food_summary[
  order(-food_summary$Total_Waste_kg), ]

rownames(food_summary) <- NULL

cat("\n========================================\n")
cat("           FOOD-WISE ANALYSIS\n")
cat("========================================\n")

print(food_summary)


# STEP 6: DAILY FOOD WASTE ANALYSIS

daily_summary <- aggregate(
  cbind(Prepared_kg,
        Consumed_kg,
        Total_Waste_kg) ~ Date,
  data = food_data,
  FUN = sum
)

daily_summary$Waste_Percentage <-
  (daily_summary$Total_Waste_kg /
     daily_summary$Prepared_kg) * 100

cat("\n========================================\n")
cat("            DAILY WASTE REPORT\n")
cat("========================================\n")

print(daily_summary)


# STEP 7: IDENTIFY THE MOST WASTED FOOD

highest_waste_food <-
  food_summary$Food_Item[1]

highest_waste_amount <-
  food_summary$Total_Waste_kg[1]

cat("\n========================================\n")
cat("          HIGHEST WASTE FOOD\n")
cat("========================================\n")

cat("Food Item:", highest_waste_food, "\n")
cat("Total Waste:", highest_waste_amount, "kg\n")


# STEP 8: IDENTIFY HIGH-WASTE RECORDS

# A waste percentage above 25% is considered high
high_waste_records <- subset(
  food_data,
  Waste_Percentage > 25
)

cat("\n========================================\n")
cat("           HIGH-WASTE RECORDS\n")
cat("========================================\n")

if (nrow(high_waste_records) > 0) {
  print(high_waste_records[
    , c("Date", "Food_Item",
        "Total_Waste_kg", "Waste_Percentage")
  ])
} else {
  cat("No records exceed the waste threshold.\n")
}


# STEP 9: CREATE A LIST OF ANALYSIS RESULTS

# Lists can store different types of R objects together
food_waste_results <- list(
  Original_Data = food_data,
  Food_Summary = food_summary,
  Daily_Summary = daily_summary,
  High_Waste_Records = high_waste_records,
  Total_Prepared_kg = total_prepared,
  Total_Consumed_kg = total_consumed,
  Total_Waste_kg = total_waste,
  Overall_Waste_Percentage =
    overall_waste_percentage,
  Highest_Waste_Food = highest_waste_food
)

cat("\n========================================\n")
cat("          LIST-BASED RESULTS\n")
cat("========================================\n")

cat("\nNames of List Elements:\n")
print(names(food_waste_results))

cat("\nHighest Waste Food from the List:\n")
print(food_waste_results$Highest_Waste_Food)

cat("\nTotal Waste from the List:\n")
print(food_waste_results[["Total_Waste_kg"]])


# STEP 10: CREATE WASTE REDUCTION RECOMMENDATIONS

generate_recommendation <- function(waste_percentage) {
  
  if (waste_percentage > 30) {
    return("Very high waste: Reduce preparation and review demand.")
  } else if (waste_percentage > 20) {
    return("Moderate waste: Improve portion planning.")
  } else {
    return("Relatively low waste: Continue monitoring.")
  }
}

food_summary$Recommendation <- vapply(
  food_summary$Waste_Percentage,
  generate_recommendation,
  character(1)
)

cat("\n========================================\n")
cat("       WASTE REDUCTION SUGGESTIONS\n")
cat("========================================\n")

print(food_summary[
  , c("Food_Item",
      "Waste_Percentage",
      "Recommendation")
])


# STEP 11: FIND THE DAY WITH THE HIGHEST WASTE

highest_waste_day_index <-
  which.max(daily_summary$Total_Waste_kg)

highest_waste_day <-
  daily_summary$Date[highest_waste_day_index]

highest_daily_waste <-
  daily_summary$Total_Waste_kg[highest_waste_day_index]

cat("\n========================================\n")
cat("          DAILY WASTE INSIGHT\n")
cat("========================================\n")

cat("Highest Waste Date:", highest_waste_day, "\n")
cat("Waste on That Date:", highest_daily_waste, "kg\n")


# STEP 12: VISUALIZATION USING BASE R

# Save current graphical settings
old_par <- par(no.readonly = TRUE)

# Display two charts side by side
par(mfrow = c(1, 2))

# Chart 1: Total waste by food item
barplot(
  food_summary$Total_Waste_kg,
  names.arg = food_summary$Food_Item,
  main = "Total Waste by Food",
  xlab = "Food Item",
  ylab = "Waste (kg)",
  las = 2,
  cex.names = 0.8
)

# Chart 2: Daily food waste
plot(
  seq_len(nrow(daily_summary)),
  daily_summary$Total_Waste_kg,
  type = "b",
  xaxt = "n",
  main = "Daily Food Waste",
  xlab = "Date",
  ylab = "Waste (kg)"
)

axis(
  1,
  at = seq_len(nrow(daily_summary)),
  labels = daily_summary$Date
)

# Restore graphical settings
par(old_par)


# STEP 13: EXPORT THE ANALYSIS REPORTS

write.csv(
  food_data,
  "food_waste_detailed_report.csv",
  row.names = FALSE
)

write.csv(
  food_summary,
  "food_waste_food_summary.csv",
  row.names = FALSE
)

write.csv(
  daily_summary,
  "food_waste_daily_summary.csv",
  row.names = FALSE
)

cat("\n========================================\n")
cat("          REPORTS GENERATED\n")
cat("========================================\n")

cat("1. food_waste_detailed_report.csv\n")
cat("2. food_waste_food_summary.csv\n")
cat("3. food_waste_daily_summary.csv\n")


# STEP 14: FINAL PROJECT SUMMARY

cat("\n========================================\n")
cat("           FINAL PROJECT SUMMARY\n")
cat("========================================\n")

cat("Number of Food Records:",
    nrow(food_data), "\n")

cat("Number of Different Food Items:",
    length(unique(food_data$Food_Item)), "\n")

cat("Total Food Prepared:",
    total_prepared, "kg\n")

cat("Total Food Consumed:",
    total_consumed, "kg\n")

cat("Total Food Waste:",
    total_waste, "kg\n")

cat("Overall Waste Percentage:",
    round(overall_waste_percentage, 2), "%\n")

cat("Food with Highest Total Waste:",
    highest_waste_food, "\n")

cat("\nFood Waste Analysis Completed Successfully!\n")
cat("========================================\n")



