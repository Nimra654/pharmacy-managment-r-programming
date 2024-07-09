library(lubridate)

# Initialize pharmacy data frame
pharmacy_data <- data.frame(
  BATCH_NO = c(157346, 286659, 546588, 384281, 247982, 640463, 364643, 394469, 619742, 147540, 158619,
               603878, 589184, 186347, 417194, 431992, 164468, 195166, 485405, 300928),
  NAME = c("Dolo650", "aspirin", "ibuprofen", "zyrtec", "prilosec", "glucophage", "cipro", "retin-A", "lotrimin", "basitracin",
           "bactroban", "cortisone cream", "delsyn", "mecinex", "sudafud", "venadrayl", "sevin", "propanlol", "flushot", "tetanus vaccine"),
  USES = c("fever and pain", "pain reliever", "headache", "itching", "stomach ulcers", "blood sugar levels", "bacterial infection", "acne", 
           "ringworm", "wound healing", "bacterial infection", "swealling", "cough", "thin mucus", "nasal congestion", "insect bites", 
           "insect bites", "high bp", "influenza infections", "tetanus infection"),
  MFT_DATE = as.Date(c("2023-10-23", "2023-08-06", "2023-02-10", "2023-07-25", "2023-05-05", "2022-09-06", "2023-10-04", "2023-06-03", 
                       "2023-05-11", "2023-08-09", "2023-04-14", "2023-02-24", "2022-03-21", "2022-10-20", "2023-07-01", "2022-08-03", 
                       "2022-04-07", "2023-12-27", "2022-09-27", "2023-03-08")),
  EXP_DATE = as.Date(c("2024-02-06", "2025-09-10", "2024-10-09", "2025-07-06", "2024-11-09", "2025-07-08", "2025-09-09",
                       "2024-09-07", "2025-12-09", "2025-05-04", "2024-10-09", "2025-07-09", "2025-06-04", "2025-08-07", "2025-06-05", 
                       "2025-04-02", "2024-07-12", "2024-08-24", "2025-12-09", "2025-08-15")),
  QUANTITY = c(200, 178, 230, 100, 213, 229, 223, 165, 276, 108, 210, 250, 190, 186, 212, 216, 206, 287, 223, 189),
  PRICE_PEICE = c(12, 13, 14, 15, 16, 23, 43, 54, 16, 17, 23, 45, 54, 13, 23, 24, 24, 12, 14, 14)
)

# Initialize workers data frame
workers_data <- data.frame(WORKER_ID = integer(), NAME = character(), SALARY = numeric())

# Load workers_data from a file
load_workers_data <- function() {
  if (file.exists("workers_data.rds")) {
    workers_data <<- readRDS("workers_data.rds")
    
  } else {
    cat("No saved worker data found.\n")
  }
}

# Save workers_data to a file
save_workers_data <- function() {
  saveRDS(workers_data, "workers_data.rds")
  cat("Worker data saved successfully.\n")
}
# Load worker data at the beginning
load_workers_data()

# Initialize electricity data frame with sample data
electricity_data <- data.frame(
  MONTH = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
  YEAR = rep(as.numeric(format(Sys.Date() - months(1:12), "%Y")), each = 1),
  AMOUNT = c(150, 160, 140, 170, 180, 130, 200, 190, 210, 180, 160, 170)
)

# Function to add worker
add_worker <- function() {
  new_worker <- data.frame(
    WORKER_ID = as.integer(readline(prompt = "Enter worker ID: ")),
    NAME = as.character(readline(prompt = "Enter worker name:")),
    SALARY = as.numeric(readline(prompt = "Enter worker salary:"))
  )
  
  workers_data <<- rbind(workers_data, new_worker)
  cat("Worker added successfully.\n")
  
  # Save workers_data after adding a worker
  save_workers_data()
}

# Function to display workers
display_workers <- function() {
  if (nrow(workers_data) == 0) {
    cat("No worker data available.\n")
  } else {
    cat("Workers:\n")
    print(workers_data)
  }
}

# Function to add electricity data
add_electricity <- function() {
  month <- as.character(readline(prompt = "Enter the month (MMM): "))
  amount <- as.numeric(readline(prompt = "Enter the electricity amount: "))
  year <- as.numeric(format(Sys.Date() - months(1), "%Y"))  # Capture the previous month's year
  
  new_electricity <- data.frame(
    MONTH = month,
    YEAR = year,
    AMOUNT = amount
  )
  
  electricity_data <<- rbind(electricity_data, new_electricity)
  cat("Electricity data added successfully.\n")
}

# Function to display electricity data for the past year
display_electricity <- function() {
  current_year <- as.numeric(format(Sys.Date(), "%Y"))
  
  if (nrow(electricity_data) == 0) {
    cat("No electricity data available.\n")
  } else {
    cat("Electricity data for the year:\n")
    print(electricity_data[electricity_data$YEAR == current_year, ])
  }
}

# Function to add drugs to the pharmacy_data with checks for manufacturing date and negative values
pms <- function() {
  N <- as.integer(readline(prompt = "Enter number of Drugs to add: "))
  for (i in 1:N) {
    new_drug <- data.frame(
      BATCH_NO = as.integer(readline(prompt = "Enter batch number: ")),
      NAME = as.character(readline(prompt = "Enter Drug Name: ")),
      USES = as.character(readline("Enter Drug uses: ")),
      MFT_DATE = as.Date(readline(prompt = "Enter manufacture Date (YYYY-MM-DD): ")),
      EXP_DATE = as.Date(readline(prompt = "Enter Expiry Date (YYYY-MM-DD): ")),
      QUANTITY = as.integer(readline(prompt = "Enter quantity: ")),
      PRICE_PEICE = as.integer(readline(prompt = "Enter Price/peice: "))
    )
    
    # Check if manufacturing date is after expiry date
    if (new_drug$MFT_DATE > new_drug$EXP_DATE) {
      cat("Error: Manufacturing date cannot be after the expiry date.\n")
      return()
    }
    
    # Check if any data is given in negative value
    if (any(new_drug[c("QUANTITY", "PRICE_PEICE")] < 0)) {
      cat("Error: Quantity and Price/peice cannot be negative.\n")
      return()
    }
    
    pharmacy_data <<- rbind(pharmacy_data, new_drug)
  }
  cat("Current data list:\n")
  print(pharmacy_data)
}

# Function to display pharmacy_data
disp <- function() {
  print(pharmacy_data)
}

# Function to read data from CSV file
reading <- function() {
  tf <- tempfile(fileext = ".csv")
  write.csv(pharmacy_data, file = tf)
  cat("Completed exporting data.\n")
  read.csv(tf)
  file.show(tf)
}

# Function to display a bar graph of quantity available
dispg <- function() {
  barplot(height = pharmacy_data$QUANTITY, names.arg = pharmacy_data$NAME, xlab = "Drug Name", ylab = "Quantity Sold", col = "skyblue", main = "Quantity Sold for Each Drug")
}

# Function to check low stock of quantity (less than 10)
check_low_stock <- function() {
  low_stock_drugs <- pharmacy_data$NAME[pharmacy_data$QUANTITY < 10]
  
  if (length(low_stock_drugs) > 0) {
    cat("Low stock alert!\n")
    cat("The following drugs have quantity less than 10:\n")
    cat(paste("*", low_stock_drugs, "\n", sep = "", collapse = ""))
  } else {
    cat("No drugs have low stock.\n")
  }
}

# Function to sell drugs and update pharmacy_data
sell_drug <- function() {
  drug_name <- readline(prompt = "Enter the name of the drug to sell: ")
  quantity_needed <- as.integer(readline(prompt = "Enter the quantity needed: "))
  discount_percentage <- as.numeric(readline(prompt = "Enter discount percentage: "))
  
  # Check if the discount is negative
  if (discount_percentage < 0) {
    cat("Invalid discount specified. Please enter a non-negative discount.\n")
    return()
  }
  
  # Find the index of the drug in the dataframe
  drug_index <- which(pharmacy_data$NAME == drug_name)
  
  # Check if the drug exists in the dataframe
  if (length(drug_index) == 0) {
    cat("The drug '", drug_name, "' does not exist in the inventory.\n", sep = "")
    return()
  }
  
  # Check if the required quantity is non-negative and less than or equal to the available quantity
  if (quantity_needed < 0) {
    cat("Invalid quantity specified. Please enter a non-negative quantity.\n")
    return()
  }
  
  if (quantity_needed > pharmacy_data$QUANTITY[drug_index]) {
    cat("Insufficient quantity of '", drug_name, "' in the inventory.\n", sep = "")
    return()
  }
  
  # Check if the drug has expired
  if (pharmacy_data$EXP_DATE[drug_index] < Sys.Date()) {
    cat("Cannot sell '", drug_name, "' as it has expired.\n", sep = "")
    return()
  }
  
  # Calculate total price before discount
  total_price_before_discount <- pharmacy_data$PRICE_PEICE[drug_index] * quantity_needed
  
  # Calculate total price after discount
  total_price_after_discount <- total_price_before_discount - (total_price_before_discount * (discount_percentage / 100))
  
  # Update the quantity after selling
  pharmacy_data$QUANTITY[drug_index] <- pharmacy_data$QUANTITY[drug_index] - quantity_needed
  
  cat("Successfully sold ", quantity_needed, " units of '", drug_name, "'.\n", sep = "")
  cat("Remaining quantity of '", drug_name, "': ", pharmacy_data$QUANTITY[drug_index], "\n", sep = "")
  cat("Total price before discount: ", total_price_before_discount, "/-\n")
  cat("Total price after discount (", discount_percentage, "%): ", total_price_after_discount, "/-\n")
  
  # Update the global pharmacy_data
  assign("pharmacy_data", pharmacy_data, envir = .GlobalEnv)
}


# Function to search for a drug and its uses
search_drug <- function() {
  drug_name <- readline(prompt = "Enter the name of the drug to search: ")
  
  # Find the drug in the dataframe
  drug_info <- pharmacy_data[pharmacy_data$NAME == drug_name, c("NAME", "USES")]
  
  # Check if the drug exists in the dataframe
  if (nrow(drug_info) == 0) {
    cat("The drug '", drug_name, "' does not exist in the inventory.\n", sep = "")
  } else {
    cat("Drug Name: ", drug_info$NAME, "\n")
    cat("Uses: ", drug_info$USES, "\n")
  }
}

# Function to check expiry dates of drugs
check_expiry_dates <- function() {
  expired_drugs <- pharmacy_data[pharmacy_data$EXP_DATE < Sys.Date(), ]
  
  if (nrow(expired_drugs) > 0) {
    cat("Expired drugs:\n")
    print(expired_drugs)
  } else {
    cat("No drugs have expired.\n")
  }
}

# Function to delete a drug from pharmacy_data
delete_drug <- function() {
  drug_name <- readline(prompt = "Enter the name of the drug to delete: ")
  
  # Find the index of the drug in the dataframe
  drug_index <- which(pharmacy_data$NAME == drug_name)
  
  # Check if the drug exists in the dataframe
  if (length(drug_index) == 0) {
    cat("The drug '", drug_name, "' does not exist in the inventory.\n", sep = "")
    return()
  }
  
  # Confirm deletion
  confirmation <- readline(prompt = paste("Are you sure you want to delete", drug_name, "? (yes/no): "))
  if (tolower(confirmation) == "yes") {
    # Remove the drug from pharmacy_data
    pharmacy_data <<- pharmacy_data[-drug_index, ]
    cat("Drug '", drug_name, "' has been deleted from the inventory.\n", sep = "")
  } else {
    cat("Deletion of drug '", drug_name, "' cancelled.\n", sep = "")
  }
}

# Function to compare which drug sold the most
compare_drug_sales <- function() {
  # Find the drug with the highest quantity sold
  max_quantity <- min(pharmacy_data$QUANTITY)
  top_drugs <- pharmacy_data$NAME[pharmacy_data$QUANTITY == max_quantity]
  
  cat("The drug(s) that sold the most:\n")
  cat("Quantity Sold: ", max_quantity, "\n")
  cat("Drug Name(s): ", paste(top_drugs, collapse = ", "), "\n")
}

# Menu loop with added options
menu_choice <- 0
while (menu_choice != 15) {
  cat("Pharmacy Management System\n")
  cat("1. Add Drug\n")
  cat("2. Bar graph of quantity available\n")
  cat("3. Read from CSV file\n")
  cat("4. Display data\n")
  cat("5. Sell Drug\n")
  cat("6. Check low stock\n")
  cat("7. Search Drug\n")
  cat("8. Check Expiry Dates\n")
  cat("9. Delete Drug\n")
  cat("10. Drug Comparison\n")
  cat("11. Add Worker\n")
  cat("12. Display Workers\n")
  cat("13. Add Electricity Data\n")
  cat("14. Display Electricity Data\n")
  cat("15. Exit\n")
  
  menu_choice <- as.numeric(readline("Enter your choice (1-15): "))
  switch(menu_choice,
         "1" = pms(),
         "2" = dispg(),
         "3" = reading(),
         "4" = disp(),
         "5" = sell_drug(),
         "6" = check_low_stock(),
         "7" = search_drug(),
         "8" = check_expiry_dates(),
         "9" = delete_drug(),
         "10" = compare_drug_sales(),
         "11" = add_worker(),
         "12" = display_workers(),
         "13" = add_electricity(),
         "14" = display_electricity(),
         "15" = save_workers_data(),
         cat("Invalid choice. Please enter a number between 1 and 15.\n"))
}