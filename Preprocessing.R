# LOAD THE NECESSARY LIBRARIES
library(dplyr) # For data manipulation and pre-processing
library(ggplot2) # For data visualization
library(readr) # For data collection

# DATA COLLECTION
car_1 <- readr::read_csv("used_cars_data//car1.csv")
car_2 <- readr::read_csv("used_cars_data//car2.csv")
car_3 <- readr::read_csv("used_cars_data//car3.csv")
car_4 <- readr::read_csv("used_cars_data//car4.csv")


# PREPROCESSING
# RE-ORDER & RE-NAME THE COLUMNS 

# Re-Order CAR 1
    colnames(car_1)
    car_1 <- car_1[, c("name", "year", "km_driven", "fuel", "seller_type", "transmission", "owner", "selling_price")]
# Re-Name CAR 1
    colnames(car_1) <- c("Name", "Year_mfd", "km_drv", "fuel_type", "seller", "transmission", "no_own", "price")

# Re-Order CAR 2
    colnames(car_2)
    car_2 <- car_2[, c("Car_Name", "Year", "Kms_Driven", "Fuel_Type", "Seller_Type", "Transmission", "Owner", "Selling_Price")]
# Re-Name CAR 2
    colnames(car_2) <- c("Name", "Year_mfd", "km_drv", "fuel_type", "seller", "transmission", "no_own", "price")
# Re-Code CAR 2
      recodeCar_2 <- car_2 %>% # Modify the 'no_own' column by reassigning values based on conditions
        mutate(
          no_own = case_when(
            no_own == '0' ~ "First Owner",
            no_own == '1' ~ "Second Owner",
            no_own == '2' ~ "Third Owner",
            no_own == '3' ~ "Fourth & Above Owner",
            TRUE ~ as.character(no_own) # If none of the above conditions match, keep the original value
            )
        )
  car_2 <- recodeCar_2
# Converting Selling_Price to thousands for CAR 2
  car_2$price <- as.numeric(round(car_2$price * 10000))

# Re-Order CAR 3
    colnames(car_3)
    car_3 <- car_3[, c("name", "year", "km_driven", "fuel", "seller_type", "transmission", "owner", "selling_price")]
# Re-Name CAR 3
    colnames(car_3) <- c("Name", "Year_mfd", "km_drv", "fuel_type", "seller", "transmission", "no_own", "price")

#Combining two columns together in CAR 4
    car_4 <- car_4 %>%
      mutate(
        Make = as.character(Make),  # Ensure Make is a character
        Model = as.character(Model),  # Ensure Model is a character
        Name = paste(Make, Model, sep = " ")  # Combine Make and Model
      )
    
    # Check if the new Name column is created
    head(car_4$Name)
    
    
# Re-Order Car 4
    colnames(car_4)
    car_4 <- car_4[, c("Name", "Year", "Kilometer", "Fuel Type","Seller Type", "Transmission", "Owner","Price")]
# Re-Name CAR 4
    colnames(car_4) <- c("Name", "Year_mfd", "km_drv", "fuel_type", "seller", "transmission", "no_own", "price")
# Re-Code CAR 4
    recodecar_4 <- car_4 %>% 
      mutate(
        no_own = case_when(
          no_own == "First" ~ "First Owner",
          no_own == "Second" ~ "Second Owner",
          no_own == "Third" ~ "Third Owner",
          no_own == "Fourth" ~ "Fourth & above Owner",
          no_own == "4 or More" ~ "Fourth & above Owner",
          TRUE ~ as.character(no_own)
        )
      )
  car_4 <- recodecar_4

    
# Merge all datasets into one
    all_cars <- bind_rows(car_1, car_2, car_3, car_4)
    
# Check the first few rows of the merged dataset
    head(all_cars)
    
# Check for missing values
    colSums(is.na(all_cars))
    
# Check data structure
    str(all_cars)

# Machine learning models work best with numerical data, so convert categorical columns to factors:
    all_cars <- all_cars %>%
      mutate(
        fuel_type = as.factor(fuel_type),
        seller = as.factor(seller),
        transmission = as.factor(transmission),
        no_own = as.factor(no_own)
      )
    write.csv(all_cars, "all_cars.csv", row.names = FALSE)
    
    
    