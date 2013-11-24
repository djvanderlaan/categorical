


categories <- data.frame(
  code = c(1001, 1002, 2001, 2002, 3001, 3002, 4001, 4002),
  city = c("Amsterdam", "Rotterdam", "Paris", "Nice", "London", "Manchester", 
    "Madrid", "Barcelona"),
  country = c("The Netherlands", "The Netherlands", "France", "France", 
    "England", "England", "Spain", "Spain"),
  capital = c("capital", "no capital", "capital", "no capital", "capital", 
    "no capital", "capital", "no capital"))

place_of_residence <- sample(categories$code, 20, replace=TRUE)

place_of_residence <- categorical(place_of_residence, categories)

place_of_residence

level(place_of_residence) <- "country"
place_of_residence
table(place_of_residence)

# it also works in data.frames
data <- data.frame(place_of_residence, income=rlnorm(length(place_of_residence)))
data

level(data$place_of_residence) <- "city"
data

# add a level
new_level <- data.frame(
  country=c("The Netherlands", "France", "England", "Spain"),
  region2=c("North", "South", "North", "South"))
place_of_residence <- add_level(place_of_residence, new_level)
place_of_residence

level(place_of_residence) <- "region2"
table(place_of_residence)

# Indexing works as expected
place_of_residence[1:3]
place_of_residence[c(TRUE, FALSE)]

# For assignment the original codes are needed
#place_of_residence[3] <- 3001
