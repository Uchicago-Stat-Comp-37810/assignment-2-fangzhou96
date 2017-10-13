#let the variable 'cars' equal to 100
cars = 100
# let the variable 'space_in_a_car' equal to 4.0
space_in_a_car = 4.0
# let the variable 'drivers' equal to 4.0
drivers = 30
# let the variable 'passengers' equal to 4.0
passengers = 90
# Let the variable 'cars_not_driven' equal to cars minus drivers, which equals 70
cars_not_driven = cars - drivers
# Set the variable 'cars_driven' equal the number of drivers
cars_driven = drivers
# Let the variable 'carpool_capacity' equal to cars_driven minus space_in_a_car, which equals 120
carpool_capacity = cars_driven * space_in_a_car
# Let the variable 'average_passengers_per_car' equal to passengers / cars_driven, which equals 3.0
average_passengers_per_car = passengers / cars_driven

#show the summary output of data above:
print("There are", cars, "cars available.")
print("There are only", drivers, "drivers available.")
print("There will be", cars_not_driven, "empty cars today.")
print("We can transport", carpool_capacity, "people today.")
print("We have", passengers, "to carpool today.")
print("We need to put about", average_passengers_per_car,
      "in each car.")

#Since we have not defined the variable 'car_pool_capacity', so python has no idea what car_pool_capacity is, therefore we could not use this variable to calculate, which comes out error.
#It is not necessary to use 4.0 for space_in_a_car, since the floating number here is same as an integer.
