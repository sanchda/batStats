# Import the data
physData = read.csv("EpomophorusPhysiology_2015.csv");


# First look at linear correlation between the variables
plot( physData$BMI )
plot( physData$BMI[ physData$class == "M_NS"] )
