# Predicting-flight-landing-distance
Federal Aviation Administration case study: 
Analysis of 950 commercial flights to draw insights on what factors affect the flight landing distance. This was done through EDA, PCA, variable selection, various regression methods and Data visualization.

Also, a predictive model (using Linear Regression and Logistic Regression) was built to predict landing distance so that risk of landing overrun can be reduced. 

# Detailed report of analysis and findings
Refer to detail report file in this repository.

## Tools Used: 
R-studio

## Datasets used
FAA1.csv and FAA2.csv datasets uploaded in this repository.


## Variables analyzed
* **Aircraft**: The make of an aircraft (Boeing or Airbus).
* **Duration** (in minutes): Flight duration between taking off and landing. The duration of a normal flight should always be greater than 40min.
* **No_pasg**: The number of passengers in a flight.
* **Speed_ground** (in miles per hour): The ground speed of an aircraft when passing over the threshold of the runway. If its value is less than 30MPH or greater than 140MPH, then the landing would be considered as abnormal.
* **Speed_air** (in miles per hour): The air speed of an aircraft when passing over the threshold of the runway. If its value is less than 30MPH or greater than 140MPH, then the landing would be considered as abnormal.
* **Height** (in meters): The height of an aircraft when it is passing over the threshold of the runway. The landing aircraft is required to be at least 6 meters high at the threshold of the runway
* **Pitch** (in degrees): Pitch angle of an aircraft when it is passing over the threshold of the runway.
* **Distance** (in feet): The landing distance of an aircraft. More specifically, it refers to the distance between the threshold of the runway and the point where the aircraft can be fully stopped. The length of the airport runway is typically less than 6000 feet.
### *Binary Response created to use Logistic Regression*
* **Long.Landing** (in feet):long.landing = 1 if Distance > 2500; =0 otherwise
* **risky.Landing** (in feet):risky.landing = 1 if distance > 3000; =0 otherwise.

