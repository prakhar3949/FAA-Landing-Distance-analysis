# FAA-Landing-Distance-analysis
Federal Aviation Administration(FAA) public data to create an equation to predict the landing distance of a flight 

predicting-flight-landing-distance
Federal Aviation Administration case study: Analysis of 950 commercial flights to draw insights on what factors affect the flight landing distance. This was done through EDA, PCA, variable selection, linear regression and logistic regression.

Also, a predictive model (using Linear Regression/Logistic Regression) was built to predict landing distance so that risk of landing overrun can be reduced.

Detailed report of analysis and findings
Refer to detail report file in this repository.

Tools used
R Studio

Datasets used
FAA.csv datasets uploaded in this repository.

Variables analyzed
Aircraft: The make of an aircraft (Boeing or Airbus).
Duration (in minutes): Flight duration between taking off and landing. The duration of a normal flight should always be greater than 40min.
No_pasg: The number of passengers in a flight.
Speed_ground (in miles per hour): The ground speed of an aircraft when passing over the threshold of the runway. If its value is less than 30MPH or greater than 140MPH, then the landing would be considered as abnormal.
Speed_air (in miles per hour): The air speed of an aircraft when passing over the threshold of the runway. If its value is less than 30MPH or greater than 140MPH, then the landing would be considered as abnormal.
Height (in meters): The height of an aircraft when it is passing over the threshold of the runway. The landing aircraft is required to be at least 6 meters high at the threshold of the runway
Pitch (in degrees): Pitch angle of an aircraft when it is passing over the threshold of the runway.
Distance (in feet): The landing distance of an aircraft. More specifically, it refers to the distance between the threshold of the runway and the point where the aircraft can be fully stopped. The length of the airport runway is typically less than 6000 feet.

