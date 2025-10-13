### D498 - R Programming / Udacity - R Programming

#### Purpose
In this project, I will make use of R to explore data related to bike share systems for three major cities in the United States—Chicago, New York City, and Washington. R-script code will be written to import the CSV data and answer 3 questions about the bike rental service in all 3 cities by computing descriptive statistics and making visualizations. A table and graph will be provided to answer all 3 questions listed below.

#### Resources
From Udacity's course on R-Programming, I will use the 3 CSV files within the R-script for the project:
<ul>
<li>new-york-city.csv</li>
<li>chicago.csv</li>
<li>washington.csv</li>
</ul>

#### Questions:
<ol>
<li>What is the most common month for bike trips in each city?</li>
<li>What is the average travel time for different users in different cities?</li>
<li>What is the most common trip (start -> end) per city?</li>
</ol>

#### Answers:
<ol>
<li>What is the most common month for bike trips in each city?: For each city, the most common month in which there were the most bike trips were all in June. 

- In Chicago, 32.7% of all trips taken on Citibike were in June.
- In New York, 25.3% of all trips taken on Citibike were in June.
- In Washington, 22.8% of all trips taken on Citibike were in June.

For each city, there were over 62,500 trips taken in June. Therefore, this data can be used to prepare for maintenance costs for each bike and allotting enough bike stations for the month of June.</li>

<li>What is the average travel time for different users in different cities?: The average travel time in minutes for each city depending on the customer type are as follows:
Chicago:
    Subscriber: 11.7 minutes
    Dependent: 5.18 minutes
    Customer: 31 minutes
New York:
    Subscriber: 12.8 minutes
    Customer: 34.3 minutes
    Unidentified Customer Type: 24.5 minutes
Washington:
    Subscriber: 12.3 minutes
    Customer: 43.9 minutes
</li>

<li>What is the most common trip (start -> end) per city?: 
The most common bike trips per city are as follows:

- Chicago: Lake Shore Dr & Monroe St -> Streeter Dr & Grand Ave at 854 trips
- New York: E 7 St & Avenue A -> Cooper Square & E 7 St at 168 trips
- Washington: Jefferson Dr & 14th St SW -> Jefferson Dr & 14th St SW at 673 trips

</li>

</ol>

#### Files In Repo
<ul>
<li>D498-Bike_Rental.R</li>
<li>D498-Bike_Rental.html</li>
</ul>