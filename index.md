nmecr provides a collection of 11 algorithms and utility functions for streamlining energy efficiency project impact quantification in buildings. 

The algorithms are:

* [Mean Model](#mean-model)
* [Simple Linear Regression](#simple-linear-regression)
* [Three Parameter Models](#three-parameter-models) 
  * [Three Parameter Cooling Model](#three-parameter-cooling-model) 
  * [Three Parameter Heating Model](#three-parameter-heating-model) 
* [Four Parameter Model](#four-parameter-model) 
* [Five Parameter Model](#five-parameter-model) 
* [Heating Degree Day Model](#heating-degree-day-model) 
* [Cooling Degree Day Model](#cooling-degree-day-model) 
* [Heating and Cooling Degree Day Model](#heating-and-cooling-degree-day-model) 
* [Time-of-Week and Temperature Model](#time-of-week-and-temperature-model) 
* [Time-of-Week Model](#time-of-week-model) 


# Mean Model

The mean model is simplest in the toolkit. The model form is:

```
Energy Consumption ~ 1
```

and the model predictions are simply the average of the observed energy consumption. The mean model is a good benchmarking model and can be used for comparison against more complex models.  

# Simple Linear Regression

The simple linear regression model, as the name suggests, is a regression using one independent variable. When modeling building energy consumption, the most common independent variable is outside-air-temperature (energy systems, such as HVAC, are directly impacted by it). The model form is:

```
Energy Consumption ~ Temperature
```

Additional influential independent variables, such as product quantity, number of occupants, can be added to this algorithm, turning it into a Multiple Linear Regression, as needed.

```
Energy Consumption ~ Temperature + Additional Variables
```

# Three Parameter Models

Three Parameter models are appropriate for modeling building energy use that varies linearly with an independent variable over part of the range and remains constant over the other part. A prime example of this would be using outside air temperature as the independent variable for electricity usage in cooling or gas usage for heating. 


![Three_P](https://github.com/kW-Labs/nmecr/tree/gh-pages/docs/assets/Three_P.PNG)



