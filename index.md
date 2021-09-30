nmecr provides a collection of 11 algorithms and 5 main utility functions for streamlining energy efficiency project impact quantification in buildings. 

These are:

* [Mean Model](#mean-model)
* [Change-point models](#change-point-models)
  * [Two Parameter Model](#two-parameter-model)
  * [Three Parameter Cooling and Heating Models](#three-parameter-cooling-and-heating-models) 
  * [Four Parameter Model](#four-parameter-model) 
  * [Five Parameter Model](#five-parameter-model) 
* [Degree Day Models](#degree-day-models) 
  * [Heating Degree Day Model](#heating-degree-day-model) 
  * [Cooling Degree Day Model](#cooling-degree-day-model) 
  * [Heating and Cooling Degree Day Model](#heating-and-cooling-degree-day-model) 
* [Time-of-Week and Temperature Model](#time-of-week-and-temperature-model) 
* [Time-of-Week Model](#time-of-week-model) 


# Mean Model

The mean model is simplest in the toolkit. The model form is:

![](https://user-images.githubusercontent.com/30964555/135513577-abc0d117-40e2-4270-94bf-7829bbd2599d.gif)

and the model predictions are simply the average of the observed energy consumption. The mean model is a good benchmarking model and can be used for comparison against more complex models.  

# Change-point Models

In modeling building energy consumption, one of the most common regressor (or feature or independent variable) is the outside-air-temperature. Many of the building's core energy systems, such as HVAC, are directly impacted by it. Change-point models are a class of models that capture the trends of energy consumption over various segments of the temperature profile. These models differ mainly in their derived temperature features. The figure below illustrates the difference between the various flavors of these models.

![](https://user-images.githubusercontent.com/30964555/135507651-01e03e78-8400-4ae0-9c4d-2af2d8f9a8c8.gif)

<font size="1"> [Image Source](https://www.sciencedirect.com/science/article/abs/pii/S0378778814009645) </font> 

Note that all model forms below reference this figure and its naming convention.

## Two-Parameter Model

A two-parameter model (or a simple linear regression), is a regression using one independent variable, and its model form is:

![](https://user-images.githubusercontent.com/30964555/135513724-34987cd8-77bf-4ac2-8283-003233eff65c.gif)

Additional influential independent variables, such as product quantity, number of occupants etc., can be added to this algorithm, turning it into a Multiple Linear Regression.

![](https://user-images.githubusercontent.com/30964555/135513748-86cb04e7-3647-4842-b0a6-9bd6a65a7c03.gif)

## Three Parameter Cooling and Heating Models

Three Parameter models are appropriate for modeling building energy use that varies linearly with an independent variable over part of the range and remains constant over the other part (see the second panel in the image above).

The model forms are:

![](https://user-images.githubusercontent.com/30964555/135513777-6306b37f-8f04-411a-a0c6-827214d4f1f0.gif)

![](https://user-images.githubusercontent.com/30964555/135513810-4dbeb77d-5965-4d47-9e09-76029d43bda2.gif)

The $()^+$ and $()^-$ indicate that the values of the terms in the paranthesis will be set to zero when they are negative and positive respectively.


