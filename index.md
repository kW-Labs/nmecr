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

![](https://user-images.githubusercontent.com/30964555/135516727-7761a61d-1b1d-4583-af4b-d11428d8fdf0.gif)

and the model predictions are simply the average of the observed energy consumption. The mean model is a good benchmarking model and can be used for comparison against more complex models.  

# Change-point Models

In modeling building energy consumption, one of the most common regressor (or feature or independent variable) is the outside-air-temperature. Many of the building's core energy systems, such as HVAC, are directly impacted by it. Change-point models are a class of models that capture the trends of energy consumption over various segments of the temperature profile. These models differ mainly in their derived temperature features. The figure below illustrates the difference between the various flavors of these models.

![](https://user-images.githubusercontent.com/30964555/135517973-c6a322cd-8937-4005-888a-76a073bbbed2.gif)

<font size="1"> [Image Source](https://www.sciencedirect.com/science/article/abs/pii/S0378778814009645) </font> 

Note that all model forms below reference this figure and its naming convention.

## Two-Parameter Model

A two-parameter model (or a simple linear regression), is a regression using one independent variable, and its model form is:

![](https://user-images.githubusercontent.com/30964555/135515611-be82bb65-2010-49f5-b6ee-74222f9bb5ff.gif)

Additional influential independent variables, such as product quantity, number of occupants etc., can be added to this algorithm, turning it into a Multiple Linear Regression.

![](https://user-images.githubusercontent.com/30964555/135515586-8ac66679-59aa-4dea-9637-9415e0bff363.gif)


## Three Parameter Cooling and Heating Models

Three Parameter models are appropriate for modeling building energy use that varies linearly with an independent variable over part of the range and remains constant over the other part (see the second panel in the image above).

The model forms are:

![](https://user-images.githubusercontent.com/30964555/135522767-880c5f43-6883-4241-8e3b-b86f24381a90.gif)

![](https://user-images.githubusercontent.com/30964555/135522805-6ca2beec-cfc9-4634-948d-44d97aebf31b.gif)

The + and - above indicate that the values of the terms in the parenthesis will be set to zero when they are negative and positive respectively.

#### When might these models be appropriate?

Imagine you are modeling a system that predominantly delivers cooling to a building. Up until a certain temperature, the building does not need cooling and this system stays dormant. However, above this temperature, known as a change-point in the industry, the system has to start cooling the building and use energy in the process. In a three-parameter cooling model, the energy use of the system is 'modeled' to vary linearly with an *increase* in outside-air-temperature above the identified change-point.  

The three-parameter heating model is used in the opposite scenario, where the energy use starts to increase linearly with a *decrease* in outside-air-temperature.

## Four Parameter Model

Four parameter models extend the three parameter models by replacing the initial flat-sloped segment with a positive or negative-sloped segment depending on the type of model: cooling vs heating. The third panel in the image above shows two examples of Four Parameter models. The model form is:

![](https://user-images.githubusercontent.com/30964555/135523748-b0288af0-82db-410d-a780-1a154247d434.gif)

As before, the + and - above indicate that the values of the terms in the parenthesis will be set to zero when they are negative and positive respectively.

#### When might these models be appropriate?

These models are appropriate for modeling heating and cooling energy use in variable-air-volume systems. 

## Five Parameter Model

The model form for a five parameter model is:

![](https://user-images.githubusercontent.com/30964555/135523843-49a2a469-6e1c-408a-a0d9-8a60da058a6c.gif)

#### When might these models be appropriate?

Five parameter models are appropriate for modeling whole-building electricity consumption from buildings with electric heat pumps or both electric chillers and electric resistance heating. 

