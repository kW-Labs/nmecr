nmecr provides a collection of 11 algorithms and 5 main utility functions for streamlining impact quantification for energy efficiency projects in buildings. 

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
* [Time-of-Week Model](#time-of-week-model) 
* [Time-of-Week and Temperature Model](#time-of-week-and-temperature-model) 

Additional influential independent variables, such as product quantity, number of occupants etc., can be added to all algorithms except the mean model to improve model accuracy over a dataset.

# Mean Model

The mean model is simplest in the toolkit. The model form is:

![](https://user-images.githubusercontent.com/30964555/135516727-7761a61d-1b1d-4583-af4b-d11428d8fdf0.gif)

and the model predictions are simply the average of the observed energy consumption. The mean model is a good benchmarking model and can be used for comparison against more complex models.  

# Change-point Models

In modeling building energy consumption, one of the most common regressor (or feature or independent variable) is the outdoor air temperature. Many of the building's core energy systems, such as HVAC, are directly impacted by it. Change-point models are a class of models that capture the trends of energy consumption over various segments of the temperature profile. These models differ mainly in their derived temperature features. The figure below illustrates the difference between the various flavors of these models.

![](https://user-images.githubusercontent.com/30964555/135517973-c6a322cd-8937-4005-888a-76a073bbbed2.gif)

<font size="1"> [Image Source](https://www.sciencedirect.com/science/article/abs/pii/S0378778814009645) </font> 

Note that all model forms below reference this figure and its naming convention.

## Two-Parameter Model

A two-parameter model (or a simple linear regression), is a regression using one independent variable, and its model form is:

![](https://user-images.githubusercontent.com/30964555/135515611-be82bb65-2010-49f5-b6ee-74222f9bb5ff.gif)

## Three Parameter Cooling and Heating Models

Three Parameter models are appropriate for modeling building energy use that varies linearly with an independent variable over part of the range and remains constant over the other part (see the second panel in the image above).

The model forms are:

![](https://user-images.githubusercontent.com/30964555/135522767-880c5f43-6883-4241-8e3b-b86f24381a90.gif)

![](https://user-images.githubusercontent.com/30964555/135522805-6ca2beec-cfc9-4634-948d-44d97aebf31b.gif)

The + and - above indicate that the values of the terms in the parenthesis will be set to zero when they are negative and positive respectively.

#### When should use these models?

Imagine you are modeling a system that predominantly delivers cooling to a building. Up until a certain temperature, the building does not need cooling and this system stays dormant. However, above this temperature, known as a change-point in the industry, the system has to start cooling the building and use energy in the process. In a three-parameter cooling model, the energy use of the system is 'modeled' to vary linearly with an *increase* in outdoor air temperature above the identified change-point.  

The three-parameter heating model is used in the opposite scenario, where the energy use starts to increase linearly with a *decrease* in outdoor air temperature.

## Four Parameter Model

Four parameter models extend the three parameter models by replacing the initial flat-sloped segment with a positive or negative-sloped segment depending on the type of model: cooling vs heating. The third panel in the image above shows two examples of Four Parameter models. The model form is:

![](https://user-images.githubusercontent.com/30964555/135523748-b0288af0-82db-410d-a780-1a154247d434.gif)

As before, the + and - above indicate that the values of the terms in the parenthesis will be set to zero when they are negative and positive respectively.

#### When should you use this model?

These models are appropriate for modeling heating and cooling energy use in variable-air-volume systems. 

## Five Parameter Model

The model form for a five parameter model is:

![](https://user-images.githubusercontent.com/30964555/135523843-49a2a469-6e1c-408a-a0d9-8a60da058a6c.gif)

The 5P algorithm requires an initial set of change-points to determine the optimum changepoints. This can be set via the **initial_breakpoints** argument in `nmecr::assign_model_inputs()`. 

#### When should you use this model?

Five parameter models are appropriate for modeling whole-building electricity consumption from buildings with electric heat pumps or both electric chillers and electric resistance heating. 

# Degree Day Models

## Heating Degree Day Model

## Cooling Degree Day Model

## Heating and Cooling Degree Day Model

## Time-of-Week and Temperature Model

The Time-of-Week and Temperature Model in nmecr is based on the [original paper](https://buildings.lbl.gov/publications/quantifying-changes-building) from Lawrence Berkeley National Laboratory (LBNL) and their [implementation](https://github.com/LBNL-ETA/RMV2.0) of the algorithm.

The two main components of the model are:

* a time-of-week indicator variable, and
* a piece-wise linear and continuous outdoor air temperature dependence


#### Original Algorithm

The original algorithm has the following key features:

1. A week is divided into 15-minute intervals, i.e. 672 time-of-week variables

2. Outdoor air temperature is divided into six equally-sized temperature intervals. For example, if a facility experienced a temperature profile ranging from 50F to 110F, the temperature segments would be 50-60F, 60-70F, 70-80F, 70-90F, 90-100F, and 100-110F. The paper recommends using at least twice the expected number of change-points.

3. The temperature parameters are only used when a facility is operating in occupied mode

4. The facility's response to temperature is not expected to change at night. Essentially, this translates to six temperature segments in the occupied mode and 1 temperature segment (that includes all data) in the unoccupied mode.

5. The start and end of the occupied mode are manually determined by visualizing average load profiles on non-DR days. 


#### Changes in the implementation ([RMV2.0](https://github.com/LBNL-ETA/RMV2.0))

The following are the main clarifications/additions in LBNL's implementation of the algorithm:

1. Occupied and unoccupied modes are based on a regression of load on outside air temperature. Times of the week that the regression usually underpredicts the load are occupied and the rest are called unoccupied. More specifically, the times of the week that are underpredicted 65% of the time are assumed to be in the occupied mode.

2. The algorithm checks to ensure there are at least 20 data points above the highest temperature change-point and 20 data points below the lowest change-point. The algorithm requires at least 1 change-point to run.

3. The temperature segment width is determined by the user-requested number of segments. Each temperature segment is equal in width.

4. The same temperature segments are applied to both occupied and unoccupied modes

5. In case of a lack of temperature data, the algorithm can be run as a Time-of-Week (see below)

6. Both TOWT and TOW can be run on 15-min, hourly, and daily intervals  


#### Further changes implemented in nmecr 

1. The occupied and unoccupied mode determination is based on a user input, accessed through the formal argument **occupancy_threshold** within `nmecr::assign_model_inputs()`. The default value is set to 0.65 to match the RMV2.0 implementation. In certain scenarios, the split into occupied and unoccupied may not be required or desired. In these cases, the occupancy_threshold   can be set to 1 to create one overall model. A practical application of this is the case when occupancy information is available as   an additional variable. The TOWT model can then be run by setting occupancy_threshold = 1 and the additional variable as a proxy for  occupancy.  

2. Temperature change-points can be manually specified when needed. This behavior can be accessed through **has_temp_knots_defined** and **temp_knots_value** within `nmecr::assign_model_inputs()`. If has_temp_knots_defines is set to False (F), the algorithm determines the change-points internally. On the other hand, if this argument is set to True (T), it looks to temp_knots_value for the specified change-points. Note that temperature change-points are referred to as temperature knots in the code.

3. The temperature segments can either be of equal width or have equal number of data points in them. This behavior is accessed through **equal_temp_segment_points** within `nmecr::assign_model_inputs()`. If set to True (T), the algorithm determines the temperature segments such that each has equal number of data points in it. If set to False (F), the segments are determined such that they are equal in width.

The model forms for TOWT are:

![](https://user-images.githubusercontent.com/30964555/135546747-630e5289-b7b6-4516-9aed-44f00a15055d.png)

![](https://user-images.githubusercontent.com/30964555/135546776-ad2734a9-014b-4f01-aac9-c0ef55a484ad.png)

Note that the actual model forms depend on a number of factors, and the actual model form may be different than the ones shown above. 

Notice that there is no (is_Fri) term or (is_Sun) term in the above eqauations. This is because the average energy use on these days is captured by the intercept the respective equations.

The is_* term implies that the value can be either a 0 or a 1. For example, is_Mon will be set to 1 when the timestamp is a Monday to 0 for all other days of the week.

# Time-of-Week Model

The time-of-week model is a simplification of the time-of-week and temperature model. It is a linear model with indicator variables for each 'time-of-week'. In a model based on daily interval data, there are 7 indicator variables, for each day of the week. Similarly, in a model based on hourly interval data, there are 168 indicator variables, for each hour of the week. The model form, therefore, is:

##### Daily Interval

![](https://user-images.githubusercontent.com/30964555/135546500-4f5fb3e4-8531-4db3-b04b-fe4a13456cfd.png)

Notice that there is no (is_Sun) term in the above equation. The average energy use for Sundays is captured by the intercept term (C) in the above equation.

##### Hourly Interval

![](https://user-images.githubusercontent.com/30964555/135546534-04403e97-dfd1-4e84-a4b5-a019153bf8ef.png)

Similarly, there is no (is_tow168) term in the above equation.


