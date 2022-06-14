# **Simulation results**

This folder contains the latest simulation results I've generated.

I have more paths generated that I haven't had time to run the simulation on yet, hence some gaps in the datapoints. I ran everything I could to show us the main trends of what's going on at speeds between 0.02 and 0.95m/s.

___
___

## **Plots**

There are two types of plots: single-speed analysis and multi-speed analysis plots.

### **Single-speed plots**

These involve using the results of simulations using one given speed parameter.

They all show the same trend of oversampling faster speeds.

There are four plots, grouped into two groups:

**1. Raw speed plots**

These are labelled 'sp0.02.png, sp0.04.png' etc.

These use results from ONE RUN ONLY of that simulation

The top plot shows the distributions of realised and observed speeds

The bottom plot shows the distributions of realised and observed speeds overlayed with the speed parameter, mean realised speed (MRS), mean observed speed (MOS), and estimated speed using four different methods (harmonic mean, lognormal, gamma, and Weibull)

**2. Error plots**

These are labelled 'sp0.02_errors.png, sp0.04_errors.png' etc.

These pool together results from 50 repeats of the same simulation

The top plot shows the errors between each observed speed and the mean realised speed for that simulation run (observed speed minus MRS)

The bottom plot shows the errors between each estimated speed and the mean realised speed for that simulation run (estimated speed minus MRS)

___

### **Multi-speed plots**

I find this one the most helpful. There are three plots arranged into one png called 'multi_sp0.02-0.95.png'. I have also provided each of these plots separately, with the first called 'obs_multi_sp0.02-0.95.png', the second 'est_multi_sp0.02-0.95.png', and the third 'added_multi_sp0.02-0.95.png'.

**1. Mean errors between observed speeds and MRS**

Each point is calculated by averaging the errors between observed speeds and MRS for a given simulation run (because there are multiple observed speeds for each simulation) (errors calculated by subtracting MRS from each observed speed)

This shows us the error in what the camera picks up relative to what the animal is actually doing

**2. Errors between estimated speeds and MRS**

Each point is the error between the estimated speed (calculated using one of the four methods) and the MRS for that simulation run (estimated speed minus MRS)

This shows us the error in the average speed we calculate using these methods relative to the true average speed of the animal

**3. Sum of the above two types of error**

Each point is calculated by summing the mean error between observed speeds and MRS with the error between estimated speed and MRS for that simulation run

This shows us how well our models are compensating for the biases in the speeds we pick up using camera traps

The rationale behind this is as follows:

1. Errors between observed speeds and MRS are positive (oversampling faster movements) - it looks like I maybe haven't simulated high enough speeds to show the second bias of higher speeds being missed...
2. Errors between estimated speeds and MRS are negative at low speeds (models are overcorrecting for the bias of oversampling fast speeds) then positive at slightly higher speeds (models not correcting for the bias enough as the bias increases with speed)
3. Therefore, when we add together the errors between MRS and observed speeds and errors between MRS and estimated speeds:
    * If the models perfectly correct for the biases of what the camera trap picks up, we would expect this graph to show a flat line at y=0
    * When the line is above 0, this shows that the models are not correcting the biases enough
    * When the line is below 0, this shows that the models are over-correcting the biases


___
___

## **Speed SD issue**

I spent a bit of time discussing the speed SD issue with Francis and James. The conclusion was that I should have been providing a logged speed standard deviation that is appropriately scaled to the input logged speed parameter.

We came up with this solution:

1. I pooled together all the camera trap data I have (Regent's Park, Panama, and India) into large and small animals (size threshold = 4kg)
2. For each species, I extracted the mean and standard deviation of the normal distribution fitted to the logged speed values of sequences carried out by that species
3. I calculated a coefficient of variation (CV) for each species:
    * there are two ways to do this: 
        - either calculate the log CV: CV_log = sd_log/mean_log (where sd_log and mean_log are the mean and sd calculated in step 2)
        - or calculate some kind of pseudo-geometric CV: CV = exp(sd_log)/exp(mean_log) (so this is the same as above except using the exponents of the mean and sd calculated in step 2)
    * choosing the best type of CV: I selected CV_log (the first option) because it behaved the most consistently across species when grouped by large and small animals
    * I then took the mean of all the CVs in each size category to generate one CV for small animals and one CV for large animals
4. I used these CVs (one for large animals and one for small animals) to work out the appropriate speed SD for each given speed parameter using the simple relationship: CV_log = sd_log / mean_log (i.e. given the logged input mean speed and CV_log for that size category of animals, I can work out the logged input speed SD required)
