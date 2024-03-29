# PHP2560-Shiny-Final-Project
Working on the final project for PHP2560
SARS is an infectious disease that spreads via droplets spread through contact with an infected individual (Zhou et al., 2004). This disease was identified in the early 2000s and due to its high infectivity was able to spread quickly and internationally (Zhou et al., 2004). Through drastic public health measures the spread of the disease was controled, but peaked mathematical modler's interest (Zhou et al., 2004). In order to have an idea of how this disease was spreading the authors created a mathematical model that allows for them to predict the number of indviduals that contracted SARS over a given amount of days (Zhou et al., 2004). In their model they have a multitude of parameters that they estimate based on their given data, to predict the number of indviduals diagnosed with SARS on a given day based on the starting valuesand parameter values (Zhou et al., 2004). The authors utilize their own data to estimate the model parameters, but we find that this model could be beneficial for other similar infectious diseases. Thus we have created this app which allows for public health officials to input parameters that were estimated based on their own data to predict the number of sick, exposed or infected indviduals after t days. 


How our app is set up:
We have three tabs in our app, the first is a home page that explains what the app is doing, a parameters tab that allows for the user to set the model parameters, and Results tab that outputs the results of the simulation. 



What is this app doing tab: Gives the overview of the motivations behind the development of this model and why we are creating this application. This tab will also include an interactive version of the model where when you click on the box it tells you what group this is (see drawing also submitted)



Parameters tab: This tab has a dropdown that allows the user to select if they are at the start of an epidemic, or the middle.  If the user is at the start of the pandemic the user must input the population size, the number of exposed and infected indviduals. If the official is starting in the middle of the epidemic then the user must input number quarantined indviduals, and number of recovered as well. In addition to this reactive input we also have sliders that will be the input into the model. The user should have estimates for their transmission rate per day (beta), death rate (delta), recovery rate (gamma), transfer rate exposed -> infected (epsilon), transfer rate exposed -> quarintined (lambda), transfer rate infective -> diagnosed (theta), the infectivity fraction (k) (Zhou et al., 2004). The user must also speficy how long they would like to run the simulation (days). 

Results tab: This will render a plot based on the inputs that the user selects on the parameters tab, the default plot will give a curve of the number of diagnosed indviduals per day over the number of days. But we also include the option of allowing the user to display curves for the other classes (exposed, quarantined, diagnosed and recovered) based on their need. 


In our app we are struggling with the randomness element as the model has fixed parameters, any ideas?





Citation 
Zhou, Y., Ma, Z., & Brauer, F. (2004). A discrete epidemic model for SARS transmission and control in China. Mathematical and Computer Modelling, 40(13), 1491–         1506. https://doi.org/10.1016/j.mcm.2005.01.007
