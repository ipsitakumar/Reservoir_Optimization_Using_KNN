# Reservoir Optimization Using KNN
The present model permits the optimization of reservoirs management under uncertain climate described by an ensemble of streamflow forecast. The illustration provided here is focused on the State of Pernambuco, Brazil, and comprises the 5 reservoirs and water trucks to provide water to 19 municipalities. 

The challenge is formulated as a linear optimization problem focused on the optimzation of costs and where failure is represented through a penalty. 

## How to run the model
The model is written in R and requires the installation of lpSolve package to perform the optimization.
One only need to run the main script in the src folder, which will produce analysis plots stored in the results folder. 
