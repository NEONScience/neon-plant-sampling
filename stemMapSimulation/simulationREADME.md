samplingSimulation
==================

Sampling simulation work for optimizing size and number of NEON Tower plots

File                            |Description
--------------------------------|--------------------------------------------------------------------------
D16_WR_stemMap.csv              | Input stem map used for the sampling simulation
D16_WR_data_preparation         | Code to add categorical variable used to classify stems to functional type
Jenkins_parameters.csv          | Table of parameters used to estimate biomass per functional type
samplingSimulation_stemMap.R    | R function that performs plot-based sampling simulation on stem map

*Note:* The R "sampSurf" package by Jeffrey Gove appears to have functionality that may be easy to appropriate for the sampling simulation goals identified in our current contract.
