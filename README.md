# neon-plant-sampling
Scripts supporting NEON plant biomass and productivity sampling.

Folders contain the following:

* **EAB_quarantine**: Script identifying plots that contain _Fraxinus spp._ across the Observatory. Litter collection initially suspended in those plots that contain _Fraxinus_.
* **bbcProtocolDev**: T-test analysis to determine whether mixing dilution samples via plunging alone is sufficient compared to vortexing first and then plunging.
* **bryophyte_sampling**: Analysis to determine percent cover of bryophytes by plot to inform whether BRY protocol should be implemented.
* **cdw_tallyAnalysis**: Script that uses CDW tally data to identify decayClass x sizeCategory x taxonID combinations that are most abundant and should be sampled for CDW bulk density.
* **cdw_transectLengthAnalysis**: Script that uses VST stemDiameter data to determine appropriate F-value and transect lengths by site.
* **clipLists**: Function for generating random per plot sampling cells that describe 0.5m x 3m areas in which Herbaceous Biomass clip harvests take place. These "cells" are also used for Plant Belowground Biomass sampling for fine roots, and Litterfall and Fine Woody Debris sampling.
* **dhpExifChecksFops**: Rmd that checks DHP Exif data to determine number of photos that meet ISO, shutterspeed, and aperture specifications in the protocol.
* **hbpSampling**: Mixed effects analysis of experimental clip harvest data to determine percent of sample at grazed sites for non-peak biomass bouts that can be sorted.
* **lidsLists**: Function for generating random per plot azimuths for LIDS sampling of Coarse Downed Wood. Also contains .csv output from function on a per domain basis.
* **photosynthesis_pathways**: Analysis of taxa by domain supporting sorting to C3 vs C4 functional groups.
* **plant_tools**: Assorted scripts and functions for working with NEON plant biomass and productivity data products.
* **spatialData**: Spatial data required for plant biomass and productivity functions or analyses. Previously housed in devTOS and not maintained here long-term.
* **stemMapSimulation**: A function that determines the uncertainty in estimated forest biomass associated with a given number of plots at a given size, using a spatially explicit stem map as input data.
* **stem_utms**: Script to calculate location of mapped stems within the plot, in UTMs.
* **towerSubplotLists**: Script and output identifying random Tower subplots in which plant biomass and productivity sampling occurs. Applicable to 40m x 40m Tower Plots with tall stature vegetation.
* **vst_arcadisDataCheck**: QC report of Arcadis Vegetation Characterization data.
* **vst_cspDataPrep**: Scripts to prepare Vegetation Characterization data for delivery to CSP for rank abundance and VST sample size analyses.
* **vst_towerPlotSubsample**: Code output for identifying 5 lowest MO plots for annual Tower Plot sampling.
