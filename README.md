# neon-plant-sampling
Scripts supporting NEON plant biomass and productivity sampling.

Folders contain the following:

* cdw_lidsAngleLists: Function for generating random per plot azimuths for LIDS sampling of Coarse Downed Wood. Also contains .csv output from function on a per domain basis.
* clipLists: Function for generating random per plot Clip Lists that describe 0.5m x 3m "cells" in which Herbaceous Biomass clip harvests take place. These "cells" are also used for Belowground Biomass Core sampling for fine roots, Bryophyte Productivity, and Litterfall and Fine Woody Debris sampling.
* stemMapSimulation: A function that determines the uncertainty in estimated forest biomass associated with a given number of plots at a given size, using a spatially explicit stem map as input data.
* soils_bgcAndMicrobes: Code for randomly selecting soil microbe and biogeochemistry sampling locations within plots.
* vst_towerPlotSubsample: Code for randomly selecting two 20m x 20m subplots for sampling when Tower plot size is 40m x 40m