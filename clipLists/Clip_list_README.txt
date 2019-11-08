Herbaceous clip list README

Clip lists are provided as one .csv file per unique plot or subplot ID, in the following folder structure:

[siteID]_[plotType]_clipList/

where:
siteID = the 4-letter siteID
plotType = "distributed" or "tower"


--For Distributed plots:
* The first 8 characters of the .csv file name correspond to the unique plotID, and there is one .csv file per plot.
* Clip Lists are NOT provided for Distributed plots with nlcdClass = deciduousForest, evergreenForest, or mixedForest
* If all Distributed plots at a site have nlcdClass = deciduousForest, evergreenForest, or mixedForest, there will be no Clip Lists provided.
* For all other NLCD classes (e.g. woodyWetlands), technicians must visually assess the plot for % herbaceous cover. If herbaceous cover is ≥ 50%, the plot should be clip harvested; if herbaceous cover is < 50%, do not perform herbaceous clip harvest. Percent herbaceous cover should be assessed from an airborne remote-sensing perspective - i.e. herbaceous cover that exists underneath a woody overstory does not count.
* A maximum of twenty (n=20) Distributed plots are harvested per bout per site; there may be more than 20 Clip Lists in the folder, so consult guidance provided by NEON Science Operations to determine which plots to harvest in this situation.


--For Tower plots (20m x 20m):
* Files are named similarly to Distributed plots.


--For Tower plots (40m x 40m):
* The first 8 characters of the .csv file name correspond to the unique plotID, and the two digits that follow the plotID indicate the subplotID. 
* Herbaceous clip harvest is performed in all Tower plots, regardless of nlcdClass.
* Two subplots are randomly selected for clip-harvest; Clip Lists are only provided for the two subplots that have been randomly selected.