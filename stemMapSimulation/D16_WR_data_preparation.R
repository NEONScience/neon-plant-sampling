###	The general strategy for assigning species codes to functional types is described in Jenkins et al. 2003 Forest Science. Basically, tree species are grouped according to the following, listed in order of decreasing importance: (1) phylogenetic relationship, (2) similarity of pseudodata - see Jenkins for explanation, (3) adequate numbers of published equations per species groups, (4) ease of applying equations for species not represented in published literature, (5) adequate diameter range of pseudodata, and (6) similarity of wood specific gravity. For both options of code below, it is necessary to manually assign species codes to a "Jenkins_type", since species codes are not standardized across different datasets. Appendix A in Jenkins etal. 2003 assigns common species to groups; those species not in Appendix A should be assigned based on phylogenetic relationships to species that are assigned to groups.

###	Species codes from Jenkins etal. 2003
# aa    Aspen, alder, cottonwood, willow
# mo    Hard maple, oak, hickory, beech
# mh    Mixed hardwood
# mb    Soft maple, birch
# cl    Cedar, larch
# df    Douglas Fir
# tf    True fir, hemlock
# pi    Pine
# sp    Spruce
# wo    Woodland conifer and softwood



###	Prepare D16 Wind River 12ha stem map dataset that will be used with sampling simulation code.

#	Read in Wind River dataset for preparation
temp.df = read.csv("D16_WR_12ha_stemMap.csv", header=T)
str(temp.df)
'data.frame':	5284 obs. of  5 variables:
 $ stemID : int  1 5 7 8 9 10 11 12 13 16 ...
 $ species: Factor w/ 11 levels "ABAM","ABGR",..: 11 11 11 9 10 11 11 11 11 9 ...
 $ dbh.cm : num  24.5 26 22.5 8.4 126.7 ...
 $ xdist  : num  317 313 310 303 301 ...
 $ ydist  : num  201 205 208 213 213 ...

#	Identify unique species codes in the dataset
species.codes = sort(as.character(unique(temp.df$species)))
species.codes
 [1] "ABAM" "ABGR" "ABPR" "ALRU" "CONU" "PIMO" "PSME" "RHPU" "TABR" "THPL" "TSHE"

#  Metadata describing column headers, codes.

#	ABAM (Pacific silver fir Abies amabilis)
#	ABGR (grand fir A. grandis)
#	ABPR (noble fir A. procera)
#	ALRU (red alder Alnus rubra)
#	CONU (Pacific dogwood Cornus nuttallii)
#	PIMO (western white pine Pinus monticola)
#	PSME (Douglas-fir Pseudotsuga menziesii)
#	RHPU (cascara Rhamnus purshiana)
#	TABR (Pacific yew Taxus brevifolia)
#	THPL (western red-cedar Thuja plicata)
#	TSHE (western hemlock Tsuga heterophylla)
#	DBH: diameter in centimeters at breast height (1.3 m above forest floor). If tree is forked above DBH then it is counted as one stem if forked below DBH then each fork > = 5 cm DBH is counted as a separate stem
#	X: easting distance in meters from plot origin
#	Y: northing distance in meters from plot origin


##	Assign "Jenkins_type" to each stem in the dataset, based on species codes listed above:

#	Remove rows from the dataset with missing values in any of the columns. The "complete.cases" function returns only those rows of the dataframe for which values in all columns ≠ NA
temp.df = temp.df[complete.cases(temp.df),]

#	Create "Jenkins_type" column to identify which Jenkins parameters should be selected for each stem
temp.df$Jenkins_type = NA

#	Assign "Jenkins_type" values based on species codes, with reference to Appendix A in Jenkins et al. 2003 Forest Science. "RHPU" not in Jenkins appendix: assigned to mixed hardwood category.
temp.df[temp.df$species == "ABAM", ][,"Jenkins_type"] = "tf"
temp.df[temp.df$species == "ABGR", ][,"Jenkins_type"] = "tf"
temp.df[temp.df$species == "ABPR", ][,"Jenkins_type"] = "tf"
temp.df[temp.df$species == "ALRU", ][,"Jenkins_type"] = "aa"
temp.df[temp.df$species == "CONU", ][,"Jenkins_type"] = "mh"
temp.df[temp.df$species == "PIMO", ][,"Jenkins_type"] = "pi"
temp.df[temp.df$species == "PSME", ][,"Jenkins_type"] = "pm"
temp.df[temp.df$species == "RHPU", ][,"Jenkins_type"] = "mh"
temp.df[temp.df$species == "TABR", ][,"Jenkins_type"] = "tf"
temp.df[temp.df$species == "THPL", ][,"Jenkins_type"] = "cl"
temp.df[temp.df$species == "TSHE", ][,"Jenkins_type"] = "tf"
temp.df[temp.df$species == "ALRU", ][,"Jenkins_type"] = "aa"

#	Check output for a few random rows to see if it worked properly
temp.df[60:69,]
   stemID species dbh.cm  xdist  ydist Jenkins_type
60     86    TABR   31.8 344.88 234.12           tf
61     87    TSHE   10.5 342.24 243.88           tf
62     88    TABR   31.2 348.10 236.67           tf
63     89    TSHE    6.3 347.82 238.76           tf
64     90    THPL    7.8 347.95 239.76           cl
65     91    PSME  166.5 347.52 199.83           pm
66     92    THPL    9.8 331.52 202.08           cl
67     93    TABR    8.7 340.48 201.54           tf
68     94    TABR   15.1 337.89 201.96           tf
69     96    TSHE   42.0 325.64 208.87           tf

	#==>	Everything fine.


#	Write .csv of prepped dataset, to be used with sampling simulation code.
write.csv(temp.df, file="D16_WR_12ha_stemMap.csv")

