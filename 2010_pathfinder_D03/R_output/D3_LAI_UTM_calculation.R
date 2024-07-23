Determine UTM Zone 17 WGS 1984 coordinates at which LAI measurements were made.


## Use relative coordinates specifying position along the LAI transect to calculate the actual UTM coordinates at which LAI measurements were taken:

	==>	Use the "rotate" function written for this purpose; file = "Rotate_coordinates.R"
	
	# Can use the function multiple times to create dataframes for points associated with the last known GPS location along the transect.

	# For example:
	
	newOS01.0 = rotate(OS01.0, 299.75, OS01E0, OS01N0)
	newOS01.50 = rotate(OS01.50, 301.35, OS01E50, OS01N50)


	# Can then use write.csv() to store multiple outputs in a .txt file:
		
	newOS01 = rbind(newOS01.0, newOS01.50)
	write.csv(newOS01, file="newOS01.csv")
	
	
	

## Import UTM coordinates for points every 50 m along each transect. UTM coordinates were copied and pasted from Access database table.

OS01.0 = c(403835.042, 3285052.236)
OS01.50 = c(403876.802, 3285080.384)
OS01.100 = c(403919.409, 3285104.023)
OS01.150 = c(403963.862, 3285129.408)
OS01.200 = c(404007.08, 3285154.891)
OS01.250 = c(404050.904, 3285178.932)
OS01.300 = c(404092.671, 3285206.203)
OS01.350 = c(404135.617, 3285231.616)
OS01.400 = c(404178.335, 3285258.187)
OS01.450 = c(404223.664, 3285280.455)
OS01.500 = c(404266.96, 3285304.032)

OS02.0 = c(403900.214, 3284937.286)
OS02.50 = c(403943.619, 3284962.665)
OS02.100 = c(403988.136, 3284987.414)
OS02.150 = c(404031.139, 3285012.468)
OS02.200 = c(404074.268, 3285037.633)
OS02.250 = c(404117.495, 3285063.399)
OS02.300 = c(404160.766, 3285087.548)
OS02.350 = c(404204.892, 3285112.922)
OS02.400 = c(404247.783, 3285137.515)
OS02.450 = c(404290.54, 3285162.468)
OS02.500 = c(404334.447, 3285187.793)

OS03.0 = c(403968.152, 3284822.2)
OS03.50 = c(404011.07, 3284847.042)
OS03.100 = c(404054.588, 3284871.486)
OS03.150 = c(404097.893, 3284896.487)
OS03.200 = c(404140.814, 3284921.624)
OS03.250 = c(404183.889, 3284947.118)
OS03.300 = c(404227.228, 3284971.906)
OS03.350 = c(404270.117, 3284997.208)
OS03.400 = c(404313.465, 3285021.6)
OS03.450 = c(404356.738, 3285046.923)
OS03.500 = c(404400, 3285071.464)

OS04.0 = c(404035.963, 3284706.611)
OS04.50 = c(404078.589, 3284732.538)
OS04.100 = c(404121.565, 3284757.824)
OS04.150 = c(404165.422, 3284782.649)
OS04.200 = c(404209.480, 3284805.099)
OS04.250 = c(404254.632, 3284827.643)
OS04.300 = c(404299.371, 3284849.215)
OS04.350 = c(404344.796, 3284871.743)
OS04.400 = c(404387.011, 3284897.179)
OS04.450 = c(404431.890, 3284919.856)
OS04.500 = c(404475.127, 3284945.370)

OS08.0 = c(404705.415, 3284791.698)
OS08.50 = c(404710.864, 3284841.083)
OS08.100 = c(404715.664, 3284890.567)
OS08.150 = c(404719.931, 3284942.273)
OS08.200 = c(404724.344, 3284990.914)
OS08.250 = c(404728.840, 3285041.147)
OS08.300 = c(404731.517, 3285090.616)
OS08.350 = c(404736.750, 3285140.867)
OS08.400 = c(404742.577, 3285190.490)
OS08.450 = c(404749.782, 3285239.289)
OS08.500 = c(404752.611, 3285289.182)

OS09.0 = c(404860.004, 3284747.693)
OS09.50 = c(404862.651, 3284796.712)
OS09.100 = c(404869.380, 3284846.061)
OS09.150 = c(404874.110, 3284896.070)
OS09.200 = c(404879.245, 3284945.981)
OS09.250 = c(404882.719, 3284995.881)
OS09.300 = c(404889.442, 3285045.178)
OS09.350 = c(404895.369, 3285094.701)
OS09.400 = c(404899.871, 3285145.817)
OS09.450 = c(404905.536, 3285194.709)
OS09.500 = c(404910.038, 3285243.891)

OS11.0 = c(403929.912, 3284704.791)
OS11.50 = c(403891.678, 3284737.264)
OS11.100 = c(403853.774, 3284768.981)
OS11.150 = c(403815.474, 3284801.096)
OS11.200 = c(403776.982, 3284833.133)
OS11.250 = c(403738.653, 3284864.561)
OS11.300 = c(403702.324, 3284895.960)
OS11.350 = c(403663.715, 3284928.741)
OS11.400 = c(403624.601, 3284960.643)
OS11.450 = c(403586.704, 3284992.510)
OS11.500 = c(403548.644, 3285024.878)

OS12.0 = c(403732.464, 3284611.681)
OS12.50 = c(403694.335, 3284644.401)
OS12.100 = c(403657.302, 3284676.096)
OS12.150 = c(403618.923, 3284707.435)
OS12.200 = c(403580.801, 3284739.440)
OS12.250 = c(403542.110, 3284772.140)
OS12.300 = c(403504.215, 3284803.814)
OS12.350 = c(403466.663, 3284835.343)
OS12.400 = c(403427.935, 3284868.160)
OS12.450 = c(403390.151, 3284899.295)
OS12.500 = c(403352.190, 3284931.243)



## Calculate angles "theta" for every 50 m along each transect for use with "rotate" function.

OS01
dOS01.0   = 360 - deg(atan((OS01.50[1]-OS01.0[1])/(OS01.50[2]-OS01.0[2]))) 			
dOS01.50  = 360 - deg(atan((OS01.100[1]-OS01.50[1])/(OS01.100[2]-OS01.50[2]))) 		
dOS01.100 = 360 - deg(atan((OS01.150[1]-OS01.100[1])/(OS01.150[2]-OS01.100[2])))	
dOS01.150 = 360 - deg(atan((OS01.200[1]-OS01.150[1])/(OS01.200[2]-OS01.150[2])))	
dOS01.200 = 360 - deg(atan((OS01.250[1]-OS01.200[1])/(OS01.250[2]-OS01.200[2])))	
dOS01.250 = 360 - deg(atan((OS01.300[1]-OS01.250[1])/(OS01.300[2]-OS01.250[2])))	
dOS01.300 = 360 - deg(atan((OS01.350[1]-OS01.300[1])/(OS01.350[2]-OS01.300[2])))	
dOS01.350 = 360 - deg(atan((OS01.400[1]-OS01.350[1])/(OS01.400[2]-OS01.350[2])))	
dOS01.400 = 360 - deg(atan((OS01.450[1]-OS01.400[1])/(OS01.450[2]-OS01.400[2])))	
dOS01.450 = 360 - deg(atan((OS01.500[1]-OS01.450[1])/(OS01.500[2]-OS01.450[2])))	

dOS01.0  	= 303.9816
dOS01.50 	= 299.0222
dOS01.100	= 299.7286
dOS01.150	= 300.5252
dOS01.200	= 298.7483	
dOS01.250	= 303.1418
dOS01.300	= 300.6146
dOS01.350	= 301.8820
dOS01.400	= 296.1627
dOS01.450	= 298.5707


OS02
dOS02.0   = 360 - deg(atan((OS02.50[1]-OS02.0[1])/(OS02.50[2]-OS02.0[2]))) 		
dOS02.50  = 360 - deg(atan((OS02.100[1]-OS02.50[1])/(OS02.100[2]-OS02.50[2]))) 	
dOS02.100 = 360 - deg(atan((OS02.150[1]-OS02.100[1])/(OS02.150[2]-OS02.100[2])))
dOS02.150 = 360 - deg(atan((OS02.200[1]-OS02.150[1])/(OS02.200[2]-OS02.150[2])))
dOS02.200 = 360 - deg(atan((OS02.250[1]-OS02.200[1])/(OS02.250[2]-OS02.200[2])))
dOS02.250 = 360 - deg(atan((OS02.300[1]-OS02.250[1])/(OS02.300[2]-OS02.250[2])))
dOS02.300 = 360 - deg(atan((OS02.350[1]-OS02.300[1])/(OS02.350[2]-OS02.300[2])))
dOS02.350 = 360 - deg(atan((OS02.400[1]-OS02.350[1])/(OS02.400[2]-OS02.350[2])))
dOS02.400 = 360 - deg(atan((OS02.450[1]-OS02.400[1])/(OS02.450[2]-OS02.400[2])))
dOS02.450 = 360 - deg(atan((OS02.500[1]-OS02.450[1])/(OS02.500[2]-OS02.450[2])))

dOS02.0 	= 300.3149
dOS02.50 	= 299.0716
dOS02.100 	= 300.2255
dOS02.150 	= 300.2628
dOS02.200 	= 300.7976
dOS02.250 	= 299.1653
dOS02.300 	= 299.9004
dOS02.350 	= 299.8293
dOS02.400 	= 300.2679
dOS02.450 	= 299.9758


OS03
dOS03.0   = 360 - deg(atan((OS03.50[1]-OS03.0[1])/(OS03.50[2]-OS03.0[2]))) 		
dOS03.50  = 360 - deg(atan((OS03.100[1]-OS03.50[1])/(OS03.100[2]-OS03.50[2]))) 	
dOS03.100 = 360 - deg(atan((OS03.150[1]-OS03.100[1])/(OS03.150[2]-OS03.100[2])))
dOS03.150 = 360 - deg(atan((OS03.200[1]-OS03.150[1])/(OS03.200[2]-OS03.150[2])))
dOS03.200 = 360 - deg(atan((OS03.250[1]-OS03.200[1])/(OS03.250[2]-OS03.200[2])))
dOS03.250 = 360 - deg(atan((OS03.300[1]-OS03.250[1])/(OS03.300[2]-OS03.250[2])))
dOS03.300 = 360 - deg(atan((OS03.350[1]-OS03.300[1])/(OS03.350[2]-OS03.300[2])))
dOS03.350 = 360 - deg(atan((OS03.400[1]-OS03.350[1])/(OS03.400[2]-OS03.350[2])))
dOS03.400 = 360 - deg(atan((OS03.450[1]-OS03.400[1])/(OS03.450[2]-OS03.400[2])))
dOS03.450 = 360 - deg(atan((OS03.500[1]-OS03.450[1])/(OS03.500[2]-OS03.450[2])))

dOS03.0		= 300.0633
dOS03.50 	= 299.3229
dOS03.100	= 299.9989
dOS03.150	= 300.3557
dOS03.200	= 300.6192
dOS03.250	= 299.7677
dOS03.300	= 300.5381
dOS03.350	= 299.3665
dOS03.400	= 300.3358
dOS03.450	= 299.5647


OS04
dOS04.0   = 360 - deg(atan((OS04.50[1]-OS04.0[1])/(OS04.50[2]-OS04.0[2]))) 		
dOS04.50  = 360 - deg(atan((OS04.100[1]-OS04.50[1])/(OS04.100[2]-OS04.50[2]))) 	
dOS04.100 = 360 - deg(atan((OS04.150[1]-OS04.100[1])/(OS04.150[2]-OS04.100[2])))
dOS04.150 = 360 - deg(atan((OS04.200[1]-OS04.150[1])/(OS04.200[2]-OS04.150[2])))
dOS04.200 = 360 - deg(atan((OS04.250[1]-OS04.200[1])/(OS04.250[2]-OS04.200[2])))
dOS04.250 = 360 - deg(atan((OS04.300[1]-OS04.250[1])/(OS04.300[2]-OS04.250[2])))
dOS04.300 = 360 - deg(atan((OS04.350[1]-OS04.300[1])/(OS04.350[2]-OS04.300[2])))
dOS04.350 = 360 - deg(atan((OS04.400[1]-OS04.350[1])/(OS04.400[2]-OS04.350[2])))
dOS04.400 = 360 - deg(atan((OS04.450[1]-OS04.400[1])/(OS04.450[2]-OS04.400[2])))
dOS04.450 = 360 - deg(atan((OS04.500[1]-OS04.450[1])/(OS04.500[2]-OS04.450[2])))

dOS04.0 	= 301.3098
dOS04.50 	= 300.4715
dOS04.100	= 299.5118
dOS04.150	= 297.0014
dOS04.200	= 296.5326
dOS04.250	= 295.7422
dOS04.300	= 296.3786
dOS04.350	= 301.0704
dOS04.400	= 296.8071
dOS04.450	= 300.5447


OS08
dOS08.0   = 360 - deg(atan((OS08.50[1]-OS08.0[1])/(OS08.50[2]-OS08.0[2]))) 		
dOS08.50  = 360 - deg(atan((OS08.100[1]-OS08.50[1])/(OS08.100[2]-OS08.50[2]))) 	
dOS08.100 = 360 - deg(atan((OS08.150[1]-OS08.100[1])/(OS08.150[2]-OS08.100[2])))
dOS08.150 = 360 - deg(atan((OS08.200[1]-OS08.150[1])/(OS08.200[2]-OS08.150[2])))
dOS08.200 = 360 - deg(atan((OS08.250[1]-OS08.200[1])/(OS08.250[2]-OS08.200[2])))
dOS08.250 = 360 - deg(atan((OS08.300[1]-OS08.250[1])/(OS08.300[2]-OS08.250[2])))
dOS08.300 = 360 - deg(atan((OS08.350[1]-OS08.300[1])/(OS08.350[2]-OS08.300[2])))
dOS08.350 = 360 - deg(atan((OS08.400[1]-OS08.350[1])/(OS08.400[2]-OS08.350[2])))
dOS08.400 = 360 - deg(atan((OS08.450[1]-OS08.400[1])/(OS08.450[2]-OS08.400[2])))
dOS08.450 = 360 - deg(atan((OS08.500[1]-OS08.450[1])/(OS08.500[2]-OS08.450[2])))

dOS08.0		= 353.7036
dOS08.50 	= 354.4596
dOS08.100	= 355.2824
dOS08.150	= 354.816
dOS08.200	= 354.8855
dOS08.250	= 356.9025
dOS08.300	= 354.0548
dOS08.350	= 353.3027
dOS08.400	= 351.6012
dOS08.450	= 356.7547


OS09
dOS09.0   = 360 - deg(atan((OS09.50[1]-OS09.0[1])/(OS09.50[2]-OS09.0[2]))) 		
dOS09.50  = 360 - deg(atan((OS09.100[1]-OS09.50[1])/(OS09.100[2]-OS09.50[2]))) 	
dOS09.100 = 360 - deg(atan((OS09.150[1]-OS09.100[1])/(OS09.150[2]-OS09.100[2])))
dOS09.150 = 360 - deg(atan((OS09.200[1]-OS09.150[1])/(OS09.200[2]-OS09.150[2])))
dOS09.200 = 360 - deg(atan((OS09.250[1]-OS09.200[1])/(OS09.250[2]-OS09.200[2])))
dOS09.250 = 360 - deg(atan((OS09.300[1]-OS09.250[1])/(OS09.300[2]-OS09.250[2])))
dOS09.300 = 360 - deg(atan((OS09.350[1]-OS09.300[1])/(OS09.350[2]-OS09.300[2])))
dOS09.350 = 360 - deg(atan((OS09.400[1]-OS09.350[1])/(OS09.400[2]-OS09.350[2])))
dOS09.400 = 360 - deg(atan((OS09.450[1]-OS09.400[1])/(OS09.450[2]-OS09.400[2])))
dOS09.450 = 360 - deg(atan((OS09.500[1]-OS09.450[1])/(OS09.500[2]-OS09.450[2])))

dOS09.0		= 356.9091
dOS09.50 	= 352.2353
dOS09.100	= 354.5969
dOS09.150	= 354.1259
dOS09.200	= 356.0175
dOS09.250	= 352.2341
dOS09.300	= 353.1752
dOS09.350	= 354.9667
dOS09.400	= 353.3907
dOS09.450	= 354.7699


OS11
dOS11.0   = abs(deg(atan((OS11.50[1]-OS11.0[1])/(OS11.50[2]-OS11.0[2])))) 		
dOS11.50  = abs(deg(atan((OS11.100[1]-OS11.50[1])/(OS11.100[2]-OS11.50[2])))) 	
dOS11.100 = abs(deg(atan((OS11.150[1]-OS11.100[1])/(OS11.150[2]-OS11.100[2]))))
dOS11.150 = abs(deg(atan((OS11.200[1]-OS11.150[1])/(OS11.200[2]-OS11.150[2]))))
dOS11.200 = abs(deg(atan((OS11.250[1]-OS11.200[1])/(OS11.250[2]-OS11.200[2]))))
dOS11.250 = abs(deg(atan((OS11.300[1]-OS11.250[1])/(OS11.300[2]-OS11.250[2]))))
dOS11.300 = abs(deg(atan((OS11.350[1]-OS11.300[1])/(OS11.350[2]-OS11.300[2]))))
dOS11.350 = abs(deg(atan((OS11.400[1]-OS11.350[1])/(OS11.400[2]-OS11.350[2]))))
dOS11.400 = abs(deg(atan((OS11.450[1]-OS11.400[1])/(OS11.450[2]-OS11.400[2]))))
dOS11.450 = abs(deg(atan((OS11.500[1]-OS11.450[1])/(OS11.500[2]-OS11.450[2]))))

dOS11.0 	= 49.6580
dOS11.50 	= 50.0783
dOS11.100	= 50.0197
dOS11.150	= 50.2292
dOS11.200	= 50.6498
dOS11.250	= 49.1632
dOS11.300	= 49.6670
dOS11.350	= 50.7987
dOS11.400	= 49.9400
dOS11.450	= 49.6206


OS12
dOS12.0   = abs(deg(atan((OS12.50[1]-OS12.0[1])/(OS12.50[2]-OS12.0[2])))) 		
dOS12.50  = abs(deg(atan((OS12.100[1]-OS12.50[1])/(OS12.100[2]-OS12.50[2])))) 	
dOS12.100 = abs(deg(atan((OS12.150[1]-OS12.100[1])/(OS12.150[2]-OS12.100[2]))))
dOS12.150 = abs(deg(atan((OS12.200[1]-OS12.150[1])/(OS12.200[2]-OS12.150[2]))))
dOS12.200 = abs(deg(atan((OS12.250[1]-OS12.200[1])/(OS12.250[2]-OS12.200[2]))))
dOS12.250 = abs(deg(atan((OS12.300[1]-OS12.250[1])/(OS12.300[2]-OS12.250[2]))))
dOS12.300 = abs(deg(atan((OS12.350[1]-OS12.300[1])/(OS12.350[2]-OS12.300[2]))))
dOS12.350 = abs(deg(atan((OS12.400[1]-OS12.350[1])/(OS12.400[2]-OS12.350[2]))))
dOS12.400 = abs(deg(atan((OS12.450[1]-OS12.400[1])/(OS12.450[2]-OS12.400[2]))))
dOS12.450 = abs(deg(atan((OS12.500[1]-OS12.450[1])/(OS12.500[2]-OS12.450[2]))))

dOS12.0		= 49.3658
dOS12.50 	= 49.4411
dOS12.100	= 50.7660
dOS12.150	= 49.9851
dOS12.200	= 49.7969
dOS12.250	= 50.1099
dOS12.300	= 49.9828
dOS12.350	= 49.7230
dOS12.400	= 50.5105
dOS12.450	= 49.9160


## Create a Y-axis vector with points every 10 m for each triangle to be rotated

dist = seq(from=0, to=40, by=10)
dist
[1]  0 10 20 30 40

## Create an empty X-axis vector for use with "rotate" function

xvar = mat.or.vec(length(dist), 1)
xvar
[1] 0 0 0 0 0


## Rotate and translate "dist" vector around appropriate angle theta

OS01
ro1.0 = rotate(OS01.0, dOS01.0, xvar, dist)
ro1.50 = rotate(OS01.50, dOS01.50, xvar, dist)
ro1.100 = rotate(OS01.100, dOS01.100, xvar, dist)
ro1.150 = rotate(OS01.150, dOS01.150, xvar, dist)
ro1.200 = rotate(OS01.200, dOS01.200, xvar, dist)
ro1.250 = rotate(OS01.250, dOS01.250, xvar, dist)
ro1.300 = rotate(OS01.300, dOS01.300, xvar, dist)
ro1.350 = rotate(OS01.350, dOS01.350, xvar, dist)
ro1.400 = rotate(OS01.400, dOS01.400, xvar, dist)
ro1.450 = rotate(OS01.450, dOS01.450, xvar, dist)

# Create matrix of rotated UTM coordinates
rOS01 = rbind(ro1.0, ro1.50, ro1.100, ro1.150, ro1.200, ro1.250, ro1.300, ro1.350, ro1.400, ro1.450, OS01.500)

# Write matrix to a .csv file
write.csv(rOS01, file="D3_OS01_UTM.csv")

OS02
ro2.0 = rotate(OS02.0, dOS02.0, xvar, dist)
ro2.50 = rotate(OS02.50, dOS02.50, xvar, dist)
ro2.100 = rotate(OS02.100, dOS02.100, xvar, dist)
ro2.150 = rotate(OS02.150, dOS02.150, xvar, dist)
ro2.200 = rotate(OS02.200, dOS02.200, xvar, dist)
ro2.250 = rotate(OS02.250, dOS02.250, xvar, dist)
ro2.300 = rotate(OS02.300, dOS02.300, xvar, dist)
ro2.350 = rotate(OS02.350, dOS02.350, xvar, dist)
ro2.400 = rotate(OS02.400, dOS02.400, xvar, dist)
ro2.450 = rotate(OS02.450, dOS02.450, xvar, dist)
rOS02 = rbind(ro2.0, ro2.50, ro2.100, ro2.150, ro2.200, ro2.250, ro2.300, ro2.350, ro2.400, ro2.450, OS02.500)
write.csv(rOS02, file="D3_OS02_UTM.csv")

OS03
ro3.0 = rotate(OS03.0, dOS03.0, xvar, dist)
ro3.50 = rotate(OS03.50, dOS03.50, xvar, dist)
ro3.100 = rotate(OS03.100, dOS03.100, xvar, dist)
ro3.150 = rotate(OS03.150, dOS03.150, xvar, dist)
ro3.200 = rotate(OS03.200, dOS03.200, xvar, dist)
ro3.250 = rotate(OS03.250, dOS03.250, xvar, dist)
ro3.300 = rotate(OS03.300, dOS03.300, xvar, dist)
ro3.350 = rotate(OS03.350, dOS03.350, xvar, dist)
ro3.400 = rotate(OS03.400, dOS03.400, xvar, dist)
ro3.450 = rotate(OS03.450, dOS03.450, xvar, dist)
rOS03 = rbind(ro3.0, ro3.50, ro3.100, ro3.150, ro3.200, ro3.250, ro3.300, ro3.350, ro3.400, ro3.450, OS03.500)
write.csv(rOS03, file="D3_OS03_UTM.csv")

OS04
ro4.0 = rotate(OS04.0, dOS04.0, xvar, dist)
ro4.50 = rotate(OS04.50, dOS04.50, xvar, dist)
ro4.100 = rotate(OS04.100, dOS04.100, xvar, dist)
ro4.150 = rotate(OS04.150, dOS04.150, xvar, dist)
ro4.200 = rotate(OS04.200, dOS04.200, xvar, dist)
ro4.250 = rotate(OS04.250, dOS04.250, xvar, dist)
ro4.300 = rotate(OS04.300, dOS04.300, xvar, dist)
ro4.350 = rotate(OS04.350, dOS04.350, xvar, dist)
ro4.400 = rotate(OS04.400, dOS04.400, xvar, dist)
ro4.450 = rotate(OS04.450, dOS04.450, xvar, dist)
rOS04 = rbind(ro4.0, ro4.50, ro4.100, ro4.150, ro4.200, ro4.250, ro4.300, ro4.350, ro4.400, ro4.450, OS04.500)
write.csv(rOS04, file="D3_OS04_UTM.csv")

OS08
ro8.0 = rotate(OS08.0, dOS08.0, xvar, dist)
ro8.50 = rotate(OS08.50, dOS08.50, xvar, dist)
ro8.100 = rotate(OS08.100, dOS08.100, xvar, dist)
ro8.150 = rotate(OS08.150, dOS08.150, xvar, dist)
ro8.200 = rotate(OS08.200, dOS08.200, xvar, dist)
ro8.250 = rotate(OS08.250, dOS08.250, xvar, dist)
ro8.300 = rotate(OS08.300, dOS08.300, xvar, dist)
ro8.350 = rotate(OS08.350, dOS08.350, xvar, dist)
ro8.400 = rotate(OS08.400, dOS08.400, xvar, dist)
ro8.450 = rotate(OS08.450, dOS08.450, xvar, dist)
rOS08 = rbind(ro8.0, ro8.50, ro8.100, ro8.150, ro8.200, ro8.250, ro8.300, ro8.350, ro8.400, ro8.450, OS08.500)
write.csv(rOS08, file="D3_OS08_UTM.csv")

OS09
ro9.0 = rotate(OS09.0, dOS09.0, xvar, dist)
ro9.50 = rotate(OS09.50, dOS09.50, xvar, dist)
ro9.100 = rotate(OS09.100, dOS09.100, xvar, dist)
ro9.150 = rotate(OS09.150, dOS09.150, xvar, dist)
ro9.200 = rotate(OS09.200, dOS09.200, xvar, dist)
ro9.250 = rotate(OS09.250, dOS09.250, xvar, dist)
ro9.300 = rotate(OS09.300, dOS09.300, xvar, dist)
ro9.350 = rotate(OS09.350, dOS09.350, xvar, dist)
ro9.400 = rotate(OS09.400, dOS09.400, xvar, dist)
ro9.450 = rotate(OS09.450, dOS09.450, xvar, dist)
rOS09 = rbind(ro9.0, ro9.50, ro9.100, ro9.150, ro9.200, ro9.250, ro9.300, ro9.350, ro9.400, ro9.450, OS09.500)
write.csv(rOS09, file="D3_OS09_UTM.csv")

OS11
ro11.0 = rotate(OS11.0, dOS11.0, xvar, dist)
ro11.50 = rotate(OS11.50, dOS11.50, xvar, dist)
ro11.100 = rotate(OS11.100, dOS11.100, xvar, dist)
ro11.150 = rotate(OS11.150, dOS11.150, xvar, dist)
ro11.200 = rotate(OS11.200, dOS11.200, xvar, dist)
ro11.250 = rotate(OS11.250, dOS11.250, xvar, dist)
ro11.300 = rotate(OS11.300, dOS11.300, xvar, dist)
ro11.350 = rotate(OS11.350, dOS11.350, xvar, dist)
ro11.400 = rotate(OS11.400, dOS11.400, xvar, dist)
ro11.450 = rotate(OS11.450, dOS11.450, xvar, dist)
rOS11 = rbind(ro11.0, ro11.50, ro11.100, ro11.150, ro11.200, ro11.250, ro11.300, ro11.350, ro11.400, ro11.450, OS11.500)
write.csv(rOS11, file="D3_OS11_UTM.csv")

OS12
ro12.0 = rotate(OS12.0, dOS12.0, xvar, dist)
ro12.50 = rotate(OS12.50, dOS12.50, xvar, dist)
ro12.100 = rotate(OS12.100, dOS12.100, xvar, dist)
ro12.150 = rotate(OS12.150, dOS12.150, xvar, dist)
ro12.200 = rotate(OS12.200, dOS12.200, xvar, dist)
ro12.250 = rotate(OS12.250, dOS12.250, xvar, dist)
ro12.300 = rotate(OS12.300, dOS12.300, xvar, dist)
ro12.350 = rotate(OS12.350, dOS12.350, xvar, dist)
ro12.400 = rotate(OS12.400, dOS12.400, xvar, dist)
ro12.450 = rotate(OS12.450, dOS12.450, xvar, dist)
rOS12 = rbind(ro12.0, ro12.50, ro12.100, ro12.150, ro12.200, ro12.250, ro12.300, ro12.350, ro12.400, ro12.450, OS12.500)
write.csv(rOS12, file="D3_OS12_UTM.csv")





## Calculations for determining UTM of LAI transect points at Donaldson Tract (DT)

# Field collected, corrected GPS coordinates for DT LAI transects. Coordinates were copied and pasted from Access DB

DT03.0 = c(387627.972, 3292042.380)
DT03.50 = c(387582.015, 3292063.033)
DT03.100 = c(387537.271, 3292082.316)
DT03.150 = c(387492.310, 3292103.594)
DT03.200 = c(387446.653, 3292123.653)
DT03.250 = c(387401.207, 3292144.785)
DT03.300 = c(387355.949, 3292168.616)
DT03.350 = c(387312.457, 3292189.777)
DT03.400 = c(387267.815, 3292210.113)
DT03.450 = c(387222.762, 3292235.135)
DT03.500 = c(387176.128, 3292253.971)

DT04.0 = c(387536.939, 3292286.903)
DT04.50 = c(387491.182, 3292308.563)
DT04.100 = c(387444.311, 3292327.708)
DT04.150 = c(387398.762, 3292345.018)
DT04.200 = c(387352.855, 3292366.895)
DT04.250 = c(387307.583, 3292388.315)
DT04.300 = c(387261.488, 3292408.818)
DT04.350 = c(387215.430, 3292429.729)
DT04.400 = c(387171.441, 3292452.213)
DT04.450 = c(387126.371, 3292471.641)
DT04.500 = c(387078.283, 3292488.908)

DT05.0 = c(387392.709, 3293037.966)
DT05.50 = c(387366.224, 3293080.785)
DT05.100 = c(387342.663, 3293123.616)
DT05.150 = c(387315.752, 3293167.029)
DT05.200 = c(387289.349, 3293209.785)
DT05.250 = c(387263.081, 3293251.239)
DT05.300 = c(387236.942, 3293294.547)
DT05.350 = c(387212.417, 3293337.262)
DT05.400 = c(387184.404, 3293378.798)
DT05.450 = c(387158.912, 3293422.250)
DT05.500 = c(387132.105, 3293463.960)

DT06.0 = c(387191.074, 3292992.834)
DT06.50 = c(387166.310, 3293035.923)
DT06.100 = c(387137.763, 3293076.573)
DT06.150 = c(387108.881, 3293117.519)
DT06.200 = c(387084.356, 3293161.387)
DT06.250 = c(387057.771, 3293203.003)
DT06.300 = c(387034.500, 3293246.989)
DT06.350 = c(387010.120, 3293292.080)
DT06.400 = c(386987.828, 3293336.531)
DT06.450 = c(386963.081, 3293379.987)
DT06.500 = c(386938.302, 3293422.812)


# Calculate angles "theta" for every 50 m along each transect for use with "rotate" function

DT03
dDT03.0   = deg(atan(abs((DT03.50[1]-DT03.0[1])/(DT03.50[2]-DT03.0[2]))))
dDT03.50  = deg(atan(abs((DT03.100[1]-DT03.50[1])/(DT03.100[2]-DT03.50[2]))))
dDT03.100 = deg(atan(abs((DT03.150[1]-DT03.100[1])/(DT03.150[2]-DT03.100[2]))))
dDT03.150 = deg(atan(abs((DT03.200[1]-DT03.150[1])/(DT03.200[2]-DT03.150[2]))))
dDT03.200 = deg(atan(abs((DT03.250[1]-DT03.200[1])/(DT03.250[2]-DT03.200[2]))))
dDT03.250 = deg(atan(abs((DT03.300[1]-DT03.250[1])/(DT03.300[2]-DT03.250[2]))))
dDT03.300 = deg(atan(abs((DT03.350[1]-DT03.300[1])/(DT03.350[2]-DT03.300[2]))))
dDT03.350 = deg(atan(abs((DT03.400[1]-DT03.350[1])/(DT03.400[2]-DT03.350[2]))))
dDT03.400 = deg(atan(abs((DT03.450[1]-DT03.400[1])/(DT03.450[2]-DT03.400[2]))))
dDT03.450 = deg(atan(abs((DT03.500[1]-DT03.450[1])/(DT03.500[2]-DT03.450[2]))))

dDT03.0		= 65.80093
dDT03.50 	= 66.68575
dDT03.100	= 64.67393
dDT03.150	= 66.28214
dDT03.200	= 65.06198
dDT03.250	= 62.23056
dDT03.300	= 64.0548
dDT03.350	= 65.50907
dDT03.400	= 60.95263
dDT03.450	= 68.00566

DT04
dDT04.0   = deg(atan(abs((DT04.50[1]-DT04.0[1])/(DT04.50[2]-DT04.0[2]))))
dDT04.50  = deg(atan(abs((DT04.100[1]-DT04.50[1])/(DT04.100[2]-DT04.50[2]))))
dDT04.100 = deg(atan(abs((DT04.150[1]-DT04.100[1])/(DT04.150[2]-DT04.100[2]))))
dDT04.150 = deg(atan(abs((DT04.200[1]-DT04.150[1])/(DT04.200[2]-DT04.150[2]))))
dDT04.200 = deg(atan(abs((DT04.250[1]-DT04.200[1])/(DT04.250[2]-DT04.200[2]))))
dDT04.250 = deg(atan(abs((DT04.300[1]-DT04.250[1])/(DT04.300[2]-DT04.250[2]))))
dDT04.300 = deg(atan(abs((DT04.350[1]-DT04.300[1])/(DT04.350[2]-DT04.300[2]))))
dDT04.350 = deg(atan(abs((DT04.400[1]-DT04.350[1])/(DT04.400[2]-DT04.350[2]))))
dDT04.400 = deg(atan(abs((DT04.450[1]-DT04.400[1])/(DT04.450[2]-DT04.400[2]))))
dDT04.450 = deg(atan(abs((DT04.500[1]-DT04.450[1])/(DT04.500[2]-DT04.450[2]))))

dDT04.0 	= 64.66852
dDT04.50 	= 67.78187
dDT04.100	= 69.19169
dDT04.150	= 64.51985
dDT04.200	= 64.67929
dDT04.250	= 66.02056
dDT04.300	= 65.58126
dDT04.350	= 62.92716
dDT04.400	= 66.68092
dDT04.450	= 70.24827

DT05
dDT05.0   = deg(atan(abs((DT05.50[1]-DT05.0[1])/(DT05.50[2]-DT05.0[2]))))
dDT05.50  = deg(atan(abs((DT05.100[1]-DT05.50[1])/(DT05.100[2]-DT05.50[2]))))
dDT05.100 = deg(atan(abs((DT05.150[1]-DT05.100[1])/(DT05.150[2]-DT05.100[2]))))
dDT05.150 = deg(atan(abs((DT05.200[1]-DT05.150[1])/(DT05.200[2]-DT05.150[2]))))
dDT05.200 = deg(atan(abs((DT05.250[1]-DT05.200[1])/(DT05.250[2]-DT05.200[2]))))
dDT05.250 = deg(atan(abs((DT05.300[1]-DT05.250[1])/(DT05.300[2]-DT05.250[2]))))
dDT05.300 = deg(atan(abs((DT05.350[1]-DT05.300[1])/(DT05.350[2]-DT05.300[2]))))
dDT05.350 = deg(atan(abs((DT05.400[1]-DT05.350[1])/(DT05.400[2]-DT05.350[2]))))
dDT05.400 = deg(atan(abs((DT05.450[1]-DT05.400[1])/(DT05.450[2]-DT05.400[2]))))
dDT05.450 = deg(atan(abs((DT05.500[1]-DT05.450[1])/(DT05.500[2]-DT05.450[2]))))

dDT05.0 	= 31.73819
dDT05.50 	= 28.81485
dDT05.100	= 31.79409
dDT05.150	= 31.69647
dDT05.200	= 32.36105
dDT05.250	= 31.11352
dDT05.300	= 29.86247
dDT05.350	= 33.99679
dDT05.400	= 30.39888
dDT05.450	= 32.72884

DT06
dDT06.0   = deg(atan(abs((DT06.50[1]-DT06.0[1])/(DT06.50[2]-DT06.0[2]))))
dDT06.50  = deg(atan(abs((DT06.100[1]-DT06.50[1])/(DT06.100[2]-DT06.50[2]))))
dDT06.100 = deg(atan(abs((DT06.150[1]-DT06.100[1])/(DT06.150[2]-DT06.100[2]))))
dDT06.150 = deg(atan(abs((DT06.200[1]-DT06.150[1])/(DT06.200[2]-DT06.150[2]))))
dDT06.200 = deg(atan(abs((DT06.250[1]-DT06.200[1])/(DT06.250[2]-DT06.200[2]))))
dDT06.250 = deg(atan(abs((DT06.300[1]-DT06.250[1])/(DT06.300[2]-DT06.250[2]))))
dDT06.300 = deg(atan(abs((DT06.350[1]-DT06.300[1])/(DT06.350[2]-DT06.300[2]))))
dDT06.350 = deg(atan(abs((DT06.400[1]-DT06.350[1])/(DT06.400[2]-DT06.350[2]))))
dDT06.400 = deg(atan(abs((DT06.450[1]-DT06.400[1])/(DT06.450[2]-DT06.400[2]))))
dDT06.450 = deg(atan(abs((DT06.500[1]-DT06.450[1])/(DT06.500[2]-DT06.450[2]))))

dDT06.0 	= 29.88673
dDT06.50 	= 35.07896
dDT06.100	= 35.19792
dDT06.150	= 29.20796
dDT06.200	= 32.57112
dDT06.250	= 27.88129
dDT06.300	= 28.3994
dDT06.350	= 26.63358
dDT06.400	= 29.66033
dDT06.450	= 30.05413


## Rotate and translate "dist" vector around appropriate angle theta

DT03
rD3.0 = rotate(DT03.0, dDT03.0, xvar, dist)
rD3.50 = rotate(DT03.50, dDT03.50, xvar, dist)
rD3.100 = rotate(DT03.100, dDT03.100, xvar, dist)
rD3.150 = rotate(DT03.150, dDT03.150, xvar, dist)
rD3.200 = rotate(DT03.200, dDT03.200, xvar, dist)
rD3.250 = rotate(DT03.250, dDT03.250, xvar, dist)
rD3.300 = rotate(DT03.300, dDT03.300, xvar, dist)
rD3.350 = rotate(DT03.350, dDT03.350, xvar, dist)
rD3.400 = rotate(DT03.400, dDT03.400, xvar, dist)
rD3.450 = rotate(DT03.450, dDT03.450, xvar, dist)
rDT03 = rbind(rD3.0, rD3.50, rD3.100, rD3.150, rD3.200, rD3.250, rD3.300, rD3.350, rD3.400, rD3.450, DT03.500)
write.csv(rDT03, file="D3_DT03_UTM.csv")

DT04
rD4.0 = rotate(DT04.0, dDT04.0, xvar, dist)
rD4.50 = rotate(DT04.50, dDT04.50, xvar, dist)
rD4.100 = rotate(DT04.100, dDT04.100, xvar, dist)
rD4.150 = rotate(DT04.150, dDT04.150, xvar, dist)
rD4.200 = rotate(DT04.200, dDT04.200, xvar, dist)
rD4.250 = rotate(DT04.250, dDT04.250, xvar, dist)
rD4.300 = rotate(DT04.300, dDT04.300, xvar, dist)
rD4.350 = rotate(DT04.350, dDT04.350, xvar, dist)
rD4.400 = rotate(DT04.400, dDT04.400, xvar, dist)
rD4.450 = rotate(DT04.450, dDT04.450, xvar, dist)
rDT04 = rbind(rD4.0, rD4.50, rD4.100, rD4.150, rD4.200, rD4.250, rD4.300, rD4.350, rD4.400, rD4.450, DT04.500)
write.csv(rDT04, file="D3_DT04_UTM.csv")

DT05
rD5.0 = rotate(DT05.0, dDT05.0, xvar, dist)
rD5.50 = rotate(DT05.50, dDT05.50, xvar, dist)
rD5.100 = rotate(DT05.100, dDT05.100, xvar, dist)
rD5.150 = rotate(DT05.150, dDT05.150, xvar, dist)
rD5.200 = rotate(DT05.200, dDT05.200, xvar, dist)
rD5.250 = rotate(DT05.250, dDT05.250, xvar, dist)
rD5.300 = rotate(DT05.300, dDT05.300, xvar, dist)
rD5.350 = rotate(DT05.350, dDT05.350, xvar, dist)
rD5.400 = rotate(DT05.400, dDT05.400, xvar, dist)
rD5.450 = rotate(DT05.450, dDT05.450, xvar, dist)
rDT05 = rbind(rD5.0, rD5.50, rD5.100, rD5.150, rD5.200, rD5.250, rD5.300, rD5.350, rD5.400, rD5.450, DT05.500)
write.csv(rDT05, file="D3_DT05_UTM.csv")

DT06
rD6.0 = rotate(DT06.0, dDT06.0, xvar, dist)
rD6.50 = rotate(DT06.50, dDT06.50, xvar, dist)
rD6.100 = rotate(DT06.100, dDT06.100, xvar, dist)
rD6.150 = rotate(DT06.150, dDT06.150, xvar, dist)
rD6.200 = rotate(DT06.200, dDT06.200, xvar, dist)
rD6.250 = rotate(DT06.250, dDT06.250, xvar, dist)
rD6.300 = rotate(DT06.300, dDT06.300, xvar, dist)
rD6.350 = rotate(DT06.350, dDT06.350, xvar, dist)
rD6.400 = rotate(DT06.400, dDT06.400, xvar, dist)
rD6.450 = rotate(DT06.450, dDT06.450, xvar, dist)
rDT06 = rbind(rD6.0, rD6.50, rD6.100, rD6.150, rD6.200, rD6.250, rD6.300, rD6.350, rD6.400, rD6.450, DT06.500)
write.csv(rDT06, file="D3_DT06_UTM.csv")