## Simple functions for converting degrees into radians, and vice versa
## 'return' displays the value on screen or passes it to another function

radians = function(degrees) {
	rad = (degrees*pi)/180
	return(rad) 	
	}

deg = function(radians) {
	degrees = (radians*180)/pi
	return(degrees)
	}
	
