circ = c(270, 90)
obs1 = 275
obs2 = 268
obs3 = 1

out$azimuthQF <- ifelse(out$stemAzimuth >= out$aAzimuth.ch & out$stemAzimuth <= out$bAzimuth.ch, 1, -9999)

ifelse(obs >= circ[1] & obs <= circ[2], "PASS", "FAIL")

ifelse(obs >= circ[1] & obs <= circ[2], "PASS", 
       ifelse(circ[1] == 270, obs >= 270 & obs <= 360 | obs >= 0 & obs <= 90,"PASS","FAIL"))

# this should pass
ifelse(circ[1] == 270, 
       ifelse(obs1 >= 270 & obs1 <= 360 | obs1 >= 0 & obs1 <= 90,"PASS1","FAIL1"),
       ifelse(obs1 >= circ[1] & obs1 <= circ[2],"PASS2","FAIL2"))


# this should pass
ifelse(circ[1] == 270, 
       ifelse(obs3 >= 270 & obs3 <= 360 | obs3 >= 0 & obs3 <= 90,"PASS1","FAIL1"),
       ifelse(obs3 >= circ[1] & obs3 <= circ[2],"PASS2","FAIL2"))

# this should fail
ifelse(circ[1] == 270, 
       ifelse(obs2 >= 270 & obs2 <= 360 | obs2 >= 0 & obs2 <= 90,"PASS1","FAIL1"),
       ifelse(obs2 >= circ[1] & obs2 <= circ[2],"PASS2","FAIL2"))

? ifelse
