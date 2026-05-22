######## ABCD QC HORMONE DATA CLEANING STEPS  ##############################

####### prepare environment
library(dplyr)

HORM_CLEAN <- read.csv("")


######## check salivary sex indicator against master sex ######## 

### Time 1 
names(HORM_CLEAN)
table(HORM_CLEAN$ysex)
################
####### 6188 Males (ysex = 1)
####### 5677 Females (ysex = 2)
####### 3 intersex (ysex = 3)
####### remove intersex 
HORM_MF <- HORM_CLEAN %>% filter(ysex != 3)
##### check point 
table(HORM_MF$ysex)
####### 6188 Males (ysex = 1)
####### 5677 Females (ysex = 2)
names(HORM_MF)
table(HORM_MF$SALSEX1)
###### 5656 Females (hormone_sal_sex_1 = 1)
###### 6120 Males  (hormone_sal_sex_1 = 2)
###### 59 participants unable to complete W1 ( hormone_sal_sex_1 = 3)
###### 19 participants refused W1 ( hormone_sal_sex_1 = 4) 
###### 8 participants not collected ( hormone_sal_sex_1 = 5) 

############ CHECK if male = male 
WrongM <- HORM_MF[HORM_MF$ysex == 1 & HORM_MF$SALSEX1 != 2, ]
print(WrongM)
######## 74 participants in this df 
table(WrongM$SALSEX1)
###### 37 participants unable to complete W1 ( hormone_sal_sex_1 = 3)
###### 12 participants refused W1 ( hormone_sal_sex_1 = 4) 
###### 6 participants not collected ( hormone_sal_sex_1 = 5) 
##### Will keep above conditions unless no future hormone collections 

######## 17 female indicated tube (hormone_sal_sex_1 = 1) (remove these)
baddata <- WrongM[WrongM$SALSEX1 == 1, ]
print(baddata)

HORM_MF2 <- anti_join(HORM_MF, baddata, by = "subid")
####### check point
table(HORM_MF2$SALSEX1)
###### 5639 Females (hormone_sal_sex_1 = 1)
###### 6120 Males  (hormone_sal_sex_1 = 2)
###### 59 participants unable to complete W1 ( hormone_sal_sex_1 = 3)
###### 19 participants refused W1 ( hormone_sal_sex_1 = 4) 
###### 8 participants not collected ( hormone_sal_sex_1 = 5)

############ CHECK if female = female 
WrongF <- HORM_MF[HORM_MF$ysex == 2 & HORM_MF$SALSEX1 != 1, ]
print(WrongF)
######## 38 participants in this df 
table(WrongF$SALSEX1)
###### 22 participants unable to complete W1 ( hormone_sal_sex_1 = 3)
###### 7 participants refused W1 ( hormone_sal_sex_1 = 4) 
###### 2 participants not collected ( hormone_sal_sex_1 = 5) 
##### Will keep above conditions unless no future hormone collections 

######## 6 male indicated tube (hormone_sal_sex_1 = 2) (remove these)
baddataF <- WrongF[WrongF$SALSEX1 == 2, ]
print(baddataF)

HORM_cleanW1 <- anti_join(HORM_MF2, baddataF, by = "subid")
####### check point
table(HORM_cleanW1$SALSEX1)
###### 5639 Females (hormone_sal_sex_1 = 1)
###### 6114 Males  (hormone_sal_sex_1 = 2)
###### 59 participants unable to complete W1 ( hormone_sal_sex_1 = 3)
###### 19 participants refused W1 ( hormone_sal_sex_1 = 4) 
###### 8 participants not collected ( hormone_sal_sex_1 = 5)

##########################################################################
################### WAVE 1 SEX MATCHING COMPLETE ######################### 
##########################################################################

table(HORM_cleanW1$ysex)
################
####### 6171 Males (ysex = 1)
####### 5671 Females (ysex = 2)
names(HORM_cleanW1)
table(HORM_cleanW1$SALSEX3)
###### 5283 Females (hormone_sal_sex_3 = 1)
###### 5796 Males  (hormone_sal_sex_3 = 2)
###### 26 participants unable to complete W1 ( hormone_sal_sex_3 = 3)
###### 30 participants refused W1 ( hormone_sal_sex_3 = 4) 
###### 50 participants not collected ( hormone_sal_sex_3 = 5)

############ CHECK if male = male 
WrongM2 <- HORM_cleanW1[HORM_cleanW1$ysex == 1 & HORM_cleanW1$SALSEX3 != 2, ]
print(WrongM2)
######## 388 participants in this df 
table(WrongM2$SALSEX3)
###### 17 participants unable to complete W1 ( hormone_sal_sex_3 = 3)
###### 13 participants refused W1 ( hormone_sal_sex_3 = 4) 
###### 29 participants not collected ( hormone_sal_sex_3 = 5) 
##### Will keep above conditions unless other concerns

######## 4 female indicated tube (hormone_sal_sex_3 = 1) (remove these)
baddata <- WrongM2[WrongM2$SALSEX3 == 1, ]
print(baddata)

HORM_MFw3 <- anti_join(HORM_cleanW1, baddata, by = "subid")
####### check point
table(HORM_MFw3$SALSEX3)
###### 5279 Females (hormone_sal_sex_3 = 1)
###### 5796 Males  (hormone_sal_sex_3 = 2)
###### 26 participants unable to complete W3 ( hormone_sal_sex_3 = 3)
###### 30 participants refused W3 ( hormone_sal_sex_3 = 4) 
###### 50 participants not collected ( hormone_sal_sex_3 = 5)

############ CHECK if female = female 
WrongF2 <- HORM_MFw3[HORM_MFw3$ysex == 2 & HORM_MFw3$SALSEX3 != 1, ]
print(WrongF2)
######## 392 participants in this df 
table(WrongF2$SALSEX3)
###### 9 participants unable to complete W1 ( hormone_sal_sex_3 = 3)
###### 17 participants refused W1 ( hormone_sal_sex_3 = 4) 
###### 21 participants not collected ( hormone_sal_sex_3 = 5) 
##### Will keep above conditions unless other concerns

######## 13 male indicated tube (hormone_sal_sex_3 = 2) (remove these)
baddataF2 <- WrongF2[WrongF2$SALSEX3 == 2, ]
print(baddataF2)

HORM_cleanW3 <- anti_join(HORM_MFw3, baddataF2, by = "subid")
####### check point
table(HORM_cleanW3$SALSEX3)
###### 5279 Females (hormone_sal_sex_3 = 1)
###### 5783 Males  (hormone_sal_sex_3 = 2)
###### 26 participants unable to complete W3 ( hormone_sal_sex_3 = 3)
###### 30 participants refused W3 ( hormone_sal_sex_3 = 4) 
###### 50 participants not collected ( hormone_sal_sex_3 = 5)



##########################################################################
################### WAVE 3 SEX MATCHING COMPLETE ######################### 
##########################################################################

table(HORM_cleanW3$ysex)
################
####### 6167 Males (ysex = 1)
####### 5658 Females (ysex = 2)
names(HORM_cleanW3)
table(HORM_cleanW3$SALSEX5)
###### 3564 Females (hormone_sal_sex_5 = 1)
###### 3954 Males  (hormone_sal_sex_5 = 2)
###### 18 participants unable to complete W5 ( hormone_sal_sex_5 = 3)
###### 27 participants refused W5 ( hormone_sal_sex_5 = 4) 
###### 306 participants not collected ( hormone_sal_sex_5 = 5)
###### 59 COVID inst not collected ( hormone_sal_sex_5 = 6)
###### 37 COVID staff not collected ( hormone_sal_sex_5 = 7)
###### 1 COVID SD not collected ( hormone_sal_sex_5 = 8)
###### 2 COVID ABCD not collected ( hormone_sal_sex_5 = 9)

############ CHECK if male = male 
WrongM3 <- HORM_cleanW3[HORM_cleanW3$ysex == 1 & HORM_cleanW3$SALSEX5 != 2, ]
print(WrongM3)
######## 2216 participants in this df 
table(WrongM3$SALSEX5)
###### 16 participants unable to complete W5 ( hormone_sal_sex_5 = 3)
###### 15 participants refused W5 ( hormone_sal_sex_5 = 4) 
###### 155 participants not collected ( hormone_sal_sex_5 = 5) 
###### 32 COVID inst not collected ( hormone_sal_sex_5 = 6)
###### 19 COVID staff not collected ( hormone_sal_sex_5 = 7)
###### 1 COVID ABCD not collected ( hormone_sal_sex_5 = 9)
##### Will keep above conditions unless other concerns

######## 9 female indicated tube (hormone_sal_sex_5 = 1) (remove these)
baddata <- WrongM3[WrongM3$SALSEX5 == 1, ]
print(baddata)

HORM_MFw5 <- anti_join(HORM_cleanW3, baddata, by = "subid")
####### check point
table(HORM_MFw5$SALSEX5)
###### 3555 Females (hormone_sal_sex_5 = 1)
###### 3954 Males  (hormone_sal_sex_5 = 2)
###### 18 participants unable to complete W5 ( hormone_sal_sex_5 = 3)
###### 27 participants refused W5 ( hormone_sal_sex_5 = 4) 
###### 306 participants not collected ( hormone_sal_sex_5 = 5)
###### 59 COVID inst not collected ( hormone_sal_sex_5 = 6)
###### 37 COVID staff not collected ( hormone_sal_sex_5 = 7)
###### 1 COVID SD not collected ( hormone_sal_sex_5 = 8)
###### 2 COVID ABCD not collected ( hormone_sal_sex_5 = 9)

############ CHECK if female = female 
WrongF3 <- HORM_MFw5[HORM_MFw5$ysex == 2 & HORM_MFw5$SALSEX5 != 1, ]
print(WrongF3)
######## 2103 participants in this df 
table(WrongF3$SALSEX5)
###### 2 participants unable to complete W5 ( hormone_sal_sex_5 = 3)
###### 12 participants refused W5 ( hormone_sal_sex_5 = 4) 
###### 151 participants not collected ( hormone_sal_sex_5 = 5) 
###### 27 COVID inst not collected ( hormone_sal_sex_5 = 6)
###### 18 COVID staff not collected ( hormone_sal_sex_5 = 7)
###### 1 COVID SD not collected ( hormone_sal_sex_5 = 8)
###### 1 COVID ABCD not collected ( hormone_sal_sex_5 = 9)
##### Will keep above conditions unless other concerns

######## 3 male indicated tube (hormone_sal_sex_5 = 2) (remove these)
baddataF3 <- WrongF3[WrongF3$SALSEX5 == 2, ]
print(baddataF3)

HORM_cleanW5 <- anti_join(HORM_MFw5, baddataF3, by = "subid")
####### check point
table(HORM_cleanW5$SALSEX5)
###### 3555 Females (hormone_sal_sex_5 = 1)
###### 3951 Males  (hormone_sal_sex_5 = 2)
###### 18 participants unable to complete W5 ( hormone_sal_sex_5 = 3)
###### 27 participants refused W5 ( hormone_sal_sex_5 = 4) 
###### 306 participants not collected ( hormone_sal_sex_5 = 5)
###### 59 COVID inst not collected ( hormone_sal_sex_5 = 6)
###### 37 COVID staff not collected ( hormone_sal_sex_5 = 7)
###### 1 COVID SD not collected ( hormone_sal_sex_5 = 8)
###### 2 COVID ABCD not collected ( hormone_sal_sex_5 = 9)




##########################################################################
################### WAVE 5 SEX MATCHING COMPLETE ######################### 
##########################################################################

table(HORM_cleanW5$ysex)
################
####### 6158 Males (ysex = 1)
####### 5655 Females (ysex = 2)
names(HORM_cleanW5)
table(HORM_cleanW5$SALSEX7)
###### 976 Females (hormone_sal_sex_7 = 1)
###### 1048 Males  (hormone_sal_sex_7 = 2)
###### 8 participants unable to complete W7 ( hormone_sal_sex_7 = 3)
###### 6 participants refused W7 ( hormone_sal_sex_7 = 4) 
###### 1002 participants not collected ( hormone_sal_sex_7 = 5)
###### 44 COVID inst not collected ( hormone_sal_sex_7 = 6)
###### 6 COVID staff not collected ( hormone_sal_sex_7 = 7)
###### 3 COVID SD not collected ( hormone_sal_sex_7 = 8)
###### 33 COVID ABCD not collected ( hormone_sal_sex_7 = 9)

############ CHECK if male = male 
WrongM5 <- HORM_cleanW5[HORM_cleanW5$ysex == 1 & HORM_cleanW5$SALSEX7 != 2, ]
print(WrongM5)
######## 5110 participants in this df 
table(WrongM5$SALSEX7)
###### 2 participants unable to complete W7 ( hormone_sal_sex_7 = 3)
###### 3 participants refused W7 ( hormone_sal_sex_7 = 4) 
###### 513 participants not collected ( hormone_sal_sex_7 = 5) 
###### 30 COVID inst not collected ( hormone_sal_sex_7 = 6)
###### 4 COVID staff not collected ( hormone_sal_sex_7 = 7)
###### 3 COVID SD not collected ( hormone_sal_sex_7 = 8)
###### 19 COVID ABCD not collected ( hormone_sal_sex_7 = 9)
##### Will keep above conditions unless other concerns

######## 1 female indicated tube (hormone_sal_sex_7 = 1) (remove these)
baddata <- WrongM5[WrongM5$SALSEX7 == 1, ]
print(baddata)

HORM_MFw7 <- anti_join(HORM_cleanW5, baddata, by = "subid")
####### check point
table(HORM_MFw7$SALSEX7)
###### 975 Females (hormone_sal_sex_7 = 1)
###### 1048 Males  (hormone_sal_sex_7 = 2)
###### 8 participants unable to complete W7 ( hormone_sal_sex_7 = 3)
###### 6 participants refused W7 ( hormone_sal_sex_7 = 4) 
###### 1002 participants not collected ( hormone_sal_sex_7 = 5)
###### 44 COVID inst not collected ( hormone_sal_sex_7 = 6)
###### 6 COVID staff not collected ( hormone_sal_sex_7 = 7)
###### 3 COVID SD not collected ( hormone_sal_sex_7 = 8)
###### 33 COVID ABCD not collected ( hormone_sal_sex_7 = 9)

############ CHECK if female = female 
WrongF5 <- HORM_MFw7[HORM_MFw7$ysex == 2 & HORM_MFw7$SALSEX7 != 1, ]
print(WrongF5)
######## 4680 participants in this df 
table(WrongF5$SALSEX7)
###### 6 participants unable to complete W5 ( hormone_sal_sex_7 = 3)
###### 3 participants refused W5 ( hormone_sal_sex_7 = 4) 
###### 489 participants not collected ( hormone_sal_sex_7 = 5) 
###### 14 COVID inst not collected ( hormone_sal_sex_7 = 6)
###### 2 COVID staff not collected ( hormone_sal_sex_7 = 7)
###### 14 COVID ABCD not collected ( hormone_sal_sex_7 = 9)
##### Will keep above conditions unless other concerns

######## 0 male indicated tube (hormone_sal_sex_7 = 2) (remove these)


HORM_cleanW7 <- HORM_MFw7
####### check point
table(HORM_cleanW7$SALSEX7)
###### 975 Females (hormone_sal_sex_7 = 1)
###### 1048 Males  (hormone_sal_sex_7 = 2)
###### 8 participants unable to complete W7 ( hormone_sal_sex_7 = 3)
###### 6 participants refused W7 ( hormone_sal_sex_7 = 4) 
###### 1002 participants not collected ( hormone_sal_sex_7 = 5)
###### 44 COVID inst not collected ( hormone_sal_sex_7 = 6)
###### 6 COVID staff not collected ( hormone_sal_sex_7 = 7)
###### 3 COVID SD not collected ( hormone_sal_sex_7 = 8)
###### 33 COVID ABCD not collected ( hormone_sal_sex_7 = 9)



##########################################################################
################### WAVE 7 SEX MATCHING COMPLETE ######################### 
##########################################################################

table(HORM_cleanW7$ysex)
################
####### 6157 Males (ysex = 1)
####### 5655 Females (ysex = 2)
names(HORM_cleanW7)
table(HORM_cleanW7$SALSEX9)
###### 1365 Females (hormone_sal_sex_9 = 1)
###### 1477 Males  (hormone_sal_sex_9 = 2)
###### 10 participants unable to complete W9 ( hormone_sal_sex_9 = 3)
###### 24 participants refused W9 ( hormone_sal_sex_9 = 4) 
###### 198 participants not collected ( hormone_sal_sex_9 = 5)
###### 150 COVID inst not collected ( hormone_sal_sex_9 = 6)
###### 85 COVID staff not collected ( hormone_sal_sex_9 = 7)
###### 36 COVID SD not collected ( hormone_sal_sex_9 = 8)
###### 25 COVID ABCD not collected ( hormone_sal_sex_9 = 9)

############ CHECK if male = male 
WrongM7 <- HORM_cleanW7[HORM_cleanW7$ysex == 1 & HORM_cleanW7$SALSEX9 != 2, ]
print(WrongM7)
######## 5109 participants in this df 
table(WrongM7$SALSEX9)
###### 5 participants unable to complete W9 ( hormone_sal_sex_9 = 3)
###### 10 participants refused W9 ( hormone_sal_sex_9 = 4) 
###### 101 participants not collected ( hormone_sal_sex_9 = 5)
###### 95 COVID inst not collected ( hormone_sal_sex_9 = 6)
###### 41 COVID staff not collected ( hormone_sal_sex_9 = 7)
###### 19 COVID SD not collected ( hormone_sal_sex_9 = 8)
###### 13 COVID ABCD not collected ( hormone_sal_sex_9 = 9)
##### Will keep above conditions unless other concerns

######## 1 female indicated tube (hormone_sal_sex_9 = 1) (remove these)
baddata <- WrongM7[WrongM7$SALSEX9 == 1, ]
print(baddata)

HORM_MFw9 <- anti_join(HORM_cleanW7, baddata, by = "subid")
####### check point
table(HORM_MFw9$SALSEX9)
###### 1364 Females (hormone_sal_sex_9 = 1)
###### 1477 Males  (hormone_sal_sex_9 = 2)
###### 10 participants unable to complete W9 ( hormone_sal_sex_9 = 3)
###### 24 participants refused W9 ( hormone_sal_sex_9 = 4) 
###### 198 participants not collected ( hormone_sal_sex_9 = 5)
###### 150 COVID inst not collected ( hormone_sal_sex_9 = 6)
###### 85 COVID staff not collected ( hormone_sal_sex_9 = 7)
###### 36 COVID SD not collected ( hormone_sal_sex_9 = 8)
###### 25 COVID ABCD not collected ( hormone_sal_sex_9 = 9)

############ CHECK if female = female 
WrongF9 <- HORM_MFw9[HORM_MFw9$ysex == 2 & HORM_MFw9$SALSEX9 != 1, ]
print(WrongF9)
######## 4291 participants in this df 
table(WrongF9$SALSEX9)
###### 5 participants unable to complete W9 ( hormone_sal_sex_9 = 3)
###### 14 participants refused W9 ( hormone_sal_sex_9 = 4) 
###### 97 participants not collected ( hormone_sal_sex_9 = 5)
###### 55 COVID inst not collected ( hormone_sal_sex_9 = 6)
###### 44 COVID staff not collected ( hormone_sal_sex_9 = 7)
###### 17 COVID SD not collected ( hormone_sal_sex_9 = 8)
###### 12 COVID ABCD not collected ( hormone_sal_sex_9 = 9)
##### Will keep above conditions unless other concerns

######## 2 male indicated tube (hormone_sal_sex_9 = 2) (remove these)
baddataF9 <- WrongF9[WrongF9$SALSEX9 == 2, ]
print(baddataF9)

HORM_cleanW9 <- anti_join(HORM_MFw9, baddataF9, by = "subid")
####### check point
table(HORM_cleanW9$SALSEX9)
###### 1364 Females (hormone_sal_sex_9 = 1)
###### 1475 Males  (hormone_sal_sex_9 = 2)
###### 10 participants unable to complete W9 ( hormone_sal_sex_9 = 3)
###### 24 participants refused W9 ( hormone_sal_sex_9 = 4) 
###### 198 participants not collected ( hormone_sal_sex_9 = 5)
###### 150 COVID inst not collected ( hormone_sal_sex_9 = 6)
###### 85 COVID staff not collected ( hormone_sal_sex_9 = 7)
###### 36 COVID SD not collected ( hormone_sal_sex_9 = 8)
###### 25 COVID ABCD not collected ( hormone_sal_sex_9 = 9)



##########################################################################
################### WAVE 9 SEX MATCHING COMPLETE ######################### 
##########################################################################

write.csv(HORM_cleanW9, "sex_matched_HORM.csv" ,row.names=FALSE, na="")



##########################################################################
###################     ALL WAVES SEX-MATCHED    ######################### 
##########################################################################

##### QC concerns

QCHORM <- read.csv("sex_matched_HORM.csv")
names(QCHORM)

##### DHEA rep 1 
sum(is.na(QCHORM$dhe11) | QCHORM$dhe11 == "")
#### 461 empty before QC 

QCHORM$dhe11[ (QCHORM$contam1 == 1 & QCHORM$lowlim1D1 == 1) |
              (QCHORM$disc1 == 1 & QCHORM$lowlim1D1 == 1) |
              (QCHORM$bubb1 == 1 & QCHORM$lowlim1D1 == 1) |
              (QCHORM$smalsamp1 == 1 & QCHORM$lowlim1D1 == 1) |
              (QCHORM$other1 == 1 & QCHORM$lowlim1D1 == 1)] <- NA

sum(is.na(QCHORM$dhe11) | QCHORM$dhe11 == "")
#### 473 empty after QC

##### DHEA rep 2
sum(is.na(QCHORM$dhe21) | QCHORM$dhe21 == "")
#### 509 empty before QC 

QCHORM$dhe21[ (QCHORM$contam1 == 1 & QCHORM$low.lim2D1 == 1) |
                (QCHORM$disc1 == 1 & QCHORM$low.lim2D1 == 1) |
                (QCHORM$bubb1 == 1 & QCHORM$low.lim2D1 == 1) |
                (QCHORM$smalsamp1 == 1 & QCHORM$low.lim2D1 == 1) |
                (QCHORM$other1 == 1 & QCHORM$low.lim2D1 == 1)] <- NA

sum(is.na(QCHORM$dhe21) | QCHORM$dhe21 == "")
#### 530 empty after QC

##### EST rep 1 
sum(is.na(QCHORM$est11) | QCHORM$est11 == "")
#### 6480 empty before QC

QCHORM$est11[ (QCHORM$contam1 == 1 & QCHORM$lowlim1E1 == 1) |
                (QCHORM$disc1 == 1 & QCHORM$lowlim1E1 == 1) |
                (QCHORM$bubb1 == 1 & QCHORM$lowlim1E1 == 1) |
                (QCHORM$smalsamp1 == 1 & QCHORM$lowlim1E1 == 1) |
                (QCHORM$other1 == 1 & QCHORM$lowlim1E1 == 1)] <- NA

sum(is.na(QCHORM$est11) | QCHORM$est11 == "")
#### 6486 empty after QC

##### EST rep 2 
sum(is.na(QCHORM$est21) | QCHORM$est21 == "")
#### 6523 empty before QC

QCHORM$est21[ (QCHORM$contam1 == 1 & QCHORM$lowlim2E1 == 1) |
                (QCHORM$disc1 == 1 & QCHORM$lowlim2E1 == 1) |
                (QCHORM$bubb1 == 1 & QCHORM$lowlim2E1 == 1) |
                (QCHORM$smalsamp1 == 1 & QCHORM$lowlim2E1 == 1) |
                (QCHORM$other1 == 1 & QCHORM$lowlim2E1 == 1)] <- NA

sum(is.na(QCHORM$est21) | QCHORM$est21 == "" )
#### 6528 empty after QC

##### TEST rep 1 
sum(is.na(QCHORM$tes11) | QCHORM$tes11 == "")
#### 434 NA before QC

QCHORM$tes11[ (QCHORM$contam1 == 1 & QCHORM$lowlim1T1 == 1) |
                (QCHORM$disc1 == 1 & QCHORM$lowlim1T1 == 1) |
                (QCHORM$bubb1 == 1 & QCHORM$lowlim1T1 == 1) |
                (QCHORM$smalsamp1 == 1 & QCHORM$lowlim1T1 == 1) |
                (QCHORM$other1 == 1 & QCHORM$lowlim1T1 == 1)] <- NA

sum(is.na(QCHORM$tes11) | QCHORM$tes11 == "")
#### 434 NA after QC

##### TEST rep 2 
sum(is.na(QCHORM$tes21) | QCHORM$tes21 == "")
#### 444 NA before QC

QCHORM$tes21[ (QCHORM$contam1 == 1 & QCHORM$lowlim2T1 == 1) |
                (QCHORM$disc1 == 1 & QCHORM$lowlim2T1 == 1) |
                (QCHORM$bubb1 == 1 & QCHORM$lowlim2T1 == 1) |
                (QCHORM$smalsamp1 == 1 & QCHORM$lowlim2T1 == 1) |
                (QCHORM$other1 == 1 & QCHORM$lowlim2T1 == 1)] <- NA

sum(is.na(QCHORM$tes21) | QCHORM$tes21 == "")
#### 445 NA after QC

##########################################################################
#################### WAVE 1 QC Concerns COMPLETE ######################### 
##########################################################################

##### DHEA rep 1 
sum(is.na(QCHORM$dhe13) | QCHORM$dhe13 == "" )
#### 1135 NA before QC 

QCHORM$dhe13[ (QCHORM$contam3 == 1 & QCHORM$lowlim1D3 == 1) |
                (QCHORM$disc3 == 1 & QCHORM$lowlim1D3 == 1) |
                (QCHORM$bubb3 == 1 & QCHORM$lowlim1D3 == 1) |
            (QCHORM$smalsamp3 == 1 & QCHORM$lowlim1D3 == 1) |
               (QCHORM$other3 == 1 & QCHORM$lowlim1D3 == 1)] <- NA

sum(is.na(QCHORM$dhe13) | QCHORM$dhe13 == "")
#### 1147 NA after QC

##### DHEA rep 2
sum(is.na(QCHORM$dhe23) | QCHORM$dhe23 == "")
#### 1174 NA before QC 

QCHORM$dhe23[ (QCHORM$contam3 == 1 & QCHORM$low.lim2D3 == 1) |
                (QCHORM$disc3 == 1 & QCHORM$low.lim2D3 == 1) |
                (QCHORM$bubb3 == 1 & QCHORM$low.lim2D3 == 1) |
            (QCHORM$smalsamp3 == 1 & QCHORM$low.lim2D3 == 1) |
               (QCHORM$other3 == 1 & QCHORM$low.lim2D3 == 1)] <- NA

sum(is.na(QCHORM$dhe23) | QCHORM$dhe23 == "")
#### 1188 NA after QC

##### EST rep 1 
sum(is.na(QCHORM$est13) | QCHORM$est13 == "")
#### 6942 NA before QC

QCHORM$est13[ (QCHORM$contam3 == 1 & QCHORM$lowlim1E3 == 1) |
                (QCHORM$disc3 == 1 & QCHORM$lowlim1E3 == 1) |
                (QCHORM$bubb3 == 1 & QCHORM$lowlim1E3 == 1) |
            (QCHORM$smalsamp3 == 1 & QCHORM$lowlim1E3 == 1) |
               (QCHORM$other3 == 1 & QCHORM$lowlim1E3 == 1)] <- NA

sum(is.na(QCHORM$est13) | QCHORM$est13 == "")
#### 6948 NA after QC

##### EST rep 2 
sum(is.na(QCHORM$est23) | QCHORM$est23 == "")
#### 6974 NA before QC

QCHORM$est23[ (QCHORM$contam3 == 1 & QCHORM$lowlim2E3 == 1) |
                (QCHORM$disc3 == 1 & QCHORM$lowlim2E3 == 1) |
                (QCHORM$bubb3 == 1 & QCHORM$lowlim2E3 == 1) |
            (QCHORM$smalsamp3 == 1 & QCHORM$lowlim2E3 == 1) |
               (QCHORM$other3 == 1 & QCHORM$lowlim2E3 == 1)] <- NA

sum(is.na(QCHORM$est23) | QCHORM$est23 == "")
#### 6978 NA after QC

##### TEST rep 1 
sum(is.na(QCHORM$tes13) | QCHORM$tes13 == "")
#### 1167 NA before QC

QCHORM$tes13[ (QCHORM$contam3 == 1 & QCHORM$lowlim1T3 == 1) |
                (QCHORM$disc3 == 1 & QCHORM$lowlim1T3 == 1) |
                (QCHORM$bubb3 == 1 & QCHORM$lowlim1T3 == 1) |
            (QCHORM$smalsamp3 == 1 & QCHORM$lowlim1T3 == 1) |
               (QCHORM$other3 == 1 & QCHORM$lowlim1T3 == 1)] <- NA

sum(is.na(QCHORM$tes13) | QCHORM$tes13 == "")
#### 1167 NA after QC

##### TEST rep 2 
sum(is.na(QCHORM$tes23) | QCHORM$tes23 == "")
#### 1171 NA before QC

QCHORM$tes23[ (QCHORM$contam3 == 1 & QCHORM$lowlim2T3 == 1) |
                (QCHORM$disc3 == 1 & QCHORM$lowlim2T3 == 1) |
                (QCHORM$bubb3 == 1 & QCHORM$lowlim2T3 == 1) |
            (QCHORM$smalsamp3 == 1 & QCHORM$lowlim2T3 == 1) |
               (QCHORM$other3 == 1 & QCHORM$lowlim2T3 == 1)] <- NA

sum(is.na(QCHORM$tes23) | QCHORM$tes23 == "")
#### 1171  NA after QC


##########################################################################
#################### WAVE 3 QC Concerns COMPLETE ######################### 
##########################################################################

##### DHEA rep 1 
sum(is.na(QCHORM$dhe15) | QCHORM$dhe15 == "")
#### 4494 NA before QC 

QCHORM$dhe15[ (QCHORM$contam5 == 1 & QCHORM$lowlim1D5 == 1) |
                (QCHORM$disc5 == 1 & QCHORM$lowlim1D5 == 1) |
                (QCHORM$bubb5 == 1 & QCHORM$lowlim1D5 == 1) |
            (QCHORM$smalsamp5 == 1 & QCHORM$lowlim1D5 == 1) |
               (QCHORM$other5 == 1 & QCHORM$lowlim1D5 == 1)] <- NA

sum(is.na(QCHORM$dhe15) | QCHORM$dhe15 == "")
#### 4504 NA after QC

##### DHEA rep 2
sum(is.na(QCHORM$dhe25) | QCHORM$dhe25 == "")
#### 4521 NA before QC 

QCHORM$dhe25[ (QCHORM$contam5 == 1 & QCHORM$low.lim2D5 == 1) |
                (QCHORM$disc5 == 1 & QCHORM$low.lim2D5 == 1) |
                (QCHORM$bubb5 == 1 & QCHORM$low.lim2D5 == 1) |
            (QCHORM$smalsamp5 == 1 & QCHORM$low.lim2D5 == 1) |
               (QCHORM$other5 == 1 & QCHORM$low.lim2D5 == 1)] <- NA

sum(is.na(QCHORM$dhe25) | QCHORM$dhe25 == "")
#### 4524 NA after QC

##### EST rep 1 
sum(is.na(QCHORM$est15) | QCHORM$est15 == "")
#### 8435 NA before QC

QCHORM$est15[ (QCHORM$contam5 == 1 & QCHORM$lowlim1E5 == 1) |
                (QCHORM$disc5 == 1 & QCHORM$lowlim1E5 == 1) |
                (QCHORM$bubb5 == 1 & QCHORM$lowlim1E5 == 1) |
            (QCHORM$smalsamp5 == 1 & QCHORM$lowlim1E5 == 1) |
               (QCHORM$other5 == 1 & QCHORM$lowlim1E5 == 1)] <- NA

sum(is.na(QCHORM$est15) | QCHORM$est15 == "")
#### 8437 NA after QC

##### EST rep 2 
sum(is.na(QCHORM$est25) | QCHORM$est25 == "")
#### 8454 NA before QC

QCHORM$est25[ (QCHORM$contam5 == 1 & QCHORM$lowlim2E5 == 1) |
                (QCHORM$disc5 == 1 & QCHORM$lowlim2E5 == 1) |
                (QCHORM$bubb5 == 1 & QCHORM$lowlim2E5 == 1) |
            (QCHORM$smalsamp5 == 1 & QCHORM$lowlim2E5 == 1) |
               (QCHORM$other5 == 1 & QCHORM$lowlim2E5 == 1)] <- NA

sum(is.na(QCHORM$est25) | QCHORM$est25 == "")
#### 8455 NA after QC

##### TEST rep 1 
sum(is.na(QCHORM$tes15) | QCHORM$tes15 == "")
#### 4443 NA before QC

QCHORM$tes15[ (QCHORM$contam5 == 1 & QCHORM$lowlim1T5 == 1) |
                (QCHORM$disc5 == 1 & QCHORM$lowlim1T5 == 1) |
                (QCHORM$bubb5 == 1 & QCHORM$lowlim1T5 == 1) |
            (QCHORM$smalsamp5 == 1 & QCHORM$lowlim1T5 == 1) |
               (QCHORM$other5 == 1 & QCHORM$lowlim1T5 == 1)] <- NA

sum(is.na(QCHORM$tes15) | QCHORM$tes15 == "")
#### 4443 NA after QC

##### TEST rep 2 
sum(is.na(QCHORM$tes25) | QCHORM$tes25 == "")
#### 4445 NA before QC

QCHORM$tes25[ (QCHORM$contam5 == 1 & QCHORM$lowlim2T5 == 1) |
                (QCHORM$disc5 == 1 & QCHORM$lowlim2T5 == 1) |
                (QCHORM$bubb5 == 1 & QCHORM$lowlim2T5 == 1) |
            (QCHORM$smalsamp5 == 1 & QCHORM$lowlim2T5 == 1) |
               (QCHORM$other5 == 1 & QCHORM$lowlim2T5 == 1)] <- NA

sum(is.na(QCHORM$tes25) | QCHORM$tes25 == "")
#### 4445  NA after QC



##########################################################################
#################### WAVE 5 QC Concerns COMPLETE ######################### 
##########################################################################

##### DHEA rep 1 
sum(is.na(QCHORM$dhe17) | QCHORM$dhe17 == "")
#### 9906 NA before QC 

QCHORM$dhe17[ (QCHORM$contam7 == 1 & QCHORM$lowlim1D7 == 1) |
                (QCHORM$disc7 == 1 & QCHORM$lowlim1D7 == 1) |
                (QCHORM$bubb7 == 1 & QCHORM$lowlim1D7 == 1) |
            (QCHORM$smalsamp7 == 1 & QCHORM$lowlim1D7 == 1) |
               (QCHORM$other7 == 1 & QCHORM$lowlim1D7 == 1)] <- NA

sum(is.na(QCHORM$dhe17) | QCHORM$dhe17 == "")
#### 9907 NA after QC

##### DHEA rep 2
sum(is.na(QCHORM$dhe27) | QCHORM$dhe27 == "")
#### 9911 NA before QC 

QCHORM$dhe27[ (QCHORM$contam7 == 1 & QCHORM$low.lim2D7== 1) |
                (QCHORM$disc7 == 1 & QCHORM$low.lim2D7 == 1) |
                (QCHORM$bubb7 == 1 & QCHORM$low.lim2D7 == 1) |
            (QCHORM$smalsamp7 == 1 & QCHORM$low.lim2D7 == 1) |
               (QCHORM$other7 == 1 & QCHORM$low.lim2D7 == 1)] <- NA

sum(is.na(QCHORM$dhe27) | QCHORM$dhe27 == "")
#### 9912 NA after QC

##### EST rep 1 
sum(is.na(QCHORM$est17) | QCHORM$est17 == "")
#### 10902 NA before QC

QCHORM$est17[ (QCHORM$contam7 == 1 & QCHORM$lowlim1E7 == 1) |
                (QCHORM$disc7 == 1 & QCHORM$lowlim1E7 == 1) |
                (QCHORM$bubb7 == 1 & QCHORM$lowlim1E7 == 1) |
            (QCHORM$smalsamp7 == 1 & QCHORM$lowlim1E7 == 1) |
               (QCHORM$other7 == 1 & QCHORM$lowlim1E7 == 1)] <- NA

sum(is.na(QCHORM$est17) | QCHORM$est17 == "")
#### 10902 NA after QC

##### EST rep 2 
sum(is.na(QCHORM$est27))
#### 10912 NA before QC

QCHORM$est27[ (QCHORM$contam7 == 1 & QCHORM$lowlim2E7 == 1) |
                (QCHORM$disc7 == 1 & QCHORM$lowlim2E7 == 1) |
                (QCHORM$bubb7 == 1 & QCHORM$lowlim2E7 == 1) |
            (QCHORM$smalsamp7 == 1 & QCHORM$lowlim2E7 == 1) |
               (QCHORM$other7 == 1 & QCHORM$lowlim2E7 == 1)] <- NA

sum(is.na(QCHORM$est27) | QCHORM$est27 == "")
#### 10911 NA after QC

##### TEST rep 1 
sum(is.na(QCHORM$tes17) | QCHORM$tes17 == "")
#### 9894 NA before QC

QCHORM$tes17[ (QCHORM$contam7 == 1 & QCHORM$lowlim1T7 == 1) |
                (QCHORM$disc7 == 1 & QCHORM$lowlim1T7 == 1) |
                (QCHORM$bubb7 == 1 & QCHORM$lowlim1T7 == 1) |
            (QCHORM$smalsamp7 == 1 & QCHORM$lowlim1T7 == 1) |
               (QCHORM$other7 == 1 & QCHORM$lowlim1T7 == 1)] <- NA

sum(is.na(QCHORM$tes17) | QCHORM$tes17 == "")
#### 9894 NA after QC

##### TEST rep 2 
sum(is.na(QCHORM$tes27) | QCHORM$tes27 == "")
#### 9895 NA before QC

QCHORM$tes27[ (QCHORM$contam7 == 1 & QCHORM$lowlim2T7 == 1) |
                (QCHORM$disc7 == 1 & QCHORM$lowlim2T7 == 1) |
                (QCHORM$bubb7 == 1 & QCHORM$lowlim2T7 == 1) |
            (QCHORM$smalsamp7 == 1 & QCHORM$lowlim2T7 == 1) |
               (QCHORM$other7 == 1 & QCHORM$lowlim2T7 == 1)] <- NA

sum(is.na(QCHORM$tes27) | QCHORM$tes27 == "")
#### 9895  NA after QC



##########################################################################
#################### WAVE 7 QC Concerns COMPLETE ######################### 
##########################################################################

##### DHEA rep 1 
sum(is.na(QCHORM$dhe19) | QCHORM$dhe19 == "")
#### 9366 NA before QC 

QCHORM$dhe19[ (QCHORM$contam9 == 1 & QCHORM$lowlim1D9 == 1) |
                (QCHORM$disc9 == 1 & QCHORM$lowlim1D9 == 1) |
                (QCHORM$bubb9 == 1 & QCHORM$lowlim1D9 == 1) |
            (QCHORM$smalsamp9 == 1 & QCHORM$lowlim1D9 == 1) |
               (QCHORM$other9 == 1 & QCHORM$lowlim1D9 == 1)] <- NA

sum(is.na(QCHORM$dhe19) | QCHORM$dhe19 == "")
#### 9366 NA after QC

##### DHEA rep 2
sum(is.na(QCHORM$dhe29) | QCHORM$dhe29 == "")
#### 9371 NA before QC 

QCHORM$dhe29[ (QCHORM$contam9 == 1 & QCHORM$low.lim2D9 == 1) |
                (QCHORM$disc9 == 1 & QCHORM$low.lim2D9 == 1) |
                (QCHORM$bubb9 == 1 & QCHORM$low.lim2D9 == 1) |
            (QCHORM$smalsamp9 == 1 & QCHORM$low.lim2D9 == 1) |
               (QCHORM$other9 == 1 & QCHORM$low.lim2D9 == 1)] <- NA

sum(is.na(QCHORM$dhe29) | QCHORM$dhe29 == "")
#### 9371 NA after QC

##### EST rep 1 
sum(is.na(QCHORM$est19) | QCHORM$est19 == "")
#### 10663 NA before QC

QCHORM$est19[ (QCHORM$contam9 == 1 & QCHORM$lowlim1E9 == 1) |
                (QCHORM$disc9 == 1 & QCHORM$lowlim1E9 == 1) |
                (QCHORM$bubb9 == 1 & QCHORM$lowlim1E9 == 1) |
            (QCHORM$smalsamp9 == 1 & QCHORM$lowlim1E9 == 1) |
               (QCHORM$other9 == 1 & QCHORM$lowlim1E9 == 1)] <- NA

sum(is.na(QCHORM$est19) | QCHORM$est19 == "")
#### 10663 NA after QC

##### EST rep 2 
sum(is.na(QCHORM$est29) | QCHORM$est29 == "")
#### 10680 NA before QC

QCHORM$est29[ (QCHORM$contam9 == 1 & QCHORM$lowlim2E9 == 1) |
                (QCHORM$disc9 == 1 & QCHORM$lowlim2E9 == 1) |
                (QCHORM$bubb9 == 1 & QCHORM$lowlim2E9 == 1) |
            (QCHORM$smalsamp9 == 1 & QCHORM$lowlim2E9 == 1) |
               (QCHORM$other9 == 1 & QCHORM$lowlim2E9 == 1)] <- NA

sum(is.na(QCHORM$est29) | QCHORM$est29 == "")
#### 10680 NA after QC

##### TEST rep 1 
sum(is.na(QCHORM$tes19) | QCHORM$tes19 == "")
#### 9359 NA before QC

QCHORM$tes19[ (QCHORM$contam9 == 1 & QCHORM$lowlim1T9 == 1) |
                (QCHORM$disc9 == 1 & QCHORM$lowlim1T9 == 1) |
                (QCHORM$bubb9 == 1 & QCHORM$lowlim1T9 == 1) |
            (QCHORM$smalsamp9 == 1 & QCHORM$lowlim1T9 == 1) |
               (QCHORM$other9 == 1 & QCHORM$lowlim1T9 == 1)] <- NA

sum(is.na(QCHORM$tes19) | QCHORM$tes19 == "")
#### 9359 NA after QC

##### TEST rep 2 
sum(is.na(QCHORM$tes29) | QCHORM$tes29 == "")
#### 9360 NA before QC

QCHORM$tes29[ (QCHORM$contam9 == 1 & QCHORM$lowlim2T9 == 1) |
                (QCHORM$disc9 == 1 & QCHORM$lowlim2T9 == 1) |
                (QCHORM$bubb9 == 1 & QCHORM$lowlim2T9 == 1) |
            (QCHORM$smalsamp9 == 1 & QCHORM$lowlim2T9 == 1) |
               (QCHORM$other9 == 1 & QCHORM$lowlim2T9 == 1)] <- NA

sum(is.na(QCHORM$tes29) | QCHORM$tes29 == "")
#### 9360  NA after QC


##########################################################################
#################### WAVE 9 QC Concerns COMPLETE ######################### 
##########################################################################

write.csv(QCHORM, "QCHORM_5.28.24.csv" ,row.names=FALSE, na="")

##########################################################################
####################  ALL QC Concerns COMPLETE   ######################### 
##########################################################################

estracheck <- read.csv("QCHORM_5.28.24.csv")

weirdW1 <- estracheck[estracheck$ysex == 1 & !is.na(estracheck$est11), ]
####30
estracheck1 <- anti_join(estracheck, weirdW1, by = "subid")


weirdW1.2 <- estracheck1[estracheck1$ysex == 1 & !is.na(estracheck1$est21), ]

weirdw3 <- estracheck1[estracheck1$ysex == 1 & !is.na(estracheck1$est13), ]
####19
estracheck2 <- anti_join(estracheck1, weirdw3, by = "subid")

weirdw3.2 <- estracheck2[estracheck2$ysex == 1 & !is.na(estracheck2$est23), ]



weirdw5 <- estracheck2[estracheck2$ysex == 1 & !is.na(estracheck2$est15), ]
####9
estracheck3 <- anti_join(estracheck2, weirdw5, by = "subid")

weirdw5.2 <- estracheck3[estracheck3$ysex == 1 & !is.na(estracheck3$est25), ]




weirdw7 <- estracheck3[estracheck3$ysex == 1 & !is.na(estracheck3$est17), ]
####1
estracheck4 <- anti_join(estracheck3, weirdw7, by = "subid")

weirdw7.2 <- estracheck4[estracheck4$ysex == 1 & !is.na(estracheck4$est25), ]



weirdw9 <- estracheck4[estracheck4$ysex == 1 & !is.na(estracheck4$est19), ]

estracheck5 <- anti_join(estracheck4, weirdw9, by = "subid")

weirdw9.2 <- estracheck5[estracheck5$ysex == 1 & !is.na(estracheck5$est29), ]


write.csv(estracheck5, "estracheck.csv" ,row.names=FALSE, na="")


##########################################################################
####################   No boys with Estradiol    ######################### 
##########################################################################


###################Outliers step optional, consider winsorization####################
Outliers <- function(x) {
  median_x <- median(x, na.rm = TRUE)
  mad_x <- mad(x, na.rm = TRUE)
  threshold <- 3 * mad_x
  
  for (i in 1:length(x)) {
    if (!is.na(x[i])) {
      if (abs(x[i] - median_x) > threshold) {
        x[i] <- NA
      }
    }
  }
  
  return(x)
}

trimming <- read.csv("estracheck_5.28.24.csv")
sum(is.na(trimming$dhe11) | trimming$dhe11 == "")
############472 empty 
names(trimming)
str(trimming)

trimming$dhe11 <- as.numeric(trimming$dhe11)




HormTrim <- trimming %>% mutate(across(90:99,FuckOutliers),
                                across(115:124,FuckOutliers),
                                across(140:149,FuckOutliers))


write.csv(HormTrim, "trimmed.csv" ,row.names=FALSE, na="")
sum(is.na(trimming$dhe11) | trimming$dhe11 == "")
###474 empty
##########################################################################
####################          No Outliers        ######################### 
##########################################################################

allempty<- read.csv("trimmed.csv")

everything <- allempty[rowSums(is.na(allempty[c("dhe11", 
                                          "dhe21",
                                          "dhe13",
                                          "dhe23",
                                          "dhe15",
                                          "dhe25",
                                          "dhe17",
                                          "dhe27",
                                          "dhe19",
                                          "dhe29",
                                          "est11",
                                          "est21",
                                          "est13",
                                          "est23",
                                          "est15",
                                          "est25",
                                          "est17",
                                          "est27",
                                          "est19",
                                          "est29",
                                          "tes11",
                                          "tes21",
                                          "tes13",
                                          "tes23",
                                          "tes15",
                                          "tes25",
                                          "tes17",
                                          "tes27",
                                          "tes19",
                                          "tes29")])) == 30 , ]
####### 24

allhormed <- anti_join(allempty, everything, by = "subid")


write.csv(allhormed, "nohormsdropped.csv" ,row.names=FALSE, na="")


#################################################################################
#################### dropped participants w no hormones ######################### 
#################################################################################

behavior <- read.csv("nohormsdropped.csv")
names(behavior)


caff1NA <- is.na(behavior[["caff1"]]) | behavior$caff1 == ""

Hormones1 <-  c("dhe11", "dhe21", "est11", "est21", "tes11", "tes21")

hashormones1 <- apply(behavior[Hormones1], 1, function(row) any(!is.na(row)))  

nocaffhorm <- caff1NA & hashormones1
table(nocaffhorm)
#### true = 27 

behavior[nocaffhorm, Hormones1 ] <- NA

sum(is.na(behavior$caff3))

caff3NA <- is.na(behavior[["caff3"]])  | behavior$caff3 == ""

Hormones3 <-  c("dhe13", "dhe23", "est13", "est23", "tes13", "tes23")

hashormones3 <- apply(behavior[Hormones3], 1, function(row) any(!is.na(row)))  

nocaffhorm3 <- caff3NA & hashormones3
table(nocaffhorm3)
#### true = 12

behavior[nocaffhorm3, Hormones3] <- NA

sum(is.na(behavior$caff5))

caff5NA <- is.na(behavior[["caff5"]])  | behavior$caff5 == ""

Hormones5 <-  c("dhe15", "dhe25", "est15", "est25", "tes15", "tes25")

hashormones5 <- apply(behavior[Hormones5], 1, function(row) any(!is.na(row)))  

nocaffhorm5 <- caff5NA & hashormones5
table(nocaffhorm5)
#### true = 5

behavior[nocaffhorm5, Hormones5] <- NA

sum(is.na(behavior$caff7))

caff7NA <- is.na(behavior[["caff7"]])  | behavior$caff7 == ""

Hormones7 <-  c("dhe17", "dhe27", "est17", "est27", "tes17", "tes27")

hashormones7 <- apply(behavior[Hormones7], 1, function(row) any(!is.na(row)))  

nocaffhorm7 <- caff7NA & hashormones7
table(nocaffhorm7)
#### true = 3

behavior[nocaffhorm7, Hormones7] <- NA

sum(is.na(behavior$caff9))

caff9NA <- is.na(behavior[["caff9"]])  | behavior$caff9 == ""

Hormones9 <-  c("dhe19", "dhe29", "est19", "est29", "tes19", "tes29")

hashormones9 <- apply(behavior[Hormones9], 1, function(row) any(!is.na(row)))  

nocaffhorm9 <- caff9NA & hashormones9
table(nocaffhorm9)
#### true = 3

behavior[nocaffhorm9, Hormones9] <- NA


write.csv(behavior, "caffcleaned.csv" ,row.names=FALSE, na="")

#################################################################################
#################### missing caffeine control removed   ######################### 
#################################################################################

behave <- read.csv("caffcleaned.csv")
names(behave)

active1NA <- is.na(behave[["active1"]])  | behave$active1 == ""

Hormones1 <-  c("dhe11", "dhe21", "est11", "est21", "tes11", "tes21")

hashormones1 <- apply(behave[Hormones1], 1, function(row) any(!is.na(row)))  

noacthorm <- active1NA & hashormones1
table(noacthorm)
#### true = 12 
behave[noacthorm, Hormones1] <- NA

sum(is.na(behave$active3))

active3NA <- is.na(behave[["active3"]]) | behave$active3 == ""

Hormones3 <-  c("dhe13", "dhe23", "est13", "est23", "tes13", "tes23")

hashormones3 <- apply(behave[Hormones3], 1, function(row) any(!is.na(row)))  

noacthorm3 <- active3NA & hashormones3
table(noacthorm3)
#### true = 1 

behave[noacthorm3, Hormones3] <- NA

sum(is.na(behave$active5))

active5NA <- is.na(behave[["active5"]]) | behave$active5 == "" 

Hormones5 <-  c("dhe15", "dhe25", "est15", "est25", "tes15", "tes25")

hashormones5 <- apply(behave[Hormones5], 1, function(row) any(!is.na(row)))  

noacthorm5 <- active5NA & hashormones5
table(noacthorm5)
#### true = 2

behave[noacthorm5, Hormones5] <- NA

sum(is.na(behave$active7))

active7NA <- is.na(behave[["active7"]]) | behave$active7 == ""

Hormones7 <-  c("dhe17", "dhe27", "est17", "est27", "tes17", "tes27")

hashormones7 <- apply(behave[Hormones7], 1, function(row) any(!is.na(row)))  

noacthorm7 <- active7NA & hashormones7
table(noacthorm7)
#### true = 0

sum(is.na(behave$active9))

active9NA <- is.na(behave[["active9"]]) | behave$active9 == ""

Hormones9 <-  c("dhe19", "dhe29", "est19", "est29", "tes19", "tes29")

hashormones9 <- apply(behave[Hormones9], 1, function(row) any(!is.na(row)))  

noacthorm9 <- active9NA & hashormones9
table(noacthorm9)
#### true = 1

behave[noacthorm9, Hormones9] <- NA

write.csv(behave, "actcleaned.csv" ,row.names=FALSE, na="")

#################################################################################
#################### missing activity control removed   ######################### 
#################################################################################

beh <- read.csv("actcleaned.csv")
names(beh)
sum(is.na(beh$wake1) | beh$wake1 == "")

wake1NA <- is.na(beh[["wake1"]]) | beh$wake1 == ""

Hormones1 <-  c("dhe11", "dhe21", "est11", "est21", "tes11", "tes21")

hashormones1 <- apply(beh[Hormones1], 1, function(row) any(!is.na(row)))  

nowakehorm <- wake1NA & hashormones1
table(nowakehorm)
#### true = 36

beh[nowakehorm, Hormones1] <- NA

sum(is.na(beh$wake3) | beh$wake3 == "")

wake3NA <- is.na(beh[["wake3"]]) | beh$wake3 == ""

Hormones3 <-  c("dhe13", "dhe23", "est13", "est23", "tes13", "tes23")

hashormones3 <- apply(beh[Hormones3], 1, function(row) any(!is.na(row))) 

nowakehorm3 <- wake3NA & hashormones3
table(nowakehorm3)
#### true = 8 
beh[nowakehorm3, Hormones3] <- NA

sum(is.na(beh$wake5) | beh$wake5 == "")

wake5NA <- is.na(beh[["wake5"]]) | beh$wake5 == ""

Hormones5 <-  c("dhe15", "dhe25", "est15", "est25", "tes15", "tes25")

hashormones5 <- apply(beh[Hormones5], 1, function(row) any(!is.na(row)))  

nowakehorm5 <- wake5NA & hashormones5
table(nowakehorm5)
#### true = 1
beh[nowakehorm5, Hormones5] <- NA

sum(is.na(beh$wake7) | beh$wake7 == "")

wake7NA <- is.na(beh[["wake7"]]) | beh$wake7 == ""

Hormones7 <-  c("dhe17", "dhe27", "est17", "est27", "tes17", "tes27")

hashormones7 <- apply(beh[Hormones7], 1, function(row) any(!is.na(row)))  

nowakehorm7 <- wake7NA & hashormones7
table(nowakehorm7)
#### true = 2 
beh[nowakehorm7, Hormones7] <- NA

sum(is.na(beh$wake9) | beh$wake9 == "")

wake9NA <- is.na(beh[["wake9"]]) | beh$wake9 == ""

Hormones9 <-  c("dhe19", "dhe29", "est19", "est29", "tes19", "tes29")

hashormones9 <- apply(beh[Hormones9], 1, function(row) any(!is.na(row)))  

nowakehorm9 <- wake9NA & hashormones9
table(nowakehorm9)
#### true = 1 
beh[nowakehorm9, Hormones9] <- NA



write.csv(beh, "wakecleaned.csv" ,row.names=FALSE, na="")
#################################################################################
####################      missing wake time removed     ######################### 
#################################################################################
install.packages("chron")
library(chron)
convert_to_minutes <- function(time_str) {
  time_parts <- strsplit(time_str, ":")[[1]]
  as.numeric(time_parts[1]) * 60 + as.numeric(time_parts[2])
}

length <- read.csv("wakecleaned.csv")
names(length)
length$start1_min <- sapply(length$start1, convert_to_minutes)
length$end1_min <- sapply(length$end1, convert_to_minutes)
length$samptime <- length$end1_min - length$start1_min

sum(is.na(length$samptime) | length$samptime == "")

ST1NA <- is.na(length[["samptime"]]) | length$samptime == ""

Hormones1 <-  c("dhe11", "dhe21", "est11", "est21", "tes11", "tes21")

hashormones1 <- apply(length[Hormones1], 1, function(row) any(!is.na(row)))  

nosthorm <- ST1NA & hashormones1
table(nosthorm)
#### true = 11

length[nosthorm, Hormones1] <- NA

length$start3_min <- sapply(length$start3, convert_to_minutes)
length$end3_min <- sapply(length$end3, convert_to_minutes)
length$samptime3 <- length$end3_min - length$start3_min

sum(is.na(length$samptime3) | length$samptime3 == "")

ST3NA <- is.na(length[["samptime3"]]) | length$samptime3 == ""

Hormones3 <-  c("dhe13", "dhe23", "est13", "est23", "tes13", "tes23")

hashormones3 <- apply(length[Hormones3], 1, function(row) any(!is.na(row)))  

nosthorm3 <- ST3NA & hashormones3
table(nosthorm3)
#### true = 19 

length[nosthorm3, Hormones3] <- NA



length$start5_min <- sapply(length$start5, convert_to_minutes)
length$end5_min <- sapply(length$end5, convert_to_minutes)
length$samptime5 <- length$end5_min - length$start5_min

sum(is.na(length$samptime5) | length$samptime5 == "")

ST5NA <- is.na(length[["samptime5"]]) | length$samptime5 == ""

Hormones5 <-  c("dhe15", "dhe25", "est15", "est25", "tes15", "tes25")

hashormones5 <- apply(length[Hormones5], 1, function(row) any(!is.na(row)))  

nosthorm5 <- ST5NA & hashormones5
table(nosthorm5)
#### true = 12

length[nosthorm5, Hormones5] <- NA




length$start7_min <- sapply(length$start7, convert_to_minutes)
length$end7_min <- sapply(length$end7, convert_to_minutes)
length$samptime7 <- length$end7_min - length$start7_min

sum(is.na(length$samptime7) | length$samptime7 == "")

ST7NA <- is.na(length[["samptime7"]]) | length$samptime7 == ""

Hormones7 <-  c("dhe17", "dhe27", "est17", "est27", "tes17", "tes27")

hashormones7 <- apply(length[Hormones7], 1, function(row) any(!is.na(row)))  

nosthorm7 <- ST7NA & hashormones7
table(nosthorm7)
#### true = 0 



length$start9_min <- sapply(length$start9, convert_to_minutes)
length$end9_min <- sapply(length$end9, convert_to_minutes)
length$samptime9 <- length$end9_min - length$start9_min

sum(is.na(length$samptime9) | length$samptime9 == "")

ST9NA <- is.na(length[["samptime9"]]) | length$samptime9 == ""

Hormones9 <-  c("dhe19", "dhe29", "est19", "est29", "tes19", "tes29")

hashormones9 <- apply(length[Hormones9], 1, function(row) any(!is.na(row)))  

nosthorm9 <- ST9NA & hashormones9
table(nosthorm9)
#### true = 3

length[nosthorm9, Hormones9] <- NA

write.csv(length, "lengthcleaned.csv" ,row.names=FALSE, na="")


#################################################################################
####################   missing sample length removed    ######################### 
#################################################################################

beha <- read.csv("lengthcleaned.csv")
names(beha)
sum(is.na(beha$freeze1) | beha$freeze1 == "")

freeze1NA <- is.na(beha[["freeze1"]]) | beha$freeze1 == ""

Hormones1 <-  c("dhe11", "dhe21", "est11", "est21", "tes11", "tes21")

hashormones1 <- apply(beha[Hormones1], 1, function(row) any(!is.na(row)))  

nofreezehorm <- freeze1NA & hashormones1
table(nofreezehorm)
#### true = 16

beha[nofreezehorm, Hormones1] <- NA


sum(is.na(beha$freeze3) | beha$freeze3 == "")

freeze3NA <- is.na(beha[["freeze3"]]) | beha$freeze3 == ""

Hormones3 <-  c("dhe13", "dhe23", "est13", "est23", "tes13", "tes23")

hashormones3 <- apply(beha[Hormones3], 1, function(row) any(!is.na(row))) 

nofreezehorm3 <- freeze3NA & hashormones3

table(nofreezehorm3)
#### true = 7
beha[nofreezehorm3, Hormones3] <- NA

sum(is.na(beha$freeze5) | beha$freeze5 == "")

freeze5NA <- is.na(beha[["freeze5"]]) | beha$freeze5 == ""

Hormones5 <-  c("dhe15", "dhe25", "est15", "est25", "tes15", "tes25")

hashormones5 <- apply(beha[Hormones5], 1, function(row) any(!is.na(row)))  

nofreezehorm5 <- freeze5NA & hashormones5
table(nofreezehorm5)
#### true = 0

sum(is.na(beha$freeze7) | beha$freeze7 == "")

freeze7NA <- is.na(beha[["freeze7"]]) | beha$freeze7 == ""

Hormones7 <-  c("dhe17", "dhe27", "est17", "est27", "tes17", "tes27")

hashormones7 <- apply(beha[Hormones7], 1, function(row) any(!is.na(row)))  

nofreezehorm7 <- freeze7NA & hashormones7
table(nofreezehorm7)
#### true = 1
beha[nofreezehorm7, Hormones7] <- NA

sum(is.na(beha$freeze9) | beha$freeze9 == "")

freeze9NA <- is.na(beha[["freeze9"]]) | beha$freeze9 == ""

Hormones9 <-  c("dhe19", "dhe29", "est19", "est29", "tes19", "tes29")

hashormones9 <- apply(beha[Hormones9], 1, function(row) any(!is.na(row)))  

nofreezehorm9 <- freeze9NA & hashormones9
table(nofreezehorm9)
#### true = 1 
beha[nofreezehorm9, Hormones9] <- NA

write.csv(beha, "freezecleaned.csv" ,row.names=FALSE, na="")

#################################################################################
####################     missing freeze time removed    ######################### 
#################################################################################

strtwke <- read.csv("freezecleaned.csv")
names(strtwke)
library(chron)
library(dplyr)
convert_to_minutes <- function(time_str) {
  time_parts <- strsplit(time_str, ":")[[1]]
  as.numeric(time_parts[1]) * 60 + as.numeric(time_parts[2])
}

strtwke$wake1_min <- sapply(strtwke$wake1, convert_to_minutes)

strtwke_discrep <- strtwke %>% filter(wake1_min > start1_min)
print(strtwke_discrep$wake1)
print(strtwke_discrep$start1)
##### 16 
Hormones1 <- c("dhe11", "dhe21", "est11", "est21", "tes11", "tes21")
strtwke[which(strtwke$wake1_min > strtwke$start1_min), Hormones1] <- NA

strtwke$wake3_min <- sapply(strtwke$wake3, convert_to_minutes)

strtwke_discrep3 <- strtwke %>% filter(wake3_min > start3_min)
print(strtwke_discrep3$wake3)
print(strtwke_discrep3$start3)
##### 29
Hormones3 <- c("dhe13", "dhe23", "est13", "est23", "tes13", "tes23")
strtwke[which(strtwke$wake3_min > strtwke$start3_min), Hormones3] <- NA

strtwke$wake5_min <- sapply(strtwke$wake5, convert_to_minutes)

strtwke_discrep5 <- strtwke %>% filter(wake5_min > start5_min)
print(strtwke_discrep5$wake5)
print(strtwke_discrep5$start5)
##### 16
Hormones5 <- c("dhe15", "dhe25", "est15", "est25", "tes15", "tes25")
strtwke[which(strtwke$wake5_min > strtwke$start5_min), Hormones5] <- NA

strtwke$wake7_min <- sapply(strtwke$wake7, convert_to_minutes)

strtwke_discrep7 <- strtwke %>% filter(wake7_min > start7_min)
print(strtwke_discrep7$wake7)
print(strtwke_discrep7$start7)
##### 4
Hormones7 <- c("dhe17", "dhe27", "est17", "est27", "tes17", "tes27")
strtwke[which(strtwke$wake7_min > strtwke$start7_min), Hormones7] <- NA

strtwke$wake9_min <- sapply(strtwke$wake9, convert_to_minutes)

strtwke_discrep9 <- strtwke %>% filter(wake9_min > start9_min)
print(strtwke_discrep9$wake9)
print(strtwke_discrep9$start9)
##### 9
Hormones9 <- c("dhe19", "dhe29", "est19", "est29", "tes19", "tes29")
strtwke[which(strtwke$wake9_min > strtwke$start9_min), Hormones9] <- NA

write.csv(strtwke, "strtdiscrepclean.csv" ,row.names=FALSE, na="")
#################################################################################
####################     start time before wake time NA  ######################## 
#################################################################################

endfreeze <- read.csv("strtdiscrepclean.csv")
names(endfreeze)
library(chron)
library(dplyr)
convert_to_minutes <- function(time_str) {
  time_parts <- strsplit(time_str, ":")[[1]]
  as.numeric(time_parts[1]) * 60 + as.numeric(time_parts[2])
}

endfreeze$freeze1_min <- sapply(endfreeze$freeze1, convert_to_minutes)

endfreeze_discrep <- endfreeze %>% filter(end1_min > freeze1_min)
print(endfreeze_discrep$end1)
print(endfreeze_discrep$freeze1)
##### 131 
Hormones1 <- c("dhe11", "dhe21", "est11", "est21", "tes11", "tes21")
endfreeze[which(endfreeze$end1_min > endfreeze$freeze1_min), Hormones1] <- NA

endfreeze$freeze3_min <- sapply(endfreeze$freeze3, convert_to_minutes)

endfreeze_discrep3 <- endfreeze %>% filter(end3_min > freeze3_min)
print(endfreeze_discrep3$end3)
print(endfreeze_discrep3$freeze3)
##### 91 
Hormones3 <- c("dhe13", "dhe23", "est13", "est23", "tes13", "tes23")
endfreeze[which(endfreeze$end3_min > endfreeze$freeze3_min), Hormones3] <- NA

endfreeze$freeze5_min <- sapply(endfreeze$freeze5, convert_to_minutes)

endfreeze_discrep5 <- endfreeze %>% filter(end5_min > freeze5_min)
print(endfreeze_discrep5$end5)
print(endfreeze_discrep5$freeze5)
##### 44
Hormones5 <- c("dhe15", "dhe25", "est15", "est25", "tes15", "tes25")
endfreeze[which(endfreeze$end5_min > endfreeze$freeze5_min), Hormones5] <- NA

endfreeze$freeze7_min <- sapply(endfreeze$freeze7, convert_to_minutes)

endfreeze_discrep7 <- endfreeze %>% filter(end7_min > freeze7_min)
print(endfreeze_discrep7$end7)
print(endfreeze_discrep7$freeze7)
##### 25
Hormones7 <- c("dhe17", "dhe27", "est17", "est27", "tes17", "tes27")
endfreeze[which(endfreeze$end7_min > endfreeze$freeze7_min), Hormones7] <- NA

endfreeze$freeze9_min <- sapply(endfreeze$freeze9, convert_to_minutes)

endfreeze_discrep9 <- endfreeze %>% filter(end9_min > freeze9_min)
print(endfreeze_discrep9$end9)
print(endfreeze_discrep9$freeze9)
##### 42
Hormones9 <- c("dhe19", "dhe29", "est19", "est29", "tes19", "tes29")
endfreeze[which(endfreeze$end9_min > endfreeze$freeze9_min), Hormones9] <- NA

write.csv(endfreeze, "Freezediscrep.csv" ,row.names=FALSE, na="")

#################################################################################
####################    FREEZE TIME BEFORE END TIME NA   ######################## 
#################################################################################
library(dplyr)
BC <- read.csv("Freezediscrep.csv")
names(BC)
table(BC$BCYR1)
##### 3 yes
BC <- subset (BC,BC$BCYR1 !=1 | is.na(BC$BCYR1))

table(BC$BCYR3)
##### 15 yes 
BC <- subset (BC,BC$BCYR3 !=1 | is.na(BC$BCYR3))

table(BC$BCYR5)
##### 29 yes
BC <- subset (BC,BC$BCYR5 !=1 | is.na(BC$BCYR5))

table(BC$BCYR7)
##### 29 yes 
BC <- subset (BC,BC$BCYR7 !=1 | is.na(BC$BCYR7))

table(BC$BCYR9)
#### 91 yes 
BC <- subset (BC,BC$BCYR9 !=1 | is.na(BC$BCYR9))

#################################################################################
####################    DROPPED PARTICIPANTS ON BIRTH CNTRL  ####################
#################################################################################

sum(is.na(BC$dhe11))

BC$dhe11[ BC$QNSD11 == 1] <-NA 
BC$dhe11[ BC$NDD11 == 1] <-NA 
sum(is.na(BC$dhe21))

BC$dhe21[ BC$QNSD21 == 1] <-NA 
BC$dhe21[ BC$NDD21 == 1] <-NA 
sum(is.na(BC$dhe13))

BC$dhe13[ BC$QNSD13 == 1] <-NA 
BC$dhe13[ BC$NDD13 == 1] <-NA 
sum(is.na(BC$dhe23))

BC$dhe23[ BC$QNSD23 == 1] <-NA 
BC$dhe23[ BC$NDD23 == 1] <-NA 
sum(is.na(BC$dhe15))

BC$dhe15[ BC$QNSD15 == 1] <-NA 
BC$dhe15[ BC$NDD15 == 1] <-NA 
sum(is.na(BC$dhe25))

BC$dhe25[ BC$QNSD25 == 1] <-NA 
BC$dhe25[ BC$NDD25 == 1] <-NA 
sum(is.na(BC$dhe17))

BC$dhe17[ BC$QNSD17 == 1] <-NA 
BC$dhe17[ BC$NDD17 == 1] <-NA
sum(is.na(BC$dhe27))

BC$dhe27[ BC$QNSD27 == 1] <-NA 
BC$dhe27[ BC$NDD27 == 1] <-NA
sum(is.na(BC$dhe19))

BC$dhe19[ BC$QNSD19 == 1] <-NA 
BC$dhe19[ BC$NDD19 == 1] <-NA
sum(is.na(BC$est29))

BC$dhe19[ BC$QNSD29 == 1] <-NA 
BC$dhe19[ BC$NDD29 == 1] <-NA
sum(is.na(BC$est11))

BC$est11[BC$QNSE11 == 1] <-NA
BC$est11[BC$NDE11 == 1] <-NA

sum(is.na(BC$est21))

BC$est21[BC$QNSE21 == 1] <-NA
BC$est21[BC$NDE21 == 1] <-NA

sum(is.na(BC$est13))

BC$est13[BC$QNSE13 == 1] <-NA
BC$est13[BC$NDE13 == 1] <-NA

sum(is.na(BC$est23))

BC$est23[BC$QNSE23 == 1] <-NA
BC$est23[BC$NDE23 == 1] <-NA

sum(is.na(BC$est15))

BC$est15[BC$QNSE15 == 1] <-NA
BC$est15[BC$NDE15 == 1] <-NA

sum(is.na(BC$est25))

BC$est25[BC$QNSE25 == 1] <-NA
BC$est25[BC$NDE25 == 1] <-NA

sum(is.na(BC$est17))

BC$est17[BC$QNSE17 == 1] <-NA
BC$est17[BC$NDE17 == 1] <-NA

sum(is.na(BC$est27))

BC$est27[BC$QNSE27 == 1] <-NA
BC$est27[BC$NDE27 == 1] <-NA

sum(is.na(BC$est19))

BC$est19[BC$QNSE19 == 1] <-NA
BC$est19[BC$NDE19 == 1] <-NA

sum(is.na(BC$est29))

BC$est29[BC$QNSE29 == 1] <-NA
BC$est29[BC$NDE29 == 1] <-NA
sum(is.na(BC$tes11))

BC$tes11[BC$QNST11 == 1] <-NA
BC$tes11[BC$NDT11 == 1] <-NA

sum(is.na(BC$tes21))

BC$tes21[BC$QNST21 == 1] <-NA
BC$tes21[BC$NDT21 == 1] <-NA

sum(is.na(BC$tes13))

BC$tes13[BC$QNST13 == 1] <-NA
BC$tes13[BC$NDT13 == 1] <-NA

sum(is.na(BC$tes23))

BC$tes23[BC$QNST23 == 1] <-NA
BC$tes23[BC$NDT23 == 1] <-NA

sum(is.na(BC$tes15))

BC$tes15[BC$QNSE15 == 1] <-NA
BC$tes15[BC$NDE15 == 1] <-NA

sum(is.na(BC$tes25))

BC$tes25[BC$QNST25 == 1] <-NA
BC$tes25[BC$NDT25 == 1] <-NA

sum(is.na(BC$tes17))

BC$tes17[BC$QNST17 == 1] <-NA
BC$tes17[BC$NDT17 == 1] <-NA

sum(is.na(BC$tes27))

BC$tes27[BC$QNST27 == 1] <-NA
BC$tes27[BC$NDT27 == 1] <-NA

sum(is.na(BC$tes19))

BC$tes19[BC$QNST19 == 1] <-NA
BC$tes19[BC$NDT19 == 1] <-NA

sum(is.na(BC$tes29))

BC$tes29[BC$QNST29 == 1] <-NA
BC$tes29[BC$NDT29 == 1] <-NA


#####################################################################
####################    ALL concerns complete    ####################
#####################################################################

write.csv(BC, "FINALCLEAN.csv" ,row.names=FALSE, na="")
