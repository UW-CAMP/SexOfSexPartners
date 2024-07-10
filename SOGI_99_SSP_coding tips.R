
# Old version

yrbs_2015 <- yrbs_final[yrbs_final$year == "2015",]
yrbs_2017 <- yrbs_final[yrbs_final$year == "2017",]
yrbs_2019 <- yrbs_final[yrbs_final$year == "2019",]
yrbs_2021 <- yrbs_final[yrbs_final$year == "2021",]

yrbs_female15 <- yrbs_2015[yrbs_2015$sex == "Female", ]
yrbs_female17 <- yrbs_2017[yrbs_2017$sex == "Female", ]
yrbs_female19 <- yrbs_2019[yrbs_2019$sex == "Female", ]
yrbs_female21 <- yrbs_2021[yrbs_2021$sex == "Female", ]

table(yrbs_2015$sex, yrbs_2015$sex_of_sp)
round(100* prop.table(table(yrbs_2015$sex, yrbs_2015$sex_of_sp)))
table(yrbs_2017$sex, yrbs_2017$sex_of_sp)
round(100* prop.table(table(yrbs_2017$sex, yrbs_2017$sex_of_sp)))
table(yrbs_2019$sex, yrbs_2019$sex_of_sp)
round(100* prop.table(table(yrbs_2019$sex, yrbs_2019$sex_of_sp)))
table(yrbs_2021$sex, yrbs_2021$sex_of_sp)
round(100* prop.table(table(yrbs_2021$sex, yrbs_2021$sex_of_sp)))




# New version

yrs <- seq(2015, 2021, 2)
nyrs <- length(yrs)
yrpos <- 1:nyrs

yrbs_by_yr <- lapply(yrs, function(x) yrbs_final[yrbs_final$year == x,])
yrbs_by_yr_F <- lapply(yrs, function(x) yrbs_final[yrbs_final$year == x  & yrbs_final$sex == "Female",])

lapply(yrpos, function(x) table(yrbs_by_yr[[x]]$sex, yrbs_by_yr[[x]]$sex_of_sp))
lapply(yrpos, function(x) round(100* prop.table(table(yrbs_by_yr[[x]]$sex, yrbs_by_yr[[x]]$sex_of_sp))))


