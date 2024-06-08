library(haven)
library(readr)
setwd("C:/Users/geike/Downloads")
getwd()

sun_study <- read_sav("PHE&RoleStress_Final_occupation.Final.SAV")
write_csv(x=sun_study, path="sun_study.csv")

