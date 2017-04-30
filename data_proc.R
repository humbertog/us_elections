library(ggplot2)
library(reshape2)

############################ 
# Load the data
############################
read_data <- function(df_files, df_vars) {
  files <- df_files$file
  years <- df_files$year
  
  n <- length(files)
  for(i in 1:n) {
    coln <- paste("id", as.character(years[i]), sep="")
    cols <- df_vars[,coln]
    #
    varn <- df_vars[,"var"]
    #
    all_content = readLines(files[i])
    dat_t = read.csv(textConnection(all_content[-2]), 
                     header = TRUE, 
                     na.strings=c("(X)", "N"),
                     stringsAsFactors = FALSE)
    #print(names(dat_t))
    dat_t <- dat_t[,cols]
    names(dat_t) <- varn
    dat_t$year <- years[i]
    if (i == 1) {
      dat <- dat_t
    } else {
      dat <- rbind(dat, dat_t)
    }
  } 
  dat 
}

### race 
dict_frace <- data.frame(
  year=c(2015, 2011, 2007),
  file=c("dat_race/ACS_15_1YR_DP05_with_ann.csv", "dat_race/ACS_11_1YR_DP05_with_ann.csv", "dat_race/ACS_07_1YR_DP5_with_ann.csv"),
  stringsAsFactors=FALSE
)

dict_race <- data.frame(
  var=c("GEO.id", "GEO.id2", "GEO.display.label", "pop_total", "pop_white_nh", "pop_afam", "pop_ind_alk", "pop_asian", "pop_island", "pop_hisp"),
  id2015=c("GEO.id", "GEO.id2", "GEO.display.label", "HC01_VC87", "HC01_VC94", "HC01_VC95", "HC01_VC96", "HC01_VC97", "HC01_VC98", "HC01_VC88"),
  id2011=c("GEO.id", "GEO.id2", "GEO.display.label", "HC01_VC81", "HC01_VC88", "HC01_VC89", "HC01_VC90", "HC01_VC91", "HC01_VC92", "HC01_VC82"), 
  id2007=c("GEO.id", "GEO.id2", "GEO.display.label", "HC01_EST_VC69", "HC01_EST_VC76", "HC01_EST_VC77", "HC01_EST_VC78", "HC01_EST_VC79", "HC01_EST_VC80", "HC01_EST_VC70"),
  stringsAsFactors=FALSE
)


df_race <- read_data(dict_frace, dict_race)
dim(df_race)
table(df_race$year)

df_race$pop_perc_white_nh <- df_race$pop_white_nh / df_race$pop_total
df_race$pop_perc_afam <- df_race$pop_afam / df_race$pop_total
df_race$pop_perc_ind_alk <- df_race$pop_ind_alk / df_race$pop_total
df_race$pop_perc_asian <- df_race$pop_asian / df_race$pop_total
df_race$pop_perc_island <- df_race$pop_island / df_race$pop_total
df_race$pop_perc_hisp <- df_race$pop_hisp / df_race$pop_total

df_race[is.na(df_race$pop_white_nh) & is.na(df_race$pop_afam),]
df_race[is.na(df_race$pop_perc_ind_alk),]



### economic variables
dict_feco <- data.frame(
  year=c(2015, 2011, 2007),
  file=c("dat_economic/ACS_15_1YR_DP03_with_ann.csv", "dat_economic/ACS_11_1YR_DP03_with_ann.csv", "dat_economic/ACS_07_1YR_DP3_with_ann.csv"),
  stringsAsFactors=FALSE
)

dict_eco <- data.frame(
  var=c("GEO.id", "GEO.id2", "GEO.display.label", "eco_unemp_rate", "eco_med_income", "eco_mean_income", "eco_perc_poverty"),
  id2015=c("GEO.id", "GEO.id2", "GEO.display.label", "HC03_VC07", "HC01_VC85", "HC01_VC86", "HC03_VC171"),
  id2011=c("GEO.id", "GEO.id2", "GEO.display.label", "HC03_VC08", "HC01_VC85", "HC01_VC86", "HC03_VC166"), 
  id2007=c("GEO.id", "GEO.id2", "GEO.display.label", "HC02_EST_VC06", "HC01_EST_VC69", "HC01_EST_VC70", "HC01_EST_VC112"),
  stringsAsFactors=FALSE
)

df_eco <- read_data(dict_feco, dict_eco)
dim(df_eco)
table(df_eco$year)

# Adjust for inflation (2007->2015=1.14, 2011->2015=1.05)
df_eco$eco_med_income[df_eco$year==2007] <- df_eco$eco_med_income[df_eco$year==2007] * 1.14
df_eco$eco_mean_income[df_eco$year==2007] <- df_eco$eco_mean_income[df_eco$year==2007] * 1.14

df_eco$eco_med_income[df_eco$year==2011] <- df_eco$eco_med_income[df_eco$year==2011] * 1.05
df_eco$eco_mean_income[df_eco$year==2011] <- df_eco$eco_mean_income[df_eco$year==2011] * 1.05

head(df_eco)

### Gini index
dict_fgini <- data.frame(
  year=c(2015, 2011, 2007),
  file=c("dat_gini/ACS_15_1YR_B19083_with_ann.csv", "dat_gini/ACS_11_1YR_B19083_with_ann.csv", "dat_gini/ACS_07_1YR_B19083_with_ann.csv"),
  stringsAsFactors=FALSE
)

dict_gini <- data.frame(
  var=c("GEO.id", "GEO.id2", "GEO.display.label", "eco_gini"),
  id2015=c("GEO.id", "GEO.id2", "GEO.display.label", "HD01_VD01"),
  id2011=c("GEO.id", "GEO.id2", "GEO.display.label", "HD01_VD01"), 
  id2007=c("GEO.id", "GEO.id2", "GEO.display.label", "HD01_VD01"),
  stringsAsFactors=FALSE
)

df_gini <- read_data(dict_fgini, dict_gini)
dim(df_eco)
table(df_eco$year)
head(df_gini)

### Education
dict_fedu <- data.frame(
  year=c(2015, 2011, 2007),
  file=c("dat_edu_attainment/ACS_15_1YR_S1501_with_ann.csv", "dat_edu_attainment/ACS_11_1YR_S1501_with_ann.csv", "dat_edu_attainment/ACS_07_1YR_S1501_with_ann.csv"),
  stringsAsFactors=FALSE
)

dict_edu <- data.frame(
  var=c("GEO.id", "GEO.id2", "GEO.display.label", "edu_perc_high", "edu_perc_college", "edu_perc_assoc", "edu_perc_bac", "edu_perc_grad"),
  id2015=c("GEO.id", "GEO.id2", "GEO.display.label", "HC02_EST_VC11", "HC02_EST_VC12", "HC02_EST_VC13", "HC02_EST_VC14", "HC02_EST_VC15"),
  id2011=c("GEO.id", "GEO.id2", "GEO.display.label", "HC01_EST_VC10", "HC01_EST_VC11", "HC01_EST_VC12", "HC01_EST_VC13", "HC01_EST_VC14"), 
  id2007=c("GEO.id", "GEO.id2", "GEO.display.label", "HC01_EST_VC09", "HC01_EST_VC10", "HC01_EST_VC11", "HC01_EST_VC12", "HC01_EST_VC12"),
  stringsAsFactors=FALSE
)

df_edu <- read_data(dict_fedu, dict_edu)
dim(df_edu)
table(df_edu$year)
summary(df_edu)

df_edu$edu_perc_high_and_more <- df_edu$edu_perc_high + df_edu$edu_perc_college + df_edu$edu_perc_assoc + df_edu$edu_perc_bac + df_edu$edu_perc_grad
df_edu$edu_perc_college_and_more <- df_edu$edu_perc_college + df_edu$edu_perc_assoc + df_edu$edu_perc_bac + df_edu$edu_perc_grad


### health % uninsured
dict_fhealth <- data.frame(
  year=c(2015),
  file=c("dat_health/ACS_15_1YR_S2701_with_ann.csv"),
  stringsAsFactors=FALSE
)

dict_health <- data.frame(
  var=c("GEO.id", "GEO.id2", "hc_perc_unins"),
  id2015=c("GEO.id", "GEO.id2", "HC05_EST_VC01"),
  stringsAsFactors=FALSE
)

df_health2015 <- read_data(dict_fhealth, dict_health)
df_health2007_2011 <- read.csv("dat_health/uninsured2007_2011.csv", na.strings=c("N/A") )[, c("ID", "Uninsured...", "Year")]
names(df_health2007_2011) <- c("GEO.id2", "hc_perc_unins", "year")


df_health2007_2011 <- merge(df_health2007_2011, df_health2015[, c(1,2)], all.x = TRUE)[,c("GEO.id", "GEO.id2", "hc_perc_unins", "year")]
df_health2007_2011 <- df_health2007_2011[!is.na(df_health2007_2011$GEO.id),]

df_health <- rbind(df_health2007_2011, df_health2015)

dim(df_health)
table(df_health$year)
summary(df_health)

### density
dict_fpop <- data.frame(
  year=c(2015),
  file=c("dat_pop_density/DEC_10_SF1_GCTPH1.US05PR_with_ann.csv"),
  stringsAsFactors=FALSE
)

dict_pop <- data.frame(
  var=c("GEO.id", "land_area"),
  id2015=c("GCT_STUB.target.geo.id", "SUBHD0303"),
  stringsAsFactors=FALSE
)

df_pop <- read_data(dict_fpop, dict_pop)
df_pop$land_area <- df_pop$land_area * 2.589988110336


############################
# Merge data
############################
# Merge data first to have only one data frame
df_eco[,c("eco_unemp_rate", "eco_perc_poverty")] <- df_eco[,c("eco_unemp_rate", "eco_perc_poverty")] / 100
df_edu[, c("edu_perc_high", "edu_perc_college", "edu_perc_assoc", "edu_perc_bac", "edu_perc_grad", "edu_perc_high_and_more", "edu_perc_college_and_more")] <- 
  df_edu[, c("edu_perc_high", "edu_perc_college", "edu_perc_assoc", "edu_perc_bac", "edu_perc_grad", "edu_perc_high_and_more", "edu_perc_college_and_more")] / 100
df_health[, c("hc_perc_unins")]  <- df_health[, c("hc_perc_unins")] / 100

df_acs <- Reduce(function(x, y) merge(x, y, by=c("GEO.id", "GEO.id2", "year")), 
                 list(df_race, df_eco[,-3], df_gini[,-3], df_edu[,-3], df_health))
dim(df_acs)
table(df_acs$year)
summary(df_acs)
df_acs[is.na(df_acs$hc_perc_unins),]


head(df_acs)
df_acs <- merge(df_acs, df_pop[,c(1,2)], by.x="GEO.id", by.y = "GEO.id") 

############################
# Merge with election results
############################
votes1 <- read.csv("./dat_elections/US_County_Level_Presidential_Results_08-16.csv")
votes <- votes1
votes$perc_gop2008 <- votes$gop_2008 / votes$total_2008#(votes$gop_2008 + votes$dem_2008)
votes$perc_gop2012 <- votes$gop_2012 / votes$total_2012#(votes$gop_2012 + votes$dem_2012)
votes$perc_gop2016 <- votes$gop_2016 / votes$total_2016#(votes$gop_2016 + votes$dem_2016)

votes <- votes[, c("fips_code", "perc_gop2008", "perc_gop2012","perc_gop2016")]

votes <- melt(votes, id.vars = "fips_code")
names(votes) <- c("GEO.id2", "variable", "perc_gop")
votes$year <- 2007
votes$year[votes$variable == "perc_gop2012"] <- 2011
votes$year[votes$variable == "perc_gop2016"] <- 2015
#votes$year[votes$variable == "total_2016"] <- 2015


table(votes$year)
df_acs_votes <- merge(df_acs, votes[,c("GEO.id2", "perc_gop", "year")], all.x = TRUE)
dim(df_acs_votes)


##########

votes <- votes1
votes$perc_dem2008 <- votes$dem_2008 / votes$total_2008#(votes$gop_2008 + votes$dem_2008)
votes$perc_dem2012 <- votes$dem_2012 / votes$total_2012#(votes$gop_2012 + votes$dem_2012)
votes$perc_dem2016 <- votes$dem_2016 / votes$total_2016#(votes$gop_2016 + votes$dem_2016)

votes <- votes[, c("fips_code", "perc_dem2008", "perc_dem2012","perc_dem2016")]

votes <- melt(votes, id.vars = "fips_code")
names(votes) <- c("GEO.id2", "variable", "perc_dem")
votes$year <- 2007
votes$year[votes$variable == "perc_dem2012"] <- 2011
votes$year[votes$variable == "perc_dem2016"] <- 2015
#votes$year[votes$variable == "total_2016"] <- 2015


df_acs_votes <- merge(df_acs_votes, votes[,c("GEO.id2", "perc_dem", "year")], all.x = TRUE)
dim(df_acs_votes)

######
votes <- votes1[, c("fips_code", "total_2008", "total_2012","total_2016")]

votes <- melt(votes, id.vars = "fips_code")
names(votes) <- c("GEO.id2", "variable", "total_votes")
votes$year <- 2007
votes$year[votes$variable == "total_2012"] <- 2011
votes$year[votes$variable == "total_2016"] <- 2015
#votes$year[votes$variable == "total_2016"] <- 2015


df_acs_votes <- merge(df_acs_votes, votes[,c("GEO.id2", "total_votes", "year")], all.x = TRUE)
dim(df_acs_votes)

df_acs_votes$eco_mean_income <- df_acs_votes$eco_mean_income / 1000
df_acs_votes$eco_med_income <- df_acs_votes$eco_med_income / 1000

#####
df_acs_votes[is.na(df_acs_votes$perc_gop),]
df_acs_votes$density <- df_acs_votes$pop_total / df_acs_votes$land_area


# fix the results of 2008 elections for Laporte county (18091)
df_acs_votes$perc_gop[df_acs_votes$GEO.id2 == 18091 & df_acs_votes$year ==2007] <- .381
save(df_acs_votes, file="prepared_data_1yr.RData")
