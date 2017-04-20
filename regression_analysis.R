library(ggplot2)
library(GGally)
library(betareg)
library(reshape2)

#load("prepared_data.RData")
load("prepared_data_1yr.RData")

# y_it = b_0t + b_t X_it 
mod_1 <- "perc_gop ~   ( density + pop_perc_white_nh + 
eco_med_income +  eco_unemp_rate + eco_gini + 
hc_perc_unins + edu_perc_college_and_more)*as.factor(year) "

betar_1 <- betareg(as.formula(mod_1), data=df_acs_votes, link="logit")
summary(betar_1)

# 
mod_1_y <- "perc_gop ~ (density + pop_perc_white_nh + 
eco_med_income +  eco_unemp_rate + eco_gini + 
hc_perc_unins + edu_perc_college_and_more)"

betar_1_2007 <- betareg(as.formula(mod_1_y), data=df_acs_votes[df_acs_votes$year==2007,], link="logit")
summary(betar_1_2007)

betar_1_2011 <- betareg(as.formula(mod_1_y), data=df_acs_votes[df_acs_votes$year==2011,], link="logit")
summary(betar_1_2011)

betar_1_2016 <- betareg(as.formula(mod_1_y), data=df_acs_votes[df_acs_votes$year==2015,], link="logit")
summary(betar_1_2016)


# y_it  = b_0 + b_1 x_i. + b_2(X_it - x_i.)
vars <- c("GEO.id2", "GEO.id", "GEO.display.label","year","density",
          "pop_perc_white_nh", "eco_unemp_rate",
          "eco_med_income", "eco_gini", "edu_perc_college_and_more", "hc_perc_unins", "perc_gop")
          
df_acs_votes__ <- df_acs_votes[,vars]

ave <- aggregate(df_acs_votes__[,-c(1:4)], by=list(df_acs_votes__$GEO.id2), FUN=mean, na.rm=TRUE)
names(ave)[1] <- "GEO.id2"

# just check
mod_2 <- "perc_gop ~ (density + pop_perc_white_nh + 
eco_med_income +  eco_unemp_rate + eco_gini + 
hc_perc_unins + edu_perc_college_and_more)"

betar_2 <- betareg(as.formula(mod_2), data=ave, link="logit")
summary(betar_2)

# now the model with the differences
df_acs_votes__ <- merge(df_acs_votes__, ave, by=c("GEO.id2"), suffixes=c("","_m"))

mod_2_d <- "I(perc_gop- perc_gop_m) ~ -1 + I(density - density_m) + 
I(pop_perc_white_nh - pop_perc_white_nh_m) + I(eco_med_income - eco_med_income_m) +
I(eco_unemp_rate - eco_unemp_rate_m) + I(eco_gini - eco_gini_m) +
I(hc_perc_unins - hc_perc_unins_m) + 
I(edu_perc_college_and_more - edu_perc_college_and_more_m) "

betar_2_d <- lm(as.formula(mod_2_d), data=df_acs_votes__)
summary(betar_2_d)
plot(betar_2_d)

#
mod_2_d_2 <- "perc_gop ~ 1 + perc_gop_m + I(density - density_m) + 
I(pop_perc_white_nh - pop_perc_white_nh_m) + I(eco_med_income - eco_med_income_m) +
I(eco_unemp_rate - eco_unemp_rate_m) + I(eco_gini - eco_gini_m) +
I(hc_perc_unins - hc_perc_unins_m) + 
I(edu_perc_college_and_more - edu_perc_college_and_more_m) "

betar_2_d_2 <- betareg(as.formula(mod_2_d_2), data=df_acs_votes__, link="logit")
summary(betar_2_d_2)


mod_2_d_3 <- "perc_gop ~ 1 + density_m + pop_perc_white_nh_m + eco_med_income_m +
eco_unemp_rate_m + eco_gini_m + hc_perc_unins_m + edu_perc_college_and_more_m + 
I(density - density_m) + 
I(pop_perc_white_nh - pop_perc_white_nh_m) + I(eco_med_income - eco_med_income_m) +
I(eco_unemp_rate - eco_unemp_rate_m) + I(eco_gini - eco_gini_m) +
I(hc_perc_unins - hc_perc_unins_m) + 
I(edu_perc_college_and_more - edu_perc_college_and_more_m)"

betar_2_d_3 <- betareg(as.formula(mod_2_d_3), data=df_acs_votes__, link="logit")
summary(betar_2_d_3)
plot(betar_2_d_3)


ggplot(df_acs_votes__, aes(eco_med_income_m, log(perc_gop / (1 - perc_gop)))) + 
  geom_point(aes(size=density), alpha=0.5, shape=1) +
  geom_smooth(se=FALSE) +
  labs(x="% de la pobación que es blanca", y="% de votos por GOP", 
       colour="año", size="dens. población"
  )

summary(betareg(perc_gop ~  eco_med_income_m, data=df_acs_votes__, link="logit"))

summary(df_acs_votes$pop_total)

