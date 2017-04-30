library(ggplot2)
library(GGally)
library(betareg)
library(reshape2)
library(gridExtra)
#load("prepared_data.RData")
load("prepared_data_1yr.RData")

###
a<-ggplot(df_acs_votes, aes(pop_perc_white_nh, perc_gop, colour=as.factor(year))) + 
  geom_point(aes(size=density), alpha=0.5, shape=1) +
  geom_smooth(se=FALSE) +
  labs(x="% de la pobación que es blanca", y="% de votos por GOP", 
       colour="año", size="dens. población"
       ) +
  ylim(c(0,1)) + xlim(c(0,1))
  

b<- ggplot(df_acs_votes, aes(as.factor(year), pop_perc_white_nh, colour=as.factor(year))) + 
  geom_boxplot(alpha=0.5, shape=1) +
  labs(x="year", y="% of white population", 
       colour="año"
  ) + labs
  theme(legend.position="none") + 

grid.arrange(a,b,widths=c(2,1), nrow=1,ncol=2)


ggplot(df_acs_votes, aes(eco_unemp_rate, perc_gop, colour=as.factor(year))) + 
  geom_point(aes(size=density), alpha=0.5, shape=1) +
  geom_smooth(se=FALSE)
ggplot(df_acs_votes, aes(eco_med_income, perc_gop, colour=as.factor(year))) + 
  geom_point(aes(size=density), alpha=0.5, shape=1) +
  geom_smooth(se=FALSE)
ggplot(df_acs_votes, aes(eco_gini, perc_gop, colour=as.factor(year))) + 
  geom_point(aes(size=density), alpha=0.5, shape=1) +
  geom_smooth(se=FALSE)

ggplot(df_acs_votes, aes(edu_perc_college_and_more, perc_gop, colour=as.factor(year))) + 
  geom_point(aes(size=density), alpha=0.5, shape=1) +
  geom_smooth(se=FALSE)
ggplot(df_acs_votes, aes(hc_perc_unins, perc_gop, colour=as.factor(year))) + 
  geom_point(aes(size=density), alpha=0.8, shape=1) +
  geom_smooth(se=FALSE)







#######
# second
m <- merge(df_acs_votes[df_acs_votes$year==2007,],
           df_acs_votes[df_acs_votes$year==2015,],
           by=c("GEO.id2", "GEO.id"),
           suffixes = c("_2008", "_2016")
)
m$gop_change <- m$perc_gop_2016 - m$perc_gop_2008

ggplot(m, aes(perc_gop_2008, perc_gop_2016)) +
  geom_point(aes(size=pop_total_2016/1000),alpha=.8, shape=1) + 
  geom_abline(intercept = 0, slope = 1, colour="black", size=.5) +
  #geom_smooth(method = "lm", formula = y ~ x) +
  #geom_smooth(se=FALSE, span=.5) + 
  xlim(0,1) + ylim(c(0,1))  +
  annotate("rect", xmin=0, xmax=.5, ymin=0, ymax=.5, alpha=0.2, fill="blue") +
  annotate("rect", xmin=0, xmax=.375, ymin=0, ymax=.375, alpha=0.2, fill="blue") +
  annotate("rect", xmin=.5, xmax=1, ymin=.5, ymax=1, alpha=0.2, fill="red") +
  annotate("rect", xmin=.625, xmax=1, ymin=.625, ymax=1, alpha=0.2, fill="red") +
  annotate("rect", xmin=.375, xmax=.625, ymin=.375, ymax=.625, alpha=0.2, fill="gray50") +
  labs(x="% of votes for the GOP in 2008", y="% of votes for the GOP in 2016", 
       colour="year", size="pop/1000"
  ) +
  theme_bw()
###
# laporte 18091

m_change <- m[abs(m$gop_change) > 0.1,]

ggplot(m_change) +
  geom_point(aes(pop_perc_white_nh_2008, pop_perc_white_nh_2016, size=density_2016, colour=perc_gop_2016), alpha=.8, shape=1) + 
  geom_abline(intercept = 0, slope = 1, colour="blue", size=.5) +
  scale_colour_gradient2(low="blue", high="red", 
                         mid="white",
                         midpoint = .5, space="Lab")

ggplot(m_change) +
  geom_point(aes(eco_med_income_2008, eco_med_income_2016, size=density_2016, colour=perc_gop_2016), alpha=.8, shape=1) + 
  geom_abline(intercept = 0, slope = 1, colour="blue", size=.5) +
  scale_colour_gradient2(low="blue", high="red", 
                         mid="white",
                         midpoint = .5, space="Lab")
ggplot(m_change) +
  geom_point(aes(eco_unemp_rate_2008, eco_unemp_rate_2016, size=density_2016, colour=perc_gop_2016), alpha=.8, shape=1) + 
  geom_abline(intercept = 0, slope = 1, colour="blue", size=.5) +
  scale_colour_gradient2(low="blue", high="red", 
                         mid="white",
                         midpoint = .5, space="Lab")
ggplot(m_change) +
  geom_point(aes(eco_gini_2008, eco_gini_2016, size=density_2016, colour=perc_gop_2016), alpha=.8, shape=1) + 
  geom_abline(intercept = 0, slope = 1, colour="blue", size=.5) +
  scale_colour_gradient2(low="blue", high="red", 
                         mid="white",
                         midpoint = .5, space="Lab")

ggplot(m_change) +
  geom_point(aes(edu_perc_college_and_more_2008, edu_perc_college_and_more_2016, size=density_2016, colour=perc_gop_2016), alpha=.8, shape=1) + 
  geom_abline(intercept = 0, slope = 1, colour="blue", size=.5) +
  scale_colour_gradient2(low="blue", high="red", 
                         mid="white",
                         midpoint = .5, space="Lab")

ggplot(m_change) +
  geom_point(aes(hc_perc_unins_2008, hc_perc_unins_2016, size=density_2016, colour=perc_gop_2016), alpha=.8, shape=1) + 
  geom_abline(intercept = 0, slope = 1, colour="blue", size=.5) +
  scale_colour_gradient2(low="blue", high="red", 
                         mid="white",
                         midpoint = .5, space="Lab")



