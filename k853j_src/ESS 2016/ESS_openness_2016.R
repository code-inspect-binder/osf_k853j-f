#--------------------------------------------------------------------------------------------------------
# Analysis of ESS data for the relationship of openess and political beliefs
# ESS 2016 (round 8)
#
# Data: http://www.europeansocialsurvey.org/data
#--------------------------------------------------------------------------------------------------------

#---- Load libraries ----
library(haven)
library(dplyr)
library(scales) # added
library(lme4)
library(lmerTest)
library(emmeans)
library(multcomp)
library(sjPlot)
library(ggplot2)
library(ggeffects)
library(optimx)# for optimizers - added

options(scipen=999)#non-scientific notation

#---- Load dataset ----
# data available at ESS website http://www.europeansocialsurvey.org/data
data8 <- as.data.frame(read_sav("ESS 2016/ESS8e02.sav"))
nrow(data8) # n = 44,387

# excluding Israel and Turkey (in case they are in the data)
summary(as.factor(data8$cntry))
data8 <- subset(data8, cntry!="IL" & cntry!="TR")
nrow(data8) # n = 41,830

#--------------------------------------------------------------------------------------------------------
#---- Recode political beliefs ----

# remove spss labels
data8$ginveco <- zap_labels(data8$ginveco)
data8$gincdif <- zap_labels(data8$gincdif)
data8$smdfslv <- zap_labels(data8$smdfslv)
data8$sbstrec <- zap_labels(data8$sbstrec)

# scaling all the variables of interest (between 0-1) for a composite score
data8$gincdif_s = rescale(data8$gincdif)
data8$smdfslv_s = rescale(data8$smdfslv)
data8$sbstrec_r = 6 - data8$sbstrec # reverse scores first
data8$sbstrec_r_s = rescale(data8$sbstrec_r)

# create economic beliefs index - the higher the more right-wing economy beliefs (neoliberal)
econ = c("gincdif_s","smdfslv_s","sbstrec_r_s")
data8$polit_econ = rowMeans(data8[econ],na.rm=TRUE)
mean(data8$polit_econ, na.rm = TRUE) # 0.37
sd(data8$polit_econ, na.rm = TRUE) # 0.17
psych::alpha(data8[econ]) # 0.41

#---- Recode political engagement ----

# interest in politics (this one was used in the first version of a manuscript)
data8$polintr <- zap_labels(data8$polintr) # remove spss labels
data8$polintr_r = 5 - data8$polintr# reversed (the higher the more interest)
data8$polintr_r_s = scales::rescale(data8$polintr_r)#scaling between 0-1

# political news reception (in minutes, each day): nwspol
data8$nwspol = zap_labels(data8$nwspol)
data8$nwspol_s = scales::rescale(data8$nwspol)#scaling between 0-1

# political action (7 actions)
#contplt: Contacted politician or government official last 12 months
#wrkprty: Worked in political party or action group last 12 months
#wrkorg: Worked in another organisation or association last 12 months
#badge: Worn or displayed campaign badge/sticker last 12 months
#sgnptit: Signed petition last 12 months
#pbldmn: Taken part in lawful public demonstration last 12 months
#bctprd: Boycotted certain products last 12 months
#pstplonl: Posted or shared anything about politics online last 12 months
# reverse code (instead of: 1 = yes, 2 = no; change into 0 = no, 1 = yes)
data8$contplt_r = 2 - data8$contplt
data8$wrkprty_r = 2 - data8$wrkprty
data8$wrkorg_r = 2 - data8$wrkorg
data8$badge_r = 2 - data8$badge
data8$sgnptit_r = 2 - data8$sgnptit
data8$pbldmn_r = 2 - data8$pbldmn
data8$bctprd_r = 2 - data8$bctprd
data8$pstplonl_r = 2 - data8$pstplonl
# get scores for underating political action
polit_action = c("contplt_r", "wrkprty_r", "wrkorg_r", "badge_r", 
                 "sgnptit_r", "pbldmn_r", "bctprd_r", "pstplonl_r")
data8$polit_action = rowSums(data8[polit_action], na.rm = TRUE)
data8$polit_action_s = scales::rescale(data8$polit_action) # scale between 0-1

# create one index of political engagement using: interest, media reception, and political action
polit_eng = c("polintr_r_s", 
              "nwspol_s",
              "polit_action_s")
data8$polit_eng = rowMeans(data8[polit_eng], na.rm = TRUE)
mean(data8$polit_eng, na.rm = TRUE) # 0.22
sd(data8$polit_eng, na.rm = TRUE) # 0.14
psych::alpha(data8[polit_eng]) # 0.39
data8$polit_eng_c = c(scale(data8$polit_eng, center = TRUE, scale = FALSE)) # center

#---- Recode control variable  ----
# remove spss labels
data8$agea <- zap_labels(data8$agea)
data8$eduyrs <- zap_labels(data8$eduyrs)
# recode
data8$gndr_01 = data8$gndr-1 # male=0, female=1
data8$agea_s = rescale(data8$agea)# scaling between 0-1
data8$eduyrs_s = rescale(data8$eduyrs)

#---- Recode welfare state classification  ----
data8$cntry <- zap_labels(data8$cntry)
data8$welstate <- recode(data8$cntry,
                         "SE" = 1, "NO" = 1, "DK" = 1, "FI" = 1, "IS" = 1,
                         "IE" = 2, "GB" = 2, # liberal 
                         "NL" = 3,"LU" = 3, "DE" = 3, # contitental
                         "CH" = 3, "BE" = 3,"AT" = 3, "FR"=3, # continental
                         "ES" = 4,"IL" = 4,"IT" = 4, "GR" = 4, "TR" = 4,"PT" = 4, "CY" = 4, # southern
                         "RU" = 5, "EE" = 5, "BG" = 5,"CZ" = 5,"PL" = 5,"HR" = 5, "HU" = 5,"LV" = 5, # eastern
                         "RO" = 5,"SI" = 5,"SK" = 5,"UA" = 5, "LT" = 5, "AL" = 5, "XK" = 5)
# change a reference category 
data8$welstate <- factor(data8$welstate,
                         levels = c(2,3,4,1,5),
                         labels = c("Liberal","Continental","Southern","Nordic","Eastern"))
summary(data8$welstate)
table(data8$welstate,data8$cntry)

#---- Add countries names ----
data8$cntry_full[data8$cntry == "AT"] <- "Austria"
data8$cntry_full[data8$cntry == "BE"] <- "Belgium"
data8$cntry_full[data8$cntry == "CH"] <- "Switzerland"
data8$cntry_full[data8$cntry == "CZ"] <- "Czechia"
data8$cntry_full[data8$cntry == "DE"] <- "Germany"
data8$cntry_full[data8$cntry == "DK"] <- "Denmark"
data8$cntry_full[data8$cntry == "ES"] <- "Spain"
data8$cntry_full[data8$cntry == "FI"] <- "Finland"
data8$cntry_full[data8$cntry == "FR"] <- "France"
data8$cntry_full[data8$cntry == "GB"] <- "Great Britain"
data8$cntry_full[data8$cntry == "GR"] <- "Greece"
data8$cntry_full[data8$cntry == "HU"] <- "Hungary"
data8$cntry_full[data8$cntry == "IE"] <- "Ireland"
#IL - Israel
data8$cntry_full[data8$cntry == "IT"] <- "Italy"
data8$cntry_full[data8$cntry == "LU"] <- "Luxembourg"
data8$cntry_full[data8$cntry == "NL"] <- "Netherlands"
data8$cntry_full[data8$cntry == "NO"] <- "Norway"
data8$cntry_full[data8$cntry == "PL"] <- "Poland"
data8$cntry_full[data8$cntry == "PT"] <- "Portugal"
data8$cntry_full[data8$cntry == "SE"] <- "Sweden"
data8$cntry_full[data8$cntry == "SI"] <- "Slovenia"
#TR - Turkey
data8$cntry_full[data8$cntry == "EE"] <- "Estonia"
data8$cntry_full[data8$cntry == "IS"] <- "Iceland"
data8$cntry_full[data8$cntry == "SK"] <- "Slovakia"
data8$cntry_full[data8$cntry == "UA"] <- "Ukraine"
data8$cntry_full[data8$cntry == "BG"] <- "Bulgaria"
data8$cntry_full[data8$cntry == "CY"] <- "Cyprus"
data8$cntry_full[data8$cntry == "RU"] <- "Russia"
data8$cntry_full[data8$cntry == "RO"] <- "Romania" 
data8$cntry_full[data8$cntry == "LV"] <- "Latvia"
data8$cntry_full[data8$cntry == "HR"] <- "Croatia"
data8$cntry_full[data8$cntry == "LT"] <- "Lithuania" 
data8$cntry_full[data8$cntry == "AL"] <- "Albania"
data8$cntry_full[data8$cntry == "XK"] <- "Kosovo"
data8$cntry_full = as.factor(data8$cntry_full)
summary(data8$cntry_full)
sum(is.na(data8$cntry_full))
sum(is.na(data8$cntry))

#--------------------------------------------------------------------------------------------------------
#---- Calculate values scores for PVQ ----
#---- recoding PVQ items (ESS uses 1 as "like me" and 6 "totally not like me)
data8$ipcrtiv_r = 7 - data8$ipcrtiv
data8$imprich_r = 7 - data8$imprich
data8$ipeqopt_r = 7 - data8$ipeqopt
data8$ipshabt_r = 7 - data8$ipshabt
data8$impsafe_r = 7 - data8$impsafe
data8$impdiff_r = 7 - data8$impdiff
data8$ipfrule_r = 7 - data8$ipfrule
data8$ipudrst_r = 7 - data8$ipudrst
data8$ipmodst_r = 7 - data8$ipmodst
data8$ipgdtim_r = 7 - data8$ipgdtim
data8$impfree_r = 7 - data8$impfree
data8$iphlppl_r = 7 - data8$iphlppl
data8$ipsuces_r = 7 - data8$ipsuces
data8$ipstrgv_r = 7 - data8$ipstrgv
data8$ipadvnt_r = 7 - data8$ipadvnt
data8$ipbhprp_r = 7 - data8$ipbhprp
data8$iprspot_r = 7 - data8$iprspot
data8$iplylfr_r = 7 - data8$iplylfr
data8$impenv_r = 7 - data8$impenv
data8$imptrad_r = 7 - data8$imptrad
data8$impfun_r = 7 - data8$impfun

#---- Reliability for the scores ----

# procedure of calculating 2 dimensions as in Grigoryan & Schwartz (2020, GPIR):
# calulate average values for the 4 "big" values, then substract scores conservation - openness and self-enh - self-trans
# reliability is checked for the "big" values (here only couple of items per dimension)

# conservation	conformity --> ipfrule: Important to do what is told and follow rules	
# conservation	conformity --> ipbhprp: Important to behave properly	
# conservation	security --> impsafe: Important to live in secure and safe surroundings	
# conservation	security --> ipstrgv: Important that government is strong and ensures safety	
# conservation	tradition --> ipmodst: Important to be humble and modest, not draw attention	
# conservation	tradition --> imptrad: Important to follow traditions and customs	
# openness	self_direction --> ipcrtiv: Important to think new ideas and being creative	
# openness	self_direction --> impfree: Important to make own decisions and be free	
# openness	stimulation --> impdiff: Important to try new and different things in life	
# openness	stimulation --> ipadvnt: Important to seek adventures and have an exciting life	
# self_enh	achievement --> ipshabt: Important to show abilities and be admired	
# self_enh	achievement --> ipsuces: Important to be successful and that people recognize achievements	
# self_enh	power --> imprich: Important to be rich, have money and expensive things	
# self_enh	power --> iprspot: Important to get respect from others	
# self_trans	benevolence --> iphlppl: Important to help people and care for others well-being	
# self_trans	benevolence --> iplylfr: Important to be loyal to friends and devote to people close	
# self_trans	universalism --> ipeqopt: Important that people are treated equally and have equal opportunities	
# self_trans	universalism --> ipudrst: Important to understand different people	
# self_trans	universalism --> impenv: Important to care for nature and environment	
# not_clear	hedonism --> ipgdtim: Important to have a good time	
# not_clear	hedonism --> impfun: Important to seek fun and things that give pleasure	

# conservation
cons_hvl = c("ipfrule_r", # _hvl stands for higher order value 
             "ipbhprp_r", 
             "impsafe_r", 
             "ipstrgv_r",
             "ipmodst_r",
             "imptrad_r")
data8$cons_hvl = rowMeans(data8[cons_hvl], na.rm=TRUE) 

# openness 
open_hvl = c("ipcrtiv_r",
             "impfree_r",
             "impdiff_r",
             "ipadvnt_r")
data8$open_hvl = rowMeans(data8[open_hvl], na.rm=TRUE) 

# value for the dimension openness - conservation (new method)
data8$conservation2 = data8$cons_hvl - data8$open_hvl
data8$conservation2_s = rescale(data8$conservation2) 
data8$conservation2_s_c = c(scale(data8$conservation2_s, scale = FALSE, center = TRUE))

#---- Per-country reliabilities for higher order values ----
# get list of countries 
countries <- unique(data8$cntry_full)
cons_hvl_a_all = data.frame() # create empty df
open_hvl_a_all = data.frame() # create empty df

# conservation
index = 0 # zero the index
for (c in countries) {
  index = index + 1
  print(paste0(c, " #", index))
  # select a country
  data8_cntry = data8 %>% filter(cntry_full == c)
  # calculate reliability
  cons_hvl_a <- psych::alpha(data8_cntry[cons_hvl])$total[1] # get alpha
  cons_hvl_a <- cons_hvl_a %>% rename(cons_hvl_a = raw_alpha) # rename
  cons_hvl_a$cntry_full <- c
  # bind rows
  cons_hvl_a_all <- bind_rows(cons_hvl_a_all, cons_hvl_a)
}

# openness
index = 0 # zero the index
for (c in countries) {
  index = index + 1
  print(paste0(c, " #", index))
  # select a country
  data8_cntry = data8 %>% filter(cntry_full == c)
  # calculate reliability
  open_hvl_a <- psych::alpha(data8_cntry[open_hvl])$total[1] # get alpha
  open_hvl_a <- open_hvl_a %>% rename(open_hvl_a = raw_alpha) # rename
  open_hvl_a$cntry_full <- c
  # bind rows
  open_hvl_a_all <- bind_rows(open_hvl_a_all, open_hvl_a)
}

#--------------------------------------------------------------------------------------------------------
#---- Remove cases with NA ----
data8 <- data8 %>%
  filter(!is.na(polit_econ),
         #!is.na(openess_s_c), # this is for data from Verkasalo et al (more NA because of the way the scores are calculated)
         !is.na(conservation2),
         !is.na(econ),
         !is.na(agea_s),
         !is.na(gndr_01),
         !is.na(eduyrs_s),
         !is.na(polit_eng))

#---- Summary stats for the full sample and countries ----
# demographics
mean(data8$agea);sd(data8$agea)
mean(data8$eduyrs);sd(data8$eduyrs)
summary(as.factor(data8$gndr_01))
# polit_econ
mean(data8$polit_econ) # 0.37
sd(data8$polit_econ) #0.17
psych::alpha(data8[econ]) # 0.41
# polit_eng
mean(data8$polit_eng) # 0.22
sd(data8$polit_eng) # 0.15
psych::alpha(data8[polit_eng]) # 0.39
# conservation
mean(data8$cons_hvl) # 4.30
sd(data8$cons_hvl) # 0.82
psych::alpha(data8[cons_hvl]) # 0.71
# openness
mean(data8$open_hvl)# 4.08
sd(data8$open_hvl) # 0.91
psych::alpha(data8[open_hvl]) # 0.65
# conservation - openness (NSC)
mean(data8$conservation2) # 0.22
sd(data8$conservation2) # 1.19

# per country descriptives
stats = data8 %>% 
  group_by(as.factor(cntry_full)) %>%
  summarize(n = n(),
            age_m = mean(agea),
            age_sd = sd(agea),
            edu_m = mean(eduyrs),
            edu_sd = sd(eduyrs),
            # economic beliefs and political engagement
            polit_econ_m = mean(polit_econ, na.rm = TRUE),
            polit_econ_sd = sd(polit_econ, na.rm = TRUE),
            polit_eng_m = mean(polit_eng, na.rm = TRUE),
            polit_eng_sd = sd(polit_eng, na.rm = TRUE),
            # conservation - higher order value
            open_hvl_m = mean(open_hvl, na.rm = TRUE),
            open_hvl_sd = sd(open_hvl, na.rm = TRUE),
            # opennes - higher order value
            cons_hvl_m = mean(cons_hvl, na.rm = TRUE),
            cons_hvl_sd = sd(cons_hvl, na.rm = TRUE),
            # final value
            conservation2_m = mean(conservation2, na.rm = TRUE),
            conservation2_sd = sd(conservation2, na.rm = TRUE)) 
#write.table(stats, "stats.csv", sep = ";", dec = ",")

# count gender per country (this could be improved) 
data8$gndr_01 <- zap_labels(data8$gndr_01) 
gender_count = data8 %>% 
  group_by(cntry_full, gndr_01) %>%
  summarize(n = n()) %>% # this will count only women
  tidyr::spread(gndr_01, n)
gender_count
#write.table(gender_count, "gender_count.csv", sep = ";", dec = ",")

#--------------------------------------------------------------------------------------------------------
#---- Analysis ---- 
#---- Political engagement ----
m_econ0_1_2 = lmer(data = data8, polit_econ ~ 
                     conservation2_s_c * polit_eng_c + 
                     gndr_01 + agea_s + eduyrs_s +
                     (1 + conservation2_s_c + polit_eng_c |cntry), 
                   REML = FALSE,
                   control=lmerControl(optimizer="optimx",
                                       optCtrl=list(method='nlminb')))
summary(m_econ0_1_2)
# slopes
trend_econ0_1_2 = cld(emtrends(m_econ0_1_2, ~ polit_eng_c, var = "conservation2_s_c", 
                               at = list(polit_eng_c = c(-0.14,0.14))), details=TRUE)
trend_econ0_1_2
# get p-vals for the slopes
trend_econ0_1_2p = test(emtrends(m_econ0_1_2, ~ polit_eng_c, var = "conservation2_s_c", 
                                 at = list(polit_eng_c = c(-0.14,0.14))))
trend_econ0_1_2p 
# plot 
ggpredict(m_econ0_1_2, terms = c("conservation2_s_c", "polit_eng_c[-0.14, 0.14]")) %>% 
  plot(color = "bw") + 
  labs(x = "NSC", 
       y = "Economic beliefs", 
       title = "", 
       linetype = "Political \nengagement") +
  ylim(0.1, 0.8) +
  scale_linetype_manual(values = c("solid", "dashed"), labels = c("Low", "High")) +
  theme_classic()

#---- Politcal enagements * welfare regime -----
m_econ2_1_2 = lmer(data = data8, polit_econ ~ 
                     conservation2_s_c * welstate * polit_eng_c +
                     gndr_01 + agea_s + eduyrs_s +
                     (1 + conservation2_s_c + polit_eng_c|cntry), 
                   REML = FALSE,
                   control=lmerControl(optimizer="optimx",
                                       optCtrl=list(method='nlminb')))
summary(m_econ2_1_2)
# compare models 
anova(m_econ0_1_2, m_econ2_1_2)
# slopes
cld(emtrends(m_econ2_1_2, ~ welstate|polit_eng_c, var = "conservation2_s_c", 
             at = list(polit_eng_c = c(-0.14,0.14))), details=TRUE)
# get slopes with the p-vals
test(emtrends(m_econ2_1_2, ~ welstate|polit_eng_c, var = "conservation2_s_c", 
              at = list(polit_eng_c = c(-0.14,0.14))))
# plot
ggpredict(m_econ2_1_2, c("conservation2_s_c", "welstate", "polit_eng_c[-0.14, 0.14]")) %>% 
  plot(colors = "bw", line.size = 0.8)  +
  labs(x = "NSC", 
       y = "Economic beliefs", 
       title = "",
       linetype = "Welfare \nstate model") +
  ylim(0.1, 0.8) +
  scale_linetype_manual(values = c("longdash", "solid", "dashed", "dotdash", "dotted")) +
  theme_classic() 
