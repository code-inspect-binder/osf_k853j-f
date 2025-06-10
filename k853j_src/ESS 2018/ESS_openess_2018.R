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
#library(effects)
library(emmeans)
library(multcomp)
library(sjPlot)
#library(sjstats)
#library(tidyr)
library(ggplot2)
library(ggeffects)
library(optimx)# for optimizers - added

#---- Load dataset ----
# data available at ESS website http://www.europeansocialsurvey.org/data
data9 <- as.data.frame(read_sav("ESS 2018/ESS9e02.sav"))
nrow(data9) # n = 47,086

# excluding Israel and Turkey (in case they are in the data)
data9 <- subset(data9, cntry!="IL" & cntry!="TR")
nrow(data9) # 47,086 (no data from Israel or Turkey)

#--------------------------------------------------------------------------------------------------------
#---- Recode political beliefs ----

# remove spss labels
data9$gincdif <- zap_labels(data9$gincdif)
data9$sofrdst <- zap_labels(data9$sofrdst) # new item 
data9$sofrpr <- zap_labels(data9$sofrpr) # new item

# scaling all the variables of interest (between 0-1) for a composite score
data9$gincdif_s = rescale(data9$gincdif)
data9$sofrdst_s = rescale(data9$sofrdst)
data9$sofrpr_s = rescale(data9$sofrpr)

# create economic beliefs index - the higher the more right-wing economy beliefs (neoliberal)
econ = c("gincdif_s","sofrdst_s","sofrpr_s")
data9$polit_econ = rowMeans(data9[econ],na.rm=TRUE)

#---- Recode political engagement ----

# interest in politics
data9$polintr <- zap_labels(data9$polintr) # remove spss labels
data9$polintr_r = 5 - data9$polintr # reversed (the higher the more interest)
data9$polintr_r_s = scales::rescale(data9$polintr_r)#scaling between 0-1

# political news reception (in minutes, each day): nwspol
data9$nwspol = zap_labels(data9$nwspol) # make sure the missing values are still coded as such (values no higher than 1440 min (which is 24h) )
data9$nwspol_s = scales::rescale(data9$nwspol)#scaling between 0-1

# political action (7 actions)
#contplt: Contacted politician or government official last 12 months
#wrkprty: Worked in political party or action group last 12 months
#wrkorg: Worked in another organisation or association last 12 months
#badge: Worn or displayed campaign badge/sticker last 12 months
#sgnptit: Signed petition last 12 months
#pbldmn: Taken part in lawful public demonstration last 12 months
#bctprd: Boycotted certain products last 12 months
#pstplonl: Posted or shared anything about politics online last 12 months
# reverse code (1 = yes, 2 = no into 0 = no, 1 = yes)
data9$contplt_r = 2 - data9$contplt
data9$wrkprty_r = 2 - data9$wrkprty
data9$wrkorg_r = 2 - data9$wrkorg
data9$badge_r = 2 - data9$badge
data9$sgnptit_r = 2 - data9$sgnptit
data9$pbldmn_r = 2 - data9$pbldmn
data9$bctprd_r = 2 - data9$bctprd
data9$pstplonl_r = 2 - data9$pstplonl
# get vector of vars
polit_action = c("contplt_r", "wrkprty_r", "wrkorg_r", "badge_r", 
                 "sgnptit_r", "pbldmn_r", "bctprd_r", "pstplonl_r")
data9$polit_action = rowSums(data9[polit_action], na.rm = TRUE)
data9$polit_action_s = scales::rescale(data9$polit_action) # scale between 0-1

# create one variable with political engagement
# includes: interest, media reception, and political action
polit_eng = c("polintr_r_s", 
              "nwspol_s",
              "polit_action_s")
data9$polit_eng = rowMeans(data9[polit_eng], na.rm = TRUE)
data9$polit_eng_c = c(scale(data9$polit_eng, center = TRUE, scale = FALSE))

#---- Recode control variable  ----
# remove spss labels
data9$agea <- zap_labels(data9$agea)
data9$eduyrs <- zap_labels(data9$eduyrs)
# recode
data9$gndr_01 = data9$gndr-1 #male=0, female=1
data9$agea_s = rescale(data9$agea)# scaling between 0-1
data9$eduyrs_s = rescale(data9$eduyrs)

#---- Recode welfare state classification  ----
data9$cntry <- zap_labels(data9$cntry)
data9$welstate <- recode(data9$cntry,
                         "SE" = 1, "NO" = 1, "DK" = 1, "FI" = 1, "IS" = 1, # nordic 
                         "IE" = 2, "GB" = 2, # liberal 
                         "NL" = 3,"LU" = 3, "DE" = 3, # contitental
                         "CH" = 3, "BE" = 3,"AT" = 3, "FR"=3, 
                         "ES" = 4,"IL" = 4,"IT" = 4, "GR" = 4, "TR" = 4,"PT" = 4, "CY" = 4, # southern
                         "RU" = 5, "EE" = 5, "BG" = 5,"CZ" = 5,"PL" = 5,"HR" = 5, "HU" = 5,"LV" = 5,"RO" = 5,"SI" = 5, # eastern
                         "SK" = 5,"UA" = 5, "LT" = 5, "AL" = 5, "XK" = 5,
                         "ME" = 5, "RS" = 5)
# change a reference category 
data9$welstate <- factor(data9$welstate,
                         levels = c(2,3,4,1,5),
                         labels = c("Liberal","Continental","Southern","Nordic","Eastern"))
summary(data9$welstate)
table(data9$welstate,data9$cntry)

#---- Add countries names ----
data9$cntry_full[data9$cntry == "AT"] <- "Austria"
data9$cntry_full[data9$cntry == "BE"] <- "Belgium"
data9$cntry_full[data9$cntry == "CH"] <- "Switzerland"
data9$cntry_full[data9$cntry == "CZ"] <- "Czechia"
data9$cntry_full[data9$cntry == "DE"] <- "Germany" 
data9$cntry_full[data9$cntry == "DK"] <- "Denmark"
data9$cntry_full[data9$cntry == "ES"] <- "Spain"
data9$cntry_full[data9$cntry == "FI"] <- "Finland"
data9$cntry_full[data9$cntry == "FR"] <- "France"
data9$cntry_full[data9$cntry == "GB"] <- "Great Britain" 
data9$cntry_full[data9$cntry == "GR"] <- "Greece"
data9$cntry_full[data9$cntry == "HU"] <- "Hungary"
data9$cntry_full[data9$cntry == "IE"] <- "Ireland"
#IL - Israel
data9$cntry_full[data9$cntry == "IT"] <- "Italy"
data9$cntry_full[data9$cntry == "LU"] <- "Luxembourg"
data9$cntry_full[data9$cntry == "NL"] <- "Netherlands"
data9$cntry_full[data9$cntry == "NO"] <- "Norway"
data9$cntry_full[data9$cntry == "PL"] <- "Poland"
data9$cntry_full[data9$cntry == "PT"] <- "Portugal"
data9$cntry_full[data9$cntry == "SE"] <- "Sweden"
data9$cntry_full[data9$cntry == "SI"] <- "Slovenia"
#TR - Turkey
data9$cntry_full[data9$cntry == "EE"] <- "Estonia"
data9$cntry_full[data9$cntry == "IS"] <- "Iceland"
data9$cntry_full[data9$cntry == "SK"] <- "Slovakia"
data9$cntry_full[data9$cntry == "UA"] <- "Ukraine"
data9$cntry_full[data9$cntry == "BG"] <- "Bulgaria"
data9$cntry_full[data9$cntry == "CY"] <- "Cyprus"
data9$cntry_full[data9$cntry == "RU"] <- "Russia"
data9$cntry_full[data9$cntry == "RO"] <- "Romania" 
data9$cntry_full[data9$cntry == "LV"] <- "Latvia"
data9$cntry_full[data9$cntry == "HR"] <- "Croatia"
data9$cntry_full[data9$cntry == "LT"] <- "Lithuania" 
data9$cntry_full[data9$cntry == "AL"] <- "Albania"
data9$cntry_full[data9$cntry == "XK"] <- "Kosovo"
data9$cntry_full[data9$cntry == "ME"] <- "Montenegro"
data9$cntry_full[data9$cntry == "RS"] <- "Serbia"
# check stats
data9$cntry_full = as.factor(data9$cntry_full)
summary(data9$cntry_full)
sum(is.na(data9$cntry_full))
sum(is.na(data9$cntry))

#--------------------------------------------------------------------------------------------------------
#---- Calculate values scores for PVQ ----
#---- recoding PVQ items (ESS uses 1 as "like me" and 6 "totally not like me)
data9$ipcrtiv_r = 7 - data9$ipcrtiv
data9$imprich_r = 7 - data9$imprich
data9$ipeqopt_r = 7 - data9$ipeqopt
data9$ipshabt_r = 7 - data9$ipshabt
data9$impsafe_r = 7 - data9$impsafe
data9$impdiff_r = 7 - data9$impdiff
data9$ipfrule_r = 7 - data9$ipfrule
data9$ipudrst_r = 7 - data9$ipudrst
data9$ipmodst_r = 7 - data9$ipmodst
data9$ipgdtim_r = 7 - data9$ipgdtim
data9$impfree_r = 7 - data9$impfree
data9$iphlppl_r = 7 - data9$iphlppl
data9$ipsuces_r = 7 - data9$ipsuces
data9$ipstrgv_r = 7 - data9$ipstrgv
data9$ipadvnt_r = 7 - data9$ipadvnt
data9$ipbhprp_r = 7 - data9$ipbhprp
data9$iprspot_r = 7 - data9$iprspot
data9$iplylfr_r = 7 - data9$iplylfr
data9$impenv_r = 7 - data9$impenv
data9$imptrad_r = 7 - data9$imptrad
data9$impfun_r = 7 - data9$impfun

#---- Reliability for the scores  ----

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
data9$cons_hvl = rowMeans(data9[cons_hvl], na.rm=TRUE) 

# openness 
open_hvl = c("ipcrtiv_r",
             "impfree_r",
             "impdiff_r",
             "ipadvnt_r")
data9$open_hvl = rowMeans(data9[open_hvl], na.rm=TRUE) 

# value for the dimension openness - conservation (new method)
data9$conservation2 = data9$cons_hvl - data9$open_hvl
data9$conservation2_s = rescale(data9$conservation2) 
data9$conservation2_s_c = c(scale(data9$conservation2_s, scale = FALSE, center = TRUE))

#---- Per-country reliabilities for higher order values ----
# get list of countries 
countries <- unique(data9$cntry_full)
countries
cons_hvl_a_all = data.frame() # create empty df
open_hvl_a_all = data.frame() # create empty df

# conservation
index = 0 # zero the index
for (c in countries) {
  index = index + 1
  print(paste0(c, " #", index))
  # select a country
  data9_cntry = data9 %>% filter(cntry_full == c)
  # calculate reliability
  cons_hvl_a <- psych::alpha(data9_cntry[cons_hvl])$total[1] # get alpha
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
  data9_cntry = data9 %>% filter(cntry_full == c)
  # calculate reliability
  open_hvl_a <- psych::alpha(data9_cntry[open_hvl])$total[1] # get alpha
  open_hvl_a <- open_hvl_a %>% rename(open_hvl_a = raw_alpha) # rename
  open_hvl_a$cntry_full <- c
  # bind rows
  open_hvl_a_all <- bind_rows(open_hvl_a_all, open_hvl_a)
}

#---- Remove cases with NA ----
data9 <- data9 %>%
  filter(!is.na(polit_econ),
         !is.na(conservation2),
         !is.na(econ),
         !is.na(agea_s),
         !is.na(gndr_01),
         !is.na(eduyrs_s),
         !is.na(polit_eng))
summary(data9$polit_econ)
nrow(data9) # n = 45,575

#---- Summary stats for the full sample and countries ----
# demographics
mean(data9$agea);sd(data9$agea) # 50.99, 18.54
mean(data9$eduyrs);sd(data9$eduyrs) # 12.93, 4.06
summary(as.factor(data9$gndr_01)) #M: 21,035; F:24,540 
# polit_econ
mean(data9$polit_econ) # 0.33
sd(data9$polit_econ) #0.18
psych::alpha(data9[econ]) # 0.51
# polit_eng
mean(data9$polit_eng) # 0.21
sd(data9$polit_eng) # 0.15
psych::alpha(data9[polit_eng]) # 0.39
# conservation
mean(data9$cons_hvl) # 4.35
sd(data9$cons_hvl) # 0.81
psych::alpha(data9[cons_hvl]) # 0.70
# openness
mean(data9$open_hvl)# 4.00
sd(data9$open_hvl) # 0.93
psych::alpha(data9[open_hvl]) # 0.66
# conservation - openness (NSC)
mean(data9$conservation2) # 0.36
sd(data9$conservation2) # 1.21

# per country descriptives
stats = data9 %>% 
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
data9$gndr_01 <- zap_labels(data9$gndr_01) 
gender_count = data9 %>% 
  group_by(cntry_full, gndr_01) %>%
  summarize(n = n()) %>% # this will count only women
  tidyr::spread(gndr_01, n)
gender_count
#write.table(gender_count, "gender_count.csv", sep = ";", dec = ",")

#--------------------------------------------------------------------------------------------------------
#---- Analysis ---- 
#---- Political engagement ----
m_econ0_1_2 = lmer(data = data9, polit_econ ~ 
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
m_econ2_1_2 = lmer(data = data9, polit_econ ~ 
                     conservation2_s_c * welstate * polit_eng_c +
                     gndr_01 + agea_s + eduyrs_s +
                     (1 + conservation2_s_c + polit_eng_c |cntry), 
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
