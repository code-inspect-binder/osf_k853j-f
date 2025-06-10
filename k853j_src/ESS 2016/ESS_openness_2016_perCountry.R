#--------------------------------------------------------------------------------------------------------
# Analysis of ESS data for the relationship of openess and political beliefs PER COUNTRY
# ESS 2016 (round 8)
#
# Note: first run the code in the "ESS_openness_2016.R" script
#--------------------------------------------------------------------------------------------------------

#---- Load libraries ----
library(tidyr)
library(gridExtra) # for grid.arrange
library(forcats)
library(ggsci) # for journal color palettes

#---- Get vector sorting countries ----
countries_sort <- c(
  # Liberal
  "Great Britain", 
  "Ireland", 
  # Continental
  "Switzerland", 
  "Germany", 
  "Netherlands", 
  "Belgium", 
  "Austria", 
  "France", 
  "Andorra", 
  # Southern
  "Spain", 
  "Cyprus", 
  "Italy", 
  "Portugal", 
  # Nordic
  "Norway", 
  "Sweden", 
  "Iceland", 
  "Finland", 
  # Eastern
  "Slovenia", 
  "Czechia", 
  "Estonia", 
  "Poland", 
  "Lithuania", 
  "Hungary", 
  "Russia", 
  "Bulgaria", 
  "Ukraine", 
  "Serbia", 
  "Romania", 
  "Moldova")
# sort the countries
countries_sort <- data.frame(countries_sort, 1:length(countries_sort))
names(countries_sort) <- c("Country", "cntry_order")
countries_sort

#---- Create list of dataframes per country -----
countries <- unique(data8$cntry_full)
index = 0
listofdfs <- list() 

# create list of separate dataframes per country
for (c in countries) {
  index = index + 1
  print(paste0(c, " #", index))
  data_c = data8 %>% filter(cntry_full == c) # select only data from a country c 
  listofdfs[[index]] <- data_c # save df in a list 
  names(listofdfs)[[index]] <- paste0(c) # add name of a df
}

#-------------------------------------------------------------------------------------------------
#---- Run per-country regression ----
# values calculated as in Grigoryan & Schwartz (2020)
listoflms = list() # create empty list for linear models
index = 0 # zero the index 
pvalslms = numeric() # pvals 
summarylms = numeric() # get model summaries
betalms <- data.frame(beta = numeric(0), SE = numeric(0)) #, period = character(0)) 
betalms_ope <- data.frame(beta = numeric(0), SE = numeric(0)) #, period = character(0)) 
betalms_eng <- data.frame(beta = numeric(0), SE = numeric(0)) #, period = character(0)) 

# run per-country regressions
for (df in listofdfs) {
  index = index + 1
  # run model
  model <- lm(data = df, 
              polit_econ ~ 
                conservation2_s_c * polit_eng_c + #* agea_s + 
                gndr_01 + agea_s + eduyrs_s)
  listoflms <- c(listoflms, list(model))
  names(listoflms)[index] <- names(listofdfs[index])# add name of a lm
  # get model summary (without intercept)
  summarylms <- c(summarylms, summary(model)$coefficients[2,])
  # get b values
  beta <- summary(model)$coefficients[7, 1:2] # coefficient for interaction
  beta_ope <- summary(model)$coefficients[2, 1:2] # coeffirients for ideology
  beta_eng <- summary(model)$coefficients[3, 1:2] # coeffirients for education
  #beta <- as.data.frame(beta)
  names(beta) <- c("beta", "SE")
  names(beta_ope) <- c("beta", "SE")
  names(beta_eng) <- c("beta", "SE")
  # bind rows for coefficients
  betalms <- rbind(betalms, beta)
  betalms_ope <- rbind(betalms_ope, beta_ope)
  betalms_eng <- rbind(betalms_eng, beta_eng)
}
names(listoflms)

#---- Plot beta weights for the interaction ----
betalms # called "beta" but represents "b"
betalms <- betalms[,1:2]
names(betalms) <- c("b", "SE")
# add a colulmn with period 
labs <- tidyr::unnest(as.data.frame(names(listofdfs)))
names(labs) <- "Country"
labs
# add a column
betalms <- betalms %>% bind_cols(labs)
betalms
# add "term" (so it can be used with a dot-whisker plot)
betalms <- 
  betalms %>%
  #arrange(-b) %>% # arrange by the interaction term OR by welfare regime
  left_join(countries_sort) %>% 
  arrange(-cntry_order) %>% # arrange by a pre-defined countries 
  left_join(data8 %>% dplyr::select(cntry_full, welstate) %>% group_by(cntry_full, welstate) %>% 
              slice(1) %>% 
              ungroup() %>%
              rename(Country = cntry_full, `Welfare state model` = welstate)) %>% # add welfare state 
  mutate(term = "Interaction", # add info about the measure
         no = 1:nrow(betalms),
         Country = fct_reorder(Country, (no))) # re-ordder factors for the plot
betalms
# plot
betalms_p <- betalms %>% 
  # plot
  ggplot() + 
  geom_pointrange(aes(x = Country, 
                      y = b,
                      color = `Welfare state model`,
                      ymin = b - (1.96 * SE) ,
                      ymax = b + (1.96 * SE)),
                  lwd = 0.7, position = position_dodge(width = 1)) + 
  geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) + 
  xlab("Economic beliefs") +
  ylab("NSC x Political engagement") +
  coord_flip() +
  ylim(c(-2.1, 2.1)) +
  theme_classic() + 
  theme(legend.position = "none") +
  scale_color_npg()
betalms_p 

#---- Plot beta weights for main effects of needs for security and certainty)
betalms_ope
betalms_ope <- betalms_ope[,1:2]
names(betalms_ope) <- c("b", "SE")
# add a colulmn with period 
labs <- tidyr::unnest(as.data.frame(names(listofdfs)))
names(labs) <- "Country"
labs
# add a column
betalms_ope <- betalms_ope %>% bind_cols(labs)
betalms_ope
# add "term" (so it can be used with a dot-whisker plot)
betalms_ope <- 
  betalms_ope %>%
  #arrange(-b) %>% # arrange by the interaction term OR by welfare regime
  left_join(countries_sort) %>% 
  arrange(-cntry_order) %>% # arrange by a pre-defined countries 
  left_join(data8 %>% dplyr::select(cntry_full, welstate) %>% group_by(cntry_full, welstate) %>% 
              slice(1) %>% 
              ungroup() %>%
              rename(Country = cntry_full, `Welfare state model` = welstate)) %>% # add welfare state 
  mutate(term = "Interaction", # add info about the measure
         no = 1:nrow(betalms_ope),
         Country = fct_reorder(Country, (no))) # re-ordder factors for the plot
betalms_ope
# plot
betalms_ope_p <- betalms_ope %>% 
  # plot
  ggplot() + 
  geom_pointrange(aes(x = Country, 
                      y = b,
                      color = `Welfare state model`,
                      ymin = b - (1.96 * SE) ,
                      ymax = b + (1.96 * SE)),
                  lwd = 0.7, position = position_dodge(width = 1)) + 
  geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) + 
  xlab("Economic beliefs") +
  ylab("NSC") + #("Needs for security and certainty") +
  coord_flip() +
  ylim(c(-0.4, 0.4)) +
  theme_classic() + 
  theme(legend.position = "none") +
  scale_color_npg()
betalms_ope_p

#---- Plot beta weights for engagement
betalms_eng
betalms_eng <- betalms_eng[,1:2]
names(betalms_eng) <- c("b", "SE")
# add a colulmn with period 
labs <- tidyr::unnest(as.data.frame(names(listofdfs)))
names(labs) <- "Country"
labs
# add a column
betalms_eng <- betalms_eng %>% bind_cols(labs)
betalms_eng
# add "term" (so it can be used with a dot-whisker plot)
betalms_eng <- 
  betalms_eng %>%
  #arrange(-b) %>% # arrange by the interaction term OR by welfare regime
  left_join(countries_sort) %>% 
  arrange(-cntry_order) %>% # arrange by a pre-defined countries 
  left_join(data8 %>% dplyr::select(cntry_full, welstate) %>% group_by(cntry_full, welstate) %>% 
              slice(1) %>% 
              ungroup() %>%
              rename(Country = cntry_full, `Welfare state model` = welstate)) %>% # add welfare state 
  mutate(term = "Interaction", # add info about the measure
         no = 1:nrow(betalms_eng),
         Country = fct_reorder(Country, (no))) # re-ordder factors for the plot
betalms_eng
# plot
betalms_eng_p <- betalms_eng %>% 
  # plot
  ggplot() + 
  geom_pointrange(aes(x = Country, 
                      y = b,
                      color = `Welfare state model`,
                      ymin = b - (1.96 * SE) ,
                      ymax = b + (1.96 * SE)),
                  lwd = 0.7, position = position_dodge(width = 1)) + 
  geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) + 
  xlab("Economic beliefs") +
  ylab("Political engagement") +
  coord_flip() +
  ylim(c(-0.4, 0.4)) +
  theme_classic() + 
  theme(legend.position = "none") +
  scale_color_npg()
betalms_eng_p

#---- Arrange b coeff plots
grid.arrange(betalms_ope_p,
             betalms_eng_p, 
             betalms_p, 
             ncol = 3, nrow = 1)

#---- Plot slopes ----
listofcharts = list() # create empty list for charts
index = 0 # zero the index 
for (df in listofdfs) {
  index = index + 1
  xlab_str = paste0("Economic beliefs in ", names(listofdfs)[index])
  listofcharts[[index]] <- 
    ggpredict(listoflms[[index]], 
              terms = c("conservation2_s_c", "polit_eng_c[-0.14,0.14]"), type = "fe") %>%
    plot(colors = "bw") + 
    ggtitle(names(listoflms[index])) + 
    xlab("NSC") + ylab("Economic beliefs") + 
    labs(linetype = "Political \nengagement") +
    scale_linetype_manual(values=c("solid", "dashed"), labels = c("Low", "High")) +
    theme_classic() + 
    theme(legend.position = "none") +
    #theme(text=element_text(size=16, family = "sans"))
    coord_cartesian(xlim = c(-0.5, 0.5), ylim = c(0.2,0.8))
  names(listofcharts)[index] <- names(listofdfs[index])# add name of a chart
}
# check names
names(listofcharts)

# change order of elements, according to the interaction coef (to match the coef chart it needs to be re-ordered again)
betalms <- betalms %>%
  #arrange(b) %>% # arrange by the interaction term
  mutate(#term = "Interaction", # add info about the measure
    Country = fct_reorder(Country, (-no))) # re-ordder factors for plotting to match b-value plot
listofcharts <- listofcharts[levels(betalms$Country)]

# print all charts
do.call("grid.arrange", c(listofcharts, ncol = 6)) # save with 1500 x 700 dim

#---- Get slope estimates ----
listofslopes = list() # create empty list for charts
index = 0 # zero the index 
for (df in listofdfs) {
  index = index + 1
  # estimates with t and p-value
  listofslopes[[index]] <- as.matrix(test(emtrends(listoflms[[index]], ~ polit_eng_c, var = "conservation2_s_c", 
                                                  at = list(polit_eng_c = c(-0.14,0.14))), details=TRUE))
  names(listofslopes)[index] <- names(listofdfs[index])# add name of a chart
}
# check names
names(listofslopes)

# re-oder the slopes (betalms need to be reversed again)
betalms <- betalms %>%  mutate(Country = fct_reorder(Country, (-no))) # re-ordder factors for plotting to match b-value plot
listofslopes <- listofslopes[levels(betalms$Country)]
listofslopes

