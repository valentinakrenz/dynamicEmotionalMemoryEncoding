## Modified mediation_analysis function to accept pre-filtered data
#mediation_analysis <- function(ROI_EES_i, univ_filtered, EES_filtered) {
#  
#  this_EES_df <- subset(EES_filtered, ROI_EES == ROI_EES_i) %>%
#    droplevels() 
#  
#  merged_df <- merge(univ_filtered, this_EES_df, by = c("item", "sj", "emotion", "subsMemory")) %>%
#    dplyr::arrange(sj, item, emotion, subsMemory) %>%
#    #mutate(subsMemory_num = as.numeric(subsMemory)-1)%>%
#    mutate(subsMemory = factor(subsMemory), emotion = factor(emotion))%>%
#    #filter(emotion == "1") %>%
#    droplevels()
#  unique(merged_df$emotion)
#  
#  med.neg.fit <- lme4::lmer(EES ~ beta_z*emotion  + (1|sj), data = merged_df)
#  summary(med.neg.fit)
#  
#  out.neg.fit <- lme4::glmer(subsMemory ~ beta_z*emotion + EES_z*emotion  + (1|sj), 
#                             data = merged_df, family = "binomial",
#                             control=glmerControl(optimizer="bobyqa",
#                                                  optCtrl=list(maxfun=2e5)))
#  summary(out.neg.fit)
#  
#  set.seed(2023)
#  med.negative <- mediation::mediate(med.neg.fit, out.neg.fit, treat = "beta_z",
#                                     mediator = "EES_z", covariates = list(emotion = 1), sims=1000)
#  summary(med.negative)
#  
#  set.seed(2023)
#  med.neutral <- mediation::mediate(med.fit, out.fit, treat = "beta_z",
#                                    mediator = "EES_z", covariates = list(emotion = 1), sims=1000)
#  
#  summary(med.neutral)
#  
#  summary_neut <- extract_mediation_summary(med.neutral)
#  summary_neg <- extract_mediation_summary(med.negative)
#  
#  # Combine the results of both emotions
#  combined_results <- list(
#    neutral = list(
#      summary = summary_neut,
#      mediate_result = med.neutral,
#      models = list(
#        mediator_model = med.fit.neut,
#        outcome_model = out.fit.neut
#      )
#    ),
#    negative = list(
#      summary = summary_neg,
#      mediate_result = med.negative,
#      models = list(
#        mediator_model = med.fit.neg,
#        outcome_model = out.fit.neg
#      )
#    )
#  )
#  
#  return(combined_results)
#}



## Modified mediation_analysis function to accept pre-filtered data
#mediation_analysis <- function(ROI_EES_i, univ_filtered, EES_filtered) {
#  
#  this_EES_df <- subset(EES_filtered, ROI_EES == ROI_EES_i) %>%
#    droplevels() 
#  
#  merged_df <- merge(univ_filtered, this_EES_df, by = c("item", "sj", "emotion", "subsMemory")) %>%
#    dplyr::arrange(sj, item, emotion, subsMemory) %>%
#    #mutate(subsMemory_num = as.numeric(subsMemory)-1)%>%
#    mutate(subsMemory = factor(subsMemory), emotion = factor(emotion))%>%
#    #filter(emotion == "1") %>%
#    droplevels()
#  unique(merged_df$emotion)
#  
#  
#  med.fit.neg <- lme4::lmer(beta_z ~ EES_z + (1|sj), data = subset(merged_df, emotion == "1"))
#  #summary(med.fit.neg)
#  
#  out.fit.neg <- lme4::glmer(subsMemory ~ beta_z + EES_z + (1|sj), 
#                             data = subset(merged_df, emotion == "1"), family = "binomial",
#                             control=glmerControl(optimizer="bobyqa",
#                                                  optCtrl=list(maxfun=2e5)))
#  
#  set.seed(2023)
#  med.negative <- mediation::mediate(med.fit.neg, out.fit.neg, treat = "EES_z",
#                                     mediator = "beta_z", sims=1000)
#  summary(med.fit.neg)
#  
#  med.fit.neut <- lme4::lmer(beta_z ~ EES_z + (1|sj), data = subset(merged_df, emotion == "0"))
#  #summary(med.fit.neut)
#  
#  out.fit.neut <- lme4::glmer(subsMemory ~ beta_z + EES_z + (1|sj), 
#                              data = subset(merged_df, emotion == "0"), family = "binomial",
#                              control=glmerControl(optimizer="bobyqa",
#                                                   optCtrl=list(maxfun=2e5)))
#  set.seed(2023)
#  med.neutral <- mediation::mediate(med.fit.neut, out.fit.neut, treat = "EES_z",
#                                    mediator = "beta_z", sims=1000)
#  
#  summary_neut <- extract_mediation_summary(med.neutral)
#  summary_neg <- extract_mediation_summary(med.negative)
#  
#  # Combine the results of both emotions
#  combined_results <- list(
#    neutral = list(
#      summary = summary_neut,
#      mediate_result = med.neutral,
#      models = list(
#        mediator_model = med.fit.neut,
#        outcome_model = out.fit.neut
#      )
#    ),
#    negative = list(
#      summary = summary_neg,
#      mediate_result = med.negative,
#      models = list(
#        mediator_model = med.fit.neg,
#        outcome_model = out.fit.neg
#      )
#    )
#  )
#  
#  return(combined_results)
#}

#EES_med_emo_on_mem <- function(merged_df) {
#  
#  med.fit <- lme4::lmer(EES ~ emotion + (1|sj), data = subset(merged_df))
#  #summary(med.fit.neg)
#  
#  out.fit.neg <- lme4::glmer(subsMemory ~ emotion * EES + (1 | sj), 
#                             data = subset(merged_df), family = "binomial",
#                             control = glmerControl(optimizer = "bobyqa",
#                                                  optCtrl = list(maxfun = 2e5)))
#  
#  set.seed(2023)
#  med.negative <- mediation::mediate(med.fit, out.fit, treat = "emotion",
#                                     mediator = "EES", sims = 1000)
#  summary(med.fit)
#  
#  summary <- extract_mediation_summary(med.neutral)
#  
#  # Combine the results of both emotions
#  combined_results <- list(
#      summary = summary,
#      mediate_result = med,
#      models = list(
#        mediator_model = med.fit,
#        outcome_model = out.fit
#      )
#  )
#  
#  return(combined_results)
#}#

mediation_analysis <- function(ROI_EES_i, Amy_df, EES_select_df) {
  
  EES_sub_df <- subset(EES_select_df, ROI == ROI_EES_i) %>%
    mutate(EES = scale(EES, scale = FALSE))%>%
    group_by(sj)%>%
    mutate(EES_ROI = ROI,
           M = scale(EES, scale = FALSE)) %>%
    ungroup() %>%
    dplyr::select(sj, item, EES_ROI, EncRuns, emotion, EES, M, subsMemory)
  
  # join EES and univ df
  merged_df <- inner_join(EES_sub_df, Amy_df, by = c("sj", "item"))
  
  merged_df <- merged_df %>%
    mutate(Y = as.integer(subsMemory),
           X = factor(emotion, levels=c("neutral","negative")), # emotion as predictor
           M = M, # within-subject centered EES as mediator
           Z = beta_z) # witin-sj centered initial Amy activity as modulator
  
  med.fit <- lme4::lmer(EES ~ X*Z + (1|sj), data = merged_df)
  #summary(med.fit)
  
  out.fit <- lme4::glmer(Y ~ X + Z + M + X:Z + M:Z + (1 | sj), 
                         data = merged_df, family = "binomial",
                         control = glmerControl(optimizer = "bobyqa",
                                                optCtrl = list(maxfun = 2e5)))
  #summary(out.fit)
  
  set.seed(2023)
  
  med <- mediation::mediate(med.fit, out.fit, treat = "X",
                            mediator = "M", covariates = list(Z = 0), sims = 5000) # at average for each subject
  #summary(med)
  
  set.seed(2023)
  
  med.high_Amy <- mediation::mediate(med.fit, out.fit, treat = "X",
                                     mediator = "M", covariates = list(Z = 1),  sims = 5000)
  #summary(med.high_Amy)
  
  set.seed(2023)
  
  med.low_Amy <- mediation::mediate(med.fit, out.fit, treat = "X",
                                    mediator = "M", covariates = list(Z = -1),  sims = 5000)
  #summary(med.low_Amy)
  
  # Combine the results of both emotions
  combined_results <- list(
    med_mean_Mod = extract_mediation_summary(med),
    med_high_Mod = extract_mediation_summary(med.high_Amy),
    med_low_Mod =  extract_mediation_summary(med.low_Amy),
    models = list(
      mediator_model = med.fit,
      outcome_model = out.fit
    )
  )
  
  return(combined_results)
}
