# This file analyzes World Cup data, once cleaned

# Created by Julian Gerez

wc_results_data <- read.csv(paste0(directory, "wc_results_data.csv"), stringsAsFactors = FALSE)

# Calculate goal statistics per game

wc_results_data$qgfpg <- wc_results_data$qgf/wc_results_data$qgp
wc_results_data$qgapg <- wc_results_data$qga/wc_results_data$qgp
wc_results_data$qgdpg <- wc_results_data$qgd/wc_results_data$qgp

wc_results_data$fgfpg <- wc_results_data$fgf/wc_results_data$fgp
wc_results_data$fgapg <- wc_results_data$fga/wc_results_data$fgp
wc_results_data$fgdpg <- wc_results_data$fgd/wc_results_data$fgp

# Split data into separate data frames

wc_results_dataframes   <- split(wc_results_data, wc_results_data$tourn_id)

# Create "modern" tournmanent data

wc_results_data_modern <- subset(wc_results_data, 
                                 wc_results_data$tourn_id == 'wc_1998' |
                                 wc_results_data$tourn_id == 'wc_2002' |
                                 wc_results_data$tourn_id == 'wc_2006' |
                                 wc_results_data$tourn_id == 'wc_2010' |
                                 wc_results_data$tourn_id == 'wc_2014' )

# You can subset the data by excluding teams with low number of qualification games played here:

wc_results_data <- subset(wc_results_data,
                          wc_results_data$tourn_id != 'total' &
                          wc_results_data$tourn_id != 'modern')

quantile(wc_results_data$qgp, na.rm = TRUE)

wc_results_data_subset <- subset(wc_results_data,
                           wc_results_data$qgp > 3) # Change this line

# Logistic models, champion dummy as outcome

  # Model 1 = points percentage
  # Model 2 = qualifying goal difference per game
  # Model 3 = qualifying goals per game
  # Model 4 = qualifying goals against per game

{
  
  # Country-tournament data
  
    # Model 1
      ctlog1 <- glm(cd ~ qpp, family=binomial(link=`logit`), 
                        data=wc_results_data)
      summary(ctlog1)
    
    # Model 2
      ctlog2 <- glm(cd ~ qgdpg, family=binomial(link=`logit`), 
                        data=wc_results_data)
      summary(ctlog2)
    
    # Model 3
      ctlog3 <- glm(cd ~ qgfpg, family=binomial(link=`logit`), 
                        data=wc_results_data)
      summary(ctlog3)
    
    # Model 4
      ctlog4 <- glm(cd ~ qgapg, family=binomial(link=`logit`), 
                        data=wc_results_data)
      summary(ctlog4)
  
  # Country-tournament data, 32-team tournaments
  
    # Model 1
      ct_m_log1 <- glm(cd ~ qpp, family=binomial(link=`logit`), 
                                 data=wc_results_data_modern)
      summary(ct_m_log1)
    
    # Model 2
      ct_m_log2 <- glm(cd ~ qgdpg, family=binomial(link=`logit`), 
                                 data=wc_results_data_modern)
      summary(ct_m_log2)
    
    # Model 3
      ct_m_log3 <- glm(cd ~ qgfpg, family=binomial(link=`logit`), 
                                 data=wc_results_data_modern)
      summary(ct_m_log3)
    
    # Model 4
      ct_m_log4 <- glm(cd ~ qgapg, family=binomial(link=`logit`), 
                                 data=wc_results_data_modern)
      summary(ct_m_log4)
    
  # Summary of logistic models
  
    # Country-tournament data
      stargazer(ctlog1, ctlog2, ctlog3, ctlog4, 
                type = "html")
    
    # Country-tournament data, 32-team tournaments
      stargazer(ct_m_log1, ct_m_log2, ct_m_log3, 
                ct_m_log4,
                type = "html")

}      
      
# Complementary log-log models, champion dummy as outcome

  # Model 1 = points percentage
  # Model 2 = qualifying goal difference per game
  # Model 3 = qualifying goals per game
  # Model 4 = qualifying goals against per game

{

  # Country-tournament data
  
    # Model 1
    ctcloglog1 <- glm(cd ~ qpp, family=binomial(link=`cloglog`), 
                    data=wc_results_data)
      summary(ctcloglog1)
    
    # Model 2
    ctcloglog2 <- glm(cd ~ qgdpg, family=binomial(link=`cloglog`), 
                        data=wc_results_data)
      summary(ctcloglog2)
    
    # Model 3
    ctcloglog3 <- glm(cd ~ qgfpg, family=binomial(link=`cloglog`), 
                        data=wc_results_data)
      summary(ctcloglog3)
    
    # Model 4
    ctcloglog4 <- glm(cd ~ qgapg, family=binomial(link=`cloglog`), 
                        data=wc_results_data)
      summary(ctcloglog4)
  
  # Country-tournament data, 32-team tournaments
    
    # Model 1
    ct_m_cloglog1 <- glm(cd ~ qpp, family=binomial(link=`cloglog`), 
                       data=wc_results_data_modern)
      summary(ct_m_cloglog1)
    
    # Model 2
    ct_m_cloglog2 <- glm(cd ~ qgdpg, family=binomial(link=`cloglog`), 
                       data=wc_results_data_modern)
      summary(ct_m_cloglog2)
    
    # Model 3
    ct_m_cloglog3 <- glm(cd ~ qgfpg, family=binomial(link=`cloglog`), 
                           data=wc_results_data_modern)
    summary(ct_m_cloglog3)
    
    # Model 4
    ct_m_cloglog4 <- glm(cd ~ qgapg, family=binomial(link=`cloglog`), 
                           data=wc_results_data_modern)
    summary(ct_m_cloglog4)

# Summary of comp-loglog models, champion dummy

  # Country-tournament data
  stargazer(ctcloglog1, ctcloglog2, ctcloglog3, ctcloglog4, 
            type = "html")
  
  # Country-tournament data, 32-team tournaments
  stargazer(ct_m_cloglog1, ct_m_cloglog2, ct_m_cloglog3, 
            ct_m_cloglog4,
            type = "html")
  
}

# Linear regression models, finals points possible as outcome

  # Model 1 = points percentage
  # Model 2 = qualifying goal difference per game
  # Model 3 = qualifying goals per game
  # Model 4 = qualifying goals against per game

{

  # Country-tournament data
  
    # Model 1
      ctlinear1 <- lm(fpp ~ qpp, data=wc_results_data)
      summary(ctlinear1)
    
    # Model 2
      ctlinear2 <- lm(fpp ~ qgdpg, data=wc_results_data)
      summary(ctlinear2)
    
    # Model 3
      ctlinear3 <- lm(fpp ~ qgfpg, data=wc_results_data)
      summary(ctlinear3)
    
    # Model 4
      ctlinear4 <- lm(fpp ~ qgapg, data=wc_results_data)
      summary(ctlinear4)
  
  # Country-tournament data, 32-team tournaments
  
    # Model 1
        ct_m_linear1 <- lm(fpp ~ qpp, data=wc_results_data_modern)
        summary(ct_m_linear1)
    
      # Model 2
        ct_m_linear2 <- lm(fpp ~ qgdpg, data=wc_results_data_modern)
        summary(ct_m_linear2)
    
    # Model 3
        ct_m_linear3 <- lm(fpp ~ qgfpg, data=wc_results_data_modern)
        summary(ct_m_linear3)
    
    # Model 4
        ct_m_linear4 <- lm(fpp ~ qgapg, data=wc_results_data_modern)
        summary(ct_m_linear4)
        
    # Country-tournament data, subset
        
        # Model 1
        ct_s_linear1 <- lm(fpp ~ qpp, data=wc_results_data_subset)
        summary(ct_s_linear1)
        
        # Model 2
        ct_s_linear2 <- lm(fpp ~ qgdpg, data=wc_results_data_subset)
        summary(ct_s_linear2)
        
        # Model 3
        ct_s_linear3 <- lm(fpp ~ qgfpg, data=wc_results_data_subset)
        summary(ct_s_linear3)
        
        # Model 4
        ct_s_linear4 <- lm(fpp ~ qgapg, data=wc_results_data_subset)
        summary(ct_s_linear4)
  
  # Summary of linear models, final points ratio
  
    # Country-tournament data
      stargazer(ctlinear1, ctlinear2, ctlinear3, ctlinear4, 
                type = "html")
    
    # Country-tournament data, 32-team tournaments
      stargazer(ct_m_linear1, ct_m_linear2, ct_m_linear3, 
                ct_m_linear4,
                type = "html")
      
    # Country-tournament data, subset
      stargazer(ct_s_linear1, ct_s_linear2, ct_s_linear3, 
                ct_s_linear4,
                type = "html")
      
}
  
# Plots for non-binary outcomes
  
  # Country-tournament data
  
    # Figure 1
    ggplot(wc_results_data, aes(x=qgfpg, y=fpp)) + 
      geom_point(color = ifelse(wc_results_data$cd == 1, "plum1", "purple4")) +
      # geom_text(label = print(wc_results_data))
      geom_smooth(method=lm, linetype="dashed",
                  color="black") +
      labs(x = "Qualifying Goals For Per Game",
           y = "Finals Points Ratio")
    
    # Figure 2
    ggplot(wc_results_data, aes(x=qgfpg, y=fpp)) + 
      geom_point(color = ifelse(wc_results_data$cd == 1, "plum1", "purple4"),
                 size = wc_results_data$qgp / 5) + 
      # geom_text(label = print(wc_results_data))
      geom_smooth(method=lm, linetype="dashed",
                  color="black") +
      labs(x = "Qualifying Goals For Per Game",
           y = "Finals Points Ratio")
    
    # Figure 3
    ggplot(wc_results_data_modern, aes(x=qgfpg, y=fpp)) + 
      geom_point(color = ifelse(wc_results_data_modern$cd == 1, "plum1", "purple4")) + 
      # geom_text(label = print(wc_results_data))
      geom_smooth(method=lm, linetype="dashed",
                  color="black") +
      labs(x = "Qualifying Goals For Per Game",
           y = "Finals Points Ratio")
    
    # Figure 4
    
    ggplot(wc_results_data, aes(x=qpp, y=fpp)) + 
        geom_point(color = ifelse(wc_results_data$cd == 1, "plum1", "purple4"),
                   size = ifelse(wc_results_data$cd == 1, 2, 1.5)) + 
        # geom_text(label = print(wc_results_data))
        geom_smooth(method=lm, linetype="dashed",
                    color="black") +
            labs(x = "Qualifying Points Ratio",
                 y = "Finals Points Ratio")
    
    # Figure 5
    
    ggplot(wc_results_data_modern, aes(x=qpp, y=fpp)) + 
      geom_point(color = ifelse(wc_results_data_modern$cd == 1, "plum1", "purple4"),
                 size = ifelse(wc_results_data_modern$cd == 1, 2, 1.5)) + 
      # geom_text(label = print(wc_results_data))
      geom_smooth(method=lm, linetype="dashed",
                  color="black") +
      labs(x = "Qualifying Points Ratio",
           y = "Finals Points Ratio")