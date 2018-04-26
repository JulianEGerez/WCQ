# This file analyzes World Cup data at the confederation level

# Created by Julian Gerez

# Note: for the models you must replace "### insert confederation data here" with the
# confederation you wish to analyze, ex: wc_results_data_AFC, and you should replace
# starting on line 127, not before then.

rm(wc_results_data, wc_results_data_modern, wc_results_data_subset, wc_results_dataframes)
rm(ctcloglog1, ctcloglog2, ctcloglog3, ctcloglog4, ct_m_cloglog1, ct_m_cloglog2, ct_m_cloglog3, 
   ct_m_cloglog4, ctlinear1, ctlinear2, ctlinear3, ctlinear4, ct_m_linear1, ct_m_linear2, 
   ct_m_linear3, ct_m_linear4, ct_s_linear1, ct_s_linear2, ct_s_linear3, ct_s_linear4)

# Read in data

wc_results_data <- read.csv(paste0(directory, "wc_results_data.csv"), stringsAsFactors = FALSE)

# Calculate goal statistics per game

wc_results_data$qgfpg <- wc_results_data$qgf/wc_results_data$qgp
wc_results_data$qgapg <- wc_results_data$qga/wc_results_data$qgp
wc_results_data$qgdpg <- wc_results_data$qgd/wc_results_data$qgp

wc_results_data$fgfpg <- wc_results_data$fgf/wc_results_data$fgp
wc_results_data$fgapg <- wc_results_data$fga/wc_results_data$fgp
wc_results_data$fgdpg <- wc_results_data$fgd/wc_results_data$fgp

# Add confederation dummies

wc_results_data$AFC <- recode(wc_results_data$team, 
     "Australia" = 1, "China" = 1, "Indonesia/Nether. Indies" = 1, "Iran" = 1,
     "Iraq" = 1, "Israel" = 1, "Japan" = 1, "Kuwait" = 1, "North Korea" = 1,
     "Saudi Arabia" = 1, "South Korea" = 1, "United Arab Emirates" = 1, .default = 0)

wc_results_data$CAF <- recode(wc_results_data$team, 
     "Algeria" = 1, "Angola" = 1, "Cameroon" = 1, "DRC/Zaire" = 1, "Egypt" = 1,
     "Ghana" = 1, "Ivory Coast" = 1, "Morocco" = 1, "Nigeria" = 1, "Senegal" = 1,
     "South Africa" = 1, "Togo" = 1, "Tunisia" = 1, .default = 0)
  
wc_results_data$CONCACAF <- recode(wc_results_data$team,
     "Canada" = 1, "Costa Rica" = 1, "Cuba" = 1, "El Salvador" = 1, "Haiti" = 1,
     "Honduras" = 1, "Jamaica" = 1, "Mexico" = 1, "Trinidad and Tobago" = 1,
     "USA" = 1, .default = 0)
  
wc_results_data$CONMEBOL <- recode(wc_results_data$team,
     "Argentina" = 1, "Bolivia" = 1, "Brazil" = 1, "Chile" = 1, "Colombia" = 1,
     "Ecuador" = 1, "Paraguay" = 1, "Peru" = 1, "Uruguay" = 1, .default = 0)
  
wc_results_data$OFC <- recode(wc_results_data$team,
     "Australia" = 1, "New Zealand" = 1, .default = 0)
  
wc_results_data$UEFA <- recode(wc_results_data$team,
     "Austria" = 1, "Belgium" = 1, "Bosnia-Herzegovina" = 1, "Bulgaria" = 1,
     "Croatia" = 1, "Czech Republic/CSFR" = 1, "Denmark" = 1, "England" = 1,
     "France" = 1, "German Dem. Rep. (East Germany)" = 1, "Germany" = 1,
     "Greece" = 1, "Hungary" = 1, "Ireland" = 1, "Israel" = 1, "Italy" = 1,
     "Netherlands" = 1, "Northern Ireland" = 1, "Norway" = 1, "Poland" = 1,
     "Portugal" = 1, "Romania" = 1, "Russia/USSR" = 1, "Scotland" = 1,
     "Serbia/Yugoslavia" = 1, "Slovakia" = 1, "Slovenia" = 1, "Spain" = 1,
     "Spain" = 1, "Sweden" = 1, "Switzerland" = 1, "Turkey" = 1,
     "Ukraine" = 1, "Wales" = 1, .default = 0)

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

# Create confederation datasets

wc_results_data_AFC      <- subset(wc_results_data,
                              wc_results_data$AFC == 1)

wc_results_data_CAF      <- subset(wc_results_data,
                              wc_results_data$CAF == 1)

wc_results_data_CONCACAF <- subset(wc_results_data,
                              wc_results_data$CONCACAF == 1)

wc_results_data_CONMEBOL <- subset(wc_results_data,
                              wc_results_data$CONMEBOL == 1)

wc_results_data_OFC      <- subset(wc_results_data,
                              wc_results_data$OFC == 1)

wc_results_data_UEFA     <- subset(wc_results_data,
                              wc_results_data$UEFA == 1)

# Create "modern" confederation datasets

wc_results_data_modern_AFC      <- subset(wc_results_data_modern,
                                   wc_results_data_modern$AFC == 1)

wc_results_data_modern_CAF      <- subset(wc_results_data_modern,
                                   wc_results_data_modern$CAF == 1)

wc_results_data_modern_CONCACAF <- subset(wc_results_data_modern,
                                   wc_results_data_modern$CONCACAF == 1)

wc_results_data_modern_CONMEBOL <- subset(wc_results_data_modern,
                                   wc_results_data_modern$CONMEBOL == 1)

wc_results_data_modern_OFC      <- subset(wc_results_data_modern,
                                   wc_results_data_modern$OFC == 1)

wc_results_data_modern_UEFA     <- subset(wc_results_data_modern,
                                   wc_results_data_modern$UEFA == 1)

# Logistic models, champion dummy as outcome

  # Model 1 = points percentage
  # Model 2 = qualifying goal difference per game
  # Model 3 = qualifying goals per game
  # Model 4 = qualifying goals against per game
  
  # Country-tournament data
  
    # Model 1
      ctlog1 <- glm(cd ~ qpp, family=binomial(link=`logit`), 
                        data= ### insert confederation data here)
      summary(ctlog1)
    
    # Model 2
      ctlog2 <- glm(cd ~ qgdpg, family=binomial(link=`logit`), 
                        data= ### insert confederation data here)
      summary(ctlog2)
    
    # Model 3
      ctlog3 <- glm(cd ~ qgfpg, family=binomial(link=`logit`), 
                        data= ### insert confederation data here)
      summary(ctlog3)
    
    # Model 4
      ctlog4 <- glm(cd ~ qgapg, family=binomial(link=`logit`), 
                        data= ### insert confederation data here)
      summary(ctlog4)
    
  # Summary of logistic models
  
    # Country-tournament data
      stargazer(ctlog1, ctlog2, ctlog3, ctlog4, 
                type = "html")
      
# Complementary log-log models, champion dummy as outcome

  # Model 1 = points percentage
  # Model 2 = qualifying goal difference per game
  # Model 3 = qualifying goals per game
  # Model 4 = qualifying goals against per game

  # Country-tournament data
  
    # Model 1
    ctcloglog1 <- glm(cd ~ qpp, family=binomial(link=`cloglog`), 
                    data= ### insert confederation data here)
      summary(ctcloglog1)
    
    # Model 2
    ctcloglog2 <- glm(cd ~ qgdpg, family=binomial(link=`cloglog`), 
                        data= ### insert confederation data here)
      summary(ctcloglog2)
    
    # Model 3
    ctcloglog3 <- glm(cd ~ qgfpg, family=binomial(link=`cloglog`), 
                        data= ### insert confederation data here)
      summary(ctcloglog3)
    
    # Model 4
    ctcloglog4 <- glm(cd ~ qgapg, family=binomial(link=`cloglog`), 
                        data= ### insert confederation data here)
      summary(ctcloglog4)

# Summary of comp-loglog models, champion dummy

  # Country-tournament data
  stargazer(ctcloglog1, ctcloglog2, ctcloglog3, ctcloglog4, 
            type = "html")

# Linear regression models, finals points possible as outcome

  # Model 1 = points percentage
  # Model 2 = qualifying goal difference per game
  # Model 3 = qualifying goals per game
  # Model 4 = qualifying goals against per game


  # Country-tournament data
  
    # Model 1
      ctlinear1 <- lm(fpp ~ qpp, data= ### insert confederation data here)
      summary(ctlinear1)
    
    # Model 2
      ctlinear2 <- lm(fpp ~ qgdpg, data= ### insert confederation data here)
      summary(ctlinear2)
    
    # Model 3
      ctlinear3 <- lm(fpp ~ qgfpg, data= ### insert confederation data here)
      summary(ctlinear3)
    
    # Model 4
      ctlinear4 <- lm(fpp ~ qgapg, data= ### insert confederation data here)
      summary(ctlinear4)
  
  # Summary of linear models, final points ratio
  
    # Country-tournament data
      stargazer(ctlinear1, ctlinear2, ctlinear3, ctlinear4, 
                type = "html")

# Figures for each confederation

ggplot(### insert confederation data here, aes(x=qpp, y=fpp)) + 
  geom_point(color = ifelse(### insert confederation data here$cd == 1, "plum1", "purple4"),
             size = ifelse(### insert confederation data here$cd == 1, 2, 1.5)) + 
  # geom_text(label = print(### insert confederation data here))
  geom_smooth(method=lm, linetype="dashed",
              color="black") +
  labs(x = "Qualifying Points Ratio",
       y = "Finals Points Ratio")