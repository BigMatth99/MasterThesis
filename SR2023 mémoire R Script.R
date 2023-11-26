#Open the file

library(readxl)

mydata <- read_excel("C:/Users/Matth/Downloads/Service Robots in restaurants _ Win a dining experience(1-175).xlsx")
View(mydata)

library(dplyr)
library(psych)
library(semPlot)
library(lavaan)
library(semTools)

# Create a lookup table to map Likert scale responses to numerical values
likert_map <- c("Strongly Disagree" = 1, "Disagree" = 2, "Neutral" = 3, "Agree" = 4, "Strongly Agree" = 5)
mydata <- mydata %>%
  mutate_all(~ifelse(. %in% names(likert_map), likert_map[.], .))

#Rename all the variables

names(mydata)[names(mydata) == "What is your age ?"] <- "Age"
names(mydata)[names(mydata) == "Do you feel more"] <- "Attitude"
names(mydata)[names(mydata) == "Pick a number"] <- "Situations"
names(mydata)[names(mydata) == "I would be willing to go to a restaurant which employs service robots if my friends are interested in doing so"] <- "SOI1"
names(mydata)[names(mydata) == "Going to restaurants that employ service robots will be a status symbol within my social networks"] <- "SOI2"
names(mydata)[names(mydata) == "People who are important to me may encourage me to visit a restaurant which employs service robots"] <- "SOI3"
names(mydata)[names(mydata) == "It seems that the service robot has a mind of its own"] <- "ANT1"
names(mydata)[names(mydata) == "It seems that the service robot is capable of reasoning of its own"] <- "ANT2"
names(mydata)[names(mydata) == "It seems that the service robot might experience emotions"] <- "ANT3"
names(mydata)[names(mydata) == "It seems that the service robot has its own free will"] <- "ANT4"
names(mydata)[names(mydata) == "Given its appearance, the service robot seems to be competent"] <- "INT1"
names(mydata)[names(mydata) == "The service robot seems to be intelligent"] <- "INT2"
names(mydata)[names(mydata) == "The service robot seems to be responsible"] <- "INT3"
names(mydata)[names(mydata) == "The service robot seems to be sensible"] <- "INT4"
names(mydata)[names(mydata) == "I think that the service robot is more accurate than humans beings"] <- "PEF1"
names(mydata)[names(mydata) == "I think that the service robot would make less mistakes than human beings"] <- "PEF2"
names(mydata)[names(mydata) == "I think that the service robot is more consistent than human beings"] <- "PEF3"
names(mydata)[names(mydata) == "I think that the service robot can provide me better informations than human beings"] <- "PEF4"
names(mydata)[names(mydata) == "The service robot itself seems safe (the design of the robot seems safe enough to prevent accidents)"] <- "SAF1"
names(mydata)[names(mydata) == "I think that the service robot is safe for me (and its user)"] <- "SAF2"
names(mydata)[names(mydata) == "The service robot does not bring me a safety hazard if I'm using it"] <- "SAF3"
names(mydata)[names(mydata) == "I think that interacting with the service robot would be fun"] <- "HEM1"
names(mydata)[names(mydata) == "I think that interacting with the service robot would be entertaining"] <- "HEM2"
names(mydata)[names(mydata) == "I think that interacting with the service robot would be pleasant"] <- "HEM3"
names(mydata)[names(mydata) == "I think that interacting with the service robot would be enjoyable"] <- "HEM4"
names(mydata)[names(mydata) == "Service robots might be profitable to the owner of the restaurant"] <- "EXM1"
names(mydata)[names(mydata) == "Service robots will be profitable to the society as a whole"] <- "EXM2"
names(mydata)[names(mydata) == "Service robots can contribute to sustainable development"] <- "EXM3"
names(mydata)[names(mydata) == "Service robots can contribute to the growth of the economy"] <- "EXM4"
names(mydata)[names(mydata) == "I am likely to interact with this kind of service robots in a quick service restaurant"] <- "WIQ1"
names(mydata)[names(mydata) == "I will feel happy to interact with this kind of service robots in a quick service restaurant"] <- "WIQ2"
names(mydata)[names(mydata) == "I am willing to receive services from this kind of service robot in a quick service restaurant"] <- "WIQ3"
names(mydata)[names(mydata) == "I am likely to interact with this kind of service robots in a traditional restaurant"] <- "WIT1"
names(mydata)[names(mydata) == "I will feel happy to interact with this kind of service robots in a traditonal restaurant"] <- "WIT2"
names(mydata)[names(mydata) == "I am willing to receive services from this kind of service robot in a traditional restaurant"] <- "WIT3"
names(mydata)[names(mydata) == "The service robot looks creepy"] <- "UNV1"
names(mydata)[names(mydata) == "The service robot looks uncanny (mysterious)"] <- "UNV2" 
names(mydata)[names(mydata) == "The service robot looks scary"] <- "UNV3"
names(mydata)[names(mydata) == "Given its appearance, the service robot seems to have the consumers' interests at heart"] <- "EMP1"
names(mydata)[names(mydata) == "I think that the service robot would be receptive if I need to ask a special request"] <- "EMP2"
names(mydata)[names(mydata) == "I think that the service robot can adapt its interactions based on individual preferences and emotions"] <- "EMP3"
names(mydata)[names(mydata) == "Overall, I perceive the service robot as empathetic"] <- "EMP4"
names(mydata)[names(mydata) == "I think that the service robot would serve me in the time promised"] <- "REL1"
names(mydata)[names(mydata) == "I think that the service robot would correct quickly anything that is wrong"] <- "REL2"
names(mydata)[names(mydata) == "I think that the service robot will serve you the food exactly as your ordered it"] <- "REL3"
names(mydata)[names(mydata) == "Given its appearance, the service robot seems to be accurate"] <- "PE1"
names(mydata)[names(mydata) == "I think that the service robot won't do mistakes"] <- "PE2"
names(mydata)[names(mydata) == "I think that the service robot is consistent"] <- "PE3"
names(mydata)[names(mydata) == "I think that the service robot could provide me accurate informations"] <- "PE4"
names(mydata)[names(mydata) == "I am likely to interact with this kind of service robots in a high standard restaurant"] <- "WIH1"
names(mydata)[names(mydata) == "I will feel happy to interact with this kind of service robots in a high standard restaurant"] <- "WIH2"
names(mydata)[names(mydata) == "I am willing to receive services from this kind of service robot in a high standard restaurant"] <- "WIH3"
names(mydata)[names(mydata) == "I have studied or worked in the field of AI"] <- "EXP1"
names(mydata)[names(mydata) == "I have had some experience with AI devices in general, such as self-ordering kiosks"] <- "EXP2"
names(mydata)[names(mydata) == "I have had some experience with AI devices such as service robots"] <- "EXP3"
names(mydata)[names(mydata) == "Assigning routine tasks to robots lets people do more meaningful tasks"] <- "INR1"
names(mydata)[names(mydata) == "Dangerous tasks should primarily be given to robots"] <- "INR2"
names(mydata)[names(mydata) == "Robots are a natural product of our civilization"] <- "INR3"
names(mydata)[names(mydata) == "Robots are necessary because they can do jobs that are too hard or too dangerous for people"] <- "INR4"
names(mydata)[names(mydata) == "Robots can make my life easier"] <- "INR5"
names(mydata)[names(mydata) == "Pick a situation with a Service Robot randomly"] <- "Robots"
names(mydata)[names(mydata) == "Personal experience with service robots in a restaurant"] <- "Experience"
names(mydata)[names(mydata) == "What is your personality type"] <- "Personality"

names(mydata)

mydata$Robots[mydata$Robots %in% c("Situation with Robot 1")] <- "Robot 1"
mydata$Robots[mydata$Robots %in% c("Situation with Robot 2")] <- "Robot 2"
mydata$Robots[mydata$Robots %in% c("Situation with Robot 3")] <- "Robot 3"
mydata$Robots[mydata$Robots %in% c("Situation with Robot 4",NA)] <- "Robot 4"


mydata$Nationality[mydata$Nationality %in% c("Dutch", "French", "German", "Lithuanian","lithuanian","Georgian", "italian","Italian","Czech","Latvian","Croatian","Bosnian","Swedish","Franco-Portuguese","Spain","Portuguese","Czech republic")] <- "Other EU"
mydata$Nationality[!mydata$Nationality %in% c("Belgian", "Other EU")] <- "Outside EU"
mydata$Gender <- ifelse(mydata$Gender == "Other", "Woman", mydata$Gender)
mydata$`Social status` <- ifelse(mydata$`Social status` %in% c("Employee", "Student"), mydata$`Social status`, "others")
mydata$Age[mydata$Age >= 16 & mydata$Age <= 25] <- "25 or under"
mydata$Age[mydata$Age >= 26 & mydata$Age <= 80] <- "More than 25"


# Assuming "Nationality" is a factor variable in your 'mydata' dataframe
# If it's not a factor, you can convert it to a factor first using the 'factor()' function.

# Assuming 'Nationality' is a factor variable in your 'mydata' dataframe
# If it's not a factor, you can convert it to a factor first using the 'factor()' function.

# Manually reorder the levels of 'Nationality' to set 'Other EU' as the reference group
mydata$Nationality <- factor(mydata$Nationality, levels = c("Belgian", "Other EU", "Outside EU"))
# Now, run the linear regression again
lm_result <- lm(HEM1 ~ Nationality, data = mydata)
# Print the summary of the regression output
summary(lm_result)

# Manually reorder the levels of 'Nationality' to set 'Other EU' as the reference group
mydata$Gender <- factor(mydata$Gender, levels = c("Woman", "Man"))
# Now, run the linear regression again
lm_result <- lm(HEM1 ~ Gender, data = mydata)
# Print the summary of the regression output
summary(lm_result)

# Manually reorder the levels of 'Nationality' to set 'Other EU' as the reference group
mydata$Personality <- factor(mydata$Personality, levels = c("Extravert", "Introvert"))
# Now, run the linear regression again
lm_result <- lm(HEM1 ~ Personality, data = mydata)
# Print the summary of the regression output
summary(lm_result)

mydata$Age <- factor(mydata$Age, levels = c("More than 25", "25 or under"))
# Now, run the linear regression again
lm_result <- lm(HEM1 ~ Age, data = mydata)
# Print the summary of the regression output
summary(lm_result)


# Calculate the frequencies for each variable
gender_count <- table(mydata$Gender)
nationality_count <- table(mydata$Nationality)
age_count <- table(mydata$Age)
personality_count <- table(mydata$Personality)
social_status_count <- table(mydata$`Social status`)
experience_count <- table(mydata$Experience)

# Calculate the percentages
gender_percentage <- prop.table(gender_count) * 100
nationality_percentage <- prop.table(nationality_count) * 100
age_percentage <- prop.table(age_count) * 100
personality_percentage <- prop.table(personality_count) * 100
social_status_percentage <- prop.table(social_status_count) * 100
experience_percentage <- prop.table(experience_count) * 100

# Print the results
print(gender_percentage)
print(nationality_percentage)
print(age_percentage)
print(personality_percentage)
print(social_status_percentage)
print(experience_percentage)



# Subset for Robot 1 and Robot 2 scenarios
robot_12_data <- mydata[mydata$Robots %in% c("Robot 1", "Robot 2"), ]

# Subset for Robot 3 and Robot 4 scenarios
robot_34_data <- mydata[mydata$Robots %in% c("Robot 3", "Robot 4"), ]


# Calculate the frequencies for each variable
gender_count <- table(robot_12_data$Gender)
nationality_count <- table(robot_12_data$Nationality)
age_count <- table(robot_12_data$Age)
personality_count <- table(robot_12_data$Personality)
social_status_count <- table(robot_12_data$`Social status`)
experience_count <- table(robot_12_data$Experience)

# Calculate the percentages
gender_percentage <- prop.table(gender_count) * 100
nationality_percentage <- prop.table(nationality_count) * 100
age_percentage <- prop.table(age_count) * 100
personality_percentage <- prop.table(personality_count) * 100
social_status_percentage <- prop.table(social_status_count) * 100
experience_percentage <- prop.table(experience_count) * 100

# Print the results
print(gender_percentage)
print(nationality_percentage)
print(age_percentage)
print(personality_percentage)
print(social_status_percentage)
print(experience_percentage)


# Calculate the frequencies for each variable
gender_count <- table(robot_34_data$Gender)
nationality_count <- table(robot_34_data$Nationality)
age_count <- table(robot_34_data$Age)
personality_count <- table(robot_34_data$Personality)
social_status_count <- table(robot_34_data$`Social status`)
experience_count <- table(robot_34_data$Experience)

# Calculate the percentages
gender_percentage <- prop.table(gender_count) * 100
nationality_percentage <- prop.table(nationality_count) * 100
age_percentage <- prop.table(age_count) * 100
personality_percentage <- prop.table(personality_count) * 100
social_status_percentage <- prop.table(social_status_count) * 100
experience_percentage <- prop.table(experience_count) * 100

# Print the results
print(gender_percentage)
print(nationality_percentage)
print(age_percentage)
print(personality_percentage)
print(social_status_percentage)
print(experience_percentage)



library(psych)
library(semPlot)
library(lavaan)
library(semTools)







#ALPHA
alpha_hm <- alpha(mydata[, c("HEM1", "HEM2", "HEM3", "HEM4")]) # Cronbach's alpha for Hedonic Motivation construct
alpha_emp <- alpha(mydata[, c("EMP2","EMP3","EMP4")]) # Cronbach's alpha for Empathy construct
alpha_int <- alpha(mydata[, c("INT1", "INT2","INT3")]) # Cronbach's alpha for Perceived Intelligence construct
alpha_saf <- alpha(mydata[, c("SAF1", "SAF2", "SAF3")]) # Cronbach's alpha for Safety construct
alpha_rel <- alpha(mydata[, c("REL1", "REL2", "REL3")]) # Cronbach's alpha for Reliability construct
alpha_ant <- alpha(mydata[, c("ANT1", "ANT2", "ANT3", "ANT4")]) # Cronbach's alpha for Anthropomorphism construct
alpha_pe_H <- alpha(mydata[, c("PEFH1","PEFH2", "PEFH3")]) # Cronbach's alpha for Performance Expectancy H construct
alpha_wtu_Q <- alpha(mydata[, c("WILQ1", "WILQ2", "WILQ3")]) # Cronbach's alpha for Willingness to Use Q construct
alpha_wtu_T <- alpha(mydata[, c("WILT1", "WILT2", "WILT3")]) # Cronbach's alpha for Willingness to Use T construct
alpha_wtu_H <- alpha(mydata[, c("WILH1", "WILH2", "WILH3")]) # Cronbach's alpha for Willingness to Use H construct
alpha_Trust <- alpha(mydata[, c("INT1","INT2","INT3")]) # Cronbach's alpha for Trust
alpha_soi <- alpha(mydata[, c("SOI1","SOI2", "SOI3")]) # Cronbach's alpha for Willingness to Use H construct
mydata$SOC1 <- as.numeric(mydata$SOC1)
mydata$SOC2 <- as.numeric(mydata$SOC2)
mydata$SOC3 <- as.numeric(mydata$SOC3)
mydata$SOC4 <- as.numeric(mydata$SOC4)
mydata$SOC5 <- as.numeric(mydata$SOC5)
alpha_soc <- alpha(na.omit(mydata[, c("SOC1", "SOC2", "SOC3", "SOC4", "SOC5")]))
alpha_eco <- alpha(mydata[, c("ECO1", "ECO2", "ECO3", "ECO4")])

 # Cronbach's alpha for Willingness to Use H construct




# Print Cronbach's alpha results
print(alpha_hm)     # Cronbach's alpha for Hedonic Motivation construct
print(alpha_int)    # Cronbach's alpha for Perceived Intelligence construct
print(alpha_saf)    # Cronbach's alpha for Safety construct
print(alpha_ant)    # Cronbach's alpha for Reliability construct
print(alpha_pe_H)   # Cronbach's alpha for Performance Expectancy construct
print(alpha_wtu_Q)  # Cronbach's alpha for Willingness to Use construct
print(alpha_wtu_T)  # Cronbach's alpha for Willingness to Use construct
print(alpha_wtu_H)  # Cronbach's alpha for Willingness to Use construct
print(alpha_soi)    # Cronbach's alpha for Social Influence
print(alpha_soc)    
print(alpha_eco)




measurement_model <- "
  Social =~ SOI1 + SOI2 + SOI3
  Anthropomorphism =~ ANT1 + ANT2 + ANT3 + ANT4
  Intelligence =~ INT1 + INT2 + INT3
  Performance_Expectancy_H =~ PEF1 + PEF2 + PEF3
  Safety =~ SAF1 + SAF2 + SAF3
  Hedonic_Motivation =~ HEM1 + HEM2 + HEM3 + HEM4
  Economy =~ EXM2 + EXM3 + ECO4
  Willingness_to_Use_Q =~ WIQ1 + WIQ2 + WIQ3
  Willingness_to_Use_T =~ WIT1 + WIT2 + WIT3
  "

# Estimate the model
fit <- cfa(measurement_model, data = mydata)
# View the modified summary output
summary(fit, standardized = TRUE, fit.measures = TRUE)


compRelSEM(fit)


# Extract factor loadings and residual variances

#Matrix of dimension [1:30, 1:9]
factor_loadings <- as.matrix(inspect(fit, what = "std")$lambda)
squared_loadings <- factor_loadings^2


# Define item counts for each column
item_counts <- c(3, 4, 3, 3, 3, 4, 3, 3, 3)

# Calculate AVE for each latent variable
ave <- colSums(squared_loadings) / item_counts

# Print the AVE values for each latent variable
ave


# Get the covariance matrix
cov_matrix <- lavInspect(fit, "cov.lv")

# Calculate the square root of AVE values
ave_sqrt <- sqrt(ave)

# Replace the diagonal values with ave_sqrt
diag(cov_matrix) <- ave_sqrt

# Print the updated covariance matrix
cov_matrix


# Get the residual variances
residual_variances <- inspect(fit, what = "std")$theta

# Get the residual variances and extract the diagonal elements
diagonal_elements <- diag(inspect(fit, what = "std")$theta)

# Create a new matrix with 7 terms
new_matrix <- matrix(0, nrow = 30, ncol = 9)

# Assign the diagonal elements for hedonic motivation
new_matrix[1:3, 1] <- diagonal_elements[1:3]
new_matrix[4:7, 2] <- diagonal_elements[4:7]
new_matrix[8:11, 3] <- diagonal_elements[8:11]
new_matrix[12:14, 4] <- diagonal_elements[12:14]
new_matrix[15:17, 5] <- diagonal_elements[15:17]
new_matrix[18:21, 6] <- diagonal_elements[18:21]
new_matrix[22:24, 7] <- diagonal_elements[22:24]
new_matrix[25:27, 8] <- diagonal_elements[25:27]
new_matrix[28:30, 9] <- diagonal_elements[28:30]


# Assign column names
colnames(new_matrix) <- c("Social","Anthropomorphism","Intelligence", "Performance_Expectancy_H", "Safety","Hedonic_Motivation", "Economy", "Willingness_to_Use_Q", "Willingness_to_Use_T")


# Calculate composite reliability
composite_reliability <- colSums(factor_loadings^2) / (colSums(factor_loadings^2) + colSums(new_matrix))

# Print the composite reliability values for each latent variable
composite_reliability


measurement_model <- "
  Social =~ SOI1 + SOI2 + SOI3
  Anthropomorphism =~ ANT1 + ANT2 + ANT3 + ANT4
  Intelligence =~ INT1 + INT2 + INT3
  Performance_Expectancy_H =~ PEF1 + PEF2 + PEF3
  Safety =~ SAF1 + SAF2 + SAF3
  Hedonic_Motivation =~ HEM1 + HEM2 + HEM3 + HEM4
  Economy =~ EXM2 + EXM3 + ECO4
  Willingness_to_Use_Q =~ WIQ1 + WIQ2 + WIQ3
  Willingness_to_Use_T =~ WIT1 + WIT2 + WIT3
  "

structural_model <- "
  Performance_Expectancy_H ~ p1*Social
  Intelligence ~ p2*Anthropomorphism
  Safety ~ p3*Intelligence + p4*Anthropomorphism
  Hedonic_Motivation ~ p5*Performance_Expectancy_H + p6*Safety
  Willingness_to_Use_Q ~ p7*Hedonic_Motivation + Economy
  Willingness_to_Use_T ~ p8*Hedonic_Motivation + Economy

SOIonPEH:= p1*p5 
SOIonWIQ:= p1*p5*p7
SOIonWIT:= p1*p5*p8

ANTonSAF:= p2*p3
ANTonHEM:=p2*p3*p6
ANTonWIQ:= p2*p3*p6*p7
ANTonWIT:=p2*p3*p6*p8

INTonHEM:= p3*p6
INTonWIQ:= p3*p6*p7
INTonWIT:= p3*p6*p8

PEFonWIQ:=p5*p7
PEFonWIT:=p5*p8

SAFonWIQ:=p6*p7
SAFonWIT:=p6*p8"


# Subset for Robot 1 and Robot 2 scenarios
robot_12_data <- mydata[mydata$Robots %in% c("Robot 1", "Robot 2"), ]

# Subset for Robot 3 and Robot 4 scenarios
robot_34_data <- mydata[mydata$Robots %in% c("Robot 3", "Robot 4"), ]

# Fit the SEM model
model <- paste(measurement_model, structural_model, sep = "")
fitsem12 <- sem(model, data = robot_12_data)
summary(fitsem12, fit.measures=TRUE,rsquare=T, standardized = TRUE)

model <- paste(measurement_model, structural_model, sep = "")
fitsem34 <- sem(model, data = robot_34_data)
summary(fitsem34, fit.measures=TRUE,rsquare=T, standardized = TRUE)


# Fit the SEM model
model <- paste(measurement_model, structural_model, sep = "")
fitsem <- sem(model, data = mydata)
summary(fitsem, fit.measures=TRUE, standardized = TRUE)







#DATA WITH MODERATING FACTORS

robot_12_data <- cbind(robot_12_data, model.matrix(~ Nationality - 1, data = robot_12_data))
names(robot_12_data)[names(robot_12_data) == "NationalityOther EU"] <- "OtherEU"
names(robot_12_data)[names(robot_12_data) == "NationalityOutside EU"] <- "OutsideEU"

robot_34_data <- cbind(robot_34_data, model.matrix(~ Nationality - 1, data = robot_34_data))
names(robot_34_data)[names(robot_34_data) == "NationalityOther EU"] <- "OtherEU"
names(robot_34_data)[names(robot_34_data) == "NationalityOutside EU"] <- "OutsideEU"

mydata <- cbind(mydata, model.matrix(~ Nationality - 1, data = mydata))
names(mydata)[names(mydata) == "NationalityOther EU"] <- "OtherEU"
names(mydata)[names(mydata) == "NationalityOutside EU"] <- "OutsideEU"



measurement_model <- "
  Social =~ SOI1 + SOI2 + SOI3
  Intelligence =~ INT1 + INT2 + INT3
  Hedonic_Motivation =~ HEM1 + HEM2 + HEM3 + HEM4
  Safety =~ SAF1 + SAF2 + SAF3
  Anthropomorphism =~ ANT1 + ANT2 + ANT3 + ANT4
  Performance_Expectancy_H =~ PEFH1 + PEFH2 + PEFH3
  Willingness_to_Use_Q =~ WILQ1 + WILQ2 + WILQ3
  Willingness_to_Use_T =~ WILT1 + WILT2 + WILT3
  Economy =~ ECO2 + ECO3 + ECO4
  "
structural_model <- "
  Social ~ p0*NationalityBelgian
  Performance_Expectancy_H ~ p1*Social + Gender + Age + Personality
  Intelligence ~ p2*Anthropomorphism + Gender + Age + Personality
  Safety ~ p3*Intelligence + p4*Anthropomorphism + Gender + Age + Personality
  Hedonic_Motivation ~ p5*Performance_Expectancy_H + p6*Safety + Gender + Age + Personality
  Willingness_to_Use_Q ~ p7*Hedonic_Motivation + Economy + Gender + Age + Personality
  Willingness_to_Use_T ~ p8*Hedonic_Motivation + Economy + Gender + Age + Personality

NATonPEF:= p0*p1
NATonHEM:= p0*p1*p5 
NATonWIQ:= p0*p1*p5*p7
NATonWIT:= p0*p1*p5*p8


SOIonHEM:= p1*p5 
SOIonWIQ:= p1*p5*p7
SOIonWIT:= p1*p5*p8

ANTonSAF:= p2*p3
ANTonHEM:=p2*p3*p6
ANTonWIQ:= p2*p3*p6*p7
ANTonWIT:=p2*p3*p6*p8

INTonHEM:= p3*p6
INTonWIQ:= p3*p6*p7
INTonWIT:= p3*p6*p8

PEFonWIQ:=p5*p7
PEFonWIT:=p5*p8

SAFonWIQ:=p6*p7
SAFonWIT:=p6*p8"

# Fit the SEM model
model <- paste(measurement_model, structural_model, sep = "")
fitsem12 <- sem(model, data = robot_12_data)
summary(fitsem12, fit.measures=TRUE,rsquare=T, standardized = TRUE)

model <- paste(measurement_model, structural_model, sep = "")
fitsem34 <- sem(model, data = robot_34_data)
summary(fitsem34, fit.measures=TRUE,rsquare=T, standardized = TRUE)

model <- paste(measurement_model, structural_model, sep = "")
fitsem <- sem(model, data = mydata)
summary(fitsem, fit.measures=TRUE,rsquare=T, standardized = TRUE)


#Mean for Robot 1-2 and Robot 3-4

mydata$anthropomorphism <- rowMeans(mydata[, c("ANT1", "ANT2", "ANT3", "ANT4")])
mydata$social <- rowMeans(mydata[, c("SOI1", "SOI2", "SOI3")])
mydata$empathy <- rowMeans(mydata[, c("EMP1", "EMP2", "EMP3", "EMP4")])
mydata$hedonic_Motivation <- rowMeans(mydata[, c("HEM1", "HEM2", "HEM3", "HEM4")])
mydata$safety <- rowMeans(mydata[, c("SAF1", "SAF2", "SAF3")])
mydata$performance_Expectancy_H <- rowMeans(mydata[, c("PEF1", "PEF2", "PEF3","PEF4")])
mydata$willingness_to_Use_Q <- rowMeans(mydata[, c("WIQ1", "WIQ2", "WIQ3")])
mydata$willingness_to_Use_T <- rowMeans(mydata[, c("WIT1", "WIT2", "WIT3")])
mydata$reliability <- rowMeans(mydata[, c("REL1", "REL2", "REL3")])
mydata$uncannyvalley <- rowMeans(mydata[, c("UNV1", "UNV2", "UNV3")])
mydata$willingness_to_Use_H <- rowMeans(mydata[, c("WIH1", "WIH2", "WIH3")])
mydata$expertise <- rowMeans(mydata[, c("EXP1", "EXP2", "EXP3")])
mydata$extrinsic_motivation <- rowMeans(mydata[, c("EXM1", "EXM2", "EXM3","ECO4")])
mydata$integration_of_robots <- rowMeans(mydata[, c("INR1", "INR2", "INR3","INR4","INR5")])
mydata$intelligence <- rowMeans(mydata[, c("INT1", "INT2", "INT3","INT4")])
mydata$performance_Expectancy <- rowMeans(mydata[, c("PE1", "PE2", "PE3","PE4")])

mydata$anthropomorphism <- rowMeans(mydata[, c("ANT1", "ANT2", "ANT3", "ANT4")])
mydata$social <- rowMeans(mydata[, c("SOI1", "SOI2", "SOI3")])
mydata$empathy <- rowMeans(mydata[, c("EMP1", "EMP2", "EMP3", "EMP4")])
mydata$hedonic_Motivation <- rowMeans(mydata[, c("HEM1", "HEM2", "HEM3", "HEM4")])
mydata$safety <- rowMeans(mydata[, c("SAF1", "SAF2", "SAF3")])
mydata$performance_Expectancy_H <- rowMeans(mydata[, c("PEF1", "PEF2", "PEF3")])
mydata$willingness_to_Use_Q <- rowMeans(mydata[, c("WIQ1", "WIQ2", "WIQ3")])
mydata$willingness_to_Use_T <- rowMeans(mydata[, c("WIT1", "WIT2", "WIT3")])
mydata$reliability <- rowMeans(mydata[, c("REL1", "REL2", "REL3")])
mydata$uncannyvalley <- rowMeans(mydata[, c("UNV1", "UNV2", "UNV3")])
mydata$willingness_to_Use_H <- rowMeans(mydata[, c("WIH1", "WIH2", "WIH3")])
mydata$expertise <- rowMeans(mydata[, c("EXP1", "EXP2", "EXP3")])
mydata$extrinsic_motivation <- rowMeans(mydata[, c("EXM2", "EXM3","ECO4")])
mydata$integration_of_robots <- rowMeans(mydata[, c("INR1", "INR2", "INR3","INR4","INR5")])
mydata$intelligence <- rowMeans(mydata[, c("INT1", "INT2", "INT3")])
mydata$performance_Expectancy <- rowMeans(mydata[, c("PE1", "PE2", "PE3","PE4")])

# Calculate the mean of each column
means <- colMeans(mydata[, c("anthropomorphism","social", "empathy", "hedonic_Motivation", "safety", 
                             "performance_Expectancy_H", "willingness_to_Use_Q", "willingness_to_Use_T", 
                             "reliability","uncannyvalley","willingness_to_Use_H","expertise",
                             "extrinsic_motivation","integration_of_robots","intelligence",
                             "performance_Expectancy")])

means

# Subset for Robot 1 and Robot 2 scenarios
robot_12_data <- mydata[mydata$Robots %in% c("Robot 1", "Robot 2"), ]

# Subset for Robot 3 and Robot 4 scenarios
robot_34_data <- mydata[mydata$Robots %in% c("Robot 3", "Robot 4"), ]

# Calculate the mean of each column
means <- colMeans(robot_12_data[, c("anthropomorphism","social", "empathy", "hedonic_Motivation", "safety", 
                             "performance_Expectancy_H", "willingness_to_Use_Q", "willingness_to_Use_T", 
                             "reliability","uncannyvalley","willingness_to_Use_H","expertise",
                             "extrinsic_motivation","integration_of_robots","intelligence",
                             "performance_Expectancy")])

means

means <- colMeans(robot_34_data[, c("anthropomorphism","social", "empathy", "hedonic_Motivation", "safety", 
                                    "performance_Expectancy_H", "willingness_to_Use_Q", "willingness_to_Use_T", 
                                    "reliability","uncannyvalley","willingness_to_Use_H","expertise",
                                    "extrinsic_motivation","integration_of_robots","intelligence",
                                    "performance_Expectancy")])

means



#Mean for Robot 1, robot 2, robot 3 and robot 4

# Subset for Robot 1 and Robot 2 scenarios
robot_1_data <- mydata[mydata$Robots %in% c("Robot 1"), ]

# Subset for Robot 3 and Robot 4 scenarios
robot_2_data <- mydata[mydata$Robots %in% c("Robot 2"), ]

# Subset for Robot 1 and Robot 2 scenarios
robot_3_data <- mydata[mydata$Robots %in% c("Robot 3"), ]

# Subset for Robot 3 and Robot 4 scenarios
robot_4_data <- mydata[mydata$Robots %in% c("Robot 4"), ]

# Calculate the mean of each column
means1 <- colMeans(robot_1_data[, c("anthropomorphism","social", "empathy", "hedonic_Motivation", "safety", 
                            "performance_Expectancy_H", "willingness_to_Use_Q", "willingness_to_Use_T", 
                            "reliability","uncannyvalley","willingness_to_Use_H","expertise",
                            "extrinsic_motivation","integration_of_robots","intelligence",
                            "performance_Expectancy")])
means1

means2 <- colMeans(robot_2_data[, c("anthropomorphism","social", "empathy", "hedonic_Motivation", "safety", 
                                   "performance_Expectancy_H", "willingness_to_Use_Q", "willingness_to_Use_T", 
                                   "reliability","uncannyvalley","willingness_to_Use_H","expertise",
                                   "extrinsic_motivation","integration_of_robots","intelligence",
                                   "performance_Expectancy")])
means2

means3 <- colMeans(robot_3_data[, c("anthropomorphism","social", "empathy", "hedonic_Motivation", "safety", 
                                   "performance_Expectancy_H", "willingness_to_Use_Q", "willingness_to_Use_T", 
                                   "reliability","uncannyvalley","willingness_to_Use_H","expertise",
                                   "extrinsic_motivation","integration_of_robots","intelligence",
                                   "performance_Expectancy")])
means3

means4 <- colMeans(robot_4_data[, c("anthropomorphism","social", "empathy", "hedonic_Motivation", "safety", 
                                   "performance_Expectancy_H", "willingness_to_Use_Q", "willingness_to_Use_T", 
                                   "reliability","uncannyvalley","willingness_to_Use_H","expertise",
                                   "extrinsic_motivation","integration_of_robots","intelligence",
                                   "performance_Expectancy")])
means4








# Select columns 20 to 76 from the dataset
selected_columns <- mydata[, 20:76]

# Calculate the means of the selected columns
means_of_selected_columns <- colMeans(selected_columns)

# Print the means
print(means_of_selected_columns)


# Select columns 20 to 76 from the dataset
selected_columns12 <- robot_12_data[, 20:76]

# Calculate the means of the selected columns
means_of_selected_columns12 <- colMeans(selected_columns12)

# Print the means
print(means_of_selected_columns12)

# Calculate the standard deviation of the selected columns
standard_deviations_of_selected_columns12 <- apply(selected_columns12, 2, sd)

# Print the standard deviations
print(standard_deviations_of_selected_columns12)


# Select columns 20 to 76 from the dataset
selected_columns34 <- robot_34_data[, 20:76]

# Calculate the means of the selected columns
means_of_selected_columns34 <- colMeans(selected_columns34)

# Print the means
print(means_of_selected_columns34)

# Calculate the standard deviation of the selected columns
standard_deviations_of_selected_columns34 <- apply(selected_columns34, 2, sd)

# Print the standard deviations
print(standard_deviations_of_selected_columns34)






# Plot the SEM diagram

semPaths(fitsem, style = "lisrel", edge.label.cex = 1.2,
         layout = "tree2", rotation = 2)
# Plot the SEM diagram with data
semPaths(fitsem, style = "lisrel", edge.label.cex = 1.2,
         layout = "tree2", rotation = 2,
         latentVar.labels = TRUE, obsVar.labels = TRUE)
semPaths(fitsem, what ="path", whatLabels ="par", style ="ram", layout ="tree", rotation = 2)

semPaths(fitsem, style = "lisrel", edge.label.cex = 1.2,
         layout = "tree2", rotation = 2)


install.packages(tidySEM)


library(tidySEM)
library(ggplot2)
library(dplyr)


#layout in layout_PolDem.csv file google drive link given in video details
data = read.csv(file.choose())
data
graph_sem(fitsem, layout=layout_PolDem)


graph_sem(model=fitsem)


library(semPlot)

# Adjust the size of the plot
bgparams <- list(
  height = 100,
  width = 4000
)

# Plot the SEM graph with adjusted size
graph_sem(model = fitsem, bgparams = bgparams)


# Call the graph_sem function with your model
graph_sem(model=fitsem)

# Display the graph
plt.show()


get_layout(fitsem)


#Defining the latent variables
latent_variables <- c("Social","Anthropomorphism","Intelligence", "Performance_Expectancy_H", "Safety","Hedonic_Motivation", "Economy", "Willingness_to_Use_Q", "Willingness_to_Use_T")