#Describe in a sentence or two (R comment) what the phsample.RData dataset is about.
#     The phsample.RData dataset contains information about people's demographic information.
#     This includes age, income, employment class, education level, sex of worker, occupation,
#     level of education, and other variables. Both of these datasets have household as well
#     as personal information. There are 2982 observations and 210 variables in the 'dhus' dataset and
#     6279 observations with 288 variables in the 'dpus' dataset.



#2.12~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Obtains a desired subset of the data.
#PINCP: Predicted income in US dollars
#ESR: 
#PERNP:
#WKHP: 
#AGEP: Age of person
#PWGTP: 
#COW: Employment Class
#SCHL: 
#This subset includes data values between $1000-$250000 in predicted income and
#people between 20-50 years of age. There are other restrictions that determine
#the data subset but I am unsure of what the variable names mean (such as ESR, etc.)

psub = subset(dpus,with(dpus,(PINCP>1000)&(ESR==1)&
                          (PINCP<=250000)&(PERNP>1000)&(PERNP<=250000)&
                          (WKHP>=40)&(AGEP>=20)&(AGEP<=50)&
                          (PWGTP1>0)&(COW %in% (1:7))&(SCHL %in% (1:24))))



#2.13~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Recode some of the variables for readability, such as recoding the enumerated integers
#into meaningful names. This prevents accidentally treating such values are only a 
#numeric value.

#Redefines the sex variable so that 1 corresponds to Male or 'M' and 2 relates to Female or 'F'
psub$SEX = as.factor(ifelse(psub$SEX==1,'M','F'))

#Change reference sex to M so that F shows a difference from M in the models produced
psub$SEX = relevel(psub$SEX,'M')

#Make the class of worker (COW) variable into a more readable form.
cowmap <- c("Employee of a private for-profit",
            "Private not-for-profit employee",
            "Local government employee",
            "State government employee",
            "Federal government employee",
            "Self-employed not incorporated",
            "Self-employed incorporated")
psub$COW = as.factor(cowmap[psub$COW])
psub$COW = relevel(psub$COW,cowmap[1])

#Make the education info. more readable and make fewer levels.
#All levels below high school will be merged into a single encoding.
schlmap = c(
  rep("no high school diploma",15),
  "Regular high school diploma",
  "GED or alternative credential",
  "some college credit, no degree",
  "some college credit, no degree",
  "Associate's degree",
  "Bachelor's degree",
  "Master's degree",
  "Professional degree",
  "Doctorate degree")
psub$SCHL = as.factor(schlmap[psub$SCHL])
psub$SCHL = relevel(psub$SCHL,schlmap[1])

#subset of data rows used for model training
dtrain = subset(psub,ORIGRANDGROUP >= 500)
#subset of data rows used for model testing
dtest = subset(psub,ORIGRANDGROUP < 500)


#2.14~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Print a summary of the COW variable from the dtrain dataframe, which is a subset of the psub
#dataframe. Variables are printed out for the classes of workers in the dataframe.
#Examples of printed variables include "employee for a private for-profit", "federal government
#employee", "local government employee", etc.
summary(dtrain$COW)







setwd("C:\\Users\\cnr\\Documents\\School\\Senior Fall 2014\\CMDA 3654 (Data Analytics and Visual)\\R scripts\\CMDA\\cmda")

#Read in the csv dataframe of the Men's Wimbleton results
wim_men <- read.table(
  'Wimbledon-men-2013.csv',
  sep=',',
  header=T
)


#The current names are too vague
names(wim_men)


#Attribute Information:

#Player 1 Name of Player 1 
#Player 2 Name of Player 2 
#Result Result of the match (0/1) - Referenced on Player 1 is Result = 1 if Player 1 wins (FNL.1>FNL.2) 
#FSP.1 First Serve Percentage for player 1 (Real Number) 
#FSW.1 First Serve Won by player 1 (Real Number) 
#SSP.1 Second Serve Percentage for player 1 (Real Number) 
# SSW.1 Second Serve Won by player 1 (Real Number) 
# ACE.1 Aces won by player 1 (Numeric-Integer) 
# DBF.1 Double Faults committed by player 1 (Numeric-Integer) 
# WNR.1 Winners earned by player 1 (Numeric) 
# UFE.1 Unforced Errors committed by player 1 (Numeric) 
# BPC.1 Break Points Created by player 1 (Numeric) 
# BPW.1 Break Points Won by player 1 (Numeric) 
# NPA.1 Net Points Attempted by player 1 (Numeric) 
# NPW.1 Net Points Won by player 1 (Numeric) 
# TPW.1 Total Points Won by player 1 (Numeric) 
# ST1.1 Set 1 result for Player 1 (Numeric-Integer) 
# ST2.1 Set 2 Result for Player 1 (Numeric-Integer) 
# ST3.1 Set 3 Result for Player 1 (Numeric-Integer) 
# ST4.1 Set 4 Result for Player 1 (Numeric-Integer) 
# ST5.1 Set 5 Result for Player 1 (Numeric-Integer) 
# FNL.1 Final Number of Games Won by Player 1 (Numeric-Integer) 
# FSP.2 First Serve Percentage for player 2 (Real Number) 
# FSW.2 First Serve Won by player 2 (Real Number) 
# SSP.2 Second Serve Percentage for player 2 (Real Number) 
# SSW.2 Second Serve Won by player 2 (Real Number) 
# ACE.2 Aces won by player 2 (Numeric-Integer) 
# DBF.2 Double Faults committed by player 2 (Numeric-Integer) 
# WNR.2 Winners earned by player 2 (Numeric) 
# UFE.2 Unforced Errors committed by player 2 (Numeric) 
# BPC.2 Break Points Created by player 2 (Numeric) 
# BPW.2 Break Points Won by player 2 (Numeric) 
# NPA.2 Net Points Attempted by player 2 (Numeric) 
# NPW.2 Net Points Won by player 2 (Numeric) 
# TPW.2 Total Points Won by player 2 (Numeric) 
# ST1.2 Set 1 result for Player 2 (Numeric-Integer) 
# ST2.2 Set 2 Result for Player 2 (Numeric-Integer) 
# ST3.2 Set 3 Result for Player 2 (Numeric-Integer) 
# ST4.2 Set 4 Result for Player 2 (Numeric-Integer) 
# ST5.2 Set 5 Result for Player 2 (Numeric-Integer) 
# FNL.2 Final Number of Games Won by Player 2 (Numeric-Integer) 
# Round Round of the tournament at which game is played (Numeric-Integer) 


#rename variables so that they are less ambiguous
names(wim_men) = c('Player 1 Name',
                   'Player 2 Name',
                   'Round Number',
                   'Winner of Match',
                   'Final No. of Games Won By Player 1',
                   'Final No. of Games Won By Player 2',
                   'First serve Percentage for Player 1',
                   'First Serve Won for Player 1',
                   'Second Serve Percentage for Player 1',
                   'Second Serve Won for Player 1',
                   'Aces Won for Player 1',
                   'Double Faults Committed By Player 1',
                   'Winners Earned for Player 1',
                   'Unforced Errors Committed by Player 1',
                   'Break Points Created by Player 1',
                   'Break Points Won by Player 1',
                   'Net Points Attempted by Player 1',
                   'Net Points Won by Player 1',
                   'Total Points Won by Player 1',
                   'Set 1 Result for Player 1',
                   'Set 2 Result for Player 1',
                   'Set 3 Result for Player 1',
                   'Set 4 Result for Player 1',
                   'Set 5 Result for Player 1',
                   'First serve Percentage for Player 2',
                   'First Serve Won for Player 2',
                   'Second Serve Percentage for Player 2',
                   'Second Serve Won for Player 2',
                   'Aces Won for Player 2',
                   'Double Faults Committed By Player 2',
                   'Winners Earned for Player 2',
                   'Unforced Errors Committed by Player 2',
                   'Break Points Created by Player 2',
                   'Break Points Won by Player 2',
                   'Net Points Attempted by Player 2',
                   'Net Points Won by Player 2',
                   'Total Points Won by Player 2',
                   'Set 1 Result for Player 2',
                   'Set 2 Result for Player 2',
                   'Set 3 Result for Player 2',
                   'Set 4 Result for Player 2',
                   'Set 5 Result for Player 2')

#Updated values of the column names
names(wim_men)

#Save the dataframe as a data file
save(wim_men, file ="tennis.RData")
