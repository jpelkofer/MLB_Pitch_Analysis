ibrary(tidyverse)
library(forcats)
library(caret)

###combinre first and last names, join datasets
player_names <- mutate(player_names, player = str_c(first_name, last_name, sep = " "))
atbats <- rename(atbats, id = pitcher_id)
atbats <- left_join(atbats, player_names, by = "id")
pitches <- left_join(pitches, atbats, by = "ab_id")
colnames(pitches)

###filter pitches by Max Scherzer and situation variables
scherzer <- filter(pitches, player == "Max Scherzer")

scherzer <- select(scherzer, b_count, b_score,
                   on_1b, on_2b, on_3b, outs, pitch_num,
                   pitch_type, p_score, s_count, inning)

###character varaibles to factor variables
scherzer$pitch_type <- as.factor(scherzer$pitch_type)
scherzer$on_1b <- as.factor(scherzer$on_1b)
scherzer$on_2b <- as.factor(scherzer$on_2b)
scherzer$on_3b <- as.factor(scherzer$on_3b)

#create new variable runDiff, win/lose/tie variable
scherzer <- mutate(scherzer, runDiff = p_score - b_score)
scherzer<- mutate(scherzer, scenario = ifelse(runDiff>0, 'winning', ifelse(runDiff==0, 'tie', 'losing')))
scherzer$scenario <- as.factor(scherzer$scenario)
summary(scherzer)

###classify pitch types as fastball or off_speed, remove other pitch types
scherzer$pitch_type <- fct_collapse(scherzer$pitch_type, fastball = 'FF')
scherzer$pitch_type <- fct_collapse(scherzer$pitch_type, off_speed = c('SL', 'CH', 'CU'))
scherzer <- filter(scherzer, pitch_type == 'fastball' | pitch_type == 'off_speed')
summary(scherzer)

###define an 80%/20% train/test split of the dataset, possibly creating a model later
set.seed(133)
trainIndex <- createDataPartition(scherzer$pitch_type, p=.80, list = FALSE)
scherzerTrain <- scherzer[trainIndex,]
scherzerTest <- scherzer[-trainIndex,]

###Distribution of Pitch Type
ggplot(scherzerTrain, aes(pitch_type)) +
  geom_bar(fill = '#AB0003', aes(y = (..count..)/sum(..count..))) +
  labs(title = 'Distribution of Pitch Type by Count (Balls vs. Strikes)', x = 'Pitch Type', y = '% Thrown') +
  theme(plot.title = element_text(color = '#AB0003')) +
  theme(axis.title = element_text(color = '#AB0003')) +
  theme(axis.text = element_text(color = '#14225A')) +
  theme(panel.background = element_rect(fill = '#14225A'))

###Distribution of Pitch Type by Count
ggplot(scherzerTrain, aes(pitch_type)) +
  geom_bar(fill = '#AB0003', aes(y = (..count..)/tapply(..count..,..PANEL..,sum)[..PANEL..])) +
  labs(title = 'Distribution of Pitch Type by Count (Balls vs. Strikes)', x = 'Pitch Type', y = '% Thrown') +
  theme(plot.title = element_text(color = '#AB0003')) +
  theme(axis.title = element_text(color = '#AB0003')) +
  theme(axis.text = element_text(color = '#14225A')) +
  theme(panel.background = element_rect(fill = '#14225A')) +
  facet_grid(s_count~b_count)

###Distribution of Pitch Type by scenario
ggplot(scherzerTrain, aes(pitch_type)) +
  geom_bar(fill = '#14225A', aes(y = (..count..)/tapply(..count..,..PANEL..,sum)[..PANEL..])) +
  labs(title = 'Distribution of Pitch Type when Losing/Tied/Winning', x = 'Pitch Type', y = '% Thrown') +
  theme(plot.title = element_text(color = '#14225A')) +
  theme(panel.background = element_rect(fill = '#AB0003')) +
  facet_wrap(~scenario, ncol = 3)

###Distribution of Pitch Type by Runner Position
ggplot(filter(scherzerTrain, on_2b == 'True' | on_3b == 'True'), aes(pitch_type)) +
  geom_bar(fill = '#14225A', aes(y=(..count..)/sum(..count..))) +
  labs(title = 'Distribution of Pitch Type w/ RISP', x = 'Pitch Type', y = '% Thrown') +
  theme(plot.title = element_text(color = '#14225A')) +
  theme(panel.background = element_rect(fill = '#AB0003')) +
  theme(axis.text = element_text(color = '#AB0003')) +
  theme(axis.title = element_text(color = '#14225A'))