##### Baysian Statistics Project

### Load Packages

library(dplyr)
library(rjags)

### Define functions

logit = function(p) {log(p/(1-p))}

### Read & Filter Data

# http://www.tennis-data.co.uk/alldata.php
origdata=read.csv("wta2022to2023.csv")

head(origdata)

### Explore Dataset

## Unique players
playercount = length(unique(c(origdata$Winner,origdata$Loser)))
playercount
length(unique(c(origdata$Winner)))
length(unique(c(origdata$Loser)))

## One winner that never lost
unique(origdata$Winner)[!unique(origdata$Winner)%in%unique(origdata$Loser)]

## Confirm same number of events as distinct tournaments in 2023
max(origdata[which(origdata$Date>='2023-01-01'),]$WTA)
max(as.numeric(as.factor(origdata[which(origdata$Date>='2023-01-01'),]$Tournament)))

## Observe Comment Frequency
table(origdata$Comment)
# If the model aims to predict "completed" wins vs. player rank, one may consider removing or reducing the weight of retires and/or walkovers

### Data Preparation

## Build ID Vars for Player, Tournament, and Surface

# Player

playertable = data.frame(player_name = unique(origdata$Loser),player_id=as.numeric(as.factor(unique(origdata$Loser))))%>%arrange(player_id)

# Add player who never lost
playertable = rbind(playertable, c(unique(origdata$Winner)[!unique(origdata$Winner)%in%unique(origdata$Loser)],max(playertable$player_id)+1))

head(playertable)

nrow(playertable) == playercount

# Tournament - Won't be included in single-year model

tournamenttable = data.frame(tournament_name = unique(origdata$Tournament),tournament_id=as.numeric(as.factor(unique(origdata$Tournament))))%>%arrange(tournament_id)

head(tournamenttable)

nrow(tournamenttable)

# Surface

surfacetable = data.frame(surface_name = unique(origdata$Surface),surface_id=as.numeric(as.factor(unique(origdata$Surface))))%>%arrange(surface_id)

head(surfacetable)

# Tier

tiertable = data.frame(tier_name = unique(origdata$Tier),tier_id=as.numeric(as.factor(unique(origdata$Tier))))%>%arrange(tier_id)

head(tiertable)

nrow(tiertable)

# Round

roundtable = data.frame(round_name = unique(origdata$Round),round_id=as.numeric(as.factor(unique(origdata$Round))))%>%arrange(round_id)

roundtable

nrow(roundtable)

## Dummy Vars

table(origdata$Court)
# Not enough volume for individual var per player so will be excluded

origdata$Outdoor = ifelse(origdata$Court=="Outdoor",1,0)

## Build Dataset for the Baysian Model

# Join ID tables to original data and select required dimensions

data = origdata %>% left_join(playertable,join_by(Winner == player_name)) %>% rename(player1 = player_id) %>% 
  left_join(playertable,join_by(Loser == player_name)) %>% rename(player2 = player_id) %>%
  left_join(tournamenttable,join_by(Tournament == tournament_name)) %>%
  left_join(surfacetable,join_by(Surface == surface_name)) %>%
  left_join(tiertable,join_by(Tier == tier_name)) %>%
  left_join(roundtable,join_by(Round == round_name)) %>%
  mutate(outcome=1) %>%
  select(outcome,player1,player2,surface_id,tier_id,round_id,Date)

# Use 2022 for prior dataset

data$Date=as.Date(data$Date)

priordata=data[which(data$Date<'2023-01-01'),]

priordata = rbind(priordata,priordata%>%mutate(player1=player2, outcome=outcome-1))%>%select(-c(player2))

table(priordata$outcome)

priordatatable = as.data.frame(priordata%>%group_by(player_id = player1)%>%summarise(priorwinperc = mean(outcome), priorn = n()))

head(priordatatable)

playertable

priordatatable[which(priordatatable$player_id==410),]

# Join in prior data to playertable, and fill NA values with a non-informative prior

priordatatable = as.data.frame(playertable %>% 
                                 left_join(priordatatable) %>% 
                                 mutate(priorwinperc=ifelse(is.na(priorwinperc),0.5,pmax(pmin(priorwinperc,0.99),0.01))) %>%
                                 select(-c(player_name,player_id)))

# , priorn = n(), priorv = var(outcome)
# 
# , priorn=ifelse(is.na(priorn),0.01,priorn), priorv=ifelse(is.na(priorv),1,pmax(0.01,priorv))
# 
# head(priordatatable[which(is.na(priordatatable$priorn)),])

# Find player score variance with 'outliers' that don't have sufficient sub samples

plot(logit(priordatatable$priorwinperc)~ifelse(is.na(priordatatable$priorn),0,priordatatable$priorn))

var(logit(priordatatable$priorwinperc))

nrow(priordatatable)

# Find player score variance without 'outliers' that don't have sufficient sub samples

outliers = priordatatable[which(abs(logit(priordatatable$priorwinperc))>4),]
remaining = priordatatable[which(abs(logit(priordatatable$priorwinperc))<4),]

par(mfrow=c(1,1))
plot(logit(priordatatable$priorwinperc)~ifelse(is.na(priordatatable$priorn),0,priordatatable$priorn))
points(logit(outliers$priorwinperc)~ifelse(is.na(outliers$priorn),0,outliers$priorn), col = "red")

var(logit(priordatatable$priorwinperc))
var(logit(remaining$priorwinperc))
nrow(remaining)

# Filter to 2023 Data

data = data[which(data$Date>='2023-01-01'),]%>%select(-c(Date))

# Observe Win Rate Distribution by games played

data_table = rbind(data,data%>%mutate(player1=player2, outcome=outcome-1))%>%select(-c(player2))

table(data_table$outcome)

data_table = as.data.frame(data_table%>%group_by(player_id = player1)%>%summarise(winperc = mean(outcome), n = n()))

head(data_table)

data_table$winpercjitter=jitter(data_table$winperc,100)
data_table$njitter=jitter(data_table$n,10)

outliers = data_table[which(abs(logit(data_table$winperc))>2),]
remaining = data_table[which(abs(logit(data_table$winperc))<2),]

nrow(data_table)

par(mfrow=c(1,1))
plot(logit(data_table$winperc)~data_table$n,xlab = "Games Played", ylab="Logit of Win Percentage",main = "WTA Player Logit Win Percentage by Games Played in 2023")
points(logit(outliers$winperc)~ifelse(is.na(outliers$n),0,outliers$n), col = "red")

par(mfrow=c(1,1))
plot(data_table$winpercjitter~data_table$njitter,xlab = "Games Played", ylab="Win Rate",main = "Chart 1: WTA Player Win Rate by Games Played in 2023")
points(outliers$winpercjitter~outliers$njitter, col = "red")

outliers[which(outliers$winperc==1),]

# Observe normal fit of win rates

nrow(remaining)

sd(logit(outliers$winperc))
par(mfrow=c(2,2))
hist(logit(remaining$winperc),xlab = "Logit of Win Rate", ylab="Player Count",ylim = c(0,100), main = "Histogram of Players by Logit Win Rate (No Outliers)")
hist(rnorm(n=224,mean=0,sd=sd(logit(remaining$winperc))), xlab = "Logit of Win Rate", ylab="Player Count",ylim = c(0,100), main = "Histogram of 181 Normal Samples with Mean 0 and SD 0.72")
hist(logit(outliers$winpercjitter),xlab = "Logit of Win Rate", ylab="Player Count",ylim = c(0,100), col="red", main = "Histogram of Players by Logit Win Rate (Outliers)")
hist(rnorm(n=104,mean=0,sd=sd(logit(pmax(pmin(outliers$winpercjitter,0.9999),0.0001)))), xlab = "Logit of Win Rate", ylab="Player Count",ylim = c(0,100), col='red', main = "Histogram of 103 Normal Samples with Mean 0 and SD 2.36")

par(mfrow=c(1,1))
hist(rnorm(n=104,mean=0,sd=sd(logit(pmax(pmin(outliers$winpercjitter,0.9999),0.0001)))), xlab = "Logit of Win Rate", ylab="Player Count",ylim = c(0,100), col='red', main = "Chart 2: Distribution Assessment of WTA Player Win Rates in 2023")

# Shuffle player1 and player2 and change outcome, so that player1 isn't always the winner

data_copy = data

str(data_copy)

set.seed(123)

shuffle = runif(nrow(data)) > 0.5
head(shuffle)

data$player2 = as.numeric(ifelse(shuffle==TRUE, data_copy$player1, data_copy$player2))
data$player1 = as.numeric(ifelse(shuffle==TRUE, data_copy$player2, data_copy$player1))
data$outcome = ifelse(shuffle==TRUE, data$outcome - 1, data$outcome)

head(data)

# (Omit for now) Aggregate data common vars to reduce row count

# Build Jags Model

mod_string = " model {
	for (i in 1:length(outcome)) {
		outcome[i] ~ dbin(phi[i], 1)
		logit(phi[i]) = b0 + playerscore[player1[i]] - playerscore[player2[i]] 
		                   + surfaceadjust[player1[i],surface_id[i]] - surfaceadjust[player2[i],surface_id[i]]
		                   + tieradjust[player1[i],tier_id[i]] - tieradjust[player2[i],tier_id[i]] 
		                   + roundadjust[player1[i],round_id[i]] - roundadjust[player2[i],round_id[i]]
	}
	
  playerstd ~ dunif(0,5)
  surfacestd ~ dunif(0,5)
	tierstd ~ dunif(0,5)
	roundstd ~ dunif(0,5)
	
	for (p in 1:max(max(player1),player2)) {
    playerscore[p] ~ dnorm(0, 1/playerstd^2)
    
    for (s in 1:max(surface_id)) {
    surfaceadjust[p,s] ~ dnorm(0, 1/surfacestd^2)
    }
    
    for (t in 1:max(tier_id)) {
    tieradjust[p,t] ~ dnorm(0, 1/tierstd^2)
    }
    
    for (r in 1:max(round_id)) {
    roundadjust[p,r] ~ dnorm(0, 1/roundstd^2)
    }
    
	}
	
	b0 ~ dnorm(0.0, 1.0/5.0^2)
	
} "

data_jags = as.list(data)

head(data_jags)

params = c("b0", "playerscore", "surfaceadjust", "tieradjust", "roundadjust")

mod = jags.model(textConnection(mod_string), data=data_jags, n.chains=3)
update(mod, 1e3)

mod_sim = coda.samples(model=mod,
                       variable.names=params,
                       n.iter=5e3)
mod_csim = as.mcmc(do.call(rbind, mod_sim))

coef = colMeans(mod_csim)

coef_sd = apply(mod_csim,2,sd)

hist(mod_csim[,length(colMeans(mod_csim))])

## Get player coefficients

pcoef=merge(playertable,data.frame(player_id=seq(1:playercount),playerscore=round(coef[2:(2+playercount-1)],3),playerscore_std=round(coef_sd[2:(2+playercount-1)],3)))%>%arrange(-playerscore)

head(pcoef)

write.csv(pcoef,"modelranks.csv")

hist(pcoef$playerscore_std)

# 
data$player1

pcoef$player_id = as.numeric(pcoef$player_id)

str(pcoef$player_id)

predconfidence=as.data.frame(data %>% left_join (pcoef,by = c("player1" = "player_id")) %>% rename(playerscore1 = playerscore, playerscore_std1 = playerscore_std, player_name1 = player_name) %>%
         left_join (pcoef,by = c("player2" = "player_id")) %>% rename(playerscore2 = playerscore, playerscore_std2 = playerscore_std, player_name2 = player_name))
  
# Find the probability winning players score is greater than loser

predconfidence$winningplayerscore=ifelse(predconfidence$outcome==1,predconfidence$playerscore1,predconfidence$playerscore2)

predconfidence$higherplayerscore=pmax(predconfidence$playerscore1,predconfidence$playerscore2)
predconfidence$lowerplayerscore=pmin(predconfidence$playerscore2,predconfidence$playerscore1)

# https://en.wikipedia.org/wiki/Sum_of_normally_distributed_random_variables
# https://stats.stackexchange.com/questions/325266/distribution-of-normal-variable-subtracted-from-another-normal-random-variable

predconfidence$prhigherplayerscore = 1-pnorm(0,mean = predconfidence$higherplayerscore - predconfidence$lowerplayerscore, sd = sqrt(predconfidence$playerscore_std1^2 + predconfidence$playerscore_std2^2))

head(predconfidence$prhigherplayerscore)

write.csv(predconfidence,"wta_predconf.csv")

head(predconfidence)

cor(predconfidence$higherplayerscore-predconfidence$lowerplayerscore, predconfidence$prhigherplayerscore)

# Get histogram of player score standard deviation

hist(pcoef$playerscore_std)

## convergence diagnostics players 1-3
plot(mod_sim[,2:4])

gelman.diag(mod_sim[,2:4])
autocorr.diag(mod_sim[,2:4])
autocorr.plot(mod_sim[,2:4])
effectiveSize(mod_sim[,2:4])

mean(effectiveSize(mod_sim[,2:playercount]))

playercount

nrow(data)

## compute DIC
dic = dic.samples(mod, n.iter=5e3)
dic

## Get residuals on training set

playerscore1_tcoef=as.vector(coef[2:(2+playercount-1)])[data$player1]
playerscore2_tcoef=as.vector(coef[2:(2+playercount-1)])[data$player2]

surfaceadjust1_tcoef=coef[(2+playercount):(2+playercount*2-1)][data$player1]
surfaceadjust2_tcoef=coef[(2+playercount):(2+playercount*2-1)][data$player2]

tieradjust1_tcoef=coef[(2+playercount*2):(2+playercount*3-1)][data$player1]
tieradjust2_tcoef=coef[(2+playercount*2):(2+playercount*3-1)][data$player2]

coef[(2+playercount*2):(2+playercount*3-1)]

roundadjust1_tcoef=coef[(2+playercount*3):(2+playercount*4-1)][data$player1]
roundadjust2_tcoef=coef[(2+playercount*3):(2+playercount*4-1)][data$player2]

predprobs = 1/(1+exp(-(coef[1] + playerscore1_tcoef - playerscore2_tcoef 
                       + surfaceadjust1_tcoef - surfaceadjust2_tcoef
                       + tieradjust1_tcoef - tieradjust2_tcoef
                       + roundadjust1_tcoef - roundadjust2_tcoef)))

data$predprob = as.numeric(predprobs)

# Probability Residual Analysis

data$residual = data$predprob - data$outcome

hist(data$residual)

mean(abs(data$residual))

# install.packages("pROC")
library(pROC)

roc(data$outcome,data$predprob)

# Accuracy at 50%

data$pred = ifelse(data$predprob>0.5,1,0)

mean(data$pred)
mean(data$outcome)

mean(data$pred == data$outcome)

table(data$outcome,data$pred)

# With only playerscores

predprobs_playeronly = 1/(1+exp(-(coef[1] + playerscore1_tcoef - playerscore2_tcoef)))

roc(data$outcome,predprobs_playeronly)

# Results in the same AUC

## Build heirarchical model with prior for player mean

mod_string_h = " model {

	for (i in 1:length(outcome)) {
		outcome[i] ~ dbin(phi[i], 1)
		logit(phi[i]) = b0 + playerscore[player1[i]] - playerscore[player2[i]]
	}
	
	for (p in 1:length(priorwinperc)) {

    ## Logit of the player win percentage will be used to pull samples of a player's score

	  playerscore[p] ~ dnorm(logit(priorwinperc[p]), 3)

	  u[p] ~ dnorm(logit(priorwinperc[p]), prec[p])
    prec[p] ~ dgamma(0.333/2, 0.333*10/2)
    std[p] = sqrt(1/prec[p])

	}

	surfacestd ~ dunif(0,10)
	tierstd ~ dunif(0,10)
	roundstd ~ dunif(0,10)

	b0 ~ dnorm(0.0, 1.0/5.0^2)

} "

data_jags_h = c(as.list(data),as.list(priordatatable))

head(data_jags_h)

params = c("b0", "playerscore")

mod_h = jags.model(textConnection(mod_string_h), data=data_jags_h, n.chains=3)
update(mod_h, 1e3)

mod_sim_h = coda.samples(model=mod_h,
                       variable.names=params,
                       n.iter=5e3)
mod_csim_h = as.mcmc(do.call(rbind, mod_sim_h))

coef_h = colMeans(mod_csim_h)

coef_sd_h=apply(mod_csim_h,2,sd)

hist(mod_csim_h[,length(colMeans(mod_csim_h))])

## Get player coefficients

pcoef_h=merge(playertable,data.frame(player_id=seq(1:playercount),playerscore=round(coef_h[2:(2+playercount-1)],3),playerscore_std=round(coef_sd_h[2:(2+playercount-1)],3)))%>%arrange(-playerscore)

head(pcoef_h)

write.csv(pcoef_h,"modelranks2.csv")

## convergence diagnostics
plot(mod_sim_h[,2:4])

gelman.diag(mod_sim_h[,2:4])
autocorr.diag(mod_sim_h[,2:4])
autocorr.plot(mod_sim_h[,2:4])
effectiveSize(mod_sim_h[,2:4])

## compute DIC
dic_h = dic.samples(mod_h, n.iter=5e3)
dic_h

## Get residuals on training set

playerscore1_tcoef_h=as.vector(coef_h[2:(2+playercount-1)])[data$player1]

playerscore2_tcoef_h=as.vector(coef_h[2:(2+playercount-1)])[data$player2]

predprobs_h = 1/(1+exp(-(coef_h[1] + playerscore1_tcoef_h - playerscore2_tcoef_h)))

data$predprob_h = as.numeric(predprobs_h)

# Probability Residual Analysis

data$residual_h = data$predprob_h - data$outcome

hist(data$residual_h)

mean(abs(data$residual_h))

# install.packages("pROC")
library(pROC)

roc(data$outcome,data$predprob_h)

# Accuracy at 50%

data$pred_h = ifelse(data$predprob_h>0.5,1,0)

mean(data$pred_h)
mean(data$outcome)

mean(data$pred_h == data$outcome)

table(data$outcome,data$pred_h)

