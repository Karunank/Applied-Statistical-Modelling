#Question 1

#install.packages(ggpubr)
getwd()
library(dplyr)
library(ggplot2)
library(ggpubr)
library(MCMCpack)
data <- read.csv('analysis.csv')
which(is.na(data))

data
df1 <- subset(data, select=c("White.Player", "White.ACPL","White.Num.Moves","White.Player_ID"))

df1
names(df1)
dim(df1)
sapply(df1,class)
class(df1$White.ACPL)

df1$White.Player <- factor(data$White.Player)
summary(df1)
head(df1,5)
df2 <- df1[order(df1$"White.ACPL"), ]
df2 
head(df4,10)

#Question 1 a

df3 <- df2[df2$White.Player %in% c("Carlsen, Magnus","Anand, Viswanathan"),]
summary(df3)
head(df3,10)

#standard deviation
df3 %>% summarise_if(is.numeric, sd)

#t test between two groups
t.test(White.ACPL ~ White.Player, data=df3, var.equal = FALSE)


df3_CM <- df2[df2$White.Player %in% c("Carlsen, Magnus"),]
summary(df3_CM)

df3_AV <- df2[df2$White.Player %in% c("Anand, Viswanathan"),]
summary(df3_AV)
head(df3,10)

mean_diff = mean(df3_AV$White.ACPL) - mean(df3_CM$White.ACPL)

mean_diff

#standard deviation
df3 %>% summarise_if(is.numeric, sd)


#comparison performace between two groups

cmp_two_players <- function(y, ind, mu0 = 12, tau0 = 0.02, del0 = 0, gamma0 = 0.01, a0 = 3, b0 = 0.2, maxiter = 5000)
{
  y1 <- y[ind == "Anand, Viswanathan"]
  y2 <- y[ind == "Carlsen, Magnus"]
  
  n1 <- length(y1) 
  n2 <- length(y2)
  
  ##### starting values
  mu <- (mean(y1) + mean(y2)) / 2
  del <- (mean(y1) - mean(y2)) / 2
  
  mat_store <- matrix(0, nrow = maxiter, ncol = 3)
  #####
  
  ##### Gibbs sampler
  an <- a0 + (n1 + n2)/2
  
  for(s in 1 : maxiter) 
  {
    
    ##update tau
    bn <- b0 + 0.5 * (sum((y1 - mu - del) ^ 2) + sum((y2 - mu + del) ^ 2))
    tau <- rgamma(1, an, bn)
    ##
    
    ##update mu
    taun <-  tau0 + tau * (n1 + n2)
    mun <- (tau0 * mu0 + tau * (sum(y1 - del) + sum(y2 + del))) / taun
    mu <- rnorm(1, mun, sqrt(1/taun))
    ##
    
    ##update del
    gamman <-  gamma0 + tau*(n1 + n2)
    deln <- ( del0 * gamma0 + tau * (sum(y1 - mu) - sum(y2 - mu))) / gamman
    del<-rnorm(1, deln, sqrt(1/gamman))
    ##
    
    ## store parameter values
    mat_store[s, ] <- c(mu, del, tau)
  }
  colnames(mat_store) <- c("mu", "del", "tau")
  return(mat_store)
}

fit <- cmp_two_players(df3$White.ACPL, as.factor(df3$White.Player))

plot(as.mcmc(fit))

raftery.diag(as.mcmc(fit))

apply(fit, 2, mean)
apply(fit, 2, sd)

mean(1/sqrt(fit[, 3])) 
sd(1/sqrt(fit[, 3]))

player1_sim <- rnorm(5000, fit[, 1] + fit[, 2], sd = 1/sqrt(fit[, 3]))
player2_sim <- rnorm(5000, fit[, 1] - fit[, 2], sd = 1/sqrt(fit[, 3]))

ggplot(data.frame(y_sim_diff = player1_sim - player2_sim)) + stat_bin(aes(y_sim_diff))

mean(player1_sim > player2_sim)

pdf_plot <- ggplot(data.frame(sim_diff = player1_sim-player2_sim),aes(y1_sim-y2_sim, ..density..))
ggplot(data.frame(player1_sim, y=player2_sim)) + geom_point(aes(player1_sim, player2_sim), alpha = 0.3) + geom_abline(slope = 1, intercept = 0)

df4 <- df3 %>% group_by(White.Player) %>% 
  summarise(White.ACPL = mean(White.ACPL))
summary(df4)
  ggdensity(df4$White.ACPL, fill = "lightgray")
plot(density(df3$White.ACPL))
hist(df4$White.ACPL, 100, col="black")
df4
ggplot(df3) + geom_boxplot(aes(White.Player, White.ACPL, fill = White.Player)) +
  geom_jitter(aes(White.Player, White.ACPL, shape = 'White.ACPL')) +
  scale_fill_manual(values=c("red","green"))

df3

# 1. B 

df5 <- df2 %>% group_by(White.Player) %>% 
        summarise(White.ACPL = mean(White.ACPL))

df5 <-arrange(df5,White.ACPL)

tail(df5,10)
summary(df2)
#standard deviation
df2 %>% summarise_if(is.numeric, sd)


ggplot(df2) +
  geom_boxplot(aes(x = reorder(White.Player, White.ACPL, mean),
                   White.ACPL,
                   fill = reorder(White.Player, White.ACPL, mean)),
               show.legend=FALSE) +
  labs(title = "ACPL mean for every player",
    x = "", y = "White ACPL") +
  theme_gray() +
  theme(axis.text.x = element_text(size = 7,angle = 90, face = "bold", vjust = 0.2, hjust = 0.98))
df2 %>% ggplot(aes(x = reorder(White.Player, White.Player, length)),color='red') + stat_count() +
  labs(#title = "Number of White Players",
    x = "", y = "count") +theme_gray() +
  theme(panel.background = element_rect(fill = "cyan",
                                        colour = "white",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "white"),
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "white"), axis.text.x = element_text(size = 7,angle = 90, face = "bold", vjust = 0.2, hjust = 0.98))
df2 %>% ggplot(aes(White.ACPL),color='cyan') + stat_bin(bins = 10) +
  labs(#title = "ACPL values among different players",
    x = "ACPL",y = "Count") +theme_gray()

ggplot(df2, aes(White.ACPL)) + stat_bin()

ggplot(data.frame(size = tapply(df2$White.ACPL, df2$White.Player, length), 
                  mean_score = tapply(df2$White.ACPL, df2$White.Player, mean)), 
       aes(size, mean_score)) + geom_point()



#Comparing multiple groups using Gibbs sampler
compare_m_gibbs <- function(y, ind, maxiter = 5000)
{
  
  ### weakly informative priors
  a0 <- 3 ; b0 <- 0.1 ## tau_w hyperparameters
  eta0 <-1/2 ; t0 <- 50 ## tau_b hyperparameters
  mu0<-20 ; gamma0 <- 0.005
  ###
  
  ### starting values
  m <- nlevels(ind)
  ybar <- theta <- tapply(y, ind, mean)
  tau_w <- mean(1 / tapply(y, ind, var)) ##within group precision
  mu <- mean(theta)
  tau_b <-var(theta) ##between group precision
  n_m <- tapply(y, ind, length)
  an <- a0 + sum(n_m)/2
  ###
  
  ### setup MCMC
  theta_mat <- matrix(0, nrow=maxiter, ncol=m)
  mat_store <- matrix(0, nrow=maxiter, ncol=3)
  ###
  
  ### MCMC algorithm
  for(s in 1:maxiter) 
  {
    
    # sample new values of the thetas
    for(j in 1:m) 
    {
      taun <- n_m[j] * tau_w + tau_b
      thetan <- (ybar[j] * n_m[j] * tau_w + mu * tau_b) / taun
      theta[j]<-rnorm(1, thetan, 1/sqrt(taun))
    }
    
    #sample new value of tau_w
    ss <- 0
    for(j in 1:m){
      ss <- ss + sum((y[ind == j] - theta[j])^2)
    }
    bn <- b0 + ss/2
    tau_w <- rgamma(1, an, bn)
    
    #sample a new value of mu
    gammam <- m * tau_b + gamma0
    mum <- (mean(theta) * m * tau_b + mu0 * gamma0) / gammam
    mu <- rnorm(1, mum, 1/ sqrt(gammam)) 
    
    # sample a new value of tau_b
    etam <- eta0 + m/2
    tm <- t0 + sum((theta - mu)^2) / 2
    tau_b <- rgamma(1, etam, tm)
    
    #store results
    theta_mat[s,] <- theta
    mat_store[s, ] <- c(mu, tau_w, tau_b)
  }
  colnames(mat_store) <- c("mu", "tau_w", "tau_b")
  return(list(params = mat_store, theta = theta_mat))
}

fit <- compare_m_gibbs(df2$White.ACPL, as.factor(df2$White.Player_ID))

plot(as.mcmc(fit$params))

apply(fit$params, 2, mean)
apply(fit$params, 2, sd)

mean(1/sqrt(fit$params[, 3]))
sd(1/sqrt(fit$params[, 3]))

raftery.diag(as.mcmc(fit$params))
## reformat samples for ggplot
theta_df <- data.frame(samples = as.numeric(fit$theta), 
                       Players = rep(1:ncol(fit$theta), each = nrow(fit$theta))) 

theta_med <- apply(theta_df, 2, mean) ## get basic posterior summary
sort(theta_med, decreasing = TRUE) ## which players did best and worst?

ggplot(theta_df) + geom_boxplot(aes(x = reorder(Players, samples, median), samples, 
                                    fill = reorder(Players, samples, median)), show.legend=FALSE)



theta_hat <- apply(fit$theta, 2, mean)
ggplot(data.frame(size = tapply(df1$White.ACPL, df1$White.Player_ID, length), theta_hat = theta_hat), aes(size, theta_hat)) + geom_point()
names(theta_hat) <- colnames(fit$theta)
head(data.frame(sort(theta_hat, decreasing = TRUE)),7)
theta_quantile_bounds <- apply(fit$theta, 2, quantile, prob = c(0.05, .95))
theta_quantile_bounds

theta_hat_df <- data.frame(theta_hat)
head(theta_hat_df,10)
