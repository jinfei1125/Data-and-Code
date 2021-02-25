## On your own: k-means and 2012 pres vote shares

# For this section, you may either stay in the class and work independently or you may log off and work independently. 

# Either way, don't forget that this code you're working on here is due to the appropriate Canvas module prior to 5:00 pm CDT tomorrow. You need only submit a *single* file/script to be considered for credit. Recall, I don't care whether you got things right. I only care that attempts to each question have been made.

# Load data and update the pres vote data for 2012
library(tidyverse)

pres <- read_csv("..\\Data\\2012_DVS.csv") %>% 
  mutate(State = X1,
         DVS = dem_vs) %>% 
  select(-c(X1, dem_vs))

# 1. Fit a k-means algorithm to the presidential vote shares data we used today, initialized at k=2. 

# estimate k clusters
pres.out <- pres %>%
  # nstart: initialize centroid 20 times
  mutate(k2 = kmeans(pres$DVS, 2, nstart = 20)$cluster)


# 2. Plot the cluster assignments for all states, varying color for each cluster. 
# Consider using a histogram as we did for the GMM case.

# plot clusters
pres.out %>%
  gather(K, pred, k2) %>%
  mutate(K = parse_number(K),
         pred = factor(pred)) %>%
  ggplot(aes(pres.out$DVS, pres.out$State, color = pred)) +
  facet_wrap(~ K, labeller = label_both) +
  geom_point() +
  scale_color_brewer(type = "qual", palette = "Dark2") +
  theme(legend.position = "none")

#or histogram
library(ggplot2)
presso <- kmeans(pres$DVS,2, nstart = 20)
pres_k<-pres %>% mutate(.,cluster=presso$cluster)
ggplot(pres_k,aes(x = DVS)) +
  geom_histogram(aes(fill=factor(cluster)), alpha = 0.4,stat="bin",bindwidth=3) +
  xlab("Democratic Vote Shares") +
  ylab("Count of States") + 
  theme_minimal()

# 3. Put a vertical cut point/line at 50. Given that in a normal election, it requires 50+1 shares of the votes to win, the idea here is if the k-means solution is good, then all states clustered together should lie on their respective sides of the line.
pres.out %>%
  gather(K, pred, k2) %>%
  mutate(K = parse_number(K),
         pred = factor(pred)) %>%
  ggplot(aes(pres.out$DVS, pres.out$State, color = pred)) +
  facet_wrap(~ K, labeller = label_both) +
  geom_point() +
  scale_color_brewer(type = "qual", palette = "Dark2") +
  theme(legend.position = "none")+
  geom_vline(xintercept = 50, linetype="solid", 
             color = "black", size=1.2)

# or histogram
ggplot(pres_k,aes(x = DVS)) +
  geom_histogram(aes(fill=factor(cluster)), alpha = 0.4,stat="bin",bindwidth=3) +
  xlab("Democratic Vote Shares") +
  ylab("Count of States") + 
  geom_vline(xintercept = 50, 
             col = amerika_palettes$Republican[3]) + 
  theme_minimal()

# 4. Is any state on the "wrong" side of the line? If so, which one?

#From the figure, we can find that NC (North Carolina) is in the wrong side.