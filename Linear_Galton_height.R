##**2. Case study: is height hereditary?**
library(HistData)
data("GaltonFamilies")

str(GaltonFamilies)

set.seed(1983)
galton_heights <- GaltonFamilies %>%
  filter(gender == "male") %>%
  group_by(family) %>%
  sample_n(1) %>%
  ungroup() %>%
  select(father, childHeight) %>%
  rename(son = childHeight)
summary(galton_heights)

galton_heights %>%
  summarize(mean(father), sd(father),
            mean(son), sd(son)) %>% mutate_if(is.numeric, round, 1)

fit <- lm(son ~ father,
          data = galton_heights)
a <- as.character(round(fit$coefficients[2],2))
b <- as.character(round(fit$coefficients[1],2))
r2 <- as.character(round(summary(fit)$r.squared,2))
eq <- as.character(paste("y = ",a,"x + ",b,", R-squared = ",r2))
eq

galton_heights %>%
  ggplot(aes(father, son)) +
  geom_point(alpha = 0.5,
             color = "orange")+
  geom_smooth(method = "lm", color = "black") +
  labs(title ="father and son regression",
       subtitle = eq) +
  theme_classic()

rss <- function(beta0, beta1, data){
  resid <- galton_heights$son - (beta0+beta1*galton_heights$father)
  return(sum(resid^2))
}

beta1 = seq(0,1, len=nrow(galton_heights))
results <- data.frame(beta1 = beta1,
                      rss = sapply(beta1, rss, beta0 = 25))
results %>% ggplot(aes(beta1, rss)) + geom_line() + 
  geom_line(aes(beta1, rss), col=2)
which.min(results$rss)



###**2.1 Correlation coefficient**
#The sample correlation coefficient r is a statistic that measures the degree of linear relationship between two sets of numbers and is computed as:
  r = Sxy / sqrt(Sxx.Sxy)

galton_heights %>% summarize(r = cor(father, son)) %>% pull(r)

#Sample correlation is a random variable
R <- sample_n(galton_heights, 25, replace = TRUE) %>%
  summarize(r = cor(father, son))
R


Monte Carlo simulation
B <- 1000
N <- 25
R <- replicate(B, {
  sample_n(galton_heights, N, replace = TRUE) %>% 
    summarize(r=cor(father, son)) %>% 
    pull(r)
})
qplot(R, geom = "histogram", binwidth = 0.05, color = I("black"))


round(mean(R),3)
round(sd(R),3)

data.frame(R) %>%
  ggplot(aes(sample = R)) +
  stat_qq() +
  geom_abline(intercept = mean(R),
              slope = sqrt((1 - mean(R)^2)/(N-2)))+
  theme_classic()


