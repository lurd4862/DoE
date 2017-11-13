## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
if(!require(daewr)){
  install.packages("daewr")
}
if(!require(tidyverse)){
  install.packages("tidyverse")
}
if(!require(MASS)){
  install.packages("MASS")
}
if(!require(agricolae)){
  install.packages("agricolae")
}
if(!require(multcomp)){
  install.packages("multcomp")
}
  library(daewr)
  library(tidyverse)
  library(MASS)
  library(agricolae)
  library(multcomp)

## ------------------------------------------------------------------------
set.seed(234789)
f <- factor(rep(c(35,40,45),each = 4))
randomized_times <- sample(f,12)
allocation <- 1:12
design <- data.frame(allocated_loaf = allocation, time = randomized_times)
design

## ------------------------------------------------------------------------
bread_example <- daewr::bread

bread_example %>% lm(formula = height~time) %>% summary

## ------------------------------------------------------------------------
bread_example %>% aov(formula = height~time) %>% summary

## ------------------------------------------------------------------------
Validate_assumptions <- function(df, formula_ = NULL, model, factor, response) {
  qq_plot <- 
    df %>% 
    ggplot(aes(sample = model %>% residuals))+
    stat_qq()+
    ggtitle("QQ plot of residuals")+
    xlab("")+
    ylab("")
  
  Res_fac <- df %>% 
    ggplot()+
    geom_point(aes(y = model %>% residuals(), x = df[,factor] ))+
    ggtitle("Residuals VS factor levels") +
    xlab("Residuals")+
    ylab("Factor levels")

  Res_fit <- df %>% 
    ggplot(aes(y = model %>% residuals(), x = model %>% predict))+
    geom_point()+
    ggtitle("Residuals VS fitted")+
    xlab("Residuals")+
    ylab("fitted")
  
  Res_eu <- df %>% 
    ggplot(aes(y = model %>% residuals(), x = seq(nrow(df))))+
    geom_point()+
    ggtitle("Residuals VS experimental unit")+
    xlab("Residuals")+
    ylab("Experimental unit")
  
  multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }

 if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
  
  multiplot(qq_plot, Res_fac, Res_fit, Res_eu, cols=2)
  
}

Validate_assumptions(df = bread_example,model =  bread_example %>% lm(formula = height~time), factor = "time") 

# qqnorm(y = bread_example %>% lm(formula = height~time) %>% residuals)

## ------------------------------------------------------------------------
box_cox <- boxcox(aov(bread_example,formula = height~time))
lambda = box_cox$x[which(box_cox$y == box_cox$y %>% max)]
lambda

## ------------------------------------------------------------------------
bread_example_transformed <- 
  bread_example %>% 
  mutate(height = height^lambda)

## ------------------------------------------------------------------------
qqnorm(y = bread_example_transformed %>% lm(formula = height~time) %>% residuals())

## ------------------------------------------------------------------------
bread_example_transformed %>% aov(formula = height~time) %>% summary

## ------------------------------------------------------------------------
bread_example_lognormal <- 
  bread_example %>% 
  mutate(height = log(height))

## ------------------------------------------------------------------------
qqnorm(y = bread_example_lognormal %>% lm(formula = height~time) %>% residuals())

## ------------------------------------------------------------------------
bread_example_lognormal %>% aov(formula = height~time) %>% summary

## ------------------------------------------------------------------------

bread_example_Weighted <- 
    bread_example %>% 
  group_by(time) %>% 
  mutate(sd_treatment = 1/sd(height))
bread_example_Weighted

## ------------------------------------------------------------------------
bread_example_Weighted %>% lm(formula = height~time,weights = bread_example_Weighted$sd_treatment) %>% summary

## ------------------------------------------------------------------------
weighted_ols_anova <-  function(df, formula_, factor, response) {
  factor_enq <- dplyr::enquo(factor)
  response_enq <- dplyr::enquo(response)
  # factor_quo <- dplyr::quo(factor)
  # response_quo <- dplyr::quo(response)
  df <- 
    df %>% 
  group_by(!!factor_enq) %>%
  mutate(sd_treatment = 1/sd(!!response_enq)) %>% 
    ungroup
  
   # y = df %>% select(!!response_enq) %>% flatten_dbl
   # x = df %>% select(!!factor_enq) %>% flatten_dbl
  
    df %>% lm(formula =  formula_, weights = sd_treatment) %>% anova
    # df %>% lm(y = y, x = x, weights = sd_treatment) %>% anova

  # return(df)
}

weighted_ols_anova(bread,height~time,time,height)

## ------------------------------------------------------------------------
mod_full <- MASS::polr(data = daewr::teach,
                   formula = score ~ method,
                   weight = count)

mod_reduced <- MASS::polr(data = daewr::teach,
                   formula = score ~ 1,
                   weight = count)

anova(mod_full,mod_reduced)


## ------------------------------------------------------------------------
# Predicted <- 
#   daewr::teach %>% 
#   mutate(predicted_score = mod_full %>% predict)

# Predicted %>% 
#   group_by(method) %>% 
#   nest %>% 
#   mutate(Histogram = data %>% map(~hist(.$score))) %>% 
#   .Histogram

daewr::teach %>% 
  ggplot()+
  geom_bar(aes(x = score,y = count),stat = "identity")+
  facet_wrap(~method)+
  ggtitle("Distribution of scores by method",subtitle = "Method 3 appears to outperform")

daewr::teach %>% 
  mutate_if(is.factor,as.numeric) %>% 
  group_by(method) %>% 
  summarise(mean_score = sum(score*count)/sum(count),
            Total_votes = sum(count))

## ----eval=FALSE----------------------------------------------------------
## # Validate_assumptions(df = daewr::teach, model = mod_full)

## ------------------------------------------------------------------------
daewr::bread %>% lm(formula = height~time) %>% residuals() %>% var

## ------------------------------------------------------------------------
power <- Fpower1(alpha = rep(0.05, 6-2+1),
                 nlev = 3,
                 nreps = 2:6, 
                 Delta = 3,
                 sigma = sqrt(1.917614)
                )
power

## ------------------------------------------------------------------------
Sugerbeet_anova <- aov(yield ~ treat, data = daewr::sugarbeet)

Sugerbeet_anova

Sugerbeet_TukeyHSD <- TukeyHSD(Sugerbeet_anova, ordered = TRUE)

Sugerbeet_TukeyHSD

## ------------------------------------------------------------------------
Sugarbeet_SNK <- SNK.test(Sugerbeet_anova, "treat", alpha = 0.05)

Sugerbeet_anova

## ------------------------------------------------------------------------
Sugarbeet_glht <- glht(Sugerbeet_anova,linfct = mcp(treat = "Dunnett"), alternative = "greater")

Sugarbeet_glht %>% summary

