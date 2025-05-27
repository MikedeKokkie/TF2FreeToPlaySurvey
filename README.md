# TF2FreeToPlaySurvey
An analysis of Shounic's Free to Play Survey Results
Video:   https://www.youtube.com/watch?v=ATzcWmuPfsA
Dataset: https://pastebin.com/raw/TDHhrQKZ
Author:  Michael de Kok
Date:    27-05-2025

## Install & Load Packages

``` r
#install.packages(c("psych","corrplot", "rpart", "rpart.plot")) # Uncomment to Install
library(psych)
```

    ## Warning: package 'psych' was built under R version 4.4.3

``` r
library(corrplot)
```

    ## Warning: package 'corrplot' was built under R version 4.4.3

    ## corrplot 0.95 loaded

``` r
library(rpart)
library(rpart.plot)
```

    ## Warning: package 'rpart.plot' was built under R version 4.4.3

## Input Data

``` r
data <- read.csv(file = "TDHhrQKZ.txt")
```

## Wrangle Data

``` r
oldcolnames    <- c("F2P","TextVoice","Age", "OtherAccounts", "MoneySpent", "VPNUsed", "SteamGuard", "Phone", "Email", "CasualLevel", "CasualRank", "Hours")
newcolnames    <- c("F2P","Text","Voice", "Age", "OtherAccounts", "MoneySpent", "VPNUsed", "SteamGuard", "Phone", "Email", "CasualLevel", "Hours")
colnames(data) <- oldcolnames
dataclean      <- as.data.frame(matrix(data = NA, nrow = dim(data)[1], ncol = dim(data)[2],  dimnames = list(rownames(data), newcolnames)))
dataclean$F2P   <- grepl(x = data$F2P, pattern = "Yes")
dataclean$Text  <- grepl(x= data$TextVoice, pattern = "can text")
dataclean$Voice <- grepl(x= data$TextVoice, pattern = "can use voice")
dataclean$Age   <- sub(x = data$Age,      pattern = "(year|years) old",         replacement = "")
dataclean$Age   <- sub(x = dataclean$Age, pattern = "(less than|less than a)", replacement = 0)
dataclean$Age   <- as.numeric(dataclean$Age)
dataclean$OtherAccounts <- sub(x = data$OtherAccounts,      pattern = "( other steam account| other steam accounts)", replacement = "")
dataclean$OtherAccounts <- sub(x = dataclean$OtherAccounts, pattern = "none, i only have 1 steam account", replacement = 0)
dataclean$OtherAccounts <- sub(x = dataclean$OtherAccounts, pattern = "more than 5", replacement = 6)
dataclean$OtherAccounts <- as.numeric(dataclean$OtherAccounts)
dataclean$MoneySpent <-  grepl(x = data$MoneySpent, pattern = "Yes")
dataclean$VPNUsed    <-  grepl(x = data$VPNUsed,    pattern = "Yes")
dataclean$SteamGuard <-  grepl(x = data$SteamGuard, pattern = "Yes")
dataclean$Phone <-  grepl(x = data$Phone, pattern = "Yes")
dataclean$Email <-  grepl(x = data$Email, pattern = "Yes")
dataclean$CasualLevel <-  round(data$CasualLevel + ((data$CasualRank-1)*150))
dataclean$Hours <- data$Hours
```

## Filter out obviously false answers

``` r
hours_since_tf2_launched <- 154536
liars       <- dataclean[dataclean$CasualLevel  < 0 | dataclean$CasualLevel >  1200 | dataclean$Hours <  0 | dataclean$Hours > hours_since_tf2_launched,]
datacleaner <- dataclean[dataclean$CasualLevel >= 0 & dataclean$CasualLevel <= 1200 & dataclean$Hours >= 0 & dataclean$Hours < hours_since_tf2_launched,]
datacleaner <- datacleaner[,c(2,3,1,4:12)]
cat("Submissions kept:", dim(datacleaner)[1], "\nSubmissions filtered out as they contains demonstratably false info:", dim(liars)[1])
```

    ## Submissions kept: 3556 
    ## Submissions filtered out as they contains demonstratably false info: 31

## Do an Initial set of Single-Parameter Correlations

``` r
cordata <- cor(datacleaner, method = "spearman")
pvalues <- corr.test(datacleaner, method = "spearman")$p
corrplot(corr = cordata, 
         method = "ellipse", 
         type = "lower",
         p.mat = pvalues, 
         insig = "label_sig", 
         sig.level = c(1e-100, 1e-50, 1e-10), 
         pch.cex = 1, 
         pch.col = "red")
```

![](F2P_Analysis_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

Based on this, I decided to drop the “Other Steam Accounts” and “VPN
Used” Parameters, as they don’t seem to be the slightest bit related.
The Following Decision Tree is adapted from the following protocol:
<https://www.statology.org/classification-and-regression-trees-in-r/>

## Set Parameters as Factors

``` r
datacleaner$F2P <- as.factor(datacleaner$F2P)
datacleaner$Text <- as.factor(datacleaner$Text)
datacleaner$Voice <- as.factor(datacleaner$Voice)
datacleaner$MoneySpent <- as.factor(datacleaner$MoneySpent)
datacleaner$SteamGuard <- as.factor(datacleaner$SteamGuard)
datacleaner$Phone <- as.factor(datacleaner$Phone)
datacleaner$Email <- as.factor(datacleaner$Email)
```

## Build the Decision Tree

``` r
# Build the Initial Tree
tree <- rpart(Text ~ F2P + MoneySpent + SteamGuard + Phone + Email + CasualLevel + Hours, data=datacleaner, control=rpart.control(cp=.0001))

# View Results
printcp(tree)
```

    ## 
    ## Classification tree:
    ## rpart(formula = Text ~ F2P + MoneySpent + SteamGuard + Phone + 
    ##     Email + CasualLevel + Hours, data = datacleaner, control = rpart.control(cp = 1e-04))
    ## 
    ## Variables actually used in tree construction:
    ## [1] CasualLevel F2P         Hours       MoneySpent  Phone       SteamGuard 
    ## 
    ## Root node error: 1041/3556 = 0.29274
    ## 
    ## n= 3556 
    ## 
    ##           CP nsplit rel error  xerror     xstd
    ## 1 0.59942363      0   1.00000 1.00000 0.026065
    ## 2 0.03602305      1   0.40058 0.40058 0.018430
    ## 3 0.01825168      3   0.32853 0.32853 0.016889
    ## 4 0.00528338      4   0.31028 0.31700 0.016621
    ## 5 0.00144092      6   0.29971 0.31028 0.016462
    ## 6 0.00096061     11   0.29203 0.32277 0.016756
    ## 7 0.00024015     14   0.28915 0.34102 0.017172
    ## 8 0.00013723     25   0.28626 0.34774 0.017322
    ## 9 0.00010000     32   0.28530 0.34966 0.017364

``` r
# Identify best cp value to use
best <- tree$cptable[which.min(tree$cptable[,"xerror"]),"CP"]

# Produce a Pruned Tree based on the best cp value
pruned_tree <- prune(tree, cp=best)

# Plot the Pruned Tree
prp(pruned_tree,type = 0, faclen=0, extra=1, roundint=T, digits=5, under = TRUE, fallen.leaves = F, yesno = 2) 
```

![](F2P_Analysis_files/figure-gfm/decisiontree-1.png)<!-- -->

After a few attempts this was the most “not too simple yet elegant”
result I got, so I exported this image as svg and prettied it up in
powerpoint.
