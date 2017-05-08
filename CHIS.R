# CHIS.R - CHIS Case Study from GGPLOT2 Part 2 Class (Cut and Pasted from R Console in Data Camp)
# Note - Had issues with Session Timing Out in Data Camp but not in R. 
# Also had warning messages from Data Camp regarding server issues.
# Added lines in this file to separate activities.


# Explore the dataset with summary and str

> summary(adult)
RBMI           BMI_P          RACEHPR2         SRSEX      
Min.   :1.000   Min.   :12.65   Min.   :1.000   Min.   :1.000  
1st Qu.:2.000   1st Qu.:22.77   1st Qu.:5.000   1st Qu.:1.000  
Median :3.000   Median :25.72   Median :6.000   Median :2.000  
Mean   :2.748   Mean   :26.64   Mean   :5.088   Mean   :1.591  
3rd Qu.:3.000   3rd Qu.:29.32   3rd Qu.:6.000   3rd Qu.:2.000  
Max.   :4.000   Max.   :93.72   Max.   :6.000   Max.   :2.000  
SRAGE_P          MARIT2           AB1            ASTCUR     
Min.   :18.00   Min.   :1.000   Min.   :1.000   Min.   :1.000  
1st Qu.:44.00   1st Qu.:1.000   1st Qu.:2.000   1st Qu.:2.000  
Median :57.00   Median :1.000   Median :2.000   Median :2.000  
Mean   :56.14   Mean   :2.043   Mean   :2.525   Mean   :1.915  
3rd Qu.:69.00   3rd Qu.:3.000   3rd Qu.:3.000   3rd Qu.:2.000  
Max.   :85.00   Max.   :4.000   Max.   :5.000   Max.   :2.000  
AB51             POVLL      
Min.   :-1.0000   Min.   :1.000  
1st Qu.:-1.0000   1st Qu.:2.000  
Median :-1.0000   Median :4.000  
Mean   :-0.7108   Mean   :3.196  
3rd Qu.:-1.0000   3rd Qu.:4.000  
Max.   : 3.0000   Max.   :4.000

> str(adult)

'data.frame':	44346 obs. of  10 variables:
  $ RBMI    : num  3 3 3 2 3 4 3 2 3 3 ...
$ BMI_P   : num  28.9 26.1 25.1 25 25.1 ...
$ RACEHPR2: num  6 6 6 6 6 6 6 6 6 6 ...
$ SRSEX   : num  1 2 1 1 1 2 1 2 1 2 ...
$ SRAGE_P : num  32 80 71 39 75 53 42 33 67 52 ...
$ MARIT2  : num  1 3 1 4 1 1 1 1 3 3 ...
$ AB1     : num  1 1 2 1 2 3 2 2 1 5 ...
$ ASTCUR  : num  2 2 1 2 2 1 2 2 2 2 ...
$ AB51    : num  -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 ...
$ POVLL   : num  4 4 4 4 4 4 4 3 4 4 ...

  > # Age histogram
  
  > ggplot(adult, aes(x=SRAGE_P)) +geom_histogram()
`stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

  
  > # BMI histogram
  
  > ggplot(adult, aes(x=BMI_P)) + geom_histogram()
`stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
> 
  
  > # Age colored by BMI, default binwidth
  
  > ggplot(adult, aes(x=SRAGE_P, y=factor(RBMI),fill=factor(RBMI))) + geom_histogram(binwidth=1)
Error: stat_bin() must not be used with a y aesthetic.
> 
  # Remove individual aboves 84
  
  > adult <- adult[adult$SRAGE_P <= 84, ]
> 
  > # Remove individuals with a BMI below 16 and above or equal to 52
  
  > adult <- adult[adult$BMI_P >= 16 & adult$BMI_P < 52, ]
> 
  > # Relabel the race variable:
  
  > adult$RACEHPR2 <- factor(adult$RACEHPR2, labels = c("Latino", "Asian", "African American", "White"))
> 
  > # Relabel the BMI categories variable:
  
  > adult$RBMI <- factor(adult$RBMI, labels = c("Under-weight", "Normal-weight", "Over-weight", "Obese"))

# The dataset adult is available
> 
  > # The color scale used in the plot
  
  > BMI_fill <- scale_fill_brewer("BMI Category", palette = "Reds")
> 
  > # Theme to fix category display in faceted plot
  
  > fix_strips <- theme(strip.text.y = element_text(angle = 0, hjust = 0, vjust = 0.1, size = 14),
                        strip.background = element_blank(), 
                        legend.position = "none")
> 
  > # Histogram, add BMI_fill and customizations
  
  > ggplot(adult, aes (x = SRAGE_P, fill= factor(RBMI))) + 
  geom_histogram(binwidth = 1) +
  fix_strips + BMI_fill + theme_classic()

# Plot 1 - Count histogram

> ggplot(adult, aes (x = SRAGE_P, fill= factor(RBMI))) + 
  geom_histogram(binwidth = 1) +
  BMI_fill
> 
  > 
  > # Plot 2 - Density histogram
 
   > ggplot(adult, aes (x = SRAGE_P, y=..density.., fill= factor(RBMI))) + 
  geom_histogram(binwidth = 1, aes(y=..density..)) +
  BMI_fill
> 
 
  > # Plot 3 - Faceted count histogram
  
  > ggplot(adult, aes (x = SRAGE_P, fill= factor(RBMI))) + 
  geom_histogram(binwidth = 1) + facet_grid(RBMI ~.) +
  BMI_fill
> 
  > 
  > 
  > # Plot 4 - Faceted density histogram
  
  > ggplot(adult, aes (x = SRAGE_P, fill= factor(RBMI))) + 
  geom_histogram(binwidth = 1, aes(y=..density..)) + facet_grid(RBMI ~.) +
  BMI_fill
> 
  
  > # Plot 5 - Density histogram with position = "fill"

  > ggplot(adult, aes (x = SRAGE_P, fill= factor(RBMI))) + 
  geom_histogram(binwidth = 1, position="fill",aes(y=..density..)) +
  BMI_fill
> 
 
  > # Plot 6 - The accurate histogram
  
  > ggplot(adult, aes (x = SRAGE_P, fill= factor(RBMI))) + 
  geom_histogram(binwidth = 1, position="fill",aes(y=..count../sum(..count..))) +
  BMI_fill
> 
  # An attempt to facet the accurate frequency histogram from before (failed)
  
  > ggplot(adult, aes (x = SRAGE_P, fill= factor(RBMI))) + 
  geom_histogram(aes(y = ..count../sum(..count..)), binwidth = 1, position = "fill") +
  BMI_fill +
  facet_grid(RBMI ~ .)
> 
  > # Create DF with table()
  
  > DF <- table(adult$RBMI, adult$SRAGE_P)
> 
  > # Use apply on DF to get frequency of each group
  
  > DF_freq <- apply(DF, 2, function(x) x/sum(x))
> 
  > # Load reshape2 and use melt on DF to create DF_melted
  
  > library(reshape2)
> DF_melted <- melt(DF_freq)
> 
  > # Change names of DF_melted
  
  > names(DF_melted) <- c("FILL", "X", "value")
> 
  > # Add code to make this a faceted plot
  
  > ggplot(DF_melted, aes(x = X, y = value, fill = FILL)) +
  geom_bar(stat = "identity", position = "stack") +
  BMI_fill + 
  facet_grid(FILL ~ .)
> 
  
  # The initial contingency table
 
   > DF <- as.data.frame.matrix(table(adult$SRAGE_P, adult$RBMI))
> 
  > # Add the columns groupsSum, xmax and xmin. Remove groupSum again.
  
  > DF$groupSum <- rowSums(DF)
> DF$xmax <- cumsum(DF$groupSum)
> DF$xmin <- DF$xmax - DF$groupSum
> 
  # The groupSum column needs to be removed, don't remove this line
  
  > DF$groupSum <- NULL
> 
  > # Copy row names to variable X
  
  > DF$X <- row.names(DF)
> 
  > # Melt the dataset
  
  > library(reshape2)
> DF_melted <- melt(DF,variable.name = "FILL",id.vars=c("X","xmin","xmax"))
> 
  > # dplyr call to calculate ymin and ymax - don't change
  
  > library(dplyr)

# Attaching package: 'dplyr'
# The following objects are masked from 'package:stats':
  
#  filter, lag
# The following objects are masked from 'package:base':intersect, setdiff, setequal, union

> DF_melted <- DF_melted %>% 
  group_by(X) %>% 
  mutate(ymax = cumsum(value/sum(value)),
         ymin = ymax - value/sum(value))
> 
  > # Plot rectangles - don't change.
 
   > library(ggthemes)
> ggplot(DF_melted, aes(ymin = ymin, 
                        ymax = ymax,
                        xmin = xmin, 
                        xmax = xmax, 
                        fill = FILL)) + 
  geom_rect(colour = "white") +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  BMI_fill +
  theme_tufte()

# Perform chi.sq test (RBMI and SRAGE_P)

> results <- chisq.test(table(adult$RBMI, adult$SRAGE_P))
> 
  > # Melt results$residuals and store as resid
 
   > resid <- melt(results$residuals)
> 
  > # Change names of resid
 
   > names(resid) <- c("FILL", "X", "residual")
> 
  > # merge the two datasets:
 
   > DF_all <- merge(DF_melted,resid)
> 
  > # Update plot command
 
   > library(ggthemes)
> ggplot(DF_all, aes(ymin = ymin, 
                     ymax = ymax,
                     xmin = xmin, 
                     xmax = xmax, 
                     fill = residual)) + 
  geom_rect() +
  scale_fill_gradient2() +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  theme_tufte()

# Position for labels on x axis

> DF_all$xtext <- DF_all$xmin + (DF_all$xmax - DF_all$xmin)/2
> 
  > # Position for labels on y axis (don't change)

    > index <- DF_all$xmax == max(DF_all$xmax)
> DF_all$ytext <- DF_all$ymin[index] + (DF_all$ymax[index] - DF_all$ymin[index])/2
> 
  > # Plot

    > ggplot(DF_all, aes(ymin = ymin, ymax = ymax, xmin = xmin, 
                       xmax = xmax, fill = residual)) + 
  geom_rect(col = "white") +

    # geom_text for ages (i.e. the x axis)

    geom_text(aes(x = xtext, 
                label = X),
            y = 1,
            size = 3,
            angle = 90,
            hjust = 1,
            show.legend = FALSE) +
 
   # geom_text for BMI (i.e. the fill axis)
  
  geom_text(aes(x = max(xmax), 
                y = ytext,
                label = FILL),
            size = 3,
            hjust = 1,
            show.legend  = FALSE) +
  scale_fill_gradient2() +
  theme_tufte() +
  theme(legend.position = "bottom")
> 
  # Load all packages
  
  > library(ggplot2)
> library(reshape2)
> library(dplyr)

#Attaching package: 'dplyr'
#The following objects are masked from 'package:stats': filter, lag
#The following objects are masked from 'package:base':intersect, setdiff, setequal, union

> library(ggthemes)
> 
  > # Script generalized into a function
 
   > mosaicGG <- mosaicGG(adult,"SRAGE_P", "RBMI")
Error: could not find function "mosaicGG"
> 
  >   # Proportions in raw data
  
  >   DF <- as.data.frame.matrix(table(data[[X]], data[[FILL]]))
Error: object 'X' not found
>   DF$groupSum <- rowSums(DF)
Error: object 'DF' not found
>   DF$xmax <- cumsum(DF$groupSum)
Error: object 'DF' not found
>   DF$xmin <- DF$xmax - DF$groupSum
Error: object 'DF' not found
>   DF$X <- row.names(DF)
Error: object 'DF' not found
>   DF$groupSum <- NULL
Error: object 'DF' not found
>   DF_melted <- melt(DF, id = c("X", "xmin", "xmax"), variable.name = "FILL")
Error: object 'DF' not found

>   library(dplyr)

>   DF_melted <- DF_melted %>% 
  group_by(X) %>% 
  mutate(ymax = cumsum(value/sum(value)),
         ymin = ymax - value/sum(value))
Error: object 'DF_melted' not found
> 
  >   # Chi-sq test
 
   >   results <- chisq.test(table(data[[FILL]], data[[X]])) # fill and then x
Error: object 'FILL' not found
>   resid <- melt(results$residuals)
Error: object 'results' not found
>   names(resid) <- c("FILL", "X", "residual")
Error: names() applied to a non-vector
> 
  >   # Merge data
 
   >   DF_all <- merge(DF_melted, resid)
Error: object 'DF_melted' not found
> 
  >   # Positions for labels
 
   >   DF_all$xtext <- DF_all$xmin + (DF_all$xmax - DF_all$xmin)/2
Error: object 'DF_all' not found
>   index <- DF_all$xmax == max(DF_all$xmax)
Error: object 'DF_all' not found
>   DF_all$ytext <- DF_all$ymin[index] + (DF_all$ymax[index] - DF_all$ymin[index])/2
Error: object 'DF_all' not found
> 
  >   # plot:
  
  >   g <- ggplot(DF_all, aes(ymin = ymin,  ymax = ymax, xmin = xmin, 
                              xmax = xmax, fill = residual)) + 
  geom_rect(col = "white") +
  geom_text(aes(x = xtext, label = X),
            y = 1, size = 3, angle = 90, hjust = 1, show.legend = FALSE) +
  geom_text(aes(x = max(xmax),  y = ytext, label = FILL),
            size = 3, hjust = 1, show.legend = FALSE) +
  scale_fill_gradient2("Residuals") +
  scale_x_continuous("Individuals", expand = c(0,0)) +
  scale_y_continuous("Proportion", expand = c(0,0)) +
  theme_tufte() +
  theme(legend.position = "bottom")
Error: object 'DF_all' not found
>   print(g)
Error: object 'g' not found
> 
  > 
  > # BMI described by age
  
  > mosaicGG <- mosaicGG(adult,"SRAGE_P", "RBMI")
Error: could not find function "mosaicGG"
> 
  > # Poverty described by age
  
  > mosaicGG <- mosaicGG(adult,"SRAGE_P", "POVLL")
Error: could not find function "mosaicGG"
> 
  > # mtcars: am described by cyl
 
   > mosaicGG <- mosaicGG(mtcars,"cyl", "am")
Error: could not find function "mosaicGG"
> 
  > # Vocab: vocabulary described by education
  > library(car)

#Attaching package: 'car'
#The following object is masked from 'package:dplyr': recode

> mosaicGG <- mosaicGG(Vocab,"education", "vocabulary")
Error: could not find function "mosaicGG"
> 
  # Load all packages
 
   > library(ggplot2)
> library(reshape2)
> library(dplyr)

#Attaching package: 'dplyr'
#The following objects are masked from 'package:stats': filter, lag
#The following objects are masked from 'package:base':intersect, setdiff, setequal, union

> library(ggthemes)
> 
  > # Script generalized into a function
 
   > mosaicGG <- mosaicGG(adult,"SRAGE_P", "RBMI")
Error: could not find function "mosaicGG"
> 
  >   # Proportions in raw data
  
  >   DF <- as.data.frame.matrix(table(data[[X]], data[[FILL]]))
Error: object 'X' not found
>   DF$groupSum <- rowSums(DF)
Error: object 'DF' not found
>   DF$xmax <- cumsum(DF$groupSum)
Error: object 'DF' not found
>   DF$xmin <- DF$xmax - DF$groupSum
Error: object 'DF' not found
>   DF$X <- row.names(DF)
Error: object 'DF' not found
>   DF$groupSum <- NULL
Error: object 'DF' not found
>   DF_melted <- melt(DF, id = c("X", "xmin", "xmax"), variable.name = "FILL")
Error: object 'DF' not found

>   library(dplyr)
>   DF_melted <- DF_melted %>% 
  group_by(X) %>% 
  mutate(ymax = cumsum(value/sum(value)),
         ymin = ymax - value/sum(value))
Error: object 'DF_melted' not found
> 
  >   # Chi-sq test
  
  >   results <- chisq.test(table(data[[FILL]], data[[X]])) # fill and then x
Error: object 'FILL' not found
>   resid <- melt(results$residuals)
Error: object 'results' not found
>   names(resid) <- c("FILL", "X", "residual")
Error: names() applied to a non-vector
> 
  >   # Merge data
  
  >   DF_all <- merge(DF_melted, resid)
Error: object 'DF_melted' not found
> 
  >   # Positions for labels
  
  >   DF_all$xtext <- DF_all$xmin + (DF_all$xmax - DF_all$xmin)/2
Error: object 'DF_all' not found
>   index <- DF_all$xmax == max(DF_all$xmax)
Error: object 'DF_all' not found
>   DF_all$ytext <- DF_all$ymin[index] + (DF_all$ymax[index] - DF_all$ymin[index])/2
Error: object 'DF_all' not found
> 
  >   # plot:
  
  >   g <- ggplot(DF_all, aes(ymin = ymin,  ymax = ymax, xmin = xmin, 
                              xmax = xmax, fill = residual)) + 
  geom_rect(col = "white") +
  geom_text(aes(x = xtext, label = X),
            y = 1, size = 3, angle = 90, hjust = 1, show.legend = FALSE) +
  geom_text(aes(x = max(xmax),  y = ytext, label = FILL),
            size = 3, hjust = 1, show.legend = FALSE) +
  scale_fill_gradient2("Residuals") +
  scale_x_continuous("Individuals", expand = c(0,0)) +
  scale_y_continuous("Proportion", expand = c(0,0)) +
  theme_tufte() +
  theme(legend.position = "bottom")
Error: object 'DF_all' not found
>   print(g)
Error: object 'g' not found
> 
  > 
  > # BMI described by age
 
   > mosaicGG <- mosaicGG(adult,"SRAGE_P", "RBMI")
Error: could not find function "mosaicGG"
> 
  > # Poverty described by age
 
   > mosaicGG <- mosaicGG(adult,"SRAGE_P", "POVLL")
Error: could not find function "mosaicGG"
> 
  > # mtcars: am described by cyl
  
  > mosaicGG <- mosaicGG(mtcars,"cyl", "am")
Error: could not find function "mosaicGG"
> 
  > # Vocab: vocabulary described by education
  
  > library(car)

#Attaching package: 'car'
#The following object is masked from 'package:dplyr':recode

> mosaicGG <- mosaicGG(Vocab,"education", "vocabulary")
Error: could not find function "mosaicGG"  
  
  
  