# Artesunate vs. Quinine Dara Simulation

Background
Quinine was the main drug used to treat malaria in Africa, South America, and Asia even though rates of mortality in cases of severe malaria remained high. There have been case studies showing treatment with artesunate to be more effective. Artesunate is also safer and easier to administer but there was still a question as to whether it reduced mortality more than quinine. A paper was published in 2006 that confirmed artesunate is more effective than quinine. Quinine was shown to be associated with hypoglycemia (meaning low blood sugar) and less effective against hyperparasitaemia (greater than 10% parasitaemia).

The purpose of this project was to simulate the results from the randomized trial peformed in 2006. I used mortality as the response variable to a variety of different conditions including treatment, hyperparasitaemia and death within two time frames (within and after 48 hours). Hyperparasitaemia or severe malaria causes a higher chance of death so I used it to increase the probabily of mortality.

Creation of data set and code:


```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
set.seed(100)
logistic <- function(t) 1 / (1 + exp(-t))
simMortality<-function(N){
  severe_malaria<-sample(1:100, N, replace= TRUE)
  Hyperparasitaemia_artesunate<- runif(N)<.23*logistic((severe_malaria-20/10))
  Hyperparasitaemia_quinine<- runif(N)<.53*logistic((severe_malaria-20/10))
  Treatment_artesunate<- runif(N)<.15*logistic((severe_malaria-20/10))
  Treatment_quinine<- runif(N)<.22*logistic((severe_malaria-20/10))
  Death_within_48hr_artesunate<- runif(N)<.08*logistic((severe_malaria-20/10))
  Death_within_48hr_quinine<- runif(N)<.10*logistic((severe_malaria-20/10))
  Death_after_48hr_artesunate<- runif(N)<.06*logistic((severe_malaria-20/10))
  Death_after_48hr_quinine<- runif(N)<.12*logistic((severe_malaria-20/10))
  data.frame(severe_malaria,Hyperparasitaemia_artesunate, Hyperparasitaemia_quinine,   Treatment_artesunate, Treatment_quinine, Death_within_48hr_artesunate,               Death_within_48hr_quinine, Death_after_48hr_artesunate, Death_after_48hr_quinine)
}
 
mydata<-simMortality(1000)
summary(mydata)
```

```
##  severe_malaria   Hyperparasitaemia_artesunate Hyperparasitaemia_quinine
##  Min.   :  1.00   Mode :logical                Mode :logical            
##  1st Qu.: 29.00   FALSE:774                    FALSE:484                
##  Median : 52.00   TRUE :226                    TRUE :516                
##  Mean   : 52.31   NA's :0                      NA's :0                  
##  3rd Qu.: 77.00                                                         
##  Max.   :100.00                                                         
##  Treatment_artesunate Treatment_quinine Death_within_48hr_artesunate
##  Mode :logical        Mode :logical     Mode :logical               
##  FALSE:847            FALSE:769         FALSE:924                   
##  TRUE :153            TRUE :231         TRUE :76                    
##  NA's :0              NA's :0           NA's :0                     
##                                                                     
##                                                                     
##  Death_within_48hr_quinine Death_after_48hr_artesunate
##  Mode :logical             Mode :logical              
##  FALSE:907                 FALSE:951                  
##  TRUE :93                  TRUE :49                   
##  NA's :0                   NA's :0                    
##                                                       
##                                                       
##  Death_after_48hr_quinine
##  Mode :logical           
##  FALSE:886               
##  TRUE :114               
##  NA's :0                 
##                          
## 
```

Here's a pair plot.


```r
plot(mydata)
```

![](My_First_Markdown_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

Here's a regression model of ozone on some predictors.


```r
fit<- lm(severe_malaria~ Hyperparasitaemia_artesunate + Hyperparasitaemia_quinine + Treatment_artesunate + Treatment_quinine + Death_after_48hr_artesunate +Death_after_48hr_quinine, data = mydata)
summary(fit)
```

```
## 
## Call:
## lm(formula = severe_malaria ~ Hyperparasitaemia_artesunate + 
##     Hyperparasitaemia_quinine + Treatment_artesunate + Treatment_quinine + 
##     Death_after_48hr_artesunate + Death_after_48hr_quinine, data = mydata)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -52.69 -23.01   0.13  25.10  52.87 
## 
## Coefficients:
##                                  Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                       50.0082     1.5743  31.765   <2e-16 ***
## Hyperparasitaemia_artesunateTRUE  -2.8793     2.1734  -1.325   0.1855    
## Hyperparasitaemia_quinineTRUE      3.3521     1.8175   1.844   0.0654 .  
## Treatment_artesunateTRUE           0.4984     2.5258   0.197   0.8436    
## Treatment_quinineTRUE              1.6034     2.1582   0.743   0.4577    
## Death_after_48hr_artesunateTRUE    5.6913     4.2155   1.350   0.1773    
## Death_after_48hr_quinineTRUE       4.3276     2.8576   1.514   0.1302    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 28.68 on 993 degrees of freedom
## Multiple R-squared:  0.01002,	Adjusted R-squared:  0.00404 
## F-statistic: 1.675 on 6 and 993 DF,  p-value: 0.1238
```



