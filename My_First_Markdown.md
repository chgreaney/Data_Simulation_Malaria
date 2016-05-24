# Artesunate vs. Quinine Dara Simulation

###Background
Quinine was the main drug used to treat malaria in Africa, South America, and Asia even though rates of mortality in cases of severe malaria remained high. There have been case studies showing treatment with artesunate to be more effective. Artesunate is also safer and easier to administer but there was still a question as to whether it reduced mortality more than quinine. A paper was published in 2006 that confirmed artesunate is more effective than quinine. Quinine was shown to be associated with hypoglycemia (meaning low blood sugar) and less effective against hyperparasitaemia (greater than 10% parasitaemia).

The purpose of this project was to simulate the results from the randomized trial peformed in 2006. I used mortality as the response variable to a variety of different conditions including treatment, hyperparasitaemia and death within two time frames (within and after 48 hours). Hyperparasitaemia or severe malaria causes a higher chance of death so I used it to increase the probabily of mortality.

###Creation of data set and code:


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
  severe_malaria<-sample(0:100, N, replace= TRUE)
  Hyperparasitaemia_artesunate<- runif(N)<.23*logistic((severe_malaria-20/10))
  Hyperparasitaemia_quinine<- runif(N)<.53*logistic((severe_malaria-40/10))
  Treatment_artesunate<- runif(N)<.15*logistic((severe_malaria-20/10))
  Treatment_quinine<- runif(N)<.22*logistic((severe_malaria-40/10))
  Death_within_48hr_artesunate<- runif(N)<.08*logistic((severe_malaria-20/10))
  Death_within_48hr_quinine<- runif(N)<.10*logistic((severe_malaria-40/10))
  Death_after_48hr_artesunate<- runif(N)<.06*logistic((severe_malaria-20/10))
  Death_after_48hr_quinine<- runif(N)<.12*logistic((severe_malaria-40/10))
  data.frame(severe_malaria,Hyperparasitaemia_artesunate, Hyperparasitaemia_quinine,   Treatment_artesunate, Treatment_quinine, Death_within_48hr_artesunate,               Death_within_48hr_quinine, Death_after_48hr_artesunate, Death_after_48hr_quinine)
}
 
mydata<-simMortality(1000)
summary(mydata)
```

```
##  severe_malaria   Hyperparasitaemia_artesunate Hyperparasitaemia_quinine
##  Min.   :  0.00   Mode :logical                Mode :logical            
##  1st Qu.: 28.00   FALSE:777                    FALSE:498                
##  Median : 52.00   TRUE :223                    TRUE :502                
##  Mean   : 51.85   NA's :0                      NA's :0                  
##  3rd Qu.: 77.00                                                         
##  Max.   :100.00                                                         
##  Treatment_artesunate Treatment_quinine Death_within_48hr_artesunate
##  Mode :logical        Mode :logical     Mode :logical               
##  FALSE:848            FALSE:776         FALSE:924                   
##  TRUE :152            TRUE :224         TRUE :76                    
##  NA's :0              NA's :0           NA's :0                     
##                                                                     
##                                                                     
##  Death_within_48hr_quinine Death_after_48hr_artesunate
##  Mode :logical             Mode :logical              
##  FALSE:909                 FALSE:951                  
##  TRUE :91                  TRUE :49                   
##  NA's :0                   NA's :0                    
##                                                       
##                                                       
##  Death_after_48hr_quinine
##  Mode :logical           
##  FALSE:889               
##  TRUE :111               
##  NA's :0                 
##                          
## 
```

###Plots


```r
plot(mydata)
```

![](My_First_Markdown_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
plot(mydata$Treatment_artesunate,mydata$Treatment_quinine,xlab= "Artesunate", ylab= "Quinine", main="Artesunate VS Quinine")
```

![](My_First_Markdown_files/figure-html/unnamed-chunk-2-2.png)<!-- -->

The plots with showed that artesunate treatment had fewer mortality in all the conditions compared to quinine.
However my plots are hard to understand because there are only two data points, either 1 or 0. 1 indicate death while 0 indicates alive.

Logistic and linear Regression of mortality based on treatment and severe malaria.


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
##     Min      1Q  Median      3Q     Max 
## -55.716 -23.547   0.464  25.284  54.648 
## 
## Coefficients:
##                                  Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                       47.5364     1.5470  30.728  < 2e-16 ***
## Hyperparasitaemia_artesunateTRUE  -2.1842     2.1928  -0.996  0.31946    
## Hyperparasitaemia_quinineTRUE      5.9429     1.8249   3.257  0.00117 ** 
## Treatment_artesunateTRUE           0.8638     2.5423   0.340  0.73411    
## Treatment_quinineTRUE              3.4807     2.1915   1.588  0.11254    
## Death_after_48hr_artesunateTRUE    5.4989     4.2329   1.299  0.19421    
## Death_after_48hr_quinineTRUE       5.7561     2.9025   1.983  0.04763 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 28.8 on 993 degrees of freedom
## Multiple R-squared:  0.02046,	Adjusted R-squared:  0.01454 
## F-statistic: 3.457 on 6 and 993 DF,  p-value: 0.002191
```

```r
fit2<- glm(Treatment_artesunate~Treatment_quinine, family = binomial(), mydata)
summary(fit2)
```

```
## 
## Call:
## glm(formula = Treatment_artesunate ~ Treatment_quinine, family = binomial(), 
##     data = mydata)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -0.5829  -0.5717  -0.5717  -0.5717   1.9452  
## 
## Coefficients:
##                       Estimate Std. Error z value Pr(>|z|)    
## (Intercept)           -1.72855    0.10032 -17.230   <2e-16 ***
## Treatment_quinineTRUE  0.04215    0.20959   0.201    0.841    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 852.33  on 999  degrees of freedom
## Residual deviance: 852.29  on 998  degrees of freedom
## AIC: 856.29
## 
## Number of Fisher Scoring iterations: 4
```


These are not very good models because the R squared value is so low. I realised that my response and predictor variables did not really match up. I wanted the patients with severe malaria to have more death in the patients that recieve quinine that artesunate. 


###Conclusions
In the end, I made a crude mock up of results for patients that are either treated with quinine or artesunate. The mortality rate was affected by whether the paitents had severe malaria. Patients treated with artesunate had a better survival rate than patients treated with quinine. 
