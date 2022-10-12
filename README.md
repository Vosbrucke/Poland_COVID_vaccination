# Analysis of fully vaccinated population share on COVID-19 across Poland by county

## Project objectives

The project aims to discuss and evaluate the impact of the factors on the rate of fully vaccinated population. The project objective is to answer the question whether voting preferences had an impact on the willingness to vaccinate and whether there is a correlation between the share of the population vaccinated with two doses and the number of deaths.

## Univariate analysis

Before moving into the comparison of the two variables let's do exploratory analysis for both of those factors separate. From the first plot we can draw a few conclusions. The first is that values of vaccination rates are much closer to each other than the voting for PiS commission during 2019 Sejm and Senat elections. The mean for the vaccination rate is equal to 54.7% while the standard deviation is only at 7 percent points. For elections the mean is 48% and the standard deviation is at 11.7 percent points. The coefficient of variability is equal to 12.9% for vaccination rates which is within the low range and 24.5% for factor regarding votes preferences. We can clearly see from the plot that the data for votes is more dispersed. As our goal is to examine the variability of fully vaccinated population we see here that, although there is a variance, it is not very big. There are however some outliers which should be interesting to inspect.

[chart]("Boxplot")


The density plot confirms the big differences in the distribution between two variables. For most of the counties the vaccination rate is between 45% to 65% due to leptokurtic shape. A few counties have a rate below 40%. The maximum vaccination rate in a county was at a level of 76%. As for the votes for PiS commission there is a bimodal distribution- the first mode is at 40% and the second, smaller one, at around 63%.

[chart]("Density plot")


Now let's examine the position values of individual counties on a map. Below we can see two maps- both of them examine the deviation from the given point. For the map on the left, with vaccination rates, the point is a mean while for the other one this point is 50%. In the case of vaccination rates we can see that people tend not to vaccinate on the East of Poland. In the case of the rest of the country the higher vaccination rates are on North-Western part of Poland. In the case of the political preferences we can clearly see the border sometimes even on voivodeship levels. 

[map]("Maps of vaccination rate and political preference")

Those maps show a pattern for regions of Eastern Poland where there was a lower percentage of the population fully vaccinated against COVID-19 and a higher percentage of votes for candidates from the PiS election committee. 


The other way to visualize the differences can be done looking at the map of winning party. Here we can clearly see the political barrier on county level in Poland. 

[map]("Map of political winning party during 2019 Sejm and Senat elections")


Based on the exploratory analysis we can see that there is a difference in the distribution of the vaccination rates and political preferences. However, those contrasts for vaccination rates are not very big. Most of the counties follow the mean vaccination rate and those which do not are not so numerous. Nevertheless, the distribution on the map delivered interesting insights and is a force fueling further analysis.


## Two-factor analysis

Thanks to the analysis of the correlation of the percentage of the population vaccinated by county and the support for the candidates of the Low and Justice (abbrevation PiS) electoral commission, it can be concluded that there is a negative strong (-0.74) correlation for the data. There is a strong correlation between the percentage of the vaccinated population and the support of candidates from the PiS election committee in 2019. This may mean that as support for PiS candidates increases, the percentage of the vaccinated population decreases or vice versa. 
[chart](Correlation_between_votes_cast_on_PiS_party_and_vaccinated_population.png, "Correlation plot")


### Linear regresion analysis

For this reason, it is worth conducting a further study in the form of a regression analysis. The results are shown below in a table. We can see that the support for PiS committee was significant factor negatively affecting the vaccination rate. Statistical findings:

|term                    | estimate| std.error| statistic| p.value|
|:-----------------------|--------:|---------:|---------:|-------:|
|(Intercept)             |    75.95|      1.03|     73.63|       0|
|Votes_on_PiS_commission |    -0.44|      0.02|    -21.24|       0|


A summary of the model statistics:

| r.squared| adj.r.squared| sigma| statistic| p.value| df|   logLik|     AIC|     BIC| deviance| df.residual| nobs|
|---------:|-------------:|-----:|---------:|-------:|--:|--------:|-------:|-------:|--------:|-----------:|----:|
|      0.55|          0.55|  4.75|    451.14|       0|  1| -1118.61| 2243.23| 2255.02|  8448.83|         374|  376|


As can be seen in the table above, support for candidates for the PiS commission is a significant variable (p value < 0.05). However, when looking at the coefficient of determination, it can be seen that the fit is still relatively small. The support of candidates for the PiS election committee explains 55% variability of percentage of the population vaccinated by county. This means that 45% of the data is not explained by the model. The rest variability is then caused by unknown factors.

Other interesting observation is the correlation between the percentage of the vaccinated population and the cumulative COVID-19 infections. Such analysis shows a positive correlation at a level of 0.67. The relationship between the variables is visible and moderately high. The magnitude of the correlation makes it possible to conclude that higher cumulative COVID-19 infections lead to a higher percentage of the vaccinated population. 

[chart]("Correlation between votes cast on PiS party and vaccinated population")


### Spatial regresion analysis

In order to better understand the dataset we can perform spatial regression analysis. The result of moran test shows that there is indeed a spatial dependency (p-value < 0.05) with moderate clustering (0.5). Running Lagrange Multiplier diagnostics Spatial Durbin Error (SDEM) was chosen as the best one. With this model the pseudo r square was at the level of 69%. We can clearly see that there is a big improvement in terms of the fit of the model. Such a model explains 69% of the variability of the vaccination rates using spatial dependency. In terms of the model insights we can learn that there are significant indirect effects on vaccination rate. This means that if the county is close to another county with similar political preference the vaccination rate is affected by it. If the neighbouring county voted on PiS committee then the county tends to have smaller vaccination rate by about -0.7%. What is worth noting the effect is relatively low due to low coefficient of variation in the vaccination rate dataset. Lambda of the SDEM is equal to 0.6. This indicates that the when the average of the residual neighbours increases by one the residual of vaccination rates increases by 0.6.

|term                         |     estimate|   std. error|   z value|  Pr(>|z|)|
|:----------------------------|------------:|------------:|---------:|---------:|
|(Intercept)                  |    78.877118|     2.210656|   35.6804|   < 2e-16|
|Votes_on_PiS_commission      |    -0.416417|     0.021518|  -19.3519|   < 2e-16|
|lag.Votes_on_PiS_commission  |    -0.078464|     0.037388|   -2.0986|   0.03585|


The SDEM model explains 70% of the variability of percentage of the population vaccinated by county. As we can see applying a model with spatial effects drastically improved the fit of the model from 55% to 70%. 

| r.squared|     AIC|     BIC| deviance|   logLik| nobs|
|---------:|-------:|-------:|--------:|--------:|----:|
|       0.7| 2128.77| 2148.42|  5678.27| -1059.39|  376|

In terms of visual representation of the two variables the bivariate plot was used. Examining the map we can see the positive clustering where counties group with similar vaccination rates. The map uses quantile grouping which is a result of very strong division between counties. There appears to be small number of counties with both high vaccinatian rate and votes on PiS committee during 2019 Sejm and Senat elections. Extreme positions on the top left and bottom right of the legend are, however, very present. This proves the results of both linear regression and spatial analysis. Counties with lower vaccination rates are the ones in which people voted on PiS committee more often.

[map](Two-factor_analysis_bivariate_plot.png, "Two-factor analysis bivariate plot")


## Multivariate analysis

Further analysis include adding further variables in order to better understand the distribution differences of the vaccination rates.

The significance of the variable and room for improvement in r square area determined the willingness to learn about other factors influencing the percentage of the vaccinated population. For this purpose, the model was extended with additional variables, the selection of which was limited by the availability of data on the poviat cross-section. Analyzed factors:
- Feminization index - are counties with women more or less willing to vaccinate?
- Votes on PiS commission (%) - does political preference affect vaccinations?
- Working age population share - is working class more or less ready to vaccinate?
- Doctors and staff per 10 thousand people in 2020 - the more doctors and medical staff the more educated local community should be to take vaccine.
- Population per 1 square km in 2021 - people living in denser places are more prone to infections which should affect their readiness to vaccinate.
- Higher education population share - counties with higher share of population with high education should be more willing to vaccinate due to wider knowledge.
- Health expenditure per 1 person - is health expenditure in county relevant for readiness to vaccinate? More expenditure should positively affect people belief in science.

First let's examine the correlation between the variables. Here we can see that the variables are not strongly correlated to each other. The correlation that is most important in the terms of this research is between the vaccination rates and other factors. There we can see that Health expenditure per 1 person, Working age population share, Votes on PiS commission (%) are negatively correlated although in different strength. The first two variables have low negative correlation. The other factors (Feminization index, Doctors and staff per 10 thousand people in 2020, Population per 1 square km in 2021, Higher education population share) moderately positively correlated. One factor that can cause problems is the correlation between Feminization index and Working age population share which are strongly correlated to each other. It is indeed a natural reason- the more women enter the job market the more working age population share there is in a population. 

[chart]("Correlation plot multivariate")

### Linear regresion analysis

Now when we know the correlation for many variables we can model linear regression using all factors.

This is the result of a regression analysis before making insignificant factors reduction:

|term                                             | estimate| std.error| statistic| p.value|
|:------------------------------------------------|--------:|---------:|---------:|-------:|
|(Intercept)                                      |   222.67|     21.47|     10.37|    0.00|
|Higher_education_population_share_in_2011        |    15.89|      4.85|      3.27|    0.00|
|Doctors_and_staff_per_10_thousand_people_in_2020 |     0.01|      0.01|      0.84|    0.40|
|Health_expenditure_per_1_person                  |     0.00|      0.01|      0.78|    0.44|
|Population_per_1_square_km_in_2021               |     0.00|      0.00|     -0.02|    0.99|
|Votes_on_PiS_commission                          |    -0.39|      0.02|    -18.00|    0.00|
|Feminization_index                               |    -0.40|      0.11|     -3.51|    0.00|
|Working_age_population_share                     |    -1.85|      0.22|     -8.22|    0.00|


The summary of model-level statistics were shown in a table below. We can see that a model consisting of all the variables explains 65% of the variability of the vaccination rates among counties. 

| r.squared| adj.r.squared| sigma| statistic| p.value| df|   logLik|     AIC|     BIC| deviance| df.residual| nobs|
|---------:|-------------:|-----:|---------:|-------:|--:|--------:|-------:|-------:|--------:|-----------:|----:|
|      0.65|          0.65|  4.21|    100.41|       0|  7| -1081.27| 2180.53| 2215.99|  6589.06|         372|  380|


Proceeding further in the regression analysis in the study, the decision was made to remove the variables with a p value below 5%. Using stepwise regression reduction, the following variables were proved to be significant:
- Feminization index
- Working age population share
- Votes on PiS commission (%)
- Higher education population share

The result of a regression analysis for those factors is shown below in a table.

|term                                      | estimate| std.error| statistic| p.value|
|:-----------------------------------------|--------:|---------:|---------:|-------:|
|(Intercept)                               |   220.75|     20.73|     10.65|       0|
|Higher_education_population_share_in_2011 |    16.61|      4.74|      3.50|       0|
|Feminization_index                        |    -0.36|      0.09|     -3.88|       0|
|Votes_on_PiS_commission                   |    -0.39|      0.02|    -18.17|       0|
|Working_age_population_share              |    -1.88|      0.22|     -8.49|       0|

Economic interpretation of the variables:
- If higher education population share increases by one percent, the rate of vaccination increases by 16.61 on average if all other factors are constant.
- If feminization index increases by one percent, the rate of vaccination decreases by 0.36 on average if all other factors are constant.
- If votes on PiS commission share increases by one percent, the rate of vaccination decreases by 0.39 on average if all other factors are constant.
- If working age population share increases by one percent, the rate of vaccination decreases by 1.88 on average if all other factors are constant.

The visual presentation of the quality of the model shows there are still more factors that shaped the vaccination rates. As we can see the model tends to overestimate the vaccination rate for counties with lower vaccination rates while underestimating for other counties with higher rates.

[chart]("The actual and modelled vaccination rate")


The summary of model-level statistics does not show any change in quality of the model. Model still predicts 65% of the percentage of the vaccinated population. The rest 35% is a result of other, random factors. Lack of access to more detailed data on media coverage, people characteristics makes it harder and more time consuming to further improve the model. 

| r.squared| adj.r.squared| sigma| statistic| p.value| df|   logLik|     AIC|     BIC| deviance| df.residual| nobs|
|---------:|-------------:|-----:|---------:|-------:|--:|--------:|-------:|-------:|--------:|-----------:|----:|
|      0.65|          0.65|   4.2|    176.15|       0|  4| -1081.95| 2175.91| 2199.55|  6612.94|         375|  380|

Statistic interpretation of model:
- Coefficient of determination (r squared) - 65% of total variability of vaccination rate were explained by the model.
- Convergence coefficient (1 - r squared) - 35% of total variability of vaccination rate were not explained by the model, i.e. it is the effect of random fluctuations in the vaccination rates. 
- Standard deviation of residuals - the observed values of the vaccination rate deviate from the theoretical values of the vaccination rate by an average of ± 2.05 percentage points.
- Coefficient of variability - the standard deviation of the residuals is 7.67% of the arithmetic mean of the vaccination rate. Let's assume the border value of the coefficient of variability to be at 15%. This means that the deviations of the empirical values of the vaccination rate from the theoretical values of the vaccination rate are acceptable.
- Joint study of the significance of structural parameters (F test) - p value for the model < 0.05
𝐻0: 𝛼1 = 𝛼2 = 0 (structural parameters 𝛼1 and 𝛼2 are statistically insignificant, i.e. explanatory variables 𝑋1 and 𝑋2 do not statistically significantly affect the dependent variable 𝑌)
𝐻1: 𝛼1 ≠ 0 ⋁ 𝛼2 ≠ 0 (at least one structural parameter 𝛼1, 𝛼2 is statistically significant, i.e. at least one explanatory variable 𝑋1, 𝑋2 has a statistically significant influence on the dependent variable 𝑌)

The verification decision is as follows:
At the significance level of 𝛼 = 0.05, the 𝐻0 is rejected with the parameters being irrelevant. We have the right to believe that at least one structural parameter 𝛼1, 𝛼2, 𝛼3 is statistically significant, i.e. at least one explanatory variable (feminization index, working age population share, votes on PiS commission (%), higher education population share) has a statistically significant impact on the dependent variable 𝑌 (vaccination rate).

Based on the chart we can also provide one more statistically important insight. We can already have some expectations about the presence of residual homoscedasticity looking at it but to confirm that there is an absence of heteroscedasticity we can use Breusch-Pagan test. Performing the test for heteroscedasticity among the residuals showed that there is indeed a homoscedasticity of residuals. The p-value is not significant (i.e., > 0.05), so we do not reject the null hypothesis on equal residual variance distribution. Therefore, we assume that the residuals are homoscedastic. This means that we can assume that the model results are reliable. 


### Spatial regresion analysis

The chart alone that we saw before, however, does not provide enough details without spatial visualization. Conducting this approach should provide us with further insights into what kind of factors may be relevant for vaccination rate. If the spatial distribution follows the political division line than the factors that are missing are politically related ones. Otherwise, we can conclude that the differences are caused by other, unknown forces. This should be crucial for interpreting the final results of the research.

[map]("Residuals actual vs model distribution using Ordinary Least Square Model")

As we can see the spatial distribution is not connected with political preferences so we can conclude that there are other factors, not related with political division, that would need to be considered in order to provide more insight into the differences between vaccination rates. To further delve into the topic it is worth conducting the new spatial regression analysis. Using moran's test for the residuals of OLS model there is a significant spatial dependency (p value < 0.05). This means that differences between counties willingness to vaccinate are not a result of political factors (at least division ones) but unknown ones. We can also conclude from the result that there is a low to medium clustering of similar values (0.38) which suggests that the missing factors can be found using spatial regression models. Monte-Carlo simulation of Moran I also proved that the spatial dependency is indeed significant. 


Applying linear regression with all variables and conducting moran's test it showed that the vaccination rate can be spatially explained using provided variables. It means that the missing factors can be found and are not strictly random effects. Lagrange multiplier diagnostics for spatial dependence revealed that the model with the best fit should be the Spatial Error Model (SEM) due to lowest p value. Further research, using stepwise regression factor reduction, the significant variables identified by spatial analysis proved to be the same used in linear regression:
- Feminization index
- Working age population share
- Votes on PiS commission (%)
- Higher education population share

Selection of the model was correct as Akaike Information Criterion (AIC) confirmed it by giving the lowest score to the Spatial Error Model. Conducting Test Model Restrictions which determine if the model should be restricted, or in other terms simplified, this model also proved to be the best. Using Breusch-Pagan test there appears to be no evidence for Heteroskedasticity (p-value = 0.69 > 0.05). The model shows that there is a dependency of errors of error values of an area with errors in other areas associated with it. Once the average of the residual neighbors increases by one the residual of vaccination rates increases by 0.62.

|term                                      | estimate| std.error| statistic| p.value|
|:-----------------------------------------|--------:|---------:|---------:|-------:|
|(Intercept)                               |   198.08|     19.69|     10.06|       0|
|Higher_education_population_share_in_2011 |    13.97|      4.06|      3.44|       0|
|lambda                                    |     0.62|      0.05|     12.07|       0|
|Feminization_index                        |    -0.32|      0.09|     -3.71|       0|
|Votes_on_PiS_commission                   |    -0.35|      0.02|    -15.17|       0|
|Working_age_population_share              |    -1.59|      0.20|     -7.81|       0|

The visual presentation on the chart of the quality of the model shows there are still more factors that shaped the vaccination rates together with lambda factor. 

[chart]("The actual and modelled vaccination rate using Spatial Error Model")


The Spatial Error Model explains 76% of the variability of percentage of the population vaccinated by county. As we can see applying a model with spatial effects improved the fit of the model although there is still 24% of unexplained differences between individual counties. The most statistically important variable was 'Votes on PiS commission'. The Wald Statistics Test confirms that the SEM is a better model than OLS model (Wald test p value < 0.05).

| r.squared|     AIC|     BIC| deviance|   logLik| nobs|
|---------:|-------:|-------:|--------:|--------:|----:|
|      0.76| 2046.55| 2074.06|   4498.4| -1016.28|  376|


[map]("Residuals actual vs model distribution using SEM")

Spatial distribution of the residuals shown below does not present spatial dependency. We can therefore confirm that after applying spatial regression analysis the rest of unknown factors can not be found in spatial regression. The missing part (24%) are the factors that do not impact vaccination rate spatially but, without other data, randomly. In order to enable comparison between the OLS and SEM the legend scale limits for SEM residuals distribution on a map were changed to the ones used on OLS map. The extreme values for SEM residuals were -15.5 and 12.9 while for OLS these were -22.2 and 14.5. Increase in R square shows the improvement when using spatial model. 


## Conclusions

Based on the verification activities carried out regarding both the economic verification of the model and the statistical verification, it can be concluded that the OLS model provides moderate fit (66%) and the model with spatial effects proved to explain 76% of variability. The research gave insight into what drives and what does not affect the tendency to vaccinate. As the researched proved the significant variables were: feminization index, working age population share, votes on PiS commission (%), higher education population share. Only the last factor positively impacted the vaccination rate in the county. ALl the other factors worked the other way round. The increase of faminization rate, working age population share and higher education population share influenced decrease of the vaccination rate. The most important role in explaining the variability of the vaccination rate had spatial distibution of votes on PiS commision. This variable itself explained 55% of the variability of the vaccination rate when applied using OLS model and 70% using SDEM. The fact that vaccination rates are so heavily linked to political preference is a fascinating result implying shared behaviors among the voters on governing party Law and Justice (PiS). By itself the result may be terrific, however one more thing should be taken into consideration. While it is true that there is a strong negative correlation between the political preference and vaccination rates the distribution of the vaccination rates is close to the mean and the coefficient of deviation is relatively small. This means that although the political preference is important, it does not result in very big discrepancies between counties. 

A negative correlation is a result partly of the message governing party Law and Justice (PiS) passed in its public media outlets. On the one hand the social campaigns were encouraging vaccination by using famous people images while on the other hand giving contradictory or unclear instructions to health recommendations. We can see effects of this situation on the bivariate map of the vaccination rates and votes on PiS commission.

There is still a pending question why is the mean of vaccination rates so low. This question should be, however, a subject of a different research.

The interesting part is also polish society reluctance to medical staff recommendations. As the research proved, the variables doctors and staff per 10 thousand people in 2020, population per 1 square km in 2021, health expenditure per 1 person proved not to be significant. 
