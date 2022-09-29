# Covid-19 analysis - is vaccination rate correlated with political preferences? 

## Project objectives

The project aims to discuss and evaluate the impact of the factors on the rate of fully vaccinated population. The project objective is to answer the question whether voting preferences had an impact on the willingness to vaccinate and whether there is a correlation between the share of the population vaccinated with two doses and the number of deaths.

## Two-factor analysis

Thanks to the analysis of the correlation of the percentage of the population vaccinated by county and the support for the candidates of the PiS electoral commission, it can be concluded that there is a negative strong (-0.74) correlation for the data. There is a strong correlation between the percentage of the vaccinated population and the support of candidates from the PiS election committee in 2019. This may mean that as support for PiS candidates increases, the percentage of the vaccinated population decreases or vice versa. 

|                                | Fully vaccinated population (%)| Votes on PiS commission (%)|
|:-------------------------------|-------------------------------:|---------------------------:|
|Fully vaccinated population (%) |                          1.0000|                     -0.7418|
|Votes on PiS commission (%)     |                         -0.7418|                      1.0000|

For this reason, it is worth conducting a further study in the form of a regression analysis with statistical significance (p-value) and determination coefficients (r-squared).


|term        |   estimate| std.error| statistic| p.value|
|:-----------|----------:|---------:|---------:|-------:|
|(Intercept) | 76.1077556| 1.0226710|  74.42057|       0|
|KOMITET_PiS | -0.4471268| 0.0207919| -21.50484|       0|


| r.squared| adj.r.squared|    sigma| statistic| p.value| df| df.residual| nobs|
|---------:|-------------:|--------:|---------:|-------:|--:|-----------:|----:|
| 0.5502452|     0.5490554| 4.759468|   462.458|       0|  1|         378|  380|


As can be seen in the table above, support for candidates for the PiS commission is a significant variable (p value < 0.05). However, when looking at the coefficient of determination, it can be seen that the fit is unsatisfactory. The percentage of the population vaccinated by county explains the variability of support for candidates for the PiS election committee only in 19%. This means that 81% of the data on the share of the vaccinated population in total by county is not explained by the model.

When analyzing the correlation for the percentage of the vaccinated population and the cumulative COVID-19 infections, a low positive relationship between the variables is visible. The magnitude of the correlation makes it impossible to conclude that higher cumulative COVID-19 infections lead to a higher percentage of the vaccinated population. In this case, it was necessary to clear the data from outliers, which were: Kraków, Łódź, Wrocław, Poznań poviat, Gdańsk, Szczecin, Bydgoszcz, Lublin, Kraków poviat, Białystok, Wołomin poviat, Gdynia, Toruń. In these areas, COVID-19 infections have significantly exceeded the total population, which may be due to the higher number of vaccinations from neighboring counties in a larger city or district due to, for example, the availability of vaccines.

The regression analysis below helps us to conclude that the proportion of just 19.8% of the vaccinated population describes the number of infections per coronavirus. Although this is a significant variable, no relationship can be established from it.

## Multivariate analysis

The significance of the variables and the low R square determined the willingness to learn about other factors influencing the percentage of the vaccinated population. For this purpose, the model was extended with additional variables, the selection of which was limited by the availability of data on the poviat cross-section. Factors added:

The maps below show a pattern for regions of Eastern Poland where there was a lower percentage of the population vaccinated with two doses of COVID-19 and a higher percentage of votes for candidates from the PiS election committee. However, the correlation for the whole country showed that there is no strong correlation, although the variables are important to each other.

Observing the results of data visualization on other maps, it is difficult to see other relationships between the variables. The pattern is the occurrence of outliers for data from urban centers. Therefore, further analysis should already be based on the calculation of the correlation between individual variables.

The nationwide analysis of correlation makes it possible to check which factors were related to each other and in what direction. Among the above-mentioned factors, the strongest, though still moderate, correlation with the percentage of the vaccinated population was found in the population density per km2 and in the share of votes in the overall PiS. The other factors showed a much smaller correlation. It is also important to pay attention to COVID-19 infections, which are strongly correlated with the number of deaths. This explains why governments around the world have taken decisive steps to reduce coronavirus infection. More cases result in more deaths, which translates into a number of economic and social aspects. There are also some more mysterious reports - e.g. the number of doctors per 1,000 inhabitants positively correlates with infections and deaths due to COVID-19. It is possible that this is due to the increased number of doctors in the cities with the highest number of identified infections. People in serious condition also go to larger centers where there is access to more medical personnel, including doctors. Deaths in such centers are most often recorded in cities.



Moving on to the regression analysis in the study, it was undertaken to remove the variables with a p value below 5%, leaving the variables important for creating the model with the highest value of the coefficient of determination. According to the regression analysis, the following variables were significant factors:
- Support for the candidates of the PiS election commission in the 2019 elections to the Sejm.
- Population density per km2

All other variables did not have a significant effect on the number of vaccinations with the second dose in relation to the population of the poviat. The R square for such data was 34%, which corresponds to a low fit. Only 34% of the variables explain the percentage of the vaccinated population of the region.