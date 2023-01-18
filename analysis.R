setwd('/C:/MA317/')
library(ggplot2)
library(maptools) # For wrld_simpl polygons
library(missForest)
library(sf)
library(VIM)
library(tidyverse)
library(caret)
library(olsrr)
library(dplyr)
library(mice)
#--------------------------------question - 1 --------------------------------------------
# Analyse using descriptive statistics (both graphical and numerical representations) 
# and R the Life Expectancy data1.csv dataset.
in.csv = read.csv('/C:/MA317/data/Life_Expectancy_Data1.csv')
# table column name
print(colnames(in.csv))
# table dimension
print(dim(in.csv))
# table summary
print(summary(in.csv))

# table data type
print(str(in.csv))

indicator_name =c('life_expectancy','Electricity','Net_National_income','Net_national_income_capita',
  'childeren_affected_with_HIV','children_out_of_school','Educational_attainment_primary','Educational_attainment_bachelor',
  'Mortality_rate','Primary_completion','Literacy_rate','Real_interest_rate',
  'Population_growth','Population_density','total_population','health_expenditure_capita',
  'health_expenditure','unemployment','GDP_annual','GDP_per_capita','Birth_rate',
  'renewablw_energy','Adults_HIV','safely_drinking_water','poverty','compulsory_education')
#changing column name eith indicator name
colnames(in.csv)[-c(1:3)] = indicator_name
print(colnames(in.csv))
as.data.frame(summary(in.csv))[-1]
output<-capture.output(summary(in.csv), file=NULL,append=FALSE)
output_df <-as.data.frame(output)
View(output_df)

print(str(in.csv))
ggplot(subset(in.csv), aes(x = GDP_per_capita, y = Mortality_rate)) + 
  geom_point()

in.csv %>% 
  ggplot(aes(x=Continent,y=life_expectancy, fill=Continent)) +
  geom_boxplot() + geom_jitter(width=0.1,alpha=0.2) 

ggplot(in.csv, aes(x = life_expectancy)) + 
  geom_histogram(aes(fill = Continent)) +
  facet_wrap(~Continent)

ggplot(in.csv, aes(life_expectancy, fill = Continent, colour = Continent)) +
  geom_density(alpha = 0.2, na.rm = TRUE) 

ggplot(in.csv, aes(life_expectancy, GDP_per_capita)) + 
  geom_point() +
  geom_smooth(aes(colour = "loess"), method = "loess", se = FALSE) + 
  geom_smooth(aes(colour = "lm"), method = "lm", se = FALSE) +
  labs(colour = "Method")

in.csv %>% select(where(~mean(is.na(.))< 0.4))%>% select(is.numeric)%>%
  select(-starts_with(c("total_population","GDP_per_capita","Population_density",'Adults_HIV',
                        'health_expenditure_capita','children_out_of_school')))%>%boxplot(las= 2, col="#69b3a2",
                                                                                     boxwex=0.5)


ggplot(subset(in.csv), aes(x = GDP_per_capita, y = life_expectancy, color = Country.Name)) + 
  geom_point() +  ggtitle("Life Expectancy and GDP Per capita by country")+
  geom_text(aes(label = Country.Name), size=3, nudge_y = 0.4) +
  scale_x_continuous(limits = c(0, 70000))+theme(legend.position="none")

ggplot(subset(in.csv), aes(x = GDP_per_capita, y = Mortality_rate, color = Country.Name)) + 
  geom_point() +  ggtitle("Infant Moratality rate by GDP capita by country")+
  geom_text(aes(label = Country.Name), size=3, nudge_y = 0.4) +
  scale_x_continuous(limits = c(0, 70000))+theme(legend.position="none")


#------------------Question-2-----------------------------------------------
#Dealing with missing value
print(colnames(in.csv))
print(str(in.csv))
#change str type of compulsory education character to integer
in.csv$compulsory_education = as.numeric(in.csv$compulsory_education)
#Check missing values introduced in the data
df_na = data.frame("colsu" = colSums(is.na(in.csv)))
print(df_na)
#selecting column which having less than 70% of NA
df_mis = in.csv %>% select(where(~mean(is.na(.))< 0.5))
#Check missing values introduced in the data
print(summary(df_mis))
# Removing categorical data
print(colnames(df_mis))
df_no_na <- subset(df_mis, select = -c(Country.Name,Country.Code,Continent))
df_plot <- aggr(df_no_na, col=c('navyblue','yellow'),
                    numbers=TRUE, sortVars=TRUE,
                    labels=names(df_no_na), cex.axis=.7,
                    gap=3, ylab=c("Missing data","Pattern"))

set.seed(300)
df.imp <- missForest(df_no_na)
print(head(df.imp$ximp,3))
print(head(df_no_na,3))
print(df.imp$OOBerror)
impu_df = df.imp$ximp
#-------------------------------Question - 3------------------------------------
#dealing with coolinearity
print(colnames(impu_df))
set.seed(123)
training.samples <- impu_df$life_expectancy %>%
  createDataPartition(p = 0.8, list = FALSE)
train.data  <- impu_df[training.samples, ]
test.data <- impu_df[-training.samples, ]
model1 = lm(life_expectancy~.,data = train.data)
# Make predictions
predictions <- model1 %>% predict(test.data)
# Model performance
data.frame(
  RMSE = RMSE(predictions, test.data$life_expectancy),
  R2 = R2(predictions, test.data$life_expectancy)
)
# The smallest possible value of VIF is one (absence of multicollinearity). As a rule of thumb,
# a VIF value that exceeds 5 or 10 indicates a problematic amount of collinearity.
car::vif(model1 )
# Build a model excluding the tax variable
model2 <- lm(life_expectancy ~. -Net_National_income - Net_national_income_capita - GDP_per_capita -Birth_rate,
             data = train.data)
# Make predictions
predictions <- model2 %>% predict(test.data)
# Model performance
data.frame(
  RMSE = RMSE(predictions, test.data$life_expectancy),
  R2 = R2(predictions, test.data$life_expectancy)
)
# It can be seen that removing the these variable
ols_vif_tol(model2)
# n practice it is common to say that any VIF greater than  5
# is cause for concern. So in this example we see there is a huge multicollinearity 
# issue as many of the predictors have a VIF greater than 5.
ols_eigen_cindex(model2)
ols_coll_diag(model2)
ols_plot_resid_fit_spread(model2)
ols_correlations(model2)
ols_plot_obs_fit(model2)
ols_plot_diagnostics(model2)
ols_plot_comp_plus_resid(model2)

#--------------------------QUSETION - 4-----------------------------
library(MASS)
# Fit the full model 
full.model <- lm(life_expectancy ~., data = train.data)

# Stepwise forward regression model
for.reg <- ols_step_forward_p(full.model, details = TRUE)
plot(for.reg)
for.aic = ols_step_forward_aic(full.model)
plot(for.aic)
# Stepwise backward regression model
back.reg <- ols_step_backward_p(full.model, details = TRUE)
plot(back.reg)
back.aic = ols_step_backward_aic(full.model)
plot(back.aic)
# Stepwise both regression model
both.reg <- ols_step_both_p(full.model, details = TRUE)
plot(both.reg)
both.aic = ols_step_both_aic(full.model)
plot(both.aic)

print(data.frame(for.reg$model$coefficients))
print(data.frame(back.reg$model$coefficients))
print(data.frame(both.reg$model$coefficients))


#-----------------Question -5----------------Renuka A-----------
print(colnames(in.csv))
print(nrow(in.csv))
print(head(in.csv))

# Read regions dataset with country categories
in.region<- read.csv("../data/Region.csv")
in.csv$Regions<- ifelse(in.csv$Country.Name == in.region$Country.Name, in.region$Region,NA)
summary(in.csv)

# Factorizing  Regions variable and getting levels of variable
in.csv$Regions = as.factor(in.csv$Regions)
levels(in.csv$Regions)

# summary after grouping by
group_by(in.csv, Regions) %>%  summarise(
  count = n(),
  mean = mean(life_expectancy, na.rm = TRUE),
  sd = sd(life_expectancy, na.rm = TRUE)
)
# analysis of variance (ANOVA) with life_expectancy vs Regions
res.aov <- aov(life_expectancy ~ Regions, data = in.csv)
summary(res.aov)

TurkeyHSD<-TukeyHSD(res.aov)
# diff: difference between means of the two groups
# lwr, upr: the lower and the upper end point of the confidence interval at 95% (default)
# p adj: p-value after adjustment for the multiple comparisons.
print(TurkeyHSD)

