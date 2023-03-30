**Statistical Learning Final Project**  
Pegah Karimi & Amie Rowland  
Instructor: Dr. Burcu Eke Rubini  
2023-03-10

================

We used a data set from Kaggle competition \[1\] with 10 columns and
more than 26,000 observations. It belonged to a shelter which held dogs
and cats and our variable of interest is what happened to those animals
at the end. There were 5 possible outcomes: 1. Adoption: They got
adopted. 2. Transfer: The got transferred to another shelter 3.
Return_to_owner: They were lost and were held at the shelter until their
owner found them there. 4. Death: They died while they were at the
shelter. 5. Euthanasia: They were euthanized by the shelter.

The goal of the shelter was to keep 95% of its animals alive \[2\], by
either getting adopted, transferred or returned to their owners.

For conducting predictive analysis in this project, we decided not to
include some of the variables in any of our models, such as `AnimalID`
and `Name`, which are specific to each animal and carry their specific
characteristics, and since we are using the animal’s characteristics
within our other columns, it doesn’t make sense to use these columns
again and for other animals as well, so we exclude them from our models.

In addition, we didn’t use the `DateTime` column as we thought our other
variables are more important in our model, but it can be included in
future studies. Another column we decided to exclude from our models was
`OutcomeSubtype`, as it is part of our y variable and we can only know
it after the output has already taken place.

We really wanted to use Breed of the animals to predict what happens to
them at the end, but we had more than 1,000 different types of breeds in
our data set and we didn’t have enough time to merge them to fewer
categories and use them in our models, so we excluded Breed as well.
However, for the Color column, although we initially had more than 300
different mixtures of colors in our data set, we managed to shrink those
categories to 29 by considering the animal’s dominant color using Excel,
and we used it in some of our models.

One of the columns in this data set was animal’s age, which was in day,
week, month, and year, so we first cleaned it in Excel and converted all
of them to days. However, there were 18 null values in the age column
before our data manipulation, and they ended up being ‘FALSE’ in our
data set, so in the following code we removed them from the data.

After conducting predictive analysis for half of the questions for our
project, we noticed that we are having issues using some models since
our data is very unbalanced. Thus, we decided to reduce 5 possible
outcomes to 4, by merging data related to animals dying or being
euthanized as one category. It didn’t solve the unbalanced-data-issue
completely, but it helped it a lot, at least this new category emerged
in our train and test sets so that our model could handle them.

We then changed the name of our desired variable from `OutputType` to
`y`.

``` r
df <- read.csv('/Users/pegahkarimi/MSBA/Term 3/Statistical learning/Project/Data/Shelter04.csv', header = TRUE)

# There were 18 null values in the age column and we want to remove them: 

df <- df %>% filter(Age_day!='FALSE')
df <- df %>% filter(SexuponOutcome!='')

# We change the column name of Output Type to y.

colnames(df)[4] <- 'y'

df$y[df$y=='Euthanasia']='Euthanasia/Death'
df$y[df$y=='Died']='Euthanasia/Death'

# table(df$Color)
# 
# df %>% distinct(Color)
```

## Part 1: Exploratory Data Analysis

1.  Check for existence of NA’s (missing data)

We checked for NA’a and we didn’t have any in our data. We also checked
for blanks in the columns which we will be using in our models, and we
didn’t have blanks in those columns as well.

``` r
# confirm there are no NAs before addressing other missing data
sum(is.na(df))
```

    ## [1] 0

``` r
# There are some columns like Outcome Subtype and Name which have missing values, but we don't want to use them for our prediction. 

sum(df$OutcomeSubtype=='')
```

    ## [1] 13611

``` r
# Checking whether other variables that want to use for our prediction have null values.

sum(df$AnimalType=='')
```

    ## [1] 0

``` r
sum(df$SexuponOutcome=='')
```

    ## [1] 0

``` r
sum(df$Age_day=='')
```

    ## [1] 0

``` r
sum(df$Breed=='')
```

    ## [1] 0

``` r
sum(df$Color=='')
```

    ## [1] 0

``` r
sum(df$y=='')
```

    ## [1] 0

2.  If necessary, classify all categorical variables **except the one
    you are predicting** as factors. Calculate the summary statistics of
    the entire data set.

We made all our categorical variables, even the ones we don’t want to
use in our models, as factors, and also made our only numeric variable,
age, as number. Although the question asked not to make y variable as
factor, we found out that making it a factor won’t negatively affect the
models we used in this project, so we decided to have it here to make
sure it is a factor.

``` r
# categorical variables as factors
df$AnimalID <- as.factor(df$AnimalID)
df$Name <- as.factor(df$Name)
df$OutcomeSubtype <- as.factor(df$OutcomeSubtype)
df$AnimalType <- as.factor(df$AnimalType)
df$SexuponOutcome <- as.factor(df$SexuponOutcome)
df$Breed <- as.factor(df$Breed)
df$Color <- as.factor(df$Color)

# numeric as number
df$Age_day <- as.numeric(df$Age_day)

# head and summary
df$y <- as.factor(df$y)
summary(df)
```

    ##     AnimalID          Name         DateTime                        y        
    ##  A006100:    1          : 7673   Length:26710       Adoption        :10769  
    ##  A047759:    1   Max    :  136   Class :character   Euthanasia/Death: 1750  
    ##  A134067:    1   Bella  :  135   Mode  :character   Return_to_owner : 4785  
    ##  A141142:    1   Charlie:  107                      Transfer        : 9406  
    ##  A163459:    1   Daisy  :  106                                              
    ##  A178569:    1   Lucy   :   94                                              
    ##  (Other):26704   (Other):18459                                              
    ##     OutcomeSubtype  AnimalType                       Breed           Color     
    ##            :13611   Cat:11117   Domestic Shorthair Mix  : 8794   Black  :6641  
    ##  Partner   : 7816   Dog:15593   Pit Bull Mix            : 1906   Brown  :5312  
    ##  Foster    : 1800               Chihuahua Shorthair Mix : 1766   White  :3343  
    ##  SCRP      : 1583               Labrador Retriever Mix  : 1363   Blue   :2146  
    ##  Suffering : 1000               Domestic Medium Hair Mix:  839   Tan    :1674  
    ##  Aggressive:  320               German Shepherd Mix     :  575   Orange :1343  
    ##  (Other)   :  580               (Other)                 :11467   (Other):6251  
    ##        SexuponOutcome    Age_day      
    ##  Intact Female:3504   Min.   :   0.0  
    ##  Intact Male  :3519   1st Qu.:  60.0  
    ##  Neutered Male:9779   Median : 365.0  
    ##  Spayed Female:8819   Mean   : 794.1  
    ##  Unknown      :1089   3rd Qu.:1095.0  
    ##                       Max.   :7300.0  
    ## 

3.  For the numerical variables, plot box plots based on values of `y`.
    Do you see a difference between the box plots for any of the
    variables you choose?

The only numerical variable we had was `Age`. We can see that the
average age of animals which get adopted are lower than other categories
probably because people tend to adopt kittens and puppies. We also see
that the average age of animals which get returned to their owners are
higher than other categories, which might be due to the fact that if
people find a dog that is micro chipped or wearing a collar, people at
the shelter would know it’s someone’s pet and the owner will come to get
their pet. In addition, for the Return_to_owner category to happen, a
dog should be first adopted and get raised by families and then get
lost, so it makes sense that its average age is higher than other
categories.

For the average age of transfer to be low, it might be the case that
they transfer specific animals at their lower age to other shelters
where they have higher chances of getting adopted. In addition, some
animals might die or get euthanized because of disease or because of
getting too old that people at the shelter would think they probably
won’t be cured or adopted.

``` r
# The only numeric variable
boxplot(Age_day~y, data = df)
```

![](Statistical_Learning_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

4.  For the categorical variables, plot bar charts for the different
    values of `y`. Do you see a difference between plots for any of the
    variables you choose?

We plot these bar charts for both the animal type (Cat or Dog) and the
sex upon outcome (Intact Female, Intact Male, Neutered Male, Spayed
Female, Unknown). We examine both the overall distributions for these
categories, as well as how their distributions change when we look at
only the data within each outcome class of `y`.

``` r
df %>% distinct(y)
```

    ##                  y
    ## 1  Return_to_owner
    ## 2 Euthanasia/Death
    ## 3         Adoption
    ## 4         Transfer

``` r
# Dog vs Cat
barplot(prop.table(table(filter(df,y=='Return_to_owner')$AnimalType)), main = 'Return_to_owner')
```

![](Statistical_Learning_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
barplot(prop.table(table(filter(df,y=='Euthanasia/Death')$AnimalType)), main = 'Euthanasia/Death')
```

![](Statistical_Learning_files/figure-gfm/unnamed-chunk-6-2.png)<!-- -->

``` r
barplot(prop.table(table(filter(df,y=='Adoption')$AnimalType)), main = 'Adoption')
```

![](Statistical_Learning_files/figure-gfm/unnamed-chunk-6-3.png)<!-- -->

``` r
barplot(prop.table(table(filter(df,y=='Transfer')$AnimalType)), main = 'Transfer')
```

![](Statistical_Learning_files/figure-gfm/unnamed-chunk-6-4.png)<!-- -->

``` r
barplot(prop.table(table(df$AnimalType)))
```

![](Statistical_Learning_files/figure-gfm/unnamed-chunk-6-5.png)<!-- -->

``` r
# Sex upon Outcome
barplot(prop.table(table(filter(df,y=='Return_to_owner')$SexuponOutcome)), main = 'Return_to_owner')
```

![](Statistical_Learning_files/figure-gfm/unnamed-chunk-6-6.png)<!-- -->

``` r
barplot(prop.table(table(filter(df,y=='Euthanasia/Death')$SexuponOutcome)), main = 'Euthanasia/Death')
```

![](Statistical_Learning_files/figure-gfm/unnamed-chunk-6-7.png)<!-- -->

``` r
barplot(prop.table(table(filter(df,y=='Adoption')$SexuponOutcome)), main = 'Adoption')
```

![](Statistical_Learning_files/figure-gfm/unnamed-chunk-6-8.png)<!-- -->

``` r
barplot(prop.table(table(filter(df,y=='Transfer')$SexuponOutcome)), main = 'Transfer')
```

![](Statistical_Learning_files/figure-gfm/unnamed-chunk-6-9.png)<!-- -->

``` r
barplot(prop.table(table(df$SexuponOutcome)))
```

![](Statistical_Learning_files/figure-gfm/unnamed-chunk-6-10.png)<!-- -->

When reviewing the plots for animal types, we see that there are more
dogs than cats in our data set. Additionally, there are many more dogs
returned to owner than cats, which we think is due to the higher
proportion of cats kept as exclusively indoor pets, whereas dogs tend to
be indoor/outdoor pets more often. Thus, more dogs returned could be due
to more dogs getting lost (and found and returned). We notice for the
Euthanasia/Death category that the two types of animal have similar
numbers, but because there is a smaller proportion of cats in our
dataset, this means a higher percentage of cats are dying or being
euthanized compared to dogs. Further research could be undertaken to
determine why this is happening. Additionally, we see from these plots
that more dogs are adopted than cats, and that more cats are transferred
than dogs. We wonder if perhaps more cats are transferred out due to
this shelter’s space considerations; perhaps they are equipped to house
more dogs, or perhaps they could be affiliated with a cat cafe. If the
shelter is keeping more of the dogs that are taken in, then it makes
sense that more dogs are being adopted as well.

We also observe the plots for sex upon outcome. Notice that adoption and
return to owner both have a high amount of spayed and neutered pets.
This seems logical considering that spaying and neutering are common
practices for pets. The rates of spayed and neutered pets are lower
among pets that are transferred and pets that die or are euthanized. We
wonder if this could be due to additional health complications that
spaying and neutering can prevent, and we also noticed in our data for
outcome subtypes that the aggression label was sometimes used for
euthanized pets, particularly those not neutered, which could explain
what we see in the bar charts.

Overall, these charts suggest that both of these variables could be
worthwhile to use in our analysis.

5.  Test/training separation: Separate your data into 80% training and
    20% testing data. Do not forget to set seed. Please use the same
    separation for the whole assignment, as it is needed to be able to
    compare the models.

We have used stratified sampling since there are only a few observations
for some of the categories of our prediction variable. Our original data
had more than 26,000 data points, and we took a sample of 2000 data
points from it to use for this project. Then we separated our data into
80% training and 20% testing sets. For an unknown reason (because even
changing the seed didn’t fix the issue), when we used p=0.8, it didn’t
make a correct 80-20 split, so we modified the probability value so that
in practice, our train set becomes exactly 80% of the sample.

Before making the above decision, we explored different options for
sampling our data. Since we had unbalanced data and the category with
lowest representative (Euthanasia/Death) was a category of interest
among our dependent variable, we thought about how we can amplify it in
our sample. We came up with three options:

1.  Take a normal sample and use SMOTE between just that category and
    all other three categories: Our main problem with this method was
    that it works like KNN and calculates distances to the nearest
    values, but since most of our variables were categorical, we had to
    made them dummy variables to use for SMOTE, and it didn’t work if we
    had a lot of dimensions, so we could only convert a few of the
    variables to dummy variables to be used this way. Thus, although
    after a lot of effort we managed to run the SMOTE code, we didn’t
    use it because we thought in our case, it had a lot of error and did
    not represent our data accurately and effectively.

In addition, we knew that we mainly use SMOTE in cases we don’t have
enough data and we want to generate more of a specific category, but in
our case, we had 26,000 data points which consisted of more of real
observations of our category of interest which we could use, instead of
estimating a new data point using SMOTE, which led us to our second
option.

2.  Choose an engineered sample: We decided to have a manipulated sample
    from our large data set that has the same number of different
    categories of y, so that we reduce the bias in our models. It didn’t
    work well and our model accuracy became worse so we didn’t use it,
    but later in our presentation we learned from Dr. Rubini that it
    would have worked better if we chose a weighted sample, instead of a
    sample with similar weight.

3.  Take a normal sample and be aware of the error our models could have
    due to unbalanced data: We decided to use this way, and used the
    following code to take the sample and divide the data into train (to
    create the models) and test (to evaluate their predictive
    performance).

``` r
# Plot showing imbalances of our prediction variable

barplot(prop.table(table(df$y)), xlab = "y")
```

![](Statistical_Learning_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

``` r
# Sampling 2,000 data points

set.seed(17)

df1 = df[createDataPartition(df$y, p = 0.07484, list = FALSE),]

# Test/train split

index=createDataPartition(df1$y, p = 0.7994, list = FALSE)

train=df1[index,]
test=df1[-index,]

c(nrow(df1), nrow(train), nrow(test))
```

    ## [1] 2000 1600  400

We are including our code for SMOTE which ran successfully, but we
didn’t use it in our predictions as we thought it would have a high
error since it’s generated with a lot of categorical variables and still
we had a lot of other variables we didn’t use in it to avoid the error
of high dimensions.

``` r
# SMOTE 
df4=subset(df1, y!="Transfer")
df5=subset(df4, y!="Return_to_owner")
table(df5[,"y"])
```

    ## 
    ##         Adoption Euthanasia/Death  Return_to_owner         Transfer 
    ##              806              131                0                0

``` r
df5[,"binary"]=ifelse(df5[,"y"] == "Euthanasia/Death", 0, 1)

df5=subset(df5, select=-y)

df5 <- df5 %>% 
  mutate(isCat=ifelse(AnimalType=="Cat",1,0))%>% 
  dplyr::select(Age_day, isCat, binary) 

table(df5[,"binary"])
```

    ## 
    ##   0   1 
    ## 131 806

``` r
set.seed(1234)
separator =  sample(1:2,size=nrow(df5), prob=c(0.80, 0.20), replace=TRUE) 
train4=df5[separator == 1, ]
test4=df5[separator == 2, ]
test4$class=test4$binary
test4=subset(test4, select=-binary)

new_train<- SMOTE(X=train4[,-3], target=train4[,3], K=5)
table(new_train$data$class)
```

    ## 
    ##   0   1 
    ## 600 643

## Part 2: Logistic Regression or LDA

1.  Develop a classification model where the variable `y` is the
    dependent variable using the Logistic Regression or LDA, rest of the
    variables, and your training data set.

We first plotted a few independent variables grouped by our dependent
variable to see whether our dependent variable is well-separated or not.
Since our prediction variable doesn’t get separated well by our
predictors, and we have a lot of categorical variables as our
predictors, we prefer using Logistic Regression, although we have
multiple outcome classes in our data and we know that LDA would be
preferred for multiclass problems.

The question asks to include the rest of variables as independent
variables, but as explained, we had some concerns which prohibited us
from doing so. There are some variables which are specific to each
observation, such as its name or ID, which would probably cause our
model to overfit, so we tried not to include those features in our
model. In addition, there were variables like Breed which had many
categories and adding them prevented our model from converging, so we
had to take them out of the model as well.

``` r
# Plot

xyplot(train$AnimalType~train$Age_day, groups=train$y, ylab="AnimalType", xlab="SexuponOutcome")
```

![](Statistical_Learning_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

``` r
xyplot(train$AnimalType~train$Breed, groups=train$y, ylab="AnimalType", xlab="Breed")
```

![](Statistical_Learning_files/figure-gfm/unnamed-chunk-9-2.png)<!-- -->

We used a multinomial version of the Logistic Regression since our y
variable had more than 2 categories. We tried 3 different models with
animal’s age, type of animal (whether it’s a cat or a dog), its sex upon
outcome, and its color. Then for each model, we calculated the
statistical significance of our model parameters to see whether keeping
them in our model explained any thing about our data. If the calculated
number becomes approximately more than 2, we can claim it’s
statistically significant. We also compared the models’ fit in terms of
AIC and BIC.

``` r
# Logistic Regression

mult.log1=multinom(y ~ Age_day+AnimalType, data = train)
```

    ## # weights:  16 (9 variable)
    ## initial  value 2218.070978 
    ## iter  10 value 1842.793790
    ## final  value 1767.907871 
    ## converged

``` r
mult.log2=multinom(y ~ Age_day+AnimalType+SexuponOutcome, data = train)
```

    ## # weights:  32 (21 variable)
    ## initial  value 2218.070978 
    ## iter  10 value 1554.183609
    ## iter  20 value 1416.203640
    ## iter  30 value 1414.715099
    ## iter  40 value 1414.691485
    ## final  value 1414.691447 
    ## converged

``` r
mult.log3=multinom(y ~ Age_day+AnimalType+SexuponOutcome+Color, data = train)
```

    ## # weights:  144 (105 variable)
    ## initial  value 2218.070978 
    ## iter  10 value 1557.848454
    ## iter  20 value 1398.562176
    ## iter  30 value 1379.392649
    ## iter  40 value 1378.083767
    ## iter  50 value 1377.514165
    ## iter  60 value 1377.415295
    ## iter  70 value 1377.397538
    ## final  value 1377.397230 
    ## converged

``` r
# Calculating the statistical significance of our model parameters
summary(mult.log1)$coefficients/summary(mult.log1)$standard.errors
```

    ##                  (Intercept)    Age_day AnimalTypeDog
    ## Euthanasia/Death  -13.667964  7.7402188     -1.628211
    ## Return_to_owner   -25.923043  9.4838863     16.624732
    ## Transfer            2.863883 -0.8220856     -5.996714

``` r
summary(mult.log2)$coefficients/summary(mult.log2)$standard.errors
```

    ##                  (Intercept)   Age_day AnimalTypeDog SexuponOutcomeIntact Male
    ## Euthanasia/Death   -4.138745 10.491048   3.463870948                  9.459955
    ## Return_to_owner   -12.301555 10.606945   9.926921685                 10.835552
    ## Transfer           29.637086  3.436318  -0.001373808                  1.287785
    ##                  SexuponOutcomeNeutered Male SexuponOutcomeSpayed Female
    ## Euthanasia/Death                   -20.17841                   -18.93649
    ## Return_to_owner                    -13.22086                   -14.28946
    ## Transfer                           -32.77837                   -34.35460
    ##                  SexuponOutcomeUnknown
    ## Euthanasia/Death          1.143673e+02
    ## Return_to_owner          -2.457534e+09
    ## Transfer                  1.032779e+02

``` r
summary(mult.log3)$coefficients/summary(mult.log3)$standard.errors
```

    ## Warning in sqrt(diag(vc)): NaNs produced

    ## Warning in sqrt(diag(vc)): NaNs produced

    ##                  (Intercept)   Age_day AnimalTypeDog SexuponOutcomeIntact Male
    ## Euthanasia/Death   -60.52484 10.386107     3.4351747                  9.146124
    ## Return_to_owner    -66.45555 10.905297    10.1736656                 10.757406
    ## Transfer            17.39753  3.650398     0.3164629                  1.724810
    ##                  SexuponOutcomeNeutered Male SexuponOutcomeSpayed Female
    ## Euthanasia/Death                   -20.67323                   -19.35506
    ## Return_to_owner                    -13.96162                   -14.26189
    ## Transfer                           -32.30526                   -33.80979
    ##                  SexuponOutcomeUnknown  ColorApricot ColorBlack ColorBlue
    ## Euthanasia/Death          1.155007e+02           NaN  29.180484 57.414871
    ## Return_to_owner          -4.076568e+07 -4.074230e+21  40.480165 37.628553
    ## Transfer                  1.046627e+02  1.405046e+18   4.663984  1.798416
    ##                  ColorBrown     ColorBuff ColorCalico ColorChocolate ColorCream
    ## Euthanasia/Death  33.440776 -6.872590e+14   146.87834    647.6079453  201.35933
    ## Return_to_owner   37.572769  5.025587e+02   583.90865    196.1403367  295.25188
    ## Transfer           6.119818  5.667635e+01    14.27992     -0.7406503   13.79673
    ##                  ColorFawn    ColorFlame ColorGold ColorGray    ColorLilac
    ## Euthanasia/Death 1590.9271           NaN 1104.8298 1697.4776 -3.149569e+10
    ## Return_to_owner  1354.0197 -6.035807e+19  670.7372  574.7257 -3.441730e+11
    ## Transfer          203.9895 -3.604717e+02  386.2095  104.6367  7.657053e+02
    ##                     ColorLiver     ColorLynx ColorOrange ColorPink   ColorRed
    ## Euthanasia/Death -8.936761e+08 -1.801727e+17   64.108998       NaN 673.980352
    ## Return_to_owner  -1.427176e+15 -8.770727e+19   72.834246         0  33.123008
    ## Transfer         -3.934812e+15 -1.999598e+01    3.098965       NaN   6.481163
    ##                  ColorRuddy    ColorSable     ColorSeal   ColorSilver  ColorTan
    ## Euthanasia/Death          0 -3.141074e+16  1.361177e+03 -4.006424e+10 83.593636
    ## Return_to_owner           0  5.489539e+02 -6.327949e+15  8.591603e+03 32.898962
    ## Transfer                NaN -2.388170e+01  4.210554e+01 -3.333607e+14  3.010927
    ##                    ColorTorbie   ColorTortie ColorTricolor ColorWhite
    ## Euthanasia/Death -4.894896e+13  3.966798e+02    315.632634  43.015088
    ## Return_to_owner   6.722351e+02 -1.776221e+09     59.231802  35.241909
    ## Transfer          4.244757e+01  1.346589e+01      6.907247   1.834881
    ##                  ColorYellow
    ## Euthanasia/Death   714.93628
    ## Return_to_owner    449.20680
    ## Transfer           -88.25222

``` r
summary(mult.log3)
```

    ## Warning in sqrt(diag(vc)): NaNs produced

    ## Call:
    ## multinom(formula = y ~ Age_day + AnimalType + SexuponOutcome + 
    ##     Color, data = train)
    ## 
    ## Coefficients:
    ##                  (Intercept)      Age_day AnimalTypeDog
    ## Euthanasia/Death   -7.551560 0.0010388531    0.61707996
    ## Return_to_owner    -8.090031 0.0008946271    1.84537349
    ## Transfer            1.827416 0.0003165244    0.04618833
    ##                  SexuponOutcomeIntact Male SexuponOutcomeNeutered Male
    ## Euthanasia/Death                  1.104201                   -4.064708
    ## Return_to_owner                   1.241488                   -1.532124
    ## Transfer                          0.183912                   -3.938612
    ##                  SexuponOutcomeSpayed Female SexuponOutcomeUnknown ColorApricot
    ## Euthanasia/Death                   -4.069248            14.3767226    -1.818718
    ## Return_to_owner                    -1.647193            -0.5379934    -5.555384
    ## Transfer                           -4.056607            13.0276917    34.469644
    ##                  ColorBlack ColorBlue ColorBrown   ColorBuff ColorCalico
    ## Euthanasia/Death  6.8876560 7.1891069  7.2815037 -19.4277330    8.196893
    ## Return_to_owner   6.5302862 6.4290648  6.5957573   6.2006227    5.753444
    ## Transfer          0.7380647 0.3494745  0.9770765   0.4548874    1.355233
    ##                  ColorChocolate ColorCream ColorFawn  ColorFlame ColorGold
    ## Euthanasia/Death      6.4491599   8.571941  7.889346 -27.3937507  9.579774
    ## Return_to_owner       5.8760834   6.876296  5.788878 -31.2134564  7.737034
    ## Transfer             -0.0148429   1.358014  1.861295  -0.9443216  2.234361
    ##                  ColorGray ColorLilac ColorLiver   ColorLynx ColorOrange
    ## Euthanasia/Death  7.189611  -7.609346  -4.060515 -26.3291835   7.2930559
    ## Return_to_owner   6.029192 -10.884093 -18.866032 -33.0235689   7.0671331
    ## Transfer          1.362270   1.928345 -25.344064  -0.2341461   0.6968038
    ##                  ColorPink ColorRed ColorRuddy ColorSable   ColorSeal
    ## Euthanasia/Death         0 7.010661          0 -22.348847   7.7119050
    ## Return_to_owner          0 6.563864          0   6.934747 -22.0404604
    ## Transfer                 0 1.075193          0  -0.150466   0.4058756
    ##                  ColorSilver ColorTan ColorTorbie ColorTortie ColorTricolor
    ## Euthanasia/Death   -7.860060 7.259924 -16.6569473   6.9966088     7.5415571
    ## Return_to_owner     7.362588 6.454538   6.9885087  -9.0545034     5.7953251
    ## Transfer          -23.173998 0.518194   0.6799658   0.6179905     0.7410892
    ##                  ColorWhite ColorYellow
    ## Euthanasia/Death  6.5698263   6.3350089
    ## Return_to_owner   6.2194741   5.3446299
    ## Transfer          0.3351899  -0.4697123
    ## 
    ## Std. Errors:
    ##                  (Intercept)      Age_day AnimalTypeDog
    ## Euthanasia/Death   0.1247679 1.000233e-04     0.1796357
    ## Return_to_owner    0.1217360 8.203601e-05     0.1813873
    ## Transfer           0.1050389 8.670957e-05     0.1459518
    ##                  SexuponOutcomeIntact Male SexuponOutcomeNeutered Male
    ## Euthanasia/Death                 0.1207289                   0.1966169
    ## Return_to_owner                  0.1154078                   0.1097383
    ## Transfer                         0.1066274                   0.1219186
    ##                  SexuponOutcomeSpayed Female SexuponOutcomeUnknown ColorApricot
    ## Euthanasia/Death                   0.2102421          1.244730e-01          NaN
    ## Return_to_owner                    0.1154961          1.319721e-08 1.363542e-21
    ## Transfer                           0.1199832          1.244731e-01 2.453276e-17
    ##                  ColorBlack ColorBlue ColorBrown    ColorBuff ColorCalico
    ## Euthanasia/Death  0.2360364 0.1252133  0.2177433 2.826843e-14 0.055807369
    ## Return_to_owner   0.1613206 0.1708560  0.1755462 1.233811e-02 0.009853329
    ## Transfer          0.1582477 0.1943235  0.1596578 8.026053e-03 0.094904850
    ##                  ColorChocolate ColorCream   ColorFawn   ColorFlame   ColorGold
    ## Euthanasia/Death    0.009958432 0.04257037 0.004958961          NaN 0.008670815
    ## Return_to_owner     0.029958567 0.02328959 0.004275327 5.171381e-19 0.011535121
    ## Transfer            0.020040363 0.09843015 0.009124461 2.619683e-03 0.005785360
    ##                    ColorGray   ColorLilac   ColorLiver    ColorLynx ColorOrange
    ## Euthanasia/Death 0.004235468 2.415996e-10 4.543609e-09 1.461330e-16  0.11376025
    ## Return_to_owner  0.010490555 3.162391e-11 1.321913e-14 3.765203e-19  0.09703036
    ## Transfer         0.013019045 2.518391e-03 6.440985e-15 1.170966e-02  0.22485052
    ##                     ColorPink   ColorRed   ColorRuddy   ColorSable    ColorSeal
    ## Euthanasia/Death          NaN 0.01040188 2.998085e-17 7.115033e-16 5.665616e-03
    ## Return_to_owner  7.138151e-26 0.19816629 2.078176e-26 1.263266e-02 3.483034e-15
    ## Transfer         0.000000e+00 0.16589510 0.000000e+00 6.300475e-03 9.639483e-03
    ##                   ColorSilver  ColorTan  ColorTorbie  ColorTortie ColorTricolor
    ## Euthanasia/Death 1.961864e-10 0.0868478 3.402922e-13 1.763793e-02    0.02389346
    ## Return_to_owner  8.569516e-04 0.1961928 1.039593e-02 5.097623e-09    0.09784144
    ## Transfer         6.951628e-14 0.1721044 1.601896e-02 4.589303e-02    0.10729153
    ##                  ColorWhite ColorYellow
    ## Euthanasia/Death  0.1527331 0.008860942
    ## Return_to_owner   0.1764795 0.011897928
    ## Transfer          0.1826766 0.005322386
    ## 
    ## Residual Deviance: 2754.794 
    ## AIC: 2946.794

``` r
AIC(mult.log1)
```

    ## [1] 3553.816

``` r
AIC(mult.log2)
```

    ## [1] 2871.383

``` r
AIC(mult.log3)
```

    ## [1] 2946.794

``` r
BIC(mult.log1)
```

    ## [1] 3602.216

``` r
BIC(mult.log2)
```

    ## [1] 2984.316

``` r
BIC(mult.log3)
```

    ## [1] 3463.059

Since Logistic Regression uses MLE for estimating model parameters, AIC
would be a great measure to be used to compare our models. We noticed
that the model with all three independent variables,
Age_day+AnimalType+SexuponOutcome, has in fact lower AIC than the model
with only two of those variables and thus provides better model fit. In
addition, we see that the model with Color as a variable doesn’t also
perform the best, which probably could be due to the fact that it has 29
categories and using it as a factor in our model seems like adding 29
variables in our model and both AIC and BIC punish our model for using
so many variables and extra complexity.

Thus, mult.log2 with 3 variables performed the best in terms of AIC and
BIC, and most of its parameters were significant. So far model 2 was our
best model, but we wanted to check its predictive performance as well to
make sure it is our best model, so we compared the confusion matrix and
error rates of all three models in the next part.

2.  Obtain the confusion matrix and compute the **testing error rate**
    based on the logistic regression classification.

``` r
# Confusion matrix for model 1

logprob1<-predict(mult.log1, newdata=test)
logprob1=as.factor(logprob1)
confusionMatrix(data=logprob1, reference=test$y)
```

    ## Confusion Matrix and Statistics
    ## 
    ##                   Reference
    ## Prediction         Adoption Euthanasia/Death Return_to_owner Transfer
    ##   Adoption               79                9              32       48
    ##   Euthanasia/Death        3                1               1        0
    ##   Return_to_owner        17                5              32        5
    ##   Transfer               62               11               7       88
    ## 
    ## Overall Statistics
    ##                                           
    ##                Accuracy : 0.5             
    ##                  95% CI : (0.4499, 0.5501)
    ##     No Information Rate : 0.4025          
    ##     P-Value [Acc > NIR] : 5.009e-05       
    ##                                           
    ##                   Kappa : 0.2373          
    ##                                           
    ##  Mcnemar's Test P-Value : 0.0006805       
    ## 
    ## Statistics by Class:
    ## 
    ##                      Class: Adoption Class: Euthanasia/Death
    ## Sensitivity                   0.4907                 0.03846
    ## Specificity                   0.6276                 0.98930
    ## Pos Pred Value                0.4702                 0.20000
    ## Neg Pred Value                0.6466                 0.93671
    ## Prevalence                    0.4025                 0.06500
    ## Detection Rate                0.1975                 0.00250
    ## Detection Prevalence          0.4200                 0.01250
    ## Balanced Accuracy             0.5591                 0.51388
    ##                      Class: Return_to_owner Class: Transfer
    ## Sensitivity                          0.4444          0.6241
    ## Specificity                          0.9177          0.6911
    ## Pos Pred Value                       0.5424          0.5238
    ## Neg Pred Value                       0.8827          0.7716
    ## Prevalence                           0.1800          0.3525
    ## Detection Rate                       0.0800          0.2200
    ## Detection Prevalence                 0.1475          0.4200
    ## Balanced Accuracy                    0.6811          0.6576

``` r
# Confusion matrix for model 2

logprob2<-predict(mult.log2, newdata=test)
logprob2=as.factor(logprob2)
confusionMatrix(data=logprob2, reference=test$y)
```

    ## Confusion Matrix and Statistics
    ## 
    ##                   Reference
    ## Prediction         Adoption Euthanasia/Death Return_to_owner Transfer
    ##   Adoption              136                5              32       47
    ##   Euthanasia/Death        0                2               3        1
    ##   Return_to_owner        20                3              27        4
    ##   Transfer                5               16              10       89
    ## 
    ## Overall Statistics
    ##                                           
    ##                Accuracy : 0.635           
    ##                  95% CI : (0.5857, 0.6823)
    ##     No Information Rate : 0.4025          
    ##     P-Value [Acc > NIR] : < 2.2e-16       
    ##                                           
    ##                   Kappa : 0.4364          
    ##                                           
    ##  Mcnemar's Test P-Value : 1.448e-10       
    ## 
    ## Statistics by Class:
    ## 
    ##                      Class: Adoption Class: Euthanasia/Death
    ## Sensitivity                   0.8447                 0.07692
    ## Specificity                   0.6485                 0.98930
    ## Pos Pred Value                0.6182                 0.33333
    ## Neg Pred Value                0.8611                 0.93909
    ## Prevalence                    0.4025                 0.06500
    ## Detection Rate                0.3400                 0.00500
    ## Detection Prevalence          0.5500                 0.01500
    ## Balanced Accuracy             0.7466                 0.53311
    ##                      Class: Return_to_owner Class: Transfer
    ## Sensitivity                          0.3750          0.6312
    ## Specificity                          0.9177          0.8803
    ## Pos Pred Value                       0.5000          0.7417
    ## Neg Pred Value                       0.8699          0.8143
    ## Prevalence                           0.1800          0.3525
    ## Detection Rate                       0.0675          0.2225
    ## Detection Prevalence                 0.1350          0.3000
    ## Balanced Accuracy                    0.6463          0.7558

``` r
# Confusion matrix for model 3

logprob3<-predict(mult.log3, newdata=test)
logprob3=as.factor(logprob3)
confusionMatrix(data=logprob3, reference=test$y)
```

    ## Confusion Matrix and Statistics
    ## 
    ##                   Reference
    ## Prediction         Adoption Euthanasia/Death Return_to_owner Transfer
    ##   Adoption              134                5              32       45
    ##   Euthanasia/Death        1                2               2        1
    ##   Return_to_owner        21                3              27        6
    ##   Transfer                5               16              11       89
    ## 
    ## Overall Statistics
    ##                                           
    ##                Accuracy : 0.63            
    ##                  95% CI : (0.5806, 0.6774)
    ##     No Information Rate : 0.4025          
    ##     P-Value [Acc > NIR] : < 2.2e-16       
    ##                                           
    ##                   Kappa : 0.4302          
    ##                                           
    ##  Mcnemar's Test P-Value : 1.994e-09       
    ## 
    ## Statistics by Class:
    ## 
    ##                      Class: Adoption Class: Euthanasia/Death
    ## Sensitivity                   0.8323                 0.07692
    ## Specificity                   0.6569                 0.98930
    ## Pos Pred Value                0.6204                 0.33333
    ## Neg Pred Value                0.8533                 0.93909
    ## Prevalence                    0.4025                 0.06500
    ## Detection Rate                0.3350                 0.00500
    ## Detection Prevalence          0.5400                 0.01500
    ## Balanced Accuracy             0.7446                 0.53311
    ##                      Class: Return_to_owner Class: Transfer
    ## Sensitivity                          0.3750          0.6312
    ## Specificity                          0.9085          0.8764
    ## Pos Pred Value                       0.4737          0.7355
    ## Neg Pred Value                       0.8688          0.8136
    ## Prevalence                           0.1800          0.3525
    ## Detection Rate                       0.0675          0.2225
    ## Detection Prevalence                 0.1425          0.3025
    ## Balanced Accuracy                    0.6418          0.7538

``` r
# Testing error rate for model 1
round( mean(logprob1!=test[,"y"]),4)
```

    ## [1] 0.5

``` r
# Testing error rate for model 2
round( mean(logprob2!=test[,"y"]),4)
```

    ## [1] 0.365

``` r
# Testing error rate for model 3
round( mean(logprob3!=test[,"y"]),4)
```

    ## [1] 0.37

All of the confusion matrices managed to predict our variable of
interest at least once correctly, which is a good sign of using Logistic
Regression in our models. When comparing testing error rates of our
models, we see that model 2 is more accurate than other models. Its
accuracy is not much different from model 3, but considering the fact
that model 2 had the best AIC and BIC and is also much simpler than
model 3, we can say that it definitely works better.

3.  Explain your choices and communicate your results.

We were interested in running a Logistic Regression model which could
predict what would happen to an animal based on its characteristics. We
created our model with some animal’s characteristics like type, age,
sex, and color, and then based on the data we already have, we can see
how effectively our model predicts animals’ destiny at the shelter.
Logistic Regression predicts the probability of that animal ending up
with different outcomes, like how much is the probability of that animal
getting adopted or dying while it is at the shelter? For each animal,
the model calculates the probabilities of each of those outcomes and
then among those probabilities, chooses the highest one and assigns that
animal to that outcome. Since we already have the data for our animals,
we then compare the result of model prediction with the actual outcome
that happened to that animal. We then calculate the percentage of times
our model predicted correctly and call it our model accuracy.

In this project, we built 3 models with different combinations of animal
type, age, sex, and color, so that we can choose the one with the best
predictive functionality. Among our models, model 2 had lower predictive
error (higher accuracy) while it had a medium number of variables. We
like our models to have as lowest number of variables as possible so
that they are not too complex. We also checked some other factors of
model performance, called AIC and BIC, in which model 2 worked the best,
so we chose model 2 as our best logistic regression model.

## Part 3: KNN

1.  Apply a KNN classification to the training data.

A KNN classification uses a certain number (K) of nearest neighbors to
determine how to classify a data point. This is done by calculating the
(Euclidean) distance between a data point and the other points, and then
taking a majority vote from the nearest K points, and classifying the
data accordingly.

The predictors used for KNN must be numeric, so we use Age and make
dummy variables of two other variables as independent variables in our
model. We chose animal type and sex upon outcome to be converted to
dummy variables and be used alongside the age in our model.

Adding other variables, like color, as dummy variables to our model
meant that we were adding 29 dummy variables to it. Thus, our model had
to go though 29 more dimensions and we got dimension errors in practice
and KNN didn’t work.

Using breed instead of color wouldn’t help either, because we had more
than 1,000 types of breeds and KNN model couldn’t handle 29 more
dimensions from color, let it be 1,000 more from breeds.

Thus, we used age, type, and sex in our KNN model. It’s worth mentioning
that we created new train and test sets for this question to add dummy
variables to them, but we used previous train and test sets to create
these ones, so in practice, we are using the same train and test
observations for every model in this project.

In order to determine the best order for our model (K), we ran a loop to
calculate the train error rates for different K’s and choose the one
with the minimum error. We then ran a KNN model using our best K.

``` r
# Creating dummy variables 

train_dummy <- dummy_cols(train, select_columns = 'AnimalType')
train_dummy <- dummy_cols(train_dummy, select_columns = 'SexuponOutcome')

test_dummy <- dummy_cols(test, select_columns = 'AnimalType')
test_dummy <- dummy_cols(test_dummy, select_columns = 'SexuponOutcome')
```

``` r
set.seed(17)

knn.train= train_dummy[,10:17]
knn.test=test_dummy[,10:17]
knn.trainLabels=train_dummy$y
knn.testLabels=test_dummy$y

# Determine k

k.grid=1:100
error=rep(0, length(k.grid))

for (i in seq_along(k.grid)) {
  pred = knn(train = scale(knn.train), 
             test  = scale(knn.test), 
             cl    = knn.trainLabels, 
             k     = k.grid[i])
  error[i] = mean(knn.testLabels !=pred)
}

min(error)
```

    ## [1] 0.355

``` r
which(error==min(error)) 
```

    ## [1] 38 39

``` r
# With our seed, the minimum error we get using KNN model is with k=38

#KNN model with k=38

knn38 <- knn(train = data.frame(knn.train), test = data.frame(knn.test), cl = knn.trainLabels, k=38)

plot(knn38)
```

![](Statistical_Learning_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

The best K which resulted in the lowest error was from K=38 or K=39, we
chose the smaller K and ran a KNN model with K=38. (Note that while
there is some benefit to having an odd K, this benefit is greatest with
a binary classification problem, and our problem is multi-class.)

The above plot shows the values our model predicted. We can see that KNN
predicted at least one observation to belong to our minority group,
animals who die or get euthanized, but we don’t necessarily know if
those predictions are correct, which is why we obtain the confusion
matrix in the next part. It is also worth mentioning that the minimum
error rate which the above loop for K=38 promises belongs to the train
set and we don’t necessarily get the same error rate on the test set,
which is also why we calculate the testing error rate in the next part.

2.  Obtain the confusion matrix and compute the testing error rate based
    on the KNN classification.

``` r
# Confusion matrix and the testing error rate, although we already knew from our loop that how much our error would be using this KNN

table(knn38, knn.testLabels)
```

    ##                   knn.testLabels
    ## knn38              Adoption Euthanasia/Death Return_to_owner Transfer
    ##   Adoption              124               11              32       57
    ##   Euthanasia/Death        0                0               0        0
    ##   Return_to_owner        31                7              36       15
    ##   Transfer                6                8               4       69

``` r
mean(knn38!=knn.testLabels)
```

    ## [1] 0.4275

As we can see, our testing error is 42.5% which is more than 35.5% error
rate which k-loop promised, because the initial error had belonged to
the training set.

By looking at the confusion matrix, we notice that our model assigned
some observations to our minority group, but it always did it wrong. On
one hand we are happy that it acknowledged the existence of that
category, on the other hand we wished it was more accurate and could
predict them correctly.

3.  Explain your choices and communicate your results.

For this part of our project, we wanted to develop a model using KNN,
K-nearest neighbors, to predict what outcome would animals which get
brought to the shelters will have. What this model does is that it
calculates the difference between each variable of that animal with the
same variables of other animals, calculates distance between them using
those differences, and then sorts other animals based on their distance
from our desired animal. It then chooses the closest k neighbors and
looks at what outcome the majority of those neighbors had, and assigns
it to our desired animal. Basically, it says whatever happens to the
majority of an animal’s k neighbors would happen to it as well.

Then the question is, how much should our k be? In order to choose our
k, we ran a loop which assigns numbers from 1 to 100 to k and calculates
the prediction error, and then announces which k resulted to the lowest
error in our model. In our case, k=38 and k=39 resulted to the lowest
error, but we chose k=38 because we prefer our model to be as simple as
possible.

We should pay attention to the fact that at the beginning of our
project, we split our data into two sets, one is called train set, which
we use to build our model upon, and the other one is test set, which we
use to check our model’s accuracy. The error that loop tried to minimize
belonged to the train set, because we were still in the process of
building our model. Thus, we still need to try our model on our test set
to see how much its accuracy really is.

When we run our model on our test set, we see that our testing error
rate is 42.75%, which more than the testing error rate we got previously
from our Logistic Regression model. In addition, by looking at our
confusion matrix, we can see that our model didn’t predict correctly any
of the cases which died or got euthanized, which is the weakness of this
model, because we want our model to be able to correctly predict our
minority group which happens to be our group of interest as well.

## Part 4: Tree Based Model

1.  Apply one of the following models to your training data:
    *Classification Tree, Random Forest, Bagging or Boosting*

Tree-based models share the commonality of classification trees. In a
classification tree model, data is classified using a set of rules. Each
decision node splits the data, eventually at the end of the tree
(leaves) the model assigns a class to the data in that group.

While classification trees are very useful and interpretable on their
own, we wanted to practice more with additional tree-based models, as
the other tree-based models are typically known for having better
predictive accuracy.

First, we wanted to try the random forest model, which is as the name
implies: the model uses a collection of trees (forest) and these trees
are generated using some randomness. The randomness is used to select
some of the predictors for each decision node of the tree. Additionally,
each tree is created using a random portion of data (bootstrap sample)
from the training set, which increases the randomness and variety
between the trees in the forest.

To classify each data point, the random forest model will then take a
majority vote of the results for that data point from all the trees in
the forest.

``` r
# Random Forest

set.seed(17)

# possible values for mtry and ntree
mtry = seq(2, ncol(train) * 0.8, 2)
ntree = seq(400, 600, 20)

# prepare for loop
hyper_grid <- expand.grid(mtry = mtry, ntree = ntree)
oob_err <- c()

# train the grid
for (i in 1:nrow(hyper_grid)) {

    # Train a Random Forest model
    model <- randomForest(formula = as.factor(y) ~ Age_day+AnimalType+SexuponOutcome , 
                          data = train,
                          mtry = hyper_grid$mtry[i],
                          ntree= hyper_grid$ntree[i])
                          
    # Store OOB error for the model                      
    oob_err[i] <- model$err.rate[nrow(model$err.rate), "OOB"]
}

# choose hyperparameters using OOB error
opt_i <- which.min(oob_err)
print(hyper_grid[opt_i,])
```

    ##   mtry ntree
    ## 9    2   440

``` r
# try RF with tuned parameters
model.rf=randomForest(as.factor(y)~Age_day+AnimalType+SexuponOutcome, data = train , importance=TRUE,proximity=TRUE, mtry=2, ntree=440)

print(model.rf)
```

    ## 
    ## Call:
    ##  randomForest(formula = as.factor(y) ~ Age_day + AnimalType +      SexuponOutcome, data = train, importance = TRUE, proximity = TRUE,      mtry = 2, ntree = 440) 
    ##                Type of random forest: classification
    ##                      Number of trees: 440
    ## No. of variables tried at each split: 2
    ## 
    ##         OOB estimate of  error rate: 35.75%
    ## Confusion matrix:
    ##                  Adoption Euthanasia/Death Return_to_owner Transfer class.error
    ## Adoption              537                4              53       51   0.1674419
    ## Euthanasia/Death       20               14              26       45   0.8666667
    ## Return_to_owner       133                8             118       28   0.5888502
    ## Transfer              146               12              46      359   0.3623446

``` r
head(model.rf$importance)
```

    ##                  Adoption Euthanasia/Death Return_to_owner   Transfer
    ## Age_day        0.11665251       0.06529845      0.18982377 0.08805818
    ## AnimalType     0.07004937       0.02305580      0.07955120 0.01592133
    ## SexuponOutcome 0.21753303       0.07483775      0.04641719 0.27073360
    ##                MeanDecreaseAccuracy MeanDecreaseGini
    ## Age_day                  0.11606667        203.78183
    ## AnimalType               0.04952664         31.54006
    ## SexuponOutcome           0.19603101        219.93249

``` r
# OOB model error
err= model.rf$err.rate
head(err)
```

    ##            OOB  Adoption Euthanasia/Death Return_to_owner  Transfer
    ## [1,] 0.4169550 0.1508621        0.9318182       0.8252427 0.4020101
    ## [2,] 0.3942614 0.1631579        0.8461538       0.7411765 0.3926380
    ## [3,] 0.3986429 0.1909871        0.8072289       0.7102804 0.3894231
    ## [4,] 0.3835821 0.1853933        0.8111111       0.6735537 0.3776371
    ## [5,] 0.3777778 0.1822917        0.7956989       0.6666667 0.3762183
    ## [6,] 0.3793333 0.1638796        0.8061224       0.7259259 0.3670412

``` r
# plot the model
plot(model.rf)
```

![](Statistical_Learning_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

We also applied a boosting model, which is also tree-based. Similarly to
random forest, boosting also grows many trees in order to classify the
data. However, where random forest relies on randomness to grow
subsequent trees, boosting instead learns slowly from the previously
generated trees. Here we have used a Gradient Boosting Machine (GBM)
which is known for a high success rate if correctly tuned. This model
combines the tree-based boosting concept with gradient descent
optimization to learn and find the best classification.

``` r
set.seed(17)
# also we did a boosting model
Model1 = gbm(as.factor(y)~Age_day+AnimalType+SexuponOutcome,data=train,
         distribution='multinomial', 
         n.trees=200,
         cv.folds=5)

print(Model1)
```

    ## gbm(formula = as.factor(y) ~ Age_day + AnimalType + SexuponOutcome, 
    ##     distribution = "multinomial", data = train, n.trees = 200, 
    ##     cv.folds = 5)
    ## A gradient boosted model with multinomial loss function.
    ## 200 iterations were performed.
    ## The best cross-validation iteration was 103.
    ## There were 3 predictors of which 3 had non-zero influence.

``` r
summary(Model1)
```

![](Statistical_Learning_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->

    ##                           var   rel.inf
    ## SexuponOutcome SexuponOutcome 53.284760
    ## Age_day               Age_day 42.019043
    ## AnimalType         AnimalType  4.696197

2.  Obtain the confusion matrix and compute the testing error rate based
    on your chosen tree based model.

For both the random forest model and the boosting model, we computed
predictions and outputted their confusion matrices, as well as the
testing error rate. This will help us assess the predictive performance
of these models.

``` r
set.seed(17)
# predictions and confusion matrix for random forest
pred.rf= predict(model.rf, newdata = test, type = "class")
confusionMatrix(data = pred.rf, reference = test$y)
```

    ## Confusion Matrix and Statistics
    ## 
    ##                   Reference
    ## Prediction         Adoption Euthanasia/Death Return_to_owner Transfer
    ##   Adoption              127                7              27       41
    ##   Euthanasia/Death        0                1               3        3
    ##   Return_to_owner        23                6              36       11
    ##   Transfer               11               12               6       86
    ## 
    ## Overall Statistics
    ##                                           
    ##                Accuracy : 0.625           
    ##                  95% CI : (0.5755, 0.6726)
    ##     No Information Rate : 0.4025          
    ##     P-Value [Acc > NIR] : < 2.2e-16       
    ##                                           
    ##                   Kappa : 0.4319          
    ##                                           
    ##  Mcnemar's Test P-Value : 1.309e-05       
    ## 
    ## Statistics by Class:
    ## 
    ##                      Class: Adoption Class: Euthanasia/Death
    ## Sensitivity                   0.7888                 0.03846
    ## Specificity                   0.6862                 0.98396
    ## Pos Pred Value                0.6287                 0.14286
    ## Neg Pred Value                0.8283                 0.93639
    ## Prevalence                    0.4025                 0.06500
    ## Detection Rate                0.3175                 0.00250
    ## Detection Prevalence          0.5050                 0.01750
    ## Balanced Accuracy             0.7375                 0.51121
    ##                      Class: Return_to_owner Class: Transfer
    ## Sensitivity                          0.5000          0.6099
    ## Specificity                          0.8780          0.8880
    ## Pos Pred Value                       0.4737          0.7478
    ## Neg Pred Value                       0.8889          0.8070
    ## Prevalence                           0.1800          0.3525
    ## Detection Rate                       0.0900          0.2150
    ## Detection Prevalence                 0.1900          0.2875
    ## Balanced Accuracy                    0.6890          0.7490

``` r
# testing error for random forest
ce(factor(pred.rf), factor(test$y))
```

    ## [1] 0.375

``` r
# predictions for boosting
pModel1 = predict(Model1,n.trees=200, newdata=test,type='response')
pModel1.scaled=apply(pModel1,1, which.max)

# convert boosting predictions
boost_pred <- pModel1.scaled
boost_pred[boost_pred==1]<-"Adoption"
boost_pred[boost_pred==2]<-"Euthanasia/Death"
boost_pred[boost_pred==3]<-"Return_to_owner"
boost_pred[boost_pred==4]<-"Transfer"

# output confusion matrix and testing error rate for boosting
confusionMatrix(factor(boost_pred), factor(test$y))
```

    ## Confusion Matrix and Statistics
    ## 
    ##                   Reference
    ## Prediction         Adoption Euthanasia/Death Return_to_owner Transfer
    ##   Adoption              135                6              31       41
    ##   Euthanasia/Death        0                2               2        2
    ##   Return_to_owner        21                2              30       10
    ##   Transfer                5               16               9       88
    ## 
    ## Overall Statistics
    ##                                           
    ##                Accuracy : 0.6375          
    ##                  95% CI : (0.5883, 0.6847)
    ##     No Information Rate : 0.4025          
    ##     P-Value [Acc > NIR] : < 2.2e-16       
    ##                                           
    ##                   Kappa : 0.4443          
    ##                                           
    ##  Mcnemar's Test P-Value : 1.838e-08       
    ## 
    ## Statistics by Class:
    ## 
    ##                      Class: Adoption Class: Euthanasia/Death
    ## Sensitivity                   0.8385                 0.07692
    ## Specificity                   0.6736                 0.98930
    ## Pos Pred Value                0.6338                 0.33333
    ## Neg Pred Value                0.8610                 0.93909
    ## Prevalence                    0.4025                 0.06500
    ## Detection Rate                0.3375                 0.00500
    ## Detection Prevalence          0.5325                 0.01500
    ## Balanced Accuracy             0.7561                 0.53311
    ##                      Class: Return_to_owner Class: Transfer
    ## Sensitivity                          0.4167          0.6241
    ## Specificity                          0.8994          0.8842
    ## Pos Pred Value                       0.4762          0.7458
    ## Neg Pred Value                       0.8754          0.8121
    ## Prevalence                           0.1800          0.3525
    ## Detection Rate                       0.0750          0.2200
    ## Detection Prevalence                 0.1575          0.2950
    ## Balanced Accuracy                    0.6580          0.7541

``` r
1-mean(test[,"y"]==boost_pred)
```

    ## [1] 0.3625

3.  Explain your choices and communicate your results.

For the random forest model, we used hyperparameter tuning to determine
the values to input to the model for mtry (the number of variables to
use at each split) and ntree (the number of trees we want in the
forest). We chose to only tune a couple of the hyperparameters to
hopefully avoid overtuning the model. Using the minimum error from this
tuning (called the out of bag error, which is calculated using data that
was not included in each tree’s random sample), we prefer mtry of 2 and
ntree of 440, and we used these values to create our random forest
model.

For the boosting model, we ran a 5-fold cross-validated model using 200
trees. The results of this model also include the relative importance of
the variables, which show that sex upon outcome has the highest
importance, followed by age, and then animal type. This result is not
surprising given the initial graphs for sex upon outcome from our data
exploration, which have noticeable differences for each outcome class.

After creating the models, we generated predictions for both, and used
these predictions along with the testing data to create confusion
matrices and testing error rates for each model, which allows us to
evaluate their effectiveness at predicting data they have not seen
before.

As we can see from the output above, the random forest model has a
testing error of 37.5%, and it also managed to predict our minority
group correctly once. However, our boosting model not only had a lower
testing error of 36.25%, but also predicted the outcome of two animals
which belonged to our minority group correctly. Thus, we believe that
boosting performed better on our data than random forest.

## Part 5: SVM

1.  Apply a SVM model to your training data.

In this part, we create a Support Vector Machine (SVM) model. The
underlying concept of SVM is to create a line to classify data. In
multiple dimensions, this would use a hyperplane (which works like a
multidimensional “line”) to classify data.

``` r
set.seed(17)

# Tuning the SVM
tune.svm=tune(svm,y ~ Age_day+AnimalType+SexuponOutcome, data = train, type = "C-classification", kernel = "radial", ranges =list(cost=c(0.001 , 0.01, 0.1, 1,5,10,100)))

# The lowest error is for cost=1
summary(tune.svm)
```

    ## 
    ## Parameter tuning of 'svm':
    ## 
    ## - sampling method: 10-fold cross validation 
    ## 
    ## - best parameters:
    ##  cost
    ##     1
    ## 
    ## - best performance: 0.348125 
    ## 
    ## - Detailed performance results:
    ##    cost    error dispersion
    ## 1 1e-03 0.596875 0.04480378
    ## 2 1e-02 0.596875 0.04480378
    ## 3 1e-01 0.360625 0.04545774
    ## 4 1e+00 0.348125 0.04310537
    ## 5 5e+00 0.350000 0.04649149
    ## 6 1e+01 0.351250 0.04998264
    ## 7 1e+02 0.348750 0.05462943

``` r
# now tune gamma
tune.radial=tune(svm, y ~ Age_day+AnimalType+SexuponOutcome, data=train,
                 kernel ="radial", 
                 type = "C-classification",
                 cost=1,
                 ranges =list(gamma=c(0.1,0.2,0.3,0.4,0.5,1,2,3)))

# the lowest error is gamma 0.2 and 0.4
summary(tune.radial)
```

    ## 
    ## Parameter tuning of 'svm':
    ## 
    ## - sampling method: 10-fold cross validation 
    ## 
    ## - best parameters:
    ##  gamma
    ##    0.3
    ## 
    ## - best performance: 0.34625 
    ## 
    ## - Detailed performance results:
    ##   gamma    error dispersion
    ## 1   0.1 0.351250 0.04143687
    ## 2   0.2 0.350625 0.04044565
    ## 3   0.3 0.346250 0.03987829
    ## 4   0.4 0.346875 0.03671044
    ## 5   0.5 0.352500 0.04126894
    ## 6   1.0 0.353125 0.04272103
    ## 7   2.0 0.351875 0.04239468
    ## 8   3.0 0.350625 0.05003905

``` r
# try tuning cost and gamma together
tune.radial2=tune(svm, y ~ Age_day+AnimalType+SexuponOutcome, data=train,
                 kernel ="radial", 
                 type = "C-classification",
                 ranges =list(cost=c(0.1,1,10,100),
                              gamma=c(0.1,0.2,0.3,0.4,0.5)))

# the lowest error is gamma=0.4 and cost=10
summary(tune.radial2)
```

    ## 
    ## Parameter tuning of 'svm':
    ## 
    ## - sampling method: 10-fold cross validation 
    ## 
    ## - best parameters:
    ##  cost gamma
    ##     1   0.2
    ## 
    ## - best performance: 0.345625 
    ## 
    ## - Detailed performance results:
    ##     cost gamma    error dispersion
    ## 1    0.1   0.1 0.363750 0.03712778
    ## 2    1.0   0.1 0.350000 0.03620447
    ## 3   10.0   0.1 0.350000 0.03547789
    ## 4  100.0   0.1 0.350000 0.03410564
    ## 5    0.1   0.2 0.363125 0.03466732
    ## 6    1.0   0.2 0.345625 0.03536148
    ## 7   10.0   0.2 0.350000 0.03435921
    ## 8  100.0   0.2 0.350625 0.02786531
    ## 9    0.1   0.3 0.361875 0.03625838
    ## 10   1.0   0.3 0.346875 0.03538602
    ## 11  10.0   0.3 0.350625 0.03246927
    ## 12 100.0   0.3 0.349375 0.03025499
    ## 13   0.1   0.4 0.352500 0.03598804
    ## 14   1.0   0.4 0.346875 0.03413744
    ## 15  10.0   0.4 0.349375 0.03054056
    ## 16 100.0   0.4 0.351250 0.02278371
    ## 17   0.1   0.5 0.351875 0.03621047
    ## 18   1.0   0.5 0.348125 0.03307845
    ## 19  10.0   0.5 0.351250 0.03059026
    ## 20 100.0   0.5 0.352500 0.02622022

``` r
# our favorite SVM model based on tuning results
svm_model<- svm(y ~ Age_day+AnimalType+SexuponOutcome, data = train, type = "C-classification", 
                kernel = "radial", cost=1, gamma=0.2)

svm_model
```

    ## 
    ## Call:
    ## svm(formula = y ~ Age_day + AnimalType + SexuponOutcome, data = train, 
    ##     type = "C-classification", kernel = "radial", cost = 1, gamma = 0.2)
    ## 
    ## 
    ## Parameters:
    ##    SVM-Type:  C-classification 
    ##  SVM-Kernel:  radial 
    ##        cost:  1 
    ## 
    ## Number of Support Vectors:  1033

2.  Calculate the confusion matrix using the testing data.

Now that we have the model, we want to evaluate its predictive
performance, which we can do using the testing data and confusion
matrix.

``` r
# calculate predictions
pred_test <- predict(svm_model, test)

# Confusion matrix
confusionMatrix(data=pred_test, reference=test$y)
```

    ## Confusion Matrix and Statistics
    ## 
    ##                   Reference
    ## Prediction         Adoption Euthanasia/Death Return_to_owner Transfer
    ##   Adoption              139                6              32       47
    ##   Euthanasia/Death        0                0               0        0
    ##   Return_to_owner        17                4              30        5
    ##   Transfer                5               16              10       89
    ## 
    ## Overall Statistics
    ##                                           
    ##                Accuracy : 0.645           
    ##                  95% CI : (0.5959, 0.6919)
    ##     No Information Rate : 0.4025          
    ##     P-Value [Acc > NIR] : < 2.2e-16       
    ##                                           
    ##                   Kappa : 0.4485          
    ##                                           
    ##  Mcnemar's Test P-Value : 2.474e-12       
    ## 
    ## Statistics by Class:
    ## 
    ##                      Class: Adoption Class: Euthanasia/Death
    ## Sensitivity                   0.8634                   0.000
    ## Specificity                   0.6444                   1.000
    ## Pos Pred Value                0.6205                     NaN
    ## Neg Pred Value                0.8750                   0.935
    ## Prevalence                    0.4025                   0.065
    ## Detection Rate                0.3475                   0.000
    ## Detection Prevalence          0.5600                   0.000
    ## Balanced Accuracy             0.7539                   0.500
    ##                      Class: Return_to_owner Class: Transfer
    ## Sensitivity                          0.4167          0.6312
    ## Specificity                          0.9207          0.8803
    ## Pos Pred Value                       0.5357          0.7417
    ## Neg Pred Value                       0.8779          0.8143
    ## Prevalence                           0.1800          0.3525
    ## Detection Rate                       0.0750          0.2225
    ## Detection Prevalence                 0.1400          0.3000
    ## Balanced Accuracy                    0.6687          0.7558

``` r
# testing error
1-mean(pred_test == test$y)
```

    ## [1] 0.355

3.  Explain your choices and communicate your results.

We began by tuning the parameters for this model using the tune.svm
function, which uses 10-fold cross-validation to evaluate the parameters
we are interested in. Using the errors for the resulting models, we can
choose which parameters we want to use. First, we tuned the cost
parameter and got the best result of 1. Then, we used this cost to tune
the gamma parameter to get a best result of 0.2 and 0.4. We also tried
tuning both cost and gamma at the same time, which recommended to use a
cost of 10 and a gamma of 0.4, however when we created this model we
found its predictive performance noticeably worse, so we wonder if
tuning both parameters simultaneously resulted in overfitting.

Thus, we opted to use the cost=1 and gamma=0.2 parameters that we
obtained when tuning individually, as these result in a model with
higher predictive accuracy.

This model has several predictors, so we did not output the
two-dimensional SVM plot, as it would not be able to fully depict the
model’s workings.

Finally, we calculate the model’s predictions in the test set and output
the confusion matrix and testing error for this model, which we can use
to evaluate its performance. Reviewing the confusion matrix, we see that
the model does not predict the minority class (Euthanasia/Death) but
that it has reasonable accuracy in detecting the remaining classes, with
an overall accuracy of 64.5%, and thus a testing error of 35.5%.

## Part 6: Conclusion

1.Based on the different classification models, which one do you think
is the best model to predict `y`? Please consider the following in your
response:

    - Accuracy/error rates
    - Do you think you can improve the model by adding any other information?

One method of evaluating model performance is the testing error, which
represents the proportion of errors made when applying the model to data
beyond the scope of its training.

For testing error rates, the models measure up as follows:

Logistic Regression: 36.5%

KNN: 42.5%

Random Forest: 37.5%

Boosting: 36.25%

SVM: 35.5%

Using testing error rates alone, SVM appears to be the best model as it
has the least error. However, error is only one way to evaluate models.
In general, we would also want to consider the AUC (area under the
curve) on a ROC test when considering classification models, however
this is not feasible in R for a multi-class problem.

However, we are also very interested in the sensitivity (or true
positive rate) associated with our minority class Euthanasia/Death, as
predicting which pets may be at risk of death or euthanasia could be
particularly helpful in preventing these outcomes. Notice that even
though the SVM model has the lowest testing error rate, it has a
sensitivity in the Euthanasia/Death class of 0%, showing that it never
predicts the class correctly. Given this, when deciding our best
predictive model we may also want to take this metric into account.

Considering this metric, the number of correct observations belonging to
our minority group are as follows:

Logistic Regression 2

KNN 0

Random Forest 1

Boosting 2

SVM 0 (ignoring the existence of the whole category)

Thus, considering this factor with the lowest error rates, we can see
that Logistic Regression and Boosting are our best models.

Furthermore, we think understanding the importance of factors is useful
in this area, which is why we like logistic regression model more, as it
provides information regarding the significance of the predictors.

We believe having more numerical data could help us improve our models.
More numerical data could enable us to use SMOTE and KNN more
effectively, and might also help us apply LDA, which should be effective
for multiple outcome classes, and thus should be very helpful for this
type of analysis. We also believe that reducing the dimension of the
categorical variables we do have (like further reducing color, or
simplifying breed into categories) could help us include these variables
in our existing models more effectively, and that by doing so we would
be able to improve the models’ fit and accuracy.

2.  What are your learning outcomes for this assignment? Please focus on
    your learning outcomes in terms of statistical learning, model
    interpretations, and R skills - it is up to you to include this part
    in your presentation or not.

We feel that we have gained a lot from this project. Specifically, we
found this a great opportunity to consider whether the purpose of the
analysis would want to be to explain or to predict. Many of our previous
examples have had some clarity as to which option may be preferred, but
in this case we notice that there could be usefulness for both: to
understand which cases may be leading to negative outcomes so that the
shelter can prepare to prevent those outcomes in future, or to predict
which animals are at-risk of negative outcomes so that more care can be
taken in their individual cases to avoid worst-case outcomes.

Additionally, this assignment allowed us practice in deciding which
models could fit a dataset, and in also applying the statistical
learning models we studied to data. We encountered some challenges in
this regard, due to the multiple outcome classes in our dataset as well
as the many categorical variables included. These aspects of our data,
combined with the minority classes present, presented some limits to the
analysis. However, we are very grateful to encounter these limitations
as students to learn more about them and discover which techniques may
still be applicable, as we feel we have learned a lot from this
challenge.

Further, in tuning our models we also continued to practice finding the
line between fitting a model and overfitting a model, as we noticed that
(over)tuning the SVM model led to decreased accuracy of predictions.

Lastly, we practiced our interpretation and communication skills in
understanding the model results, presenting our work to the class, and
writing this report.

## References

\[1\] Kaggle Competition, Shelter Animal Outcomes:
<https://www.kaggle.com/competitions/shelter-animal-outcomes/overview>

\[2\] Austin Animal Center:
<https://www.austintexas.gov/department/about-austin-animal-center>
