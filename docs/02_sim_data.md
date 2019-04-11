
# Simulating Data {#sim_data}

This tutorial details a few ways I simulate data. I'll be using some functions from my [`faux` package](https://github.com/debruine/faux) to make it easier to generate sets of variables with specific correlations.


```r
library(tidyverse)
library(faux) # devtools::install_github("debruine/faux")
set.seed(8675309) # this makes sure your script uses the same set of random numbers each time you run the full script (never set this inside a function or loop)
```

## Independent samples

Let's start with a simple independent-samples design where the variables are from a normal distribution. Each subject produces one score (in conditions A or B). What we need to know about these scores is:

* How many subjects are in each condition?
* What are the score means?
* What are the score variances?

I start simulation scripts by setting variables for these values.


```r
A_sub_n <- 50
B_sub_n <- 50
A_mean <- 10
B_mean <- 12
A_sd <- 2.5
B_sd <- 2.5
```

We can the generate the scores using the `rnorm()` <a class='glossary' target='_blank' title='A named section of code that can be reused.' href='https://psyteachr.github.io/glossary/f#function'>function</a>.


```r
A_scores <- rnorm(A_sub_n, A_mean, A_sd)
B_scores <- rnorm(B_sub_n, B_mean, B_sd)
```

I always use the `tidyverse` for <a class='glossary' target='_blank' title='The process of preparing data for visualisation and statistical analysis.' href='https://psyteachr.github.io/glossary/d#data-wrangling'>data wrangling</a>", so I'll create a data table using the `tibble()` function. We need to know what condition each subject is in, so set the first `A_sub_n` values to "A" and the next `B_sub_n` values to "B". Then set the score to the `A_scores` <a class='glossary' target='_blank' title='To combine strings or vectors.' href='https://psyteachr.github.io/glossary/c#concatenate'>concatenated</a> to the `B_scores`.


```r
dat <- tibble(
  sub_condition = rep( c("A", "B"), c(A_sub_n, B_sub_n) ),
  score = c(A_scores, B_scores)
)
```

Always examine your simulated data after you generate it to make sure it looks like you want.


```r
dat %>%
  group_by(sub_condition) %>%
  summarise(n = n() ,
            mean = mean(score),
            sd = sd(score)) %>%
  knitr::kable()
```



sub_condition     n       mean         sd
--------------  ---  ---------  ---------
A                50   10.25673   2.149247
B                50   12.00478   2.499963

Now you can analyse your simulated data.


```r
t.test(score~sub_condition, dat)
```

```
## 
## 	Welch Two Sample t-test
## 
## data:  score by sub_condition
## t = -3.7492, df = 95.843, p-value = 0.0003034
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  -2.6735571 -0.8225523
## sample estimates:
## mean in group A mean in group B 
##        10.25673        12.00478
```



## Paired samples

Now let's try a paired-samples design where the variables are from a normal distribution. Each subject produces two scores (in conditions A and B). What we need to know about these two scores is:

* How many subjects?
* What are the score means?
* What are the score variances?
* What is the correlation between the scores?



```r
sub_n <- 100
A_mean <- 10
B_mean <- 12
A_sd <- 2.5
B_sd <- 2.5
AB_r <- 0.5
```

You can then use `rnorm_multi()` to generate a data table with simulated values for correlated scores:


```r
dat <- faux::rnorm_multi(
  n = sub_n, 
  vars = 2, 
  cors = AB_r, 
  mu = c(A_mean, B_mean), 
  sd = c(A_sd, B_sd), 
  varnames = c("A", "B")
)
```

Now check your data; `faux` has a function `check_sim_stats()` that gives you the correlation table, means, and SDs for each numeric column in a data table.


var       A      B    mean     sd
----  -----  -----  ------  -----
A      1.00   0.48    9.90   2.46
B      0.48   1.00   11.88   2.53

Finally, you can analyse your simulated data.


```r
t.test(dat$A, dat$B, paired = TRUE)
```

```
## 
## 	Paired t-test
## 
## data:  dat$A and dat$B
## t = -7.7821, df = 99, p-value = 7.01e-12
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  -2.494424 -1.480841
## sample estimates:
## mean of the differences 
##               -1.987633
```

## Intercept model

Now I'm going to show you a different way to simulate the same design. This might seem excessively complicated, but you will need this pattern when you start simulating data for [mixed effects models](#sim_lmer).

Remember, we used the following variables to set up our simulation above:


```r
sub_n <- 100
A_mean <- 10
B_mean <- 12
A_sd <- 2.5
B_sd <- 2.5
AB_r <- 0.5
```

From these, we can calculate the grand intercept (the overall mean regardless of condition), and the effect of condition (the mean of B minus A).


```r
grand_i <- (A_mean + B_mean)/2
AB_effect <- B_mean - A_mean
```

We also need to think about variance a little differently. First, calculate the pooled variance as the mean of the variances for A and B (remember, variance is SD squared). The variance of the subject intercepts is `r` times this pooled variance and the error variance is what is left over. We take the square root (`sqrt()`) to set the subject intercept and error SDs for simulation later.


```r
pooled_var <- (A_sd^2 + B_sd^2)/2
sub_sd <- sqrt(pooled_var * AB_r)
error_sd <- sqrt(pooled_var * (1-AB_r))
```

<div class="info">
<p>You can think about the subject intercept variance as how much subjects vary in the score in general, regardless of condition. If they vary a lot, in comparison to the random &quot;error&quot; variation, then scores in the two conditions will be highly correlated. If they don't vary much (or random variation from trial to trial is quite large), then scores won't be well correlated.</p>
</div>

Now we use these variables to create a data table for our subjects. Each subject gets an ID and a **random intercept** (`sub_i`). The intercept is simulated from a random normal distribution with a mean of 0 and an SD of `sub_sd`. This represents how much higher or lower than the average score each subject tends to be (regardless of condition).


```r
sub <- tibble(
  sub_id = 1:sub_n,
  sub_i = rnorm(sub_n, 0, sub_sd)
)
```

Next, set up a table where each row represents one observation. We'll use one of my favourite functions for simulation: `expand.grid()`. This creates every possible combination of the listed factors. Here, we're using it to create a row for each subject in each condition, since this is a fully within-subjects design.


```r
obs <- expand.grid(
  sub_id = 1:sub_n,
  condition = c("A", "B")
)
```

Next, we join the subject table so each row has the information about the subject's random intercept and then calculate the score. I've done it in a few steps below for clarity. The score is just the sum of:

* the overall mean (`grand_i`)
* the subject-specific intercept (`sub_i`)
* the effect of condition (-50% of `AB_effect` for condition A and +50% of `AB_effect` for condition B)
* the error term (simulated from a normal distribution with mean of 0 and SD of `error_sd`)


```r
dat <- obs %>%
  left_join(sub, by = "sub_id") %>%
  mutate(
    condition.e = recode(condition, "A" = -0.5, "B" = 0.5),
    effect = AB_effect * condition.e,
    error = rnorm(nrow(.), 0, error_sd),
    score = grand_i + sub_i + effect + error
  )
```

<div class="info">
<p>The code above &quot;effect codes&quot; condition, which we will use later in a mixed effect model. You can learn more about <a href="https://debruine.github.io/posts/coding-schemes/">coding schemes</a> here.</p>
</div>

You can use the following code to put the data table into a more familiar "wide" format.


```r
dat_wide <- dat %>%
  select(sub_id, condition, score) %>%
  spread(condition, score)
```


Then you can use the `check_sim_stats` function to check this looks correct (remove the subject ID to leave it out of the table, since it's numeric).


var       A      B    mean     sd
----  -----  -----  ------  -----
A      1.00   0.54    9.61   2.72
B      0.54   1.00   11.83   2.72

You can analyse the data from the wide format:


```r
t.test(dat_wide$A, dat_wide$B, paired = TRUE)
```

```
## 
## 	Paired t-test
## 
## data:  dat_wide$A and dat_wide$B
## t = -8.4641, df = 99, p-value = 2.408e-13
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  -2.738493 -1.698367
## sample estimates:
## mean of the differences 
##                -2.21843
```

Or in the long format:


```r
t.test(score ~ condition, dat, paired = TRUE)
```

```
## 
## 	Paired t-test
## 
## data:  score by condition
## t = -8.4641, df = 99, p-value = 2.408e-13
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  -2.738493 -1.698367
## sample estimates:
## mean of the differences 
##                -2.21843
```

You can even analyse the data with a mixed effects model. 


```r
lmem <- lmerTest::lmer(score ~ condition.e + (1 | sub_id), data = dat)

summary(lmem)$coefficients %>% 
  # nicer formatting with p-values
  as_tibble(rownames = "Factor") %>%
  mutate_if(~max(.) < .001, ~as.character(signif(., 3))) %>% 
  knitr::kable(digits = 3)
```



Factor         Estimate   Std. Error   df   t value  Pr(>|t|) 
------------  ---------  -----------  ---  --------  ---------
(Intercept)      10.716        0.238   99    44.985  1.04e-67 
condition.e       2.218        0.262   99     8.464  2.41e-13 


We can put everything together into a function where you specify the subject number, means, SDs and correlation and it returns a data table.


```r
sim_paired_data <- function(n = 100, mu = c(0,0), sd = c(1,1), r = 0) {
  sub_n <- n
  A_mean <- mu[1]
  B_mean <- mu[2]
  A_sd <- sd[1]
  B_sd <- sd[2]
  AB_r <- r
  
  grand_i <- (A_mean + B_mean)/2
  AB_effect <- B_mean - A_mean
  pooled_var <- (A_sd^2 + B_sd^2)/2
  sub_sd <- sqrt(pooled_var * AB_r)
  error_sd <- sqrt(pooled_var * (1-AB_r))
  
  sub <- tibble(
    sub_id = 1:sub_n,
    sub_i = rnorm(sub_n, 0, sub_sd)
  )
  
  expand.grid(
    sub_id = 1:sub_n,
    condition = c("A", "B")
  ) %>%
  left_join(sub, by = "sub_id") %>%
  mutate(
    effect = case_when(
      condition == "A" ~ AB_effect * -0.5,
      condition == "B" ~ AB_effect * +0.5
    ),
    error = rnorm(nrow(.), 0, error_sd),
    score = grand_i + sub_i + effect + error
  ) %>%
  select(sub_id, condition, score) %>%
  spread(condition, score)
}
```


```r
sim_paired_data(100, c(10, 12), c(2.5, 2.5), .5) %>%
  select(-sub_id) %>%
  faux::check_sim_stats(usekable = TRUE)
```



var       A      B    mean     sd
----  -----  -----  ------  -----
A      1.00   0.48    9.98   2.32
B      0.48   1.00   11.97   2.62


```r
sim_paired_data(10000, c(0, 0.5), c(1, 1), .25) %>%
  select(-sub_id) %>%
  faux::check_sim_stats(usekable = TRUE)
```



var       A      B    mean     sd
----  -----  -----  ------  -----
A      1.00   0.26   -0.02   1.00
B      0.26   1.00    0.51   0.99



