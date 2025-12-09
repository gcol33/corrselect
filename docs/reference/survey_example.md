# Example Survey Data for Social Science Research

A simulated questionnaire dataset with 30 Likert-scale items measuring
three latent constructs (satisfaction, engagement, loyalty), plus
demographic variables and an overall satisfaction score.

## Usage

``` r
survey_example
```

## Format

A data frame with 200 rows and 35 variables:

- respondent_id:

  Integer. Unique respondent identifier

- age:

  Integer. Respondent age (18-75 years)

- gender:

  Factor. Gender (Male, Female, Other)

- education:

  Ordered factor. Education level (High School, Bachelor, Master, PhD)

- overall_satisfaction:

  Integer. Overall satisfaction score (0-100)

- satisfaction_1, satisfaction_2, satisfaction_3, satisfaction_4,
  satisfaction_5, satisfaction_6, satisfaction_7, satisfaction_8,
  satisfaction_9, satisfaction_10:

  Ordered factor. Satisfaction items (1-7 Likert scale)

- engagement_1, engagement_2, engagement_3, engagement_4, engagement_5,
  engagement_6, engagement_7, engagement_8, engagement_9, engagement_10:

  Ordered factor. Engagement items (1-7 Likert scale)

- loyalty_1, loyalty_2, loyalty_3, loyalty_4, loyalty_5, loyalty_6,
  loyalty_7, loyalty_8, loyalty_9, loyalty_10:

  Ordered factor. Loyalty items (1-7 Likert scale)

## Source

Simulated data based on typical customer satisfaction survey structures

## Details

This dataset represents a common scenario in survey research: multiple
items measuring similar constructs lead to redundancy and
multicollinearity. Items within each construct are correlated
(satisfaction, engagement, loyalty), and the constructs themselves are
inter-correlated.

**Use case**: Demonstrating
[`assocSelect()`](https://gillescolling.com/corrselect/reference/assocSelect.md)
for identifying redundant questionnaire items in mixed-type data
(ordered factors + numeric variables).

## See also

[`assocSelect()`](https://gillescolling.com/corrselect/reference/assocSelect.md),
[`corrPrune()`](https://gillescolling.com/corrselect/reference/corrPrune.md)

## Examples

``` r
data(survey_example)

# This dataset has mixed types: numeric (age, overall_satisfaction),
# factors (gender, education), and ordered factors (Likert items)
str(survey_example[, 1:10])
#> 'data.frame':    200 obs. of  10 variables:
#>  $ respondent_id       : int  1 2 3 4 5 6 7 8 9 10 ...
#>  $ age                 : num  38 32 18 18 19 39 33 26 26 42 ...
#>  $ gender              : Factor w/ 3 levels "Female","Male",..: 2 3 1 2 2 1 1 2 2 1 ...
#>  $ education           : Ord.factor w/ 4 levels "High School"<..: 3 1 4 2 2 1 1 1 2 3 ...
#>  $ overall_satisfaction: num  58 40 44 40 58 67 61 49 51 52 ...
#>  $ satisfaction_1      : Ord.factor w/ 7 levels "1"<"2"<"3"<"4"<..: 6 4 5 3 4 5 4 5 4 6 ...
#>  $ satisfaction_2      : Ord.factor w/ 7 levels "1"<"2"<"3"<"4"<..: 6 3 4 3 4 6 6 4 4 4 ...
#>  $ satisfaction_3      : Ord.factor w/ 7 levels "1"<"2"<"3"<"4"<..: 6 3 4 4 3 4 5 3 4 4 ...
#>  $ satisfaction_4      : Ord.factor w/ 7 levels "1"<"2"<"3"<"4"<..: 7 3 4 4 4 5 4 3 2 4 ...
#>  $ satisfaction_5      : Ord.factor w/ 7 levels "1"<"2"<"3"<"4"<..: 7 4 5 4 5 4 3 5 3 6 ...

# \donttest{
# Use assocSelect() for mixed-type data pruning
# This may take a few seconds with 34 variables
pruned <- assocSelect(survey_example[, -1],  # Exclude respondent_id
                      threshold = 0.8,
                      method_ord_ord = "spearman")
length(attr(pruned, "selected_vars"))
#> [1] 0
# }
```
