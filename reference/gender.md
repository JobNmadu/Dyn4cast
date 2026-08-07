# Create Gender Variable

Often, there is need to differentiate between sex and gender. Many
wonder if there is any difference at all. This function will create
clarity between them.

## Usage

``` r
gender(data)
```

## Arguments

- data:

  data frame containing **Age** and **Sex** variables

## Value

The `data.frame` with:

- `Gender`:

  data frame with two additional variables.

## Examples

``` r
df <- data.frame(Age = c(49, 30, 44, 37, 29, 56),
 Sex = c("male", "female", "female", "male", "Prefer not to say",
  "Non-binary/third gender"))
 gender(df)
#>   Age                     Sex Group                  Gender
#> 1  49                    male     2              Adult male
#> 2  30                  female     2            Adult female
#> 3  44                  female     2            Adult female
#> 4  37                    male     2              Adult male
#> 5  29       Prefer not to say     2       Prefer not to say
#> 6  56 Non-binary/third gender     2 Non-binary/third gender
```
