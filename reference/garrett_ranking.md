# Garrett Ranking of Categorical Data

There are three main types of ranking: Standard competition, Ordinal and
Fractional. Garrett's Ranking Technique is the application of fractional
ranking in which the data points are ordered and given an ordinal
number/rank. The ordering and ranking provide additional information
which may not be available from frequency distribution. Again, the
ordering is based on the level of seriousness or severity of the data
point from the view point of the respondent. Ranking enables ease of
comparison and makes grouping more meaningful. It is used in social
science, psychology and other survey types of research. This functions
performs Garrett Ranking of up to 15 ranks.

## Usage

``` r
garrett_ranking(data, num_rank, ranking = NULL, m_rank = c(2:15))
```

## Arguments

- data:

  The data for the Garrett Ranking, must be a `data.frame`.

- num_rank:

  A vector representing the number of ranks applied to the data. If the
  data is a five-point Likert-type data, then number of ranks is 5.

- ranking:

  A vector of list representing the ranks applied to the data. If not
  available, positional ranks are applied.

- m_rank:

  The scope of the ranking methods which is between 2 and 15.

## Value

A list with the following components:

- `RII`:

  Relative importance index.

- `Garrett ranked data`:

  Table of data ranked using Garrett mean score.

- `Garrett value`:

  Table of ranking Garrett values

## Examples

``` r
library(readr)
garrett_data <- data.frame(garrett_data)
ranking <- c("Serious constraint", "Constraint",
"Not certain it is a constraint", "Not a constraint",
"Not a serious constraint")

## ranking is supplied
garrett_ranking(garrett_data, 5, ranking)
#> New names:
#> • `` -> `...1`
#> • `V1` -> `V1...2`
#> • `V1` -> `V1...8`
#> • `` -> `...9`
#> $`Garrett value`
#> # A tibble: 5 × 4
#>   Number `Garrett point` `Garrett index` `Garrett value`
#>    <dbl>           <dbl>           <dbl>           <dbl>
#> 1      1            3.33              15              85
#> 2      2           10                 25              75
#> 3      3           16.7               31              69
#> 4      4           23.3               36              64
#> 5      5           30                 40              60
#> 
#> $`Garrett ranked data`
#>    S/No Description Serious constraint Constraint
#> 1     2          S2                  5          3
#> 2     9          S9                  7          6
#> 3    15         S15                  7          6
#> 4     5          S5                 10          2
#> 5    11         S11                 10          2
#> 6     4          S4                  4          4
#> 7    10         S10                  4          4
#> 8     3          S3                  1          2
#> 9     1          S1                  0          0
#> 10    6          S6                  0          4
#> 11   12         S12                  0          4
#> 12    7          S7                  0          2
#> 13   13         S13                  0          2
#> 14    8          S8                  0          0
#> 15   14         S14                  0          0
#>    Not certain it is a constraint Not a constraint Not a serious constraint
#> 1                               2                2                        1
#> 2                               0                5                        1
#> 3                               0                5                        1
#> 4                               8                5                        0
#> 5                               8                5                        0
#> 6                               6                7                        3
#> 7                               6                7                        3
#> 8                               5                5                        1
#> 9                               2                1                        0
#> 10                              6                5                        6
#> 11                              6                5                        6
#> 12                              0                2                        2
#> 13                              0                2                        2
#> 14                              5                2                       17
#> 15                              5                2                       17
#>    Total      Mean Total Garrett Score Mean Garrett score Total Item score
#> 1     13  8.172414                 976           75.07692               48
#> 2     19  4.517241                1425           75.00000               70
#> 3     19  4.517241                1425           75.00000               70
#> 4     25  3.413793                1872           74.88000               92
#> 5     25  3.413793                1872           74.88000               92
#> 6     24  3.310345                1682           70.08333               71
#> 7     24  3.310345                1682           70.08333               71
#> 8     14  5.965517                 960           68.57143               39
#> 9      3 14.758621                 202           67.33333                8
#> 10    21  3.965517                1394           66.38095               50
#> 11    21  3.965517                1394           66.38095               50
#> 12     6  7.034483                 398           66.33333               14
#> 13     6  7.034483                 398           66.33333               14
#> 14    24  1.862069                1493           62.20833               36
#> 15    24  1.862069                1493           62.20833               36
#>    Relative importance index Rank
#> 1                 0.33103448    1
#> 2                 0.48275862    2
#> 3                 0.48275862    3
#> 4                 0.63448276    4
#> 5                 0.63448276    5
#> 6                 0.48965517    6
#> 7                 0.48965517    7
#> 8                 0.26896552    8
#> 9                 0.05517241    9
#> 10                0.34482759   10
#> 11                0.34482759   11
#> 12                0.09655172   12
#> 13                0.09655172   13
#> 14                0.24827586   14
#> 15                0.24827586   15
#> 
#> $RII
#>    V1 V2 V3 V4 V5
#> 1   0  0  6  2  0
#> 2  25 12  6  4  1
#> 3   5  8 15 10  1
#> 4  20 16 18 14  3
#> 5  50  8 24 10  0
#> 6   0 16 18 10  6
#> 7   0  8  0  4  2
#> 8   0  0 15  4 17
#> 9  35 24  0 10  1
#> 10 20 16 18 14  3
#> 11 50  8 24 10  0
#> 12  0 16 18 10  6
#> 13  0  8  0  4  2
#> 14  0  0 15  4 17
#> 15 35 24  0 10  1
#> 

# ranking not supplied
garrett_ranking(garrett_data, 5)
#> New names:
#> • `` -> `...1`
#> • `V1` -> `V1...2`
#> • `V1` -> `V1...8`
#> • `` -> `...9`
#> $`Garrett value`
#> # A tibble: 5 × 4
#>   Number `Garrett point` `Garrett index` `Garrett value`
#>    <dbl>           <dbl>           <dbl>           <dbl>
#> 1      1            3.33              15              85
#> 2      2           10                 25              75
#> 3      3           16.7               31              69
#> 4      4           23.3               36              64
#> 5      5           30                 40              60
#> 
#> $`Garrett ranked data`
#>    S/No Description 1st Rank 2nd Rank 3rd Rank 4th Rank 5th Rank Total
#> 1     2          S2        5        3        2        2        1    13
#> 2     9          S9        7        6        0        5        1    19
#> 3    15         S15        7        6        0        5        1    19
#> 4     5          S5       10        2        8        5        0    25
#> 5    11         S11       10        2        8        5        0    25
#> 6     4          S4        4        4        6        7        3    24
#> 7    10         S10        4        4        6        7        3    24
#> 8     3          S3        1        2        5        5        1    14
#> 9     1          S1        0        0        2        1        0     3
#> 10    6          S6        0        4        6        5        6    21
#> 11   12         S12        0        4        6        5        6    21
#> 12    7          S7        0        2        0        2        2     6
#> 13   13         S13        0        2        0        2        2     6
#> 14    8          S8        0        0        5        2       17    24
#> 15   14         S14        0        0        5        2       17    24
#>         Mean Total Garrett Score Mean Garrett score Total Item score
#> 1   8.172414                 976           75.07692               48
#> 2   4.517241                1425           75.00000               70
#> 3   4.517241                1425           75.00000               70
#> 4   3.413793                1872           74.88000               92
#> 5   3.413793                1872           74.88000               92
#> 6   3.310345                1682           70.08333               71
#> 7   3.310345                1682           70.08333               71
#> 8   5.965517                 960           68.57143               39
#> 9  14.758621                 202           67.33333                8
#> 10  3.965517                1394           66.38095               50
#> 11  3.965517                1394           66.38095               50
#> 12  7.034483                 398           66.33333               14
#> 13  7.034483                 398           66.33333               14
#> 14  1.862069                1493           62.20833               36
#> 15  1.862069                1493           62.20833               36
#>    Relative importance index Rank
#> 1                 0.33103448    1
#> 2                 0.48275862    2
#> 3                 0.48275862    3
#> 4                 0.63448276    4
#> 5                 0.63448276    5
#> 6                 0.48965517    6
#> 7                 0.48965517    7
#> 8                 0.26896552    8
#> 9                 0.05517241    9
#> 10                0.34482759   10
#> 11                0.34482759   11
#> 12                0.09655172   12
#> 13                0.09655172   13
#> 14                0.24827586   14
#> 15                0.24827586   15
#> 
#> $RII
#>    V1 V2 V3 V4 V5
#> 1   0  0  6  2  0
#> 2  25 12  6  4  1
#> 3   5  8 15 10  1
#> 4  20 16 18 14  3
#> 5  50  8 24 10  0
#> 6   0 16 18 10  6
#> 7   0  8  0  4  2
#> 8   0  0 15  4 17
#> 9  35 24  0 10  1
#> 10 20 16 18 14  3
#> 11 50  8 24 10  0
#> 12  0 16 18 10  6
#> 13  0  8  0  4  2
#> 14  0  0 15  4 17
#> 15 35 24  0 10  1
#> 

# you can rank subset of the data
garrett_ranking(garrett_data, 8)
#> New names:
#> • `` -> `...1`
#> • `V1` -> `V1...2`
#> • `V1` -> `V1...11`
#> • `` -> `...12`
#> $`Garrett value`
#> # A tibble: 8 × 4
#>   Number `Garrett point` `Garrett index` `Garrett value`
#>    <dbl>           <dbl>           <dbl>           <dbl>
#> 1      1            3.33              15              85
#> 2      2           10                 25              75
#> 3      3           16.7               31              69
#> 4      4           23.3               36              64
#> 5      5           30                 40              60
#> 6      6           36.7               43              57
#> 7      7           43.3               47              53
#> 8      8           50                 50              50
#> 
#> $`Garrett ranked data`
#>    S/No Description 1st Rank 2nd Rank 3rd Rank 4th Rank 5th Rank 6th Rank
#> 1     7          S7        4        2        2        0        2        0
#> 2    13         S13        4        2        2        0        2        0
#> 3     2          S2        2        0        2        5        3        2
#> 4     9          S9        0        4        4        7        6        0
#> 5    15         S15        0        4        4        7        6        0
#> 6     3          S3        1        3        4        1        2        5
#> 7     5          S5        0        1        0       10        2        8
#> 8    11         S11        0        1        0       10        2        8
#> 9     4          S4        0        1        3        4        4        6
#> 10   10         S10        0        1        3        4        4        6
#> 11    6          S6        0        1        1        0        4        6
#> 12   12         S12        0        1        1        0        4        6
#> 13    1          S1        0        0        0        0        0        2
#> 14    8          S8        1        0        0        0        0        5
#> 15   14         S14        1        0        0        0        0        5
#>    7th Rank 8th Rank Total      Mean Total Garrett Score Mean Garrett score
#> 1         2        2    14  7.034483                 954           68.14286
#> 2         2        2    14  7.034483                 954           68.14286
#> 3         2        1    17  8.172414                1078           63.41176
#> 4         5        1    27  4.517241                1699           62.92593
#> 5         5        1    27  4.517241                1699           62.92593
#> 6         5        1    22  5.965517                1370           62.27273
#> 7         5        0    26  3.413793                1556           59.84615
#> 8         5        0    26  3.413793                1556           59.84615
#> 9         7        3    28  3.310345                1641           58.60714
#> 10        7        3    28  3.310345                1641           58.60714
#> 11        5        6    23  3.965517                1291           56.13043
#> 12        5        6    23  3.965517                1291           56.13043
#> 13        1        0     3 14.758621                 167           55.66667
#> 14        2       17    25  1.862069                1326           53.04000
#> 15        2       17    25  1.862069                1326           53.04000
#>    Total Item score Relative importance index Rank
#> 1                72                0.31034483    1
#> 2                72                0.31034483    2
#> 3                76                0.32758621    3
#> 4               122                0.52586207    4
#> 5               122                0.52586207    5
#> 6                92                0.39655172    6
#> 7                99                0.42672414    7
#> 8                99                0.42672414    8
#> 9                96                0.41379310    9
#> 10               96                0.41379310   10
#> 11               63                0.27155172   11
#> 12               63                0.27155172   12
#> 13                8                0.03448276   13
#> 14               44                0.18965517   14
#> 15               44                0.18965517   15
#> 
#> $RII
#>    V1 V2 V3 V4 V5 V6 V7 V8
#> 1   0  0  0  0  0  6  2  0
#> 2  16  0 12 25 12  6  4  1
#> 3   8 21 24  5  8 15 10  1
#> 4   0  7 18 20 16 18 14  3
#> 5   0  7  0 50  8 24 10  0
#> 6   0  7  6  0 16 18 10  6
#> 7  32 14 12  0  8  0  4  2
#> 8   8  0  0  0  0 15  4 17
#> 9   0 28 24 35 24  0 10  1
#> 10  0  7 18 20 16 18 14  3
#> 11  0  7  0 50  8 24 10  0
#> 12  0  7  6  0 16 18 10  6
#> 13 32 14 12  0  8  0  4  2
#> 14  8  0  0  0  0 15  4 17
#> 15  0 28 24 35 24  0 10  1
#> 

garrett_ranking(garrett_data, 4)
#> New names:
#> • `` -> `...1`
#> • `V1` -> `V1...2`
#> • `V1` -> `V1...7`
#> • `` -> `...8`
#> $`Garrett value`
#> # A tibble: 4 × 4
#>   Number `Garrett point` `Garrett index` `Garrett value`
#>    <dbl>           <dbl>           <dbl>           <dbl>
#> 1      1            3.33              15              85
#> 2      2           10                 25              75
#> 3      3           16.7               31              69
#> 4      4           23.3               36              64
#> 
#> $`Garrett ranked data`
#>    S/No Description 1st Rank 2nd Rank 3rd Rank 4th Rank Total      Mean
#> 1     9          S9        6        0        5        1    12  4.517241
#> 2    15         S15        6        0        5        1    12  4.517241
#> 3     2          S2        3        2        2        1     8  8.172414
#> 4     5          S5        2        8        5        0    15  3.413793
#> 5    11         S11        2        8        5        0    15  3.413793
#> 6     3          S3        2        5        5        1    13  5.965517
#> 7     4          S4        4        6        7        3    20  3.310345
#> 8    10         S10        4        6        7        3    20  3.310345
#> 9     1          S1        0        2        1        0     3 14.758621
#> 10    7          S7        2        0        2        2     6  7.034483
#> 11   13         S13        2        0        2        2     6  7.034483
#> 12    6          S6        4        6        5        6    21  3.965517
#> 13   12         S12        4        6        5        6    21  3.965517
#> 14    8          S8        0        5        2       17    24  1.862069
#> 15   14         S14        0        5        2       17    24  1.862069
#>    Total Garrett Score Mean Garrett score Total Item score
#> 1                  919           76.58333               35
#> 2                  919           76.58333               35
#> 3                  607           75.87500               23
#> 4                 1115           74.33333               42
#> 5                 1115           74.33333               42
#> 6                  954           73.38462               34
#> 7                 1465           73.25000               51
#> 8                 1465           73.25000               51
#> 9                  219           73.00000                8
#> 10                 436           72.66667               14
#> 11                 436           72.66667               14
#> 12                1519           72.33333               50
#> 13                1519           72.33333               50
#> 14                1601           66.70833               36
#> 15                1601           66.70833               36
#>    Relative importance index Rank
#> 1                 0.30172414    1
#> 2                 0.30172414    2
#> 3                 0.19827586    3
#> 4                 0.36206897    4
#> 5                 0.36206897    5
#> 6                 0.29310345    6
#> 7                 0.43965517    7
#> 8                 0.43965517    8
#> 9                 0.06896552    9
#> 10                0.12068966   10
#> 11                0.12068966   11
#> 12                0.43103448   12
#> 13                0.43103448   13
#> 14                0.31034483   14
#> 15                0.31034483   15
#> 
#> $RII
#>    V1 V2 V3 V4
#> 1   0  6  2  0
#> 2  12  6  4  1
#> 3   8 15 10  1
#> 4  16 18 14  3
#> 5   8 24 10  0
#> 6  16 18 10  6
#> 7   8  0  4  2
#> 8   0 15  4 17
#> 9  24  0 10  1
#> 10 16 18 14  3
#> 11  8 24 10  0
#> 12 16 18 10  6
#> 13  8  0  4  2
#> 14  0 15  4 17
#> 15 24  0 10  1
#> 
```
