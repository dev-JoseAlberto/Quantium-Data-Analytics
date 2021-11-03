Experimentation and uplift testing
================
Jose Alberto
03/11/2021

``` r
rm(list=ls(all=TRUE))
```

Load packages in r

``` r
library(rmarkdown)
library(htmltools)
library(tidyverse)
library(dplyr)
library(plyr)
library(kableExtra)
library(stringr)
library(openxlsx)
library(lubridate)
library(ggplot2)
library(GGally)
library(fpp2)
library(stats)
library(fastcluster)
```

``` r
customers_chips_brand_dataset <- read.csv("datasets/QVI_data.csv")
```

The structure of the data

``` r
str(customers_chips_brand_dataset)
```

    ## 'data.frame':    264834 obs. of  12 variables:
    ##  $ LYLTY_CARD_NBR  : int  1000 1002 1003 1003 1004 1005 1007 1007 1009 1010 ...
    ##  $ DATE            : chr  "2018-10-17" "2018-09-16" "2019-03-07" "2019-03-08" ...
    ##  $ STORE_NBR       : int  1 1 1 1 1 1 1 1 1 1 ...
    ##  $ TXN_ID          : int  1 2 3 4 5 6 7 8 9 10 ...
    ##  $ PROD_NBR        : int  5 58 52 106 96 86 49 10 20 51 ...
    ##  $ PROD_NAME       : chr  "Natural Chip        Compny SeaSalt175g" "Red Rock Deli Chikn&Garlic Aioli 150g" "Grain Waves Sour    Cream&Chives 210G" "Natural ChipCo      Hony Soy Chckn175g" ...
    ##  $ PROD_QTY        : int  2 1 1 1 1 1 1 1 1 2 ...
    ##  $ TOT_SALES       : num  6 2.7 3.6 3 1.9 2.8 3.8 2.7 5.7 8.8 ...
    ##  $ PACK_SIZE       : int  175 150 210 175 160 165 110 150 330 170 ...
    ##  $ BRAND           : chr  "NATURAL" "RRD" "GRNWVES" "NATURAL" ...
    ##  $ LIFESTAGE       : chr  "YOUNG SINGLES/COUPLES" "YOUNG SINGLES/COUPLES" "YOUNG FAMILIES" "YOUNG FAMILIES" ...
    ##  $ PREMIUM_CUSTOMER: chr  "Premium" "Mainstream" "Budget" "Budget" ...

``` r
customers_chips_brand_dataset$DATE <- as.Date(customers_chips_brand_dataset$DATE)
```

``` r
sum(is.na(customers_chips_brand_dataset))
```

    ## [1] 0

``` r
summary(customers_chips_brand_dataset)
```

    ##  LYLTY_CARD_NBR         DATE              STORE_NBR         TXN_ID       
    ##  Min.   :   1000   Min.   :2018-07-01   Min.   :  1.0   Min.   :      1  
    ##  1st Qu.:  70021   1st Qu.:2018-09-30   1st Qu.: 70.0   1st Qu.:  67601  
    ##  Median : 130357   Median :2018-12-30   Median :130.0   Median : 135137  
    ##  Mean   : 135549   Mean   :2018-12-30   Mean   :135.1   Mean   : 135158  
    ##  3rd Qu.: 203094   3rd Qu.:2019-03-31   3rd Qu.:203.0   3rd Qu.: 202700  
    ##  Max.   :2373711   Max.   :2019-06-30   Max.   :272.0   Max.   :2415841  
    ##     PROD_NBR       PROD_NAME            PROD_QTY       TOT_SALES     
    ##  Min.   :  1.00   Length:264834      Min.   :1.000   Min.   : 1.500  
    ##  1st Qu.: 28.00   Class :character   1st Qu.:2.000   1st Qu.: 5.400  
    ##  Median : 56.00   Mode  :character   Median :2.000   Median : 7.400  
    ##  Mean   : 56.58                      Mean   :1.906   Mean   : 7.299  
    ##  3rd Qu.: 85.00                      3rd Qu.:2.000   3rd Qu.: 9.200  
    ##  Max.   :114.00                      Max.   :5.000   Max.   :29.500  
    ##    PACK_SIZE        BRAND            LIFESTAGE         PREMIUM_CUSTOMER  
    ##  Min.   : 70.0   Length:264834      Length:264834      Length:264834     
    ##  1st Qu.:150.0   Class :character   Class :character   Class :character  
    ##  Median :170.0   Mode  :character   Mode  :character   Mode  :character  
    ##  Mean   :182.4                                                           
    ##  3rd Qu.:175.0                                                           
    ##  Max.   :380.0

``` r
anyDuplicated(customers_chips_brand_dataset)
```

    ## [1] 106250

``` r
customers_chips_brand_dataset <- customers_chips_brand_dataset %>%
  distinct()
```

``` r
count_brand <- head(customers_chips_brand_dataset %>% 
  dplyr::count(BRAND),40)
```

``` r
knitr::kable(count_brand)
```

<table>
<thead>
<tr>
<th style="text-align:left;">
BRAND
</th>
<th style="text-align:right;">
n
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
BURGER
</td>
<td style="text-align:right;">
1564
</td>
</tr>
<tr>
<td style="text-align:left;">
CCS
</td>
<td style="text-align:right;">
4551
</td>
</tr>
<tr>
<td style="text-align:left;">
CHEETOS
</td>
<td style="text-align:right;">
2927
</td>
</tr>
<tr>
<td style="text-align:left;">
CHEEZELS
</td>
<td style="text-align:right;">
4603
</td>
</tr>
<tr>
<td style="text-align:left;">
COBS
</td>
<td style="text-align:right;">
9693
</td>
</tr>
<tr>
<td style="text-align:left;">
DORITOS
</td>
<td style="text-align:right;">
28145
</td>
</tr>
<tr>
<td style="text-align:left;">
FRENCH
</td>
<td style="text-align:right;">
1418
</td>
</tr>
<tr>
<td style="text-align:left;">
GRNWVES
</td>
<td style="text-align:right;">
7740
</td>
</tr>
<tr>
<td style="text-align:left;">
INFUZIONS
</td>
<td style="text-align:right;">
14201
</td>
</tr>
<tr>
<td style="text-align:left;">
KETTLE
</td>
<td style="text-align:right;">
41288
</td>
</tr>
<tr>
<td style="text-align:left;">
NATURAL
</td>
<td style="text-align:right;">
7469
</td>
</tr>
<tr>
<td style="text-align:left;">
OLD
</td>
<td style="text-align:right;">
9324
</td>
</tr>
<tr>
<td style="text-align:left;">
PRINGLES
</td>
<td style="text-align:right;">
25102
</td>
</tr>
<tr>
<td style="text-align:left;">
RRD
</td>
<td style="text-align:right;">
17779
</td>
</tr>
<tr>
<td style="text-align:left;">
SMITHS
</td>
<td style="text-align:right;">
31822
</td>
</tr>
<tr>
<td style="text-align:left;">
SUNBITES
</td>
<td style="text-align:right;">
3008
</td>
</tr>
<tr>
<td style="text-align:left;">
THINS
</td>
<td style="text-align:right;">
14075
</td>
</tr>
<tr>
<td style="text-align:left;">
TOSTITOS
</td>
<td style="text-align:right;">
9471
</td>
</tr>
<tr>
<td style="text-align:left;">
TWISTIES
</td>
<td style="text-align:right;">
9454
</td>
</tr>
<tr>
<td style="text-align:left;">
TYRRELLS
</td>
<td style="text-align:right;">
6442
</td>
</tr>
<tr>
<td style="text-align:left;">
WOOLWORTHS
</td>
<td style="text-align:right;">
14757
</td>
</tr>
</tbody>
</table>

Building a contigency table based on life stage and product name

``` r
table(customers_chips_brand_dataset$LIFESTAGE, customers_chips_brand_dataset$PROD_NAME)
```

    ##                         
    ##                          Burger Rings 220g CCs Nacho Cheese    175g
    ##   MIDAGE SINGLES/COUPLES               152                      156
    ##   NEW FAMILIES                          40                       26
    ##   OLDER FAMILIES                       353                      317
    ##   OLDER SINGLES/COUPLES                292                      243
    ##   RETIREES                             256                      225
    ##   YOUNG FAMILIES                       293                      313
    ##   YOUNG SINGLES/COUPLES                178                      218
    ##                         
    ##                          CCs Original 175g CCs Tasty Cheese    175g
    ##   MIDAGE SINGLES/COUPLES               141                      136
    ##   NEW FAMILIES                          32                       35
    ##   OLDER FAMILIES                       306                      318
    ##   OLDER SINGLES/COUPLES                295                      313
    ##   RETIREES                             275                      241
    ##   YOUNG FAMILIES                       292                      293
    ##   YOUNG SINGLES/COUPLES                173                      203
    ##                         
    ##                          Cheetos Chs & Bacon Balls 190g Cheetos Puffs 165g
    ##   MIDAGE SINGLES/COUPLES                            141                124
    ##   NEW FAMILIES                                       35                 27
    ##   OLDER FAMILIES                                    306                309
    ##   OLDER SINGLES/COUPLES                             318                262
    ##   RETIREES                                          226                265
    ##   YOUNG FAMILIES                                    276                274
    ##   YOUNG SINGLES/COUPLES                             177                187
    ##                         
    ##                          Cheezels Cheese 330g Cheezels Cheese Box 125g
    ##   MIDAGE SINGLES/COUPLES                  335                      108
    ##   NEW FAMILIES                             76                       53
    ##   OLDER FAMILIES                          516                      297
    ##   OLDER SINGLES/COUPLES                   660                      306
    ##   RETIREES                                611                      256
    ##   YOUNG FAMILIES                          514                      257
    ##   YOUNG SINGLES/COUPLES                   437                      177
    ##                         
    ##                          Cobs Popd Sea Salt  Chips 110g
    ##   MIDAGE SINGLES/COUPLES                            318
    ##   NEW FAMILIES                                       84
    ##   OLDER FAMILIES                                    516
    ##   OLDER SINGLES/COUPLES                             727
    ##   RETIREES                                          648
    ##   YOUNG FAMILIES                                    523
    ##   YOUNG SINGLES/COUPLES                             449
    ##                         
    ##                          Cobs Popd Sour Crm  &Chives Chips 110g
    ##   MIDAGE SINGLES/COUPLES                                    320
    ##   NEW FAMILIES                                              102
    ##   OLDER FAMILIES                                            559
    ##   OLDER SINGLES/COUPLES                                     646
    ##   RETIREES                                                  586
    ##   YOUNG FAMILIES                                            475
    ##   YOUNG SINGLES/COUPLES                                     471
    ##                         
    ##                          Cobs Popd Swt/Chlli &Sr/Cream Chips 110g
    ##   MIDAGE SINGLES/COUPLES                                      323
    ##   NEW FAMILIES                                                102
    ##   OLDER FAMILIES                                              549
    ##   OLDER SINGLES/COUPLES                                       663
    ##   RETIREES                                                    650
    ##   YOUNG FAMILIES                                              506
    ##   YOUNG SINGLES/COUPLES                                       476
    ##                         
    ##                          Dorito Corn Chp     Supreme 380g
    ##   MIDAGE SINGLES/COUPLES                              300
    ##   NEW FAMILIES                                         91
    ##   OLDER FAMILIES                                      533
    ##   OLDER SINGLES/COUPLES                               684
    ##   RETIREES                                            619
    ##   YOUNG FAMILIES                                      484
    ##   YOUNG SINGLES/COUPLES                               472
    ##                         
    ##                          Doritos Cheese      Supreme 330g
    ##   MIDAGE SINGLES/COUPLES                              275
    ##   NEW FAMILIES                                         86
    ##   OLDER FAMILIES                                      534
    ##   OLDER SINGLES/COUPLES                               632
    ##   RETIREES                                            606
    ##   YOUNG FAMILIES                                      462
    ##   YOUNG SINGLES/COUPLES                               457
    ##                         
    ##                          Doritos Corn Chip Mexican Jalapeno 150g
    ##   MIDAGE SINGLES/COUPLES                                     328
    ##   NEW FAMILIES                                                84
    ##   OLDER FAMILIES                                             551
    ##   OLDER SINGLES/COUPLES                                      647
    ##   RETIREES                                                   637
    ##   YOUNG FAMILIES                                             504
    ##   YOUNG SINGLES/COUPLES                                      453
    ##                         
    ##                          Doritos Corn Chip Southern Chicken 150g
    ##   MIDAGE SINGLES/COUPLES                                     314
    ##   NEW FAMILIES                                                99
    ##   OLDER FAMILIES                                             504
    ##   OLDER SINGLES/COUPLES                                      712
    ##   RETIREES                                                   613
    ##   YOUNG FAMILIES                                             495
    ##   YOUNG SINGLES/COUPLES                                      435
    ##                         
    ##                          Doritos Corn Chips  Cheese Supreme 170g
    ##   MIDAGE SINGLES/COUPLES                                     294
    ##   NEW FAMILIES                                                98
    ##   OLDER FAMILIES                                             550
    ##   OLDER SINGLES/COUPLES                                      683
    ##   RETIREES                                                   602
    ##   YOUNG FAMILIES                                             511
    ##   YOUNG SINGLES/COUPLES                                      479
    ##                         
    ##                          Doritos Corn Chips  Nacho Cheese 170g
    ##   MIDAGE SINGLES/COUPLES                                   328
    ##   NEW FAMILIES                                              80
    ##   OLDER FAMILIES                                           525
    ##   OLDER SINGLES/COUPLES                                    655
    ##   RETIREES                                                 636
    ##   YOUNG FAMILIES                                           491
    ##   YOUNG SINGLES/COUPLES                                    445
    ##                         
    ##                          Doritos Corn Chips  Original 170g
    ##   MIDAGE SINGLES/COUPLES                               303
    ##   NEW FAMILIES                                          88
    ##   OLDER FAMILIES                                       506
    ##   OLDER SINGLES/COUPLES                                663
    ##   RETIREES                                             618
    ##   YOUNG FAMILIES                                       469
    ##   YOUNG SINGLES/COUPLES                                474
    ##                         
    ##                          Doritos Mexicana    170g
    ##   MIDAGE SINGLES/COUPLES                      281
    ##   NEW FAMILIES                                100
    ##   OLDER FAMILIES                              515
    ##   OLDER SINGLES/COUPLES                       650
    ##   RETIREES                                    656
    ##   YOUNG FAMILIES                              478
    ##   YOUNG SINGLES/COUPLES                       435
    ##                         
    ##                          Doritos Salsa       Medium 300g
    ##   MIDAGE SINGLES/COUPLES                             131
    ##   NEW FAMILIES                                        28
    ##   OLDER FAMILIES                                     322
    ##   OLDER SINGLES/COUPLES                              290
    ##   RETIREES                                           245
    ##   YOUNG FAMILIES                                     254
    ##   YOUNG SINGLES/COUPLES                              179
    ##                         
    ##                          Doritos Salsa Mild  300g
    ##   MIDAGE SINGLES/COUPLES                      107
    ##   NEW FAMILIES                                 30
    ##   OLDER FAMILIES                              325
    ##   OLDER SINGLES/COUPLES                       304
    ##   RETIREES                                    239
    ##   YOUNG FAMILIES                              286
    ##   YOUNG SINGLES/COUPLES                       181
    ##                         
    ##                          French Fries Potato Chips 175g
    ##   MIDAGE SINGLES/COUPLES                            117
    ##   NEW FAMILIES                                       32
    ##   OLDER FAMILIES                                    283
    ##   OLDER SINGLES/COUPLES                             286
    ##   RETIREES                                          228
    ##   YOUNG FAMILIES                                    278
    ##   YOUNG SINGLES/COUPLES                             194
    ##                         
    ##                          Grain Waves         Sweet Chilli 210g
    ##   MIDAGE SINGLES/COUPLES                                   316
    ##   NEW FAMILIES                                             104
    ##   OLDER FAMILIES                                           545
    ##   OLDER SINGLES/COUPLES                                    628
    ##   RETIREES                                                 629
    ##   YOUNG FAMILIES                                           490
    ##   YOUNG SINGLES/COUPLES                                    455
    ##                         
    ##                          Grain Waves Sour    Cream&Chives 210G
    ##   MIDAGE SINGLES/COUPLES                                   297
    ##   NEW FAMILIES                                              79
    ##   OLDER FAMILIES                                           545
    ##   OLDER SINGLES/COUPLES                                    683
    ##   RETIREES                                                 585
    ##   YOUNG FAMILIES                                           458
    ##   YOUNG SINGLES/COUPLES                                    458
    ##                         
    ##                          GrnWves Plus Btroot & Chilli Jam 180g
    ##   MIDAGE SINGLES/COUPLES                                   119
    ##   NEW FAMILIES                                              30
    ##   OLDER FAMILIES                                           339
    ##   OLDER SINGLES/COUPLES                                    292
    ##   RETIREES                                                 252
    ##   YOUNG FAMILIES                                           273
    ##   YOUNG SINGLES/COUPLES                                    163
    ##                         
    ##                          Infuzions BBQ Rib   Prawn Crackers 110g
    ##   MIDAGE SINGLES/COUPLES                                     300
    ##   NEW FAMILIES                                                92
    ##   OLDER FAMILIES                                             538
    ##   OLDER SINGLES/COUPLES                                      657
    ##   RETIREES                                                   627
    ##   YOUNG FAMILIES                                             493
    ##   YOUNG SINGLES/COUPLES                                      467
    ##                         
    ##                          Infuzions Mango     Chutny Papadums 70g
    ##   MIDAGE SINGLES/COUPLES                                     141
    ##   NEW FAMILIES                                                28
    ##   OLDER FAMILIES                                             311
    ##   OLDER SINGLES/COUPLES                                      308
    ##   RETIREES                                                   263
    ##   YOUNG FAMILIES                                             274
    ##   YOUNG SINGLES/COUPLES                                      182
    ##                         
    ##                          Infuzions SourCream&Herbs Veg Strws 110g
    ##   MIDAGE SINGLES/COUPLES                                      302
    ##   NEW FAMILIES                                                 98
    ##   OLDER FAMILIES                                              535
    ##   OLDER SINGLES/COUPLES                                       661
    ##   RETIREES                                                    590
    ##   YOUNG FAMILIES                                              506
    ##   YOUNG SINGLES/COUPLES                                       442
    ##                         
    ##                          Infuzions Thai SweetChili PotatoMix 110g
    ##   MIDAGE SINGLES/COUPLES                                      316
    ##   NEW FAMILIES                                                 82
    ##   OLDER FAMILIES                                              542
    ##   OLDER SINGLES/COUPLES                                       701
    ##   RETIREES                                                    627
    ##   YOUNG FAMILIES                                              502
    ##   YOUNG SINGLES/COUPLES                                       472
    ##                         
    ##                          Infzns Crn Crnchers Tangy Gcamole 110g
    ##   MIDAGE SINGLES/COUPLES                                    344
    ##   NEW FAMILIES                                               93
    ##   OLDER FAMILIES                                            570
    ##   OLDER SINGLES/COUPLES                                     635
    ##   RETIREES                                                  612
    ##   YOUNG FAMILIES                                            440
    ##   YOUNG SINGLES/COUPLES                                     450
    ##                         
    ##                          Kettle 135g Swt Pot Sea Salt Kettle Chilli 175g
    ##   MIDAGE SINGLES/COUPLES                          309                310
    ##   NEW FAMILIES                                     91                 89
    ##   OLDER FAMILIES                                  534                488
    ##   OLDER SINGLES/COUPLES                           740                659
    ##   RETIREES                                        630                628
    ##   YOUNG FAMILIES                                  501                453
    ##   YOUNG SINGLES/COUPLES                           452                411
    ##                         
    ##                          Kettle Honey Soy    Chicken 175g
    ##   MIDAGE SINGLES/COUPLES                              306
    ##   NEW FAMILIES                                        107
    ##   OLDER FAMILIES                                      543
    ##   OLDER SINGLES/COUPLES                               672
    ##   RETIREES                                            612
    ##   YOUNG FAMILIES                                      480
    ##   YOUNG SINGLES/COUPLES                               428
    ##                         
    ##                          Kettle Mozzarella   Basil & Pesto 175g
    ##   MIDAGE SINGLES/COUPLES                                    326
    ##   NEW FAMILIES                                               98
    ##   OLDER FAMILIES                                            550
    ##   OLDER SINGLES/COUPLES                                     665
    ##   RETIREES                                                  669
    ##   YOUNG FAMILIES                                            488
    ##   YOUNG SINGLES/COUPLES                                     508
    ##                         
    ##                          Kettle Original 175g
    ##   MIDAGE SINGLES/COUPLES                  311
    ##   NEW FAMILIES                             90
    ##   OLDER FAMILIES                          495
    ##   OLDER SINGLES/COUPLES                   647
    ##   RETIREES                                636
    ##   YOUNG FAMILIES                          534
    ##   YOUNG SINGLES/COUPLES                   446
    ##                         
    ##                          Kettle Sea Salt     And Vinegar 175g
    ##   MIDAGE SINGLES/COUPLES                                  330
    ##   NEW FAMILIES                                             87
    ##   OLDER FAMILIES                                          531
    ##   OLDER SINGLES/COUPLES                                   679
    ##   RETIREES                                                596
    ##   YOUNG FAMILIES                                          506
    ##   YOUNG SINGLES/COUPLES                                   444
    ##                         
    ##                          Kettle Sensations   BBQ&Maple 150g
    ##   MIDAGE SINGLES/COUPLES                                294
    ##   NEW FAMILIES                                           85
    ##   OLDER FAMILIES                                        513
    ##   OLDER SINGLES/COUPLES                                 683
    ##   RETIREES                                              603
    ##   YOUNG FAMILIES                                        438
    ##   YOUNG SINGLES/COUPLES                                 467
    ##                         
    ##                          Kettle Sensations   Camembert & Fig 150g
    ##   MIDAGE SINGLES/COUPLES                                      319
    ##   NEW FAMILIES                                                 73
    ##   OLDER FAMILIES                                              541
    ##   OLDER SINGLES/COUPLES                                       704
    ##   RETIREES                                                    650
    ##   YOUNG FAMILIES                                              458
    ##   YOUNG SINGLES/COUPLES                                       474
    ##                         
    ##                          Kettle Sensations   Siracha Lime 150g
    ##   MIDAGE SINGLES/COUPLES                                   287
    ##   NEW FAMILIES                                              94
    ##   OLDER FAMILIES                                           535
    ##   OLDER SINGLES/COUPLES                                    685
    ##   RETIREES                                                 609
    ##   YOUNG FAMILIES                                           471
    ##   YOUNG SINGLES/COUPLES                                    446
    ##                         
    ##                          Kettle Sweet Chilli And Sour Cream 175g
    ##   MIDAGE SINGLES/COUPLES                                     332
    ##   NEW FAMILIES                                                91
    ##   OLDER FAMILIES                                             542
    ##   OLDER SINGLES/COUPLES                                      658
    ##   RETIREES                                                   652
    ##   YOUNG FAMILIES                                             461
    ##   YOUNG SINGLES/COUPLES                                      464
    ##                         
    ##                          Kettle Tortilla ChpsBtroot&Ricotta 150g
    ##   MIDAGE SINGLES/COUPLES                                     316
    ##   NEW FAMILIES                                                90
    ##   OLDER FAMILIES                                             525
    ##   OLDER SINGLES/COUPLES                                      701
    ##   RETIREES                                                   607
    ##   YOUNG FAMILIES                                             459
    ##   YOUNG SINGLES/COUPLES                                      448
    ##                         
    ##                          Kettle Tortilla ChpsFeta&Garlic 150g
    ##   MIDAGE SINGLES/COUPLES                                  293
    ##   NEW FAMILIES                                             86
    ##   OLDER FAMILIES                                          508
    ##   OLDER SINGLES/COUPLES                                   663
    ##   RETIREES                                                633
    ##   YOUNG FAMILIES                                          512
    ##   YOUNG SINGLES/COUPLES                                   443
    ##                         
    ##                          Kettle Tortilla ChpsHny&Jlpno Chili 150g
    ##   MIDAGE SINGLES/COUPLES                                      322
    ##   NEW FAMILIES                                                 90
    ##   OLDER FAMILIES                                              546
    ##   OLDER SINGLES/COUPLES                                       691
    ##   RETIREES                                                    669
    ##   YOUNG FAMILIES                                              516
    ##   YOUNG SINGLES/COUPLES                                       462
    ##                         
    ##                          Natural Chip        Compny SeaSalt175g
    ##   MIDAGE SINGLES/COUPLES                                    129
    ##   NEW FAMILIES                                               31
    ##   OLDER FAMILIES                                            300
    ##   OLDER SINGLES/COUPLES                                     301
    ##   RETIREES                                                  263
    ##   YOUNG FAMILIES                                            250
    ##   YOUNG SINGLES/COUPLES                                     194
    ##                         
    ##                          Natural Chip Co     Tmato Hrb&Spce 175g
    ##   MIDAGE SINGLES/COUPLES                                     156
    ##   NEW FAMILIES                                                37
    ##   OLDER FAMILIES                                             328
    ##   OLDER SINGLES/COUPLES                                      298
    ##   RETIREES                                                   262
    ##   YOUNG FAMILIES                                             295
    ##   YOUNG SINGLES/COUPLES                                      196
    ##                         
    ##                          Natural ChipCo      Hony Soy Chckn175g
    ##   MIDAGE SINGLES/COUPLES                                    128
    ##   NEW FAMILIES                                               36
    ##   OLDER FAMILIES                                            286
    ##   OLDER SINGLES/COUPLES                                     295
    ##   RETIREES                                                  250
    ##   YOUNG FAMILIES                                            290
    ##   YOUNG SINGLES/COUPLES                                     175
    ##                         
    ##                          Natural ChipCo Sea  Salt & Vinegr 175g
    ##   MIDAGE SINGLES/COUPLES                                    137
    ##   NEW FAMILIES                                               28
    ##   OLDER FAMILIES                                            340
    ##   OLDER SINGLES/COUPLES                                     304
    ##   RETIREES                                                  265
    ##   YOUNG FAMILIES                                            272
    ##   YOUNG SINGLES/COUPLES                                     204
    ##                         
    ##                          NCC Sour Cream &    Garden Chives 175g
    ##   MIDAGE SINGLES/COUPLES                                    144
    ##   NEW FAMILIES                                               26
    ##   OLDER FAMILIES                                            317
    ##   OLDER SINGLES/COUPLES                                     268
    ##   RETIREES                                                  246
    ##   YOUNG FAMILIES                                            260
    ##   YOUNG SINGLES/COUPLES                                     158
    ##                         
    ##                          Old El Paso Salsa   Dip Chnky Tom Ht300g
    ##   MIDAGE SINGLES/COUPLES                                      317
    ##   NEW FAMILIES                                                 91
    ##   OLDER FAMILIES                                              578
    ##   OLDER SINGLES/COUPLES                                       624
    ##   RETIREES                                                    608
    ##   YOUNG FAMILIES                                              480
    ##   YOUNG SINGLES/COUPLES                                       427
    ##                         
    ##                          Old El Paso Salsa   Dip Tomato Med 300g
    ##   MIDAGE SINGLES/COUPLES                                     285
    ##   NEW FAMILIES                                                83
    ##   OLDER FAMILIES                                             514
    ##   OLDER SINGLES/COUPLES                                      670
    ##   RETIREES                                                   607
    ##   YOUNG FAMILIES                                             508
    ##   YOUNG SINGLES/COUPLES                                      447
    ##                         
    ##                          Old El Paso Salsa   Dip Tomato Mild 300g
    ##   MIDAGE SINGLES/COUPLES                                      313
    ##   NEW FAMILIES                                                 71
    ##   OLDER FAMILIES                                              497
    ##   OLDER SINGLES/COUPLES                                       651
    ##   RETIREES                                                    624
    ##   YOUNG FAMILIES                                              479
    ##   YOUNG SINGLES/COUPLES                                       450
    ##                         
    ##                          Pringles Barbeque   134g
    ##   MIDAGE SINGLES/COUPLES                      312
    ##   NEW FAMILIES                                 99
    ##   OLDER FAMILIES                              543
    ##   OLDER SINGLES/COUPLES                       672
    ##   RETIREES                                    667
    ##   YOUNG FAMILIES                              479
    ##   YOUNG SINGLES/COUPLES                       438
    ##                         
    ##                          Pringles Chicken    Salt Crips 134g
    ##   MIDAGE SINGLES/COUPLES                                 295
    ##   NEW FAMILIES                                            73
    ##   OLDER FAMILIES                                         540
    ##   OLDER SINGLES/COUPLES                                  668
    ##   RETIREES                                               592
    ##   YOUNG FAMILIES                                         487
    ##   YOUNG SINGLES/COUPLES                                  449
    ##                         
    ##                          Pringles Mystery    Flavour 134g
    ##   MIDAGE SINGLES/COUPLES                              260
    ##   NEW FAMILIES                                         83
    ##   OLDER FAMILIES                                      541
    ##   OLDER SINGLES/COUPLES                               649
    ##   RETIREES                                            607
    ##   YOUNG FAMILIES                                      482
    ##   YOUNG SINGLES/COUPLES                               492
    ##                         
    ##                          Pringles Original   Crisps 134g
    ##   MIDAGE SINGLES/COUPLES                             301
    ##   NEW FAMILIES                                        93
    ##   OLDER FAMILIES                                     551
    ##   OLDER SINGLES/COUPLES                              667
    ##   RETIREES                                           617
    ##   YOUNG FAMILIES                                     457
    ##   YOUNG SINGLES/COUPLES                              471
    ##                         
    ##                          Pringles Slt Vingar 134g
    ##   MIDAGE SINGLES/COUPLES                      309
    ##   NEW FAMILIES                                 88
    ##   OLDER FAMILIES                              517
    ##   OLDER SINGLES/COUPLES                       658
    ##   RETIREES                                    583
    ##   YOUNG FAMILIES                              481
    ##   YOUNG SINGLES/COUPLES                       459
    ##                         
    ##                          Pringles SourCream  Onion 134g
    ##   MIDAGE SINGLES/COUPLES                            304
    ##   NEW FAMILIES                                       86
    ##   OLDER FAMILIES                                    541
    ##   OLDER SINGLES/COUPLES                             641
    ##   RETIREES                                          630
    ##   YOUNG FAMILIES                                    508
    ##   YOUNG SINGLES/COUPLES                             452
    ##                         
    ##                          Pringles Sthrn FriedChicken 134g
    ##   MIDAGE SINGLES/COUPLES                              288
    ##   NEW FAMILIES                                         89
    ##   OLDER FAMILIES                                      502
    ##   OLDER SINGLES/COUPLES                               658
    ##   RETIREES                                            598
    ##   YOUNG FAMILIES                                      485
    ##   YOUNG SINGLES/COUPLES                               463
    ##                         
    ##                          Pringles Sweet&Spcy BBQ 134g
    ##   MIDAGE SINGLES/COUPLES                          320
    ##   NEW FAMILIES                                     87
    ##   OLDER FAMILIES                                  509
    ##   OLDER SINGLES/COUPLES                           694
    ##   RETIREES                                        657
    ##   YOUNG FAMILIES                                  450
    ##   YOUNG SINGLES/COUPLES                           460
    ##                         
    ##                          Red Rock Deli Chikn&Garlic Aioli 150g
    ##   MIDAGE SINGLES/COUPLES                                   129
    ##   NEW FAMILIES                                              32
    ##   OLDER FAMILIES                                           305
    ##   OLDER SINGLES/COUPLES                                    286
    ##   RETIREES                                                 241
    ##   YOUNG FAMILIES                                           272
    ##   YOUNG SINGLES/COUPLES                                    169
    ##                         
    ##                          Red Rock Deli Sp    Salt & Truffle 150G
    ##   MIDAGE SINGLES/COUPLES                                     132
    ##   NEW FAMILIES                                                35
    ##   OLDER FAMILIES                                             338
    ##   OLDER SINGLES/COUPLES                                      294
    ##   RETIREES                                                   244
    ##   YOUNG FAMILIES                                             271
    ##   YOUNG SINGLES/COUPLES                                      184
    ##                         
    ##                          Red Rock Deli SR    Salsa & Mzzrlla 150g
    ##   MIDAGE SINGLES/COUPLES                                      136
    ##   NEW FAMILIES                                                 33
    ##   OLDER FAMILIES                                              302
    ##   OLDER SINGLES/COUPLES                                       282
    ##   RETIREES                                                    264
    ##   YOUNG FAMILIES                                              267
    ##   YOUNG SINGLES/COUPLES                                       174
    ##                         
    ##                          Red Rock Deli Thai  Chilli&Lime 150g
    ##   MIDAGE SINGLES/COUPLES                                  138
    ##   NEW FAMILIES                                             37
    ##   OLDER FAMILIES                                          325
    ##   OLDER SINGLES/COUPLES                                   287
    ##   RETIREES                                                288
    ##   YOUNG FAMILIES                                          242
    ##   YOUNG SINGLES/COUPLES                                   178
    ##                         
    ##                          RRD Chilli&         Coconut 150g
    ##   MIDAGE SINGLES/COUPLES                              123
    ##   NEW FAMILIES                                         33
    ##   OLDER FAMILIES                                      321
    ##   OLDER SINGLES/COUPLES                               286
    ##   RETIREES                                            255
    ##   YOUNG FAMILIES                                      296
    ##   YOUNG SINGLES/COUPLES                               192
    ##                         
    ##                          RRD Honey Soy       Chicken 165g
    ##   MIDAGE SINGLES/COUPLES                              167
    ##   NEW FAMILIES                                         30
    ##   OLDER FAMILIES                                      304
    ##   OLDER SINGLES/COUPLES                               284
    ##   RETIREES                                            236
    ##   YOUNG FAMILIES                                      299
    ##   YOUNG SINGLES/COUPLES                               193
    ##                         
    ##                          RRD Lime & Pepper   165g RRD Pc Sea Salt     165g
    ##   MIDAGE SINGLES/COUPLES                      135                      122
    ##   NEW FAMILIES                                 31                       31
    ##   OLDER FAMILIES                              335                      321
    ##   OLDER SINGLES/COUPLES                       275                      237
    ##   RETIREES                                    245                      226
    ##   YOUNG FAMILIES                              283                      288
    ##   YOUNG SINGLES/COUPLES                       169                      206
    ##                         
    ##                          RRD Salt & Vinegar  165g
    ##   MIDAGE SINGLES/COUPLES                      143
    ##   NEW FAMILIES                                 40
    ##   OLDER FAMILIES                              323
    ##   OLDER SINGLES/COUPLES                       279
    ##   RETIREES                                    256
    ##   YOUNG FAMILIES                              265
    ##   YOUNG SINGLES/COUPLES                       168
    ##                         
    ##                          RRD SR Slow Rst     Pork Belly 150g
    ##   MIDAGE SINGLES/COUPLES                                 122
    ##   NEW FAMILIES                                            31
    ##   OLDER FAMILIES                                         362
    ##   OLDER SINGLES/COUPLES                                  256
    ##   RETIREES                                               252
    ##   YOUNG FAMILIES                                         312
    ##   YOUNG SINGLES/COUPLES                                  191
    ##                         
    ##                          RRD Steak &         Chimuchurri 150g
    ##   MIDAGE SINGLES/COUPLES                                  121
    ##   NEW FAMILIES                                             32
    ##   OLDER FAMILIES                                          335
    ##   OLDER SINGLES/COUPLES                                   273
    ##   RETIREES                                                235
    ##   YOUNG FAMILIES                                          300
    ##   YOUNG SINGLES/COUPLES                                   159
    ##                         
    ##                          RRD Sweet Chilli &  Sour Cream 165g
    ##   MIDAGE SINGLES/COUPLES                                 146
    ##   NEW FAMILIES                                            23
    ##   OLDER FAMILIES                                         324
    ##   OLDER SINGLES/COUPLES                                  285
    ##   RETIREES                                               238
    ##   YOUNG FAMILIES                                         301
    ##   YOUNG SINGLES/COUPLES                                  199
    ##                         
    ##                          Smith Crinkle Cut   Bolognese 150g
    ##   MIDAGE SINGLES/COUPLES                                133
    ##   NEW FAMILIES                                           38
    ##   OLDER FAMILIES                                        330
    ##   OLDER SINGLES/COUPLES                                 277
    ##   RETIREES                                              252
    ##   YOUNG FAMILIES                                        265
    ##   YOUNG SINGLES/COUPLES                                 156
    ##                         
    ##                          Smith Crinkle Cut   Mac N Cheese 150g
    ##   MIDAGE SINGLES/COUPLES                                   134
    ##   NEW FAMILIES                                              40
    ##   OLDER FAMILIES                                           312
    ##   OLDER SINGLES/COUPLES                                    280
    ##   RETIREES                                                 266
    ##   YOUNG FAMILIES                                           298
    ##   YOUNG SINGLES/COUPLES                                    182
    ##                         
    ##                          Smiths Chip Thinly  Cut Original 175g
    ##   MIDAGE SINGLES/COUPLES                                   152
    ##   NEW FAMILIES                                              39
    ##   OLDER FAMILIES                                           339
    ##   OLDER SINGLES/COUPLES                                    298
    ##   RETIREES                                                 245
    ##   YOUNG FAMILIES                                           331
    ##   YOUNG SINGLES/COUPLES                                    210
    ##                         
    ##                          Smiths Chip Thinly  CutSalt/Vinegr175g
    ##   MIDAGE SINGLES/COUPLES                                    144
    ##   NEW FAMILIES                                               25
    ##   OLDER FAMILIES                                            311
    ##   OLDER SINGLES/COUPLES                                     291
    ##   RETIREES                                                  238
    ##   YOUNG FAMILIES                                            271
    ##   YOUNG SINGLES/COUPLES                                     160
    ##                         
    ##                          Smiths Chip Thinly  S/Cream&Onion 175g
    ##   MIDAGE SINGLES/COUPLES                                    131
    ##   NEW FAMILIES                                               39
    ##   OLDER FAMILIES                                            322
    ##   OLDER SINGLES/COUPLES                                     296
    ##   RETIREES                                                  244
    ##   YOUNG FAMILIES                                            264
    ##   YOUNG SINGLES/COUPLES                                     177
    ##                         
    ##                          Smiths Crinkle      Original 330g
    ##   MIDAGE SINGLES/COUPLES                               296
    ##   NEW FAMILIES                                          85
    ##   OLDER FAMILIES                                       566
    ##   OLDER SINGLES/COUPLES                                656
    ##   RETIREES                                             618
    ##   YOUNG FAMILIES                                       477
    ##   YOUNG SINGLES/COUPLES                                444
    ##                         
    ##                          Smiths Crinkle Chips Salt & Vinegar 330g
    ##   MIDAGE SINGLES/COUPLES                                      314
    ##   NEW FAMILIES                                                 72
    ##   OLDER FAMILIES                                              589
    ##   OLDER SINGLES/COUPLES                                       651
    ##   RETIREES                                                    604
    ##   YOUNG FAMILIES                                              502
    ##   YOUNG SINGLES/COUPLES                                       465
    ##                         
    ##                          Smiths Crinkle Cut  Chips Barbecue 170g
    ##   MIDAGE SINGLES/COUPLES                                     116
    ##   NEW FAMILIES                                                34
    ##   OLDER FAMILIES                                             355
    ##   OLDER SINGLES/COUPLES                                      280
    ##   RETIREES                                                   244
    ##   YOUNG FAMILIES                                             276
    ##   YOUNG SINGLES/COUPLES                                      184
    ##                         
    ##                          Smiths Crinkle Cut  Chips Chicken 170g
    ##   MIDAGE SINGLES/COUPLES                                    155
    ##   NEW FAMILIES                                               21
    ##   OLDER FAMILIES                                            290
    ##   OLDER SINGLES/COUPLES                                     301
    ##   RETIREES                                                  259
    ##   YOUNG FAMILIES                                            296
    ##   YOUNG SINGLES/COUPLES                                     162
    ##                         
    ##                          Smiths Crinkle Cut  Chips Chs&Onion170g
    ##   MIDAGE SINGLES/COUPLES                                     127
    ##   NEW FAMILIES                                                34
    ##   OLDER FAMILIES                                             308
    ##   OLDER SINGLES/COUPLES                                      263
    ##   RETIREES                                                   255
    ##   YOUNG FAMILIES                                             310
    ##   YOUNG SINGLES/COUPLES                                      184
    ##                         
    ##                          Smiths Crinkle Cut  Chips Original 170g
    ##   MIDAGE SINGLES/COUPLES                                     129
    ##   NEW FAMILIES                                                28
    ##   OLDER FAMILIES                                             315
    ##   OLDER SINGLES/COUPLES                                      293
    ##   RETIREES                                                   239
    ##   YOUNG FAMILIES                                             257
    ##   YOUNG SINGLES/COUPLES                                      200
    ##                         
    ##                          Smiths Crinkle Cut  French OnionDip 150g
    ##   MIDAGE SINGLES/COUPLES                                      125
    ##   NEW FAMILIES                                                 31
    ##   OLDER FAMILIES                                              293
    ##   OLDER SINGLES/COUPLES                                       304
    ##   RETIREES                                                    228
    ##   YOUNG FAMILIES                                              275
    ##   YOUNG SINGLES/COUPLES                                       182
    ##                         
    ##                          Smiths Crinkle Cut  Salt & Vinegar 170g
    ##   MIDAGE SINGLES/COUPLES                                     102
    ##   NEW FAMILIES                                                38
    ##   OLDER FAMILIES                                             320
    ##   OLDER SINGLES/COUPLES                                      290
    ##   RETIREES                                                   255
    ##   YOUNG FAMILIES                                             262
    ##   YOUNG SINGLES/COUPLES                                      188
    ##                         
    ##                          Smiths Crinkle Cut  Snag&Sauce 150g
    ##   MIDAGE SINGLES/COUPLES                                 125
    ##   NEW FAMILIES                                            37
    ##   OLDER FAMILIES                                         306
    ##   OLDER SINGLES/COUPLES                                  323
    ##   RETIREES                                               270
    ##   YOUNG FAMILIES                                         260
    ##   YOUNG SINGLES/COUPLES                                  182
    ##                         
    ##                          Smiths Crinkle Cut  Tomato Salsa 150g
    ##   MIDAGE SINGLES/COUPLES                                   166
    ##   NEW FAMILIES                                              32
    ##   OLDER FAMILIES                                           305
    ##   OLDER SINGLES/COUPLES                                    288
    ##   RETIREES                                                 239
    ##   YOUNG FAMILIES                                           274
    ##   YOUNG SINGLES/COUPLES                                    166
    ##                         
    ##                          Smiths Crnkle Chip  Orgnl Big Bag 380g
    ##   MIDAGE SINGLES/COUPLES                                    328
    ##   NEW FAMILIES                                               94
    ##   OLDER FAMILIES                                            539
    ##   OLDER SINGLES/COUPLES                                     672
    ##   RETIREES                                                  622
    ##   YOUNG FAMILIES                                            495
    ##   YOUNG SINGLES/COUPLES                                     483
    ##                         
    ##                          Smiths Thinly       Swt Chli&S/Cream175G
    ##   MIDAGE SINGLES/COUPLES                                      132
    ##   NEW FAMILIES                                                 29
    ##   OLDER FAMILIES                                              314
    ##   OLDER SINGLES/COUPLES                                       269
    ##   RETIREES                                                    261
    ##   YOUNG FAMILIES                                              280
    ##   YOUNG SINGLES/COUPLES                                       176
    ##                         
    ##                          Smiths Thinly Cut   Roast Chicken 175g
    ##   MIDAGE SINGLES/COUPLES                                    147
    ##   NEW FAMILIES                                               43
    ##   OLDER FAMILIES                                            329
    ##   OLDER SINGLES/COUPLES                                     287
    ##   RETIREES                                                  274
    ##   YOUNG FAMILIES                                            280
    ##   YOUNG SINGLES/COUPLES                                     158
    ##                         
    ##                          Snbts Whlgrn Crisps Cheddr&Mstrd 90g
    ##   MIDAGE SINGLES/COUPLES                                  132
    ##   NEW FAMILIES                                             31
    ##   OLDER FAMILIES                                          337
    ##   OLDER SINGLES/COUPLES                                   310
    ##   RETIREES                                                291
    ##   YOUNG FAMILIES                                          295
    ##   YOUNG SINGLES/COUPLES                                   180
    ##                         
    ##                          Sunbites Whlegrn    Crisps Frch/Onin 90g
    ##   MIDAGE SINGLES/COUPLES                                      117
    ##   NEW FAMILIES                                                 30
    ##   OLDER FAMILIES                                              285
    ##   OLDER SINGLES/COUPLES                                       274
    ##   RETIREES                                                    244
    ##   YOUNG FAMILIES                                              301
    ##   YOUNG SINGLES/COUPLES                                       181
    ##                         
    ##                          Thins Chips         Originl saltd 175g
    ##   MIDAGE SINGLES/COUPLES                                    126
    ##   NEW FAMILIES                                               34
    ##   OLDER FAMILIES                                            301
    ##   OLDER SINGLES/COUPLES                                     276
    ##   RETIREES                                                  250
    ##   YOUNG FAMILIES                                            270
    ##   YOUNG SINGLES/COUPLES                                     184
    ##                         
    ##                          Thins Chips Light&  Tangy 175g
    ##   MIDAGE SINGLES/COUPLES                            325
    ##   NEW FAMILIES                                       77
    ##   OLDER FAMILIES                                    542
    ##   OLDER SINGLES/COUPLES                             664
    ##   RETIREES                                          670
    ##   YOUNG FAMILIES                                    492
    ##   YOUNG SINGLES/COUPLES                             418
    ##                         
    ##                          Thins Chips Salt &  Vinegar 175g
    ##   MIDAGE SINGLES/COUPLES                              294
    ##   NEW FAMILIES                                         93
    ##   OLDER FAMILIES                                      530
    ##   OLDER SINGLES/COUPLES                               667
    ##   RETIREES                                            608
    ##   YOUNG FAMILIES                                      487
    ##   YOUNG SINGLES/COUPLES                               424
    ##                         
    ##                          Thins Chips Seasonedchicken 175g
    ##   MIDAGE SINGLES/COUPLES                              263
    ##   NEW FAMILIES                                         80
    ##   OLDER FAMILIES                                      523
    ##   OLDER SINGLES/COUPLES                               719
    ##   RETIREES                                            605
    ##   YOUNG FAMILIES                                      458
    ##   YOUNG SINGLES/COUPLES                               466
    ##                         
    ##                          Thins Potato Chips  Hot & Spicy 175g
    ##   MIDAGE SINGLES/COUPLES                                  308
    ##   NEW FAMILIES                                             94
    ##   OLDER FAMILIES                                          579
    ##   OLDER SINGLES/COUPLES                                   643
    ##   RETIREES                                                659
    ##   YOUNG FAMILIES                                          479
    ##   YOUNG SINGLES/COUPLES                                   467
    ##                         
    ##                          Tostitos Lightly    Salted 175g
    ##   MIDAGE SINGLES/COUPLES                             298
    ##   NEW FAMILIES                                        90
    ##   OLDER FAMILIES                                     514
    ##   OLDER SINGLES/COUPLES                              674
    ##   RETIREES                                           627
    ##   YOUNG FAMILIES                                     454
    ##   YOUNG SINGLES/COUPLES                              417
    ##                         
    ##                          Tostitos Smoked     Chipotle 175g
    ##   MIDAGE SINGLES/COUPLES                               319
    ##   NEW FAMILIES                                          86
    ##   OLDER FAMILIES                                       511
    ##   OLDER SINGLES/COUPLES                                650
    ##   RETIREES                                             621
    ##   YOUNG FAMILIES                                       512
    ##   YOUNG SINGLES/COUPLES                                446
    ##                         
    ##                          Tostitos Splash Of  Lime 175g Twisties Cheese     270g
    ##   MIDAGE SINGLES/COUPLES                           307                      314
    ##   NEW FAMILIES                                     101                       79
    ##   OLDER FAMILIES                                   521                      516
    ##   OLDER SINGLES/COUPLES                            715                      635
    ##   RETIREES                                         602                      627
    ##   YOUNG FAMILIES                                   501                      473
    ##   YOUNG SINGLES/COUPLES                            505                      471
    ##                         
    ##                          Twisties Cheese     Burger 250g Twisties Chicken270g
    ##   MIDAGE SINGLES/COUPLES                             282                  335
    ##   NEW FAMILIES                                        77                   77
    ##   OLDER FAMILIES                                     563                  565
    ##   OLDER SINGLES/COUPLES                              668                  646
    ##   RETIREES                                           665                  598
    ##   YOUNG FAMILIES                                     466                  473
    ##   YOUNG SINGLES/COUPLES                              448                  476
    ##                         
    ##                          Tyrrells Crisps     Ched & Chives 165g
    ##   MIDAGE SINGLES/COUPLES                                    313
    ##   NEW FAMILIES                                               88
    ##   OLDER FAMILIES                                            543
    ##   OLDER SINGLES/COUPLES                                     694
    ##   RETIREES                                                  651
    ##   YOUNG FAMILIES                                            502
    ##   YOUNG SINGLES/COUPLES                                     477
    ##                         
    ##                          Tyrrells Crisps     Lightly Salted 165g
    ##   MIDAGE SINGLES/COUPLES                                     298
    ##   NEW FAMILIES                                                99
    ##   OLDER FAMILIES                                             550
    ##   OLDER SINGLES/COUPLES                                      646
    ##   RETIREES                                                   608
    ##   YOUNG FAMILIES                                             495
    ##   YOUNG SINGLES/COUPLES                                      478
    ##                         
    ##                          Woolworths Cheese   Rings 190g
    ##   MIDAGE SINGLES/COUPLES                            125
    ##   NEW FAMILIES                                       36
    ##   OLDER FAMILIES                                    346
    ##   OLDER SINGLES/COUPLES                             281
    ##   RETIREES                                          235
    ##   YOUNG FAMILIES                                    301
    ##   YOUNG SINGLES/COUPLES                             192
    ##                         
    ##                          Woolworths Medium   Salsa 300g
    ##   MIDAGE SINGLES/COUPLES                            137
    ##   NEW FAMILIES                                       20
    ##   OLDER FAMILIES                                    301
    ##   OLDER SINGLES/COUPLES                             272
    ##   RETIREES                                          266
    ##   YOUNG FAMILIES                                    267
    ##   YOUNG SINGLES/COUPLES                             167
    ##                         
    ##                          Woolworths Mild     Salsa 300g
    ##   MIDAGE SINGLES/COUPLES                            120
    ##   NEW FAMILIES                                       34
    ##   OLDER FAMILIES                                    292
    ##   OLDER SINGLES/COUPLES                             305
    ##   RETIREES                                          240
    ##   YOUNG FAMILIES                                    283
    ##   YOUNG SINGLES/COUPLES                             217
    ##                         
    ##                          WW Crinkle Cut      Chicken 175g
    ##   MIDAGE SINGLES/COUPLES                              101
    ##   NEW FAMILIES                                         32
    ##   OLDER FAMILIES                                      322
    ##   OLDER SINGLES/COUPLES                               316
    ##   RETIREES                                            250
    ##   YOUNG FAMILIES                                      280
    ##   YOUNG SINGLES/COUPLES                               166
    ##                         
    ##                          WW Crinkle Cut      Original 175g
    ##   MIDAGE SINGLES/COUPLES                               128
    ##   NEW FAMILIES                                          35
    ##   OLDER FAMILIES                                       310
    ##   OLDER SINGLES/COUPLES                                259
    ##   RETIREES                                             246
    ##   YOUNG FAMILIES                                       269
    ##   YOUNG SINGLES/COUPLES                                163
    ##                         
    ##                          WW D/Style Chip     Sea Salt 200g
    ##   MIDAGE SINGLES/COUPLES                               105
    ##   NEW FAMILIES                                          37
    ##   OLDER FAMILIES                                       354
    ##   OLDER SINGLES/COUPLES                                272
    ##   RETIREES                                             237
    ##   YOUNG FAMILIES                                       281
    ##   YOUNG SINGLES/COUPLES                                183
    ##                         
    ##                          WW Original Corn    Chips 200g
    ##   MIDAGE SINGLES/COUPLES                            139
    ##   NEW FAMILIES                                       34
    ##   OLDER FAMILIES                                    308
    ##   OLDER SINGLES/COUPLES                             305
    ##   RETIREES                                          255
    ##   YOUNG FAMILIES                                    282
    ##   YOUNG SINGLES/COUPLES                             172
    ##                         
    ##                          WW Original Stacked Chips 160g
    ##   MIDAGE SINGLES/COUPLES                            140
    ##   NEW FAMILIES                                       37
    ##   OLDER FAMILIES                                    310
    ##   OLDER SINGLES/COUPLES                             296
    ##   RETIREES                                          234
    ##   YOUNG FAMILIES                                    270
    ##   YOUNG SINGLES/COUPLES                             200
    ##                         
    ##                          WW Sour Cream &OnionStacked Chips 160g
    ##   MIDAGE SINGLES/COUPLES                                    150
    ##   NEW FAMILIES                                               30
    ##   OLDER FAMILIES                                            328
    ##   OLDER SINGLES/COUPLES                                     287
    ##   RETIREES                                                  249
    ##   YOUNG FAMILIES                                            251
    ##   YOUNG SINGLES/COUPLES                                     188
    ##                         
    ##                          WW Supreme Cheese   Corn Chips 200g
    ##   MIDAGE SINGLES/COUPLES                                 144
    ##   NEW FAMILIES                                            35
    ##   OLDER FAMILIES                                         331
    ##   OLDER SINGLES/COUPLES                                  300
    ##   RETIREES                                               239
    ##   YOUNG FAMILIES                                         277
    ##   YOUNG SINGLES/COUPLES                                  183

``` r
chips_brand <- customers_chips_brand_dataset[
  customers_chips_brand_dataset$BRAND =="KETTLE"|
     customers_chips_brand_dataset$BRAND=="FRENCH"|
    customers_chips_brand_dataset$BRAND=="SMITHS"|
  customers_chips_brand_dataset$BRAND=="DORITOS"|
    customers_chips_brand_dataset$BRAND=="PRINGLES",]
```

``` r
ggplot(data = chips_brand) + 
  geom_bar( mapping = aes(x= BRAND), fill="steelblue")
```

![](task2_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

Density plot

``` r
ggplot(chips_brand, aes(x=TOT_SALES, fill=BRAND))+
  geom_density(alpha = 0.5)
```

![](task2_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

Density plot of the log10

``` r
ggplot(chips_brand, aes(x=log10(TOT_SALES), fill=BRAND))+
  geom_density(alpha = 0.5)
```

![](task2_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

``` r
customers_chips_brand_dataset02 <- customers_chips_brand_dataset %>% 
  select(PROD_QTY,TOT_SALES,BRAND)
```

Changing row names

``` r
row.names(customers_chips_brand_dataset02) <- paste(customers_chips_brand_dataset02$BRAND, 1:nrow(customers_chips_brand_dataset02))
```

Removing column

``` r
customers_chips_brand_dataset02 <- select(customers_chips_brand_dataset02, -BRAND)
```

Selecting 1000 samples

``` r
customers_chips_brand_dataset02 <- 
  customers_chips_brand_dataset02 %>% slice(1:1000)
```

Standardization According to various studies , clustering with
standardized attributes results into better cluster output compared to
non standardized values

``` r
customers_chips_brand_dataset02_scaled <- scale(customers_chips_brand_dataset02)
```

Implements hierarchical clustering with Euclidean distance

``` r
hc.complete <- fastcluster::hclust(dist(customers_chips_brand_dataset02_scaled),
                                   method="complete")
```

``` r
plot(hc.complete, main = "Complete Linkage", xlab = " Chips Brand", cex = 1)
```

![](task2_files/figure-gfm/unnamed-chunk-22-1.png)<!-- -->

``` r
sub_groups <- cutree(hc.complete, 3)
```

``` r
table(sub_groups)
```

    ## sub_groups
    ##   1   2   3 
    ## 162 835   3
