SIP R Package
================

The SIP R Package is designed to help researchers implement the
single-iteration permutation (SIP) method for large-scale biobank data
(Annis et. al. “False discovery rates for genome-wide association tests
in biobanks with thousands of phenotypes.”
<https://doi.org/10.21203/rs.3.rs-873449/v1>).

Briefly, the SIP method takes advantage of analyzing many phenotypes
across a biobank simultaneously. When a large number of phenotypes are
analyzed in parallel, a single permutation across all phenotypes
followed by genetic association analyses of the permuted data enables
estimation of false discovery rates (FDRs) across the phenome. FDR
estimates in turn help with interpretation of genetic associations in a
biobank context.

## SIP Example

The basic permutation procedure can be used with unrelated samples. SIP
requires a sample-by-variable input file containing an ID, genotypic
covariates, phenotypes, and phenotypic covariates:

``` r
sip_exampleData[1:5, 1:15]
```

    ##        FID     IID ANCESTRY SEX AGE BATCH STUDY         PC1       PC2
    ## 1 FAM00001 ID00001      AFR   1  78     1  STU1 -0.00997818 0.0363497
    ## 2 FAM00002 ID00002      AFR   1  86     1  STU1 -0.01303270 0.0349262
    ## 3 FAM00003 ID00003      AFR   1  71     1  STU1 -0.01071080 0.0345597
    ## 4 FAM00004 ID00004      EUR   2  54     1  STU1 -0.01244230 0.0364125
    ## 5 FAM00005 ID00005      EUR   2  74     1  STU1 -0.01109950 0.0370039
    ##            PC3        PC4 PHENO1 PHENO2 PHENO3 PHENO4
    ## 1 -0.002534320 0.00362118      0      0      0      0
    ## 2  0.002535730 0.00291765      1      1      0      0
    ## 3 -0.002234060 0.00399563      1      1      0      1
    ## 4 -0.000127672 0.00474610      1      1      0      1
    ## 5 -0.001726030 0.00579052      1      1      1      1

The default permutation method will permute within biological sex and
requires specification of a sex variable and male and female values as
well as ID and genotypic covariates. Additional ID variables (e.g.,
family ID) can be placed within the genotypic covariate vector. For
example:

``` r
sip(df = sip_exampleData, id.var = "IID", sex.var = "SEX", male.val = 1, female.val = 2, geno.vars = c("FID","ANCESTRY","BATCH",paste0("PC",1:4)))
```

Users may specify a seed. If not specified, a random seed will be
chosen:

``` r
sip(df = sip_exampleData, id.var = "IID", sex.var = "SEX", male.val = 1, female.val = 2, geno.vars = c("FID","ANCESTRY","BATCH",paste0("PC",1:4)), seed = 123)
```

If permutation without regard to sex is desired, the **within.sex**
argument can be set to **FALSE** and the sex variable and male and
female values are no longer required:

``` r
sip(df = sip_exampleData, id.var = "IID", within.sex = FALSE, geno.vars = c("FID","ANCESTRY","BATCH",paste0("PC",1:4)))
```

## SIP-pair Example

The paired permutation procedure should be used with related samples:

``` r
sipPair_exampleData[1:5, 1:15]
```

    ##       FID     IID BATCH SEX AGE     PC1     PC2    PC3     PC4 PHENO1 PHENO2
    ## 1 FAM0001 ID00001     1   M  72 -0.0006 -0.0006 0.0034 -0.0010      0      0
    ## 2 FAM0001 ID00002     1   F  80 -0.0006 -0.0010 0.0033  0.0007      0      0
    ## 3 FAM0002 ID00003     1   F  82  0.0052 -0.0001 0.0022  0.0001      0      0
    ## 4 FAM0002 ID00004     1   M  80  0.0055 -0.0006 0.0020  0.0015      1      0
    ## 5 FAM0003 ID00005     1   F  78 -0.0013 -0.0012 0.0007 -0.0002      0      0
    ##   PHENO3 PHENO4 PHENO5 PHENO6
    ## 1      1      0      0      1
    ## 2      1      1      0      1
    ## 3      1      0      0      0
    ## 4      1      1      1      0
    ## 5      1      0      0      0

SIP-pair requires a data frame with identity by descent (IBD)
relatedness information between individuals:

``` r
head(sipPair_relatednessData)
```

    ##         IID1    IID2 PropIBD
    ## 952  ID05824 ID05827  0.5365
    ## 2526 ID03368 ID03369  0.5018
    ## 8001 ID02703 ID03115  0.0025
    ## 8578 ID00721 ID05478  0.0012
    ## 7376 ID07132 ID07133  0.2674
    ## 3454 ID06339 ID06340  0.4998

The default permutation method will permute within biological sex and
requires specification of a sex variable and male and female values as
well as ID and genotypic covariates. Additional ID variables (e.g.,
family ID) can be placed within the genotypic covariate vector. SIP-pair
also requires specification of the relatedness data frame, pairs of ID
variables within the relatedness data frame, and an IBD variable:

``` r
sip_pair(df = sipPair_exampleData, id.var = "IID", sex.var = "SEX", male.val = "M", female.val = "F", geno.vars = c("FID","BATCH",paste0("PC",1:4)), rel.df = sipPair_relatednessData, rid.vars=c("IID1","IID2"), ibd.var="PropIBD")
```

Users may specify a seed. If not specified, a random seed will be
chosen:

``` r
sip_pair(df = sipPair_exampleData, id.var = "IID", sex.var = "SEX", male.val = "M", female.val = "F", geno.vars = c("FID","BATCH",paste0("PC",1:4)), rel.df = sipPair_relatednessData, rid.vars=c("IID1","IID2"), ibd.var="PropIBD", seed = 123)
```

If permutation without regard to sex is desired, the **within.sex**
argument can be set to **FALSE** and the sex variable and male and
female values are no longer required:

``` r
sip_pair(df = sipPair_exampleData, id.var = "IID", geno.vars = c("FID","BATCH",paste0("PC",1:4)), rel.df = sipPair_relatednessData, rid.vars=c("IID1","IID2"), ibd.var="PropIBD", within.sex = FALSE)
```

## Phenotype Correlation Plots

The SIP method maintains correlation among phenotypes:

1.  Permute data

``` r
sip_exampleData_permuted <- sip(df = sip_exampleData, id.var = "IID", sex.var = "SEX", male.val = 1, female.val = 2, geno.vars = c("FID","ANCESTRY","BATCH",paste0("PC",1:4)))
```

    ## [1] "Seed: 811831"

``` r
sip_exampleData_permuted[1:5, 1:15]
```

    ##        FID     IID ANCESTRY SEX AGE BATCH STUDY         PC1       PC2
    ## 1 FAM00001 ID00001      AFR   1  57     1  STU2 -0.00997818 0.0363497
    ## 2 FAM00002 ID00002      AFR   1  60     1  STU1 -0.01303270 0.0349262
    ## 3 FAM00003 ID00003      AFR   1  64     1  STU1 -0.01071080 0.0345597
    ## 4 FAM00004 ID00004      EUR   2  71     1  STU1 -0.01244230 0.0364125
    ## 5 FAM00005 ID00005      EUR   2  82     1  STU1 -0.01109950 0.0370039
    ##            PC3        PC4 PHENO1 PHENO2 PHENO3 PHENO4
    ## 1 -0.002534320 0.00362118      0      0      0      1
    ## 2  0.002535730 0.00291765      0      1      0      0
    ## 3 -0.002234060 0.00399563      0      1      1      0
    ## 4 -0.000127672 0.00474610      0      0      0      0
    ## 5 -0.001726030 0.00579052      0      1      1      1

2.  Plot correlations between phenotypes in the primary data

``` r
plot_phenotype_correlations(df = sip_exampleData, pheno.vars = paste0("PHENO",1:500))
```

![](README_files/figure-gfm/SIP%20plot%20phenotype%20correlations-1.png)<!-- -->

3.  Plot correlations between phenotypes in the permuted data

``` r
plot_phenotype_correlations(df = sip_exampleData_permuted, pheno.vars = paste0("PHENO",1:500))
```

![](README_files/figure-gfm/SIP%20plot%20permuted%20phenotype%20correlations-1.png)<!-- -->

## Phenotype-IBD Correlation

The SIP-pair method preserves correlation among related samples:

1.  Permute data without pairing

``` r
sipPair_exampleData_permuted <- sip(df = sipPair_exampleData, id.var = "IID", sex.var = "SEX", male.val = "M", female.val = "F", geno.vars = c("FID","BATCH",paste0("PC",1:4)))
```

    ## [1] "Seed: 442093"

``` r
sipPair_exampleData_permuted[1:5, 1:15]
```

    ##       FID     IID BATCH SEX AGE     PC1     PC2    PC3     PC4 PHENO1 PHENO2
    ## 1 FAM0001 ID00001     1   M  85 -0.0006 -0.0006 0.0034 -0.0010      1      0
    ## 2 FAM0001 ID00002     1   F  66 -0.0006 -0.0010 0.0033  0.0007      0      1
    ## 3 FAM0002 ID00003     1   F  62  0.0052 -0.0001 0.0022  0.0001      0      0
    ## 4 FAM0002 ID00004     1   M  60  0.0055 -0.0006 0.0020  0.0015      0      0
    ## 5 FAM0003 ID00005     1   F  71 -0.0013 -0.0012 0.0007 -0.0002      0      0
    ##   PHENO3 PHENO4 PHENO5 PHENO6
    ## 1      0      0      1      0
    ## 2      1      0      0      0
    ## 3      0      0      0      0
    ## 4      0      0      0      0
    ## 5      0      0      0      0

2.  Permute data with pairing

``` r
sipPair_exampleData_pairPermuted <- sip_pair(df = sipPair_exampleData, id.var = "IID", sex.var = "SEX", male.val = "M", female.val = "F", geno.vars = c("FID","BATCH",paste0("PC",1:4)), rel.df = sipPair_relatednessData, rid.vars=c("IID1","IID2"), ibd.var="PropIBD")
```

    ## [1] "Seed: 588066"

``` r
sipPair_exampleData_pairPermuted[1:5, 1:15]
```

    ##       FID     IID BATCH SEX AGE     PC1     PC2    PC3     PC4 PHENO1 PHENO2
    ## 1 FAM0001 ID00001     1   M  82 -0.0006 -0.0006 0.0034 -0.0010      1      0
    ## 2 FAM0001 ID00002     1   F  81 -0.0006 -0.0010 0.0033  0.0007      1      0
    ## 3 FAM0002 ID00003     1   F  69  0.0052 -0.0001 0.0022  0.0001      1      0
    ## 4 FAM0002 ID00004     1   M  71  0.0055 -0.0006 0.0020  0.0015      1      0
    ## 5 FAM0003 ID00005     1   F  63 -0.0013 -0.0012 0.0007 -0.0002      0      0
    ##   PHENO3 PHENO4 PHENO5 PHENO6
    ## 1      0      0      0      0
    ## 2      0      0      0      1
    ## 3      0      0      0      0
    ## 4      0      0      0      0
    ## 5      1      0      0      1

3.  Calculate pairwise correlations between samples in primary,
    permuted, and pair-permuted phenotype data. Then calculate
    correlation between pairwise phenotype correlations and pairwise IBD
    status for samples.

``` r
phenotype_IBD_correlation(df = sipPair_exampleData, id.var = "IID", rel.df = sipPair_relatednessData, rid.vars=c("IID1","IID2"), ibd.var="PropIBD", pheno.vars = paste0("PHENO",1:300))
phenotype_IBD_correlation(df = sipPair_exampleData_permuted, id.var = "IID", rel.df = sipPair_relatednessData, rid.vars=c("IID1","IID2"), ibd.var="PropIBD", pheno.vars = paste0("PHENO",1:300))
phenotype_IBD_correlation(df = sipPair_exampleData_pairPermuted, id.var = "IID", rel.df = sipPair_relatednessData, rid.vars=c("IID1","IID2"), ibd.var="PropIBD", pheno.vars = paste0("PHENO",1:300))
```

Multiple paired permutations show preserved correlation between pairwise
phenotypes and IBD status. Multiple non-paired permutations show
correlations around 0 for pairwise phenotypes and IBD status.

    ## [1] "Seed: 366"
    ## [1] "Seed: 307284"
    ## [1] "Seed: 662842"
    ## [1] "Seed: 170098"
    ## [1] "Seed: 952820"
    ## [1] "Seed: 340164"
    ## [1] "Seed: 985319"
    ## [1] "Seed: 151133"
    ## [1] "Seed: 866457"
    ## [1] "Seed: 418214"
    ## [1] "Seed: 92447"
    ## [1] "Seed: 523505"
    ## [1] "Seed: 367554"
    ## [1] "Seed: 128286"
    ## [1] "Seed: 649882"
    ## [1] "Seed: 367116"
    ## [1] "Seed: 730912"
    ## [1] "Seed: 70387"

    ##    Permutation   Primary Pair_Permuted      Permuted
    ## 1            1 0.2996147     0.1238149  0.0180828042
    ## 2            2 0.2996147     0.1905018  0.0177049375
    ## 3            3 0.2996147     0.1249722 -0.0095113809
    ## 4            4 0.2996147     0.1336003  0.0049160614
    ## 5            5 0.2996147     0.1289361  0.0078498653
    ## 6            6 0.2996147     0.1361119  0.0062080423
    ## 7            7 0.2996147     0.1386090  0.0123207028
    ## 8            8 0.2996147     0.1393477  0.0001982628
    ## 9            9 0.2996147     0.1406379  0.0138342221
    ## 10          10 0.2996147     0.1027001 -0.0159871786

## Additional Data

Summary statistics for associations with p\<1e-5 for our primary and
permuted UKB analyses (Annis et. al. “False discovery rates for
genome-wide association tests in biobanks with thousands of phenotypes.”
<https://doi.org/10.21203/rs.3.rs-873449/v1>)
