Análise de variância com mais de uma variável resposta
================
Gleynner Ghiotto

-   [1 Dados do Problema](#1-dados-do-problema)
-   [2 Verificação das
    pressuposições](#2-verificação-das-pressuposições)
    -   [2.1 Os resíduos apresentam distribuição
        Normal](#21-os-resíduos-apresentam-distribuição-normal)
    -   [2.2 Homogeneidade de variâncias dos
        resíduos](#22-homogeneidade-de-variâncias-dos-resíduos)
    -   [2.3 Os erros aleatórios sejam
        independentes](#23-os-erros-aleatórios-sejam-independentes)
-   [3 Análise de variância (ANOVA)](#3-análise-de-variância-anova)
    -   [3.1 ANOVA para o tempo gasto para
        acender](#31-anova-para-o-tempo-gasto-para-acender)
    -   [3.2 ANOVA para o tempo de
        queima](#32-anova-para-o-tempo-de-queima)
    -   [3.3 ANOVA para o nível de poluentes
        emitidos](#33-anova-para-o-nível-de-poluentes-emitidos)
-   [4 Conclusão](#4-conclusão)
-   [5 Referências](#5-referências)

<style> body {text-align: justify} </style>

<br>

**Exercício proposto:** a prática de fazer churrasco é um habito comum
entre os brasileiros aos finais de semana e datas comemorativas. Porém,
nem sempre esse costume é amigo do meio ambiente, pois a grande maioria
utiliza o carvão para se obter a brasa e, quando queimado, o carvão
libera umagrande quantidade de poluentes na atmosfera. Dessa forma, um
churrasqueiro está testando qual carvão trará mais benefícios na hora de
realizar seu trabalho. Cada dado é referente a 5 kg de carvão.

![](dados_problema.png)

1)  Faça as análises de de variâncias e estudo das médias.
2)  Com os resultados das três caracter´sticas analisadas, tente
    formular uma conclusão. <br>

# 1 Dados do Problema

``` r
Tempo_acender <- data.frame(A = c(12,16,13,16,12,15,13,14), 
                            B = c(16,17,18,15,16,14,17,15), 
                            C = c(18,16,15,19,20,16,18,17)) %>% 
                 gather(key = trat,value = y,1:3)
Tempo_acender$trat <- as.factor(Tempo_acender$trat)
resd_tempAcender <- residuals(lm(Tempo_acender$y ~ Tempo_acender$trat))
Tempo_acender <- cbind(Tempo_acender,resd_tempAcender) 
head(Tempo_acender)
```

      trat  y resd_tempAcender
    1    A 12           -1.875
    2    A 16            2.125
    3    A 13           -0.875
    4    A 16            2.125
    5    A 12           -1.875
    6    A 15            1.125

``` r
Tempo_queimando <- data.frame(A = c(2,2.3,2,2.1,1.9,2.6,2.2,1.8),
                              B = c(2.1,2,1.5,1.8,2.1,2.3,1.8,1.9),
                              C = c(1.5,1.3,1,1.4,1.6,1.3,1,0.9)) %>% 
                   gather(trat,y,1:3)
Tempo_queimando$trat <- as.factor(Tempo_queimando$trat)
resd_tempQueimando <- residuals(lm(Tempo_queimando$y ~ Tempo_queimando$trat))
Tempo_queimando <- cbind(Tempo_queimando,resd_tempQueimando)
head(Tempo_queimando)
```

      trat   y resd_tempQueimando
    1    A 2.0            -0.1125
    2    A 2.3             0.1875
    3    A 2.0            -0.1125
    4    A 2.1            -0.0125
    5    A 1.9            -0.2125
    6    A 2.6             0.4875

``` r
Poluentes_emitidos <- data.frame(A = c(8.22,7.44,7.4,8.36,7.23,8.35,7.41,7.1),
                                 B = c(9.78,9.59,9.72,8.63,8.41,9.6,8.56,9.93),
                                 C = c(8.72,7.65,8.85,8.73,7.56,7.83,8.96,7.74)) %>% 
                      gather(trat,y,1:3)
Poluentes_emitidos$trat <- as.factor(Poluentes_emitidos$trat)
resd_poluEmit <- residuals(lm(Poluentes_emitidos$y ~ Poluentes_emitidos$trat))
Poluentes_emitidos <- cbind(Poluentes_emitidos,resd_poluEmit)
head(Poluentes_emitidos)
```

      trat    y resd_poluEmit
    1    A 8.22       0.53125
    2    A 7.44      -0.24875
    3    A 7.40      -0.28875
    4    A 8.36       0.67125
    5    A 7.23      -0.45875
    6    A 8.35       0.66125

# 2 Verificação das pressuposições

## 2.1 Os resíduos apresentam distribuição Normal

-   **Teste de normalidade para tempo gasto para acender**

``` r
shapiro.test(resd_tempAcender)
```

        Shapiro-Wilk normality test

    data:  resd_tempAcender
    W = 0.95421, p-value = 0.3333

Valor de *p* igual a 0,3333 (*p* \> 0,05), logo, não significativo. Isto
indica que os dados seguem distribuição normal (hipótese H<sub>0</sub> é
verdadeira).

-   **Teste de normalidade para o tempo de queima**

``` r
shapiro.test(resd_tempQueimando)
```

        Shapiro-Wilk normality test

    data:  resd_tempQueimando
    W = 0.9841, p-value = 0.9578

Valor de *p* igual a 0,9578 (*p* \> 0,05), logo, os dados seguem
distribuição normal (hipótese H<sub>0</sub> é verdadeira).

-   **Teste de normalidade para os poluentes emitidos**

``` r
shapiro.test(resd_poluEmit)
```

        Shapiro-Wilk normality test

    data:  resd_poluEmit
    W = 0.85707, p-value = 0.002966

Valor de *p* igual a 0,002966 (*p* ≤ 0,05), logo, o teste é
significativo. Isto indica que os dados não seguem distribuição normal
(hipótese H<sub>0</sub> é falsa). Apesar do teste de normalidade ser
significativo para o nível de poluentes emitidos, Baydili e Sirgile
(2007) e Milliken e Johnson (1992) afirmam que a sensibilidade do teste
F da ANOVA é mais afetada pela ausência de homogeneidade de variância
das populações.

## 2.2 Homogeneidade de variâncias dos resíduos

-   **Teste de homogeneidade de variâncias para tempo gasto para
    acender**

``` r
bartlett.test(resd_tempAcender ~ trat, data = Tempo_acender)
```

        Bartlett test of homogeneity of variances

    data:  resd_tempAcender by trat
    Bartlett's K-squared = 0.47845, df = 2, p-value = 0.7872

Valor *p* \> 0,05, logo, o teste é não signicativo (isto é, as
variâncias amostrais são homogêneas).

-   **Teste de homogeneidade de variâncias para tempo de queima**

``` r
bartlett.test(resd_tempQueimando ~ trat, data = Tempo_queimando)
```

        Bartlett test of homogeneity of variances

    data:  resd_tempQueimando by trat
    Bartlett's K-squared = 0.0155, df = 2, p-value = 0.9923

Valor *p* \> 0,05, logo, as variâncias amostrais são homogêneas.

-   **Teste de homogeneidade de variâncias para os poluentes emitidos**

``` r
bartlett.test(resd_poluEmit ~ trat, data = Poluentes_emitidos)
```

        Bartlett test of homogeneity of variances

    data:  resd_poluEmit by trat
    Bartlett's K-squared = 0.218, df = 2, p-value = 0.8967

Valor *p* \> 0,05, logo, o teste é não signicativo.

## 2.3 Os erros aleatórios sejam independentes

O uso da casualização na distribuição das amostras experimentais faz com
que a estimativa do erro experimental tenha validade, além de fazer com
que as amostras sejam independentes.

# 3 Análise de variância (ANOVA)

## 3.1 ANOVA para o tempo gasto para acender

``` r
ANOVA_DIC_TempAc <- aov(y ~ trat, data = Tempo_acender)
Saida_DIC_TempAc <- summary(ANOVA_DIC_TempAc);Saida_DIC_TempAc
```

                Df Sum Sq Mean Sq F value   Pr(>F)    
    trat         2  49.75  24.875   10.29 0.000766 ***
    Residuals   21  50.75   2.417                     
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Valor de *p* igual a 0,000766 (*p* ≤ 0,05), logo, o teste é
significativo. Isto indica que pelo menos uma das médias dos tratamentos
difere das demais.

``` r
#str(Saida_DIC_TempAc)
str(Saida_DIC_TempAc[[1]])
```

    Classes 'anova' and 'data.frame':   2 obs. of  5 variables:
     $ Df     : num  2 21
     $ Sum Sq : num  49.8 50.8
     $ Mean Sq: num  24.88 2.42
     $ F value: num  10.3 NA
     $ Pr(>F) : num  0.000766 NA

``` r
sq.res <- Saida_DIC_TempAc[[1]]$`Sum Sq`[2]
gl.res <- Saida_DIC_TempAc[[1]]$Df[2]

tukey(Tempo_acender$y,Tempo_acender$trat,
      DFerror = gl.res, 
      SSerror = sq.res, 
      alpha = 0.05)
```

    Teste de Tukey
    ------------------------------------------------------------------------
    Grupos Tratamentos Medias
    a    C   17.375 
    a    B   16 
     b   A   13.875 
    ------------------------------------------------------------------------

## 3.2 ANOVA para o tempo de queima

``` r
ANOVA_TempQ <- aov(y ~ trat, data = Tempo_queimando)
Saida_DIC_TempQ <- summary(ANOVA_TempQ);Saida_DIC_TempQ
```

                Df Sum Sq Mean Sq F value   Pr(>F)    
    trat         2  3.326  1.6629   26.31 1.91e-06 ***
    Residuals   21  1.327  0.0632                     
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Valor de *p* igual a 1,91e-06 (*p* ≤ 0,05), logo, o teste indica que
pelo menos uma das médias amostrais difere das demais.

``` r
#str(Saida_DIC_TempQ)
str(Saida_DIC_TempQ[[1]])
```

    Classes 'anova' and 'data.frame':   2 obs. of  5 variables:
     $ Df     : num  2 21
     $ Sum Sq : num  3.33 1.33
     $ Mean Sq: num  1.6629 0.0632
     $ F value: num  26.3 NA
     $ Pr(>F) : num  1.91e-06 NA

``` r
sq.res <- Saida_DIC_TempQ[[1]]$`Sum Sq`[2]
gl.res <- Saida_DIC_TempQ[[1]]$Df[2]

tukey(Tempo_queimando$y,Tempo_queimando$trat,
      DFerror = gl.res, 
      SSerror = sq.res, 
      alpha = 0.05)
```

    Teste de Tukey
    ------------------------------------------------------------------------
    Grupos Tratamentos Medias
    a    A   2.1125 
    a    B   1.9375 
     b   C   1.25 
    ------------------------------------------------------------------------

## 3.3 ANOVA para o nível de poluentes emitidos

``` r
ANOVA_PoluEmit <- aov(y ~ trat, data = Poluentes_emitidos)
Saida_DIC_PoluEmit <- summary(ANOVA_PoluEmit);Saida_DIC_PoluEmit
```

                Df Sum Sq Mean Sq F value   Pr(>F)    
    trat         2  10.37   5.187   14.92 9.29e-05 ***
    Residuals   21   7.30   0.348                     
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Valor de *p* igual a 9,29e-05 (*p* ≤ 0,05), logo, o teste é
significativo (pelo menos uma das médias difere das demais).

``` r
#str(Saida_DIC_PoluEmit)
str(Saida_DIC_PoluEmit[[1]])
```

    Classes 'anova' and 'data.frame':   2 obs. of  5 variables:
     $ Df     : num  2 21
     $ Sum Sq : num  10.4 7.3
     $ Mean Sq: num  5.187 0.348
     $ F value: num  14.9 NA
     $ Pr(>F) : num  9.29e-05 NA

``` r
sq.res <- Saida_DIC_PoluEmit[[1]]$`Sum Sq`[2]
gl.res <- Saida_DIC_PoluEmit[[1]]$Df[2]

tukey(Poluentes_emitidos$y,Poluentes_emitidos$trat,
      DFerror = gl.res, 
      SSerror = sq.res, 
      alpha = 0.05)
```

    Teste de Tukey
    ------------------------------------------------------------------------
    Grupos Tratamentos Medias
    a    B   9.2775 
     b   C   8.255 
     b   A   7.68875 
    ------------------------------------------------------------------------

# 4 Conclusão

De acordo com o teste da ANOVA, que foi significativo para as três
características em estudo, e o teste de comparação de médias de Tukey,
escolhe-se carvão A. Segundo os resultados, este carvão é um dos que
emite menores níveis de poluentes, está entre os que tem maior tempo de
queima e é o carvão que gasta o menor tempo para acender.

# 5 Referências

BAYDILI, K. N.; SIĞIRLI, D. Comparison of Variance Homogenity Tests for
Different Distributions. Turkiye Klinikleri Journal of Biostatistics, v.
9, n. 3, p. 197–212, 2017.

MILLIKEN, G. A.; JOHNSON, D. E. Analysis of Messy Data Volume 1:
Designed Experiments. 1. ed. Londres: Chapman and Hall, 1992.
