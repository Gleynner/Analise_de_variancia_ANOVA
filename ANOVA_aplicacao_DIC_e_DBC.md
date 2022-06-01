Exercício de aplicação do DIC e DBC
================
Gleynner Ghiotto

-   [1 **Exercício proposto para o
    DIC**](#1-exercício-proposto-para-o-dic)
    -   [1.1 Dados do probelma](#11-dados-do-probelma)
    -   [1.2 Suposições para a realização da
        ANOVA:](#12-suposições-para-a-realização-da-anova)
        -   [1.2.1 Os erros seguem distribuição
            normal](#121-os-erros-seguem-distribuição-normal)
        -   [1.2.2 Os erros devem apresentar variâncias
            homogêneas](#122-os-erros-devem-apresentar-variâncias-homogêneas)
        -   [1.2.3 Os erros são
            independentes](#123-os-erros-são-independentes)
    -   [1.3 Análise de variância
        (ANOVA)](#13-análise-de-variância-anova)
    -   [1.4 Coeficiente de variação](#14-coeficiente-de-variação)
-   [2 **Exercício proposto para o
    DBC**](#2-exercício-proposto-para-o-dbc)
    -   [2.1 Dados do probelma](#21-dados-do-probelma)
    -   [2.2 Análise de variância
        (ANOVA)](#22-análise-de-variância-anova)
    -   [2.3 Coeficiente de variação](#23-coeficiente-de-variação)

<style> body {text-align: justify} </style>

<br>

# 1 **Exercício proposto para o DIC**

Suponha que um melhorista de milho desenvolveu quatro híbridos *h1*,
*h2*, *h3* e *h4*. Esses híbridos foram identificados de acordo com os
potenciais produtivos das linhagens que os originaram, ou seja,
espera-se que o híbrido *h1* seja o mais produtivo e o híbrido *h4* seja
o menos produtivo. O melhorista, com base em conhecimento prévio, define
a unidade experimental (UE) como sendo uma área de 4 m<sup>2</sup>
(constituída de 25 plantas) e distribui os híbridos às unidades
experimentais de forma aleatória. Identifique o híbrido que apresenta a
maior média de produção de grãos (kg).

![](dic.png)

## 1.1 Dados do probelma

``` r
# Dados do problema
trat <- factor(rep(c("h1", "h2", "h3", "h4"),  each = 5))
y <- c(45,43,39,41,42,48,52,55,54,51, 52,56,58,55,59, 66,59,61,64,60)
res <- residuals(lm(y ~ trat)) %>% round(2)
df_dic <- data.frame(trat,y,res)

head(df_dic)
```

      trat  y res
    1   h1 45   3
    2   h1 43   1
    3   h1 39  -3
    4   h1 41  -1
    5   h1 42   0
    6   h2 48  -4

## 1.2 Suposições para a realização da ANOVA:

### 1.2.1 Os erros seguem distribuição normal

``` r
# Obs.: no R notebook, executar qqnorm e qqline simultaneamente.
qqnorm(df_dic$res, pch = 1, frame = FALSE,  ylab = "Erros")
qqline(df_dic$res, col = "steelblue", lwd = 2)
```

<img src="aplicacao_DIC_e_DBC_files/figure-gfm/unnamed-chunk-3-1.png" style="display: block; margin: auto;" />

``` r
# Teste de normalidade de Shapiro-Wilk
shapiro.test(df_dic$res)
```

        Shapiro-Wilk normality test

    data:  df_dic$res
    W = 0.9465, p-value = 0.3171

``` r
# H0: os erros aleatórios seguem uma distribuição normal
# Ha: os erros aleatórios não seguem uma distribuição normal
```

Valor de *p* igual a 0,3171 (*p* \> 0,05), logo, não significativo. Isto
indica que os dados seguem distribuição normal (hipótese H<sub>0</sub> é
verdadeira).

### 1.2.2 Os erros devem apresentar variâncias homogêneas

``` r
# Dispersão dos resíduos por tratamento
ggplot(data = df_dic) +
  geom_point(aes(x = trat, y=res, color = trat), size = 3) +
  labs(x = "Fator em estudo", y = "Resíduos") +
  theme_bw()
```

<img src="aplicacao_DIC_e_DBC_files/figure-gfm/unnamed-chunk-4-1.png" style="display: block; margin: auto;" />

``` r
# teste de Bartlett para homogeneidade de variâncias
bartlett.test(res ~ trat, data = df_dic)
```

        Bartlett test of homogeneity of variances

    data:  res by trat
    Bartlett's K-squared = 0.27216, df = 3, p-value = 0.9652

``` r
# H0: os i tratamentos possuem variâncias homogêneas
# Ha: a variância de pelo menos um dos tratamentos se difere dos demais
```

Valor *p* \> 0,05, logo, o teste é não signicativo (isto é, as
variâncias amostrais são homogêneas).

### 1.2.3 Os erros são independentes

O uso da casualização faz com que a estimativa do erro experimental seja
válida, além de fazer com que as amostras sejam independentes.

## 1.3 Análise de variância (ANOVA)

``` r
# Teste ANOVA
anova.dic <-aov(y ~ trat, data = df_dic)
saida.dic <- summary(anova.dic);saida.dic # summary() retorna uma lista
```

                Df Sum Sq Mean Sq F value   Pr(>F)    
    trat         3   1060   353.3   49.59 2.52e-08 ***
    Residuals   16    114     7.1                     
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

*p* ≤ 0,05, portanto, o teste é significativo.

``` r
#str(saida.dic)
str(saida.dic[[1]])
```

    Classes 'anova' and 'data.frame':   2 obs. of  5 variables:
     $ Df     : num  3 16
     $ Sum Sq : num  1060 114
     $ Mean Sq: num  353.33 7.12
     $ F value: num  49.6 NA
     $ Pr(>F) : num  2.52e-08 NA

``` r
sq.res <- saida.dic[[1]]$`Sum Sq`[2]
gl.res <- saida.dic[[1]]$Df[2]

#Teste de Tukey - expdes.pt
tukey(y, trat, DFerror = gl.res, SSerror = sq.res, alpha = 0.05)
```

    Teste de Tukey
    ------------------------------------------------------------------------
    Grupos Tratamentos Medias
    a    h4      62 
     b   h3      56 
     b   h2      52 
      c      h1      42 
    ------------------------------------------------------------------------

Como o teste da ANOVA foi significativo (*p* ≤ 0,05), pelo menos uma das
médias difere das demais. Por meio de teste de comparação de médias de
Tukey é possível observar que o híbrido 4 é o que apresenta maior média
de produção de grãos.

## 1.4 Coeficiente de variação

``` r
qm.res <- saida.dic[[1]]$`Mean Sq`[2]
m <- mean(df_dic$y)

cv <- sqrt(qm.res)/m; cv
```

    ## [1] 0.05036358

``` r
#    CV           Precisão
#  < 10%           Alta
#  10 a 20%        Média
#  20 a 30%        Baixa
#  > 30%           Muito Baixa
```

Como o coeficiente de variação foi igual a 5,04% o experimento foi de
alta precisão.

<br>

# 2 **Exercício proposto para o DBC**

Suponha que um melhorista estuda os mesmos quatro híbridos de milho
(*h1*, *h2*, *h3* e *h4*) do exercício anterior, contudo, agora a área
experimental não é mais uniforme e se divide em dois blocos de áreas
homogêneas. Para cada bloco o melhorista distribui os híbridos às
unidades experimentais de forma aleatória. Sabendo disso, identifique o
híbrido que apresenta a maior média de produção de grãos (kg).

![](dbc.png)

Para resolver esse exercício foi considerado que todas as pressuposições
foram satisfeitas.

## 2.1 Dados do probelma

``` r
# Dados do problema
trat <- factor(rep(c("h1", "h2", "h3", "h4"),times = 2))
bloco <- factor(rep(c(1, 2),each = 4))
y <- c(43,46,44,45,14,18,19,16)
df_dbc <- data.frame(trat,bloco,y); df_dbc
```

      trat bloco  y
    1   h1     1 43
    2   h2     1 46
    3   h3     1 44
    4   h4     1 45
    5   h1     2 14
    6   h2     2 18
    7   h3     2 19
    8   h4     2 16

## 2.2 Análise de variância (ANOVA)

``` r
# Teste ANOVA
anova.dbc <-aov(y ~ trat + bloco, data = df_dbc)
saida.dbc <- summary(anova.dbc);saida.dbc # summary() retorna uma lista
```

                Df Sum Sq Mean Sq F value   Pr(>F)    
    trat         3   14.4     4.8   2.674     0.22    
    bloco        1 1540.1  1540.1 859.605 8.71e-05 ***
    Residuals    3    5.4     1.8                     
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Como o valor de *p* é igual a 0,22 (*p* \> 0,05), os híbridos possuem a
mesma média de produção de grãos (hipótese H0 é verdadeira).

## 2.3 Coeficiente de variação

``` r
qm.res <- saida.dbc[[1]]$`Mean Sq`[3]
m <- mean(df_dbc$y)
cv <- sqrt(qm.res)/m; cv
```

    ## [1] 0.04370715

``` r
#    CV           Precisão
#  < 10%           Alta
#  10 a 20%        Média
#  20 a 30%        Baixa
#  > 30%           Muito Baixa
```

O coeficiente de variação é igual a 4,37%, o experimento foi realizado
com alta precisão.
