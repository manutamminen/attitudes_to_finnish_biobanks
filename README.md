# Data ja koodi artikkeliin "Perinnöllinen sairastumisalttius kiinnostaa kansalaisia"

## Kyselyaineiston selitys

| Koodi | Kysymys                                                                                                                                                                                                                                                          |
|------ | -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| Q007  | Oletko antanut näytteesi biopankkiin?                                                                                                                                                                                                                            |
| Q008  | Oletko valmis antamaan näytteesi biopankkiin?                                                                                                                                                                                                                    |
| Q010  | Olisitko valmis osallistumaan kliiniseen lääketutkimukseen, jossa on mahdollisuus saada uudenlaista hoitoa (jonka vaikuttavuutta suhteessa nykyisiin hoitoihin ei vielä tunneta, ja jolla voi olla myös haittavaikutuksia), vai koetko parempana saada nykyisten |
| Q011  | Jos perimästäsi paljastuisi sairauden mahdollisesti aiheuttava geeni, haluaisitko tietää siitä?                                                                                                                                                                  |
| Q012  | Olisitko valmis tekemään konkreettisia elämäntapamuutoksia (ruokavalion muutos, liikunnan lisääminen) jos geenitutkimuksen perusteella riskisi sairastua esimerkiksi sydänsairauteen on korkeampi kuin väestöllä keskimäärin?                                    |


| Koodi       | Vastaustyyppi |
|------------ | --------------|
| Q007 - Q012 | Kyllä         |
| Q007 - Q012 | En            |
| Q007 - Q012 | En osaa sanoa |


| Koodi       | Kysymys                                                                                           |
|------------ | --------------------------------------------------------------------------------------------------|
| Q009        | Mitä mieltä olet perimään liittyvän (genomitiedon) käytöstä tutkimuksissa?                        |
| Q013        | Mihin seuraavista ikäryhmistä kuulut?                                                             |
| Tnuts2      | NUTS2                                                                                             |
| TT11edu1    | Koulutus                                                                                          |
| T_gender    | Sukupuoli                                                                                         |
| T_Q001_2cat | Oletko koskaan aiemmin ollut tai oletko parhaillaan osallisena lääketieteellisessä tutkimuksessa? |


| Koodi       | Vastaustyyppi                                                       |
|------------ | --------------------------------------------------------------------|
| Q009        | Näen perimään liittyvän tiedon käytön tutkimuksissa mahdollisuutena |
| Q009        | Perimään liittyvän tiedon käyttö tutkimuksissa huolestuttaa minua   |
| Q009        | Minulla ei ole asiasta mielipidettä                                 |
| Q013        | 16-24-vuotias                                                       |
| Q013        | 25-34-vuotias                                                       |
| Q013        | 35-44-vuotias                                                       |
| Q013        | 45-54-vuotias                                                       |
| Q013        | 55-64-vuotias                                                       |
| Tnuts2      | Helsinki-Uusimaa                                                    |
| Tnuts2      | Etelä-Suomi                                                         |
| Tnuts2      | Länsi-Suomi                                                         |
| Tnuts2      | Pohjois- ja Itä-Suomi                                               |
| Tnuts2      | Ahvenanmaa                                                          |
| TT11edu1    | Ammatillinen perustutkinto, ammattikoulu                            |
| TT11edu1    | Ylioppilas                                                          |
| TT11edu1    | Opistoasteen ammatillinen tutkinto                                  |
| TT11edu1    | Ylempi opisto-, AMK- tai alempi korkeakoulu                         |
| TT11edu1    | Yliopistotutkinto tai ylempi                                        |
| T_gender    | Nainen                                                              |
| T_gender    | Mies                                                                |
| T_Q001_2cat | Kyllä                                                               |
| T_Q001_2cat | En                                                                  |



## Lue kyselyaineisto ja valmistele visualisointi

```r

library(tidyverse)
library(readxl)
library(nnet)
library(broom)

questionnaire <-
    read_csv("questionnaire.csv")

questionnaire_fig <- 
    questionnaire %>% 
    pivot_longer(
        cols=c("Q007", "Q008", "Q010", "Q011", "Q012", "Q009"),
        names_to="Question1",
        values_to="Answer1") %>% 
    pivot_longer(
        cols= c("Q013", "Tnuts2", "TT11edu1", "T_gender", "T_Q001_2cat"),
        names_to="Question2",
        values_to="Answer2") %>% 
    mutate(
        Answer1=as.factor(Answer1),
        Answer2=as.factor(Answer2)) %>% 
    filter(complete.cases(.)) %>% 
    ggplot(data=., aes(x=Answer1, fill=Answer2)) +
    geom_bar(position="dodge") +
    facet_grid(Question1 ~ Question2, scales="free") + 
    guides(fill=FALSE)

pdf("questionnaire.pdf", useDingbats = FALSE)
print(questionnaire_fig)
dev.off()


```


## Laske vastausten prosenttiosuudet

```r

questionnaire %>% 
    pivot_longer(
        cols=c("Q007", "Q008", "Q010", "Q011", "Q012", "Q009"),
        names_to="Question1",
        values_to="Answer1") %>% 
    pivot_longer(
        cols= c("Q013", "Tnuts2", "TT11edu1", "T_gender", "T_Q001_2cat"),
        names_to="Question2",
        values_to="Answer2") %>% 
    mutate_all( ~ fct_explicit_na(as.factor(.))) %>% 
    group_by(Question1, Answer1, Question2, Answer2) %>% 
    summarise(n=n()) %>% 
    filter(Answer1 != "(Missing)",
           Answer2 != "(Missing)") %>% 
    group_by(Question1, Answer1, Question2) %>%
    mutate(n_sum=sum(n)) %>%
    ungroup %>%
    group_by(Question1, Question2) %>%
    mutate(tot_sum = sum(n), 
           perc=n_sum / tot_sum * 100) %>% 
    select(Question1, Answer1, Question2, perc) %>% 
    unique %>% 
    arrange(desc(Question2)) %>% 
    data.frame

```



## Testaa poikkeamat multinomiaalisella logistisella regressiolla

```r

prep_model <- function(dep, data)
{
    model <-
        paste(dep, "~ T_gender + Q013 + Tnuts2 + TT11edu1 + T_Q001_2cat") %>%
        as.formula
    multinom(model, data) %>%
        tidy %>%
        mutate(Dep_variable=dep)
}

map_dfr(
    c("Q007", "Q008", "Q009", "Q010", "Q011", "Q012"),
    ~ prep_model(., raaka)) %>%
    filter(p.value < 0.1,
           term != "(Intercept)")

```


## Versioinformaatio

R version 3.6.1 (2019-07-05)
Platform: x86_64-apple-darwin13.4.0 (64-bit)
Running under: macOS Catalina 10.15.1

Matrix products: default
BLAS/LAPACK: /Users/mavatam/miniconda3/lib/libopenblasp-r0.3.7.dylib

locale:
[1] C

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
 [1] broom_0.5.2     nnet_7.3-12     readxl_1.3.1    forcats_0.4.0  
 [5] stringr_1.4.0   dplyr_0.8.3     purrr_0.3.2     readr_1.3.1    
 [9] tidyr_1.0.0     tibble_2.1.3    ggplot2_3.2.1   tidyverse_1.2.1

loaded via a namespace (and not attached):
 [1] Rcpp_1.0.2       cellranger_1.1.0 pillar_1.4.2     compiler_3.6.1  
 [5] tools_3.6.1      zeallot_0.1.0    jsonlite_1.6     lubridate_1.7.4 
 [9] lifecycle_0.1.0  gtable_0.3.0     nlme_3.1-141     lattice_0.20-38 
[13] pkgconfig_2.0.3  rlang_0.4.0      cli_1.1.0        rstudioapi_0.10 
[17] haven_2.1.1      withr_2.1.2      xml2_1.2.2       httr_1.4.1      
[21] generics_0.0.2   vctrs_0.2.0      hms_0.5.1        grid_3.6.1      
[25] tidyselect_0.2.5 glue_1.3.1       R6_2.4.0         fansi_0.4.0     
[29] modelr_0.1.5     magrittr_1.5     backports_1.1.5  scales_1.0.0    
[33] rvest_0.3.4      assertthat_0.2.1 colorspace_1.4-1 utf8_1.1.4      
[37] stringi_1.4.3    lazyeval_0.2.2   munsell_0.5.0    crayon_1.3.4    
> 
