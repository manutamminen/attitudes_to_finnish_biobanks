# Data ja koodi artikkeliin "Perinnöllinen sairastumisalttius kiinnostaa kansalaisia"


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

