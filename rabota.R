library("tidyverse") 
library("tidyr") 
library("stringr") 
library("dplyr") 
library("tibble") 
library("readr") 
library("ggplot2") 
library("dplyr")
tbl = read_csv("D:/group_124/Vetrova/MathMod/eddypro.csv", skip = 1, na =c("","NA","-9999","-9999.0"), comment=c("["))
tbl = tbl[-1,]
tbl
glimpse(tbl)
tbl = select(tbl, -(roll))
tbl<-tbl[,c(-1,-3,-9,-12,-15,-18,-21,-30,-35,-70,-88:-99)]
tbl
tbl = tbl %>% mutate_if(is.character, factor)
names(tbl) = names(tbl) %>% 
str_replace_all("[!]","_emph_") %>% 
str_replace_all("[?]","_quest_") %>% 
str_replace_all("[*]","_star_") %>% 
str_replace_all("[+]","_plus_") %>% 
str_replace_all("[-]","_minus_") %>% 
str_replace_all("[@]","_at_") %>% 
str_replace_all("[$]","_dollar_") %>% 
str_replace_all("[#]","_hash_") %>% 
str_replace_all("[/]","_div_") %>% 
str_replace_all("[%]","_perc_") %>% 
str_replace_all("[&]","_amp_") %>% 
str_replace_all("[\\^]","_power_") %>% 
str_replace_all("[()]","_") 
glimpse(tbl)
#отбор показателей осенний период
tbl=tbl[tbl$DOY > 222 & tbl$DOY < 349,] 
tbl
#отбор показателей дневного света
tbl=tbl[tbl$daytime == TRUE,] 
tbl
sapply(tbl,is.numeric) 
tbl_numeric = tbl[,sapply(tbl,is.numeric)] 
tbl_numeric
cor_tbl = cor(tbl_numeric) 
cor_tbl 
cor_tbl = cor(na.omit(tbl_numeric)) 
cor_tbl 
cor_tbl = cor(na.omit(tbl_numeric)) %>% as.data.frame %>%
select(h2o_flux)
cor_tbl 
vars = row.names(cor_tbl)[cor_tbl$h2o_flux^2 > .7] %>% na.exclude 
vars
formula = as.formula(paste("h2o_flux~", paste(vars,collapse = "+"), sep = "")) 
formula
row_numbers = 1:length(tbl$date) 
teach = sample(row_numbers, floor(length(tbl$date)*.7))
test = row_numbers[-teach] 
teaching_tbl_unq = tbl[teach,] 
testing_tbl_unq = tbl[test,]
mod1 = lm(h2o_flux~(LE + rand_err_LE + rand_err_h2o_flux + 
                      un_LE + un_h2o_flux + w_div_h2o_cov)^2,data=tbl)
summary(mod1) 
predict(mod1) 
resid(mod1) 
coef(mod1) 
anova(mod1) 
plot(mod1)
mod2 = lm(h2o_flux~(LE + rand_err_h2o_flux)^2,data=tbl)
summary(mod2) 
predict(mod2) 
resid(mod2) 
coef(mod2) 
anova(mod2) 
plot(mod2)