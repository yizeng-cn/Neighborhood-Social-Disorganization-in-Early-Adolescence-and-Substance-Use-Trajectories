###############################################################################################################################################
######################################### data pre-processing #################################################################################
###############################################################################################################################################

#Tobacco use: 888 was treated as 0, and 999 was treated as missing.For tobacco use at T4 and T5, which has no 'zero'answer in the question for this variable,we set the 999 as 0 based on the constructed variables.
#Daily prevalence (smoke every day or not) was not used since we found that zero value of this variable does not necessaily indicate no tobacco use in the past 4 weeks when comparing the values of these two variables.
#Please note that participants who reported quiting and with the -888/-999 cell (4 weeks) all had -999 in the constructed month prevelance.

#Alcohol use: from T1 to T3, all -888 and -999 were treated as missing.From T4 to T5,we filled the missingness of quantity variables based on the frequency variables(zero).
#Specifically, missingness of the quantity variables was filled based on the zero frequency of the correponding days (week or weekend days).

#Cannabis use: all -888 and -999 were set as missing.


all <- Zeng_Y_250424_all %>%
######################################### fill -888 and -999 in the tobacco-use variable #####################################################
  mutate(c2rad3a = ifelse(c2rad3 == -888, 0, c2rad3)) %>%        
  mutate(c3rad3a = ifelse(c3rad3 == -888, 0, c3rad3)) %>%
  mutate(c4rad3a = ifelse(c4rad3 == -888, 0, 
                          ifelse ((c4rad3 == -999) & (c4tobMP == 0 | c4tobLP == 0 | c4rad1 == 3 ), 0, c4rad3))) %>%
  mutate(c5rad3a = ifelse(c5rad3 == -888, 0,
                          ifelse((c5rad3 == -999) & (c5tobMP == 0 | c5tobLP == 0 | c5rad1 == 3), 0, c5rad3))) %>%
######################################### recode the tobacco-use variable ####################################################################
  mutate(c1toba = case_when(c1as8 == 0 ~ 0,        #recoded as binary
                            c1as8 == 1 ~ 1,
                            c1as8 == 2 ~ 1,
                            c1as8 == 3 ~ 1,
                            c1as8 == 4 ~ 1,
                            c1as8 == 9 ~ NA)) %>%
  mutate(c2toba = ifelse(c2rad3a == -888 | c2rad3a == -999 | is.na(c2rad3a) , NA,
                         ifelse(c2rad3a == 0|c2rad3a == 1, 0,
                                ifelse(c2rad3a == 2|c2rad3a == 3, 1,
                                       ifelse(c2rad3a == 4, 3,
                                              ifelse(c2rad3a == 5, 8,
                                                     ifelse(c2rad3a == 6, 15, 21))))))) %>%
  mutate(c3toba = ifelse(c3rad3a == -888 | c3rad3a == -999 | is.na(c3rad3a) , NA, 
                         ifelse(c3rad3a == 0|c3rad3a == 1, 0,
                                ifelse(c3rad3a == 2|c3rad3a == 3, 1,
                                       ifelse(c3rad3a == 4, 3,
                                              ifelse(c3rad3a == 5, 8,
                                                     ifelse(c3rad3a == 6, 15, 21))))))) %>%
  mutate(c4toba = ifelse(c4rad3a == -888 | c4rad3a == -999 | is.na(c4rad3a) , NA, 
                         ifelse(c4rad3a == 0|c4rad3a == 1, 0,
                                ifelse(c4rad3a == 2|c4rad3a == 3, 1,
                                       ifelse(c4rad3a == 4, 3,
                                              ifelse(c4rad3a == 5, 8,
                                                     ifelse(c4rad3a == 6, 15, 
                                                            ifelse(c4rad3a == 7, 25, 31)))))))) %>%
  mutate(c5toba = ifelse(c5rad3a == -888 | c5rad3a == -999 | is.na(c5rad3a) , NA, 
                         ifelse(c5rad3a == 0|c5rad3a == 1, 0,
                                ifelse(c5rad3a == 2|c5rad3a == 3, 1,
                                       ifelse(c5rad3a == 4, 3,
                                              ifelse(c5rad3a == 5, 8,
                                                     ifelse(c5rad3a == 6, 15, 
                                                            ifelse(c5rad3a == 7, 25, 31)))))))) %>%
######################################### fill -888 and -999 in the alcohol-use variable #####################################################
  mutate(c2rad6a = c2rad6) %>%                                     
  mutate(c2rad7a = c2rad7) %>%
  mutate(c2rad8a = c2rad8) %>%
  mutate(c2rad9a = c2rad9) %>%
  mutate(c3rad6a = c3rad6) %>%
  mutate(c3rad7a = c3rad7) %>%
  mutate(c3rad8a = c3rad8) %>%
  mutate(c3rad9a = c3rad9) %>%
  mutate(c4rad14a = c4rad14) %>%
  mutate(c4rad15a = ifelse((c4rad14 == 0), 0, c4rad15)) %>%
  mutate(c4rad16a = c4rad16) %>%
  mutate(c4rad17a = ifelse((c4rad16 == 0), 0, c4rad17)) %>%
  mutate(c5rad15a = c5rad15) %>%
  mutate(c5rad16a = ifelse((c5rad15 == 0), 0, c5rad16)) %>%
  mutate(c5rad17a = c5rad17) %>%
  mutate(c5rad18a = ifelse((c5rad17 == 0), 0, c5rad18)) %>%
######################################### recode the alcohol-use variable ####################################################################
  mutate(c1alco = case_when(c1as7 == 0 ~ 0,       #recoded as binary
                            c1as7 == 1 ~ 1,
                            c1as7 == 2 ~ 1,
                            c1as7 == 3 ~ 1,
                            c1as7 == 4 ~ 1,
                            c1as7 == 9 ~ NA)) %>%
  mutate(c2alco = ifelse(is.na(c2rad6a) | is.na(c2rad7a) | is.na(c2rad8a) | is.na(c2rad9a) | 
                           c2rad6a == -888 | c2rad6a == -999 | c2rad7a == -888 | c2rad7a == -999 | c2rad8a == -888 | c2rad8a == -999 | c2rad9a == -888 | c2rad9a == -999, NA,
                         c2rad6a * ifelse(c2rad7a == 7, 8, ifelse(c2rad7a == 8, 11, c2rad7a)) 
                         +c2rad8a * ifelse(c2rad9a == 7, 8, ifelse(c2rad9a == 8, 12, ifelse(c2rad9a == 9, 17, ifelse(c2rad9a == 10, 20, c2rad9a)))))) %>%
  mutate(c3alco = ifelse(is.na(c3rad6a) | is.na(c3rad7a) | is.na(c3rad8a) | is.na(c3rad9a) | 
                           c3rad6a == -888 | c3rad6a == -999 | c3rad7a == -888 | c3rad7a == -999 | c3rad8a == -888 | c3rad8a == -999 | c3rad9a == -888 | c3rad9a == -999, NA,
                         c3rad6a * ifelse(c3rad7a == 7, 8, ifelse(c3rad7a == 8, 11, c3rad7a)) 
                         +c3rad8a * ifelse(c3rad9a == 7, 8, ifelse(c3rad9a == 8, 12, ifelse(c3rad9a == 9, 17, ifelse(c3rad9a == 10, 20, c3rad9a)))))) %>%
  mutate(c4alco = ifelse(is.na(c4rad14a) | is.na(c4rad15a) | is.na(c4rad16a) | is.na(c4rad17a) | 
                           c4rad14a == -888 | c4rad14a == -999 | c4rad15a == -888 | c4rad15a == -999 | c4rad16a == -888 | c4rad16a == -999 | c4rad17a == -888 | c4rad17a == -999, NA,
                         c4rad14a * ifelse(c4rad15a == 7, 8, ifelse(c4rad15a == 8, 12, ifelse(c4rad15a == 9, 17, ifelse(c4rad15a == 10, 20, c4rad15a)))) 
                         +c4rad16a * ifelse(c4rad17a == 7, 8, ifelse(c4rad17a == 8, 12, ifelse(c4rad17a == 9, 17, ifelse(c4rad17a == 10, 20, c4rad17a)))))) %>%
  mutate(c5alco = ifelse(is.na(c5rad15a) | is.na(c5rad16a) | is.na(c5rad17a) | is.na(c5rad18a) |
                           c5rad15a == -888 | c5rad15a == -999 | c5rad16a == -888 | c5rad16a == -999 | c5rad17a == -888 | c5rad17a == -999 | c5rad18a == -888 | c5rad18a== -999, NA,
                         c5rad15a * ifelse(c5rad16a == 7, 8, ifelse(c5rad16a == 8, 12, ifelse(c5rad16a == 9, 17, ifelse(c5rad16a == 10, 20, c5rad16a)))) 
                         +c5rad17a * ifelse(c5rad18a == 7, 8, ifelse(c5rad18a == 8, 12, ifelse(c5rad18a == 9, 17, ifelse(c5rad18a == 10, 20, c5rad18a)))))) %>%
######################################### fill -888 and -999 in the cannabis-use variable ##################################################### 
  mutate(c2rad13ca = c2rad13c) %>%                 
  mutate(c3rad13ca = c3rad13c) %>% 
  mutate(c4rad47a = c4rad47) %>% 
  mutate(c5rad39a = c5rad39) %>%
######################################### recode the cannabis-use variable ####################################################################
  mutate(c1canna = case_when(c1as9 == 0 ~ 0,
                           c1as9 == 1 ~ 1,
                           c1as9 == 2 ~ 1,
                           c1as9 == 3 ~ 1,
                           c1as9 == 4 ~ 1,
                           c1as9 == 9 ~ NA)) %>%
  mutate(c2canna = ifelse(c2rad13ca ==  -888 | c2rad13ca == -999 | is.na(c2rad13ca), NA, 
                          ifelse(c2rad13ca == 11, 15, 
                                 ifelse(c2rad13ca == 12, 29, 
                                        ifelse(c2rad13ca == 13, 40, c2rad13ca))))) %>%
  mutate(c3canna = ifelse(c3rad13ca ==  -888 | c3rad13ca == -999 | is.na(c3rad13ca), NA, 
                          ifelse(c3rad13ca == 11, 15, 
                                 ifelse(c3rad13ca == 12, 29, 
                                        ifelse(c3rad13ca == 13, 40, c3rad13ca))))) %>%
  mutate(c4canna = ifelse(c4rad47a ==  -888 | c4rad47a == -999 | is.na(c4rad47a), NA, 
                          ifelse(c4rad47a == 11, 15, 
                                 ifelse(c4rad47a == 12, 29, 
                                        ifelse(c4rad47a == 13, 40, c4rad47a))))) %>%
  mutate(c4canna = ifelse(c4rad47a ==  -888 | c4rad47a == -999 | is.na(c4rad47a), NA, 
                          ifelse(c4rad47a == 11, 15, 
                                 ifelse(c4rad47a == 12, 29, 
                                        ifelse(c4rad47a == 13, 40, c4rad47a))))) %>%
  mutate(c5canna = ifelse(c5rad39a ==  -888 | c5rad39a == -999 | is.na(c5rad39a), NA, 
                          ifelse(c5rad39a == 11, 15, 
                                 ifelse(c5rad39a == 12, 29, 
                                        ifelse(c5rad39a == 13, 40, c5rad39a))))) %>%
######################################### recode the parental tobacco-use variable ####################################################################
  mutate(p1toba = ifelse(is.na(p1av5a), NA,
                         ifelse(p1av5a == 1, 0,
                                ifelse(p1av5a == 2, 1,
                                       ifelse(p1av5a == 3, 5,
                                              ifelse(p1av5a == 4, 15,
                                                     ifelse(p1av5a == 5, 30, 41))))))) %>%
  mutate(p1toba_partner = ifelse(is.na(p1av5b), NA,
                                 ifelse(p1av5b == 1, 0,
                                        ifelse(p1av5b == 2, 1,
                                               ifelse(p1av5b == 3, 5,
                                                      ifelse(p1av5b == 4, 15,
                                                             ifelse(p1av5b == 5, 30, 41))))))) %>%
  mutate(p1toba_av = ifelse(is.na(p1toba) & is.na(p1toba_partner), NA,
                            ifelse(is.na(p1toba), p1toba_partner,
                                   ifelse(is.na(p1toba_partner), p1toba, ceiling((p1toba+p1toba_partner)/2))))) %>%
  mutate(p1toba_hi = ifelse(is.na(p1toba) & is.na(p1toba_partner), NA,
                            ifelse(is.na(p1toba), p1toba_partner,
                                   ifelse(is.na(p1toba_partner), p1toba, pmax(p1toba,p1toba_partner))))) %>%
  mutate(p2toba = ifelse(is.na(p2av8a), NA,
                         ifelse(p2av8a == 0|p2av8a == 1, 0,
                                ifelse(p2av8a == 2|p2av8a == 3, 1,
                                       ifelse(p2av8a == 4, 3,
                                              ifelse(p2av8a == 5, 8,
                                                     ifelse(p2av8a == 6, 15, 21))))))) %>%
  mutate(p2toba_partner = ifelse(is.na(p2av8b), NA,
                                 ifelse(p2av8b == 0|p2av8b == 1, 0,
                                        ifelse(p2av8b == 2|p2av8b == 3, 1,
                                               ifelse(p2av8b == 4, 3,
                                                      ifelse(p2av8b == 5, 8,
                                                             ifelse(p2av8b == 6, 15, 21))))))) %>%
  mutate(p2toba_av = ifelse(is.na(p2toba) & is.na(p2toba_partner), NA,
                            ifelse(is.na(p2toba), p2toba_partner,
                                   ifelse(is.na(p2toba_partner), p2toba, ceiling((p2toba+p2toba_partner)/2))))) %>%
  mutate(p2toba_hi = ifelse(is.na(p2toba) & is.na(p2toba_partner), NA,
                            ifelse(is.na(p2toba), p2toba_partner,
                                   ifelse(is.na(p2toba_partner), p2toba, pmax(p2toba,p2toba_partner))))) %>%
  mutate(p3toba = ifelse(is.na(p3hb7a), NA,
                         ifelse(p3hb7a == 0|p3hb7a == 1, 0,
                                ifelse(p3hb7a == 2|p3hb7a == 3, 1,
                                       ifelse(p3hb7a == 4, 3,
                                              ifelse(p3hb7a == 5, 8,
                                                     ifelse(p3hb7a == 6, 15, 21))))))) %>%
  mutate(p3toba_partner = ifelse(is.na(p3hb7b), NA,
                                 ifelse(p3hb7b == 0|p3hb7b == 1, 0,
                                        ifelse(p3hb7b == 2|p3hb7b == 3, 1,
                                               ifelse(p3hb7b == 4, 3,
                                                      ifelse(p3hb7b == 5, 8,
                                                             ifelse(p3hb7b == 6, 15, 21))))))) %>%
  mutate(p3toba_av = ifelse(is.na(p3toba) & is.na(p3toba_partner), NA,
                            ifelse(is.na(p3toba), p3toba_partner,
                                   ifelse(is.na(p3toba_partner), p3toba, ceiling((p3toba + p3toba_partner)/2))))) %>%
  mutate(p3toba_hi = ifelse(is.na(p3toba) & is.na(p3toba_partner), NA,
                            ifelse(is.na(p3toba), p3toba_partner,
                                   ifelse(is.na(p3toba_partner), p3toba, pmax(p3toba,p3toba_partner))))) %>%
######################################### recode the parental alcohol-use variable ####################################################################
  mutate(p1alco = ifelse(is.na(p1av6a), NA,
                         ifelse(p1av6a == 1, 0,
                                ifelse(p1av6a == 2, 1,
                                       ifelse(p1av6a == 3, 2,
                                              ifelse(p1av6a == 4, 7, 
                                                     ifelse(p1av6a == 5, 15, 21))))))) %>%
  mutate(p1alco_partner = ifelse(is.na(p1av6b), NA,
                                 ifelse(p1av6b == 1, 0,
                                        ifelse(p1av6b == 2, 1,
                                               ifelse(p1av6b == 3, 2,
                                                      ifelse(p1av6b == 4, 7, 
                                                             ifelse(p1av6b == 5, 15, 21))))))) %>%
  mutate(p1alco_av = ifelse(is.na(p1alco) & is.na(p1alco_partner), NA,
                            ifelse(is.na(p1alco), p1alco_partner,
                                   ifelse(is.na(p1alco_partner), p1alco, ceiling((p1alco + p1alco_partner)/2))))) %>%
  mutate(p1alco_hi = ifelse(is.na(p1alco) & is.na(p1alco_partner), NA,
                            ifelse(is.na(p1alco), p1alco_partner,
                                   ifelse(is.na(p1alco_partner), p1alco, pmax(p1alco,p1alco_partner))))) %>%
  mutate(p2alco = ifelse(is.na(p2av9a) | is.na(p2av10a) | is.na(p2av11a) | is.na(p2av12a), NA,
                         p2av9a * ifelse(p2av10a == 7, 8, ifelse(p2av10a == 8, 11, p2av10a)) 
                         +p2av11a * ifelse(p2av12a == 7, 8, ifelse(p2av12a == 8, 12, ifelse(p2av12a == 9, 17, ifelse(p2av12a == 10, 20, p2av12a)))))) %>%
  mutate(p2alco_partner = ifelse(is.na(p2av9b) | is.na(p2av10b) | is.na(p2av11b) | is.na(p2av12b), NA,
                                 p2av9b * ifelse(p2av10b == 7, 8, ifelse(p2av10b == 8, 11, p2av10b)) 
                                 +p2av11b * ifelse(p2av12b == 7, 8, ifelse(p2av12b == 8, 12, ifelse(p2av12b == 9, 17, ifelse(p2av12b == 10, 20, p2av12b)))))) %>%
  mutate(p2alco_av = ifelse(is.na(p2alco) & is.na(p2alco_partner), NA,
                            ifelse(is.na(p2alco), p2alco_partner,
                                   ifelse(is.na(p2alco_partner), p2alco, ceiling((p2alco + p2alco_partner)/2))))) %>%
  mutate(p2alco_hi = ifelse(is.na(p2alco) & is.na(p2alco_partner), NA,
                            ifelse(is.na(p2alco), p2alco_partner,
                                   ifelse(is.na(p2alco_partner), p2alco, pmax(p2alco,p2alco_partner))))) %>%
  mutate(p3alco = ifelse(is.na(p3hb8a) | is.na(p3hb9a) | is.na(p3hb10a) | is.na(p3hb11a), NA,
                         p3hb8a * ifelse(p3hb9a == 7, 8, ifelse(p3hb9a == 8, 11, p3hb9a)) 
                         +p3hb10a * ifelse(p3hb11a == 7, 8, ifelse(p3hb11a == 8, 12, ifelse(p3hb11a == 9, 17, ifelse(p3hb11a == 10, 20, p3hb11a)))))) %>%
  mutate(p3alco_partner = ifelse(is.na(p3hb8b) | is.na(p3hb9b) | is.na(p3hb10b) | is.na(p3hb11b), NA,
                                 p3hb8b * ifelse(p3hb9b == 7, 8, ifelse(p3hb9b == 8, 11, p3hb9b)) 
                                 +p3hb10b * ifelse(p3hb11b == 7, 8, ifelse(p3hb11b == 8, 12, ifelse(p3hb11b == 9, 17, ifelse(p3hb11b == 10, 20, p3hb11b)))))) %>%
  mutate(p3alco_av = ifelse(is.na(p3alco) & is.na(p3alco_partner), NA,
                            ifelse(is.na(p3alco), p3alco_partner,
                                   ifelse(is.na(p3alco_partner), p3alco, ceiling((p3alco + p3alco_partner)/2))))) %>%
  mutate(p3alco_hi = ifelse(is.na(p3alco) & is.na(p3alco_partner), NA,
                            ifelse(is.na(p3alco), p3alco_partner,
                                   ifelse(is.na(p3alco_partner), p3alco, pmax(p3alco,p3alco_partner))))) %>%
######################################### recode the parental cannabis-use variable ####################################################################
  mutate(p1fa14aa = ifelse(is.na(p1fa14a) | p1fa14a == 9, NA,
                         ifelse(p1fa14a == 0 | p1fa14a == 1, 0, 1))) %>%
  mutate(p1fa14ba = ifelse(is.na(p1fa14b) | p1fa14b == 9, NA,
                           ifelse(p1fa14b == 0 | p1fa14b == 1, 0, 1))) %>%
  mutate(p1addic = ifelse(is.na(p1fa14aa) & is.na(p1fa14ba), NA,
                          ifelse(is.na(p1fa14aa), p1fa14ba, 
                                 ifelse(is.na(p1fa14ba), p1fa14aa, pmax(p1fa14aa, p1fa14ba))))) %>%
  mutate(p1canna = ifelse(is.na(p1fa16ac) & is.na(p1fa16bc), NA,
                          ifelse(is.na(p1fa16ac), p1fa16bc,
                                 ifelse(is.na(p1fa16bc), p1fa16ac, pmax(p1fa16ac, p1fa16bc))))) %>%
  mutate(p2canna = ifelse(is.na(p2av13ac), NA, 
                          ifelse(p2av13ac == 11, 15, 
                                 ifelse(p2av13ac == 12, 29, 
                                        ifelse(p2av13ac == 13, 40, p2av13ac))))) %>%
  mutate(p2canna_partner = ifelse(is.na(p2av13bc), NA, 
                                  ifelse(p2av13bc == 11, 15, 
                                         ifelse(p2av13bc == 12, 29, 
                                                ifelse(p2av13bc == 13, 40, p2av13bc))))) %>%
  mutate(p2canna_av = ifelse(is.na(p2canna) & is.na(p2canna_partner), NA,
                             ifelse(is.na(p2canna), p2canna_partner,
                                    ifelse(is.na(p2canna_partner), p2canna, ceiling((p2canna + p2canna_partner)/2))))) %>%
  mutate(p2canna_hi = ifelse(is.na(p2canna) & is.na(p2canna_partner), NA,
                             ifelse(is.na(p2canna), p2canna_partner,
                                    ifelse(is.na(p2canna_partner), p2canna, pmax(p2canna,p2canna_partner))))) %>%
  mutate(p3canna = ifelse(is.na(p3hb12ac), NA, 
                          ifelse(p3hb12ac == 11, 15, 
                                 ifelse(p3hb12ac == 12, 29, 
                                        ifelse(p3hb12ac == 13, 40, p3hb12ac))))) %>%
  mutate(p3canna_partner = ifelse(is.na(p3hb12bc), NA, 
                                  ifelse(p3hb12bc == 11, 15, 
                                         ifelse(p3hb12bc == 12, 29, 
                                                ifelse(p3hb12bc == 13, 40, p3hb12bc))))) %>%
  mutate(p3canna_av = ifelse(is.na(p3canna) & is.na(p3canna_partner), NA,
                             ifelse(is.na(p3canna), p3canna_partner,
                                    ifelse(is.na(p3canna_partner), p3canna, ceiling((p3canna + p3canna_partner)/2))))) %>%
  
  mutate(p3canna_hi = ifelse(is.na(p3canna) & is.na(p3canna_partner), NA,
                             ifelse(is.na(p3canna), p3canna_partner,
                                    ifelse(is.na(p3canna_partner), p3canna, pmax(p3canna,p3canna_partner))))) %>%
######################################### reversely code the safety variable ####################################################################
  mutate(g1VEILIG = -(g1VEILIG)) %>%
  mutate(g2VEILIG = -(g2VEILIG)) %>%
  mutate(g3VEILIG = -(g3VEILIG)) %>%
  mutate(g4VEILIG = -(g4VEILIG)) %>%
  mutate(g5VEILIG = -(g5VEILIG))