library(mice)
library(miceadds)
library(countimp)
library(micemd)
library(broom)
library(broom.mixed)

#Imputation
#1) Impute missingness of dat_exposure_full
## Specify the data type for each variable
dat_imp_exposure_full <- dat_exposure_full %>%
  as_tibble() %>%
  mutate(g1pc4 = as.numeric(g1pc4)) %>%
  mutate(c1toba = as.factor(c1toba)) %>%
  mutate(c1alco = as.factor(c1alco)) %>%
  mutate(c1canna = as.factor(c1canna)) %>%
  mutate(c2toba = as.integer(c2toba)) %>%
  mutate(c2alco = as.integer(c2alco)) %>%
  mutate(c2canna = as.integer(c2canna)) %>%
  mutate(c3toba = as.integer(c3toba)) %>%
  mutate(c3alco = as.integer(c3alco)) %>%
  mutate(c3canna = as.integer(c3canna)) %>% 
  mutate(c4toba = as.integer(c4toba)) %>%
  mutate(c4alco = as.integer(c4alco)) %>%
  mutate(c4canna = as.integer(c4canna)) %>%
  mutate(c5toba = as.integer(c5toba)) %>%
  mutate(c5alco = as.integer(c5alco)) %>%
  mutate(c5canna = as.integer(c5canna)) %>%
  mutate(p1toba_hi = as.integer(p1toba_hi)) %>%
  mutate(p1alco_hi = as.integer(p1alco_hi)) %>%
  mutate(p1addic = as.numeric(p1addic)) %>%
  mutate(g1sex = as.factor(g1sex)) %>%
  mutate(p1ethni2 = as.factor(p1ethni2)) %>%
  mutate(p1divor = as.factor(p1divor)) %>%
  mutate(T2_ladder_4cat = as.integer(T2_ladder_4cat)) %>%
  mutate(c_alco = as.integer(c_alco)) %>%
  mutate(c_toba = as.integer(c_toba)) %>%
  mutate(c_canna_li = as.integer(c_canna_li)) %>%
  mutate(c_canna_qu = as.integer(c_canna_qu))

#Include the interaction term
dat_imp_exposure_full <- data.frame(dat_imp_exposure_full,inter_g1fragefc = NA,inter_g1depriefc = NA,inter_g1VEIefc= NA,
                                    inter_g1fragp1toba = NA,inter_g1deprip1toba = NA,inter_g1VEIp1toba = NA,
                                    inter_g1fragp1alco = NA,inter_g1deprip1alco = NA,inter_g1VEIp1alco = NA,
                                    inter_g1fragp1addic = NA,inter_g1deprip1addic = NA,inter_g1VEIp1addic = NA)

#check the missing pattern
mispat1<- md.pattern(dat_imp_exposure_full)

#run the initial imputation to set up  
ini1 <- mice(dat_imp_exposure_full, m=1, maxit = 0)

pred1 <- ini1$pred
method1 <- ini1$method

#Specify all level-1 variables as 2l.pmm
method1[] <- "2l.pmm"

#Specify those showing singular issues as pmm, possibly due to the very low icc
method1[c("p1toba_hi","p1genint","T2_ladder_4cat","p1alco_hi",
          "c1toba","c2toba","c3toba","c4toba","c5toba",
          "c1alco","c2alco","c4alco","c5alco",
          "c2canna","c3canna","c4canna","c5canna",
          "c_canna_li","c_canna_qu")] <-"pmm"

#remove pc4 and outcome trajectories from the imputation list
pred1[c("g4pc4","g5pc4"),] <- 0
pred1[,c("g4pc4","g5pc4")] <- 0
pred1[c("c_alco","c_toba","c_canna_li","c_canna_qu"),] <- 0



#impute the outcomes by the interaction terms
pred1[c("c_toba","c1toba","c2toba","c3toba","c4toba","c5toba"),
      c("inter_g1fragefc","inter_g1depriefc","inter_g1VEIefc",
        "inter_g1fragp1toba","inter_g1deprip1toba","inter_g1VEIp1toba")] <- 1

pred1[c("c_alco","c1alco","c2alco","c3alco","c4alco","c5alco"),
      c("inter_g1fragefc","inter_g1depriefc","inter_g1VEIefc",
        "inter_g1fragp1alco","inter_g1deprip1alco","inter_g1VEIp1alco")] <- 1

pred1[c("c_canna_li","c_canna_qu","c1canna","c2canna","c3canna","c4canna","c5canna"),
      c("inter_g1fragefc","inter_g1depriefc","inter_g1VEIefc",
        "inter_g1fragp1addic","inter_g1deprip1addic","inter_g1VEIp1addic")] <- 1

#Specify the level-2 variables as 2lonly.pmm
method[c("g1VEILIG")] <- "2lonly.pmm"

#set the cluster as -2 for all factors, and 0 for itself
pred1[,"g1pc4"] <- -2

pred1["g1pc4","g1pc4"]<-0

#person-id does not predict any factors
pred1[,"idno"] <- 0


#derive interactions
method1[c("inter_g1fragefc")] <- "~ I(g1frag*p1eaefc)"
method1[c("inter_g1depriefc")] <- "~ I(g1depri*p1eaefc)"
method1[c("inter_g1VEIefc")] <- "~ I(g1VEILIG*p1eaefc)"
method1[c("inter_g1fragp1toba")] <- "~ I(g1frag*p1toba_hi)"
method1[c("inter_g1deprip1toba")] <- "~ I(g1depri*p1toba_hi)"
method1[c("inter_g1VEIp1toba")] <- "~ I(g1VEILIG*p1toba_hi)"
method1[c("inter_g1fragp1alco")] <- "~ I(g1frag*p1alco_hi)"
method1[c("inter_g1deprip1alco")] <- "~ I(g1depri*p1alco_hi)"
method1[c("inter_g1VEIp1alco")] <- "~ I(g1VEILIG*p1alco_hi)"
method1[c("inter_g1fragp1addic")] <- "~ I(g1frag*p1addic)"
method1[c("inter_g1deprip1addic")] <- "~ I(g1depri*p1addic)"
method1[c("inter_g1VEIp1addic")] <- "~ I(g1VEILIG*p1addic)"

imp_exposure_full <- mice(dat_imp_exposure_full, predictorMatrix = pred1, method = method1, visit = "monotone", m = 50, maxit = 100, seed = 1234, allow.na = TRUE)
