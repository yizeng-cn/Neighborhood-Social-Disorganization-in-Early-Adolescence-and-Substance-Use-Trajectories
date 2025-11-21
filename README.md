# Neighborhood-Social-Disorganization-in-Early-Adolescence-and-Substance-Use-Trajectories
> **Note:** These are codes of analyses for the study: "Neighborhood Social Disorganization in Early Adolescence and Substance Use Trajectories into Young Adulthood: The Moderating Role of Effortful Control and Parental Substance Use" (https://doi.org/10.1007/s10964-025-02270-0)

## File description
* Data Pre-processing
  - Pre-processing of TRAILS data and the construction of variables (e.g., tobacco, alcohol, and cannabis use). Detailed information on what each variable code represents can be found in the TRAILS codebooks.
* Multiple Imputation
  - The MICE package in R was used to impute missingness in all variables included in Step 3 (details are presented in the research article).
* Latent_Growth_Curve_Models (LCGA)
  - Two files were uploaded: 1) The R package MplusAutomation was used to run batches of LCGA models; 2) The Mplus code (in a zip) for models with the inflation part of the zero-inflated model additionally considered (used in the main analysis).
* Multilevel multinomial logistic regression models
  - Model for alcohol use: Code for Model 3 with all indicators of neighborhood social disorganization and their interaction terms with moderators included.
  - Model for tobacco use: Code for Model 3 with all indicators of neighborhood social disorganization and their interaction terms with moderators included.
  - Model for cannabis use: Code for three Model 1 with indicators of neighborhood social disorganization separately included (Reasons and details are presented in the research article).

## Variable descriptions (multilevel multinomial logistic regression models)
* Outcomes
  - c_toba_qu_infla: tobacco use trajectories
  - c_alco: alcohol use trajectories
  - c_canna_qu_infla: cannabis use trajectories

* Moderators
  - p1eaefc: adolescents' effortful control
  - p1toba_hi: Parental tobacco use
  - p1alco_hi: Parental alcohol use
  - p1addic: Parental addiction

* Individual- and family-level covariates
  - p1ses: family SES
  - p1genext: parental externalising problems
  - p1genint: parental internalising problems
  - g1sex: sex
  - p1ethni2: ethnicity
  - p1divor: parental divorce

* Exposures (indicators of social disorganization)
  - g1depri: socioeconomic deprivation
  - g1frag: social fragmentation
  - g1VEILIG: disorder




---

> [Yi-Zeng](https://www.uu.nl/staff/YZeng1/Publications) &nbsp;&middot;&nbsp;
> ORCID (https://orcid.org/0000-0002-2586-6674) &nbsp;&middot;&nbsp;
> Twitter [@YiZengoooo] &nbsp;&middot;&nbsp;
> Email (zengyi971031@gmail.com)

