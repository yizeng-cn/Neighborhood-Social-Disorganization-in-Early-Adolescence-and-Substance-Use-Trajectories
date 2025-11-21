###############################################################################################################################################
######################################### Latent mixture modelling ############################################################################
###############################################################################################################################################

library(MplusAutomation)
library(rhdf5)
library(shiny)
setwd("U:/Paper3/Analysis/class")

#General comments: 
##filename_stem: the name of the generated file (input, output, gh5 file, saved file)
##Missingess(need to be set as NA in advance) in the dataset was self-recognised
##In the analysis command, TYPE = MIXTURE is automatically added
##rdata indicates if the Mplus input file will be generated (e.g., 1 means only generate the input file for the first class; 1L means all)

#Estimate linear trajectory for cannabis use (1-6 classes)
canna_linear <- createMixtures(
  classes = 1:6,
  filename_stem = "canna_linear_traj",
  VARIABLE = "USEVARIABLES = c2canna c3canna c4canna c5canna;
COUNT = c2canna(i) c3canna(i) c4canna(i) c5canna(i);
idvariable is idno;",
  model_overall = "i s| c2canna@0 c3canna@1 c4canna@2 c5canna@3;",
  OUTPUT = "SAMP STDYX MOD(4) PATTERNS TECH11 TECH14;",
  PLOT = "TYPE = PLOT3;
SERIES = c2canna(0) c3canna(1) c4canna(2) c5canna(3);",
  ANALYSIS = "STARTS = 100 10;
LRTSTARTS = 0 0 500 100;
PROCESSORS = 8;
ESTIMATOR = MLR;",
  rdata = studyvari,
  run = 1L,
)

#Estimate quadratic trajectory for cannabis use (1-6 classes)
canna_quad <- createMixtures(
  classes = 1:6,
  filename_stem = "canna_quad_traj",
  VARIABLE = "USEVARIABLES = c2canna c3canna c4canna c5canna;
COUNT = c2canna(i) c3canna(i) c4canna(i) c5canna(i);
idvariable is idno;",
  model_overall = "i s q| c2canna@0 c3canna@1 c4canna@2 c5canna@3;",
  OUTPUT = "SAMP STDYX MOD(4) PATTERNS TECH11 TECH14;",
  PLOT = "TYPE = PLOT3;
SERIES = c2canna(0) c3canna(1) c4canna(2) c5canna(3);",
  ANALYSIS = "STARTS = 100 10;
LRTSTARTS = 0 0 500 100;
PROCESSORS = 8;
ESTIMATOR = MLR;",
  rdata = studyvari,
  run = 1L,
)

#Estimate linear trajectory for tobacco use (1-4 classes)
toba_linear <- createMixtures(
  classes = 1:4,
  filename_stem = "toba_linear_traj",
  VARIABLE = "USEVARIABLES = c2toba c3toba c4toba c5toba;
COUNT = c2toba(i) c3toba(i) c4toba(i) c5toba(i);
idvariable is idno;",
  model_overall = "i s| c2toba@0 c3toba@1 c4toba@2 c5toba@3;",
  OUTPUT = "SAMP STDYX MOD(4) PATTERNS TECH11 TECH14;",
  PLOT = "TYPE = PLOT3;
SERIES = c2toba(0) c3toba(1) c4toba(2) c5toba(3);",
  ANALYSIS = "STARTS = 100 10;
LRTSTARTS = 0 0 500 100;
PROCESSORS = 8;
ESTIMATOR = MLR;",
  rdata = studyvari,
  run = 1L,
)

#Estimate quadratic trajectory for tobacco use (1-4 classes)
toba_quad <- createMixtures(
  classes = 1:4,
  filename_stem = "toba_quad_traj",
  VARIABLE = "USEVARIABLES = c2toba c3toba c4toba c5toba;
COUNT = c2toba(i) c3toba(i) c4toba(i) c5toba(i);
idvariable is idno;",
  model_overall = "i s q| c2toba@0 c3toba@1 c4toba@2 c5toba@3;",
  OUTPUT = "SAMP STDYX MOD(4) PATTERNS TECH11 TECH14;",
  PLOT = "TYPE = PLOT3;
SERIES = c2toba(0) c3toba(1) c4toba(2) c5toba(3);",
  ANALYSIS = "STARTS = 100 10;
LRTSTARTS = 0 0 500 100;
PROCESSORS = 8;
ESTIMATOR = MLR;",
  rdata = studyvari,
  run = 1L,
)

#Estimate linear trajectory for alcohol use (1-5 classes)
alco_linear <- createMixtures(
  classes = 1:5,
  filename_stem = "alco_linear_traj",
  VARIABLE = "USEVARIABLES = c2alco c3alco c4alco c5alco;
COUNT = c2alco c3alco c4alco c5alco;
idvariable is idno;",
  model_overall = "i s| c2alco@0 c3alco@1 c4alco@2 c5alco@3;",
  OUTPUT = "SAMP STDYX MOD(4) PATTERNS TECH11 TECH14;",
  PLOT = "TYPE = PLOT3;
SERIES = c2alco(0) c3alco(1) c4alco(2) c5alco(3);",
  ANALYSIS = "STARTS = 100 10;
LRTSTARTS = 0 0 500 100;
PROCESSORS = 8;
ESTIMATOR = MLR;",
  rdata = studyvari,
  run = 1L,
)

#Estimate quadratic trajectory for alcohol use (1-5 classes)
alco_quad <- createMixtures(
  classes = 1:5,
  filename_stem = "alco_quad_traj",
  VARIABLE = "USEVARIABLES = c2alco c3alco c4alco c5alco;
COUNT = c2alco c3alco c4alco c5alco;
idvariable is idno;",
  model_overall = "i s q| c2alco@0 c3alco@1 c4alco@2 c5alco@3;",
  OUTPUT = "SAMP STDYX MOD(4) PATTERNS TECH11 TECH14;",
  PLOT = "TYPE = PLOT3;
SERIES = c2alco(0) c3alco(1) c4alco(2) c5alco(3);",
  ANALYSIS = "STARTS = 100 10;
LRTSTARTS = 0 0 500 100;
PROCESSORS = 8;
ESTIMATOR = MLR;",
  rdata = studyvari,
  run = 1L,
)