pkgname <- "gWQS"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
library('gWQS')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("gwqs")
### * gwqs

flush(stderr()); flush(stdout())

### Name: gwqs
### Title: Fitting Weighted Quantile Sum regression models
### Aliases: gwqs

### ** Examples

# we save the names of the mixture variables in the variable "toxic_chems"
toxic_chems = c("log_LBX074LA", "log_LBX099LA", "log_LBX105LA", "log_LBX118LA",
"log_LBX138LA", "log_LBX153LA", "log_LBX156LA", "log_LBX157LA", "log_LBX167LA",
"log_LBX170LA", "log_LBX180LA", "log_LBX187LA", "log_LBX189LA", "log_LBX194LA",
"log_LBX196LA", "log_LBX199LA", "log_LBXD01LA", "log_LBXD02LA", "log_LBXD03LA",
"log_LBXD04LA", "log_LBXD05LA", "log_LBXD07LA", "log_LBXF01LA", "log_LBXF02LA",
"log_LBXF03LA", "log_LBXF04LA", "log_LBXF05LA", "log_LBXF06LA", "log_LBXF07LA",
"log_LBXF08LA", "log_LBXF09LA", "log_LBXPCBLA", "log_LBXTCDLA", "log_LBXHXCLA")

# To run a linear model and save the results in the variable "results". This linear model
# (family="Gaussian") will rank/standardize variables in quartiles (q = 4), perform a
# 40/60 split of the data for training/validation (validation = 0.6), and estimate weights
# over 5 bootstrap samples (b = 3). Weights will be derived from mixture effect
# parameters that are positive (b1_pos = TRUE). A unique seed was specified (seed = 2016) so
# this model will be reproducible, and plots describing the variable weights and linear
# relationship will be generated as output (plots = TRUE). In the end tables describing the
# weights values and the model parameters with the respectively statistics are generated in
# the viewer window
results = gwqs(y ~ NULL, mix_name = toxic_chems, data = wqs_data, q = 4, validation = 0.6,
               b = 3, b1_pos = TRUE, b1_constr = FALSE, family = "gaussian", seed = 2016,
               wqs2 = FALSE, plots = TRUE, tables = TRUE)

# to test the significance of the covariates
summary(results$fit)




### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
