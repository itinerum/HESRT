###########################################################################
###########################################################################
############### MARKOV MODELS FOR HEALTH ECONOMIC EVALUATION ##############
###########################################################################
###########################################################################
#clean all variables and objects first
rm(list=ls())
#
# Using heemod to model Markov models in HEE
#
# Example taken from: Briggs, Andrew. Decision Modelling for Health Economic Evaluation
# (Handbooks in Health Economic Evaluation) . OUP Oxford. Kindle Edition.
# Chapter 2

#Loading the packages
library(heemod)
library(diagram) #diagram is used to plot the transition matrix into a plot
library(ggplot2) #ggplot2 will be used later to plot the results
library(hrbrthemes) #hrbrthemes goes with ggplot2
#
#
###########################################################################
########################## DEFINE THE PARAMETERS ##########################
###########################################################################
#
#
par_mod <-define_parameters(
  #Defining the transition probabilities in the parameters is very useful, because can be included
  # in psa, but but the plot of the network will not include the numbers. If we want the plot to
  #include the numbers, put the transition probbilities outside of define_parameters
  p_AA_base = 0.721,
  p_AB_base = 0.202,
  p_AC_base = 0.067,
  p_AD_base = 0.010,
  
  p_BC_base = 0.407,
  p_BD_base = 0.012,
  
  p_CD_base = 0.250,
  
  p_AA = p_AA_base,
  p_AB = p_AB_base,
  p_AC = p_AC_base,
  p_AD = p_AD_base,
  
  p_BC = p_BC_base,
  p_BD = p_BD_base,
  
  p_CD = p_CD_base,
  
  #Defining the costs, without the discount
  cost_zido = 2278,
  cost_lami = 2086,
  cost_health_A_notdiscounted = 2756,
  cost_health_B_notdiscounted = 3052,
  cost_health_C_notdiscounted = 9007,
  cost_health_D_notdiscounted = 0,
  #Defining other variables
  rr = 0.509,#Define the relative risk of intervention 2 compared to 1
  dsct = 0.06 #Define discount
)
#
###########################################################################
##################### DEFINE THE TRANSITION MATRICES ######################
###########################################################################
# Define one transition matrix per treatment/intervention/strategy
#
# FIRST MATRIX: DEFINE THE TRANSITION MATRIX FOR MONOTHERAPY
#
mat_monotherapy <- define_transition(
  p_AA, p_AB, p_AC, p_AD, 
 0,    C, p_BC, p_BD,
 0,    0,    C, p_CD,
 0, 0, 0, 1
)
#In this way the plot of the network has no numbers. It might be preferable sometimes to 
#put the numeric values in the matrix and remove from the parameters list
#
#Print and plot the matrix for monotherapy (optional)
mat_monotherapy # optional
plot(mat_monotherapy) # optional
# The look and feel of the plot can be modified (plot is a function of the package "diagram")
# Some examples below, see package help
plot(mat_monotherapy,
     main= "Transition matrix for monotherapy",
     cex.main = 1, 
     relsize=0.6,
     box.col= "white",
     box.cex=1.3, cex.txt=0.8,
     box.type="circle", 
     arr.lwd=0.5, arr.lcol="blue", 
     arr.col="orange",
     arr.type="triangle")
#
# SECOND MATRIX: DEFINE THE TRANSITION MATRIX FOR COMBINED THERAPY
#
#The combined therapy group has its transition probabilities multiplied by the relative risk (in this example rr = 0.509),
#the relative risk of event for the population treated by combined therapy.
#Since rr < 1, the combined therapy group has less chance to transition to worst health states.
#
#Define relative risk
#rr<-0.509
# Define the transition matrix with values calculated from rr
# The probabilities to stay in the same state are equal to 1 − ∑ Ptrans where Ptrans are the probabilities to change
# to another state (because all transition probabilities from a given state must sum to 1).
# We use the alias C as a convenient way to specify the probability complement, equal to 1 − ∑ Ptrans 
mat_combination <-define_transition(
  C, p_AB*rr, p_AC*rr,   p_AD*rr, 
  0, C,       0.407**rr, 0.012*rr,
  0, 0,       C,         0.250*rr,
  0, 0,       0,         1
)
#
#Print and plot the matrix for comnined therapy (optional)
mat_combination # optional
plot(mat_combination)# optional
#
#
# Plot both graphs in the same panel
#
par(mfrow=c(1,2))
plot(mat_monotherapy, main="Monotherapy")
plot(mat_combination, main="Combined")
#
###########################################################################
########################### DEFINE STATES #################################
###########################################################################
#st <- define_state(
  #cost = 6453,
  #utility/qaly/life_year = .876 #####here we use the metric for the effect of the intervention
#)
#st
#
#Above the standard sintaxis given in the information for define_state
##################
#Define each state
#State A
state_A <- define_state(
  cost_health = discount(cost_health_A_notdiscounted, r=dsct),
  cost_drugs = discount(dispatch_strategy(mono = cost_zido,comb = cost_zido + cost_lami), r=dsct),
  cost_total = cost_health + cost_drugs,
  life_year = 1
  )
  state_A
#
state_B <- define_state(
  cost_health = discount(cost_health_B_notdiscounted, r=dsct),
  cost_drugs = discount(dispatch_strategy(mono = cost_zido,comb = cost_zido + cost_lami),r=dsct),
  cost_total = cost_health + cost_drugs,
  life_year = 1
  )
#
state_C <- define_state(
  cost_health = discount(cost_health_C_notdiscounted, r=dsct),
  cost_drugs = discount(dispatch_strategy(mono = cost_zido,comb = cost_zido + cost_lami),r=dsct),
  cost_total = cost_health + cost_drugs,
  life_year = 1
  )
#
state_D <- define_state(
  cost_health = cost_health_D_notdiscounted,
  cost_drugs = 0,
  cost_total = 0,
  life_year = 0
  )
#
#Print states (optional)
state_A
state_B
state_C
state_D
#


###########################################################################
########################### DEFINE MODEL ##################################
###########################################################################
#Combine information in a model
#Now that the transition matrix and the state values are defined for a given strategy, we can combine them with define_strategy():
#define_strategy() creates the model
#
# Define model for monotherapy
strat_monotherapy <- define_strategy(
  transition = mat_monotherapy,
  state_A,
  state_B,
  state_C,
  state_D
)
# Define model for combined therapy
strat_combination <- define_strategy(
  transition = mat_combination,
  state_A,
  state_B,
  state_C,
  state_D
)
#
#RUN THE MODEL
#Both strategies can then be combined in a model and run for 20 years with run_model().
#Strategies are given names (mono and comb) in order to facilitate result interpretation.
res_mod <- run_model(
  mono = strat_monotherapy,
  comb = strat_combination,
  parameters=par_mod,
  init=c(1000,0,0,0),
  cycles = 20,
  method = "life-table", #methods are "beginning", "end" and "life-table"
  cost = cost_total,
  effect = life_year
)
res_mod
#
###########################################################################
######################## EXTRACT RESULTS FROM THE MODEL ###################
###########################################################################
#
########## GET COUNTS
# All counts, individuals (patients, subjects) all strategies, per Markov cycle and per state, produces one single data frame
counts_total<-get_counts(res_mod) 
counts_total
write.csv(counts_total,file="counts_total.csv")
### Counts per strategy
# Counts for strategy mono
counts_mono<-res_mod$eval_strategy_list$mono$counts
counts_mono
cycles_mono<-res_mod$eval_strategy_list$mono$parameters$markov_cycle
cycles_mono
cycles<-cycles_mono
counts_with_cycle_mono<-cbind(cycles_mono,counts_mono)
counts_with_cycle_mono
# Counts for strategy comb
counts_comb<-res_mod$eval_strategy_list$comb$counts
counts_comb
cycles_comb<-res_mod$eval_strategy_list$comb$parameters$markov_cycle
cycles_comb
counts_with_cycle_comb<-cbind(cycles_comb,counts_comb)
counts_with_cycle_comb
#
### Counts per state
# Extracting individual counts can be used to combine states, for example to calculate survival combining all non-death states
# It can be obtained from counts_total<-get_counts(res_mod) that creates a data frame that can be manipulated in excel, but is
# more convenient to do all analysis in R
#
# Counts per state for monotherapy
counts_mono_stateA<-res_mod$eval_strategy_list$mono$counts$A
counts_mono_stateB<-res_mod$eval_strategy_list$mono$counts$B
counts_mono_stateC<-res_mod$eval_strategy_list$mono$counts$C
counts_mono_stateD<-res_mod$eval_strategy_list$mono$counts$D
#
counts_mono_alive<-counts_mono_stateA+counts_mono_stateB+counts_mono_stateC
counts_with_cycle_mono_alive<-data.frame(cycles_mono,counts_mono_alive) #using cbind here creates a matrix instead of a data.frame
counts_with_cycle_mono_alive
plot(counts_with_cycle_mono_alive) # This creates the survival curve for the cohort mono
#
# Counts per state for combined therapy
counts_comb_stateA<-res_mod$eval_strategy_list$comb$counts$A
counts_comb_stateB<-res_mod$eval_strategy_list$comb$counts$B
counts_comb_stateC<-res_mod$eval_strategy_list$comb$counts$C
counts_comb_stateD<-res_mod$eval_strategy_list$comb$counts$D
#
counts_comb_alive<-counts_comb_stateA+counts_comb_stateB+counts_comb_stateC
counts_with_cycle_comb_alive<-data.frame(cycles_comb,counts_comb_alive)
counts_with_cycle_comb_alive
plot(counts_with_cycle_comb_alive) # This creates the survival curve for the cohort combined
#
# Now we can create a combined plot for the two survival curves using ggplot2
# First we create a new data.frame combining the x, y1 and y2
# And we plot
survival_combined <- data.frame(cycles, counts_mono_alive, counts_comb_alive)
ggplot(survival_combined, aes(cycles, y = number_of_patients, color = variable)) +
  geom_point(aes(y = counts_mono_alive, col = "Monotherapy")) + geom_point(aes(y = counts_comb_alive, col = "Combined"))
#
########## GET VALUES
#Values are the results of the model for costs and effects
# Get all values into a data.frame that can be manipulated with excel
values_all<-get_values(res_mod)
values_all
values_mono<-res_mod$eval_strategy_list$mono$values
values_mono
values_comb<-res_mod$eval_strategy_list$comb$values
values_comb
# Get costs (total costs)
cost_comb_total<-res_mod$eval_strategy_list$comb$values$cost_total
cost_comb_total
cost_mono_total<-res_mod$eval_strategy_list$mono$values$cost_total
cost_mono_total
# Get effects (life_years)
effect_mono<-res_mod$eval_strategy_list$mono$values$life_year
effect_mono
effect_comb<-res_mod$eval_strategy_list$comb$values$life_year
effect_comb
#
########## GET MODEL SUMMARY
#
summary(res_mod,
        threshold = c(1000, 2000, 3000, 4000 , 4500, 5000, 6000, 1e4, 20000))
#
########## GET MODEL PLOTS
# Plot transition graphs
plot(mat_monotherapy)
plot(mat_combination)
#
# Plot survival curves (plotting counts)
# some ggplot2 sintaxis hereis needed
survival_combined <- data.frame(cycles, counts_mono_alive, counts_comb_alive)
ggplot(survival_combined, aes(cycles, y = number_of_patients, color = variable)) +
  geom_point(aes(y = counts_mono_alive, col = "Monotherapy")) + geom_point(aes(y = counts_comb_alive, col = "Combined"))
#
# Plot model by strategy (plotting counts)
plot(res_mod) # By default is by strategy
plot(res_mod, type = "counts", panel = "by_strategy") +
  xlab("Time") +
  theme_bw() +
  scale_color_brewer(
    name = "State",
    palette = "Set1"
  )
#
# Plot model by state (plotting counts)
plot(res_mod, type = "counts", panel = "by_state") +
  xlab("Time") +
  theme_bw() +
  scale_color_brewer(
    name = "State",
    palette = "Set1"
  )
#
# Plot cost and effects by strategy (plotting values)
plot(res_mod, type = "values", panel = "by_value",
     free_y = TRUE) +
  xlab("Time") +
  theme_bw() +
  scale_color_brewer(
    name = "Strategy",
    palette = "Set1"
  )
#
# Plot cost and effects by strategy (plotting values)
plot(res_mod, type = "values", panel = "by_strategy",
     free_y = TRUE) +
  xlab("Time") +
  theme_bw() +
  scale_color_brewer(
    name = "Strategy",
    palette = "Set1"
  )
#
# Plot total costs by strategy (plotting values)
# some ggplot2 sintaxis hereis needed
costs_combined <- data.frame(cycles, cost_mono_total, cost_comb_total)
costs_combined
ggplot(costs_combined, aes(cycles, y = Costs, color = variable)) +
  geom_point(aes(y = cost_mono_total, col = "Monotherapy")) + geom_point(aes(y = cost_comb_total, col = "Combined"))


###########################################################################
#################### DETERMINISTIC SENSITIVITY ANALYSIS ###################
###########################################################################
#
def_dsa <- define_dsa(
  cost_zido, 2000, 3000,
  cost_lami, 1500, 3000,
  cost_health_A_notdiscounted, 2000, 3000,
  cost_health_B_notdiscounted, 2000, 2500,
  cost_health_C_notdiscounted, 8000, 10000,
  rr, 0.4, 0.6,
  dsct, 0.03, 0.09
  )
res_dsa <- run_dsa (res_mod, dsa=def_dsa)
res_dsa
#
#Plotting tornado plots
plot(res_dsa, result = "cost", strategy = "mono", type="simple",limits_by_bars = TRUE)
plot(res_dsa, result = "cost", strategy = "comb", type="simple",limits_by_bars = TRUE)
plot(res_dsa, result = "difference", strategy = "comb", type="simple",limits_by_bars = TRUE)
plot(res_dsa, result = "icer", strategy = "comb", type="difference",limits_by_bars = TRUE)
#
###########################################################################
#################### PROBABILISTIC SENSITIVITY ANALYSIS ###################
###########################################################################
#
def_psa <- define_psa(
  cost_zido ~ normal (mean = 2000, sd = 250),
  cost_lami ~ normal (mean = 1500, sd = 500),
  cost_health_A_notdiscounted ~ normal (mean = 2000, sd = 900),
  cost_health_B_notdiscounted ~ normal (mean= 2000, sd = 1500),
  cost_health_C_notdiscounted ~ normal (mean = 8000, sd =100),
  rr ~normal (mean = 0.509, sd =0.05)
  #dsct, 0.03, 0.09
  )

res_psa <- run_psa(res_mod, psa = def_psa, N = 1000)
summary(res_psa)
plot(res_psa, type = "ce")+ stat_binhex()+coord_fixed(ratio = 1/10000)
plot(res_psa)
plot(res_psa) + stat_binhex(binwidth = c(0.05, 0.05))

plot(res_psa, type = "ac", n = 100, log_scale = FALSE, diff = TRUE, 50000, bw = TRUE)




# Get costs (total costs)

cost_psa<-res_psa$psa$cost_total
cost_psa
effect_psa<-res_psa$psa$life_year
effect_psa
strategy_psa<-res_psa$psa$.strategy_names
export_psa<-data.frame(strategy_psa,cost_psa,effect_psa)
export_psa
write.csv(export_psa,file="forBCEA.csv")

cost_comb_total_psa<-res_psa$psa$.strategy_names
cost_comb_total_psa
cost_comb_total_psa
cost_mono_total_psa<-res_psa$eval_strategy_list$mono$values$cost_total
cost_mono_total_psa
# Get effects (life_years)
effect_mono_psa<-res_psa$eval_strategy_list$mono$values$life_year
effect_mono_psa



#
###########################################################################
########################### BUDGET IMPACT ANALYSIS ########################
###########################################################################
#
res_bia <- run_model(
  parameters=par_mod,
  mono = strat_monotherapy,
  comb = strat_combination,
  init=c(3000,0,0,0),
  cycles = 10,
  cost = cost_total,
  effect = life_year,
  inflow =c (8000,0,0,0)
)

res_bia
summary(res_bia)

#
###########################################################################
########################### INTERFACE WITH BCEA ###########################
###########################################################################
#

library(BCEA)
bcea1<-run_bcea(res_psa, plot=TRUE, Kmax=10000)
summary(bcea1)

