library(base)
library(ggplot2)
library(MASS)
library(splines)
library(matrixStats)
library(tidyverse)
library(cowplot)
library(ggrepel)
library(stats)
library(data.table)

# Load in data
data_orphan<-read.csv('DATA/global_extrapolation.csv',stringsAsFactors = FALSE)


# Calculate country specific SD
data_orphan$sd = (data_orphan$tfr_upper-data_orphan$tfr_lower)/(2*1.96)

# pearson test
print(sprintf("Pearsons r^2 primary: %f",  cor(data_orphan$tfr, data_orphan$orphan_death_ratio, method = "pearson") ))


# scatter plot
p <- ggplot(data_orphan) +
  geom_point( aes(x = tfr, y = orphan_death_ratio)) +
  xlab("Total fertility rate") + ylab("Ratio of children orphaned to all-cause deaths") + 
  theme_bw()
p <- p + geom_text_repel(aes(x = tfr, y = orphan_death_ratio, label = country), size = 4, max.overlaps = 100)
p

cowplot::save_plot("figures/global/scatter_plot.pdf", p, base_height = 5, base_width = 5)


#################################################################

# linear model 1
fit_linear <- lm(orphan_death_ratio ~ tfr, data = data_orphan)
summary(fit_linear)
#plot(fit_linear)
print(predict(fit_linear))
plot(data_orphan$tfr,data_orphan$orphan_death_ratio)
lines(data_orphan$tfr,predict(fit_linear))
new_tfr <- seq(min(data_orphan$tfr), max(data_orphan$tfr), 0.01)
pred_orphan_death_ratio <- data.frame(predict(fit_linear, newdata = data.frame(tfr = new_tfr),
                                              interval = "confidence"), 
                                      new_tfr = new_tfr)
# p_linear<-p + 
#   geom_line(data = pred_orphan_death_ratio, mapping = aes(x = new_tfr, y = fit), 
#             color = "red", size = 1, alpha = 0.5) +
#   geom_ribbon(data = pred_orphan_death_ratio, mapping = aes(x = new_tfr, 
#                                             ymin = lwr, ymax = upr), 
#               fill = "grey", alpha = 0.5)

p_linear<-p + 
  geom_abline(slope = fit_linear$coefficients[2], intercept = fit_linear$coefficients[1], 
              size = 1, alpha = 0.5,col = 'black') +labs(tag='A')
# geom_ribbon(data = pred_orphan_death_ratio, mapping = aes(x = new_tfr, 
#                                           ymin = lwr, ymax = upr), 
#             fill = "grey", alpha = 0.5)

print(p_linear)
cowplot::save_plot("figures/global/linear_plot.pdf", p_linear, base_height = 5, base_width = 5)



calc_ratio_linear <- function(intercept, beta, tfr){
  return(beta*tfr+intercept)
} 
#x = seq(0, 5, 0.1)
# x=data_orphan$tfr
calculated_parent_ratio_linear =  calc_ratio_linear(fit_linear$coefficients[1], fit_linear$coefficients[2], data_orphan$tfr)
data_orphan$calculated_parent_ratio_linear =calculated_parent_ratio_linear
p_obs_pred_pa = ggplot(data_orphan) +
  geom_point(aes(orphan_death_ratio, calculated_parent_ratio)) + 
  geom_point(data = data_orphan, 
             aes(orphan_death_ratio, calculated_parent_ratio), col = "red") + 
  geom_abline(slope = 1, intercept = 0) + 
  xlab("Ratio of children orphaned to deaths of parents (observed)") + 
  ylab("Ratio of children orphaned to deaths of parents (predicted)") + labs(tag='B')+
  geom_text_repel(aes(x = orphan_death_ratio, y = calculated_parent_ratio, label = country),
                  size = 4, max.overlaps = 100) +
  scale_color_manual(values = c("black", "red")) + 
  theme_bw() + theme(legend.position = "none")
p_obs_pred_pa

cowplot::save_plot("figures/global/linear_predic_observed_plot.pdf", p_obs_pred_pa, base_height = 5, base_width = 5)


#################################################################

# Linear model 2 -- fixed effect for European countries

calc_ratio_linear_fix <- function(intercept, beta, beta2,euro,tfr){
  return(beta*tfr+beta2*euro+intercept)
} 

fit_linear_adjust <- lm(orphan_death_ratio ~ tfr+ euro, data = data_orphan)
summary(fit_linear_adjust)




# linear graph
x = seq(0, 5, 0.1)
y0 = rep(0,51)
y1 = rep(1,51)
line_all_li2 = data.frame(x = x, y = y0,
                          z = calc_ratio_linear_fix(fit_linear_adjust$coefficients[1], fit_linear_adjust$coefficients[2], 
                                                    fit_linear_adjust$coefficients[3],  y0,x))

line_all_li2_e = data.frame(x = x, y = y1,
                            z = calc_ratio_linear_fix(fit_linear_adjust$coefficients[1], fit_linear_adjust$coefficients[2], 
                                                      fit_linear_adjust$coefficients[3],y1,x))



p_adjust_s <- ggplot(data_orphan) + 
  geom_point(aes(tfr, orphan_death_ratio)) + 
  geom_line(data = line_all_li2, aes(x, z), col = 'blue') +  
  geom_line(data = line_all_li2_e, aes(x, z), col = 'red') +
  xlab("Total fertility rate") + ylab("Ratio of children orphaned to deaths of parents") + 
  theme_bw()+labs(tag = 'C')
p_adjust_s_label2 <- p_adjust_s + geom_text_repel(aes(x = tfr, y = orphan_death_ratio, label = country), size = 4, max.overlaps = 100)
p_adjust_s_label2









# p_adjust_s<-p+ 
#   geom_abline(slope = fit_linear_adjust$coefficients[2], intercept = fit_linear_adjust$coefficients[1], 
#               color = "black", size = 1, alpha = 0.5) +labs(tag='C')
# p_adjust_s
cowplot::save_plot("figures/global/linear_adjust_plot.pdf", p_adjust_s_label2, base_height = 5, base_width = 5)


calculated_parent_ratio_adjust =  calc_ratio_linear_fix(fit_linear_adjust$coefficients[1], 
                                                        fit_linear_adjust$coefficients[2], 
                                                        fit_linear_adjust$coefficients[3],
                                                        data_orphan$euro,
                                                        data_orphan$tfr)
data_orphan$calculated_parent_ratio_adjust =calculated_parent_ratio_adjust
p_obs_pred_pa_adjust = ggplot(data_orphan) +
  geom_point(aes(orphan_death_ratio, calculated_parent_ratio_adjust)) + 
  geom_point(data = data_orphan, 
             aes(orphan_death_ratio, calculated_parent_ratio_adjust), col = "red") + 
  geom_abline(slope = 1, intercept = 0) + 
  xlab("Ratio of children orphaned to deaths of parents (observed)") + 
  ylab("Ratio of children orphaned to deaths of parents (predicted)") + 
  geom_text_repel(aes(x = orphan_death_ratio, y = calculated_parent_ratio_adjust, label = country),
                  size = 4, max.overlaps = 100) +
  scale_color_manual(values = c("black", "red")) + 
  theme_bw() + theme(legend.position = "none")+labs(tag='D')
p_obs_pred_pa_adjust 

cowplot::save_plot("figures/global/adjust_linear_predic_observed_plot.pdf", p_obs_pred_pa_adjust , base_height = 5, base_width = 5)


#################################################################

#formula for logistic model 3
calc_ratio <- function(alpha, beta, gamma, tfr){
  return(gamma * (exp(alpha + beta * tfr))/(1 + exp(alpha + beta * tfr)))
} 

error_parent<- function(params, data){
  return(sum((calc_ratio(params[1], params[2], params[3], data$tfr) - data$orphan_death_ratio)^2))
}

set.seed(10)


pars = c(0.1, 0.1, 0.1)
output_p = optim(pars, error_parent, data=data_orphan)
saveRDS(output_p$par, "DATA/global/parent_coefficients.RDS")
print(output_p$par)

# logistic graph
x = seq(0, 5, 0.1)
line_all = data.frame(x = x, 
                      y = calc_ratio(output_p$par[1], output_p$par[2], 
                                     output_p$par[3], x))
p_fit_pa <- ggplot(data_orphan) + 
  geom_point(aes(tfr, orphan_death_ratio)) + 
  geom_line(data = line_all, aes(x, y), col = 'black') + 
  xlab("Total fertility rate") + ylab("Ratio of children orphaned to deaths of parents") + 
  theme_bw()+labs(tag = 'E')
p_fit_pa_label <- p_fit_pa + geom_text_repel(aes(x = tfr, y = orphan_death_ratio, label = country), size = 4, max.overlaps = 100)
p_fit_pa_label


cowplot::save_plot("figures/global/logistic_plot.pdf", p_fit_pa_label , base_height = 5, base_width = 5)



# Calculate deterministic number of orphans
data_orphan$calculated_parent_ratio <- calc_ratio(output_p$par[1], output_p$par[2], 
                                             output_p$par[3], data_orphan$tfr)
data_orphan$calculated_orphans <- data_orphan$calculated_parent_ratio * data_orphan$total_deaths
#data_orphan$final_parent_orphans <- ifelse(is.na(data_orphan$all_parents), data_orphan$calculated_orphans, data_orphan$all_parents)

# Calculate uncertainty
n = 1000
estimates_parent <- matrix(nrow = length(data_orphan$country), ncol = n)
estimates_parent_orphans <- matrix(nrow = length(data_orphan$country), ncol = n)



for (i in 1:n){
  rn <- rnorm(length(data_orphan$country), mean = data_orphan$tfr, sd = data_orphan$sd)
  estimates_parent[, i] <- calc_ratio(output_p$par[1], output_p$par[2], output_p$par[3], rn)
  estimates_parent_orphans[, i] <- estimates_parent[, i] * data_orphan$total_deaths
}

data_orphan$estimates_parent <- rowMeans(estimates_parent_orphans)
#data_orphan$final_parent_orphans_uq <- ifelse(data_orphan$all == 0, data_orphan$estimates_parent, data_orphan$all_parents)

#orphans_samples <- colSums(estimates_parent_orphans)
orphans_samples <- rowSums(estimates_parent_orphans)

print(sprintf("Parent orphans: %0.f [%0.f - %0.f]", 
              sum(data_orphan$estimates_parent), floor(quantile(orphans_samples, probs = 0.025)), 
              ceiling(quantile(orphans_samples, probs = 0.975))))

print(sprintf("Parent orphans: %0.f [%0.f - %0.f]", 
              data_orphan$estimates_parent, floor(quantile(estimates_parent_orphans[12,], probs = 0.025)), 
              ceiling(quantile(estimates_parent_orphans[12,], probs = 0.975))))


data_orphan$colour = ifelse(data_orphan$country == "I.R. Iran", 1, 0)
data_orphan$colour = factor(data_orphan$colour)
p_obs_pred_pa = ggplot(data_orphan ) +
  geom_point(aes(orphan_death_ratio, calculated_parent_ratio)) + 
  geom_point(data = data_orphan, 
             aes(orphan_death_ratio, calculated_parent_ratio), col = "red") + 
  geom_abline(slope = 1, intercept = 0) + 
  xlab("Ratio of children orphaned to deaths of parents (observed)") + 
  ylab("Ratio of children orphaned to deaths of parents (predicted)") + 
  geom_text_repel(aes(x = orphan_death_ratio, y = calculated_parent_ratio, label = country, col = colour),
                  size = 4, max.overlaps = 100) +
  scale_color_manual(values = c("black", "red")) + 
  theme_bw() + theme(legend.position = "none")+labs(tag = 'F')
p_obs_pred_pa


cowplot::save_plot("figures/global/logistic_oberved_predict_plot.pdf", p_obs_pred_pa , base_height = 5, base_width = 5)

## Primary loo
pars = c(0.1, 0.1, 0.1)
loo_orphans = vector(length = length(data_orphan$country))
p <- ggplot(data_orphan) + 
  geom_point(aes(tfr, orphan_death_ratio)) + 
  geom_line(data = line_all, aes(x, y), col = 'black') + 
  xlab("Total fertility rate") + ylab("Ratio of children orphaned to deaths of parents") + 
  theme_bw()+labs(tag='A')
for (i in 1:length(data_orphan$country)){
  leave_one_out = data_orphan[which(data_orphan$country != data_orphan$country[i]),]
  output_loo_p = optim(pars, error_parent, data=leave_one_out)
  
  x = seq(0, 5, 0.1)
  line = data.frame(x = x, 
                    y = calc_ratio(output_loo_p$par[1], output_loo_p$par[2], 
                                   output_loo_p$par[3], x))
  p <- p + geom_line(data = line, aes(x, y), col = "blue", alpha = 0.2)
  
  data_orphan$calculated_ratio_loo_parent <- calc_ratio(output_loo_p$par[1], output_loo_p$par[2], 
                                                   output_loo_p$par[3], data_orphan$tfr)
  data_orphan$calculated_orphans_loo <- data_orphan$calculated_ratio_loo_parent * data_orphan$total_deaths 
  #data_orphan$final_orphans_loo <- ifelse(is.na(data_orphan$all_parents), data_orphan$calculated_orphans_loo, data_orphan$all_parents)
  loo_orphans[i] = sum(data_orphan$calculated_orphans_loo)
}

loo_combined = data.frame("country" = data_orphan$country,
                          "orphans" = loo_orphans)
p_loo_pa <- p + geom_line(data = line_all, aes(x, y), col = 'red') +
  geom_text_repel(aes(x = tfr, y = orphan_death_ratio, label = country), max.overlaps = 100)
p_loo_pa
print("Range loo")
print(loo_combined[which(loo_combined$orphans == min(loo_combined$orphans)),])
print(loo_combined[which(loo_combined$orphans == max(loo_combined$orphans)),])

save(p_fit_pa_label, p_obs_pred_pa, p_loo_pa, file = "DATA/extrapolate_parent.RData")

cowplot::save_plot("figures/global/loo_m3.pdf", p_loo_pa , base_height = 5, base_width = 5)






#################################################################


#formula for logistic model 4
calc_ratio_lo2 <- function(alpha, beta1, beta2, gamma, tfr,euro){
  return(gamma * (exp(alpha + beta1 * tfr + beta2* euro))/(1 + exp(alpha + beta1 * tfr+ beta2* euro)))
} 

error_parent_lo2<- function(params, data){
  return(sum((calc_ratio_lo2(params[1], params[2], params[3], params[4],data$tfr,data$euro) - data$orphan_death_ratio)^2))
}

set.seed(10)


# Calculate country specific SD
data_orphan$sd = (data_orphan$tfr_upper-data_orphan$tfr_lower)/(2*1.96)

# pearson test
print(sprintf("Pearsons r^2 primary: %f",  cor(data_orphan$tfr, data_orphan$orphan_death_ratio, method = "pearson") ))

pars = c(0.1, 0.1, 0.1,0.1)
output_p2 = optim(pars, error_parent_lo2, data=data_orphan)
saveRDS(output_p2$par, "DATA/global/parent_coefficients_m4.RDS")
print(output_p2$par)
# logistic graph
x = seq(0, 5, 0.1)
y0 = rep(0,51)
y1 = rep(1,51)
line_all_lo2 = data.frame(x = x, y = y0,
                      z = calc_ratio_lo2(output_p2$par[1], output_p2$par[2], 
                                     output_p2$par[3], output_p2$par[4], x,y0))

line_all_lo2_e = data.frame(x = x, y = y1,
                          z = calc_ratio_lo2(output_p2$par[1], output_p2$par[2],
                                             output_p2$par[3], output_p2$par[4], x,y1))



p_fit_pa2 <- ggplot(data_orphan) + 
  geom_point(aes(tfr, orphan_death_ratio)) + 
  geom_line(data = line_all_lo2, aes(x, z), col = 'blue') +  
  geom_line(data = line_all_lo2_e, aes(x, z), col = 'red') +
  xlab("Total fertility rate") + ylab("Ratio of children orphaned to deaths of parents") + 
  theme_bw()+labs(tag = 'G')
p_fit_pa_label2 <- p_fit_pa2 + geom_text_repel(aes(x = tfr, y = orphan_death_ratio, label = country), size = 4, max.overlaps = 100)
p_fit_pa_label2


cowplot::save_plot("figures/global/logistic_m4_plot.pdf", p_fit_pa_label2 , base_height = 5, base_width = 5)


# Calculate deterministic number of orphans
data_orphan$calculated_parent_ratio_m4 <- calc_ratio_lo2(output_p2$par[1], output_p2$par[2], 
                                                      output_p2$par[3], output_p2$par[4],data_orphan$tfr,data_orphan$euro)
data_orphan$calculated_orphans_m4 <- round(data_orphan$calculated_parent_ratio_m4 * data_orphan$total_deaths)
#data_orphan$final_parent_orphans <- ifelse(is.na(data_orphan$all_parents), data_orphan$calculated_orphans, data_orphan$all_parents)

# Calculate uncertainty
n = 1000
estimates_parent <- matrix(nrow = length(data_orphan$country), ncol = n)
estimates_parent_orphans <- matrix(nrow = length(data_orphan$country), ncol = n)



for (i in 1:n){
  rn <- rnorm(length(data_orphan$country), mean = data_orphan$tfr, sd = data_orphan$sd)
  estimates_parent[, i] <- calc_ratio_lo2(output_p2$par[1], output_p2$par[2], 
                                          output_p2$par[3], output_p2$par[4], rn,data_orphan$euro)
  estimates_parent_orphans[, i] <- estimates_parent[, i] * data_orphan$total_deaths
}

data_orphan$estimates_parent_m4 <- rowMeans(estimates_parent_orphans)
#data_orphan$final_parent_orphans_uq <- ifelse(data_orphan$all == 0, data_orphan$estimates_parent, data_orphan$all_parents)

#orphans_samples <- colSums(estimates_parent_orphans)
orphans_samples_m4 <- rowSums(estimates_parent_orphans)

print(sprintf("Parent orphans: %0.f [%0.f - %0.f]", 
              sum(data_orphan$estimates_parent), floor(quantile(orphans_samples, probs = 0.025)), 
              ceiling(quantile(orphans_samples, probs = 0.975))))

print(sprintf("Parent orphans: %0.f [%0.f - %0.f]", 
              data_orphan$estimates_parent_m4, floor(quantile(estimates_parent_orphans[1,], probs = 0.025)), 
              ceiling(quantile(estimates_parent_orphans[1,], probs = 0.975))))


data_orphan$colour = ifelse(data_orphan$country == "I.R. Iran", 1, 0)
data_orphan$colour = factor(data_orphan$colour)
p_obs_pred_pa_m4 = ggplot(data_orphan ) +
  geom_point(aes(orphan_death_ratio, calculated_parent_ratio_m4)) + 
  geom_point(data = data_orphan, 
             aes(orphan_death_ratio, calculated_parent_ratio_m4), col = "red") + 
  geom_abline(slope = 1, intercept = 0) + 
  xlab("Ratio of children orphaned to deaths of parents (observed)") + 
  ylab("Ratio of children orphaned to deaths of parents (predicted)") + 
  geom_text_repel(aes(x = orphan_death_ratio, y = calculated_parent_ratio_m4, label = country, col = colour),
                  size = 4, max.overlaps = 100) +
  scale_color_manual(values = c("black", "red")) + 
  theme_bw() + theme(legend.position = "none")+labs(tag = 'H')
p_obs_pred_pa_m4


cowplot::save_plot("figures/global/logistic_m4_oberved_predict_plot.pdf", p_obs_pred_pa_m4 , base_height = 5, base_width = 5)



## Primary loo for m4
pars = c(0.1, 0.1, 0.1,0.1)
loo_orphans_m4 = vector(length = length(data_orphan$country))
p_m4 <- ggplot(data_orphan) + 
  geom_point(aes(tfr, orphan_death_ratio)) + 
  geom_line(data = line_all_lo2, aes(x, z), col = 'black') +  
  xlab("Total fertility rate") + ylab("Ratio of children orphaned to deaths of parents") + 
  theme_bw()+labs(tag='B')
for (i in 1:length(data_orphan$country)){
  leave_one_out_m4 = data_orphan[which(data_orphan$country != data_orphan$country[i]),]
  output_loo_p_m4 = optim(pars, error_parent_lo2, data=leave_one_out_m4)
  
  # x = seq(0, 5, 0.1)
  # line = data.frame(x = x, 
  #                   y = calc_ratio(output_loo_p$par[1], output_loo_p$par[2], 
  #                                  output_loo_p$par[3], x))
  # 
  x = seq(0, 5, 0.1)
  y = rep(0,51)
  line_all_lo2 = data.frame(x = x, y = y,
                            z = calc_ratio_lo2(output_loo_p_m4$par[1], output_loo_p_m4$par[2], 
                                               output_loo_p_m4$par[3], output_loo_p_m4$par[4], x,y))
  
  
  p_m4 <- p_m4 + geom_line(data = line_all_lo2, aes(x, z), col = "blue", alpha = 0.2)
  
  data_orphan$calculated_ratio_loo_parent_m4 <- calc_ratio_lo2(output_loo_p_m4$par[1], output_loo_p_m4$par[2], 
                                                               output_loo_p_m4$par[3], output_loo_p_m4$par[4],data_orphan$tfr,data_orphan$euro)
  data_orphan$calculated_orphans_loo_m4 <- data_orphan$calculated_ratio_loo_parent_m4 * data_orphan$total_deaths 
  #data_orphan$final_orphans_loo <- ifelse(is.na(data_orphan$all_parents), data_orphan$calculated_orphans_loo, data_orphan$all_parents)
  loo_orphans[i] = sum(data_orphan$calculated_orphans_loo_m4)
}

loo_combined = data.frame("country" = data_orphan$country,
                          "orphans" = loo_orphans)
p_loo_pa_m4 <- p_m4 + geom_line(data = line_all_lo2, aes(x, z), col = 'red') +
  geom_text_repel(aes(x = tfr, y = calculated_parent_ratio_m4, label = country), max.overlaps = 100)
p_loo_pa_m4

print("Range loo")
print(loo_combined[which(loo_combined$orphans == min(loo_combined$orphans)),])
print(loo_combined[which(loo_combined$orphans == max(loo_combined$orphans)),])

save(p_fit_pa_label, p_obs_pred_pa, p_loo_pa, file = "DATA/extrapolate_parent.RData")

cowplot::save_plot("figures/global/loo_m4.pdf", p_loo_pa_m4 , base_height = 5, base_width = 5)




















# global studied

#############################
# Calculate uncertainty
n = 1000
estimates_parent <- matrix(nrow = length(data_orphan$country), ncol = n)
estimates_parent_orphans <- matrix(nrow = length(data_orphan$country), ncol = n)


for (i in 1:n){
  rn <- rnorm(length(data_orphan$country), mean = data_orphan$tfr, sd = data_orphan$sd)
  estimates_parent[, i] <- calc_ratio_linear(fit_linear$coefficients[1], fit_linear$coefficients[2], rn)
  estimates_parent_orphans[, i] <- estimates_parent[, i] * data_orphan$total_deaths
}

data_orphan$estimates_parent_m1 <- rowMeans(estimates_parent_orphans)
#data_orphan$final_parent_orphans_uq <- ifelse(data_orphan$all == 0, data_orphan$estimates_parent, data_orphan$all_parents)

#orphans_samples <- colSums(estimates_parent_orphans)
orphans_samples <- rowSums(estimates_parent_orphans)

print(sprintf("Parent orphans: %0.f [%0.f - %0.f]", 
              sum(data_orphan$estimates_parent), floor(quantile(orphans_samples, probs = 0.025)), 
              ceiling(quantile(orphans_samples, probs = 0.975))))

print(sprintf("Parent orphans: %0.f [%0.f - %0.f]", 
              data_orphan$estimates_parent, floor(quantile(estimates_parent_orphans[-4,], probs = 0.025)), 
              ceiling(quantile(estimates_parent_orphans[-4,], probs = 0.975))))



#############################
# Calculate uncertainty
n = 1000
estimates_parent <- matrix(nrow = length(data_orphan$country), ncol = n)
estimates_parent_orphans <- matrix(nrow = length(data_orphan$country), ncol = n)


for (i in 1:n){
  rn <- rnorm(length(data_orphan$country), mean = data_orphan$tfr, sd = data_orphan$sd)
  estimates_parent[, i] <- calc_ratio_linear_fix(fit_linear_adjust$coefficients[1], 
                                                 fit_linear_adjust$coefficients[2], 
                                                 fit_linear_adjust$coefficients[3],
                                                 data_orphan$euro,
                                                 rn)
  estimates_parent_orphans[, i] <- estimates_parent[, i] * data_orphan$total_deaths
}

data_orphan$estimates_parent_m1 <- rowMeans(estimates_parent_orphans)
#data_orphan$final_parent_orphans_uq <- ifelse(data_orphan$all == 0, data_orphan$estimates_parent, data_orphan$all_parents)

#orphans_samples <- colSums(estimates_parent_orphans)
orphans_samples <- rowSums(estimates_parent_orphans)


print(sprintf("Parent orphans: %0.f [%0.f - %0.f]", 
              data_orphan$estimates_parent, floor(quantile(estimates_parent_orphans[12,], probs = 0.025)), 
              ceiling(quantile(estimates_parent_orphans[12,], probs = 0.975))))








# linear
extrapolation <-read.csv("DATA/global_extrapolation.csv",stringsAsFactors = FALSE)
print(extrapolation)
#extrapolation$observed=round(extrapolation$observed)
predicted_linear =  calc_ratio_linear(fit_linear$coefficients[1], fit_linear$coefficients[2], extrapolation$tfr)
extrapolation$predicted_linear =predicted_linear
extrapolation$predicted_linear_orphan =round(predicted_linear*extrapolation$total_deaths)
print(extrapolation)

# linear add fixed
predicted_linear_fix =  calc_ratio_linear_fix(fit_linear_adjust$coefficients[1], 
                                              fit_linear_adjust$coefficients[2], 
                                              fit_linear_adjust$coefficients[3],
                                              extrapolation$euro,
                                              extrapolation$tfr)
extrapolation$predicted_linear_fix =predicted_linear_fix
extrapolation$predicted_fix_orphan =round(predicted_linear_fix*extrapolation$deaths)
print(extrapolation)
######################################33
#PI for lMs
pre=data.frame(extrapolation)
pre1 <- predict(fit_linear,pre,interval = 'prediction',level=0.95)
round(pre1*extrapolation$total_deaths)
extrapolation$country

pree=data.frame(extrapolation)
pre2 <- predict(fit_linear_adjust,pree,interval = 'prediction',level=0.95)
round(pre2*extrapolation$total_deaths)
extrapolation$country

data_orphan$estimates_parent_m2<-round(data_orphan$total_deaths*data_orphan$calculated_parent_ratio_adjust)
data_orphan$estimates_parent_m1<-round(data_orphan$total_deaths*data_orphan$calculated_parent_ratio_linear)

mse_m1<-sum((data_orphan$calculated_parent_ratio_linear-data_orphan$orphan_death_ratio)^2)/12
mse_m1
mse_m2<-sum((data_orphan$calculated_parent_ratio_adjust-data_orphan$orphan_death_ratio)^2)/12
mse_m2
mse_m3<-sum((data_orphan$calculated_parent_ratio-data_orphan$orphan_death_ratio)^2)/12
mse_m3
mse_m4<-sum((data_orphan$calculated_parent_ratio_m4-data_orphan$orphan_death_ratio)^2)/12
mse_m4



mse_m1_or<-sum((data_orphan$estimates_parent_m1-data_orphan$total_orphans)^2)/12
mse_m1_or
mse_m2_or<-sum((data_orphan$estimates_parent_m1-data_orphan$total_orphans)^2)/12
mse_m2_or
mse_m3_or<-sum((data_orphan$calculated_orphans-data_orphan$total_orphans)^2)/12
mse_m3_or
mse_m4_or<-sum((data_orphan$estimates_parent_m4-data_orphan$total_orphans)^2)/12
mse_m4_or

euro_countries<-data_orphan%>%filter(euro==1)
mse_m3_or_eu<-sum((euro_countries$calculated_orphans-euro_countries$total_orphans)^2)/12
mse_m3_or_eu
mse_m4_or_eu<-sum((euro_countries$estimates_parent_m4-euro_countries$total_orphans)^2)/12
mse_m4_or_eu


###############################################3
#logistic
extrapolation$predicted_log <- calc_ratio(output_p$par[1], output_p$par[2], 
                                       output_p$par[3], extrapolation$tfr)
extrapolation$predicted_log_orphan =round(extrapolation$predicted_log*extrapolation$deaths)
print(extrapolation)

extrapolation$linear_acc <- extrapolation$predicted_linear_orphan/extrapolation$observed
extrapolation$linear_fix_acc <- extrapolation$predicted_fix_orphan/extrapolation$observed
extrapolation$log_acc <- extrapolation$predicted_log_orphan/extrapolation$observed
print(extrapolation)

##############################################################################################3
# global validation---tested countries

validation <-read.csv("DATA/global_test.csv",stringsAsFactors = FALSE)
print(validation)
# Calculate country specific SD
validation$sd = (validation$tfr_u-validation$tfr_l)/(2*1.96)

#############################
#logistic m3
# Calculate uncertainty
n = 1000
estimates_parent <- matrix(nrow = length(validation$country), ncol = n)
estimates_parent_orphans <- matrix(nrow = length(validation$country), ncol = n)


for (i in 1:n){
  rn <- rnorm(length(validation$country), mean = validation$tfr, sd = validation$sd)
  estimates_parent[, i] <- calc_ratio(output_p$par[1], output_p$par[2], output_p$par[3], rn)
  estimates_parent_orphans[, i] <- estimates_parent[, i] * validation$deaths
}

validation$estimates_parent_m3 <- rowMeans(estimates_parent_orphans)
#validation$final_parent_orphans_uq <- ifelse(validation$all == 0, validation$estimates_parent, validation$all_parents)

#orphans_samples <- colSums(estimates_parent_orphans)
#orphans_samples <- rowSums(estimates_parent_orphans)


print(sprintf("Parent orphans: %0.f [%0.f - %0.f]", 
              validation$estimates_parent_m3, floor(quantile(estimates_parent_orphans[6,], probs = 0.025)), 
              ceiling(quantile(estimates_parent_orphans[6,], probs = 0.975))))

#############################
#logistic m4
# Calculate uncertainty
n = 1000
estimates_parent <- matrix(nrow = length(validation$country), ncol = n)
estimates_parent_orphans <- matrix(nrow = length(validation$country), ncol = n)


for (i in 1:n){
  rn <- rnorm(length(validation$country), mean = validation$tfr, sd = validation$sd)
  estimates_parent[, i] <- calc_ratio_lo2(output_p2$par[1], output_p2$par[2], 
                                          output_p2$par[3], output_p2$par[4], rn,validation$euro)
  estimates_parent_orphans[, i] <- estimates_parent[, i] * validation$deaths
}

validation$estimates_parent_m4 <- rowMeans(estimates_parent_orphans)
#validation$final_parent_orphans_uq <- ifelse(validation$all == 0, validation$estimates_parent, validation$all_parents)

#orphans_samples <- colSums(estimates_parent_orphans)
#orphans_samples <- rowSums(estimates_parent_orphans)


print(sprintf("Parent orphans: %0.f [%0.f - %0.f]", 
              validation$estimates_parent_m4, floor(quantile(estimates_parent_orphans[1,], probs = 0.025)), 
              ceiling(quantile(estimates_parent_orphans[1,], probs = 0.975))))







print(validation)
