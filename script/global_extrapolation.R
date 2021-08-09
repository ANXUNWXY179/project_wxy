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

calc_ratio <- function(alpha, beta, gamma, tfr){
  return(gamma * (exp(alpha + beta * tfr))/(1 + exp(alpha + beta * tfr)))
} 

error_parent<- function(params, data){
  return(sum((calc_ratio(params[1], params[2], params[3], data$tfr) - data$orphan_death_ratio)^2))
}

set.seed(10)

# Load in data
data_orphan<-read.csv('DATA/global_extrapolation.csv',stringsAsFactors = FALSE)


# Calculate country specific SD
data_orphan$sd = (data_orphan$tfr_upper-data_orphan$tfr_lower)/(2*1.96)

# pearson test
print(sprintf("Pearsons r^2 primary: %f",  cor(data_orphan$tfr, data_orphan$orphan_death_ratio, method = "pearson") ))

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
p_fit_pa_label <- p_fit_pa + geom_text_repel(aes(x = tfr, y = orphan_death_ratio, label = country), size = 3, max.overlaps = 100)
p_fit_pa_label


cowplot::save_plot("figures/global/logistic_plot.pdf", p_fit_pa_label , base_height = 10, base_width = 10)



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

orphans_samples <- colSums(estimates_parent_orphans)
print(sprintf("Parent orphans: %0.f [%0.f - %0.f]", 
              sum(data_orphan$estimates_parent), floor(quantile(orphans_samples, probs = 0.025)), 
              ceiling(quantile(orphans_samples, probs = 0.975))))

print(sprintf("Parent orphans: %0.f [%0.f - %0.f]", 
              data_orphan$estimates_parent, floor(quantile(orphans_samples, probs = 0.025)), 
              ceiling(quantile(orphans_samples, probs = 0.975))))


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
                  size = 2, max.overlaps = 100) +
  scale_color_manual(values = c("black", "red")) + 
  theme_bw() + theme(legend.position = "none")+labs(tag = 'F')
p_obs_pred_pa


cowplot::save_plot("figures/global/logistic_oberved_predict_plot.pdf", p_obs_pred_pa , base_height = 10, base_width = 10)

## Primary loo
pars = c(0.1, 0.1, 0.1)
loo_orphans = vector(length = length(data_orphan$country))
p <- p_fit_pa
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
p_loo_pa <- p + geom_line(data = line_all, aes(x, y), col = 'red') 
# + 
#   geom_text_repel(aes(x = tfr, y = orphan_death_ratio, label = country), max.overlaps = 100)

print("Range loo")
print(loo_combined[which(loo_combined$orphans == min(loo_combined$orphans)),])
print(loo_combined[which(loo_combined$orphans == max(loo_combined$orphans)),])

save(p_fit_pa_label, p_obs_pred_pa, p_loo_pa, file = "DATA/extrapolate_parent.RData")




# r <- cor.test(data_orphan$tfr,data_orphan$orphan_death_ratio, use='complete.obs', method = 'pearson')
# r
# scatter plot
p <- ggplot(data_orphan) +
  geom_point( aes(x = tfr, y = orphan_death_ratio)) +
  xlab("Total fertility rate") + ylab("Ratio of children orphaned to all-cause deaths") + 
  theme_bw()
p <- p + geom_text_repel(aes(x = tfr, y = orphan_death_ratio, label = country), size = 3, max.overlaps = 100)
p

cowplot::save_plot("figures/global/scatter_plot.pdf", p, base_height = 10, base_width = 10)


# logistic regression 
fit_logistic <- glm(orphan_death_ratio ~ tfr, family = 'binomial', data = data_orphan)
summary(fit_logistic)




# linear model
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
cowplot::save_plot("figures/global/linear_plot.pdf", p_linear, base_height = 10, base_width = 10)



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
                  size = 2, max.overlaps = 100) +
  scale_color_manual(values = c("black", "red")) + 
  theme_bw() + theme(legend.position = "none")
p_obs_pred_pa

cowplot::save_plot("figures/global/linear_predic_observed_plot.pdf", p_obs_pred_pa, base_height = 10, base_width = 10)



# fixed effect for European countries

calc_ratio_linear_fix <- function(intercept, beta, beta2,euro,tfr){
  return(beta*tfr+beta2*euro+intercept)
} 

fit_linear_adjust <- lm(orphan_death_ratio ~ tfr+ euro, data = data_orphan)
summary(fit_linear_adjust)



p_adjust_s<-p+ 
  geom_abline(slope = fit_linear_adjust$coefficients[2], intercept = fit_linear_adjust$coefficients[1], 
              color = "black", size = 1, alpha = 0.5) +labs(tag='C')
p_adjust_s
cowplot::save_plot("figures/global/linear_adjust_plot.pdf", p_adjust_s, base_height = 10, base_width = 10)


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
                  size = 2, max.overlaps = 100) +
  scale_color_manual(values = c("black", "red")) + 
  theme_bw() + theme(legend.position = "none")+labs(tag='D')
p_obs_pred_pa_adjust 

cowplot::save_plot("figures/global/adjust_linear_predic_observed_plot.pdf", p_obs_pred_pa_adjust , base_height = 10, base_width = 10)






# global validation
# linear
validation <-read.csv("DATA/global_test.csv",stringsAsFactors = FALSE)
print(validation)
validation$observed=round(validation$observed)
predicted_linear =  calc_ratio_linear(fit_linear$coefficients[1], fit_linear$coefficients[2], validation$tfr)
validation$predicted_linear =predicted_linear
validation$predicted_linear_orphan =round(predicted_linear*validation$deaths)
print(validation)

# linear add fixed
predicted_linear_fix =  calc_ratio_linear_fix(fit_linear_adjust$coefficients[1], 
                                                        fit_linear_adjust$coefficients[2], 
                                                        fit_linear_adjust$coefficients[3],
                                                        validation$euro,
                                                        validation$tfr)
validation$predicted_linear_fix =predicted_linear_fix
validation$predicted_fix_orphan =round(predicted_linear_fix*validation$deaths)
print(validation)

#logistic
validation$predicted_log <- calc_ratio(output_p$par[1], output_p$par[2], 
                                                  output_p$par[3], validation$tfr)
validation$predicted_log_orphan =round(validation$predicted_log*validation$deaths)
print(validation)

validation$linear_acc <- validation$predicted_linear_orphan/validation$observed
validation$linear_fix_acc <- validation$predicted_fix_orphan/validation$observed
validation$log_acc <- validation$predicted_log_orphan/validation$observed
print(validation)