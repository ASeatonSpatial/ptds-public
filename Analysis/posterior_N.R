# Investigate effet of different integration schemes
# on the posterior for abundance

library(INLA)
library(inlabru)
library(ggplot2)
library(rgeos)
library(patchwork)
library(dplyr)
source(here::here("Analysis", "gg.R"))   # some small edits to inlabru gg methods

# set ggplot theme
theme_set(theme_minimal())

#### Fitted model  ####
model_path = here::here("Analysis", "fitted_model.RDS")
fit = readRDS(model_path)

#### Other things ####
data_path = here::here("Data")
study_area = readRDS(here::here(data_path, "study_area_extended_no_crs.RDS"))
samplers = readRDS(here::here(data_path, "samplers_extended_no_crs.RDS"))
obs = readRDS(here::here(data_path, "obs_extended_no_crs.RDS"))
mesh = readRDS(here::here(data_path, "mesh_extended_no_crs.RDS"))

#### Where to save figures ####
fig_path = here::here("Figures")

# Total surveyed region as % of study area?
W = 58 / 1000
nrow(samplers@data)*pi*W^2  / gArea(study_area) * 100

# Buffer samplers points df to create transects
samplers_buffered = gBuffer(samplers, width = W, byid = TRUE)

# Did this buffer produce something of the right area?
abs(gArea(samplers_buffered) - nrow(samplers@coords)*pi*W^2) / nrow(samplers@data)*pi*W^2 * 100
# seems pretty close.

# Remove transects from study area
study_area_no_samplers = gDifference(study_area, samplers_buffered)
abs(gArea(study_area) - (gArea(study_area_no_samplers) + gArea(samplers_buffered))) / gArea(study_area) * 100
# ideally would be zero. Less than 0.03% difference, this seems... okay?  Not perfect. 

# For integration points before they are projected to mesh nodes
# use method = "direct"

# plot raw integration weights
ips_unsurveyed = ipoints(study_area_no_samplers,
                         domain = mesh,
                         int.args = list(method = "direct"))

ips_full = ipoints(study_area,
                   domain = mesh,
                   int.args = list(method = "direct"))

g1 = ggplot() +
  gg(mesh) +
  gg(ips_full, aes(colour = weight), cex = 0.3) +
  gg(study_area_no_samplers, alpha = 0.01) +
  coord_equal(xlim = c(257.75, 258.25),
              ylim = c(2190.75, 2191.25)) +
  scale_colour_viridis_c() +
  xlab("") +
  ylab("") +
  guides(colour = FALSE) +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        legend.title = element_text(size = 12, vjust = 0.75),
        legend.text = element_text(size = 12),
        legend.key.size = unit(2, "line"))

g2 = ggplot() +
  gg(mesh) +
  gg(ips_unsurveyed, aes(colour = weight), cex = 0.3) +
  gg(study_area_no_samplers, alpha = 0.01) +
  coord_equal(xlim = c(257.75, 258.25),
              ylim = c(2190.75, 2191.25)) +
  scale_colour_viridis_c() +
  xlab("") +
  ylab("") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        legend.title = element_text(size = 12, vjust = 0.75),
        legend.text = element_text(size = 12),
        legend.key.size = unit(2, "line"))

g1 + g2 + plot_layout(guides = "collect") + plot_annotation(tag_levels = "A") 

ggsave(filename = here::here(fig_path, "compare_int_scheme_raw.png"),
       width = 10, height = 5, units = "in", dpi = 150)

# Create integration points for full and for unsurveyed region
ips_unsurveyed = ipoints(study_area_no_samplers,
                         domain = mesh)

ips_full = ipoints(study_area,
                   domain = mesh)

# plot weights projected to mesh nodes
g1 = ggplot() +
  gg(mesh) +
  gg(ips_full, aes(size = weight, colour = weight)) +
  gg(study_area_no_samplers, alpha = 0.01) +
  coord_equal(xlim = c(257.75, 258.25),
              ylim = c(2190.75, 2191.25)) +
  scale_colour_viridis_c() +
  xlab("") +
  ylab("") +
  guides(size = FALSE) +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        legend.title = element_text(size = 12, vjust = 0.75),
        legend.text = element_text(size = 12),
        legend.key.size = unit(2, "line"))

g2 = ggplot() +
  gg(mesh) +
  gg(ips_unsurveyed, aes(size = weight, colour = weight)) +
  gg(study_area_no_samplers, alpha = 0.01) +
  coord_equal(xlim = c(257.75, 258.25),
              ylim = c(2190.75, 2191.25)) +
  scale_colour_viridis_c() +
  xlab("") +
  ylab("") +
  guides(size = FALSE, colour = FALSE) +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        legend.title = element_text(size = 12, vjust = 0.75),
        legend.text = element_text(size = 12),
        legend.key.size = unit(2, "line"))

g1 + g2 + plot_layout(guides = "collect") + plot_annotation(tag_levels = "A")

ggsave(filename = here::here(fig_path, "compare_int_scheme_projected.png"),
       width = 10, height = 5, units = "in", dpi = 150)

# create integration points object

ips_unsurveyed$distance = W + 10    # just anything larger than W here
W = 0.058
distance_domain = inla.mesh.1d(seq(.Machine$double.eps, W, length.out = 30))
distance_ips = ipoints(distance_domain,
                       name = "distance",
                       int.args = list(nsub = 4))
ips_surveyed = cprod(ipoints(samplers), distance_ips)
ips_surveyed$weight = ips_surveyed$weight * 2 * pi * ips_surveyed$distance

# should be close
sum(ips_surveyed$weight)
gArea(samplers_buffered)

ips_surveyed@data = ips_surveyed@data[,c("weight", "distance")]

# should be close
sum(ips_unsurveyed$weight)
gArea(study_area_no_samplers)

ips_full$distance = -1    # another indicator

ips = rbind(ips_unsurveyed,
            ips_surveyed,
            ips_full)

# should be close
sum(ips$weight)
gArea(study_area)

# Log half-normal
log_hn = function(distance, lsig){
  -0.5*(distance/exp(lsig))^2
}

# Half-normal
hn <- function(distance, lsig) exp(log_hn(distance, lsig))

# Integration scheme formulas
int_formulas = ~ {
  tmp <- c(
    unsurveyed = sum(weight * exp(Intercept + grf) * (distance > W)),
    surveyed_unobserved = sum(weight * exp(Intercept + grf) * (distance <= W & distance > 0) * (1 - hn(distance, lsig)))
  )
  c(tmp,
    predict_unobserved = sum(tmp),
    predict_surveyed_observed = sum(weight * exp(Intercept + grf) * (distance <= W & distance > 0) * hn(distance, lsig)),
    predict_everywhere = sum(weight * exp(Intercept + grf) * (distance > 0)),
    predict_old_way = sum(weight * exp(Intercept + grf) * (distance < 0))
  )
}

# test these integration schemes:
test = generate(fit,
                ips,
                formula = int_formulas,
                n.samples = 100)

head(test[,1:4])
test2 = test[c("predict_unobserved", "predict_surveyed_observed", "predict_everywhere"),]
test3 = colSums(test2[c(1,2),]) - test2[3,]
max(test3)

# compare integration schemes
test4 = test["predict_everywhere",] - test["predict_old_way",]
hist(test4, main = "difference between integration schemes")

# define range of N
N_min = round(mean(test["predict_everywhere",]) - 4*sd(test["predict_everywhere",]))
N_max = round(mean(test["predict_everywhere",]) + 4*sd(test["predict_everywhere",]))
by = round((N_max - N_min)/100)
N_seq = seq(N_min, N_max, by = by) # should be roughly length 100

n.mc = 3000
N_post = predict(fit,
                 ips,
                 ~ {
                   c(unobserved = dpois(N_seq,
                                      lambda = sum(weight * exp(Intercept + grf) *
                                        ((distance > W) +
                                        (distance <= W & distance > 0)*(1 - hn(distance, lsig))))),
                     predict_everywhere = dpois(N_seq,
                                                lambda = sum(weight * exp(Intercept + grf) * (distance > 0)))
                   )
                 },
                 n.samples = n.mc)

# For unobserved we need to shift
# PMF to be PMF for total abundance, not just unobserved.  Can then compare
# with the "predict everywhere" approach
N_post$N = c(N_seq + nrow(obs), N_seq)
N_post$type = rep(c("use observed counts", "predict everywhere"), each = length(N_seq))

ggplot(N_post) +
  geom_line(aes(x = N, y = mean, colour = type)) +
  geom_ribbon(aes(x = N,
                  ymin = mean - 2 * sd / sqrt(n.mc),
                  ymax = mean + 2 * sd / sqrt(n.mc),
                  fill = type),
              alpha = 0.2) +
  theme(legend.title = element_blank()) +
  scale_colour_manual(values = c("black", "blue")) +
  scale_fill_manual(values = c("black", "blue")) +
  ylab("p(N)")

ggsave(filename = here::here(fig_path, "compare_N_posteriors.png"),
       width = 5, height = 5, units = "in")

# Just the abundance posterior
N_post %>%
  filter(type == "predict everywhere") %>%
  ggplot() +
  geom_line(aes(x = N, y = mean)) +
  geom_ribbon(aes(x = N,
                  ymin = mean - 2 * sd / sqrt(n.mc),
                  ymax = mean + 2 * sd / sqrt(n.mc)),
              alpha = 0.2) +
  ylab("p(N)")

ggsave(filename = here::here(fig_path, "N_posterior.png"),
       width = 5, height = 5, units = "in")

# does N_post integrate to 1?
N_post %>%
  filter(type == "predict everywhere") %>%
  str()
N_ips = ipoints(inla.mesh.1d(N_seq))
sum(N_ips$weight/2 * N_post$mean) # should be close to 1

# get mean density per hectare
# this allows comparison with Figure 2 in Camp et al 2020
whole_area = gArea(study_area)*100   # hectares
N_mode = N_seq[which(N_post$mean == max(N_post$mean))]  # posterior mode
N_mode / whole_area
5500 / whole_area
4200 / whole_area  # some random end points just to get a feeling
7500 / whole_area

# just convert the plot scale to show density
N_post %>%
  filter(type == "predict everywhere") %>%
  mutate(dens = N/whole_area) %>%
  ggplot() +
  geom_line(aes(x = dens, y = mean)) +
  geom_ribbon(aes(x = dens,
                  ymin = mean - 2 * sd / sqrt(n.mc),
                  ymax = mean + 2 * sd / sqrt(n.mc)),
              alpha = 0.2) +
  ylab("posterior probability") + 
  xlab("akepa density per hectare") 

ggsave(filename = here::here(fig_path, "density_posterior.png"),
       width = 5, height = 5, units = "in")

