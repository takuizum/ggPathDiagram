# Write Path Diagram through ggplot.
library(ggnetwork);library(tidyverse);library(lavaan);library(broom)


# 
# The Holzinger and Swineford (1939) example
HS.model <- ' visual  =~ x1 + x2 + x3
              textual =~ x4 + x5 + x6
              speed   =~ x7 + x8 + x9 '

fit <- lavaan(HS.model, data=HolzingerSwineford1939,
              auto.var=TRUE, auto.fix.first=TRUE,
              auto.cov.lv.x=TRUE)
summary(fit, fit.measures=TRUE)
para <- tidy(fit)

# Detect coordinates of nodes.
# Check model structure
operator <- para %>% 
  distinct(op) %>% pull

LATENT <- operator %in% "=~" %>% any # Check if model has latent structure.
REGRESSION <- operator %in% "~" %>% any # Check if model has stracture equation.
VCOV <- operator %in% "~~" %>% any # Check if model has VCOV assumption between variables.
CONSTRAIN <- operator %>% str_detect("~[0-9]") %>% any # Check if model impose any constrain on any valiable.

# Confirm the number of variables in each type.
ov_name <- fit@pta$vnames$ov %>% pluck(1)
eqs_y_name <- fit@pta$vnames$eqs.y %>% pluck(1)
eqs_x_name <- fit@pta$vnames$eqs.x %>% pluck(1)
if(!is.null(eqs_x_name))
  ov_name <- ov_name %>% str_remove_all(eqs_x_name)
lv_name <- fit@pta$vnames$lv %>% pluck(1)

# Model structure detection
STRUCTURE <- list(ov_name, lv_name, eqs_x_name, eqs_y_name) %>% 
  map_chr(~ is_null(.x) %>% ifelse("F", "T")) %>% 
  str_c(collapse = "")
SEM <- case_when(STRUCTURE, 
                 "TTFF" ~ "FACTOR ANALYSIS MODEL", 
                 "TFFF" ~ "OBSERVED REGRESSION MODEL",
                 "TTTT" ~ "MEASURE & STRUCTURE EQUATION MODEL"
                 )

# ov
n_ov <- length(ov_name)
n_lv <- length(lv_name)
n_eqx <- length(eqs_x_name)
n_eqy <- length(eqs_y_name)

spacer <- function(x) x[-c(1, length(x))]

# y axis lovation
ov_y <- seq(0, 1, by = 1/ (n_ov - n_eqx + 1) ) %>% spacer # blank space
lv_y <- seq(0, 1, by = 1/ (n_lv+1) ) %>% spacer # blank space
if(n_eqx != 0){
  ov_x <- 0.2
  lv_x <- 0.7
} else {
  ov_x <- 0.0
  lv_x <- 0.5
  eqs_x_x <- 1.0
  eqs_x_y <- seq(0, 1, by = 1/(n_eqx+1) ) %>% spacer # blank spase
}

node_coord <- 
  fit %>% 
  parameterEstimates %>% as_tibble %>% 
  distinct(lhs) %>%
  mutate(n_x = 0, n_y = 0)
# x axis
node_coord[node_coord$lhs %in% ov_name, "n_x"] <- ov_x
node_coord[node_coord$lhs %in% lv_name, "n_x"] <- lv_x
node_coord[node_coord$lhs %in% eqs_x_name, "n_x"] <- eqs_x_x
# y axis
node_coord[node_coord$lhs %in% ov_name, "n_y"] <- ov_y
node_coord[node_coord$lhs %in% lv_name, "n_y"] <- lv_y
node_coord[node_coord$lhs %in% eqs_x_name, "n_y"] <- eqs_x_y
# end point
node_coord <- node_coord %>% mutate(n_xend = n_x, n_yend = n_y)


# Create Edges
edge_coord <- fit %>% 
  parameterEstimates %>% as_tibble %>% 
  select(lhs, op, rhs, est, se, pvalue)

node_edge_coord <- node_coord %>% 
  bind_rows(
    left_join(edge_coord, node_coord %>% select(lhs, n_x, n_y), by = "lhs") %>% 
      left_join(node_coord %>% select(lhs, n_xend, n_yend), by = c("rhs" = "lhs"))
  ) %>% 
  mutate(
    est = round(est, digits = 3),
    shape = ifelse(lhs %in% lv_name, "latent", "observed"),
    midpoint.x = (n_x + n_xend)/2,
    midpoint.y = (n_y + n_yend)/2,
    e_x    = ifelse(op == "~", n_xend, n_x),
    e_xend = ifelse(op == "~", n_x, n_xend),
    e_y    = ifelse(op == "~", n_yend, n_y),
    e_yend = ifelse(op == "~", n_y, n_yend),
    rise = e_yend - e_y,
    run  = e_x - e_xend,
    dist = sqrt(run^2 + rise^2) %>% round(2), 
    e_xend = case_when(lhs %in% lv_name ~ (e_x + .9 *(e_xend - e_x)),
                       lhs %in% ov_name ~ (e_x + .9*(e_xend - e_x)),
                       lhs %in% eqs_x_name ~ (e_x + .9*(e_xend - e_x))
    ), 
    e_yend = case_when(lhs %in% lv_name ~ (e_y + .9 *(e_yend - e_y)),
                       lhs %in% ov_name ~ (e_y + .9*(e_yend - e_y)),
                       lhs %in% eqs_x_name ~ (e_y + .9*(e_yend - e_y))
    )
  ) 

full_join(node_coord, edge_coord) %>% view


# Make the Diagram
node_edge_coord %>% 
  ggplot(aes(x = n_x, y = n_y, xend = n_xend, yend = n_yend)) +
  geom_edges(aes(x = e_x, y = e_y, xend = e_xend, yend = e_yend),
             arrow = arrow(length = unit(6, "pt"), type = "closed", ends = "last")) +
  geom_nodes(aes(shape = factor(shape, levels = c("observed", "latent"))), 
             colour = "gray50", size = 16) + 
  ggrepel::geom_label_repel(aes(x = midpoint.x, y = midpoint.y, label = est), 
                            colour = "black", label.size = NA, hjust = .5, vjust = .5) +
  scale_y_continuous(expand = c(.05, 0)) +
  scale_shape_manual(values = c(15, 19), guide = FALSE) + # specify the shape
  theme_blank()


# Re try

lav_para_tbl <- 
  fit %>% 
  parameterEstimates %>% as_tibble# %>% view
# distinct(lhs, op, rhs)
lav_para_tbl %>% 
  mutate(vl_type = case_when(op == "=~" ~ 'latent', 
                             op == "~"  ~ "regression",
                             op == "~~" ~ "vcov", 
                             op == "~1" ~ "meanstructure"))

# Confirm the number of variables in each type.
ov_name <- fit@pta$vnames$ov %>% .[[1]] %>% 
  str_remove_all(fit@pta$vnames$eqs.x %>% .[[1]])
lv_name <- fit@pta$vnames$lv %>% .[[1]]
eqs_y_name <- fit@pta$vnames$eqs.y %>% .[[1]]
eqs_x_name <- fit@pta$vnames$eqs.x %>% .[[1]]

# ov
ov_y <- seq(0, 1, 
            by = 1/ (length(fit@pta$vnames$ov %>% .[[1]]) - length(fit@pta$vnames$eqs.x %>% .[[1]]) + 1) ) %>% 
  .[-1] %>% .[-length(.)]
lv_y <- seq(0, 1, by = 1/ (length(fit@pta$vnames$lv %>% .[[1]])+1) ) %>% .[-1] %>% .[-length(.)]# blank space
if(length(fit@pta$vnames$eqs.x %>% .[[1]]) != 0){
  ov_x <- 0.2
  lv_x <- 0.7
} else {
  ov_x <- 0.0
  lv_x <- 0.5
  eqs_x_x <- 1.0
  eqs_x_y <- seq(0, 1, by = 1/(length(fit@pta$vnames$eqs.x %>% .[[1]])+1) ) %>% .[-1] %>% .[-length(.)] # blank spase
}