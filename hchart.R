if (!require(highcharter)){install.packages("highcharter")}; library(highcharter)
if (!require(dplyr)){install.packages("dplyr")}; library(dplyr)
if (!require(palmerpenguins)){install.packages("palmerpenguins")}; library(palmerpenguins) # remotes::install_github("allisonhorst/palmerpenguins")
if (!require(ggplot2)){install.packages("ggplot2")}; library(ggplot2)

# data frames -----------------------------------------------------------------
data(penguins, package = "palmerpenguins")
data(diamonds, economics_long, package = "ggplot2")

##
hchart(penguins, "scatter", hcaes(x = body_mass_g, y = flipper_length_mm , group = species))

##
penguins2 <- penguins %>%
  count(species, island) %>%
  glimpse()
hchart(penguins2, "column", hcaes(x = island, y = n, group = species))

# Check automatically if the x column is date class:
economics_long2 <- economics_long %>%
  filter(variable %in% c("pop", "uempmed", "unemploy"))
hchart(economics_long2, "line", hcaes(x = date, y = value01, group = variable))

# Numeric histogram -------------------------------------------------------
x <- diamonds$price
hchart(x)

# Densities ---------------------------------------------------------------
hchart(density(x), type = "area", color = "#B71C1C", name = "Price")

# Character factor --------------------------------------------------------
x <- diamonds$cut
hchart(x, type = "column")

# Time series -------------------------------------------------------------
hchart(LakeHuron, name = "Level") %>% 
  hc_title(text = "Level of Lake Huron 1875–1972")

# Seasonal decomposition of time series by loess --------------------------
x <- stl(log(AirPassengers), "per")
hchart(x)

# Igraph package ----------------------------------------------------------
library(igraph)
N <- 40
net <- sample_gnp(N, p = 2 / N)
wc <- cluster_walktrap(net)

V(net)$label <- seq(N)
V(net)$name <- paste("I'm #", seq(N))
V(net)$page_rank <- round(page.rank(net)$vector, 2)
V(net)$betweenness <- round(betweenness(net), 2)
V(net)$degree <- degree(net)
V(net)$size <- V(net)$degree
V(net)$comm <- membership(wc)
V(net)$color <- colorize(membership(wc))

hchart(net, layout = layout_with_fr)

# Survival package --------------------------------------------------------
library(survival)
data(cancer, package = "survival")
lung <- dplyr::mutate(cancer, sex = ifelse(sex == 1, "Male", "Female"))
fit <- survfit(Surv(time, status) ~ sex, data = cancer)

hchart(fit, ranges = TRUE)

# Quantmod package --------------------------------------------------------
library(quantmod)

x <- getSymbols("GOOG", auto.assign = FALSE)
hchart(x)

# Multivariate time series ------------------------------------------------
x <- cbind(mdeaths, fdeaths)
hchart(x)

# Autocovariance Autocorrelation ------------------------------------------
x <- acf(diff(AirPassengers), plot = FALSE)
hchart(x)

# Principal components ----------------------------------------------------
hchart(princomp(USArrests, cor = TRUE))

# Matrix ------------------------------------------------------------------
data(volcano)

hchart(volcano) %>% # changing default color
  hc_colorAxis(
    stops = color_stops(colors = c("#000004FF", "#56106EFF", "#BB3754FF", "#F98C0AFF", "#FCFFA4FF"))
  )

# Distance matrix ---------------------------------------------------------
mtcars2 <- mtcars[1:20, ]
x <- dist(mtcars2)
hchart(x)

# Correlation matrix ------------------------------------------------------
hchart(cor(mtcars))
