# https://crd230.github.io/lab7.html#Moran_Scatterplot
# https://crd230.github.io/lab8.html
# https://r-tmap.github.io/tmap/reference/tm_polygons.html
# https://tmap.geocompx.org/xx-scales
# https://cran.r-project.org/web/packages/patchwork/readme/README.html

  rm(list=ls())
  
# Load necessary packages
  library(plm)
  library(spdep)
  library(tidyverse)
  library(lmtest)
  library(splm)
  library(fixest)
  library(modelsummary)
  library(spatialreg) 
  library(gstat)
  library(patchwork)
  library(beepr)
  library(estimatr)
  library(spgwr)
 
  t1 = Sys.time()
  set.seed(1234)
  n_squares = 20
  n_hex =n_squares^2
  
# Create a square base area and project it to 3857 (meters), and then make a grid
  base <- st_as_sfc(st_bbox(c(xmin = 0, ymin = 0, xmax = 20000, ymax = 20000)), crs = 3857)
  bg_grid <- st_make_grid(base,n = c(n_squares, n_squares), square = TRUE, crs = 3857)
  bg_grid <- st_sf(geometry=bg_grid)
  hex_df <- tibble( hex_id = 1:n_hex, shelter = 0)
  bg_grid <- bg_grid %>% cbind(hex_df)
  centroids <- st_centroid(bg_grid)
  coords <- st_coordinates(centroids)
# Convert to data.frame for gstat compatibility
  sim_data <- data.frame(x = coords[,1], y = coords[,2])

# Define spatial structure (variogram model)
# Adjust 'range' and 'sill' for different spatial dependence
  vg_model <- vgm(psill = 10000, model = "Exp", range = 10000, nugget = 100)
  # Simulate med_income
  g.med <- gstat(formula = z ~ 1, locations = ~x + y, dummy = TRUE, beta = 60000,
                 model = vg_model, nmax = 20)
  bg_grid$med_income <- predict(g.med, newdata = sim_data, nsim = 1)$sim1
  bg_grid <- bg_grid %>% mutate(med_income= med_income/1000)
  bg_grid$med_income <- scales::rescale(bg_grid$med_income, to = c(20,50))
  hist(bg_grid$med_income)
  
  # Shelter more likely in places with low income
  bg_grid <- bg_grid %>% mutate(shelter = rbinom(nrow(bg_grid), 1, .085 - med_income/1000))
  hist(bg_grid$shelter)
  
  # Crime higher in places with low income and with shelters
  bg_grid <- bg_grid %>% mutate(cr_inc = 1.5 - .03 * med_income + .5 * shelter+rnorm(n_hex, mean = .25, sd = .25))
  hist(bg_grid$cr_inc)
  
# Plot to see the spatial correlation (with small sample may not be visible much)
  p1 <- ggplot(bg_grid) + geom_sf(aes(fill = med_income), color = NA) +
    scale_fill_viridis_c(option = "plasma") +
    labs(title = "Simulated med_income", fill = "med_income") +
    theme_minimal()
  p2 <- ggplot(bg_grid) + geom_sf(aes(fill = cr_inc), color = NA) +
    scale_fill_viridis_c(option = "magma") +
    labs(title = "Simulated cr_inc", fill = "cr_inc") +
    theme_minimal()
  p1 + p2 # Combine using patchwork package 
 
# Distribution of shelters 
  ggplot(bg_grid) + geom_sf(aes(fill = shelter), color = NA) +
    scale_fill_viridis_c(option = "magma") +
    labs(title = "Simulated cr_inc", fill = "cr_inc") +
    theme_minimal()

  datasummary_balance(med_income+cr_inc~shelter, bg_grid, stars=TRUE)
  
# Estimate cross section regressions with spatial features 
  bg_grid_b<-poly2nb(bg_grid, queen=T)
  bg_grid_w<-nb2listw(bg_grid_b, style="W", zero.policy = TRUE) 
  moran.plot(as.numeric(scale(bg_grid$med_income)), listw=bg_grid_w, 
           xlab="med income", 
           ylab="Neighbors Standardized med income",
           main=c("Moran Scatterplot for med income", "xx") )
  moran.plot(as.numeric(scale(bg_grid$cr_inc)), listw=bg_grid_w, 
           xlab="cr_inc", 
           ylab="Neighbors Standardized cr_inc",
           main=c("Moran Scatterplot for cr_inc", "xx") )
  
  moran.mc(bg_grid$med_income, bg_grid_w, nsim=999)
  moran.test(bg_grid$med_income, bg_grid_w)  
  
  reg1 <- lm(cr_inc ~ med_income+shelter, data = bg_grid)
  reg2 <- lagsarlm(cr_inc ~ med_income+shelter, data = bg_grid, listw = bg_grid_w) 
  reg3 <- errorsarlm(cr_inc ~ med_income+shelter, data = bg_grid,  listw = bg_grid_w) 
  reg4 <- lagsarlm(cr_inc ~ med_income + shelter, data = bg_grid, listw = bg_grid_w, type = "mixed")
  reg5 <- errorsarlm(cr_inc ~ med_income + shelter, data = bg_grid, listw = bg_grid_w, Durbin = TRUE)
  reg6 <- sacsarlm(cr_inc ~ med_income + shelter, data = bg_grid, listw = bg_grid_w)
  models=list(reg1, reg2, reg3, reg4, reg5, reg6)
  modelsummary(models, stars=T)
  
  reg7 <- spgwr::gwr(cr_inc ~ med_income + shelter, data = bg_grid, coords = coords, adapt = 0.1)  
  bg_grid$med_income_coef <- reg7$SDF$med_income
  bg_grid$shelter_coef    <- reg7$SDF$shelter
  bg_grid$med_income_tval <- reg7$SDF$med_income / gwr_model$SDF$med_income_se  
  
  ggplot(bg_grid) +
  geom_sf(aes(fill = shelter_coef)) +
  scale_fill_viridis_c() +
  theme_minimal() +
  labs(title = "Local effect of Median Income on Crime Rate")
  
  bg_grid$residuals <- reg7$SDF$gwr.e
  
  ggplot(bg_grid) +
  geom_sf(aes(fill = residuals)) +
  scale_fill_gradient2() +
  theme_minimal() +
  labs(title = "GWR Residuals")
  
  beep()
  t2 = Sys.time()
  difftime(t2,t1, units=c("mins"))
 