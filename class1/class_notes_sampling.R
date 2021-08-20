# U = letters
# S = {x_1, x_2, x_3, x_4} <= P(u\inS)
# -> combinations
# -> letters
N <- length(letters)
n <- 4
fact  <- function(n) prod(seq(n))
combs <- function(N, n) fact(N)/(fact(N-n)*fact(n))
combs(N-1, n-1)/combs(N, n)
# ----------------------------------------------------------
# EJERCICIO 2
# 1 = extraer prefijos name1.name2@mail.com -> name1name2
# 2 = generar una muestra
# 3 = calcular estimador
# 4 = comparar con valor poblacional
# ----------------------------------------------------------
# Lectura de nombres
sample_size <- 5
names <- read.csv('./data/alumnos.txt', header=F)
# Limpieza de nombres
(clean_names <- 
    apply(names, 1,
          function(t) str_length(str_replace_all(str_split(t, '@' )[[1]][1], 
                                                 '\\.', 
                                                 ''))
))
N <- length(clean_names)
# Extract sample
names_sample <- sample(clean_names, sample_size)
# Compute stat
t_hat <- N*mean(names_sample)
# Comparar contra valor real
sum(clean_names)
# TLC
print('Values dist x')
print(paste('MEAN x: ', mean(clean_names)))
print(paste('SD x: ', sd(clean_names)))
# Extraer muestras
N <- length(clean_names)
n <- 15
names_samps <- combinations(N, n, clean_names, set=F)
names_means <- apply(names_samps, 1, mean)
print('Values dist x_bar')
print(paste('MEAN x_bar: ', mean(names_means)))
print(paste('SD x_bar: ', sd(names_means)))
hist(names_means)
### EJERCICIO CENSO
census <- (read.csv('./data/census.csv') %>%
             rename_with(tolower) %>%
             na_if('') %>%
             na_if('*') %>%
             filter(!is.na(longitud)) %>%
             select(nom_ent, entidad, pobtot,  graproes) %>%
             mutate(graproes = parse_number(graproes)) %>%
             na.omit())

## Generate Stats
samp_size <- 500
census_samp <- sample(nrow(census), samp_size)
(census[census_samp, ] %>%
    summarise(
      mean_pop = mean(pobtot), 
      s_square_pop = var(pobtot), 
      var_mean_pop = (1-n/N)*s_square_pop/n, 
      se_mean_pop = sqrt(var_mean_pop), 
      cv_mean_pop = se_mean_pop/mean_pop,
      tot_pop = N*mean_pop, 
      var_tot_pop = N**2*var_mean_pop, 
      se_tot_pop = sqrt(var_tot_pop), 
      cv_tot_pop = se_tot_pop/tot_pop, 
      mean_grad = mean(graproes), 
      s_square_grad = var(graproes),
      var_mean_grad = (1-n/N)*s_square_grad/n, 
      se_mean_grad = sqrt(var_mean_grad), 
      cv_mean_grad = se_mean_grad/mean_grad)
  )

hist(census[census_samp, ]$pobtot)
# ------------------------------
# Experimento TLC
# ------------------------------
pob <- rexp(10000, 1)
hist(pob)
var(pob)
# 0.973288
n_samples <- 100
sample_size <- 100
## 
mean_pob <- map_dbl(seq(n_samples), ~mean(sample(pob, sample_size)))
hist(mean_pob)
var(mean_pob)
# ------------------------------
# Dist muestra
# ------------------------------
head(locs_samp)

ggplot(locs_samp) + geom_density(aes(x = graproes))

ggplot(locs_samp) + geom_density(aes(x=pobtot))

# -----------------------------------
# qqplots
# -----------------------------------
step <- .01
x <- seq(-3, 3, length.out = 100)
norm_dist <- dnorm(x, 0, 1)
ggplot(tibble(x = x, y = norm_dist)) + geom_line(aes(x=x, y=y))
# REAL NORMAL
norm_cuants <- quantile(norm_dist, seq(0, 1, step))
# GRAD PROES
grad_proes <- (locs_samp$pobtot - mean(locs_samp$pobtot))/sd(locs_samp$pobtot)

(
ggplot(tibble(grads = grad_proes)) + geom_density(aes(x=grads)) + 
    geom_line(data=(tibble(x = x, y = norm_dist)), 
                 aes(x=x, y = y),
                 color='blue')
)

grad_cuants <- quantile(grad_proes, seq(0, 1, step))

(ggplot(data = tibble(norm = norm_cuants, 
                     grad = grad_cuants)) + 
    geom_point(aes(x = norm, y = grad)) + 
    geom_point(aes(x = norm, y = norm), col='red') + 
    ylim(-1, 2) 
  )
# Intervalos de confianza

# n \dist norm
norm_df <- tibble(
x = seq(-3.5, 3.5, .01),
y = dnorm(x))

alpha <- .05
q1 <- qnorm(alpha/2)
q2 <- qnorm(1-alpha/2)

(ggplot(data=norm_df) + 
    geom_line(aes(x = x, y = y)) + 
    geom_area(data=(norm_df %>% 
                      filter(x >= q1 & x <= q2)), 
              aes(x = x, y = y), fill='blue')
    )

q1 <- qnorm(.05)
## n depende de la precision epsilon
epsilon <- .03
n_0 <- (q1*stats$se_mean_grap/epsilon)**2
# ---------------------------------
# Bootstrap
# ---------------------------------
n_min <- 28 + 25*(skewness(locs_samp_2$pobtot))**2
print(n_min)

locs_samp <- locs[sample(N, n_min), ]
stats <- get_stats(locs_samp, N, n_min)
stats$tot_pop

# Definir un intervalo de confianza para este estimador
B <- 500

b_pop <- map_dbl(seq(B),
                 ~N*mean(sample(locs_samp$pobtot, nrow(locs_samp),
                                replace = T)))

q1_b <- quantile(b_pop, alpha/2)
q2_b <- quantile(b_pop, 1-alpha/2)

(ggplot(tibble(b_pop = b_pop)) + 
    geom_histogram(aes(x=b_pop), binwidth = 1000000) + 
  geom_histogram(data=(tibble(b_pop = b_pop) %>% 
                         filter(b_pop >= q1_b &  b_pop <= q2_b)), 
                       aes(x=b_pop), fill='blue', binwidth = 1000000))

### INTERVALOS ESTRATIFICADOS
locs <- (read_csv('./data/census.csv') %>% 
           rename_with(tolower) %>% 
           select(nom_ent, entidad, longitud, pobtot, graproes, 
                  graproes_f, graproes_m) %>%
           na_if('*') %>%
           mutate(longitud = parse_number(longitud), 
                  graproes = parse_number(graproes), 
                  graproes_f = parse_number(graproes_f), 
                  graproes_m = parse_number(graproes_m)) %>%
           drop_na())

# Obtener muestras de 10% por entidad federativa
# comparar resultados de estadisticos
# población total y educación promedio. 
percent <- .1
ents <- unique(locs$entidad)
all_strata <- list()
n_strats <- list()
for(ent in ents){
  strat <- locs %>% filter(entidad == ent)
  N_strat <- nrow(strat)
  n_strat <- floor(N_strat*percent) + 1 # queremos el 10%
  # Obtener muestra
  strat_samp <- sample(nrow(strat), n_strat)
  # Obtener estadisticos
  strat_stats <- get_stats(strat[strat_samp, ], N_strat, n_strat)
  all_strata[[ent]] <- strat_stats
  n_strats[[ent]] <- n_strat
}



all_strata_df <- map_df(all_strata, ~.)
all_strata_df['ent'] <- ents
all_strata_df['n_strat'] <- unlist(n_strats)

## Get global stats from strata
# Tot pob
sum(all_strata_df$tot_pop)
# Mean pob
(all_strata_df %>% 
    transmute(pop_strata = mean_pop*n_strat) %>% 
    summarise(mean_pop = sum(pop_strata)/(nrow(locs)*.1)))
# Mean grap
(all_strata_df %>% 
    transmute(grap_strata = mean_grap*n_strat) %>% 
    summarise(mean_grap = sum(grap_strata)/(nrow(locs)*.1)))


# Muestreo reserva
samp_size <- 1000
S <- locs[1:samp_size, ] 
U <- runif(samp_size)
for(i in samp_size:nrow(locs)){
  xt <- locs[i, ]
  ut <- runif(1)
  imax <- which.max(U)
  if(ut < U[imax]){
    S[imax, ] <- xt
    U[imax] <- ut
  }
}

ggplot(S) + geom_bar(aes(x=nom_ent, y = ..prop.., group = 1), stat = "count", fill='blue', alpha=.5) + 
  geom_bar(data=locs, aes(x=nom_ent, y=..prop.., group=1), sat='count', fill='red', alpha=.5) + coord_flip()


ggplot(S) + geom_bar(aes(x=graproes, y = ..prop.., group = 1), stat = "count", fill='blue', alpha=.5) + 
  geom_bar(data=locs, aes(x=graproes, y=..prop.., group=1), sat='count', fill='red', alpha=.5)








































