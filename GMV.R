# c pour faire des vecteurs 

# Importation


symbols <-
  c(
    "AI.PA",
    "CS.PA",
    "BNP.PA",
    "EN.PA",
    "CA.PA",
    "BN.PA",
    "ENGI.PA",
    "EL.PA",
    "LR.PA",
    "OR.PA",
    "MC.PA",
    "ML.PA",
    "RI.PA",
    "SGO.PA",
    "SAN.PA",
    "SU.PA",
    "GLE.PA",
    "STLAP.PA",
    "TTE.PA",
    "VIE.PA",
    "CAP.PA",
    "AIR.PA",
    "KER.PA",
    "ORA.PA"
  )
# 
stock_prices <- symbols %>%
  tq_get(get  = "stock.prices",
         from = "2006-05-02",
         to   = "2025-08-31") %>%
  group_by(symbol) # group_by pour les regrouper par symbol (ca ne change pas la matrice mais ca permet de trier par symbol pendant les calculs)
# La matrice créee par le tq_get mets en colonne le ticker avec sa date suivi du close / volume etc
# donc c'est trier par ticker et ensuite par date

print(stock_prices %>% slice(1, n()), n = 48) # n() = la dernière ligne du groupe
# le pip permet de renvoyer direct le stock prices dans la fonction slice sans avoira le rappeler 
# équivalent a ca : print(slice(stock_prices, 1, n()))
print(stock_prices %>% slice(1, 48)) # la je regarde la matrice uniquement la premiere et la 48e ligne de la matrice


# Wrangling
 # tq_transmute package tidyquant qui sert à transformer les données financières (prix → rendements, moyennes mobiles, indicateurs techniques, etc.).
# remplace les colonnes par le résultat demandé
monthly_returns <- stock_prices %>%
  group_by(symbol) %>%
  tq_transmute(
    select     = adjusted,         # Column we want 
    mutate_fun = periodReturn,     # fonction PeriodReturn() qui calcul les rendements sur la periode
    period     = "monthly",        # On regroupe les données par la periode donnée (on prend premiere valeur du mois & la derniere et on fait le rendement)
    type       = "arithmetic",     # rendement arithmetic 
    col_rename = "monthly.returns" # nom de la nouvelle colonne 
  )
print(monthly_returns)


# Fonction pivot_wider permet de convertir le tableau en format long (largeur) 
# -> Chaque symbol devient une colonne avec ses rendement par date 

# Package tidyr
table_returns <- monthly_returns %>%
  pivot_wider(names_from = symbol, values_from = monthly.returns) %>%
  select(-"date") %>% # On enleve la colonne date car pas utile pour faire des maths
  na.omit()           # On enleve les valeurs manquantes NaN

(table_returns_first <- table_returns %>% select(1:6))

# plot_intro affiche un rapport visuel des données (histogramme de distribution / Na / apercu variables)
plot_intro(table_returns_first)
print(table_returns_first)


# Visualisation

# On part du tableau monthly_returns 
# mutate() rajoute une nouvelle colonne 
# cumprod() fait le produit cumulatif sur les rendements
# Indice cumulatif=100×(1+0.02)(1−0.01)=100×1.0098=100.98
# Résultat : toutes les actions sont mises sur la même base initiale (100), ce qui permet de comparer leur évolution relative


monthly_returns %>%  
  mutate(price.index = 100 * cumprod(1 + monthly.returns)) %>%
  ggplot(aes(x = date, y = price.index, color = symbol)) +      # visualisation ggplot, aes = mapping des variables
  geom_line(size = 1) +                                         # trace une courbe pour pour chaque action
  labs(title = "Stock Prices in the same initial base") +       # Titre
  theme_tq() +                                                  # Theme graphique 
  scale_color_tq()                                              # Palette de couleurs 



monthly_prices <- stock_prices %>%
  group_by(symbol) %>%
  tq_transmute(select = adjusted,           # On reprend la colonne ajusted 
               mutate_fun = to.monthly,     # On regroupe les observations par mois et on garde que celles du premier jour 
               indexAt = "lastof")          # Date observée a chaque mois sera celle du dernier jour du mois
# Incohérence entre les date et les observations ?


monthly_prices %>%
  ungroup() %>%
  slice_head(n = nrow(table_returns) * 6) %>%
  ggplot(aes(x = date, y = adjusted, color = symbol)) +
  geom_line(size = 1) +
  labs(
    title = "Monthly Stock Prices",
    x = "Date",
    y = "Adjusted Prices",
    color = ""
  ) +
  facet_wrap( ~ symbol, ncol = 2, scales = "free_y") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "€", prefix = "")) +
  theme_tq() +
  scale_color_tq()


summary(table_returns_first)



# distribution graphics: the non-parametric (kernel method) estimation of the distribution and QQ-plots.

monthly_returns %>%
  ungroup() %>%
  slice_head(n = nrow(table_returns) * 6) %>%
  ggplot(aes(x = monthly.returns, fill = symbol)) +
  geom_density(alpha = 0.5) +
  labs(title = "Densities of arithmetic monthly returns",
       x = "Monthly returns", y = "Density") +
  theme_tq() +
  scale_fill_tq() +
  facet_wrap(~ symbol, ncol = 2)

monthly_returns %>%
  ungroup() %>%
  slice_head(n = nrow(table_returns) * 6) %>%
  ggplot(aes(sample = monthly.returns, fill = symbol)) +
  stat_qq() +
  stat_qq_line() +
  labs(title = "QQ-plots versus Normal distribution",
       x = "Normal returns", y = "Monthly returns") +
  theme_tq() +
  scale_fill_tq() +
  facet_wrap(~ symbol, ncol = 2)

library(corrplot)
corrplot(
  cor(table_returns),
  type = 'upper',
  tl.col = 'black',
  tl.cex = 0.1
)

# separate the initial sample between a learning sample and a backtest sample to evaluate the performance of our modelling
end_date <- nrow(table_returns)
table_returns_learning <- table_returns %>% slice(1:164)
table_returns_backtest <- table_returns %>% slice(165:end_date)

n <- ncol(table_returns_learning)
T <- nrow(table_returns_learning)
e <- rep(1, n)
perio <- 12 # On est en mois 
print(table_returns_learning %>% cov)
Sigma <- cov(table_returns_learning) * (T - 1) / (T - n - 2) * perio
C <- t(e) %*% solve(Sigma) %*% e
sigmag <- sqrt(1 / C)
omega <- 1 / as.numeric(C) * solve(Sigma) %*% e
barplot(as.numeric(omega), col = 'black')

return_p <- as.matrix(table_returns_backtest) %*% omega

vol_1 = sd(as.matrix(table_returns_backtest) %*% omega) * sqrt(perio)  #a verifier sur le sqrt pour voir si c'est additif 

cat("la vol =", vol_1)
