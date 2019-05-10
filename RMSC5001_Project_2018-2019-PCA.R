## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, dpi = 300, fig.width = 7)


## ----load packages, message=FALSE, warning=FALSE-------------------------
library(corrplot)
library(ggplot2)
library(ggthemes)
library(tidyverse)
library(zoo)
library(factoextra)
library(FactoMineR)
library(knitr)


## ----echo=FALSE, results='asis'------------------------------------------
field <- read.csv("field.csv")
print(field)
kable(field, caption = "Field information")


## ----read data-----------------------------------------------------------
set.seed(123456)
d <- read.csv("season-1718_csv.csv", stringsAsFactors = FALSE)
d1 <- d %>%
  group_by(HomeTeam) %>%
  summarise(
    HS = sum(HS),
    AS = sum(AS),
    HST = sum(HST),
    AST = sum(AST),
    HF = sum(HF),
    AF = sum(AF),
    HC = sum(HC),
    AC = sum(AC),
    HY = sum(HY),
    AY = sum(AY),
    HR = sum(HR),
    AR = sum(AR)
  )
league.df <- column_to_rownames(d1, var = "HomeTeam")
print(league.df)
print(summary(league.df))
ggplot(stack(league.df), aes(x = ind, y = values)) +
  geom_boxplot(fill = "#3d195b", colour = "red") +
  labs(
    x = "Variables",
    y = "Values",
    title = "Boxplot for 12 variables"
  )


## ----correlation plot----------------------------------------------------
r <- cor(league.df)
print(r)
corrplot.mixed(r, lower = "square", upper = "number", order = "FPC")


## ----pca-----------------------------------------------------------------
pca <- prcomp(league.df, center = TRUE, scale = TRUE)
print(summary(pca))


## ----scree plot----------------------------------------------------------
fviz_screeplot(pca,
  addlabels = TRUE, ylim = c(0, 50),
  main = "Screeplot of the first 10 PCs",
  barfill = "#3d195b", barcolor = "#3d195b"
)


## ----pca variance--------------------------------------------------------
var <- get_pca_var(pca)
print(var)
corrplot(var$cos2, is.corr = FALSE, method = "square")
# Total cos2 of variables on PC1 and PC2
fviz_cos2(pca,
  choice = "var", axes = 1:2,
  title = "Sum of independent variables in PC1 and PC2",
  fill = "#3d195b", color = "#3d195b"
)


## ----eigenvalues analysis------------------------------------------------
print(zapsmall(get_eigenvalue(pca)))


## ----loadings of pc------------------------------------------------------
print((zapsmall(var$contrib[, 1:5])))


## ----plot loadings contribution to pc1-----------------------------------
# Contributions of variables to PC1
fviz_contrib(pca, choice = "var", axes = 1, top = 12, fill = "#3d195b", color = "#3d195b")


## ----pc2-----------------------------------------------------------------
# Contributions of variables to PC2
fviz_contrib(pca, choice = "var", axes = 2, top = 12, fill = "#3d195b", color = "#3d195b")


## ----pc3-----------------------------------------------------------------
# Contributions of variables to PC3
fviz_contrib(pca, choice = "var", axes = 3, top = 12, fill = "#3d195b", color = "#3d195b")


## ----pc4-----------------------------------------------------------------
# Contributions of variables to PC4
fviz_contrib(pca, choice = "var", axes = 4, top = 12, fill = "#3d195b", color = "#3d195b")


## ----pc5-----------------------------------------------------------------
# Contributions of variables to PC5
fviz_contrib(pca, choice = "var", axes = 5, top = 12, fill = "#3d195b", color = "#3d195b")


## ----scatter plot--------------------------------------------------------
score1 <- pca$x[, 1:5]
pairs(score1)


## ----pca var-------------------------------------------------------------
fviz_pca_var(pca,
  col.var = "cos2",
  gradient.cols = c("black", "blue", "red"),
  repel = TRUE # Avoid text overlapping
)


## ----clubs contribution of each pc---------------------------------------
fviz_contrib(pca,
  choice = "ind",
  axes = 1,
  fill = "#3d195b",
  color = "#3d195b",
  title = "Total Contribution of clubs to PC1"
)
fviz_contrib(pca,
  choice = "ind",
  axes = 2,
  fill = "#3d195b",
  color = "#3d195b",
  title = "Total Contribution of clubs to PC2"
)
fviz_contrib(pca,
  choice = "ind",
  axes = 3,
  fill = "#3d195b",
  color = "#3d195b",
  title = "Total Contribution of clubs to PC3"
)
fviz_contrib(pca,
  choice = "ind",
  axes = 4,
  fill = "#3d195b",
  color = "#3d195b",
  title = "Total Contribution of clubs to PC4"
)
fviz_contrib(pca,
  choice = "ind",
  axes = 5,
  fill = "#3d195b",
  color = "#3d195b",
  title = "Total Contribution of clubs to PC5"
)


## ----biplot--------------------------------------------------------------
fviz_pca_biplot(pca,
  repel = TRUE,
  col.var = "blue", # Variables color
  col.ind = "#3d195b" # Individuals color
)


## ----loadings------------------------------------------------------------
(Loadings <- pca$rotation[, 1:5] %>%
  round(2) %>%
  data.frame() %>%
  mutate(Attribute = rownames(.)) %>%
  select(Attribute, everything()) %>%
  arrange(PC1))
print(Loadings)

## ----time series of clubs------------------------------------------------
d$PC1 <- pca$x[, 1]
d$PC2 <- pca$x[, 2]

d %>%
  ggplot() +
  geom_line(aes(x = as.Date(Date), y = PC1, group = 1), show.legend = F, colour = "#3d195b") +
  geom_hline(yintercept = 0, colour = "red") +
  facet_wrap(~HomeTeam) +
  labs(
    x = "Date",
    y = "PC1",
    title = "PC1 time series by clubs"
  ) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_x_date(date_labels = "%Y (%b)")


## ----comparison----------------------------------------------------------
d %>%
  ggplot() +
  geom_line(aes(x = as.Date(Date), y = PC2, group = 1), show.legend = F, colour = "#3d195b") +
  geom_hline(yintercept = 0, colour = "red") +
  facet_wrap(~HomeTeam) +
  labs(
    x = "Date",
    y = "PC2",
    title = "PC2 time series by clubs"
  ) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_x_date(date_labels = "%Y (%b)")


## ----pc1 and pc2 mean----------------------------------------------------
d %>%
  group_by(HomeTeam) %>%
  summarise(
    Index.1 = mean(PC1),
    Index.2 = mean(PC2)
  ) %>%
  arrange(desc(Index.1)) %>%
  select(HomeTeam, Index.1, Index.2)


## ----rolling mean of pc1 and pc2-----------------------------------------
d %<>% group_by(HomeTeam) %>%
  arrange(Date) %>%
  mutate(
    M.A.PC1 = rollmeanr(PC1, 15, fill = NA),
    M.A.PC2 = rollmeanr(PC2, 15, fill = NA)
  )

Index.1718 <- d %>%
  group_by(HomeTeam) %>%
  filter(Date == max(Date)) %>%
  select(HomeTeam, M.A.PC1, M.A.PC2, Date) %>%
  arrange(M.A.PC1)

ggplot(Index.1718, aes(x = M.A.PC1, y = M.A.PC2)) +
  geom_point(show.legend = F, colour = "#3d195b", size = 5) +
  geom_text(aes(label = HomeTeam),
    check_overlap = TRUE, nudge_y = 0.08,
    show.legend = F, size = 2.9
  ) +
  labs(x = "PC1", y = "PC2")


## ----away team analysis--------------------------------------------------
d2 <- d %>%
  group_by(AwayTeam) %>%
  summarise(
    HS = sum(HS),
    AS = sum(AS),
    HST = sum(HST),
    AST = sum(AST),
    HF = sum(HF),
    AF = sum(AF),
    HC = sum(HC),
    AC = sum(AC),
    HY = sum(HY),
    AY = sum(AY),
    HR = sum(HR),
    AR = sum(AR)
  )
away.df <- column_to_rownames(d2, var = "AwayTeam")
print((away.df))
print(summary(away.df))
ggplot(stack(away.df), aes(x = ind, y = values)) +
  geom_boxplot(fill = "#3d195b", colour = "red") +
  labs(
    x = "Variables",
    y = "Values",
    title = "Boxplot for 12 variables"
  )
pca2 <- prcomp(away.df, center = TRUE, scale = TRUE)
print(summary(pca2))
fviz_pca_var(pca2,
  col.var = "cos2",
  gradient.cols = c("black", "blue", "red"),
  repel = TRUE # Avoid text overlapping
)
d$PC1a <- pca2$x[, 1]
d$PC2a <- pca2$x[, 2]

d %>%
  ggplot() +
  geom_line(aes(x = as.Date(Date), y = PC1a, group = 1), show.legend = F, colour = "#3d195b") +
  geom_hline(yintercept = 0, colour = "red") +
  facet_wrap(~HomeTeam) +
  labs(
    x = "Date",
    y = "PC1a",
    title = "PC1a time series by clubs (Away)"
  ) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_x_date(date_labels = "%Y (%b)")

d %>%
  ggplot() +
  geom_line(aes(x = as.Date(Date), y = PC2a, group = 1), show.legend = F, colour = "#3d195b") +
  geom_hline(yintercept = 0, colour = "red") +
  facet_wrap(~AwayTeam) +
  labs(
    x = "Date",
    y = "PC2a",
    title = "PC2a time series by clubs (Away)"
  ) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_x_date(date_labels = "%Y (%b)")

d %>%
  group_by(AwayTeam) %>%
  summarise(
    Index.1 = mean(PC1a),
    Index.2 = mean(PC2a)
  ) %>%
  arrange(desc(Index.1)) %>%
  select(AwayTeam, Index.1, Index.2)

