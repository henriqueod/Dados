# ------------------------------------------------------------
# Disciplina: Análise de Dados Categorizados – 1/2023
# Profa.: Maria Terezs
# ------------------------------------------------------------
# Em um estudo deseja-se analisar fatores associados a um chefe
# de família ter uma poupança ou não. Para tal foi selecionada
# uma amostra aleatória de 196 chefes de família em dois setores
# de uma cidade.
# ------------------------------------------------------------
# Variáveis
# ------------------------------------------------------------
# ID - número de identificação do chefe da família
# X1 - Idade (em anos)
# X2 - Status econômico    (1 = superior, 2 = médio, 3 = inferior)
# X3 - Possui casa própria (1 = não ou ainda não quitada
#                           2 = sim, quitada)
# X4 - Setor da cidade     (1 = setor A, 0 = setor B)
# X5 - Conta poupança      (0 = não, 1 = sim)
# ------------------------------------------------------------
# Analisem os dados de modo a alcançar os objetivos do estudo
# utilizando modelos de regressão logística. O pesquisador deseja
# correr no máximo um risco de 5% de estar errado em suas conclusões.
# ------------------------------------------------------------

# ------------------------------------------------------------
# BIBLIOTECAS
# ------------------------------------------------------------
lapply(c('dplyr','lubridate','stats','ggplot2', 'readxl', 'viridis',
         'hrbrthemes'),require,character.only=TRUE)
setwd("C:/Users/henri/OneDrive/Documentos/GitHub/Dados")



# ------------------------------------------------------------
# Banco de dados
# ------------------------------------------------------------
dados <- read_xlsx("dados_trabalho.xlsx")
colnames(dados) <- c("ID", "idade", "status", "casa", "setor",
                     "poup", "hrbrthemes")
dados$status <- factor(dados$status)
dados$casa   <- factor(dados$casa)
dados$setor  <- factor(dados$setor)
dados$poup   <- factor(dados$poup)
str(dados)


# ------------------------------------------------------------
# Separando entre dados de treino e de teste
# ------------------------------------------------------------
dados$ID <- 1:nrow(dados)
train <- dados %>% dplyr::sample_n(100)
test  <- dplyr::anti_join(dados, train, by = 'ID')
write.csv(train, "train.csv")
write.csv(test, "test.csv")

# ------------------------------------------------------------
# Análise descritiva
# ------------------------------------------------------------
summary(dados)

# Box-plot idade por poupança
ggplot(data = train, aes(x = poup, y = idade, fill = poup, alpha = 0.5)) +
  geom_boxplot() +
  scale_fill_manual(values=c("antiquewhite", "snow2")) +
  geom_jitter(size=0.5, alpha=0.9) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)) +
  ggtitle("Distribuição das idades x Conta poupança") +
  xlab("") + ylab("Idade")

hist(train$idade)

# Gráfico de barras das demais variáveis
ggplot(data = train, aes(x = poup)) +
  geom_bar(position="dodge") +
  scale_fill_viridis(discrete = T, option = "E") +
  ggtitle("Studying 4 species..") +
  facet_wrap(~ poup , status , casa , setor)

# Gráfico de barras das demais variáveis
par(mfrow = c(2, 2))
barplot(table(train$poup), names = c("Sem poupança", "Com poupança"), density=c(50,50), col="brown", main="Conta poupança", ylab = "Frequência", ylim = c(0,120))
barplot(table(train$status), names = c("Superior", "Médio", "Inferior"), density=c(50,50,50), col="blue", main = "Status econômico", ylim = c(0,80))
barplot(table(train$casa ), names = c("Sim, quitada", "Não"), density=c(50,50), col="gray", main = "Possui casa própria", ylab = "Frequência", ylim = c(0,120))
barplot(table(train$setor), names = c("Setor B", "Setor A"), density=c(50,50), col="orange", main = "Setor da cidade", ylim = c(0,150))

# Modelo de regressão logística
fit1 <- glm(poup ~ idade + status + casa + setor,
            data = train,
            family = binomial(link = "logit"))
summary(fit1)

table(train$poup, train$status)
table(train$poup, train$casa)
table(train$poup, train$setor)
table(train$poup)

@yanvianna




