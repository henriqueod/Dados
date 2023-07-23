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
         'hrbrthemes','mdscore','ROCR','pROC'),'DescTools',
         require,character.only=TRUE)
setwd("C:/Users/henri/OneDrive/Documentos/GitHub/Dados")



# ------------------------------------------------------------
# Banco de dados
# ------------------------------------------------------------
dados <- read_xlsx("dados_trabalho.xlsx")
colnames(dados) <- c("ID", "idade", "status", "casa", "setor",
                     "poup", "hrbrthemes")
train$status <- factor(train$status)
train$casa   <- factor(train$casa)
train$setor  <- factor(train$setor)
train$poup   <- factor(train$poup)
test$status  <- factor(test$status)
test$casa    <- factor(test$casa)
test$setor   <- factor(test$setor)
test$poup    <- factor(test$poup)
dados$status <- factor(dados$status)
dados$casa   <- factor(dados$casa)
dados$setor  <- factor(dados$setor)
dados$poup   <- factor(dados$poup)
str(dados)


# ------------------------------------------------------------
# Separando entre dados de treino e de teste
# ------------------------------------------------------------
#dados$ID <- 1:nrow(dados)
#train <- dados %>% dplyr::sample_n(100)
#test  <- dplyr::anti_join(dados, train, by = 'ID')
#write.csv(train, "train.csv")
#write.csv(test, "test.csv")

# ------------------------------------------------------------
# Ler dados já salvos
# ------------------------------------------------------------
train <- read.csv("train.csv")
test  <- read.csv("test.csv")

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

# Gráfico de barras das demais variáveis
par(mfrow = c(2, 2))
barplot(table(dados$poup), names = c("Sem poupança", "Com poupança"), density=c(20,20), col="brown", main="Conta poupança", ylab = "Frequência", ylim = c(0,120))
barplot(table(dados$status), names = c("Superior", "Médio", "Inferior"), density=c(20,20,20), col="blue", main = "Status econômico", ylim = c(0,80))
barplot(table(dados$casa ), names = c("Sim, quitada", "Não"), density=c(20,20), col="gray", main = "Possui casa própria", ylab = "Frequência", ylim = c(0,120))
barplot(table(dados$setor), names = c("Setor B", "Setor A"), density=c(20,20), col="orange", main = "Setor da cidade", ylim = c(0,150))

# Gráfico de Dispersão: Proporção com Poupança x Idade
dados$idadecat <- NA
for(i in seq(0, 70, by=10)){
dados$idadecat[dados$idade>=i & dados$idade<i+10] <- i+5
}
dados$idadecat[dados$idade>=70] <- 75
idade=seq(5, 75, by=10)
casos <- NA
proporcao <- NA
for(i in 1:8){
casos[i] <- length(dados$poup[dados$idadecat==idade[i]])
proporcao[i]=sum(dados$poup[dados$idadecat==idade[i]])/length(dados$poup[dados$idadecat==idade[i]])
}
tabela <- as.data.frame(cbind(idade,casos,proporcao))
ggplot(tabela, aes(x=idade, y=proporcao)) + 
  geom_point(size=3) +
  labs(x='Idade',y='Proporção com poupança') +
  theme_minimal() +
  theme(panel.border=element_blank(),axis.text=element_text(size = 13),
        axis.title=element_text(size = 14),axis.title.x=element_text(margin=margin(t=15)),
        axis.title.y=element_text(margin=margin(r=15))) +
  ylim(.15,1.05)

# Testes de Independencia
chisq.test(table(dados$casa,dados$poup))
chisq.test(table(dados$setor,dados$poup))
MHChisqTest(table(dados$status,dados$poup))

# Modelo de regressão logística
fit1 <- glm(data = train,
            poup ~ idade + status + casa + setor,
            family = binomial(link = "logit"))

summary(fit1)
wald.test(fit1, 5)

anova(fit1, test="Chisq")

fit2 <- glm(data = train,
            poup ~ idade + status + casa,
            family = binomial(link = "logit"))

summary(fit2)
wald.test(fit2, 4)


table(train$poup, train$status)
table(train$poup, train$casa)
table(train$poup, train$setor)
table(train$poup)

# 4 a 4 e 3 a 3
summary(glm(data = train,
            poup ~ idade + status + casa + setor,
            family = binomial(link = "logit")))
summary(glm(data = train,
            poup ~ idade + status + casa,
            family = binomial(link = "logit")))
summary(glm(data = train,
            poup ~ idade + status + setor,
            family = binomial(link = "logit")))
summary(glm(data = train,
            poup ~ idade + casa + setor,
            family = binomial(link = "logit")))
summary(glm(data = train,
            poup ~ status + casa + setor,
            family = binomial(link = "logit")))
# 2 a 2
summary(glm(data = train,
            poup ~ idade + status,
            family = binomial(link = "logit")))
summary(glm(data = train,
            poup ~ idade + casa,
            family = binomial(link = "logit")))
summary(glm(data = train,
            poup ~ idade + setor,
            family = binomial(link = "logit")))
summary(glm(data = train,
            poup ~ status + casa,
            family = binomial(link = "logit")))
summary(glm(data = train,
            poup ~ status + setor,
            family = binomial(link = "logit")))
summary(glm(data = train,
            poup ~ casa + setor,
            family = binomial(link = "logit")))
# 1 a 1
summary(glm(data = train,
            poup ~ idade,
            family = binomial(link = "logit")))
summary(glm(data = train,
            poup ~ status,
            family = binomial(link = "logit")))
summary(glm(data = train,
            poup ~ casa,
            family = binomial(link = "logit")))
summary(glm(data = train,
            poup ~ setor,
            family = binomial(link = "logit")))

plot(fit2)
# -------------------------------------------------------
# Predição
# -------------------------------------------------------
pred <- fit2 %>%
  predict(test, type="response")

pred

roc(test$poup ~ pred, plot = TRUE, print.auc = TRUE,
    xlab = "1-Especificidade", ylab = "Sensibilidade")

predicted.classes <- ifelse(pred > 1-0.5459184, 1, 0)
table(predicted.classes, test$poup)
mean(predicted.classes == test$poup)


prediction <- prediction(pred, test$poup)
perf <- performance(prediction)
plot(perf, colorize=TRUE)
?performance
# -------------------------------------------------------
# Proporção de poupança
# -------------------------------------------------------
sum(as.numeric(dados$poup))/nrow(dados$poup)
sum(as.numeric(dados$poup)-1)/nrow(dados)
dados$poup


# -------------------------------------------------------
# Gráfico envelope
# -------------------------------------------------------
fit.model <- fit2
par(mfrow=c(1,1))
X <- model.matrix(fit.model)
n <- nrow(X)
p <- ncol(X)
w <- fit.model$weights
W <- diag(w)
H <- solve(t(X)%*%W%*%X)
H <- sqrt(W)%*%X%*%H%*%t(X)%*%sqrt(W)
h <- diag(H)
td <- resid(fit.model,type="deviance")/sqrt(1-h)
e <- matrix(0,n,100)
#
for(i in 1:100){
  dif <- runif(n) - fitted(fit.model)
  dif[dif >= 0 ] <- 0
  dif[dif<0] <- 1
  nresp <- dif
  fit <- glm(nresp ~ X, family=binomial)
  w <- fit$weights
  W <- diag(w)
  H <- solve(t(X)%*%W%*%X)
  H <- sqrt(W)%*%X%*%H%*%t(X)%*%sqrt(W)
  h <- diag(H)
  e[,i] <- sort(resid(fit,type="deviance")/sqrt(1-h))}
#
e1 <- numeric(n)
e2 <- numeric(n)
#
for(i in 1:n){
  eo <- sort(e[i,])
  e1[i] <- (eo[2]+eo[3])/2
  e2[i] <- (eo[97]+eo[98])/2}
#
med <- apply(e,1,mean)
faixa <- range(td,e1,e2)
par(pty="s")
qqnorm(td,xlab="Percentil da N(0,1)",
       ylab="Componente do Desvio", ylim=faixa, pch=16, main="")
#
par(new=TRUE)
#
qqnorm(e1,axes=F,xlab="",ylab="",type="l",ylim=faixa,lty=1, main="")
par(new=TRUE)
qqnorm(e2,axes=F,xlab="",ylab="", type="l",ylim=faixa,lty=1, main="")
par(new=TRUE)
qqnorm(med,axes=F,xlab="", ylab="", type="l",ylim=faixa,lty=2, main="")
#------------------------------------------------------------#
