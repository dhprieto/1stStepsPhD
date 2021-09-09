# Anova


c_O_A.A <- readRDS("data/cronicoOrinaAnt_Antro.csv")

# Retiramos datos ordinales y texto

set.A <- c_O_A.A[,-c(1,2,3,9)]

anova <- aov(Peso.final ~ ., data = set.A)
summary(anova)
par(mfrow=c(1,2))
plot(anova, which=1:4)
par(mfrow = c(1,1))
lsr::etaSquared(anova)