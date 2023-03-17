##### Instalações de Bibliotecas para o Projeto

##### Site para Consulta de Ajuda em R
# https://gabrielluizone.notion.site/Linguagem-R-4583a81cd8404b2791ddc6eef80044fc

if(!require(tidyverse)) install.packages('tidyverse');library(tidyverse)
if(!require(ggplot2)) install.packages('ggplot2');library(ggplot2)
if(!require(dplyr)) install.packages('dplyr');library(dplyr)
if(!require(ggcorrplot)) install.packages('ggcorrplot');library(ggcorrplot)

df <- df %>% 
  mutate(Genere = ifelse(Sex == 'female', 1, 0 ))

##### Carregamento dos dados
df <- read.csv('data/titanic.csv') # Variável principal `Survived

### Gráfico de Correlação (Eu tentei, mas não ficava perfeito)
df %>%
  select_if(is.numeric) %>%
  na.omit() %>%
  cor() %>%
  ggcorrplot(lab = TRUE, lab_size = 3, hc.order = TRUE, type = "lower")

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #


which(df$Name == "Joseph Laroche")


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #


### Comparação das Faixa Etária dos Sexos em relação a Sobrevivencia 

df %>%
  na.omit() %>%
  mutate(age_group = cut(Age, breaks = seq(0, max(Age) + 5, by = 5), right = FALSE, labels = paste0(seq(0, max(Age), by = 5), "-", seq(4, max(Age), by = 5)))) %>%
  count(age_group, Survived, Sex) %>%
  mutate(Percentage = round(n/sum(n)*100,2)) %>%
  ggplot(aes(x = age_group, y = n, fill = factor(Survived))) +
  geom_col(position = "dodge") +
  geom_text(aes(label = paste0(Percentage, "%")), position = position_dodge(width = 1), vjust = -0.5, color = "#202124", size = 3) +
  scale_fill_manual(name = "Survived", values = c("#d3290f", "#52d053"), labels = c("Based", "Tmj")) +
  facet_wrap(~ Sex) +
  labs(title = "Frequência de Sobreviventes e Mortes por Idade e Sexo", x = "Idade", y = "Frequência")

### Houve uma Enviesamento entre Classes no Salvamento?

ggplot(df, aes(x = Age, fill = factor(Pclass))) +
  geom_histogram(binwidth = 5, color = "black", alpha = 0.8) +
  facet_grid(. ~ Sex) +
  labs(title = "Distribuição de idades dos passageiros do Titanic",
       x = "Idade", y = "Frequência") +
  scale_fill_discrete(name = "Classe", labels = c("1ª classe", "2ª classe", "3ª classe"))

### Agrupando os dados por classe e sobrevivência e calculando as contagens

df %>%
  group_by(Pclass, Survived) %>%
  summarise(count = n()) %>%
  mutate(Percentage = round(count/sum(count)*100,2)) %>%
  mutate(Survived = ifelse(Survived == 1, "Survived", "Died")) %>%
  ggplot(aes(x = factor(Pclass), y = Percentage, fill = Survived)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = paste0(count)), position = position_dodge(width = 1), vjust = -0.5, color = "#202124", size = 3) +
  geom_text(aes(label = paste0(round(Percentage, 0), "%")), position = position_dodge(width = 1), vjust = 1.5, color = "black", size = 3) +
  scale_fill_manual(name = "Survived", values = c("#d3290f", "#52d053"), labels = c("Died", "Survived")) +
  scale_x_discrete(limits=c("1", "2", "3")) +
  labs(title = "Proporção de sobreviventes e óbitos por classe de passageiros", x = "Classe", y = "Proporção")

### Verificando a quantidade por genero

df %>%
  group_by(Pclass, Survived, Sex) %>%
  summarise(count = n()) %>%
  mutate(Percentage = round(count/sum(count)*100,2)) %>%
  mutate(Survived = ifelse(Survived == 1, "Survived", "Died")) %>%
  ggplot(aes(x = factor(Pclass), y = Percentage, fill = Survived)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = paste0(count)), position = position_dodge(width = 1), vjust = -0.5, color = "#202124", size = 3) +
  geom_text(aes(label = paste0(round(Percentage, 0), "%")), position = position_dodge(width = 1), vjust = 1.5, color = "black", size = 3) +
  scale_fill_manual(name = "Survived", values = c("#d3290f", "#52d053"), labels = c("Died", "Survived")) +
  scale_x_discrete(limits=c("1", "2", "3")) +
  facet_wrap(~Sex, nrow=1) +
  labs(title = "Proporção de sobreviventes e óbitos por classe de passageiros", x = "Classe", y = "Proporção")



##########################################################
# Calculando as proporções e as porcentagens
survival_by_class <- df %>%
  group_by(Pclass, Survived) %>%
  summarise(count = n())

survival_by_class$prop <- survival_by_class$count / sum(survival_by_class$count)
survival_by_class$percent <- paste0(round(survival_by_class$prop * 100, 1), "%")

# Gerando o gráfico
ggplot(survival_by_class, aes(x = factor(Pclass), y = prop, fill = factor(Survived))) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = percent), position = position_stack(vjust = 0.5)) + # adiciona as porcentagens nas barras
  labs(title = "Taxa de sobrevivência por classe", x = "Classe", y = "Proporção") +
  scale_fill_manual(name = "Sobreviveu?", values = c("#d3290f", "#52d053"), labels = c("Não", "Sim"))

##########################################################################
### Distribuição de idades por classe e gênero;

survival_by_class_gender <- df %>%
  group_by(Pclass, Survived, Sex) %>%
  summarise(count = n())

survival_by_class_gender$prop <- survival_by_class_gender$count / sum(survival_by_class_gender$count)
survival_by_class_gender$percent <- paste0(round(survival_by_class_gender$prop * 100, 1), "%")

ggplot(survival_by_class_gender, aes(x = factor(Pclass), y = prop, fill = factor(Survived))) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = percent), position = position_stack(vjust = 0.5)) +
  labs(title = "Taxa de sobrevivência por classe e gênero", x = "Classe", y = "Proporção") +
  scale_fill_manual(name = "Sobreviveu?", values = c("#d3290f", "#52d053"), labels = c("Não", "Sim")) +
  facet_wrap(. ~ Sex)


### Relação entre a tarifa paga e a classe em que o passageiro estava;

ggplot(df, aes(x = factor(Pclass), y = Fare, fill = factor(Pclass))) +
  geom_boxplot() +
  labs(title = "Relação entre tarifa paga e classe",
       x = "Classe", y = "Tarifa") +
  scale_fill_discrete(name = "Classe",
                      labels = c("1ª classe", "2ª classe", "3ª classe"))

cor(family_survival$FamilySize, family_survival$Survived)

### Taxa de Sobrevivencia das Familias e seus membros

survival_by_fam <- df %>%
  mutate(fam_size = SibSp + Parch + 1) %>%
  group_by(fam_size) %>%
  summarise(survival_rate = mean(Survived)) 

ggplot(survival_by_fam, aes(x = fam_size, y = survival_rate)) +
  geom_bar(stat = "identity", fill = "#52d053") +
  labs(title = "Taxa de sobrevivência em relação ao número de familiares a bordo",
       x = "Número de familiares a bordo", y = "Taxa de sobrevivência")


hist(df$Age, breaks = 20, main = "Distribuição de Idade dos Passageiros", xlab = "Idade", ylab = "Frequência")

