ggplot(df, aes(y = Age)) +
  geom_boxplot() +
  labs(title = "Boxplot da Idade dos passageiros do Titanic")

# Cálculo dos limites de valor extremo
q1_age <- quantile(df$Age, 0.25, na.rm = TRUE)
q3_age <- quantile(df$Age, 0.75, na.rm = TRUE)
iqr_age <- q3_age - q1_age
lower_age <- q1_age - 1.5 * iqr_age
upper_age <- q3_age + 1.5 * iqr_age

cat("Limite inferior para a variável Age:", lower_age, "\n")
cat("Limite superior para a variável Age:", upper_age, "\n\n")

# Visualização de outliers na variável Fare
ggplot(df, aes(y = Fare)) +
  geom_boxplot() +
  labs(title = "Boxplot da Tarifa dos passageiros do Titanic")

# Cálculo dos limites de valor extremo
q1_fare <- quantile(df$Fare, 0.25, na.rm = TRUE)
q3_fare <- quantile(df$Fare, 0.75, na.rm = TRUE)
iqr_fare <- q3_fare - q1_fare
lower_fare <- q1_fare - 1.5 * iqr_fare
upper_fare <- q3_fare + 1.5 * iqr_fare

cat("Limite inferior para a variável Fare:", lower_fare, "\n")
cat("Limite superior para a variável Fare:", upper_fare, "\n\n")

# Visualização de outliers na variável SibSp (número de irmãos ou cônjuges a bordo)
ggplot(df, aes(y = SibSp)) +
  geom_boxplot() +
  labs(title = "Boxplot do Número de irmãos/cônjuges a bordo dos passageiros do Titanic")

# Cálculo dos limites de valor extremo
q1_sibsp <- quantile(df$SibSp, 0.25, na.rm = TRUE)
q3_sibsp <- quantile(df$SibSp, 0.75, na.rm = TRUE)
iqr_sibsp <- q3_sibsp - q1_sibsp
lower_sibsp <- q1_sibsp - 1.5 * iqr_sibsp
upper_sibsp <- q3_sibsp + 1.5 * iqr_sibsp

cat("Limite inferior para a variável SibSp:", lower_sibsp, "\n")
cat("Limite superior para a variável SibSp:", upper_sibsp, "\n\n")