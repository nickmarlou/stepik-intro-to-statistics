# Однофакторный дисперсионный анализ
# One-way ANOVA

# Импортируем данные из csv файла
data <- read.csv(file.choose())

# Выводим уровни фактора "Терапия" (независимой переменной)
levels <- levels(data$Therapy)

# Сортируем данные по фактору "Терапия"
data$Therapy <- ordered(data$Therapy, levels = levels)

# Считаем базовые статистики по группам - количество налюдений, средняя, стандартное отклонение
library(dplyr)
summary <- group_by(data, Therapy) %>%
  summarise(
    count = n(),
    mean = mean(expr, na.rm = TRUE),
    sd = sd(expr, na.rm = TRUE)
  )

boxplot(expr~Therapy,data=data, main="Генная терапия", 
        xlab="Вид терапии", ylab="Экспрессия гена")

# Проводим анализ вариации
anova.res <- aov(expr~Therapy, data=data)
# Выводим summary результатов анализа
summary(anova.res)

# Интерпретируем результаты

# Df - Число степеней свободы
# Sum Sq - Сумма квадратов
# Mean Sq - Средний квадрат (Sum Sq / Df)
# F value - Критерий Фишера или F-значение
# Pr (>F) - Значение P-уровня значимости для F-значения больше полученного

# В строке фактора - межгрупповые значения
# В строке residuals (остатки) - внутригрупповые значения