# api.R

#* @apiTitle API para classificação de sobrevivência no Titanic

#* Classificação via Regressão Logística
#* @param age Valor numérico para a variável Age
#* @param sex Valor para a variável Sex (por exemplo, 'male' ou 'female')
#* @param pclass Valor para a variável Pclass (1, 2 ou 3)
#* @get /classificacao
function(age, sex, pclass) {
  # Converter age
  age_numeric <- as.numeric(age)
  
  # Converter sex e pclass para factor
  sex_factor <- as.factor(sex)
  pclass_factor <- as.factor(pclass)
  
  # Data frame para predizer
  new_data <- data.frame(
    Age = age_numeric,
    Sex = sex_factor,
    Pclass = pclass_factor
  )
  
  # Probabilidade estimada (type='response' -> probabilidade de Survived = 1)
  prob <- predict(modelo_logistico, newdata = new_data, type = 'response')
  
  # Converter a probabilidade em classe binária
  classe <- ifelse(prob > 0.5, 1, 0)
  
  list(
    probabilidade = unname(prob),
    classe = classe
  )
}
