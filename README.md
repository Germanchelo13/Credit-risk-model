# Credit_risk_model

En este trabajo se contó con la base de datos loan_data_2007_2014.csv obtenida a través de kaggle. Esta contiene información perteneciente a usuarios entre los años 2007 y 2014 de lendingclub, una empresa que realiza préstamos digitales en Estados Unidos ( lendingclub ).

Se ajusta un modelo logístico para calcular la probabilidad de que un usuario inclumpla con sus obligaciones financieras al cabo de un año desde que empezo un crédito, luego este modelo se traslada a un score crediticio donde se crea una aplicación web donde el usuario puede ingresar valores que puede responder a la hora de solicitar un prestamo y tener un score de referencia.

## ¿Qué hay en el repositorio?


- coef_score.csv: coeficientes del modelo logístico transformados a score.

- loan_data_2007_2014.csv: data set del trabajo.

- score_poblacional.csv: el score de todos los individuos (excluyendo individuos según el informe) del data set.

- app.R: es el código para generar la <a href="https://germanpatino.shinyapps.io/Credit-risk-model/"> aplicación web </a>.

- informe.Rmd: Código que genera el informe (lenguaje python y R).

- informe.html: Informe.


