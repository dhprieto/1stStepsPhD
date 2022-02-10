# pasos 

1. preprocess.R 
----

genera las columnas de tiempo y endulzante a partir de los códigos de los voluntarios.

--> evalua los na's y quita columnas/filas
--> quita outliers
--> escala si es necesario


2. descriptiveStatistics.R
----

--> realiza análisis estadísticos descriptivos y devuelve un reporte


3. featureImportance.R
----

--> Realiza un análisis de las variables más relevantes para los modelos que se van a lanzar. 


4. clustering
----

busca posibles clusters en 

5. anova.R
----

realiza varias anovas de tres vías de medidas repetidas y devuelve reporte

6. regression.R
----

modeliza con diferentes métodos predictores para obtener las variables continuas.

7. classification.R
----

modeliza predictores para obtener los factores Sexo/Endulzante a partir del resto de variables.
