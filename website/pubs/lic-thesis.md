---
title:   Modelando Generadores de Datos Aleatorios Mediante Procesos Estocásticos
authors: Agustín Mista
year:    2018
venue:   National University of Rosario
type:    thesis
pdf:     /assets/pdf/lic-thesis.pdf
comment: Licentiate thesis (in Spanish)
date:    2018-06-22
---

El testing aleatorio de software es una de las técnicas de verificación más
prolíferas de los últimos años. En la comunidad de Haskell, QuickCheck es sin
lugar a dudas la herramienta de testing aleatorio más influyente. El
desarrollador describe las propiedades que su sistema debe verificar, y
QuickCheck luego se encarga de generar un gran número de datos aleatorios
buscando vulnerar alguna de ellas. Este proceso usualmente requiere que el
desarrollador escriba manualmente generadores de datos aleatorios para los tipos
de datos involucrados en las propiedades del sistema. Si bien este proceso puede
ser automatizado utilizando herramientas de meta-programación, los resultados
obtenidos carecen de la robustez requerida muchas veces en la práctica.

En este trabajo se provee un formalismo matemático basado en procesos
estocásticos capaz de predecir la distribución de datos generados aleatoriamente
mediante generadores escritos usando QuickCheck. A partir del mismo, se presenta
un mecanismo de obtención automática de generadores aleatorios, junto a una
heurística capaz de optimizar los parámetros de generación, con el fin de
ajustar la distribución de los datos generados a las demandas del desarrollador.
Junto a esto, se provee una implementación en Haskell de nuestras ideas, con la
cual realizamos experimentos sobre sistemas de software existentes de diversa
complejidad. Nuestros resultados indican mejoras importantes en el cubrimiento
de código obtenido respecto de las herramientas presentes en el estado del arte
en materia de testing aleatorio automático en Haskell.
