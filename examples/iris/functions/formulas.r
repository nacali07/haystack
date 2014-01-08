irisLogitFormula<-formula(
  isVirginica ~ Petal_Width + Petal_Length + Sepal_Length + Sepal_Width
)
irisRfFormula<-formula(
  isVirginica ~ Petal_Width + Petal_Length + Sepal_Length + Sepal_Width
)
" In both logistic and R factor fitting models formulae isVirginica is modeled as petal_Width, Petal_Length
  Sepal_Length, and Sepal_Width (i.e these are predictors used to model isVirginica)
"
