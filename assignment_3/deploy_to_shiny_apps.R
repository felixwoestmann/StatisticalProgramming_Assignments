install.packages('rsconnect')
rsconnect::setAccountInfo(name='felixwoestmann',
                          token='794C26F05F3D92ECC4E15CA1D102126D',
                          secret='se9kMtFpzrW9oYIX09buulmmqOM5X85EGekdKaiX')
library(rsconnect)
rsconnect::deployApp('/Users/felix/Documents/Slovenia/University/StatisticalProgramming/assignments/assignment_3')
