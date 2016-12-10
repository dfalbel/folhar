# folhar

[![Travis-CI Build Status](https://travis-ci.org/dfalbel/folhar.svg?branch=master)](https://travis-ci.org/dfalbel/folhar)

O objetivo deste pacote é fornecer algumas funções simples permitindo fazer
webscrapping de notícias do site da Folha de São Paulo.

## Instalação

You can install folhar from github with:

```R
# install.packages("devtools")
devtools::install_github("dfalbel/folhar")
```

## Exemplo

```R
library(folhar)
busca <- folha_buscar("estatistica", "01/11/2016", "05/12/2016")
```

## Encontrou algum problema?

Para alguma notícia o parser funcionou mal? Ficou faltando alguma informação que 
você precisava? Reporte [aqui](https://github.com/dfalbel/folhar)!
