### 
###  Script em R criado para apoio a tese de doutorado de 
###  Jefferson Ferreira Barbosa, aluno do CIn-UFPE. 
###  Email: jfb2@cin.ufpe.br
###  Orientador: Hermano Perrelli de Moura -hermano@cin.ufpe.br
###  Coorientador: Marcelo Luiz Monteiro Marinho - mlmm@cin.ufpe.br

# Pacotes utilizados 
# BiocManager::install(c("gRbase", "RBGL", "Rgraphviz", "gRain"))

# install.packages("bnlearn")
# https://www.bnlearn.com/

# https://www.bioconductor.org/packages/release/bioc/html/Rgraphviz.html
# https://www.rdocumentation.org/packages/Rgraphviz/versions/2.16.0

# install.packages("BiocManager")

# install.packages("BiocManager")
# BiocManager::install("Rgraphviz")

if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install(c("graph", "RBGL", "Rgraphviz"))

library(bnlearn)
library(gRain)
library(Rgraphviz)

# Definição dos nós da rede bayesiana
euler <- empty.graph(nodes = c("U1",  "U2", "U3",
                               "U4",  "U5", "U6",
                               "U7",  "U8", "U9", 
                               "U10", "U11", "U12",
                               "U13", "U14"))

# Verificaçãodo tipo de class do objeto
class(euler)

# Configuração das ligações / arcos entre os nós da rede bayesiana
arc.set <- matrix(c("U1", "U3",
                    "U1", "U14",
                    "U2", "U3",
                    "U2", "U6",
                    "U2", "U8",
                    "U2", "U5",
                    "U2", "U13",
                    "U3", "U4",
                    "U3", "U7",
                    "U3", "U11",
                    "U3", "U10",
                    "U6", "U9",
                    "U6", "U12",
                    "U7", "U14"),
                  byrow = TRUE, ncol = 2,
                  dimnames = list(NULL, c("from", "to")))
arcs(euler) <- arc.set

arc.set

# Verificação do modelo e as interligações entre os arcos
modelstring(euler)

# Carga do survey realizado com a equipe do projeto. 
euler_survey <- read.table("euler_survey.txt", 
                          colClasses = "factor",
                          header = TRUE)

# Apresentação dos primeiros registros do survey
head(euler_survey)


# Formatação das saídas numéricas para3 digitos.
options(digits = 3)

# Criação da rede bayesiana
euler.bayes <- bn.fit(euler, data = euler_survey, method = "bayes", iss = 10, debug=TRUE)

# Criação de uma tabela de distribuição de probabilidades
junction <- compile(as.grain(euler.bayes))

# Consulta da probabilidade de ocorrência de uma incerteza específica.
querygrain(junction, nodes = "U1")$U1

# Consulta da probabilidade de ocorrência de uma incerteza específica.
querygrain(junction, nodes = "U2")$U2

# Consulta da probabilidade de ocorrência de uma incerteza específica.
querygrain(junction, nodes = "U3")$U3

# Apresentação visual da rede bayesiana. 
graphviz.plot(euler,
              shape = "ellipse",
              # highlight = list(nodes = "U3", col = "tomato", fill = "orange")
              )