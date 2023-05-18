#intalando biblioteca para ler aquivos JSON 
install.packages("jsonlite")
library(jsonlite) 
# arquivos json
#lendo o arquivo e transformando em um dataframe

df <- jsonlite::fromJSON("https://www.bcb.gov.br/api/servico/sitebcb/copomminutes/ultimas?quantidade=1000&filtro=")

View(df$conteudo)
#extraindo o conteudo do data frame
df <- data.frame(df$conteudo)

View(df)

# visualizando o link da celula 1 da coluna link
primeira_celula <- df$link[1]

# Exibe o valor da primeira célula
print(primeira_celula)


#pacotes necessários para instalar os pacotes abaixo
install.packages("pdftools")
library(pdftools)

install.packages("httr")
library(httr)

install.packages("dplyr")
library(dplyr)

install.packages("purrr")
library(purrr)


#Isolar apenas os números das atas a partir da coluna Titulo
#concatenar o endereço do banco central com coluna Url para baixar os PDFs
#extrair os textos dos PDFS
df <- df %>%
  mutate(
    ata = stringr::str_extract(string = Titulo, pattern = "^[[:digit:]]{2,3}") %>% as.numeric(),
    link = paste0("https://www.bcb.gov.br", Url) %>% URLencode(),
    texto = purrr::map_chr(
      .x = link,
      .f = ~paste0(pdftools::pdf_text(.x), collapse = "\n")
    )
  )





View(df)


# visualizando o link da celula 1 (ou qualquer) da coluna texto
primeira_celula <- df$texto[1]

# Exibe o texto da célula escolhida
print(primeira_celula)







#TRATAMENTO DOS DADOS, STOP WORS, MINUSCULAS, PONTUAÇÃO, ETC.
install.packages("dplyr")
library(dplyr)
install.packages("tm")
library(tm)
install.packages("stringr")
library(stringr)
install.packages("SnowballC")
library(SnowballC)

# Carregar lista de stopwords em inglês do pacote tm
data("stopwords")

# Definir lista de stopwords em inglês
stopwords_en <- stopwords("en")

# Função para pré-processamento do texto
preprocess_text <- function(text) {
  # Converter para minúsculas
  text <- tolower(text)
  
  # Remover números
  text <- str_remove_all(text, "[0-9]")
  
  # Remover sinais de pontuação e caracteres especiais
  text <- str_remove_all(text, "[[:punct:]]")
  text <- str_remove_all(text, "[^[:alnum:]\\s]")
  
  # Remover stopwords
  text <- str_split(text, "\\s+") %>%
    unlist() %>%
    setdiff(stopwords_en) %>%
    paste(collapse = " ")
  
  # Stemming
  text <- wordStem(text)
  
  return(text)
}

# Aplicar pré-processamento do texto
df$texto <- sapply(df$texto, preprocess_text)

#TOKENIZAR
# Instalar pacotes necessários
install.packages("tidytext")
library(tidytext)

# Criar novo data frame com as palavras tokenizadas
df_tokenized <- df %>%
  select(ata, texto) %>%
  unnest_tokens(tokenizado, texto)

# Visualizar o novo data frame
head(df_tokenized)


#DICIONÁRIO

install.packages("SnowballC")
library(SnowballC)
library(textdata)




# Carregar dicionário Loughran-McDonald
lexicon <- textdata::lexicon_loughran() # Digite 1 (yes) no console

# Criar novo dataframe com as palavras tokenizadas
df_tokenized <- df %>%
  unnest_tokens(token, texto)

# Remover palavras repetidas
df_tokenized <- df_tokenized %>%
  distinct()

# Stemização das palavras do dicionário
lexicon <- lexicon %>%
  mutate(stemmed_word = SnowballC::wordStem(word))

# Cruzar os dados do dicionário com a coluna "tokenizado" do dataframe "df_tokenized"
merged_df <- merge(df_tokenized, lexicon, by.x = "token", by.y = "stemmed_word", all.x = TRUE)

# Filtrar apenas as palavras com sentimentos não nulos (negativo ou positivo)
merged_df <- merged_df %>%
  filter(!is.na(sentiment))

# Contar o número de sentimentos positivos e negativos pela coluna "ata"
sentiment_counts <- merged_df %>%
  group_by(ata, sentiment) %>%
  summarise(count = n())

# Criar a nova variável "sentiment_ata" baseada na contagem de sentimentos
sentiment_ata <- sentiment_counts %>%
  group_by(ata) %>%
  summarise(sentiment_ata = ifelse(sum(count[sentiment == "positive"]) > sum(count[sentiment == "negative"]), "positive", "negative"))

# Unir a nova variável "sentiment_ata" com o dataframe final
final_df <- merged_df %>%
  left_join(sentiment_ata, by = "ata")

# Selecionar as colunas desejadas
final_df <- final_df %>%
  select(ata, sentiment, word, token, sentiment_ata)

# Visualizar o resultado
head(final_df)




# Remover linhas com NA na coluna "ata"
sentiment_ata <- sentiment_ata %>%
  na.omit()

# Visualizar o resultado
head(sentiment_ata)


# Criar variável dummy
sentiment_ata$sentiment_dummy <- ifelse(sentiment_ata$sentiment_ata == "positive", 1, 0)



#GERAR UM GRÁFICO

library(ggplot2)

# Filtrar as linhas com sentimentos positivos e negativos
positive_counts <- sentiment_counts[sentiment_counts$sentiment == "positive", ]
negative_counts <- sentiment_counts[sentiment_counts$sentiment == "negative", ]

# Combinação dos dados
combined_counts <- merge(positive_counts, negative_counts, by = "ata", all = TRUE)
combined_counts <- combined_counts[order(combined_counts$ata), ]

# Calcular a diferença entre positivos e negativos
combined_counts$sentiment_diff <- combined_counts$count.x - combined_counts$count.y

# Gerar o gráfico
ggplot(combined_counts, aes(x = ata, y = sentiment_diff, fill = sentiment_diff >= 0)) +
  geom_bar(stat = "identity", width = 0.7) +
  labs(x = "Ata", y = "Diferença (Positive - Negative)", fill = "Sentimento") +
  scale_fill_manual(values = c("red", "green"), labels = c("Negative", "Positive")) +
  theme_minimal()




#SALVANDO A BASE DE DADOS PARA BACKUP

# Salvar o data frame em um arquivo CSV
write.csv(df, "C:\Users\jeffe\atascopom.csv", row.names = FALSE)




#limpar o environment
rm(list = ls())

#limpar console
cat("\014")




