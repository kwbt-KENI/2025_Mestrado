# Setar configurações do renderizador pdf
## normalmente o projeto já irá rodar com gerenciador de pacotes do MikTex e instalação Latex nativa, mas por garantia

## definir path
### a quem interessar: configure aqui o cominho do MikTex
Sys.setenv(
  PATH = paste(
    "C:/Users/kawab/AppData/Local/Programs/MiKTeX/miktex/bin/x64",
    Sys.getenv("PATH"),
    sep = ";"
  )
)

## bloquear tinytex
options(tinytex.compile.min_times = 1)
