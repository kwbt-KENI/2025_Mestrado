# Informações

Projeto baseado no repositório de [jefferds](<https://github.com/jefferds/abntexQuarto>). Editado na formatação padrões de trabalhos da Universidade Estadual de Londrina (UEL).

Trabalho de Dissertação de mestrado em ciências biológicas de Edson Kenji Kawabata (trabalho em andamento).

# abntexQuarto

Este projeto busca somar a class abntex2 (<https://www.abntex.net.br/>) e o sistema de publicação científica e técnica Quarto.org. (<https://quarto.org/>).

### Utilização

Para utilização é necessário a instalação do sistema Quarto no site oficial. (<https://quarto.org/>).

Após a instalação do sistema, é necessário a instalação do Miktex (<https://miktex.org/download>) ou semelhante, para utilização de LaTeX (e.g.: tinytex (quartotool)).
Obs. Em caso de utilização de LaTeX em versões antigas, há necessidade da instalação da biblioteca abntex2 (<https://www.abntex.net.br/>) para a geração do PDF.

O arquivo principal do projeto a ser renderizado (render PDF) é o `abntexQuarto.qmd`.

Para alterar dados como nome do autor, título, data e outras informações, basta editar o arquivo `_variables.yml`. Observa-se que, como o projeto inicial foi feito para realização de Trabalho de Conclusão de Curso, os termos utilizados são referentes a esse modelo de trabalho.


#### Observações

1. Por alguma razão o sistema Quarto algumas vezes não consegue sincronizar arquivos em pastas como as do Google Drive. Manter seu projeto em uma pasta local (ex: "C:\Users\SeuUsuario\Documents\") pode ajudar a evitar problemas de sincronização.
2. Algumas vezes o problema sincronização do Quarto pode ser resolvido deletando os arquivos temporários gerados pelo latex e a apagando a pasta `.quarto` (faça backup do seus arquivos antes de usar delete!).
3. Opção de freeze, de projetos quarto, está atualmente ativa, podendo causar, em algumas situações, lentidão.
4. A base para de dados deste projeto (pasta `data`), no atual momento não está contida neste repositório, devido ao espaço ocupado pelos arquivos lá contidos. Recomenda-se, para a execução dos arquivos deste repositório em diretório local, a criação da pasta `data`.
