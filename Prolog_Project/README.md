# Turma 12 - 369_6

## Intervenientes
- João Antónito Maricato Malva  V- up202006605
- João Tomás Marques Félix      - up202008867
### Contribuição
  Ambos os membros contribuíram de forma igual no trabalho ( 50/50 ).

## Instalação e Execução
Não é necessário nenhum passo extra para a correta execução do jogo 369 em ambientes Linux e Windows (para além da instalação do SICStus Prolog 4.7.1).
Apenas foi incluiída a biblioteca `"library(lists): List Manipulation"`:
  ```prolog
  :- use_module(library(lists)).
  ```
Para executar o jogo é necessário compilar o ficheiro `main.pl` e executar `play.`


## Descrição do jogo

- O jogo 369 é um jogo de tabuleiro, jogado neste caso num tabuleiro 9x9 (ou 6x6), onde cada jogador no seu turno coloca uma pedra numa coordenada do tabuleiro. Havendo um total de 81 pedras para serem jogadas.
  - ex. (4,5); (6,6); (8,4).
- Se no momento do seu turno o jogador ao colocar a pedra conseguir ter:
  - 3 pedras em linha (ortogonal ou diagonal) ganha 1 ponto;
  - 6 pedras em linha (ortogonal ou diagonal) ganha 2 pontos;
  - 9 pedras em linha (ortogonal ou diagonal) ganha 3 pontos;
- Todos os jogadores jogam com pedras iguais pois não há distinção entre as mesmas. Os pontos são obtidos através de combinações no tabuleiro, não com as pedras que o jogador A jogou mas sim com todas as peças postas no tabuleiro até ao momento.
- O objetivo do jogo é sumar o máximo de pontos possíveis até o tabuleiro estar totalmente preenchido com pedras. Quando isto acontecer o jogo acaba e ganha o jogador com mais pontos.

## Lógica do jogo

### Representação interna do estado do jogo

- O tabuleiro é representado a partir de uma matriz (lista de listas), cujo cada cordenada corresponde a um espaço vazio ou a uma peça. Cada elemento duranto o jogo posiciona no tabuleiro a sua peça. Não existe qualquer distinção entre peças colocadas por cada jogador.

- São guardados também os jogadores como `player(P , Pontos)`, em que `P` é o indicativo do jogador e `Pontos`, os seus respetivos pontos. Em execução apenas dois valores em `player` são utilizados, um para cada jogador.
O "jogador atual" é encontraado com o seu `P` em `who_turn`.

- No inicio do jogo o estado incial é carregado e são guardados os jogadores, o tabuleiro, o tamanho do tabuleiro, o jogador que joga em primeiro e a dificuldade do CPU.
Estes são os valores que podem ser alterados na execução do jogo e este termina quando o tabuleiro não tiver lugar para mais peças.


### Visualização do estado do jogo

Após iniciar o jogo, o jogador tem ao seu dispor um menu inicial com as opções do jogo.
Para realizar a escolha de uma opção o jogador apenas escreve o número relativo à opção que quer. 
As opções 'Rules' e 'Authors' apenas contêm texto sobre as suas secções.
```
    3 - 6 - 9 
  1 -> Play
  2 -> Settings
  3 -> Rules
  0 -> Exit
  Choose Option
  |: 
```

As primeiras 4 opções depois de ter selecionado 'Play', são respetivamente jogar Player vs Player, Player vs CPU, CPU vs CPU ou CPU vs Player.
No jogo, os jogadores poderam interagir com tabuleiro, indicado as cordenadas onde querem colocar a sua peça. (Coluna, Linha)
O tablueiro é dado a conhcer neste formato:
```
  |  1  2  3  4  5  6  7  8  9  
--+-----------------------------
1 |  X  _  X  _  _  _  _  X  _  
2 |  X  X  _  _  _  X  _  X  _  
3 |  _  _  _  _  _  _  _  _  _  
4 |  X  X  _  _  X  _  _  X  _  
5 |  X  _  _  X  X  _  _  _  _  
6 |  X  _  _  _  _  _  _  _  X  
7 |  _  _  _  _  X  X  _  X  _  
8 |  X  X  X  X  _  _  _  X  _  
9 |  _  _  X  _  X  _  X  _  _ 
```


### Execução de jogadas

  No caso de ser um jogador é lhe pedido um input para colocar a sua peça no tabuleiro. Este input é validado, pois só pode ser colocada uma peça num local livre e se possível é colocada a peça, caso contrário é requesitado um novo input.
  ```
        Player 1
        Chose Colomn
|: 1.
        Chose Row
|: 1.

Colomn : 1      Row : 1
```
  Tendo os dados onde colocar a peça, esta é adicionada ao tabuleiro
  substituindo o valor na matriz pelo de uma peça e removendo a coordenada das jogadas possiveis e são feitos ajustes na pontuação de quem jogou (quando aplicável), terminando com o *display* do estado de jogo atualizado.

  No caso de ser um jogador controlado pelo computrado a adição da peça ao tabuleiro é semelhante, sendo a única diferença a forma como é elegida a jogada.

### Lista de jogadas válidas
  As jogadas válidas são carregadas, no início de jogo.
  Correspondem às coordenadas do tabuleiro (X, Y), onde ainda não existe uma peça.

### Final do Jogo
  O jogo termina quando, o tabuleiro se encontrar cheio, isto é, não existirem mais jogadas válidas.
  O jogo termina quando o tabuleiro se encontrar cheio isto é, não existirem mais jogadas válidas (nehuma a opção onde se poderia colocar uma pedra) e depois verifica-se qual é o jogador ou CPU com mais pontos, determinando o vencedor.
  É mostrada uma mensagem anunciando o vencedor e retorna-se ao menu inicial.

### Avaliação 
  A pontuação está sempre presente durante a duração do jogo e é atulizada ao fim de cada turno.
  Está presente em `player(Player, Pontuação)` para ser de fácil acesso.

### Jogada do computador
  No caso de um jogador controlado pelo computador o mecânismo de eleição da jogada não exige um input e depende da definção da inteligência com que está a opção 'CPU':
    - Random:
      É escolhida uma posição aleatória de todas as `valid_move` possiveis e é colocada a peça,
    - Greedy:
      É calculada para todas as `valid_move` a pontuação correspondente a essa movimento e depois é escolhida a jogada que proporcionará mais pontos, em caso de empate, a escolha é aleatória entre as jogadas que pontuarem mais.
  
  De forma semelhante a um jogador normal, é adiconada a peça ao tabuleiro, seguindo o mesmo protocolo e termina com ajustes na pontuação se aplicável.

## Conclusões

  Esta implementação do jogo 369 em Prolog apresenta aspetos positivos, como uma boa estruturação de código e o facto da "inteligência artificial" realizar as ações como esperado.
  No entanto, surgiram algumas dificuldades, como a dúvida na utilização de alguns *cuts* e na forma lenta e pouco intituitiva que é feita para input de dados pelo jogador.
  Este trabalho serviu principalmente para conhecer e explorar alguns novos conceitos sobre a linguagem em utilização, Prolog.

## Bibliografia

- https://www.di.fc.ul.pt/~jpn/gv/369.htm
- https://boardgamegeek.com/boardgame/32597/369
- https://www.swi-prolog.org/
