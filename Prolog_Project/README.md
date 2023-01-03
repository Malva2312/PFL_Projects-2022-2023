# Turma 12 - 369_6

## Intervenientes
- João Antónito Maricato Malva - up202006605
- João Tomás Marques Félix - up202008867
- Contribuição 50/50

## Instalação e Execução

## Descrição do jogo

- O jogo 369 é um jogo de tabuleiro, jogado neste caso num tabuleiro 9x9, onde cada jogador no seu turno coloca uma pedra numa coordenada do tabuleiro. Havendo um total de 81 pedras para serem jogadas.
  - ex. (4,5); (6,6); (8,4).
- Se no momento do seu turno o jogador ao colocar a pedra conseguir ter:
  - 3 pedras em linha (ortogonal ou diagonal) ganha 1 ponto;
  - 6 pedras em linha (ortogonal ou diagonal) ganha 2 pontos;
  - 9 pedras em linha (ortogonal ou diagonal) ganha 3 pontos;
- Todos os jogadores jogam com pedras iguais pois não há distinção entre as mesmas. Os pontos são obtidos através de combinações no tabuleiro, não com as pedras que o jogador A jogou mas sim com todas as peças postas no tabuleiro até ao momento.
- O objetivo do jogo é sumar o máximo de pontos possíveis até o tabuleiro estar totalmente preenchido com pedras. Quando isto acontecer o jogo acaba e ganha o jogador com mais pontos.

## Lógica do jogo

### Representação interna do estado do jogo

O tabuleiro é representado a partir de uma lista com sublistas, sendo cada sublista uma linha do tabuleiro. Cada elemento duranto o jogo posiciona no tabuleiro a sua peça.
O player tem dois estados possíveis, Player 1 e Player 2.
Na representação gráfica do tabuleiro, as peças do Player 1 e do Player 2 não fazem distinção entre elas.

### Visualização do estado do jogo

Após iniciar o jogo, o jogador tem ao seu dispor um menu inicial com as opções do jogo.
Para realizar a escolha de uma opção o jogador apenas escreve o número relativo à opção que quer. 
As opções 'Rules' e 'Authors' apenas contêm texto sobre as suas secções.
As primeiras 3 opções depois de ter selecionado 'Play', são respetivamente jogar Player vs Player, Player vs CPU ou CPU vs CPU.

### Execução de jogadas

### Lista de jogadas válidas

### Final do jogo

O jogo termina quando todas as opções onde se poderia colocar uma pedra ficam preenchidas e depois verifica se qual é o jogador ou CPU com mais pontos e assim determinamos o vencedor.

### Avaliação do tabuleiro

### Jogada do computador

## Conclusões

## Bibliografia

- https://www.di.fc.ul.pt/~jpn/gv/369.htm
- https://boardgamegeek.com/boardgame/32597/369
