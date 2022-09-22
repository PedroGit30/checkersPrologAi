module(ui,[start/0]).

idexes_board(B) :- B = [
"   *             -----------------------------------------              *   ",
"   *             | 1  | 2  | 3  | 4  | 5  | 6  | 7  | 8  |              *   ",
"   *             -----------------------------------------              *   ",
"   *             | 9  | 10 | 11 | 12 | 13 | 14 | 15 | 16 |              *   ",
"   *             -----------------------------------------              *   ",
"   *             | 17 | 18 | 19 | 20 | 21 | 22 | 23 | 24 |              *   ",
"   *             -----------------------------------------              *   ",
"   *             | 25 | 26 | 27 | 28 | 29 | 30 | 31 | 32 |              *   ",
"   *             -----------------------------------------              *   ",
"   *             | 33 | 34 | 35 | 36 | 37 | 38 | 39 | 40 |              *   ",
"   *             -----------------------------------------              *   ",
"   *             | 41 | 42 | 43 | 44 | 45 | 46 | 47 | 48 |              *   ",
"   *             -----------------------------------------              *   ",
"   *             | 49 | 50 | 51 | 52 | 53 | 54 | 55 | 56 |              *   ",
"   *             -----------------------------------------              *   ",
"   *             | 57 | 58 | 59 | 60 | 61 | 62 | 63 | 64 |              *   ",
"   *             -----------------------------------------              *   "].

read_key([Code|Codes]) :-
    get_single_char(Code),
    read_pending_codes(user,Codes,[]).

read_keyatom(KAtom) :-
    read_key(Codes),
    write(Codes),
    codes_keyatom(Codes,KAtom).

codes_keyatom([27,91,65],up)    :- !.
codes_keyatom([27,91,66],down)  :- !.
codes_keyatom([27,91,67],right) :- !.
codes_keyatom([27,91,68],left)  :- !.
codes_keyatom([115],s)          :- !.
codes_keyatom([105],i)          :- !.
codes_keyatom([13],enter)       :- !.
codes_keyatom(_,ignore)         :- !.

% gets the value of the global var if given a var
% if given a value then its ignored
get_global_vars(Board,Moves,Player,MoveIndex,LastMove,ShowMove,Depth,N_Moves,ShowIndexes) :-
    !,( \+ var(Board)       ; b_getval(board,Board)               ),   % current board of the game
    !,( \+ var(Moves)       ; b_getval(moves,Moves)               ),   % list of the moves [ From, [([To|T1],Board) | T2] | T3 ]
    !,( \+ var(Player)      ; b_getval(player,Player)             ),   % current player
    !,( \+ var(MoveIndex)   ; b_getval(moveIndex,MoveIndex)       ),   % currently selected index of the moves
    !,( \+ var(LastMove)    ; b_getval(lastMove,LastMove)         ),   % string of the last move ex : 45 -> 27 -> 9
    !,( \+ var(ShowMove)    ; b_getval(showMove,ShowMove)         ),   % bool that indicates if the show move mode is active
    !,( \+ var(Depth)       ; b_getval(depth,Depth)               ),   % difficultity or depth of search
    !,( \+ var(N_Moves)     ; b_getval(n_moves,N_Moves)           ),   % number of moves
    !,( \+ var(ShowIndexes) ; b_getval(showIndexes,ShowIndexes)   ),   % bool that indicates if the show indexes mode is active
    !.

% sets the value of the global var if given a value,
% if given a var then its ignored
set_global_vars(Board,Moves,Player,MoveIndex,LastMove,ShowMove,Depth,N_Moves,ShowIndexes) :-
    !,( var(Board)       ; b_setval(board,Board)               ),
    !,( var(Moves)       ; b_setval(moves,Moves)               ),
    !,( var(Player)      ; b_setval(player,Player)             ),
    !,( var(MoveIndex)   ; b_setval(moveIndex,MoveIndex)       ),
    !,( var(LastMove)    ; b_setval(lastMove,LastMove)         ),
    !,( var(ShowMove)    ; b_setval(showMove,ShowMove)         ),
    !,( var(Depth)       ; b_setval(depth,Depth)               ),
    !,( var(N_Moves)     ; b_setval(n_moves,N_Moves)           ),
    !,( var(ShowIndexes) ; b_setval(showIndexes,ShowIndexes)   ),   % bool that indicates if the show indexes mode is active
    !.

% returns a string representing the startGame button selected
start_game(Str) :-
    get_global_vars(-,-,-,-,-,-,Depth,-,-),
    (Depth < 10 -> swritef(Depth2,'%w ', [Depth]) ; Depth >= 10 -> swritef(Depth2,'%w', [Depth])),
    swritef(S,'   *                          |      %w      |                          *   ', [Depth2]),
    Str = [
        "   **********************************************************************   ",
        "   *                                                                    *   ",
        "   *                           CHECKERS GAME                            *   ",
        "   *                                                                    *   ",
        "   *                          ****************                          *   ",
        "   *                ----->    |  START GAME  |                          *   ",
        "   *                          ****************                          *   ",
        "   *                                                                    *   ",
        "   *                      DIFFICULTY (SEARCH DEPTH)                     *   ",
        "   *                          ----------------                          *   ",
        S,
        "   *                          ----------------                          *   ",
        "   *                                                                    *   ",
        "   **********************************************************************   "
    ].

% returns a string representing the difficulty slider selected
difficulty(Str) :-
    get_global_vars(-,-,-,-,-,-,Depth,-,-),
    (Depth < 10 -> swritef(Depth2,'%w ', [Depth]) ; Depth >= 10 -> swritef(Depth2,'%w', [Depth])),
    swritef(S,'   *                ----->    |      %w      |                          *   ', [Depth2]),
    Str = [
        "   **********************************************************************   ",
        "   *                                                                    *   ",
        "   *                           CHECKERS GAME                            *   ",
        "   *                                                                    *   ",
        "   *                          ----------------                          *   ",
        "   *                          |  START GAME  |                          *   ",
        "   *                          ----------------                          *   ",
        "   *                                                                    *   ",
        "   *                      DIFFICULTY (SEARCH DEPTH)                     *   ",
        "   *                          ****************                          *   ",
        S,
        "   *                          ****************                          *   ",
        "   *                                                                    *   ",
        "   **********************************************************************   "
    ].

% returns a string representing the game state
game(Str) :-
    get_global_vars(Board,Moves,Player,MoveIndex,LastMove,ShowMove,-,-,ShowIndexes),
    game:playerPieces(Player,(SP,DP)),
    string_length(LastMove,L),
    Spaces is 26 - (L + 3),
    ui_strings:add_spaces(LastMove,Spaces,LastMove2),
    swritef(InfoStr,'   *         %w         |        %w,%w        |   %w*      i : show indexes on the board', [Player,SP,DP,LastMove2]),
    ui_strings:moves_string(Moves,MovesStrList),
    ui_strings:select_Move(MovesStrList,MoveIndex,MovesStrList2),
    ui_strings:string_unsplit(MovesStrList2,"\n",MovesStrList3),
    length(MovesStrList,N_Moves),
    set_global_vars(_,_,_,_,_,_,_,N_Moves,_),
    (ShowMove -> game:get_n_move(Moves,MoveIndex,Board2), ui_strings:board_string(Board2,BoardStr) ;
        (ShowIndexes -> idexes_board(BoardStr2),ui_strings:string_unsplit(BoardStr2,"\n",BoardStr) ; ui_strings:board_string(Board,BoardStr))),

    Str = [
        "   **********************************************************************   ",
        "   *       Player       |       Pieces       |        Last Move         *                  Controls",
        "   *--------------------------------------------------------------------*      s : turn on/off move visualization",
        InfoStr,
        "   *--------------------------------------------------------------------*      enter : chose move",
        "   *                                                                    *      up arrow : go up on moves",
        "   *                                                                    *      down arrow : go down on moves",
        "   *                                                                    *      If is p2 turn(AI) you can view is moves",
        "   *                                                                    *      and click enter for AI to chose one.",
        BoardStr,
        "   *                                                                    *   ",
        "   *                                                                    *   ",
        MovesStrList3,
        "   *                                                                    *   ",
        "   *--------------------------------------------------------------------*   "

].

% returns a string representing the game end
endGame(Str) :-
    get_global_vars(-,-,Player,-,-,-,-,-,-),
    swritef(InfoStr,'   *                              %w Won !!                             *   ', [Player]),

    Str = [
        "   **********************************************************************   ",
        "   *                                                                    *   ",
        InfoStr,
        "   *                                                                    *   ",
        "   *                                                                    *   ",
        "   *                    Click Enter to Play again                       *   ",
        "   *                                                                    *   ",
        "   *--------------------------------------------------------------------*   "

    ].

up(difficulty,start_game).
up(game,game) :- get_global_vars(-,-,-,MoveIndex,-,-,-,-,-),
                (MoveIndex > 0 -> MoveIndex2 is MoveIndex - 1 ; MoveIndex2 = 0),
                set_global_vars(_,_,_,MoveIndex2,_,_,_,_,_).
up(D,D).

down(start_game,difficulty).
down(game,game) :-  get_global_vars(-,-,-,MoveIndex,-,-,-,N_Moves,-),
                    (MoveIndex < N_Moves-1 -> MoveIndex2 is MoveIndex + 1 ; MoveIndex2 is N_Moves-1),
                    set_global_vars(_,_,_,MoveIndex2,_,_,_,_,_).
down(D,D).

left(difficulty,difficulty) :-  get_global_vars(-,-,-,-,-,-,Depth,-,-),
                                (Depth > 1 -> Depth2 is Depth - 1 ; Depth2 = 1),
                                set_global_vars(_,_,_,_,_,_,Depth2,_,_).

left(D,D).

right(difficulty,difficulty) :- get_global_vars(-,-,-,-,-,-,Depth,-,-),
                                (Depth < 15 -> Depth2 is Depth + 1 ; Depth2 = 15),
                                set_global_vars(_,_,_,_,_,_,Depth2,_,_).

right(D,D).

enter(start_game,game) :-   game:start_board(B),
                            game:get_all_moves(B,p1,M),
                            set_global_vars(B,M,p1,0,"",false,_,_,false).


enter(game,G) :- get_global_vars(-,Moves,Player,MoveIndex,-,-,Depth,-,-),
                game:nextPlayer(Player,Player2),
                (Player == p1 ->
                    % if p1
                    ui_strings:moves_string(Moves,MovesStrList),
                    nth0(MoveIndex,MovesStrList,LastMove),
                    game:get_n_move(Moves,MoveIndex,Board2),
                    game:get_all_moves(Board2,Player2,Moves2) ;
                    % if p2
                    ui_strings:moves_string(Moves,MovesStrList),
                    game:chose_best_move(Moves,Depth,Board2,Index),
                    nth0(Index,MovesStrList,LastMove),
                    game:get_all_moves(Board2,Player2,Moves2)
                ),
                set_global_vars(Board2,Moves2,Player2,0,LastMove,_,_,_,_),
                (Moves2 == [] -> set_global_vars(_,_,Player,_,_,_,_,_,_), G = endGame ; G = game).

enter(endGame,start_game).
enter(D,D).

s(game,game) :- get_global_vars(-,-,-,-,-,ShowMove,-,-,-),
                (ShowMove -> set_global_vars(_,_,_,_,_,false,_,_,_) ;
                    set_global_vars(_,_,_,_,_,true,_,_,_)).

i(game,game) :- get_global_vars(-,-,-,-,-,-,-,-,ShowIndexes),
                (ShowIndexes -> set_global_vars(_,_,_,_,_,_,_,_,false) ;
                    set_global_vars(_,_,_,_,_,_,_,_,true)).

ignore(D,D).

show2([]).
show2([H|T]) :- writeln(H), show2(T).

show(L) :- tty_clear,show2(L).

draw(Selected) :- call(Selected,Str1),
                show(Str1),
                read_keyatom(Key),
                call(Key,Selected,NewSelected),
                draw(NewSelected).

start() :-  set_global_vars(_,_,_,_,_,_,5,_,_),draw(start_game).
