
% rules used :
%     GAME :
%     * 8 * 8 board.
%     * Each player begins the game with 12 discs and places them on the 12 dark squares closest to him or her.
%     * Black opens the game, then players alternate their turns.
%     * Randomly determine who gets the black pieces first. When playing a series of games,
%       the players alternate who gets the black pieces.
%     MOVE :
%     * The pieces always move diagonally and single pieces are always limited to forward moves.
%     * A piece making a non-capturing move may move only one square.
%     CAPTURE :
%     * To capture a piece of your opponent, your piece leaps over one of the opponent's pieces and
%       lands in a straight diagonal line on the other side. This landing square must be empty.
%     * When a piece is captured, it is removed from the board.
%     * Only one piece may be captured in a single jump, but multiple jumps are allowed on a single turn.
%     * If a player is able to make the capture, then the jump must be made.
%     * Single pieces may shift direction diagonally during a multiple capture turn, but must always
%       jump forward (toward the opponent).
%     JING OR QUEEN :
%     * When a piece reaches the furthest row, it is crowned and becomes a king.
%     * One of the pieces which had been captured is placed on top of the king so that
%       it is twice as high as a single piece.
%     * Kings are limited to moving diagonally but can move both forward and backward.
%     * Kings may combine jumps in several directions (forward and backward) on the same turn.
%     END GAME :
%     * A player wins the game when the opponent cannot make a move.

% # - player1 pieces
% o - player2 pieces
% ## - player1 queen
% oo - player2 queen

%************************************************* GAME LOGIC ********************************************************

module(game,[
    start_board/1,      % start_board(+Board)
    playerPieces/2,     % playerPieces(-Player,+(SinglePiece,Queen))
    nextPlayer/2,       % nextPlayer(-Player,+NextPlayer)
    get_all_moves/3,    % get_all_moves(-Board,-Player,+ListOfMoves)
    get_n_move/3,       % get_n_move(-Moves,-N,+N_Board)
    chose_best_move/4   % chose_best_move(-Moves,-Depth,+Board,+IndexOfBoard)
]).

start_board(X) :- X = [
    -,o,-,o,-,o,-,o,
    o,-,o,-,o,-,o,-,
    -,o,-,o,-,o,-,o,
    -,-,-,-,-,-,-,-,
    -,-,-,-,-,-,-,-,
    #,-,#,-,#,-,#,-,
    -,#,-,#,-,#,-,#,
    #,-,#,-,#,-,#,-].

test_board(X) :- X = [
    -,-,-,-,-,-,-,-,
    -,-,-,-,o,-,-,-,
    -,-,-,-,-,#,-,-,
    -,-,o,-,-,-,-,-,
    -,-,-,-,-,-,-,-,
    -,-,-,-,-,-,-,-,
    -,-,-,-,-,-,-,-,
    -,-,-,-,-,-,-,-].


% limits(-Index)
limits(I) :- I > 0, I =< 64.

% toIndex(-X,-Y,+Index)
toIndex(X,Y,I) :- I is 8*(X-1)+Y.
toXY(I,X,Y) :- limits(I),
               X2 is div(I,8) + 1,
               Y2 is mod(I,8),
               (Y2 == 0 -> X is X2 - 1, Y = 8 ; X = X2, Y = Y2).

% update_index(-Board,-Index,-Value,+Board,+Replaced)
update_index([H|T],1,V,[V|T],H) :- !.
update_index([H|T],I,V,[H|OUT],V2) :- I2 is I - 1,
                                      update_index(T,I2,V,OUT,V2).

% get_board_cell(-Board,-Index,+Piece)
get_board_cell(B,I,Piece) :- update_index(B,I,Piece,_,Piece).

% move left
move(#,I1,I2) :- toXY(I1,_,Y), Y >= 2, I2 is I1 - 9, limits(I1), limits(I2).
move(o,I1,I2) :- toXY(I1,_,Y), Y >= 2, I2 is I1 + 7, limits(I1), limits(I2).
% move rigth
move(#,I1,I2) :- toXY(I1,_,Y), Y =< 7, I2 is I1 - 7, limits(I1), limits(I2).
move(o,I1,I2) :- toXY(I1,_,Y), Y =< 7, I2 is I1 + 9, limits(I1), limits(I2).

move(##,I1,I2) :- move(#,I1,I2) ; move(o,I1,I2).
move(oo,I1,I2) :- move(#,I1,I2) ; move(o,I1,I2).

% jump left
jump(#,I1,I2,I3) :- toXY(I1,_,Y), Y >= 3, I2 is I1 - 9, I3 is I2 - 9, limits(I1), limits(I2), limits(I3).
jump(o,I1,I2,I3) :- toXY(I1,_,Y), Y >= 3, I2 is I1 + 7, I3 is I2 + 7, limits(I1), limits(I2), limits(I3).
% jump rigth
jump(#,I1,I2,I3) :- toXY(I1,_,Y), Y =< 6, I2 is I1 - 7, I3 is I2 - 7, limits(I1), limits(I2), limits(I3).
jump(o,I1,I2,I3) :- toXY(I1,_,Y), Y =< 6, I2 is I1 + 9, I3 is I2 + 9, limits(I1), limits(I2), limits(I3).

jump(##,I1,I2,I3) :- jump(#,I1,I2,I3) ; jump(o,I1,I2,I3).
jump(oo,I1,I2,I3) :- jump(#,I1,I2,I3) ; jump(o,I1,I2,I3).

% opponentPiece(-MyPiece,+OpponentPiece)
opponentPiece(##,P) :- P = o ; P = oo .
opponentPiece(#,P)  :- P = o ; P = oo .
opponentPiece(oo,P) :- P = # ; P = ## .
opponentPiece(o,P)  :- P = # ; P = ## .

% make_queen(-Piece,-Index,+Queen)
make_queen(#,I,##) :- toXY(I,1,_).
make_queen(o,I,oo) :- toXY(I,8,_).

% capture_move(-Board,-Index,-Piece)
capture_move(B,I,P) :- jump(P,I,I2,I3),
                       get_board_cell(B,I,P),
                       get_board_cell(B,I3,-),
                       opponentPiece(P,OP),
                       get_board_cell(B,I2,OP).

% capture_move(-Board,-Index,-Piece,-Counter,+[Move],+Board)
capture_move(B,I,P,C1,[I3|M],FB) :-
    jump(P,I,I2,I3),                         % get all jumps,I2 is the index of the captured piece an I3 is the index of the landing
    update_index(B,I,-,B2,P),                % if the original index has my piece then remove it
    (make_queen(P,I3,P2) -> true ; P2 = P),  % change the piece to queen if final line
    update_index(B2,I3,P2,B3,-),             % if the landing index is empty then put my piece there
    opponentPiece(P,OP),                     % get oponent possible pieces
    update_index(B3,I2,-,B4,OP),             % if the captured piece index as the oponent piece then remove it
    C2 is C1 + 1,
    capture_move(B4,I3,P,C2,M,FB).

capture_move(FB,I,P,C1,[],FB) :- C1 > 0,
                                 \+ capture_move(FB,I,P).

% regular_move(-Board,-Index,-Piece,+Move,+Board)
regular_move(B,I,P,I2,B3) :- update_index(B,I,-,B2,P),               % if the original index has my piece then remove it
                             move(P,I,I2),                           % get all move indexes I2
                             (make_queen(P,I2,P2) -> true ; P2 = P), % change the piece to queen if final line
                             update_index(B2,I2,P2,B3,-).            % if index I2 is empty then put my piece there

% move(-Board,-Index,-Pieces,[Move]
% [Move] = jump : [([39,53], Board),..].
%          move : [(39, Board),..].
move(B,I,P,M) :- findall((X,Y),capture_move(B,I,P,0,X,Y),M1),
                 (M1 == [] -> findall((X,Y),regular_move(B,I,P,X,Y),M) ; M = M1).

%regular_move2(-Board,-Index,-Piece,+Board)
get_all_moves2(_,0,_,[]) :- !.
get_all_moves2(B,I,P,[(I,M)|MR]) :- move(B,I,P,M),
                                    M \= [],
                                    I2 is I - 1,
                                    !,get_all_moves2(B,I2,P,MR).

get_all_moves2(B,I,P,M) :- !,I2 is I - 1,get_all_moves2(B,I2,P,M).

% filter_capture(-Moves,+CaptureMoves)
filter_capture([],[]).
filter_capture([(From,[([To|T1],Board)|T2]) | T3],[(From,[([To|T1],Board)|T2]) | T4]) :- !,filter_capture(T3,T4).
filter_capture([_|T],F) :- !, filter_capture(T,F).

% filter_regular(-Moves,+RegularMoves)
filter_regular([],[]).
filter_regular([(From,[(To,Board)|T2]) | T3],[(From,[(To,Board)|T2]) | T4]) :- number(To),!,filter_regular(T3,T4).
filter_regular([_|T],F) :- !, filter_regular(T,F).

filter_board2([],[]).
filter_board2([(_,Board)|T],[Board|R]) :- filter_board2(T,R).

filter_board([],R,R).
filter_board([(_,L)|T],LS,O) :- filter_board2(L,B),
                                append(LS,B,LB),
                                filter_board(T,LB,O).

% filter_board(-Moves,+ListOfBoards)
filter_board(M,B) :- filter_board(M,[],B).

% get_all_moves(-Board,-Player,+ListOfMoves)
get_all_moves(B,P,M) :-  playerPieces(P,(P1,P2)),
                         get_all_moves2(B,64,P1,M1),
                         get_all_moves2(B,64,P2,M2),
                         append(M1,M2,M3),
                         filter_capture(M3,CM),
                         filter_regular(M3,RM),
                         (CM == [] -> M = RM ; M = CM).


get_n_move2([],N,N,_) :- !.
get_n_move2([(_,B)|_],0,0,B) :- !.
get_n_move2([_|T],N,FN,FB) :- N2 is N - 1,
                              get_n_move2(T,N2,FN,FB).

get_n_move(_,0,B,B) :- \+ var(B) ,!.
get_n_move([(_,L) | T],N,_,BF) :- get_n_move2(L,N,N2,B2),
                                  get_n_move(T,N2,B2,BF).

% get_n_move(-Moves,-N,+N_Board)
get_n_move(M,N,B) :- get_n_move(M,N,_,B).

% playerPieces(-Player,+(SinglePiece,Queen))
playerPieces(p1,(#,##)).
playerPieces(p2,(o,oo)).

% nextPlayer(-Player,+NextPlayer)
nextPlayer(p1,p2).
nextPlayer(p2,p1).

% sum_pieces(-Board,-SinglePiece,-DoublePiece,-StartValue,+TotalSum)
sum_pieces([],_,_,V,I,V).
sum_pieces([SP|T],SP,DP,V,I,V3) :- V2 is V + 1, I2 is I + 1, !, sum_pieces(T,SP,DP,V2,I2,V3).
sum_pieces([DP|T],SP,DP,V,I,V3) :- V2 is V + 5, I2 is I + 1, !, sum_pieces(T,SP,DP,V2,I2,V3).
sum_pieces([_|T],SP,DP,V,I,V3)  :- !,sum_pieces(T,SP,DP,V,I,V3).

% Player 2 is the maximizer
% High values should benifit player 2
% Low values should benifit player 1
% heuristic(-Board,+Value)
heuristic(B,V) :- playerPieces(p1,(SP1,DP1)),
                  playerPieces(p2,(SP2,DP2)),
                  sum_pieces(B,SP1,DP1,0,1,V1),
                  sum_pieces(B,SP2,DP2,0,1,V2),
                  V is V2 - V1.

% child(-ListBoards,-Depth,-Alpha,-Beta,-Value,-Player,-min_or_max,+Value)
child([],_,_,_,V,_,_,V).
child([Board|T],D,A,B,V,P,max,FV) :-
    D2 is D - 1,
    alphabeta(Board,D2,A,B,P,V2),
    V3 is max(V,V2),
    ( V3 >= B -> FV = V3 ; (A2 is max(A,V3), child(T,D,A2,B,V3,P,min,FV)) ).

child([Board|T],D,A,B,V,P,min,FV) :-
    D2 is D - 1,
    alphabeta(Board,D2,A,B,P,V2),
    V3 is min(V,V2),
    ( V3 =< A -> FV = V3 ; (B2 is min(B,V3), child(T,D,A,B2,V3,P,max,FV)) ).

% base cases return heuristic of the node
% desired depth reached
alphabeta(Board,0,_,_,_,V) :- !,heuristic(Board,V).

% alphabeta(-Board,-Depth,-Alpha,-Beta,-Player,+Value)
alphabeta(Board,Depth,A,B,Player,V2) :-
    nextPlayer(Player,NextToPlay),
    get_all_moves(Board,NextToPlay,Moves),
    (Moves == [] -> heuristic(Board,V2) ;
    filter_board(Moves,BS),
    (Player == p2 ->  % maximizing player is P2
            child(BS,Depth,A,B,-999999999,NextToPlay,max,V2) ;
            child(BS,Depth,A,B,999999999,NextToPlay,min,V2))).

% alphabeta(-Board,-Depth,+Value)
alphabeta(B,D,V) :- alphabeta(B,D,-999999999,999999999,p2,V).

% get_alphabeta_values(-ListOfBoards,-Acumulator,-IndexofBoard,-Depth,+[(Board,Value,Index), ...],+MaxValue)
get_alphabeta_values([],MAX,_,_,[],MAX).
get_alphabeta_values([B|BS],ACC,I,D,[(B,V,I)|VS],MAX) :-
    alphabeta(B,D,V),
    ACC2 is max(V,ACC),
    I2 is I + 1,
    get_alphabeta_values(BS,ACC2,I2,D,VS,MAX).

% best_boards(-[(Board,Value,Index),..], -MaxValue, +[(Board,Index),..])
best_boards([],_,[]) :- !.
best_boards([(B,M,I)|T],M,[(B,I)|T2]) :- !,best_boards(T,M,T2).
best_boards([_|T],M,T2) :- !,best_boards(T,M,T2).

% chose_best_move(-Moves,-Depth,+Board,+IndexOfBoard)
chose_best_move(M,Depth,FB,Index) :- filter_board(M,BS),
                                    get_alphabeta_values(BS,-999999999,0,Depth,BS2,Max),
                                    best_boards(BS2,Max,BestBoards),
                                    random_member((FB,Index),BestBoards).
