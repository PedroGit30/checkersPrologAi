
module(ui_strings,[
    board_string/2,       % board_string(-Board,+BoardString)       
    moves_string/2,       % moves_string(-Moves,+MovesString)
    string_unsplit/3,     % string_unsplit(-ListOfStrings,-Separator,+String)
    add_spaces/3,         % add_spaces(-String,-N_Spaces,+StringWithSpaces)
    select_move/3         % select_move(-MovesStrList,-MoveIndex,+MovesStrListSelected)
]).


cell_str(P,S3) :- (P == - -> S3 = "|    ";
                        S1 = "| ", string_concat(S1,P,S2), atom_length(P,L), 
                            (L == 2 -> string_concat(S2,' ',S3) ; L == 1 -> string_concat(S2,'  ',S3))).

board_string2([P],_,S,S3) :-  cell_str(P,S1),
                       string_concat(S,S1,S2),
                       string_concat(S2,"|              *   \n   *             -----------------------------------------              *   ",S3).                                     

board_string2([P|T],N,S,FS) :- 0 is N mod 8,
                        cell_str(P,S1),     
                        string_concat(S,S1,S2),
                        string_concat(S2,"|              *   \n   *             -----------------------------------------              *   \n   *             ",S3),   
                        !,N2 is N + 1,
                        board_string2(T,N2,S3,FS).

board_string2([P|T],N,S,FS) :- cell_str(P,S1), string_concat(S,S1,S2), N2 is N + 1, !, board_string2(T,N2,S2,FS).

board_string(BOARD,FS) :- S = "   *             -----------------------------------------              *   \n   *             ",
                       board_string2(BOARD,1,S,FS). 

moves_string3(H,Str,FS) :- number(H), string_concat(Str,H,FS).
moves_string3([H],Str,FS) :- !, string_concat(Str,H,FS).
moves_string3([H|T],Str,FS) :-   string_concat(Str,H,Str1),                            
                                string_concat(Str1," -> ",Str2),                            
                                moves_string3(T,Str2,FS).


moves_string2(_,[],[]) :- !.
moves_string2(F,[(To,_)|T1],[S2|R]) :- string_concat(F," -> ",S1),                                                                      
                                    moves_string3(To,S1,S2),                                    
                                    moves_string2(F,T1,R).

moves_string([],FL,FL).
moves_string([(From,L) | T2],LM,FL) :-  moves_string2(From,L,LMove),
                                    append(LM,LMove,LM2),
                                    moves_string(T2,LM2,FL).

moves_string(M,LStr) :- moves_string(M,[],LStr).

string_unsplit([H],S,_,S2) :- string_concat(S,H,S2).
string_unsplit([H|T],S,Sep,R) :- string_concat(S,H,S2),
                                string_concat(S2,Sep,S3),
                                string_unsplit(T,S3,Sep,R).

string_unsplit(ListOfStrings,Seperator,Str) :- string_unsplit(ListOfStrings,"",Seperator,Str).

add_spaces(S,0,S).
add_spaces(S,I,R) :- string_concat(S," ",S2),
                     I2 is I - 1,
                     add_spaces(S2,I2,R).

select_Move([],_,[]).
select_Move([H|T],0,[S3|R]) :- !,
                            swritef(S,'   *                          %w  <-- ', [H]),                               
                            string_length(S,L),
                            ToAdd is 72 - L,
                            add_spaces(S,ToAdd,S2),  
                            string_concat(S2,"*   ",S3),
                            select_Move(T,-1,R).


select_Move([H|T],I,[S3|R]) :- string_concat("   *                          ",H,S),                            
                            string_length(S,L),
                            ToAdd is 72 - L,
                            add_spaces(S,ToAdd,S2),
                            string_concat(S2,"*   ",S3),
                            I2 is I - 1,
                            select_Move(T,I2,R).
