% Copyright

implement main
    open core,console
    class facts
    результат:(integer,integer,integer,integer,integer).
    родитель:(integer,integer,integer,integer,integer).
    ребенок:(integer,integer,integer,integer,integer).

class predicates
удалитьродителей:(integer) nondeterm.
заменитьродителей:(integer[out],integer[out],integer[out],integer[out],integer) nondeterm.
удалитьдетей:(integer,integer,integer,integer,integer) nondeterm.
мутация:(integer,integer,integer,integer,integer,integer[out],integer[out],integer[out],integer[out]).
кроссовер:(integer,integer,integer,integer,integer,integer,integer,integer,integer,integer[out],integer[out],integer[out],integer[out]).
создатьребенка:(integer,integer,integer,integer,integer,integer,integer,integer,integer,integer,integer[out],integer[out],integer[out],integer[out],integer[out]) nondeterm.
ср:(integer) nondeterm.
генетическийшаг:(integer) nondeterm.
создатьреб:(integer,integer,integer,integer,integer,integer,integer,integer,integer).
сменапоколения:(integer)nondeterm.

clauses
результат(1,2,3,4,30).
родитель(13,10,2,7,0).
родитель(13,9,5,7,1).
родитель(11,5,12,7,2).
родитель(1,3,2,1,3).

создатьребенка(4,N0,N1,N2,N3,A,A1,A2,A3,A4,A,A1,A2,A3,A4):-!.

создатьребенка(0,N0,N1,N2,N3,A,A1,A2,A3,A4,R,R1,R2,R3,R4):-результат(S1,S2,S3,S4,S5),родитель(V1,V2,V3,V4,I), NN=math::abs(S5-S1*V1-S2*V2-S3*V3-S4*V4)*N0,A>NN,создатьребенка(1,N0,N1,N2,N3,NN,V1,V2,V3,V4,R,R1,R2,R3,R4).
создатьребенка(0,N0,N1,N2,N3,A,A1,A2,A3,A4,R,R1,R2,R3,R4):-результат(S1,S2,S3,S4,S5),родитель(V1,V2,V3,V4,I), NN=math::abs(S5-S1*V1-S2*V2-S3*V3-S4*V4)*N0,A<NN,создатьребенка(1,N0,N1,N2,N3,A,A1,A2,A3,A4,R,R1,R2,R3,R4).
создатьребенка(1,N0,N1,N2,N3,A,A1,A2,A3,A4,R,R1,R2,R3,R4):-результат(S1,S2,S3,S4,S5),родитель(V1,V2,V3,V4,I), NN=math::abs(S5-S1*V1-S2*V2-S3*V3-S4*V4)*N1,A>NN,создатьребенка(2,N0,N1,N2,N3,NN,V1,V2,V3,V4,R,R1,R2,R3,R4).
создатьребенка(1,N0,N1,N2,N3,A,A1,A2,A3,A4,R,R1,R2,R3,R4):-результат(S1,S2,S3,S4,S5),родитель(V1,V2,V3,V4,I), NN=math::abs(S5-S1*V1-S2*V2-S3*V3-S4*V4)*N1,A<NN,создатьребенка(2,N0,N1,N2,N3,A,A1,A2,A3,A4,R,R1,R2,R3,R4).
создатьребенка(2,N0,N1,N2,N3,A,A1,A2,A3,A4,R,R1,R2,R3,R4):-результат(S1,S2,S3,S4,S5),родитель(V1,V2,V3,V4,I), NN=math::abs(S5-S1*V1-S2*V2-S3*V3-S4*V4)*N2,A>NN,создатьребенка(3,N0,N1,N2,N3,NN,V1,V2,V3,V4,R,R1,R2,R3,R4).
создатьребенка(2,N0,N1,N2,N3,A,A1,A2,A3,A4,R,R1,R2,R3,R4):-результат(S1,S2,S3,S4,S5),родитель(V1,V2,V3,V4,I), NN=math::abs(S5-S1*V1-S2*V2-S3*V3-S4*V4)*N2,A<NN,создатьребенка(3,N0,N1,N2,N3,A,A1,A2,A3,A4,R,R1,R2,R3,R4).
создатьребенка(3,N0,N1,N2,N3,A,A1,A2,A3,A4,R,R1,R2,R3,R4):-результат(S1,S2,S3,S4,S5),родитель(V1,V2,V3,V4,I), NN=math::abs(S5-S1*V1-S2*V2-S3*V3-S4*V4)*N3,A>NN,создатьребенка(4,N0,N1,N2,N3,NN,V1,V2,V3,V4,R,R1,R2,R3,R4).
создатьребенка(3,N0,N1,N2,N3,A,A1,A2,A3,A4,R,R1,R2,R3,R4):-результат(S1,S2,S3,S4,S5),родитель(V1,V2,V3,V4,I), NN=math::abs(S5-S1*V1-S2*V2-S3*V3-S4*V4)*N3,A<NN,создатьребенка(4,N0,N1,N2,N3,A,A1,A2,A3,A4,R,R1,R2,R3,R4).



удалитьродителей(I):-retract(родитель(_,_,_,_,I)).
заменитьродителей(A,B,C,D,I):-ребенок(A,B,C,D,I),N=math::random(4),мутация(A,B,C,D,N,A1,B1,C1,D1),assert(родитель(A1,B1,C1,D1,I)), write(A1),write(" "),write(B1),write(" "),write(C1),write(" "),write(D1),результат(S1,S2,S3,S4,S5), SS=math::abs(S5-S1*A1-S2*B1-S3*C1-S4*D1), write(" "),write(SS),nl.
мутация(A,B,C,D,0,S,B,C,D):-результат(S6,S7,S8,S9,S10),S=math::random(S10),!.
мутация(A,B,C,D,1,A,S,C,D):-результат(S6,S7,S8,S9,S10),S=math::random(15),!.
мутация(A,B,C,D,2,A,B,S,D):-результат(S6,S7,S8,S9,S10),S=math::random(10),!.
мутация(A,B,C,D,3,A,B,C,S):-результат(S6,S7,S8,S9,S10),S=math::random(5),!.
мутация(A,B,C,D,_,A,B,C,D):-!.

создатьреб(A,B,C,D,A1,B1,C1,D1,I):-N=math::random(6),кроссовер(A,B,C,D,A1,B1,C1,D1,N,R1,R2,R3,R4), assert(ребенок(R1,R2,R3,R4,I)).


кроссовер(A1,B1,C1,D1,A2,B2,C2,D2,0,A1,B2,C2,D2):-!.
кроссовер(A1,B1,C1,D1,A2,B2,C2,D2,1,A2,B1,C1,D1):-!.
кроссовер(A1,B1,C1,D1,A2,B2,C2,D2,2,A1,B1,C2,D2):-!.
кроссовер(A1,B1,C1,D1,A2,B2,C2,D2,3,A2,B2,C1,D1):-!.
кроссовер(A1,B1,C1,D1,A2,B2,C2,D2,4,A1,B1,C1,D2):-!.
кроссовер(A1,B1,C1,D1,A2,B2,C2,D2,5,A2,B2,C2,D1):-!.
кроссовер(A1,B1,C1,D1,A2,B2,C2,D2,_,A1,B2,C1,D2):-!.
удалитьдетей(A,B,C,D,I):-retractAll(ребенок(A,B,C,D,I)).
сменапоколения(4):-!.
сменапоколения(I):-удалитьродителей(I),заменитьродителей(A,B,C,D,I),удалитьдетей(A,B,C,D,I),сменапоколения(I+1).


ср(4):-!.
ср(I):-N0=math::random(100),N1=math::random(100),N2=math::random(100),N3=math::random(100),N4=math::random(100),N5=math::random(100),N6=math::random(100),N7=math::random(100),создатьребенка(0,N0,N1,N2,N3,1000000,0,0,0,0,R,A,B,C,D),создатьребенка(0,N4,N5,N6,N7,1000000,0,0,0,0,R1,A1,B1,C1,D1),создатьреб(A,B,C,D,A1,B1,C1,D1,I),ср(I+1).
генетическийшаг(0):-!.
генетическийшаг(I):- ср(0), сменапоколения(0),nl,nl, генетическийшаг(I-1).


    run() :-
   генетическийшаг(25),
     Завершение=readLine(),!;
         _=readchar(). % place your own code here

end implement main

goal
    console::runUtf8(main::run).