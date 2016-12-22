%%% SMALL PROOF CHECK
%%% 
%%% Find if a clause can unify with unit clauses directly or indirectly.
%%% The idea is as follows:
%%%	Take a clause C = [L1,L2,...,Ln]. Search a clause D = [M1,M2,...,Mm]
%%%	such that Mi unifies with L1 and the remaining literals of D
%%%	unifies with unit clauses. Similarly for L2,..., and Ln.
%%%	If we can find such a C, then the set of clauses is unsatisfiable.
%%% Two options:
%%%	Use input clauses as nuclei, i.e. C.
%%%	Use all clauses as nuclei.
%%% Instance deletion:
%%%	We only consider those clauses which is not an instance of other
%%%	clauses.
%%% Tiem control:
%%%	Use the time spent in the last hyper-linking as time-limit for
%%%	the small proof checking.
%%% Replacement consideration:
%%% 	If replacement is specified, we should use all clauses as nuclei.
%%% Ground literals are linked first.
%%% Change small proof size definition.
%%%	If a literal L in a nucleus is ground, don't count size for the 
%%%	electron of L. 
%%%	Otherwise, increase size count by the length of the electron - 1.
%%%	This definition is concerned with the number of backtracking.
%%% We always check for unit electrons first.
%%% Ordering clauses.
%%%	We order clauses by the number of non-ground literals.
%%% 

     spc :- not(not(spc_fail)).

     spc_fail :-
	small_proof_check,
	cputime(TT1),
	spc(Result),
	cputime(TT4),
	TT5 is TT4 - TT1,
	write_line(5,'Small proof checking(s): ',TT5),
	!,
	Result == 1.
     spc_fail :-
      	fail.
	
%%% If no unit clauses, then done.
%%% Otherwise, find non-instant clauses.
     spc(R) :-
	sent_C(cl(_,_,by([_],_,_,_,_),_,_,_,_,_,_)),
	not(not(find_non_instances)),
	spc_1(R),
	not(not(spc_clear)).
     spc(0).

     spc_clear :-
	abolish(cps_A,5),
	abolish(cps_I,6),
	abolish(cps_O,5),
	abolish(cps_U,5),
	abolish(cps_V,5).

%%% If succeeds, print out the instance and its electrons.
     spc_1(1) :-
	spc_1_1(Cnn,Cn1,Electrons),
	print_out_sp(Cnn,Cn1,Electrons).
     spc_1(0).

%%% Obtain small proof size bound.
%%% Obtain time limit.
%%% Set intial time.
     spc_1_1(Cnn,Cn1,Electrons) :-
	clause_largest_length(L),
	LL is L - 1,
	get_spsb(LL,B),
	get_last_hyperlink_time(Tl),
	not(not(initialize_spc_time)), !,
	spc_2(B,LL,Tl,Cnn,Cn1,Electrons,Size), 
	assert(sp_size(Size)).

%%% Obtain small proof size bound.
     get_spsb(LL,D) :-
        small_proof_size_bound(N),
        X is LL*(LL+1),
        minimum(N,X,D).
     get_spsb(_,0).

%%% Obtain time limit.
%%% 10 seconds for the 0th round.
     get_last_hyperlink_time(T) :-
	round_no(X1),
	get_last_hyperlink_time(X1,T4),
	get_spc_time_factor(P),
	T is P * T4.
     get_last_hyperlink_time(0,10) :- !.
     get_last_hyperlink_time(_,T3) :-
	last_hyperlink_time(T3).

%%% Obtain time factor.
     get_spc_time_factor(P) :-
	spc_time_factor(P).
     get_spc_time_factor(2).

%%% Set intial time.
     initialize_spc_time :-
	abolish(start_spc_time,1),
	cputime(T),
	assert(start_spc_time(T)).

%%% Find a contradiction with unit electrons first.
     spc_2(_,_,_,Cnn,Cn1,Es,0) :-
	spc_2_U(Cnn,Cn1,Es).
%%% Run starting from size 0 to the small proof size bound.
     spc_2(B,LL,Tl,Cnn,Cn1,Es,Size) :-
	\+ simple_small_proof_check,
	spc_2_1(0,B,LL,Tl,Cnn,Cn1,Es,Size).

%%% A small proof check succeeds.
     spc_2_1(B1,B2,LL,Tl,Cnn,Cn1,Es,B1) :-
	spc_round(B1,LL,Tl,Cnn,Cn1,Es), !.
%%% If clauses are deleted, then stop.
%%% Otherwise do next round.
     spc_2_1(B1,B2,LL,Tl,Cnn,Cn1,Es,Size) :-
	cps_A(_,_,_,_,_),
	BB is B1 + 1,
	!, BB =< B2,
	!, spc_2_1(BB,B2,LL,Tl,Cnn,Cn1,Es,Size).

%%% Chceck for unit electrons.
     spc_2_U(Cnn,Cn1,Es) :-
	spc_nucleus_U(Cnn,Cn1,Ws1),
	negate_clause(Cn1,NCn1),
	spc_2_U_clause(NCn1,Ws1,Es).

%%% Contradicting a clause.
     spc_2_U_clause([L1|Ls1],Ws1,[[L2]|Es]) :-
	spc_lit_reorder_2(Ws1,[L1|Ls1],W2,Ws2,L2,Ls2),
	!, spc_unify_unit(L2),
	!, spc_2_U_clause(Ls2,Ws2,Es).
     spc_2_U_clause([L1|Ls1],[W1|Ws1],[[L1]|Es]) :-
	spc_unify_unit(L1),
	w1_w2(Ws1,Ws2),
	spc_2_U_clause(Ls1,Ws2,Es).
     spc_2_U_clause([],[],[]).

%%% Pick nucleus for unit electrons.
%%% At round 0, we have to consider unit clauses as nucleus,
%%% otherwise we will get trouble with the following set:
%%% [p(a,X)].
%%% [not p(Y,b)].
     spc_nucleus_U([Lnn],[Ln1],Ws1) :-
	spc_unit_nucleus_condition,
        clause(cps_U(_,Ln1,V11,V11,Ws1),true,Ref1),
	clause(cps_U(_,Lnn,Vnn,Vnn,_),true,Ref1).
     spc_nucleus_U(Cnn,Cn1,Ws1) :-
	spc_nucleus(Cnn,_,Cn1,Ws1).

     spc_unit_nucleus_condition :-
        round_no(0),
	!, session_no(1),
	!, \+ tried_round0(1).

%%% Do not consider unit clauses as nuclei.
%%% If small_proof_check_all.
     spc_nucleus(Cnn,CV1,Cn1,Ws1) :-
        small_proof_check_all, !,
        clause(cps_A(CV1,Cn1,V11,V11,Ws1),true,Ref1),
	clause(cps_A(_,Cnn,Vnn,Vnn,_),true,Ref1).
%%% If not small_proof_check_all.
%%% If replacement is specified, we should use all clauses as nuclei.
     spc_nucleus(Cnn,CV1,Cn1,Ws1) :-
        replacement, !,
        clause(cps_A(CV1,Cn1,V11,V11,Ws1),true,Ref1),
	clause(cps_A(_,Cnn,Vnn,Vnn,_),true,Ref1).
%%% otherwise, we only use input clauses as nuclei.
     spc_nucleus(Cnn,CV1,Cn1,Ws1) :-
        clause(cps_O(CV1,Cn1,V11,V11,Ws1),true,Ref1),
	clause(cps_O(_,Cnn,Vnn,Vnn,_),true,Ref1).

%%% Unify with a unit clause.
     spc_unify_unit(L1) :-
	cps_U(_,L1,V11,V12,_),
	unify_lists(V11,V12).

%%% Get a nucleus.
%%% Check if we have to run this nucleus to avoid redundancy.
%%% Run this nucleus.
     spc_round(0,LL,Tl,Cnn,Cn1,Es) :-
	!, spc_nucleus(Cnn,0,Cn1,Ws1),
	negate_clause(Cn1,NCn1),
	spc_clause(NCn1,Ws1,0,LL,0,Tl,1,Es).
     spc_round(B1,LL,Tl,Cnn,Cn1,Es) :-
	spc_nucleus(Cnn,CV1,Cn1,Ws1),
	CV1 \== 0,
	negate_clause(Cn1,NCn1),
	spc_clause(NCn1,Ws1,CV1,LL,B1,Tl,1,Es).

%%% Check to avoid redundancy.
     spc_check_bound(CV1,LL,B1) :-
	X is CV1 * LL,
	!, X >= B1.

%%% Check one literal at a time.
     spc_clause([L1|Ls1],Ws1,CV1,LL,B1,Tl,F1,[E|Es]) :-
	spc_check_bound(CV1,LL,B1),
	Bl is B1 + 1,
	!, spc_literal([L1|Ls1],Ws1,CV1,B1,Bl,Tl,F1,Ls2,Ws2,CV2,F2,E,B2),
        spc_clause(Ls2,Ws2,CV2,LL,B2,Tl,F2,Es).
     spc_clause([],_,_,_,_,_,_,[]).

%%% If the literal is the last literal, then we treat it as ground since
%%%	if it succeeds, we have found a contradiction. So no backtracking.
     spc_literal([L1],_,_,_,Bl,Tl,F1,[],_,_,_,E,_) :-
	!, spc_literal_last(Bl,F1,L1,Tl,E).
%%% If the literal is ground.
     spc_literal(Ls1,Ws1,CV1,B1,_,Tl,F1,Ls2,Ws2,CV1,F2,E,B1) :-
	spc_lit_reorder_2(Ws1,Ls1,_,Ws2,L2,Ls2),
        !, spc_literal_gall(F1,L2,Tl,F2,E), !.
%%% If the literal is not ground.
%%% If the literal is next to the last one, we don't update W.
     spc_literal([L1|[Ls1]],_,CV1,B1,Bl,Tl,F1,[Ls1],_,CV2,F2,E,B2) :-
	!, spc_literal_vall(B1,Bl,F1,L1,Tl,F2,E,B2),
	CV2 is CV1 - 1.
%%% If the literal is not ground.
     spc_literal([L1|Ls1],[_|Ws1],CV1,B1,Bl,Tl,F1,Ls1,Ws2,CV2,F2,E,B2) :-
        spc_literal_vall(B1,Bl,F1,L1,Tl,F2,E,B2),
	w1_w2(Ws1,Ws2),
	CV2 is CV1 - 1.

%%% If the literal is the last literal in a nucleus.
     spc_literal_last(1,1,_,_,_) :- !, fail.
     spc_literal_last(1,_,L1,_,[L1]) :-
	spc_unify_unit(L1).
     spc_literal_last(B1,_,L1,Tl,E) :-
	spc_nonunit_electron_N(B1,L1,Tl,E).

%%% Consider a non-unit electron with length B1.
     spc_nonunit_electron_N(B1,L1,Tl,Cn2) :-
	cps_V(B1,Cn2,V21,V22,Ws2),
	spc_unify_electron(L1,Tl,Cn2,V21,V22,Ws2).

%%% Unify an electron.
     spc_unify_electron(L1,Tl,Cn2,V21,V22,Ws2) :-
	not(not(spc_time_underflow(Tl))),
	oneliteral_link(Cn2,V21,V22,Ws2,L1),
	not(not(spc_time_underflow(Tl))).

%%% Consider an electron for a ground literal of a nucleus.
     spc_literal_gall(F1,L1,_,F1,[L1]) :-
	spc_unify_unit(L1).
     spc_literal_gall(_,L1,Tl,2,E) :-
	spc_nonunit_electron_g(L1,Tl,E).

%%% Consider a non-unit electron for a ground literal of a nucleus.
     spc_nonunit_electron_g(L1,Tl,Cn2) :-
	cps_V(_,Cn2,V21,V22,Ws2),
	spc_unify_electron(L1,Tl,Cn2,V21,V22,Ws2).

%%% Consider an electron for a non-ground literal of a nucleus.
     spc_literal_vall(B1,_,F1,L1,_,F1,[L1],B1) :-
	spc_unify_unit(L1).
     spc_literal_vall(B1,Bl,_,L1,Tl,2,E,B2) :-
	spc_nonunit_electron_v(B1,Bl,L1,Tl,E,B2).

%%% Consider a non-unit electron for a non-ground literal of a nucleus.
     spc_nonunit_electron_v(B1,Bl,L1,Tl,Cn2,B2) :-
	cps_V(CV2,Cn2,V21,V22,Ws2),
	Bl >= CV2,
	spc_unify_electron(L1,Tl,Cn2,V21,V22,Ws2),
	B2 is B1 - CV2 + 1.
	
%%% Unifies one literal, and linkes the left with unit clauses.
     oneliteral_link(Cn2,V21,V22,Ws2,L1) :-
	mate_and_rest(Cn2,V21,V22,Ws2,L1,Ls3,W1,Ws3),
	negate_clause(Ls3,NLs3),
	spc_w1_w2(W1,Ws3,Ws4),
        spc_unify_units(NLs3,Ws4).

     mate_and_rest([L1|Ls3],V21,V22,[W1|Ws2],L1,Ls3,W1,Ws2) :-
        unify_lists(V21,V22).
     mate_and_rest([L2|Ls2],V21,V22,[W2|Ws2],L1,[L2|Ls3],W1,[W2|Ws3]) :-
        mate_and_rest(Ls2,V21,V22,Ws2,L1,Ls3,W1,Ws3).

%%% If W1 is var, do nothing.
     spc_w1_w2(W1,Ws3,Ws3) :-
	var(W1), !.
     spc_w1_w2(_,Ws3,Ws4) :-
	w1_w2(Ws3,Ws4), !.

%%% Unifies with unit clauses.
     spc_unify_units([L1|Ls1],Ws1) :-
	spc_lit_reorder_2(Ws1,[L1|Ls1],_,Ws2,L2,Ls2),
	!, spc_unify_unit(L2),
	!, spc_unify_units(Ls2,Ws2).
     spc_unify_units([L1|Ls1],[_|Ws1]) :-
	spc_unify_unit(L1),
	w1_w2(Ws1,Ws2),
	spc_unify_units(Ls1,Ws2).
     spc_unify_units([],_).

%%% Check time overflow.
     spc_time_underflow(Tl) :-
        spc_time_underflow_1(Tl), !.
     spc_time_underflow(_) :-
        abolish(cps_A,5), !, fail.

     spc_time_underflow_1(Tl) :-
	cputime(T1),
	start_spc_time(T2),
	T3 is T1 - T2, !,
	T3 =< Tl.

%%% Find all clauses which is not an instance of other clauses.
%%% The non-instance clauses are asserted into database in increasing size
%%% of literals.

     find_non_instances :-
        find_non_instances_input,
        find_non_instances_hl,
        order_clause, !.
 
     find_non_instances_input :-
        sent_C(cl(_,CN1,by(Cn1,V11,V12,V1,W1),1,_,_,_,_,_)),
        check_instance(1,CN1,Cn1,V11,V12,V1,W1),
        fail.
     find_non_instances_input.
       
     find_non_instances_hl :-
        sent_C(cl(_,CN1,by(Cn1,V11,V12,V1,W1),0,_,_,_,_,_)),
        check_instance(0,CN1,Cn1,V11,V12,V1,W1),
        fail.
     find_non_instances_hl.
       
     check_instance(T,CN1,[X],V11,V12,V1,W1) :-
        asserta(cps_I(T,CN1,[X],V11,V12,W1)), !.
     check_instance(T,CN1,Cn1,V11,V12,V1,W1) :-
        check_instance_1(T,CN1,Cn1,V11,V12,V1,W1), !.
 
%%% We assume that input clauses are not instances of other clauses.
     check_instance_1(1,CN1,Cn1,V11,V12,_,W1) :-
        asserta(cps_I(1,CN1,Cn1,V11,V12,W1)), !.
     check_instance_1(T,CN1,Cn1,V11,V12,V1,W1) :-
        \+ clause_instance(CN1,Cn1,V11,V12,V1),
        asserta(cps_I(T,CN1,Cn1,V11,V12,W1)), !.
     check_instance_1(_,_,_,_,_,_,_).
       
     clause_instance(CN1,Cn1,V11,V11,V1) :-
        const_list(V1), !,
        cps_I(_,CN1,Cn1,V21,V21,_).
 
%%% Order the clauses by the number of literals.
     order_clause :-
	order_clause_1(0).
     order_clause :-
	order_clause_2.
     order_clause.

     order_clause_1(N) :-
	cps_I(_,_,_,_,_,_),
	order_clause_1_1(N),
	NN is N + 1,
	!, order_clause_1(NN).
 
%%% Pick up clauses with N non-ground literals.
     order_clause_1_1(N) :-
	retract(cps_I(T,N,Cn1,V11,V12,W1)),
	assertz(cps_A(N,Cn1,V11,V12,W1)),
	assert_cps_O(Cn1,T,N,V11,V12,W1),
	fail.
     order_clause_1_1(_).

%%% We put aside input clauses.
%%% We don't assert unit clauses.
     assert_cps_O([_],_,_,_,_,_) :- !, fail.
     assert_cps_O(Cn1,1,N,V11,V12,W1) :-
	assertz(cps_O(N,Cn1,V11,V12,W1)), !.

%%% Retract unit clauses.
     order_clause_2 :-
	order_clause_2_U.
%%% Separate all non-ground literals clauses.
     order_clause_2 :-
	order_clause_2_V.

%%% Retract unit clauses.
     order_clause_2_U :-
	retract(cps_A(N,[X],V11,V12,W1)),
	assertz(cps_U(N,X,V11,V12,W1)),
	fail.

%%% Separate all non-ground literals clauses.
     order_clause_2_V :-
	cps_A(N,Cn1,V11,V12,W1),
	length(Cn1,N),
	assertz(cps_V(N,Cn1,V11,V12,W1)),  
	fail.
	
%%% Print out nucleus, instance and electrons.
     print_out_sp(Cnn,Cn1,Electrons) :-
        write_line(10,'Small proof found at nucleus:'),
        vars_tail(Cnn,Vn),
        print_clause_2(10,Cnn,Vn),
        vars_tail(Cn1,V1),
        write_line(10,'The instance is:'),
        print_clause_2(10,Cn1,V1),
        write_line(10,'The electrons are:'),
        init_num,
        print_electrons(Electrons).
 
%%% Print out electrons.
     print_electrons([E|Es]) :-
        vars_tail(E,V),
        next_num(N),
        write_numberedline_head(10,N,'.'),
        print_clause_2(2,E,V), !,
        print_electrons(Es).
     print_electrons([]).

     spc_lit_reorder_2(W1,Ls,W11,Ws1,L1,Ls1) :-
	spc_literal_reordering,
	!, sep_gr_lit_2(W1,Ls,W11,Ws1,L1,Ls1).
