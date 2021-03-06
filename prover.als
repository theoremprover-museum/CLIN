%%%                   Clause-Linking Prover
%%%                          (CLIN)
%%%
%%%             DAVID A. PLAISTED and SHIE-JUE LEE
%%%               Department of Computer Science
%%%                University of North Carolina
%%%                Chapel Hill, NC 27599--3175
%%%                   (919) 962-{1751|1934}
%%%                 {plaisted|lee}@cs.unc.edu
%%%
%%% version 1.0
%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% ALS-Prolog/CProlog Version %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%    :- [-'/unc/alexande/research/clin/als_2.05.9'].
     :- [-'ALS'].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Quintus Prolog Version %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%    :- compile('/unc/alexande/research/clin/quintus_2.05.8').
%    :- compile('/unc/alexande/research/clin/quintus_compile_options_2.05.8').

%%% Consult files.
     :- load_file(auxiliary).
     :- load_file('clausify').
     :- load_file(command).
     :- load_file('exprules').
     :- load_file(fls).
     :- load_file(hyper).
     :- load_file(instdel).
     :- load_file(interp).
     :- load_file(skolem).
     :- load_file(define).
     :- load_file(library).
     :- load_file(main).
     :- load_file(pc).
     :- load_file(replace).
     :- load_file(simplify).
     :- load_file(spc).
     :- load_file(try).
     :- load_file(xvisor).
     
%%% Initialization
     :- initialization(abolish(clin_version,1)).
     :- initialization(assert(clin_version('1.0'))).

%%% Welcome message.
     :- initialization((clin_version(Version),
	nl, tab(15),write('WELCOME TO CLIN '),
	version(Version),write(Version),nl,nl,nl)).
     helpmsg :-
	nl,write_line(2,'USER GUIDE : '),
	write_line(2,'Key in "prove(File).".'),
	write_line(2,'Key in "settings." to list all current settings.'),
	write_line(2,'For more info., type "choice.".'),
	write_line(2,'These info. can be reviewed by typing in "helpmsg.".'),
	nl.
     :- initialization(helpmsg).
