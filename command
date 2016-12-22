%%% COMMANDS 
%%%
%%% This file contains a list of settings. Commands for setting and
%%% retracting settings are also provided.
%%%

%%% A list of default settings and possible options.
     choice :-
	write_line(2,'Default settings:'),
	write_line(5,'count_all_literals.'),
	write_line(5,'depth_coef(0).'),
	write_line(5,'do_hyper_linking.'),
	write_line(5,'greatest_time_bound(800).'),
	write_line(5,'hl_literal_reordering.'),
	write_line(5,'least_time_bound(200).'),
	write_line(5,'literal_coef(0).'),
	write_line(5,'max_no_clauses(1600).'),
	write_line(5,'relevance_bound(100).'),
	write_line(5,'relevance_coef(0).'),
	write_line(5,'replace_literal_reordering.'),
	write_line(5,'simple_small_proof_check.'),
	write_line(5,'size_coef(1).'),
	write_line(5,'small_proof_check.'),
	write_line(5,'small_proof_size_bound(100).'),
	write_line(5,'spc_literal_reordering.'),
	write_line(5,'start_no_clauses_coef(3).'),
	write_line(5,'support_list([b,f]).'),
	write_line(5,'tautology_deletion.'),
	write_line(5,'time_limit(2000).'),
	write_line(5,'unit_resolution.'),
	write_line(5,'user_control.'),
	write_line(2,'more (y./n.)?'),
	read(Step1),
	choice2(Step1).

     choice2(y) :-
	write_line(2,'print-out commands(set/clear):'),
	write_line(5,'outCagen: print out clauses after generation'),
	write_line(5,'outCahl: print out clauses after hyper-linking'),
	write_line(5,'outCainst: print out clauses after instance deletion.'),
	write_line(5,'outCasimp: print out clauses after subsump/simplif.'),
	write_line(5,'outfls: compute real fully linked subset.'),
	write_line(5,'outinst: print out nucleus and hyper-instances.'),
	write_line(5,'outhllits: print out literal lists for hyper-linking.'),
	write_line(5,'outpc: print out model or relevant clauses for PC.'),
	write_line(5,'outpcin: print out input set to PC.'),
	write_line(2,'more (y./n.)?'),
	read(Step2),
	choice3(Step2), !.
     choice2(n).

     choice3(y) :-
	write_line(2,'settings[assign(name,value)/delete(name,arity)]:'),
	write_line(5,'depth_coef(D): depth coefficient for priority.'),
	write_line(5,'literal_coef(L): depth coefficient for priority.'),
	write_line(5,'max_no_clauses(M): max number of clauses retained.'),
	write_line(5,'relevance_bound(R): relevance bound for clauses.'),
	write_line(5,'relevance_coef(R): depth coefficient for priority.'),
	write_line(5,'size_coef(S): depth coefficient for priority.'),
	write_line(5,'start_no_clauses(S): starting number of clauses retained.'),
	write_line(5,'start_no_clauses_coef(S): factor to multiply start_no_clauses.'),
	write_line(5,'support_list(S): support strategies.'),
	write_line(5,'time_limit(M): time limit for a run.'),
	write_line(5,'time_limit_coef(C): time limit coefficient for a run.'),
	write_line(5,'time_limit_multiple(M): advance time limit by M times for supervisor mode.'),
	write_line(2,'more (y./n.)?'),
	read(Step3),
	choice4(Step3), !.
     choice3(n).

     choice4(y) :-
	write_line(2,'settings[assign(name,value)/delete(name,arity)]:'),
	write_line(5,'back_literalbound(B): literal bound of number of literals for a backward supported clause.'),
	write_line(5,'small_proof_size_bound(D).'),
	write_line(5,'for_literalbound(F): literal bound of number of literals for a forward supported clause.'),
	write_line(5,'literal_bound(L): literal bound of number of literals for a clause.'),
	write_line(5,'user_literalbound(U): literal bound of number of literals for a user supported clause.'),
	write_line(5,'start_wu_bound(S): starting work unit bound for sliding priority.'),
	write_line(2,'more (y./n.)?'),
	read(Step4),
	choice5(Step4), !.
     choice4(n).

     choice5(y) :-
	write_line(2,'options(set/clear):'),
	write_line(5,'count_all_literals: count all literals in the sliding priority computation.'),
	write_line(5,'delete_all_instances: delete all instances.'),
	write_line(5,'delete_nf_instances: delete non-forward supported instances.'),
	write_line(5,'do_hyper_linking: perform hyper-linking.'),
	write_line(5,'ground_disting_after_match: ground distinguished literals after matching for replacements.'),
	write_line(5,'ground_restriction: ground instances for replacements.'),
	write_line(5,'ground_substitution: ground electrons for replacements.'),
	write_line(5,'hl_literal_reordering: reordering for hyper-linking.'),
	write_line(5,'realfls: compute real largest fully linked subset.'),
	write_line(5,'replace_literal_reordering: reordering for replacement.'),
	write_line(5,'replacement: use definition replacements for literals.'),
	write_line(5,'simple_small_proof_check: use unit clauses as elctrons in small proof check.'),
	write_line(5,'size_by_clause: the clause size for priority is the sum of all literal sizes.'),
	write_line(5,'slidepriority: use sliding priority.'),
	write_line(5,'small_proof_check: call small proof checking.'),
	write_line(5,'small_proof_check_all: use all clauses for nucleus in small proof checking.'),
	write_line(5,'spc_literal_reordering: reordering for small proof.'),
	write_line(5,'sum_of_measures: priority is the sum of measures.'),
	write_line(5,'super_batch: batch mode for top level supervisor.'),
	write_line(5,'tautology_deletion: perform tautology deletion.'),
	write_line(5,'unit_resolution: call unit subsumption/simplification.'),
	write_line(5,'user_control: do not call top level supervisor.'),
	write_line(2,'more (y./n.)?'),
	read(Step5),
	choice6(Step5), !.
     choice5(n).

     choice6(y) :-
	write_line(2,'These are for top level supervisor[assign(name,value)/delete(name,arity)]:'),
	write_line(5,'least_time_bound(B): lower time bound.'),
	write_line(5,'greatest_time_bound(B): upper time bound.'), nl.
     choice6(n).

%%% Change options.
     set_clear_comlist([outCagen, outCahl, outCainst, outCasimp, outfls,
			outinst, outhllits, outpc, outpcin, 
			count_all_literals, delete_all_instances, 
			delete_nf_instances, do_hyper_linking, 
			ground_disting_after_match, ground_restriction,
			ground_substitution, hl_literal_reordering, 
			realfls, replace_literal_reordering,
			replacement, simple_small_proof_check,
			size_by_clause, slidepriority,
			small_proof_check, small_proof_check_all,
			spc_literal_reordering, sum_of_measures,
			super_batch, tautology_deletion, 
			unit_resolution, user_control]).

     assign_delete_comlist([depth_coef, literal_coef, max_no_clauses,
			    relevance_bound, relevance_coef, size_coef,
			    start_no_clauses, start_no_clauses_coef,
			    support_list, time_limit, time_limit_coef,
			    time_limit_multiple, 
			    back_literalbound, small_proof_size_bound,
			    for_literalbound, literal_bound,
			    user_literalbound, start_wu_bound,
			    least_time_bound, greatest_time_bound]).

     set(Command) :- 
	set_clear_comlist(ComList), 
	member(Command, ComList), 
	!, assertin(Command,0,Command).
     set(Command) :- 
	write_line(5, 'No such command: ', Command).

     clear(Command) :- 
	set_clear_comlist(ComList), 
	member(Command, ComList),
	!, pullout(Command,0).
     clear(Command) :- 
	write_line(5, 'No such command: ', Command). 

     assign_cmd(Command,X) :-
	assign_delete_comlist(ComList), 
	member(Command, ComList),
	Com =.. [Command, X],
	!, assertin(Command,1,Com).
     assign_cmd(Command,_) :-
	write_line(5, 'No such command: ', Command).  

     delete(Command,X) :-
	assign_delete_comlist(ComList),    
	member(Command, ComList), 
	!, abolish(Command,X).
     delete(Command,_) :-
	write_line(5, 'No such command: ', Command).

     assertin(X,N,Y) :-
	abolish(X,N),
	assert(Y).

     pullout(X,N) :-
	abolish(X,N).

%%% SET DEFAULTS
     count_all_literals.
     depth_coef(0).
     do_hyper_linking.
     greatest_time_bound(800).
     hl_literal_reordering.
     infinity(1000).
     least_time_bound(200).
     literal_coef(0).
     max_no_clauses(1600).
     relevance_bound(100).
     relevance_coef(0).
     replace_literal_reordering.
     size_coef(1).
     simple_small_proof_check.
     small_proof_check.
     small_proof_size_bound(100).
     spc_literal_reordering.
     start_no_clauses_coef(3).
     support_list([b,f]).
     tautology_deletion.
     time_limit(2000).
     unit_resolution.
     user_control.

%%% constant list for transforming clauses or literals to ground.
     const_list(['$1','$2','$3','$4','$5','$6','$7','$8','$9','$10',
		 '$11','$12','$13','$14','$15','$16','$17','$18','$19','$20']).

%%% constant list for transforming clauses to Gr(F).
     grf_list(['$','$','$','$','$','$','$','$','$','$',
	       '$','$','$','$','$','$','$','$','$','$']).

%%% A list of 20 variables. This is for making pretty outputs.
     var_list(['V','W','X','Y','Z', 'V1','W1','X1','Y1','Z1',
	      'V2','W2','X2','Y2','Z2', 'V3','W3','X3','Y3','Z3',
	      'V4','W4','X4','Y4','Z4']).
