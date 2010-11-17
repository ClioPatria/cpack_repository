/*  Part of ClioPatria SeRQL and SPARQL server

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2010, University of Amsterdam,
		   VU University Amsterdam

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(cpack_dependency,
	  [ file_used_by_file_in_package/3, % +File, -UsedBy, -Package
	    cpack_requires/3,		% +Package, -Package, -Why
	    cpack_conflicts/3,		% +Package, -Package, -Why
	    cpack_list/2,		% +Package, -ListOfImplied
	    cpack_not_satisfied/2,	% +Package, -Reasons
	    file_not_satisfied/2,	% +File, -Reasons
	    file_imports_from/3		% +File, -Imports, -From
	  ]).
:- use_module(library(assoc)).
:- use_module(library(ugraphs)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(repository).

/** <module> Query the CPACK dependency graph

This module queries  the  RDF  graph   produced  by  xref.pl  to compute
high-level dependencies between objects. Currently, we keep track of:

  * Requirement reasons
    - Files needed by one and provided by another package
    - Tokens needed by one and provided by another package
  * Conflict reasons
    - Packages holding files that provide the same module
    - Packages holding files with the same path
  * Satisfied status

@tbd	Extend reasoning
*/

%%	file_used_by_file_in_package(+File, -UsedBy, -Package) is nondet.
%
%	True when UsedBy is a file in Package that imports File.

file_used_by_file_in_package(File, UsedBy, Pack) :-
	rdf_has(File, cpack:resolves, FileRef),
	rdf_has(UsedBy, cpack:usesFile, FileRef),
	rdf_has(UsedBy, cpack:inPack, Pack).


%%	cpack_requires(+Package, -Required, -Reasons) is nondet.
%
%	True when Package requires Required. Reasons   is an ordered set
%	of reasons. Individual reasons are one of:
%
%	  * token(Name)
%	  Package requires _token_ that is provided by Required.
%	  * file_ref(FileRef)
%	  Package uses the file FileRef, which is provided by Required

cpack_requires(Package, Required, AllReasons) :-
	setof(Why, cpack_requires_by(Package, Required, Why), AllReasons).

cpack_requires_by(Package, Required, token(Token)) :-
	rdf_has(Package, cpack:requires, Req),
	(   rdf_is_literal(Req)
	->  Token = Req
	;   rdf_has(Req, cpack:name, Token)
	),
	rdf_has(Required, cpack:provides, Token).

cpack_requires_by(Package, Required, file_ref(FileRef)) :-
	rdf_has(File, cpack:inPack, Package),
	rdf_has(File, cpack:usesFile, FileRef),
	rdf_has(ReqFile, cpack:resolves, FileRef),
	rdf_has(ReqFile, cpack:inPack, Required),
	Required \== Package.

%%	cpack_conflicts(+Package, -Conflict, -Why) is nondet.
%
%	True  when  Package  and  Conflict   are  in  conflict.  Defined
%	conflicts are:
%
%	  * same_module(Module,File1,File2)
%	  Both files define the same module.  They cannot be loaded into
%	  the same Prolog instance.  Note that this can cause a package
%	  to conflict with itself!
%	  * same_file(Path,File1,File2)
%	  Two packages define files at the same path.  This is actually
%	  not an issue in itself. It only becomes an issue if there are
%	  file_ref objects that resolve them ambiguously.

cpack_conflicts(Package, Conflict, AllReasons) :-
	setof(Why, cpack_conflicts_by(Package, Conflict, Why), AllReasons).

cpack_conflicts_by(Package, Conflict, same_module(M,File1,File2)) :-
	rdf_has(File1, cpack:module, Module),
	rdf_has(File2, cpack:module, Module),
	File1 \== File2,
	Module = literal(M),
	rdf_has(Package, cpack:in_file, File1),
	rdf_has(Conflict, cpack:in_file, File2).
cpack_conflicts_by(Package, Conflict, same_file(Path,File1,File2)) :-
	rdf_has(File1, cpack:path, LPath),
	rdf_has(File2, cpack:path, LPath),
	File1 \== File2,
	LPath = literal(Path),
	rdf_has(Package, cpack:in_file, File1),
	rdf_has(Conflict, cpack:in_file, File2).


		 /*******************************
		 *	      GRAPH		*
		 *******************************/

%%	cpack_list(+Pack, -PackList) is det.
%
%	PackList is a list of all packages  that need to be installed to
%	get Pack working. This list is ensured to contain Pack.
%
%	@tbd	Toplogical sorting may not be possible.  As ordering is
%		not always necessary, we should try to relax
%		dependencies if a topological sort is not possible due
%		to cycles.  There are two heuristics here.  First of
%		all, explicit (token) dependencies may be removed and
%		second, libraries must be loaded before applications.

cpack_list(Pack, Packs) :-
	dependency_ugraph(Pack, Ugraph),
	(   top_sort(Ugraph, Packs)
	->  check_conflicts(Packs),
	    check_satisfied(Packs)
	;   domain_error(non_cyclic_dependency_structure, Ugraph)
	).

check_conflicts(Packs) :-
	append(_,[P1|Rest], Packs),
	cpack_conflicts(P1, Conflict, Reasons),
	member(Conflict, Rest), !,
	throw(error(cpack_error(conflict(P1, Conflict, Reasons)), _)).
check_conflicts(_).

check_satisfied(Packs) :-
	maplist(cpack_satisfied, Packs).

cpack_satisfied(Pack) :-
	cpack_not_satisfied(Pack, Reasons), !,
	throw(error(cpack_error(not_satisfied(Pack, Reasons)), _)).
cpack_satisfied(_).

%%	dependency_ugraph(+Pack, -Ugraph) is det.
%
%	Create a full dependency graph for pack as a ugraph.

dependency_ugraph(Pack, Ugraph) :-
	empty_assoc(Visited),
	dependency_ugraph([Pack], Visited, Ugraph).

dependency_ugraph([], _, []).
dependency_ugraph([H|T], Visited, Graph) :-
	(   get_assoc(H, Visited, _)
	->  dependency_ugraph(T, Visited, Graph)
	;   findall(Required, cpack_requires(H, Required, _), RList),
	    Graph = [H-RList|More],
	    put_assoc(H, Visited, true, Visited2),
	    append(RList, T, Agenda),
	    dependency_ugraph(Agenda, Visited2, More)
	).


		 /*******************************
		 *	     SATISFIED		*
		 *******************************/

%%	cpack_not_satisfied(+Package, -WhyNot) is semidet.
%
%	True when WhyNot describes why Package is not satisfied.

cpack_not_satisfied(Pack, AllReasons) :-
	setof(Due, cpack_not_satisfied_due(Pack, Due), AllReasons).

cpack_not_satisfied_due(Package, no_token(Token)) :-
	rdf_has(Package, cpack:requires, Req),
	(   rdf_is_literal(Req)
	->  Token = Req
	;   rdf_has(Req, cpack:name, Token)
	),
	\+ rdf_has(_, cpack:provides, Token).
cpack_not_satisfied_due(Package, file(File, Problems)) :-
	rdf_has(File, cpack:inPack, Package),
	file_not_satisfied(File, Problems).


%%	file_not_satisfied(+File, -Reasons) is semidet.
%
%	True when File's conditions are not satisfied due to Reasons.

file_not_satisfied(File, AllReasons) :-
	setof(Due, file_not_satisfied_due(File, Due), AllReasons).

%%	file_not_satisfied_due(+File, -Problem)
%
%	True when Conflict describes an import   problem for File. There
%	are two types of import problems:
%
%	  * double_import(PI, File1, File2)
%	  File imports File1 and File2 using use_module/1, both of which
%	  export PI.
%	  * locally_defined(PI, File)
%	  A locally defined predicate is also imported from File.
%	  * file_not_found(FileRef)
%	  The given symbolic path cannot be found.
%
%	@tbd We do not yet keep track of locally defined predicates

file_not_satisfied_due(File, double_import(PI,File1,File2)) :-
	file_imports_pi_from(File, File1, PI),
	file_imports_pi_from(File, File2, PI),
	File1 \== File2.
file_not_satisfied_due(File, file_not_found(FileRef)) :-
	rdf_has(File, cpack:usesFile, FileRef),
	rdfs_individual_of(FileRef, cpack:'FileRef'),
	\+ rdf_has(_, cpack:resolves, FileRef).
file_not_satisfied_due(File, predicate_not_found(PI)) :-
	LPI = literal(PI),
	rdf_has(File, cpack:requiresPredicate, LPI),
	\+ file_imports_pi_from(File, _, PI),
	\+ file_calls_public_from(File, _, PI),
	\+ other_source(PI).

other_source(API) :-
	atom_to_term(API, PI, []),
	pi_head(PI, Head),
	(   predicate_property(Head, multifile)
	;   predicate_property(Head, autoload(_))
	).

pi_head(M:PI, M:Head) :- !,
	pi_head(PI, Head).
pi_head(Name/Arity, Head) :-
	functor(Head, Name, Arity).


%%	file_imports_from(+File, -Predicates, -From) is nondet.
%
%	True if File imports Predicates from the file From.
%
%	@param Predicates is a list of canonical predicate indicators.

file_imports_from(File, PIs, From) :-
	setof(PI, file_imports_pi_from(File, From, PI), PIs).

file_imports_pi_from(File, UsedFile, PI) :-
	rdf_has(File, cpack:usesFile, Uses),
	(   rdfs_individual_of(Uses, cpack:'FileRef')
	->  rdf_has(UsedFile, cpack:resolves, Uses)
	;   UsedFile = Uses
	),
	rdf_has(UsedFile, cpack:exportsPredicate, literal(PI)).

%%	file_calls_public_from(+File, -UsedFile, +PI) is semidet.
%
%	True if PI is a module-qualified  term   that  can  be called in
%	UsedFile, that is imported from File.

file_calls_public_from(File, UsedFile, PI) :-
	(   rdf_has(UsedFile, cpack:publicPredicate, literal(PI))
	->  true
	;   atom_to_term(PI, M:PPI, []),
	    rdf_has(UsedFile, cpack:module, literal(M)),
	    format(atom(Plain), '~q', [PPI]),
	    rdf_has(UsedFile, cpack:exportsPredicate, literal(Plain))
	),
	(   rdf_has(File, cpack:usesFile, UsedFile)
	->  true
	;   rdf_has(UsedFile, cpack:resolves, Uses),
	    rdf_has(File, cpack:usesFile, Uses)
	), !.



