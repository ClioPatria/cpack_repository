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

:- module(cpack_xref,
	  [ xref_cpack/1,
	    xref_cpack_file/1
	  ]).
:- use_module(library(apply)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(git)).
:- use_module(library(prolog_xref)).
:- use_module(library(prolog_source)).
:- use_module(repository).

/** <module> Compute dependencies between CPACKs

This module runs the Prolog  cross-referencer   on  a  submitted pack to
analyse the package dependencies.

*/

%%	xref_cpack(+Pack) is det.
%
%	Create cross-reference info for a complete pack.

xref_cpack(Pack) :-
	findall(File, pack_prolog_file(Pack, File), Files),
	maplist(xref_cpack_file, Files),
	maplist(resolve_file, Files).

pack_prolog_file(Pack, File) :-
	rdf_has(File, cpack:inPack, Pack),
	rdf_has(File, cpack:name, literal(Name)),
	file_name_extension(_, Ext, Name),
	prolog_file_type(Ext, prolog).


%%	xref_cpack_file(+File) is det.
%
%	Do cross-reference analysis on the CPACK   file  File. This adds
%	various properties to help dependency tracking to File:
%
%	  * cpack:module
%	  * cpack:exportsPredicate

xref_cpack_file(File) :-
	xref_source(File),
	xref_to_rdf(File).

:- rdf_meta
	file_property(r,r,o).

xref_to_rdf(File) :-
	rdf_has(File, cpack:inPack, Graph),
	forall(setof(O, file_property(File, P, O), OL),
	       assert_objects(OL, File, P, Graph)).

assert_objects([], _, _, _).
assert_objects([O|T], S, P, G) :-
	rdf_assert(S,P,O,G),
	assert_objects(T, S, P, G).


%%	file_property(+File, ?P, ?O) is nondet.
%
%	True when rdf(File,P,O) describes a property   of  File. Used to
%	generate all properties from the cross-referencer output.
%
%	@tbd	We can compute other dependencies (referenced RDF
%		namespaces, used HTML resources, etc.)
%	@tbd	We can also compute possible dangerous code.  See
%		safecode as developed as part of SWAPP.

file_property(File, cpack:module, literal(Module)) :-
	xref_module(File, Module).
file_property(File, cpack:exportsPredicate, literal(Pred)) :-
	xref_exported(File, Callable),
	head_atom(Callable, Pred).
file_property(File, cpack:requiresPredicate, literal(Pred)) :-
	xref_called(File, Callable, _),
	\+ xref_local_defined(File, Callable),
	head_atom(Callable, Pred).
file_property(File, UsesFile, Uses) :-
	xref_uses_file(File, Spec, Path),
	(   rdf_is_resource(Path),
	    rdfs_individual_of(Path, cpack:'File')
	->  rdf_equal(UsesFile, cpack:usesPackageFile),
	    Uses = Path
	;   file_ref(Spec, Uses),
	    (   Path == '<not_found>'
	    ->  rdf_equal(UsesFile, cpack:usesPackageFile)
	    ;   system_file(Path, FileURI, Graph)
	    ->  rdf_equal(UsesFile, cpack:usesSystemFile),
		rdf_assert(FileURI, cpack:resolves, Uses, Graph)
	    ;   cliopatria_file(Path, FileURI, Graph)
	    ->  rdf_equal(UsesFile, cpack:usesClioPatriaFile),
		rdf_assert(FileURI, cpack:resolves, Uses, Graph)
	    ;   rdf_equal(UsesFile, cpack:usesPackageFile)
	    )
	).


head_atom(Head, Atom) :-
	head_pi(Head, PI),
	format(atom(Atom), '~q', [PI]).

head_pi(M:Term, M:PI) :- !,
	head_pi(Term, PI).
head_pi(Term, Name/Arity) :-
	functor(Term, Name, Arity).

xref_local_defined(Src, Callable) :-
	xref_defined(Src, Callable, How),
	How \= imported(_From).


%%	system_file(+Path, -FileURI, -Graph) is semidet.
%%	cliopatria_file(+Path, -FileURI, -Graph) is semidet.
%
%	Classify file according to their origin.

:- rdf_meta
	system_file_uri(+, +, r, r, r).

system_file(Path, FileURI, Graph) :-
	current_prolog_flag(home, Home),
	sub_atom(Path, 0, _, _, Home), !,
	cpack_uri(graph, prolog, Graph),
	system_file_uri(Path, Home, cpack:'SystemFile', Graph, FileURI).

cliopatria_file(Path, FileURI, Graph) :-
	absolute_file_name(cliopatria(.),
			   Home,
			   [ file_type(directory),
			     access(read)
			   ]),
	sub_atom(Path, 0, _, _, Home),
	\+ loaded_package_file(Path), !,
	cpack_uri(graph, cliopatria, Graph),
	system_file_uri(Path, Home, cpack:'ClioPatriaFile', Graph, FileURI).

loaded_package_file(Path) :-
	setting(cpack:package_directory, PackageDir),
	absolute_file_name(PackageDir, PackageRoot),
	sub_atom(Path, 0, _, _, PackageRoot).

system_file_uri(Path, Root, Class, Graph, URI) :-
	directory_file_path(Root, RelPath, Path),
	cpack_uri(prolog, RelPath, URI),
	(   rdfs_individual_of(URI, cpack:'File')
	->  true
	;   file_base(RelPath, Base),
	    file_base_name(RelPath, FileName),
	    rdf_assert(URI, rdf:type, Class, Graph),
	    rdf_assert(URI, cpack:path, literal(RelPath), Graph),
	    rdf_assert(URI, cpack:name, literal(FileName), Graph),
	    rdf_assert(URI, cpack:base, literal(Base), Graph),
	    (	xref_public_list(Path, _, Module, Public, _Meta, -)
	    ->	rdf_assert(URI, cpack:module, literal(Module), Graph),
		forall(member(PI, Public),
		       (   cannonical_pi(PI, CannPI),
		           format(atom(Id), '~q', [CannPI]),
			   rdf_assert(URI, cpack:exportsPredicate,
				      literal(Id), Graph)
		       ))
	    ;	true
	    )
	).

cannonical_pi(Name//DCGArity, Name/Arity) :- !,
	Arity is DCGArity + 2.
cannonical_pi(PI, PI).

		 /*******************************
		 *	   FIND FILES		*
		 *******************************/

%%	search_file(+Spec, -File:uri) is nondet.
%
%	True when File is a candidate   for resolving the given symbolic
%	path Spec. The trick is that  the   path  =cpacks= refers to all
%	packs in our database, so we   need to rewrite our specification
%	as cpacks(Path).  I.e.,
%
%		library(X) --> cpacks(lib/X)

search_file(Spec, File) :-
	path_rule(Spec, cpacks(Segments)),
	path_segments_atom(Segments, InPack),
	(   Target = InPack
	;   user:prolog_file_type(Ext, prolog),
	    file_name_extension(InPack, Ext, Target)
	),
	once(rdf_has(File, cpack:path, literal(Target))).

path_rule(Alias, NewAlias) :-
	Alias =.. [Name,Local],
	user:file_search_path(Name, Exp),
	Exp =.. [NewName,Parent],
	NewAlias =.. [NewName,Parent/Local].


		 /*******************************
		 *	 FILE REFERENCES	*
		 *******************************/

%%	file_ref(+Spec, -URI) is det.
%
%	True when URI is the URI for  a FileRef instance that represents
%	the  symbolic  file-reference  Spec.  If  there  is  already  an
%	existing URI for the file-reference, this is returned. Otherwise
%	it creates an instance of cpack:FileRef.

file_ref(Spec, URI) :-
	format(atom(Id), '~q', [Spec]),
	cpack_uri(file_ref, Id, URI),
	(   rdf(URI, rdf:type, cpack:'FileRef')
	->  true
	;   cpack_uri(graph, 'file-references', Graph),
	    file_base(Spec, BaseName),
	    rdf_assert(URI, rdf:type, cpack:'FileRef', Graph),
	    rdf_assert(URI, cpack:name, literal(Id), Graph),
	    rdf_assert(URI, cpack:base, literal(BaseName), Graph),
	    resolve_file_ref(URI)
	).

file_base(Spec, Base) :-
	atom(Spec), !,
	file_base_name(Spec, File),
	file_name_extension(Base, _Ext, File). % demand Prolog?
file_base(Spec, Base) :-
	arg(1, Spec, Name),
	file_base(Name, Base).

%%	resolve_file(+File) is det.
%
%	Create links to the  FileRef  objects   to  which  this file may
%	resolve.

resolve_file(File) :-
	rdf_has(File, cpack:base, Base),
	findall(FileRef,
		(   rdf_has(FileRef, cpack:base, Base),
		    rdfs_individual_of(FileRef, cpack:'FileRef')
		),
		Candidates),
	maplist(resolve_file_ref, Candidates).


%%	resolve_file_ref(+FileRef) is det.
%
%	File files to which FileRef can resolve  and add relate the file
%	to this FileRef.

resolve_file_ref(FileRef) :-
	rdf_has(FileRef, cpack:name, literal(Id)),
	atom_to_term(Id, Spec, _Vars),
	forall(search_file(Spec, File),
	       assert_resolves(File, FileRef)).

assert_resolves(File, FileRef) :-
	rdf_has(File, cpack:resolves, FileRef), !.
assert_resolves(File, FileRef) :-
	rdf_has(File, cpack:inPack, Pack),
	rdf_assert(File, cpack:resolves, FileRef, Pack).


		 /*******************************
		 *   HOOKS TO WORK ON GIT REPO	*
		 *******************************/

:- multifile
	prolog:xref_open_source/2,
	prolog:xref_source_identifier/2,
	prolog:xref_source_file/3.

prolog:xref_open_source(File, Stream) :-
	rdf_is_resource(File),
	rdf_has(File, cpack:path, literal(Path)),
	rdf_has(File, cpack:inPack, Pack),
	(   rdf_has(File, cpack:mirrorRepository, Mirror),
	    rdf_has(Mirror, cpack:branch, literal(Branch))
	->  true
	;   Branch = master
	),
	cpack_our_mirror(Pack, BareGitDir),
	git_open_file(BareGitDir, Path, Branch, Stream).

prolog:xref_source_identifier(File, File) :-
	rdf_is_resource(File),
	rdfs_individual_of(File, cpack:'PrologFile').

%%	prolog:xref_source_file(+Spec, -File, +Options) is semidet.
%
%	True when File is the URI  of   a  file referenced by Spec. This
%	predicate hooks into xref_source_file/3, making  it possible for
%	the cross-referencer to analyse files in GIT repositories.

prolog:xref_source_file(Spec, File, _Options) :-
	rdf_is_resource(Spec),
	(   File = Spec
	;   prolog_file_type(Ext, prolog),
	    file_name_extension(Spec, Ext, File)
	),
	rdfs_individual_of(File, cpack:'File'), !.
prolog:xref_source_file(Spec, File, _Options) :-
	callable(Spec),
	Spec =.. [Package, Local],
	path_segments_atom(Local, Path),
	rdf_has(File, cpack:path, Path),
	rdf_has(File, cpack:inPack, Pack),
	rdf_has(Pack, cpack:packageName, literal(Package)), !.
