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
	  [ xref_cpack/1,
	    xref_cpack_file/1
	  ]).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(git)).
:- use_module(library(prolog_xref)).
:- use_module(repository).

/** <module> Compute dependencies between CPACKs

*/

%%	xref_cpack(+Pack) is det.
%
%	Create cross-reference info for a complete pack.

xref_cpack(Pack) :-
	findall(File, pack_prolog_file(Pack, File), Files),
	forall(member(File, Files),
	       xref_cpack_file(File)).

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
	forall(file_property(File, P, O),
	       rdf_assert(File, P, O, Graph)).

file_property(File, cpack:module, literal(Module)) :-
	xref_module(File, Module).
file_property(File, cpack:exportsPredicate, literal(Pred)) :-
	xref_exported(File, Callable),
	functor(Callable, Name, Arity),
	format(atom(Pred), '~w/~w', [Name, Arity]).


		 /*******************************
		 *   HOOKS TO WORK ON GIT REPO	*
		 *******************************/

:- multifile
	prolog:xref_open_source/2.

prolog:xref_open_source(File, Stream) :-
	rdf_has(File, cpack:path, literal(Path)),
	rdf_has(File, cpack:inPack, Pack),
	(   rdf_has(File, cpack:mirrorRepository, Mirror),
	    rdf_has(Mirror, cpack:branch, literal(Branch))
	->  true
	;   Branch = master
	),
	cpack_our_mirror(Pack, BareGitDir),
	git_open_file(BareGitDir, Path, Branch, Stream).
