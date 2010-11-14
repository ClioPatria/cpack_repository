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
	    cpack_conflicts/3		% +Package, -Package, -Why
	  ]).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(repository).

/** <module> Query the CPACK dependency graph

This module queries the RDF graph produced by xref.pl to compute
high-level dependencies between objects.
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
%	True when Package and Conflict are in conflict.

cpack_conflicts(_Package, _Conflict, _Why).
