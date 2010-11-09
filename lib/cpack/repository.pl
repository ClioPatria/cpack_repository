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

:- module(cpack_repository,
	  [ cpack_add_repository/3	% +User, +GitRepo, +Options
	  ]).
:- use_module(library(lists)).
:- use_module(library(git)).
:- use_module(library(settings)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_turtle)).
:- use_module(library(filesex)).
:- use_module(library(http/http_wrapper)).
:- use_module(library(http/http_host)).


/** <module> Manage CPACK repositories

  - git clone --mirror URL
  - git fetch
*/

:- setting(cpack:mirrors, atom, 'cpack-mirrors',
	   'Directory for mirroring external repositories').

%%	cpack_add_repository(+User, +URL, +Options)
%
%	Add a git repository from URL. Fetch  the meta-data into a graph
%	named =|cpack:<package>|= and add a   provenance  statement that
%	indicates the creator of the graph.  Options include:
%
%	    * branch(Branch)
%	    Add the given branch rather than the master

cpack_add_repository(User, URL, Options) :-
	url_package(URL, Package),
	package_graph(Package, Graph),
	file_name_extension(Package, git, BareGit),
	setting(cpack:mirrors, MirrorDir),
	make_directory_path(MirrorDir),
	directory_file_path(MirrorDir, BareGit, BareGitPath),
	(   exists_directory(BareGitPath)
	->  cpack_update_repository(User, URL, Options)
	;   git([clone, '--mirror', URL, BareGitPath], [])
	),
	update_metadata(BareGitPath, Graph, [user(User)|Options]).

%%	cpack_update_repository(+User, +URL)
%
%	Update a package

cpack_update_repository(User, URL, Options) :-
	url_package(URL, PackageName),
	package_graph(PackageName, Graph),
	Package = Graph,
	update_allowed(User, Package),
	file_name_extension(PackageName, git, BareGit),
	setting(cpack:mirrors, MirrorDir),
	directory_file_path(MirrorDir, BareGit, BareGitPath),
	git([fetch], [directory(BareGitPath)]),
	update_metadata(BareGitPath, Graph, [user(User)|Options]).

update_allowed(User, Package) :-
	rdf_has(Package, cpack:submittedBy, User, Package), !.
update_allowed(_, Package) :-
	permission_error(update, cpack, Package).


%%	update_metadata(+BareGitPath, +Graph, +Options) is det.
%
%	Update metadata for a repository

update_metadata(BareGitPath, Graph, Options) :-
	rdf_retractall(_,_,_,Graph),
	add_files(BareGitPath, Graph, Options),
	load_meta_data(BareGitPath, Graph, Options),
	add_timestamp(Graph),
	(   option(user(User), Options)
	->  rdf_assert(Graph, cpack:submittedBy, User)
	;   true
	).

add_timestamp(Graph) :-
	get_time(Now),
	format_time(atom(DateTime), '%FT%T%Oz', Now),
	rdf_assert(Graph, cpack:submittedDate,
		   literal(type(xsd:dateTime, DateTime)), Graph).

%%	add_files(+BareGitPath, +Graph, +Options) is det.
%
%	Add objects for the files in BareGitPath to Graph.

add_files(BareGitPath, Graph, Options) :-
	option(branch(Branch), Options, master),
	git_process_output(['ls-tree', '-r', '--name-only', Branch],
			   read_files(Graph),
			   [directory(BareGitPath)]).

read_files(Graph, In) :-
	read_line_to_codes(In, Line1),
	read_files(Line1, Graph, In).

read_files(end_of_file, _, _) :- !.
read_files(Line, Graph, In) :-
	atom_codes(FileName, Line),
	atomic_list_concat([Graph, /, FileName], File),
	file_base_name(FileName, BaseName),
	rdf_assert(File, cpack:path, literal(FileName), Graph),
	rdf_assert(File, cpack:name, literal(BaseName), Graph),
	rdf_assert(File, cpack:inPack, Graph, Graph),
	rdf_assert(File, rdf:type, cpack:'File', Graph),
	read_line_to_codes(In, Line2),
	read_files(Line2, Graph, In).


%%	load_meta_data(+BareGitPath, +Graph, +Options) is det.
%
%	Load the meta-data from the GIT  repository BareGitPath into the
%	named graph Graph.

load_meta_data(BareGitPath, Graph, Options) :-
	option(branch(Branch), Options, master),
	url_package(BareGitPath, Package),
	format(atom(File), '~w:rdf/cpack/~w.ttl', [Branch, Package]),
	git_process_output([show, File],
			   rdf_load_git_stream(Graph, turtle),
			   [directory(BareGitPath)]).

rdf_load_git_stream(Graph, Format, In) :-
	set_stream(In, file_name(Graph)),
	rdf_read_turtle(stream(In),
			RDF,
			[ base_uri(Graph),
			  format(Format)
			]),
	forall(member(rdf(S,P,O), RDF),
	       rdf_assert(S,P,O,Graph)).

package_graph(Package, Graph) :-
	http_current_request(Request),
	http_current_host(Request, Host, Port,
			  [ global(true)
			  ]),
	format(atom(Graph), 'http://~w:~w/cpack/~w',
	       [ Host, Port, Package ]).


url_package(URL, Package) :-
	file_base_name(URL, Base),
	(   atom_concat(Package0, '.git', Base)
	->  Package = Package0
	;   Package = Base
	).

