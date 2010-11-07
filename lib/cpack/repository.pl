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
:- use_module(library(git)).
:- use_module(library(settings)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(filesex)).

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
%	indicates the creator of the graph.

cpack_add_repository(User, URL, _Options) :-
	url_package(URL, Package),
	package_graph(Package, Graph),
	file_name_extension(Package, git, BareGit),
	setting(cpack:mirrors, MirrorDir),
	make_directory_path(MirrorDir),
	directory_file_path(MirrorDir, BareGit, BareGitPath),
	(   exists_directory(BareGitPath)
	->  cpack_update_repository(User, URL)
	;   git([clone, '--mirror', URL, BareGitPath], [])
	),
	load_meta_data(BareGitPath, Graph),
	rdf_assert(Graph, cpack:owner, User, Graph).

%%	cpack_update_repository(+User, +URL)
%
%	Update a package

cpack_update_repository(User, URL) :-
	url_package(URL, Package),
	package_graph(Package, Graph),
	rdf(Graph, cpack:owner, User, Graph),
	file_name_extension(Package, git, BareGit),
	setting(cpack:mirrors, MirrorDir),
	directory_file_path(MirrorDir, BareGit, BareGitPath),
	git([fetch], [directory(BareGitPath)]),
	load_meta_data(BareGitPath, Graph).


load_meta_data(BareGitPath, Graph) :-
	url_package(BareGitPath, Package),
	format(atom(File), 'master:rdf/cpack/~w.ttl', Package),
	git_process_output([show, File],
			   load_meta_data(Graph, turtle),
			   [directory(BareGitPath)]).

load_meta_data(Graph, Format, In) :-
	rdf_load(stream(In),
		 [ graph(Graph),
		   format(Format)
		 ]).

package_graph(Package, Graph) :-
	atom_concat('cpack://', Package, Graph).

url_package(URL, Package) :-
	file_base_name(URL, Base),
	(   atom_concat(Package0, '.git', Base)
	->  Package = Package0
	;   Package = Base
	).

