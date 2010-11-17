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

:- module(api_cpack,
	  [
	  ]).
:- use_module(library(cpack/dependency)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_label)).
:- use_module(library(http/http_dispatch)).

/** <module> CPACK API for installing packages

This module defines the server-end API for

    * Quick discovery of packages
    * Installation of packages

To *install* a package, we must discover

    * Its GIT repository and branch
    * Dependencies (package names)
    * Possible conflicts with packages that we already have and that
      may not come from the server.  We need to do two things:
	- Make sure none of the modules conflict
	- Make sure that the relative paths of the packages provide
	  the required predicates.

So, conflict detection requires us to  send   the  list  of files in the
package and for each file the  =module=,   the  list of FileRefs and the
list of predicates these must supply and the list of exports.

To *discover* a package, we will search for

    * The package name
    * Words in the package title and description
    * Files in the package (exact)
    * Exported predicates (exact)
*/

:- http_handler(root('cpack/'),	cpack_install_data, [prefix]).

%%	cpack_install_data(+Request)
%
%	Return installation info for installing Pack.  The data is
%	returned as a Prolog term.

cpack_install_data(Request) :-
	memberchk(path_info(PackName), Request),
	(   rdf_has(Pack, cpack:packageName, literal(PackName))
	->  (   catch(pack_install_data(Pack, Data), E, true)
	    ->  (   nonvar(E)
		->  client_error(E, Error),
		    reply(PackName, error(Error))
		;   reply(PackName, cpack(PackName, Data))
		)
	    ;	reply(PackName, fail)
	    )
	;   reply(PackName, no_pack(PackName))
	).

reply(PackName, Data) :-
	format('Content-type: application/x-prolog~n~n'),
	format('% Installation data for CPACK "~w"~n~n', [PackName]),
	format('~q.~n', [Data]).

client_error(URL, Name) :-
	rdf_is_resource(URL),
	(   rdf_has(URL, cpack:path, literal(Name))
	->  true
	;   rdf_has(URL, cpack:name, literal(Name))
	), !.
client_error(literal(Text), Text) :- !.
client_error(Term0, Term) :-
	Term0 =.. [H|List0],
	maplist(client_error, List0, List),
	Term =.. [H|List].




%%	pack_install_data(+Pack, -Data) is det.
%
%	Provides the information to install  Pack.   Data  is  a list of
%	packages. Each element  is  a   term  cpack(Name,  Options). The
%	packages are topologically sorted on their dependency.
%
%	The  option  list  for  each  package  may  hold  the  following
%	information:
%
%	    * title(Title)
%	    * license(License)
%	    * pack_repository(git(URL,Options))
%	    * author_repository(git(URL,Options))
%	    Options include:
%	        - branch(Branch)
%	        - hash(Hash)
%	        - tag(Tag)
%	    * files(ListOfFile)
%	    Each file is a term file(Path, Options), where options is
%	        - module(Module)

pack_install_data(Pack, Data) :-
	cpack_list(Pack, List),
	maplist(pack_info, List, Data).

pack_info(Pack, cpack(Name, Options)) :-
	rdf_has(Pack, cpack:packageName, literal(Name)),
	findall(O, pack_option(Pack, O), Options).

pack_option(Pack, title(Title)) :-
	rdf_has(Pack, dcterms:title, Literal),
	literal_text(Literal, Title).
pack_option(Pack, pack_repository(Git)) :-
	rdf_has(Pack, cpack:mirrorRepository, Mirror),
	rdf_git_repo(Mirror, Git).
pack_option(Pack, author_repository(Git)) :-
	rdf_has(Pack, cpack:clonedRepository, Mirror),
	rdf_git_repo(Mirror, Git).
pack_option(Pack, files(FileData)) :-
	findall(F, rdf_has(F, cpack:inPack, Pack), Files),
	maplist(file_info, Files, FileData).

rdf_git_repo(URI, git(GitURL,Options)) :-
	rdf_has(URI, cpack:gitURL, GitURL),
	findall(O, repo_info(URI,O), Options).

repo_info(URI, branch(Branch)) :-
	rdf_has(URI, cpack:branch, literal(Branch)).

file_info(URI, module(M)) :-
	rdf_has(URI, cpack:module, literal(M)).
