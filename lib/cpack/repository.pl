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
	  [ cpack_add_repository/3,	% +User, +GitRepo, +Options
	    cpack_update_package/2,	% +User, +Package
	    cpack_our_mirror/2,		% +Package, -MirrorDir
	    cpack_shortlog/3,		% +Package, -ShortLog, +Options
	    cpack_uri/3,		% +Type, +Object, -URI
	    git_log_data/3		% ?Field, ?Record, ?Data
	  ]).
:- use_module(library(lists)).
:- use_module(library(record)).
:- use_module(library(git)).
:- use_module(library(settings)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_turtle)).
:- use_module(library(semweb/rdf_label)).
:- use_module(library(filesex)).
:- use_module(library(http/http_wrapper)).
:- use_module(library(http/http_host)).
:- use_module(xref).

/** <module> Manage CPACK repositories

  - git clone --mirror URL
  - git fetch
*/

:- setting(cpack:mirrors, atom, 'cpack-mirrors',
	   'Directory for mirroring external repositories').
:- setting(git:host, atom, '',
	   'Public host for git:// access').

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
	;   git([clone, '--mirror', URL, BareGitPath], []),
	    rdf_assert(User, cpack:submitted, Graph, User)
	),
	update_metadata(BareGitPath, Graph, [user(User),cloned(URL)|Options]).

%%	cpack_update_package(+User, +Package) is det.
%
%	Update the given package.

cpack_update_package(User, Package) :-
	rdf_has(Package, cpack:clonedRepository, Cloned),
	rdf_has(Cloned,  cpack:gitURL, GitURL),
	(   rdf_has(Cloned, cpack:branch, literal(Branch))
	->  true
	;   Branch = master
	),
	cpack_update_repository(User, GitURL,
				[ branch(Branch),
				  allowed(true)
				]).


%%	cpack_update_repository(+User, +URL)
%
%	Update a package

cpack_update_repository(User, URL, Options) :-
	option(branch(Branch), Options, master),
	url_package(URL, PackageName),
	package_graph(PackageName, Graph),
	Package = Graph,
	update_allowed(User, Package, Options),
	file_name_extension(PackageName, git, BareGit),
	setting(cpack:mirrors, MirrorDir),
	directory_file_path(MirrorDir, BareGit, BareGitPath),
	git_hash(BareGitPath, Branch, Hash0),
	git([fetch, origin], [directory(BareGitPath)]),
	git_hash(BareGitPath, Branch, Hash1),
	print_message(informational, cpack(updated(Graph, Hash0, Hash1))),
	(   (   Hash1 \== Hash0
	    ;	option(update_metadata(always), Options, always)
	    )
	->  update_metadata(BareGitPath, Graph, [user(User),cloned(URL)|Options])
	;   true
	).

update_allowed(_, _, Options) :-
	option(allowed(true), Options), !.
update_allowed(User, Package, _) :-
	rdf_has(User, cpack:submitted, Package, User), !.
update_allowed(_, Package, _) :-
	permission_error(update, cpack, Package).


%%	update_metadata(+BareGitPath, +Graph, +Options) is det.
%
%	Update metadata for a repository

update_metadata(BareGitPath, Graph, Options) :-
	rdf_retractall(_,_,_,Graph),
	add_files(BareGitPath, Graph, Options),
	load_meta_data(BareGitPath, Graph, Options),
	update_decription(BareGitPath, Graph),
	add_timestamp(Graph),
	(   option(cloned(ClonedURL), Options)
	->  option(branch(Branch), Options, master),
	    rdf_bnode(Cloned),
	    rdf_assert(Graph, cpack:clonedRepository, Cloned, Graph),
	    rdf_assert(Cloned, rdf:type, cpack:'Repository', Graph),
	    rdf_assert(Cloned, cpack:gitURL, ClonedURL, Graph),
	    rdf_assert(Cloned, cpack:branch, literal(Branch), Graph),
	    git_hash(BareGitPath, Branch, Hash),
	    rdf_assert(Cloned, cpack:hash, literal(Hash), Graph)
	;   true
	),
	(   git_export(BareGitPath, MirroredURL)
	->  rdf_bnode(Mirror),
	    rdf_assert(Graph, cpack:mirrorRepository, Mirror, Graph),
	    rdf_assert(Mirror, rdf:type, cpack:'Repository', Graph),
	    rdf_assert(Mirror, cpack:gitURL, MirroredURL, Graph),
	    rdf_assert(Cloned, cpack:branch, literal(Branch), Graph),
	    rdf_assert(Mirror, cpack:hash, literal(Hash), Graph)
	;   true
	),
	xref_cpack(Graph).

add_timestamp(Graph) :-
	get_time(Now),
	format_time(atom(DateTime), '%FT%T%Oz', Now),
	rdf_assert(Graph, cpack:submittedDate,
		   literal(type(xsd:dateTime, DateTime)), Graph).

update_decription(BareGitPath, Graph) :-
	rdf_has(Graph, dcterms:title, Literal),
	literal_text(Literal, Title),
	directory_file_path(BareGitPath, description, DescFile),
	setup_call_cleanup(open(DescFile, write, Out),
			   format(Out, '~w~n', [Title]),
			   close(Out)).

%%	git_export(+BareGitPath, -MirroredURL) is det.
%
%	Make sure =|git-daemon-export-ok|= exists and deduce the URL for
%	cloning using =|git://|=
%
%	@tbd	Find the proper hostname if we have multiple.  I guess w

git_export(BareGitPath, MirroredURL) :-
	(   setting(git:host, Host),
	    Host \== ''
	->  GitHost = Host
	;   gethostname(GitHost)
	),
	absolute_file_name(BareGitPath, AbsGitPath),
	format(atom(MirroredURL), 'git://~w~w', [GitHost, AbsGitPath]),
	directory_file_path(BareGitPath, 'git-daemon-export-ok', ExportOK),
	(   exists_file(ExportOK)
	->  true
	;   setup_call_cleanup(open(ExportOK, write, Out),
			       true,
			       close(Out))
	).


%%	git_hash(+BareGitPath, +RevSpec, -Hash) is det.
%
%	Add a cpack:hash to the repository.

git_hash(BareGitPath, RevSpec, Hash) :-
	git_process_output(['rev-parse', RevSpec],
			   read_to_atom(Hash),
			   [directory(BareGitPath)]).

read_to_atom(Hash, In) :-
	read_line_to_codes(In, Line),
	atom_codes(Hash, Line).

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
	file_base_name(FileName, BaseName),
	file_base(FileName , BaseID),
	file_type(BaseName, Class),
	atomic_list_concat([Graph, /, FileName], File),
	rdf_assert(File, cpack:path, literal(FileName), Graph),
	rdf_assert(File, cpack:name, literal(BaseName), Graph),
	rdf_assert(File, cpack:base, literal(BaseID), Graph),
	rdf_assert(File, cpack:inPack, Graph, Graph),
	rdf_assert(File, rdf:type, Class, Graph),
	read_line_to_codes(In, Line2),
	read_files(Line2, Graph, In).

file_base(Path, Base) :-
	file_base_name(Path, File),
	file_name_extension(Base, _Ext, File).


:- rdf_meta
	file_type(+, r).

file_type(File, cpack:'PrologFile') :-
	file_name_extension(_Base, Ext, File),
	prolog_file_type(Ext, prolog), !.
file_type(_, cpack:'File').


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

%%	cpack_uri(+Type, +Identifier, -URI) is det.
%
%	Create a persistent URI for Identifier of the given Type.

cpack_uri(Type, Name, URI) :-
	(   type_root(Type, Root)
	->  true
	;   domain_error(uri_type, Type)
	),
	http_current_request(Request),
	http_current_host(Request, Host, Port,
			  [ global(true)
			  ]),
	format(atom(URI), 'http://~w:~w/cpack/~w~w',
	       [ Host, Port, Root, Name ]).

type_root(package,  'packs/').
type_root(file_ref, 'file_ref/').
type_root(graph,    'graph/').


package_graph(Package, Graph) :-
	cpack_uri(package, Package, Graph).

url_package(URL, Package) :-
	file_base_name(URL, Base),
	(   atom_concat(Package0, '.git', Base)
	->  Package = Package0
	;   Package = Base
	).

		 /*******************************
		 *	     FETCH INFO		*
		 *******************************/

%%	cpack_our_mirror(+Pack, -Dir) is det.
%
%	Dir is the directory holding the bare git repository for Pack.

cpack_our_mirror(Pack, BareGitPath) :-
	rdf_has(Pack, cpack:packageName, literal(PackageName)),
	file_name_extension(PackageName, git, BareGit),
	setting(cpack:mirrors, MirrorDir),
	directory_file_path(MirrorDir, BareGit, BareGitPath).

%%	cpack_shortlog(+Pack, -ShortLog, Options) is det.
%
%	Fetch information like the  GitWeb   change  overview. Processed
%	options:
%
%	    * limit(+Count)
%	    Maximum number of commits to show (default is 10)
%	    * path(+Path)
%	    Only show commits that affect Path
%
%	@param ShortLog is a list of =git_log= records.

:- record
	git_log(hash:atom,
		date:atom,
		committer:atom,
		title:atom,
		decoration:atom).

cpack_shortlog(Pack, ShortLog, Options) :-
	option(limit(Limit), Options, 10),
	(   option(path(Path), Options)
	->  Extra = ['--', Path]
	;   Extra = []
	),
	cpack_our_mirror(Pack, BareGitPath),
	git_process_output([ log, '-n', Limit,
			     '--format=%H%x00%cr%x00%cn%x00%s%x00%d%x00'
			   | Extra
			   ],
			   read_shortlog(ShortLog),
			   [directory(BareGitPath)]).


read_shortlog(ShortLog, In) :-
	read_line_to_codes(In, Line0),
	read_shortlog(Line0, In, ShortLog).

read_shortlog(end_of_file, _, []).
read_shortlog(Line, In, [H|T]) :-
	phrase(parse_shortlog(H), Line),
	read_line_to_codes(In, Line1),
	read_shortlog(Line1, In, T).

parse_shortlog(Record) -->
	{ default_git_log(Record) },
	field(Record, hash),
	field(Record, date),
	field(Record, committer),
	field(Record, title),
	field(Record, decoration).

field(Record, Field) -->
	to_nul_s(Codes),
	{ atom_codes(Value, Codes),
	  git_log_data(Field, Record, Value)
	}.

to_nul_s([]) --> [0], !.
to_nul_s([H|T]) --> [H], to_nul_s(T).


		 /*******************************
		 *	      MESSAGES		*
		 *******************************/

:- multifile prolog:message//1.

prolog:message(cpack(updated(Graph, Hash0, Hash1))) -->
	package_name(Graph),
	(   { Hash0 == Hash1 }
	->  [ ' no change'-[] ]
	;   { sub_atom(Hash0, 0, 6, _, Short0),
	      sub_atom(Hash1, 0, 6, _, Short1)
	    },
	    [ ' g~w..g~w'-[Short0,Short1] ]
	).

package_name(Graph) -->
	{ rdf_has(Graph, cpack:name, Literal),
	  literal_text(Literal, Text)
	}, !,
	[ '~w'-[Text] ].
package_name(Graph) -->
	[ '~p'-[Graph] ].
