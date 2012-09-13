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
	    cpack_refresh_metadata/0,
	    cpack_refresh_metadata/1,	% +MirrorGit
	    cpack_our_mirror/2,		% +Package, -MirrorDir
	    cpack_clone_server/3,	% +User, +Server, +Options)
	    cpack_uri/3,		% +Type, +Object, -URI
	    cpack_log/3,		% +Package, -ShortLog, +Options
	    cpack_show/4,		% +Package, +Hash, -Data, +Options
	    commit_data/3		% ?Field, ?Record, ?Data
	  ]).
:- use_module(library(lists)).
:- use_module(library(record)).
:- use_module(library(git)).
:- use_module(library(uri)).
:- use_module(library(settings)).
:- use_module(library(option)).
:- use_module(library(error)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_turtle)).
:- use_module(library(semweb/rdf_label)).
:- use_module(library(semweb/rdf_foaf)).
:- use_module(library(filesex)).
:- use_module(library(http/http_wrapper)).
:- use_module(library(http/http_host)).
:- use_module(library(http/http_path)).
:- use_module(library(http/http_open)).
:- use_module(library(dcg/basics)).
:- use_module(user(user_db)).
:- use_module(library(foaf_schema)).
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
	git_check_url(URL),
	url_package(URL, Package),
	package_graph(Package, Graph),
	file_name_extension(Package, git, BareGit),
	setting(cpack:mirrors, MirrorDir),
	make_directory_path(MirrorDir),
	directory_file_path(MirrorDir, BareGit, BareGitPath),
	(   exists_directory(BareGitPath)
	->  cpack_update_repository(User, URL, Options)
	;   git([clone, '--mirror', URL, BareGitPath], []),
	    rdf_assert(User, cpack:submitted, Graph, User),
	    update_metadata(BareGitPath, Graph,
			    [user(User),cloned(URL)|Options])
	).

%%	git_check_url(+URL) is det.
%
%	Verify that the URL  is  either   git://,  http://  or https://.
%	Notaby, avoid SSH URLs that would make the ClioPatria server try
%	ssh connections that would normally not be allowed.
%
%	@error(permission_error(add_repository_from, url, URL)

git_check_url(URL) :-
	uri_components(URL, Components),
	uri_data(scheme, Components, Scheme),
	safe_scheme(Scheme), !.
git_check_url(URL) :-
	permission_error(add_repository_from,
			 url,
			 URL).

safe_scheme(git).
safe_scheme(http).
safe_scheme(https).


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
	->  update_metadata(BareGitPath, Graph,
			    [user(User),cloned(URL)|Options])
	;   true
	).

update_allowed(_, _, Options) :-
	option(allowed(true), Options), !.
update_allowed(User, Package, _) :-
	rdf(User, cpack:submitted, Package, User), !.
update_allowed(_, _, _) :-
	catch(authorized(admin(cpack)), _, fail).
update_allowed(_, Package, _) :-
	permission_error(update, cpack, Package).


%%	update_metadata(+BareGitPath, +Graph, +Options) is det.
%
%	Update metadata for a repository

update_metadata(BareGitPath, Graph, Options) :-
	rdf_retractall(_,_,_,Graph),
	add_files(BareGitPath, Graph, Options),
	catch(load_meta_data(BareGitPath, Graph, Options), E,
	      print_message(error, E)),
	update_decription(BareGitPath, Graph),
	add_timestamp(Graph, Options),
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
	foaf_merge(_),
	set_prolog_flag(message_ide, false),	% do not expose messages
	xref_cpack(Graph).

add_timestamp(Graph, Options) :-
	option(submitted_date(DateTime), Options), !,
	rdf_assert(Graph, cpack:submittedDate, DateTime, Graph).
add_timestamp(Graph, _Options) :-
	get_time(Now),
	format_time(atom(DateTime), '%FT%T%Oz', Now),
	rdf_assert(Graph, cpack:submittedDate,
		   literal(type(xsd:dateTime, DateTime)), Graph).

update_decription(BareGitPath, Graph) :-
	rdf_has(Graph, dcterms:title, Literal), !,
	literal_text(Literal, Title),
	directory_file_path(BareGitPath, description, DescFile),
	setup_call_cleanup(open(DescFile, write, Out),
			   format(Out, '~w~n', [Title]),
			   close(Out)).
update_decription(_, _).

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
	git_process_output(['ls-tree', '-lr', Branch],
			   read_files(Graph),
			   [directory(BareGitPath)]).

read_files(Graph, In) :-
	read_line_to_codes(In, Line1),
	read_files(Line1, Graph, In).

read_files(end_of_file, _, _) :- !.
read_files(Line, Graph, In) :-
	phrase(file_l(_Mode, _Type, _Hash, Size, FileName), Line),
	atom_number(SizeAtom, Size),
	file_base_name(FileName, BaseName),
	file_base(FileName , BaseID),
	file_type(BaseName, Class),
	atomic_list_concat([Graph, /, FileName], File),
	rdf_assert(File, cpack:path, literal(FileName), Graph),
	rdf_assert(File, cpack:name, literal(BaseName), Graph),
	rdf_assert(File, cpack:base, literal(BaseID), Graph),
	rdf_assert(File, cpack:size, literal(type(xsd:integer, SizeAtom)), Graph),
	rdf_assert(File, cpack:inPack, Graph, Graph),
	rdf_assert(File, rdf:type, Class, Graph),
	read_line_to_codes(In, Line2),
	read_files(Line2, Graph, In).

file_base(Path, Base) :-
	file_base_name(Path, File),
	file_name_extension(Base, _Ext, File).

file_l(Mode, Type, Hash, Size, Name) -->
	string_without(" ", MCodes), blanks,
	string_without(" ", TCodes), blanks,
	string_without(" ", HCodes), blanks,
	integer(Size), blanks,
	string_without(" \n", NCodes), blanks,
	{ number_codes(Mode, [0'0, 0'o|MCodes]),
	  atom_codes(Type, TCodes),
	  atom_codes(Hash, HCodes),
	  atom_codes(Name, NCodes)
	}.


:- rdf_meta
	file_type(+, r).

file_type(File, cpack:'PrologFile') :-
	file_name_extension(_Base, Ext, File),
	user:prolog_file_type(Ext, prolog), !.
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


		 /*******************************
		 *	UPDATE FROM MIRROR	*
		 *******************************/

%%	cpack_refresh_metadata(+BareGitPath) is det.
%
%	Regenerate the metadata associated  with   BareGitPath  from the
%	plain (mirrored) git repository.

cpack_refresh_metadata(BareGitPath) :-
	file_base_name(BareGitPath, BareGit),
	file_name_extension(PackageName, git, BareGit),
	package_graph(PackageName, Graph),
	git_remote_url(origin, Origin, [directory(BareGitPath)]),
	git_default_branch(DefBranch, [directory(BareGitPath)]),
	(   rdf_has(Graph, cpack:submittedDate, Date)
	->  Extra = [submitted_date(Date)]
	;   Extra = []
	),
	update_metadata(BareGitPath, Graph,
			[ cloned(Origin),
			  branch(DefBranch)
			| Extra
			]).

%%	cpack_refresh_metadata
%
%	Rebuild all (xref) metadata for all  packages from scratch. This
%	is intended to deal with changes   to the metadata formats, lost
%	GIT mirrors, etc.

cpack_refresh_metadata :-
	setting(cpack:mirrors, MirrorDir),
	directory_file_path(MirrorDir, '*.git', Pattern),
	expand_file_name(Pattern, BareGits),
	clear_xref_graphs,
	maplist(cpack_refresh_metadata, BareGits).

clear_xref_graphs :-
	clear_xref_graph(prolog),
	clear_xref_graph(cliopatria),
	clear_xref_graph('file-references').

clear_xref_graph(Name) :-
	cpack_uri(graph, Name, URI),
	rdf_retractall(_,_,_,URI).


		 /*******************************
		 *	  CLONE A SERVER	*
		 *******************************/

%%	cpack_clone_server(+User, +Server, +Options)
%
%	Clone all packages from Server.

cpack_clone_server(User, Server, _Options) :-
	atom_concat(Server, '/cpack/clone_data', CloneURL),
	http_prolog_data(CloneURL, Terms),
	forall(member(PackInfo, Terms),
	       clone_package(User, PackInfo)).

%%	clone_package(+User, +PackInfo)
%
%	Clone package from another server.

clone_package(User, cpack(Name, Options)) :-
	print_message(informational, cpack(clone(Name, Options))),
	option(pack_repository(git(GitURL, GitOptions)), Options),
	cpack_add_repository(User, GitURL, GitOptions).

%%	http_prolog_data(+URL, -Term) is det.
%
%	Read a Prolog term from URL.

http_prolog_data(URL, Terms) :-
	setup_call_cleanup(http_open(URL, In, []),
			   read_stream_to_terms(In, Terms),
			   close(In)).

read_stream_to_terms(In, Terms) :-
	read_term(In, Term0, []),
	read_stream_to_terms(Term0, In, Terms).

read_stream_to_terms(end_of_file, _, []) :- !.
read_stream_to_terms(Term, In, [Term|T]) :-
	read_term(In, Term1, []),
	read_stream_to_terms(Term1, In, T).



		 /*******************************
		 *	       URIs		*
		 *******************************/

%%	cpack_uri(+Type, +Identifier, -URI) is det.
%
%	Create a persistent URI for Identifier of the given Type.

cpack_uri(Type, Name, URI) :-
	(   type_root(Type, RootSpec)
	->  http_absolute_location(RootSpec, Root0, []),
	    ensure_slash(Root0, Root)
	;   domain_error(uri_type, Type)
	),
	http_current_request(Request),
	http_current_host(Request, Host, Port,
			  [ global(true)
			  ]),
	uri_authority_data(host, AD, Host),
	(   Port =:= 80
	->  true
	;   uri_authority_data(port, AD, Port)
	),
	uri_authority_components(Authority, AD),
	uri_data(scheme, Data, http),
	uri_data(authority, Data, Authority),
	uri_data(path, Data, Root),
	uri_components(Start, Data),
	atom_concat(Start, Name, URI).

ensure_slash(Root0, Root) :-
	(   sub_atom(Root0, _, _, 0, /)
	->  Root = Root0
	;   atom_concat(Root0, /, Root)
	).

type_root(package,    root(packs)).
type_root(pack,	      root(cpack)).		% Sync with api(cpack)!
type_root(file_ref,   root(file_ref)).
type_root(graph,      root(graph)).
type_root(prolog,     root(prolog)).
type_root(cliopatria, root(cliopatria)).

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


		 /*******************************
		 *	  GIT OPERATIONS	*
		 *******************************/

%%	cpack_log(+Pack, -ShortLog, Options) is det.
%
%	Fetch information like the  GitWeb   change  overview. Processed
%	options:
%
%	    * limit(+Count)
%	    Maximum number of commits to show (default is 10)
%	    * git_path(+Path)
%	    Only show commits that affect Path
%
%	@param	ShortLog is a list of =git_log= records. See
%		git_shortlog/3.

cpack_log(Pack, ShortLog, Options) :-
	cpack_our_mirror(Pack, BareGitPath),
	git_shortlog(BareGitPath, ShortLog, Options).


%%	cpack_show(+Pack, +Hash, -Commit) is det.
%
%	Fetch info from a GIT commit.  Options processed:
%
%	  * diff(Diff)
%	  GIT option on how to format diffs.  E.g. =stat=
%	  * max_lines(Count)
%	  Truncate the body at Count lines.
%
%	@param	Commit is a term git_commit(...)-Body.  Body is currently
%		a list of lines, each line represented as a list of
%		codes.

:- record
	commit(tree_hash:atom,
	       parent_hashes:list,
	       author_name:atom,
	       author_date:atom,
	       committer_name:atom,
	       committer_date:atom,
	       subject:atom).

cpack_show(Pack, Hash, Commit, Options) :-
	cpack_our_mirror(Pack, BareGitPath),
	git_format_string(commit, Fields, Format),
	option(diff(Diff), Options, patch),
	diff_arg(Diff, DiffArg),
	git_process_output([ show, DiffArg, Hash, Format ],
			   read_commit(Fields, Commit, Options),
			   [directory(BareGitPath)]).

diff_arg(patch, '-p').
diff_arg(stat, '--stat').

read_commit(Fields, Data-Body, Options, In) :-
	read_line_to_codes(In, Line1),
	record_from_line(commit, Fields, Line1, Data),
	read_line_to_codes(In, Line2),
	Line2 == [],
	option(max_lines(Max), Options, -1),
	read_n_lines(In, Max, Body).

read_n_lines(In, Max, Lines) :-
	read_line_to_codes(In, Line1),
	read_n_lines(Line1, Max, In, Lines).

read_n_lines(end_of_file, _, _, []) :- !.
read_n_lines(_, 0, In, []) :- !,
	setup_call_cleanup(open_null_stream(Out),
			   copy_stream_data(In, Out),
			   close(Out)).
read_n_lines(Line, Max0, In, [Line|More]) :-
	read_line_to_codes(In, Line2),
	Max is Max0-1,
	read_n_lines(Line2, Max, In, More).


record_from_line(RecordName, Fields, Line, Record) :-
	phrase(fields_from_line(Fields, Values), Line),
	Record =.. [RecordName|Values].

fields_from_line([], []) --> [].
fields_from_line([F|FT], [V|VT]) -->
	to_nul_s(Codes),
	{ field_to_prolog(F, Codes, V) },
	fields_from_line(FT, VT).

to_nul_s([]) --> [0], !.
to_nul_s([H|T]) --> [H], to_nul_s(T).

field_to_prolog(ref_names, Line, List) :-
	phrase(ref_names(List), Line), !.
field_to_prolog(_, Line, Atom) :-
	atom_codes(Atom, Line).

ref_names([]) --> [].
ref_names(List) -->
	blanks, "(", ref_name_list(List), ")".

ref_name_list([H|T]) -->
	string_without(",)", Codes),
	{ atom_codes(H, Codes) },
	(   ",", blanks
	->  ref_name_list(T)
	;   {T=[]}
	).

%%	git_format_string(+Record, -FieldNames, -Format)
%
%	If Record is a record with  fields   whose  names  match the GIT
%	format field-names, Format is a  git =|--format=|= argument with
%	the appropriate format-specifiers,  terminated   by  %x00, which
%	causes the actual field to be 0-terminated.

:- meta_predicate
	git_format_string(:, -, -).

git_format_string(M:RecordName, Fields, Format) :-
	current_record(RecordName, M:Term),
	findall(F, record_field(Term, F), Fields),
	maplist(git_field_format, Fields, Formats),
	atomic_list_concat(['--format='|Formats], Format).

record_field(Term, Name) :-
	arg(_, Term, Field),
	field_name(Field, Name).

field_name(Name:_Type=_Default, Name) :- !.
field_name(Name:_Type, Name) :- !.
field_name(Name=_Default, Name) :- !.
field_name(Name, Name).

git_field_format(Field, Fmt) :-
	(   git_format(NoPercent, Field)
	->  atomic_list_concat(['%', NoPercent, '%x00'], Fmt)
	;   existence_error(git_format, Field)
	).

git_format('H', commit_hash).
git_format('h', abbreviated_commit_hash).
git_format('T', tree_hash).
git_format('t', abbreviated_tree_hash).
git_format('P', parent_hashes).
git_format('p', abbreviated_parent_hashes).

git_format('an', author_name).
git_format('aN', author_name_mailcap).
git_format('ae', author_email).
git_format('aE', author_email_mailcap).
git_format('ad', author_date).
git_format('aD', author_date_rfc2822).
git_format('ar', author_date_relative).
git_format('at', author_date_unix).
git_format('ai', author_date_iso8601).

git_format('cn', committer_name).
git_format('cN', committer_name_mailcap).
git_format('ce', committer_email).
git_format('cE', committer_email_mailcap).
git_format('cd', committer_date).
git_format('cD', committer_date_rfc2822).
git_format('cr', committer_date_relative).
git_format('ct', committer_date_unix).
git_format('ci', committer_date_iso8601).

git_format('d', ref_names).		% git log?
git_format('e', encoding).		% git log?

git_format('s', subject).
git_format('f', subject_sanitized).
git_format('b', body).
git_format('N', notes).

git_format('gD', reflog_selector).
git_format('gd', shortened_reflog_selector).
git_format('gs', reflog_subject).


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
prolog:message(cpack(clone(Name, _Options))) -->
	[ 'Cloning CPACK ~w ...'-[Name] ].

package_name(Graph) -->
	{ rdf_has(Graph, cpack:name, Literal),
	  literal_text(Literal, Text)
	}, !,
	[ '~w'-[Text] ].
package_name(Graph) -->
	[ '~p'-[Graph] ].
