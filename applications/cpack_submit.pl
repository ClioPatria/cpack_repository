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

:- module(cpack_submit, []).
:- include(bundle(html_page)).
:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(git)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(semweb/rdf_label)).
:- use_module(user(user_db)).
:- use_module(library(cpack/repository)).
:- use_module(library(cpack/dependency)).
:- use_module(components(messages)).
:- use_module(components(label)).
:- use_module(components(cpack)).

http:location(cpack_api,  api(cpack),  []).

:- http_handler(cpack(submit),		     cpack_submit_form,		 []).
:- http_handler(cpack(clone_server),	     cpack_clone_server_form,	 []).
:- http_handler(cpack(list_packages),	     cpack_list_packages,	 []).
:- http_handler(cpack(my_packages),	     cpack_my_packages,		 []).
:- http_handler(cpack(update_my_packages),   cpack_update_my_packages,	 []).
:- http_handler(cpack_api(submit),	     cpack_submit,		 []).
:- http_handler(cpack_api(resubmit),	     cpack_resubmit,		 []).
:- http_handler(cpack_api(clone_server),     cpack_clone_server,	 []).
:- http_handler(cpack_api(refresh_metadata), cpack_refresh_metadata_api, []).
:- http_handler(cpack(show_file),	     cpack_show_file,		 []).
:- http_handler(cpack(git_show),	     git_show,			 []).

/** <module> User interaction to manage CPACKS
*/

%%	cpack_submit_form(+Request)
%
%	HTTP handler that emits a form to   submit a git repository as a
%	CPACK.

cpack_submit_form(_Request) :-
	authorized(write(cpack, _)),
	reply_html_page(cliopatria(cpack),
			[ title('Submit repository to CPACK')
			],
			[ h1('Submit repository to CPACK'),
			  \explain_submit,
			  form([ action(location_by_id(cpack_submit))
			       ],
			       table(class(form),
				     [ \form_input('GIT repository:',
						   input([ name(giturl),
							   size(50)
							 ])),
				       \form_input('branch:',
						   input([ name(branch),
							   value(master),
							   size(50)
							 ])),
				       \form_submit('Register')
				     ]))
			]).

explain_submit -->
	html(p([ 'Please enter a valid GIT URL from which the CPACK manager ',
		 'can clone the package.  The URL must be a git://, http:// or ',
		 'https:// URL.  Notably SSH URLs are not allowed.'
	       ])).


%%	cpack_submit(+Request)
%
%	HTTP API to add a new GIT repository  as a pack. This clones the
%	repository and loads the metadata into the RDF store.

cpack_submit(Request) :-
	logged_on(User),
	http_parameters(Request,
			[ giturl(GitURL,
				 [ description('URL of GIT repository')
				 ]),
			  branch(Branch,
				 [ default(master),
				   description('Branch in the repo')
				 ])
			]),
	authorized(write(cpack, GitURL)),
	user_property(User, url(UserURL)),
	call_showing_messages(cpack_add_repository(UserURL, GitURL,
						   [ branch(Branch)
						   ]),
			      []).


%%	cpack_resubmit(+Request)
%
%	Resubmit an already submitted package: run   a =|git pull|= from
%	the cloned repository and recompute the  meta-data. This is very
%	similar to cpack_submit/1, but the information is extracted from
%	the existing package resource.

cpack_resubmit(Request) :-
	logged_on(User),
	http_parameters(Request,
			[ pack(Pack,
			       [ description('URI of the CPACK to update')
			       ]),
			  giturl(GitURL,
				 [ optional(true),
				   description('GIT URL to pull from')
				 ]),
			  branch(Branch,
				 [ optional(true),
				   description('Branch to update from')
				 ]),
			  confirm(Confirm,
				  [ boolean,
				    default(true),
				    description('Present confirmation screen')
				  ]),
			  return_to(ReturnTo,
				    [ optional(true),
				      description('Return link')
				    ])
			]),
	authorized(write(cpack, Pack)),
	(   nonvar(GitURL)
	->  true
	;   rdf_has(Pack, cpack:clonedRepository, GitRepo),
	    rdf_has(GitRepo, cpack:gitURL, GitURL)
	->  true
	;   GitURL = '<invalid>'
	),
	user_property(User, url(UserURL)),
	(   var(ReturnTo)
	->  MsgOptions = []
	;   MsgOptions = [return_to(ReturnTo)]
	),
	findall(O, submit_option(User, Pack, Branch, O), RepoOptions),
	(   Confirm == true
	->  reply_html_page(cliopatria(cpack),
			    title('Pull new version'),
			    \pull_version_form(Pack, GitURL, RepoOptions, MsgOptions))
	;   call_showing_messages(cpack_add_repository(UserURL, GitURL, RepoOptions),
				  MsgOptions)
	).

submit_option(_User, Pack, Branch, branch(Branch)) :-
	(   var(Branch)
	->  rdf_has(Pack, cpack:clonedRepository, GitRepo),
	    rdf_has(GitRepo, cpack:branch, literal(Branch))
	;   true
	).
submit_option(User, _Pack, _Branch, allowed(true)) :-
	catch(check_permission(User, admin(cpack)), _, fail).


%%	pull_version_form(+Pack, +GitURL, +RepoOptions, +MsgOptions)// is det.
%
%	Present a form to confirm for pulling a new version.

pull_version_form(Pack, GitURL, RepoOptions, MsgOptions) -->
	{ (   catch(git_remote_branches(GitURL, Branches), E,
		    (print_message(warning, E), fail))
	  ->  true
	  ;   Branches = [master],
	      option(branch(Branch), RepoOptions, master)
	  )
	},
	html([ h1('Pull new version'),
	       form(action(location_by_id(cpack_resubmit)),
		    [ \hidden(confirm, false),
		      \hidden(pack, Pack),
		      \pull_options(MsgOptions),
		      table(class(form),
			    [ \form_input('GIT repository:',
					  input([ name(giturl),
						  size(50),
						  value(GitURL)
						])),
			      \form_input('Branch:',
					  \select_branch(Branches, Branch)),
			      \form_submit('Pull new version')
			    ])
		    ])
	     ]).

pull_options(MsgOptions) -->
	{ option(return_to(Return), MsgOptions)
	}, !,
	hidden(return_to, Return).
pull_options(_) --> [].

select_branch(Branches, Default) -->
	html(select(name(branch), \list_branches(Branches, Default))).

list_branches([], _) --> [].
list_branches([H|T], H) --> !,
	html(option([value(H),selected], H)),
	list_branches(T, H).
list_branches([H|T], D) --> !,
	html(option(value(H), H)),
	list_branches(T, D).


%%	cpack_list_packages(+Request) is det.
%%	cpack_my_packages(+Request) is det.
%
%	List registered CPACK packages.   The  variant cpack_my_packages
%	lists packages for  the  currently  logged   on  user.  It  is a
%	separate handler to make it accessible from the menu.

cpack_list_packages(Request) :-
	list_packages(Request, []).

cpack_my_packages(Request) :-
	ensure_logged_on(User),
	user_property(User, url(UserURI)),
	list_packages(Request, [user(UserURI), update_all_link(true)]).

list_packages(Request, Options) :-
	http_parameters(Request,
			[ sort_by(By,
				  [ oneof([status,name,title,type,author,date]),
				    default(name),
				    description('Sort table by this key')
				  ])
			]),
	reply_html_page(cliopatria(cpack),
			title('CPACK packages'),
			[ div(class(cpack),
			      [ h1('CPACK packages'),
				\package_table([sort_by(By)|Options]),
				\update_all_link(Options)
			      ])
			]).

update_all_link(Options) -->
	{ option(update_all_link(true), Options),
	  http_link_to_id(cpack_update_my_packages, [], HREF)
	}, !,
	html(p([a(href(HREF), 'Update'), ' all my packages'])).
update_all_link(_) --> [].

%%	cpack_update_my_packages(+Request) is det.
%
%	Update all packages owned by the currently logged on user.

cpack_update_my_packages(_Request) :-
	logged_on(User),
	user_property(User, url(UserURI)),
	findall(Package, current_package(Package, [user(UserURI)]), Packages),
	forall(member(Package, Packages),
	       authorized(write(cpack, Package))),
	call_showing_messages(maplist(cpack_update_package(UserURI), Packages),
			      []).

%%	cpack_show_file(+Request) is det.
%
%	Show the content of File.

cpack_show_file(Request) :-
	http_parameters(Request,
			[ r(File,
			    [ description('URI of a file in a package')
			    ])
			]),
	rdf_has(File, cpack:inPack, Package),
	rdf_has(File, cpack:path, literal(Path)),
	cpack_our_mirror(Package, BareGitDir),
	rdf_has(Package, cpack:mirrorRepository, Mirror),
	rdf_has(Mirror, cpack:hash, literal(Hash)),
	setup_call_cleanup(git_open_file(BareGitDir, Path, Hash, In),
			   (   format('Content-type: text/plain~n~n'),
			       copy_stream_data(In, current_output)
			   ),
			   close(In)).

%%	git_show(+Request) is det.
%
%	HTTP handler to handle GIT requests.

git_show(Request) :-
	http_parameters(Request,
			[ a(_Action,
			    [ oneof([commit]),
			      description('Action to perform')
			    ]),
			  h(Hash,
			    [ description('Hash to work on')
			    ]),
			  r(Pack,
			    [ description('URI of a cpack:Package')
			    ]),
			  diff(Diff,
			       [ oneof([stat,patch]),
				 default(stat),
				 description('Diff-style for commit')
			       ])
			]),
	rdf_display_label(Pack, Label),
	reply_html_page(cliopatria(cpack),
			title('Commit info'),
			[ h1([Label, /, commit]),
			  \commit_info(Pack, Hash, [diff(Diff)])
			]).


%%	cpack_clone_server_form(+Request)
%
%	Provide a form to clone a CPACK server

cpack_clone_server_form(_Request) :-
	authorized(write(cpack, _)),
	setting(cpack:server, DefaultServer),
	reply_html_page(cliopatria(cpack),
			[ title('Clone a CPACK server')
			],
			[ h1('Clone a CPACK server'),
			  \explain_clone,
			  form([ action(location_by_id(cpack_clone_server))
			       ],
			       table(class(form),
				     [ \form_input('Server URL:',
						   input([ name(server),
							   size(50),
							   value(DefaultServer)
							 ])),
				       \form_submit('Clone server')
				     ]))
			]).

explain_clone -->
	html(p([ 'Clone all packages from the given server. ',
		 'This is intended to populate a server for trying new ',
		 'package configurations.'
	       ])).


%%	cpack_clone_server(+Request)
%
%	API handler to clone a server

cpack_clone_server(Request) :-
	logged_on(User),
	http_parameters(Request,
			[ server(ServerURL,
				 [ description('URL of server to clone')
				 ])
			]),
	authorized(write(cpack, clone)),
	user_property(User, url(UserURL)),
	call_showing_messages(cpack_clone_server(UserURL, ServerURL, []),
			      []).


%%	cpack_refresh_metadata_api(+Request)
%
%	API to reload all metadata.

cpack_refresh_metadata_api(Request) :-
	authorized(write(cpack, refresh_metadata)),
	http_parameters(Request,
			[ p(Pack, [optional(true)])
			]),
	(   var(Pack)
	->  call_showing_messages(cpack_refresh_metadata, [])
	;   file_name_extension(Pack, git, Dir),
	    directory_file_path('cpack-mirrors', Dir, FullDir),
	    call_showing_messages(cpack_refresh_metadata(FullDir), [])
	).


