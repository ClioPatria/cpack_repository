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
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(semweb/rdf_label)).
:- use_module(user(user_db)).
:- use_module(library(http/http_path)).
:- use_module(library(cpack/repository)).
:- use_module(components(messages)).
:- use_module(components(label)).
:- use_module(components(cpack)).

http:location(cpack_api,  api(cpack),  []).

:- http_handler(cpack(submit),		   cpack_submit_form,	     []).
:- http_handler(cpack(list_packages),	   cpack_list_packages,	     []).
:- http_handler(cpack(my_packages),	   cpack_my_packages,	     []).
:- http_handler(cpack(update_my_packages), cpack_update_my_packages, []).
:- http_handler(cpack_api(submit),	   cpack_submit,	     []).

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


%%	cpack_list_packages(+Request) is det.
%%	cpack_my_packages(+Request) is det.
%
%	List registered CPACK packages.   The  variant cpack_my_packages
%	lists packages for  the  currently  logged   on  user.  It  is a
%	separate handler to make it accessible from the menu.

cpack_list_packages(_Request) :-
	list_packages([]).

cpack_my_packages(_Request) :-
	ensure_logged_on(User),
	user_property(User, url(UserURI)),
	list_packages([user(UserURI), update_all_link(true)]).

list_packages(Options) :-
	findall(Package, current_package(Package, Options), Packages),
	reply_html_page(cliopatria(cpack),
			title('CPACK packages'),
			[ h1('CPACK packages'),
			  \package_table(Packages, []),
			  \update_all_link(Options)
			]).

current_package(Package, Options) :-
	(   option(user(User), Options)
	->  rdf_has(Package, cpack:submittedBy, User)
	;   true
	),
	rdfs_individual_of(Package, cpack:'Package').


package_table(Packages, Options) -->
	html(table(class(block),
		   [ tr([ th('Name'),
			  th('Title'),
			  th('Type'),
			  th('Submitter')
			])
		   | \package_rows(Packages, 1, Options)
		   ])).

package_rows([], _, []) --> [].
package_rows([H|T], Row, Options) -->
	odd_even_row(Row, Next, \package_row(H, Options)),
	package_rows(T, Next, Options).

package_row(Package, _Options) -->
	html([ td(\cpack_link(Package)),
	       td(\cpack_prop(Package, dcterms:title)),
	       td(\cpack_prop(Package, rdf:type)),
	       td(\cpack_prop(Package, cpack:submittedBy))
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
