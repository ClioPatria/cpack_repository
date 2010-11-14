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

:- module(c_cpack,
	  [ cpack//2,			% +Pack, +Options
	    cpack_link//1,		% +Resource
	    cpack_prop//2,		% +Resource, +Prop
	    commit_info//3		% +Record, +Body, +Options
	  ]).
:- include(bundle(html_page)).
:- use_module(library(http/dcg_basics)).
:- use_module(library(cpack/repository)).
:- use_module(library(cpack/dependency)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(semweb/rdf_label)).
:- use_module(components(label)).
:- use_module(cliopatria(hooks)).
:- use_module('cpack/graphs').

/** <module> CPACK HTML components

This module defines vizualisation  primitives   for  CPACK resources. It
also hooks the local-view  of  ClioPatria   to  provide  nicer pages for
instances of CPACK objects such as cpack:Package.

@tbd	Use PlDoc Wiki for rendering the description.  Should that be
	yet another package?
*/

%%	cliopatria:list_resource(+Pack)//
%
%	Hook the ClioPatria local view page   to  create a more sensible
%	represention of a package.

cliopatria:list_resource(Pack) -->
	{ rdfs_individual_of(Pack, cpack:'Package') },
	cpack(Pack, []).
cliopatria:list_resource(Pack) -->
	{ rdfs_individual_of(Pack, cpack:'File') },
	cpack_file(Pack, []).


		 /*******************************
		 *	      PACKAGE		*
		 *******************************/

%%	cpack(+Pack, +Options)// is det.
%
%	Display information about Pack.

cpack(Pack, _Options) -->
	{ rdf_has(Pack, cpack:name, literal(Name))
	},
	html_requires(css('cpack.css')),
	html(div(class(cpack),
		 [ h2(['Package "', Name, '" -- ',
		       \cpack_prop(Pack, dcterms:title)]),
		   table(class(infobox),
			 [ \p_row(Pack, rdf:type),
			   \p_row(Pack, cpack:author),
			   \p_row(Pack, cpack:submittedBy),
			   \p_row(Pack, cpack:submittedDate),
			   \p_row(Pack, cpack:requires),
			   \p_row(Pack, cpack:clonedRepository),
			   \p_row(Pack, cpack:mirrorRepository)
			 ]),
		   br([class('after-ptable')]),
		   div(class(description),
		       \cpack_prop(Pack, cpack:description)),
		   br(clear(all)),
		   \git_shortlog(Pack, [limit(5)]),
		   h3('Files in package'),
		   \files_in_pack(Pack),
		   \cpack_dependency_graph(Pack, [])
		 ])).


%%	git_shortlog(+Pack, +Options)//
%
%	Component that show the top-N most recent changes in Pack.

git_shortlog(Pack, Options) -->
	{ cpack_log(Pack, ShortLog, Options) },
	html([ h3('Recent changes'),
	       table(class(git_shortlog),
		     \shortlog_rows(ShortLog, Pack, 1))
	     ]).

shortlog_rows([], _, _) --> [].
shortlog_rows([H|T], Pack, Row) -->
	odd_even_row(Row, Next, \shortlog_row(H, Pack)),
	shortlog_rows(T, Pack, Next).

shortlog_row(Record, Pack) -->
	html([ \td_git_log(Pack, committer_date_relative, Record),
	       \td_git_log(Pack, committer_name, Record),
	       \td_git_log(Pack, subject_and_refnames, Record)
	     ]).

td_git_log(Pack, subject_and_refnames, Record) --> !,
	{ git_log_data(subject, Record, Subject),
	  git_log_data(ref_names, Record, RefNames),
	  git_log_data(commit_hash, Record, Commit),
	  http_link_to_id(git_show, [a(commit),h(Commit),r(Pack)], HREF)
	},
	html(td(class(subject),
		[ a(href(HREF), Subject), \ref_names(RefNames)])).
td_git_log(_, Field, Record) -->
	{ git_log_data(Field, Record, Value),
	  (   Value == ''
	  ->  Class = empty
	  ;   Class = Field
	  )
	},
	html(td(class(Class), Value)).

ref_names([]) --> !.
ref_names(List) -->
	html(span(class(ref_names), \ref_name_list(List))).

ref_name_list([]) --> [].
ref_name_list([H|T]) -->
	html(span(class(ref_name), H)), ref_name_list(T).


%%	commit_info(+Pack, +Hash, +Options)//
%
%	Component to show an individual commit.  Options:
%
%	  * diff(Diff)
%	  One of =stat= (default) or =patch= (full difference)

commit_info(Pack, Hash, Options) -->
	{ select_option(diff(Diff), Options, Rest, stat),
	  cpack_show(Pack, Hash, Record-Body, [diff(Diff)|Rest]),
	  commit_data(subject, Record, Subject)
	},
	html_requires(css('cpack.css')),
	html(div(class(cpack),
		 [ h2(Subject),
		   table(class(commit),
			 [ \tr_commit(author,	 author_name, Record),
			   \tr_commit('',        author_date, Record),
			   \tr_commit(committer, committer_name, Record),
			   \tr_commit('',        committer_date, Record),
			   tr([th(commit),       td(class(commit), Hash)]),
			   \tr_commit(tree,      tree_hash, Record),
			   \tr_commit(parent,    parent_hashes, Record)
			 ]),
		   \select_diff(Diff),
		   pre(class(commitdiff),
		       \diff_lines(Body, Diff))
		 ])).

select_diff(Now) -->
	{ other_diff(Now, Other),
	  http_current_request(Request),
	  http_reload_with_parameters(Request, [diff(Other)], HREF)
	},
	html(div(class(diffstyle),
	       ['Diff style: ', b(Now), ' ', a(href(HREF), Other)])).

other_diff(patch, stat).
other_diff(stat, patch).

diff_lines([], _) --> [].
diff_lines([Line|T], Diff) -->
	(   { diff_line_class(Line, Diff, Class) }
	->  html(span(class(Class), ['~s'-[Line]]))
	;   diff_line(Line, Diff)
	->  []
	;   html('~s'-[Line])
	),
	(   {T==[]}
	->  []
	;   ['\n'],
	    diff_lines(T, Diff)
	).

term_expansion(diff_line_class(Start, Diff, Class),
	       diff_line_class(Codes, Diff, Class)) :-
	is_list(Start),
	append(Start, _, Codes).

diff_line_class("diff ", patch, diff).
diff_line_class("--- ", patch, a).
diff_line_class("+++ ", patch, b).
diff_line_class("-", patch, del).
diff_line_class("+", patch, add).

diff_line(Line, stat) -->
	{ phrase(dirstat(File, Sep, Count, Plusses, Minus), Line) },
	html([ ' ', span(class(file), '~s'-[File]),
	       '~s'-[Sep],
	       '~s'-[Count], ' ',
	       span(class(add), '~s'-[Plusses]),
	       span(class(del), '~s'-[Minus])
	     ]).

dirstat(File, Sep, [D0|RD], Plusses, Minus) -->
	" ",
	string_without(" ", File),
	string(Sep),
	digit(D0),digits(RD),
	" ",
	codes("+", Plusses),
	codes("-", Minus).

codes(Set, [H|T]) --> [H], { memberchk(H, Set) }, !, codes(Set, T).
codes(_, []) --> [].


tr_commit(Label, Field, Record) -->
	{ commit_data(Field, Record, Value) },
	html(tr([th(Label), td(class(Field), Value)])).


%%	files_in_pack(+Pack)// is det.
%
%	Create a =ul= for all files that   appear  in the pack. Maybe we
%	should consider a tree-styled nested =ul=?

files_in_pack(Pack) -->
	{ findall(File, rdf_has(File, cpack:inPack, Pack), Files) },
	list_ul(Files, [class(files), predicate(cpack:path)]).


		 /*******************************
		 *	      FILE		*
		 *******************************/

%%	cpack_file(+FileURL, +Options)// is det.
%
%	Show local view for the file FileURL

cpack_file(FileURL, _Options) -->
	{ rdf_has(FileURL, cpack:path, literal(Path)),
	  rdf_has(FileURL, cpack:inPack, Pack)
	},
	html_requires(css('cpack.css')),
	html(div(class(cpack),
		 [ h2(['File "', Path, '"', \download(FileURL)]),
		   table(class(infobox),
			 [ \p_row(FileURL, cpack:inPack),
			   \p_row(FileURL, cpack:module)
			 ]),
		   br(clear(all)),
		   \git_shortlog(Pack, [limit(5), path(Path)]),
		   \prolog_file(FileURL)
		 ])).

download(FileURL) -->
	{ http_link_to_id(cpack_show_file, [r(FileURL)], HREF)
	},
	html(a([href(HREF), style('float:right')], '[download]')).


%%	prolog_file(+FileURL)// is det.
%
%	Describe our knowledge about a Prolog source file.

prolog_file(FileURL) -->
	{ rdfs_individual_of(FileURL, cpack:'PrologFile') }, !,
	html([ \file_imports(FileURL),
	       \used_by(FileURL),
	       \exported_predicates(FileURL),
	       \required_predicates(FileURL)
	     ]).
prolog_file(_) --> [].


exported_predicates(FileURL) -->
	{ findall(PI, rdf_has(FileURL, cpack:exportsPredicate, PI), List),
	  List \== []
	}, !,
	html(h3('Exported predicates')),
	list_ul(List, []).
exported_predicates(_) --> [].

required_predicates(FileURL) -->
	{ findall(PI, rdf_has(FileURL, cpack:requiresPredicate, PI), List)
	},
	html(h3('Required predicates')),
	list_ul(List, []).

%%	file_imports(+File)// is det.
%
%	Show required dependencies of this file.

file_imports(File) -->
	html([ h3('Imported files'),
	       ul([ \li_imports(File, 'From packages',
				cpack:usesPackageFile),
		    \li_imports(File, 'From ClioPatria',
				cpack:usesClioPatriaFile),
		    \li_imports(File, 'From Prolog',
				cpack:usesSystemFile)
		  ])
	     ]).

li_imports(File, Label, P0) -->
	{ rdf_global_id(P0, P),
	  findall(I, rdf_has(File, P, I), Imports),
	  Imports \== []
	}, !,
	html(li([ Label,
		  \list_ul(Imports, [])
		])).
li_imports(_, _, _) -->
	[].

%%	used_by(+File)// is det.
%
%	Indicates which other files in  which   package  depend  on this
%	file.

used_by(File) -->
	{ findall(By-Pack,
		  file_used_by_file_in_package(File, By, Pack),
		  Pairs),
	  Pairs \== []
	}, !,
	html([ h3('This file is used by'),
	       \list(Pairs, file_in_package, ul)
	     ]).
used_by(_) --> [].

file_in_package(File-Pack) -->
	html([ \cpack_link(File, cpack:path),
	       ' from package ',
	       \cpack_link(Pack)
	     ]).


		 /*******************************
		 *	       BASICS		*
		 *******************************/

%%	p_row(+R, +P)// is det.
%
%	Row in a propery table.

p_row(R, P0) -->
	{ rdf_global_id(P0, P),
	  rdf_display_label(P, Label),
	  rdf_has(R, P, _)
	}, !,
	html(tr([th([Label, :]), td(\cpack_prop(R, P))])).
p_row(_, _) --> [].


%%	list_ul(+ItemList, +Options)
%
%	Create an =ul= list from the items in ItemList.  Options are
%	passed as attributes to the =ul= element, except for:
%
%	  * predicate(P)
%	  Use the predicate P as preferenced prediate to generate a
%	  label.
%
%	@tbd: Allow for sorting

list_ul(List, Options) -->
	{ (   select_option(predicate(P0), Options, Rest)
	  ->  rdf_global_id(P0, P)
	  ;   P = (-),
	      Rest = Options
	  )
	},
	html(ul(Rest,
		\list_li(List, P))).

list_li([], _) --> [].
list_li([H|T], P) -->
	html(li(\cpack_link(H, P))),
	list_li(T, P).


%%	list(+List, :Goal, +Type)// is det.
%
%	Create an HTML list from the elements   of  List. Each member of
%	List is _typeset_ in  an  =li=   element  by  calling call(Goal,
%	Member). Type is one of =ul= or   =ol=, optionally with an extra
%	argument that provides attributes for the list.   For example:
%
%	  ==
%	  	list(List, make_item, ul(class(mylist))),
%	  	...
%
%	  make_item(Name, Mail) -->
%	  	html([Name, ' <mailto:', Mail, '>']).
%	  ==

:- meta_predicate
	list(+,3,+,?,?).

list(List, Goal, Type) -->
	{ Type =.. L,
	  append(L, [\list_item(List, Goal)], L1),
	  Term =.. L1
	},
	html(Term).

list_item([], _) --> [].
list_item([H|T], Goal) -->
	html(li(\call(Goal, H))),
	list_item(T, Goal).


%%	cpack_prop(+Resource, +Property)
%
%	Display the value of  Property  for   Resource  in  the  current
%	location.
%
%	@tbd: Deal with multiple values?

cpack_prop(R, P0) -->
	{ rdf_global_id(P0, P),
	  rdf_has(R, P, O)
	}, !,
	(   { rdf_is_literal(O) }
	->  literal(O)
	;   cpack_link(O)
	).
cpack_prop(_, _) --> [].

literal(literal(type(Type, Value))) -->
	{ rdf_equal(Type, xsd:dateTime),
	  parse_time(Value, Time), !,
	  format_time(atom(Human), '%+', Time)
	},
	html(Human).
literal(O) -->
	{ literal_text(O, Text) },
	html(Text).

%%	cpack_link(+R)// is det.
%%	cpack_link(+R, +P)// is det.
%
%	Display a link to a CPACK   resource.  The version cpack_link//2
%	can be used to select a given property for producing the label.

:- rdf_meta
	cpack_label_property(r).

cpack_link(R) -->
	cpack_link(R, '-').

cpack_link(R, P0) -->
	{ (   P0 \== (-),
	      rdf_global_id(P0, P)
	  ;   cpack_label_property(P)
	  ),
	  rdf_has(R, P, Name), !,
	  literal_text(Name, Text),
	  resource_link(R, HREF)
	},
	html(a(href(HREF), Text)).
cpack_link(L, _) -->
	{ rdf_is_literal(L), !,
	  literal_text(L, Text)
	},
	html(Text).
cpack_link(R, _) -->
	rdf_link(R).

cpack_label_property(cpack:name).
cpack_label_property(foaf:name).
cpack_label_property(rdfs:label).


		 /*******************************
		 *	       LABELS		*
		 *******************************/

%%	rdf_label:display_label_hook(+Pack, ?Lang, -Label) is semidet.
%
%	Provide the label  of  a   package  using  the cpack:packageName
%	property.

rdf_label:display_label_hook(R, _, Label) :-
	rdfs_individual_of(R, cpack:'Package'),
	rdf_has(R, cpack:packageName, Literal),
	literal_text(Literal, Label).
