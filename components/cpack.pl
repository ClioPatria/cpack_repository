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
	    cpack_prop//2		% +Resource, +Prop
	  ]).
:- include(bundle(html_page)).
:- use_module(library(cpack/repository)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(semweb/rdf_label)).
:- use_module(components(label)).
:- use_module(cliopatria(hooks)).

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
			   \p_row(Pack, cpack:clonedRepository),
			   \p_row(Pack, cpack:mirrorRepository)
			 ]),
		   br([class('after-ptable')]),
		   div(class(description),
		       \cpack_prop(Pack, cpack:description)),
		   br(clear(all)),
		   h3('Recent changes'),
		   \git_shortlog(Pack, [limit(5)]),
		   h3('Files in package'),
		   ul(class(files),
		      \files_in_pack(Pack))
		 ])).


git_shortlog(Pack, Options) -->
	{ cpack_shortlog(Pack, ShortLog, Options) },
	html(table(class(git_shortlog),
		   \shortlog_rows(ShortLog, 1))).

shortlog_rows([], _) --> [].
shortlog_rows([H|T], Row) -->
	odd_even_row(Row, Next, \shortlog_row(H)),
	shortlog_rows(T, Next).

shortlog_row(Record) -->
	html([ \td_git_log(date, Record),
	       \td_git_log(committer, Record),
	       \td_git_log(title, Record)
	     ]).

td_git_log(Field, Record) -->
	{ git_log_data(Field, Record, Value) },
	html(td(class(Field), Value)).


%%	files_in_pack(+Pack)// is det.
%
%	Create a =ul= for all files that   appear  in the pack. Maybe we
%	should consider a tree-styled nested =ul=?

files_in_pack(Pack) -->
	{ findall(File, rdf_has(File, cpack:inPack, Pack), Files) },
	list_li(Files).

list_li([]) --> [].
list_li([H|T]) -->
	html(li(\cpack_link(H, cpack:path))),
	list_li(T).


%%	cpack_file(+FileURL, Options)// is det.
%
%	Show info about the file FileURL

cpack_file(FileURL, _Options) -->
	{ rdf_has(FileURL, cpack:path, literal(Path))
	},
	html_requires(css('cpack.css')),
	html(div(class(cpack),
		 [ h2(['File "', Path, '"']),
		   table(class(infobox),
			 [ \p_row(FileURL, cpack:inPack),
			   \p_row(FileURL, cpack:module)
			 ]),
		   br(clear(all)),
		   h3('Exported predicates'),
		   ul(class(exports),
		      \exported_predicates(FileURL))
		 ])).

exported_predicates(FileURL) -->
	{ findall(PI, rdf_has(FileURL, cpack:exportsPredicate, literal(PI)), List)
	},
	atoms_li(List).

atoms_li([]) --> [].
atoms_li([H|T]) -->
	html(li(H)),
	atoms_li(T).


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
cpack_link(R, _) -->
	rdf_link(R).

cpack_label_property(cpack:name).
cpack_label_property(foaf:name).
cpack_label_property(rdfs:label).
