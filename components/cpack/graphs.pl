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

:- module(c_cpack_graphs,
	  [ cpack_dependency_graph//2	% +URI, +Options
	  ]).
:- include(bundle(html_page)).
:- use_module(library(lists)).
:- use_module(components(graphviz)).
:- use_module(library(cpack/dependency)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_abstract)).
:- use_module(components(label)).


%%	cpack_dependency_graph(+URI, +Options)// is det.
%
%	Show a package and its dependencies

cpack_dependency_graph(URI, _Options) -->
	html([ h3('Dependency graph'),
	       \graphviz_graph(dependency_graph(URI),
			       [ object_attributes([width('100%')]),
				 wrap_url(resource_link),
				 graph_attributes([ rankdir('RL')
						  ]),
				 shape_hook(shape(URI))
			       ])
	     ]).


%%	shape(+Start, +URI, -Shape) is semidet.
%
%	Specify GraphViz shape for URI. This   predicate  calls the hook
%	cliopatria:node_shape/3.

shape(Start, Start,
      [ shape(box3d),style(filled),fillcolor('#ff85fd') ]).

%%	dependency_graph(+URI, -Triples) is det.
%
%	Triples is a graph that describes the dependencies of a package.

dependency_graph(URI, RDF) :-
	findall(T, dependency_triple(URI, T), RDF0),
	sort(RDF0, RDF1),
	minimise_graph(RDF1, RDF2),		% remove inverse/symmetric/...
	(   length(RDF2, Len2),
	    Len2 < 20
	->  RDF3 = RDF2
	;   bagify_graph(RDF2, RDF3, Bags, []) 	% Create bags of similar resources
	),
	append(RDF3, Bags, RDF).

dependency_triple(_, rdf(Pack1, P, Pack2)) :-
	rdf_equal(cpack:requiresPackage, P),
	cpack_requires(Pack1, Pack2, _Why).
