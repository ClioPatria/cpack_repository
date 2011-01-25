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

:- module(cpack_home, []).
:- include(bundle(html_page)).
:- use_module(components(cpack)).

:- http_handler(root(cpack_home), cpack_home,      []).
:- http_handler(root(home),       cliopatria_home, [id(home), priority(10)]).

%%	cpack_home(+Request)
%
%	HTTP handler that provides the CPACK welcome page

cpack_home(Request) :-
	http_parameters(Request,
			[ sort_by(By,
				  [ oneof([status,name,title,type,author,date]),
				    default(name),
				    description('Sort table by this key')
				  ])
			]),
	reply_html_page(cliopatria(cpack),
			title('CPACK: The ClioPatria Package Manager'),
			div(class(cpack),
			    [ \insert_html_file(html('cpack_home.html')),
			      \package_table([sort_by(By)])
			    ])).

%%	cliopatria_home(+Request)
%
%	HTTP handler that provides the ClioPatria welcome page.

cliopatria_home(_Request) :-
	reply_html_page(cliopatria(cpack),
			title('ClioPatria: the SWI-Prolog RDF toolkit'),
			\insert_html_file(html('cliopatria_home.html'))).
