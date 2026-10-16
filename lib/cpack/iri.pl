:- module(iri,
          []).

:- public iri_http/3.
iri_http(open(_,_), _IRI, _) :- fail.
iri_http(access(_), _IRI, false).
iri_http(time,      _IRI, _) :- fail.
iri_http(size,      _IRI, _) :- fail.

:- register_iri_scheme(http, iri_http, []).
:- register_iri_scheme(https, iri_http, []).
