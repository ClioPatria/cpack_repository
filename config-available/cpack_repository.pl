:- module(conf_cpack_repository, []).
:- use_module(config_available(foaf)).
:- use_module(cpack_repository(applications/cpack_submit)).
:- use_module(cpack_repository(applications/cpack_home)).
:- use_module(library(http/http_path)).
:- use_module(user(user_db)).
:- use_module(cliopatria(hooks)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_library)).

/** <module> CPACK repository interface
*/

http:location(cpack, root(cpack), []).

cliopatria:menu_popup_order(cpack, 250).
cliopatria:menu_label(cpack, 'CPACK').

cliopatria:menu_item(100=cpack/cpack_list_packages, 'List packs').
cliopatria:menu_item(200=cpack/cpack_submit_form,   'Submit pack').
cliopatria:menu_item(275=current_user/cpack_my_packages, 'My CPACKs') :-
	logged_on(_).

:- rdf_attach_library(cliopatria(rdf)).
:- rdf_load_library(cpack).
:- rdf_load_library(owl).
:- rdf_load_library(dcterms).
:- rdf_load_library(graphviz).

% Hack, make submittedBy work for rdf_has/3.

:- rdf_set_predicate(cpack:submitted,   inverse_of(cpack:submittedBy)).
:- rdf_set_predicate(cpack:submittedBy, inverse_of(cpack:submitted)).
:- rdf_set_predicate(cpack:resolves,    inverse_of(cpack:resolvesAs)).
:- rdf_set_predicate(cpack:resolvesAs,  inverse_of(cpack:resolves)).
