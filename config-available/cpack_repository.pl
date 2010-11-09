:- module(conf_cpack_repository, []).
:- use_module(config_available(foaf)).
:- use_module(cpack_repository(applications/cpack_submit)).
:- use_module(library(http/http_path)).
:- use_module(cliopatria(hooks)).

/** <module> CPACK repository interface
*/

http:location(cpack, root(cpack), []).

cliopatria:menu_popup_order(cpack, 250).
cliopatria:menu_label(cpack, 'CPACK').

cliopatria:menu_item(100=cpack/cpack_list_packages, 'List packs').
cliopatria:menu_item(200=cpack/cpack_submit_form,   'Submit pack').
