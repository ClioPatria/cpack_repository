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

:- module(api_cpack,
	  [
	  ]).

/** <module> CPACK API for installing packages

This module defines the server-end API for

    * Quick discovery of packages
    * Installation of packages

To *install* a package, we must discover

    * Its GIT repository and branch
    * Dependencies (package names)
    * Possible conflicts with packages that we already have and that
      may not come from the server.  We need to do two things:
	- Make sure none of the modules conflict
	- Make sure that the relative paths of the packages provide
	  the required predicates.

So, conflict detection requires us to  send   the  list  of files in the
package and for each file the  =module=,   the  list of FileRefs and the
list of predicates these must supply and the list of exports.

To *discover* a package, we will search for

    * The package name
    * Words in the package title and description
    * Files in the package (exact)
    * Exported predicates (exact)
*/



