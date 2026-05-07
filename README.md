# Package luadraw for LuaLaTeX version 2.8

* The *luadraw* package defines the environment of the same name, which lets you create mathematical graphs (2d and 3d) using the Lua language. These graphs are ultimately drawn by tikz (and automatically saved), so why make them in Lua? Because Lua brings all the power of a simple, efficient programming language, capable of performing calculations, using tikz's graphics capabilities.  
Run `l3build install` ([`l3build`](https://ctan.org/pkg/l3build) required) or
copy *files* into texmf/tex/lualatex/luadraw/ 
and copy *doc* into texmf/doc/lualatex/

* Le paquet *luadraw* définit l'environnement du même nom, celui-ci permet de créer des graphiques mathématiques (2d et 3d) en utilisant le langage Lua. Ces graphiques sont dessinés au final par tikz (et automatiquement sauvegardés), alors pourquoi les faire en Lua ? Parce que celui-ci apporte toute la puissance d'un langage de programmation simple, efficace, capable de faire des calculs, tout en utilisant les possibilités graphiques de tikz.  
Exécutez `l3build install` ([`l3build`](https://ctan.org/pkg/l3build) est requis) ou
 copier le contenu de *files* dans : texmf/tex/lualatex/luadraw/  
 et copier le contenu de *doc*  : texmf/doc/lualatex/
 
**Version 3.0**

This version introduces a major change: all data related to the *luadraw* package is now stored in the namespace (table) named *luadraw*. This necessitates the use of dot notation, for example, *luadraw.graph* instead of *graph*. However, it is possible to create shortcuts; for instance, the instruction `local ld = luadraw` will allow you to use *ld* instead of *luadraw* in dot notation. Refer to the very beginning of this document for more details.

This change results in incompatibility with previous versions; however, the changes required to adapt older code are minimal, especially since there are no changes to the graphics methods (they were already encapsulated in two classes).

This change also brings some (minor) modifications to extensions; refer to the documentation for more details.


* Added a second possible syntax for the functions *ld.surface()* and *ld.obj_surface()*: *ld.surface(f, mesh)* and *ld.obj_surface(f, mesh)* where *mesh={{u1,...,u_n}, {v1,...,vm}}* (increasing list of values ​​of parameter *u* and increasing list of values ​​of parameter *v*).

* In the *luadraw_decorations* module: the 2D and 3D line drawing methods have been overridden so that the *draw_options* argument, which is normally a string passed to the *\draw* instruction, can be replaced by a table whose fields represent possible options (such as adding a label). The method names remain unchanged, and the old syntax is still valid.

* In the functions *ld.curve2cone()*, *ld.curve2cylinder()*, *ld.line2tube()*, *ld.section2tube()*, *ld.rotcurve()* and *ld.rotline()*, add the option *obj=true/false*; with the value *false* (default) the functions return a list of facets, with the value *true* they return a table {vertices={3D points}, facets=\{{index1,...},...}, normals={3D vectors}}}. If the *g:Dpoly()* method does not take into account the *normals* field, the *g:Pov_facet()* method of the *luadraw_povray* module, on the other hand, uses this field when it is present.

* Added the function *ld.cutpolyline2(P,f,sg,close)* where *P* is a polygon (list of complex numbers), *f* is a function (x-> f(x) real), and *sg* is a string equal to ">" or "<". This function returns the outline of the part of the polygon satisfying the constraint *y>f(x)* or *y<f(x)* depending on the value of *sg*.

* Added the method *g:Dinequalities(constraints, options)* which draws the set of points satisfying a constraint system of the type *y>fi(x)* or *y<fi(x)*.

* In the *luadraw_povray* module: added the option *usepalette={palette, mode, minmax}*.

* Bug fixes...
