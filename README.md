# Package luadraw for LuaLaTeX version 3.1

* The *luadraw* package defines the environment of the same name, which lets you create mathematical graphs (2D and 3D) using the Lua language. These graphs are ultimately drawn by TikZ (and automatically saved), so why make them in Lua? Because Lua brings all the power of a simple, efficient programming language, capable of performing calculations, using TikZ's graphics capabilities.  
Run `l3build install` ([`l3build`](https://ctan.org/pkg/l3build) required) or
copy *files* into texmf/tex/lualatex/luadraw/ 
and copy *doc* into texmf/doc/lualatex/luadraw/

* Le paquet *luadraw* définit l'environnement du même nom, celui-ci permet de créer des graphiques mathématiques (2D et 3D) en utilisant le langage Lua. Ces graphiques sont dessinés au final par TikZ (et automatiquement sauvegardés), alors pourquoi les faire en Lua ? Parce que celui-ci apporte toute la puissance d'un langage de programmation simple, efficace, capable de faire des calculs, tout en utilisant les possibilités graphiques de TikZ.  
Exécutez `l3build install` ([`l3build`](https://ctan.org/pkg/l3build) est requis) ou
 copier le contenu de *files* dans : texmf/tex/lualatex/luadraw/  
 et copier le contenu de *doc*  : texmf/doc/lualatex/luadraw/
 
**Version 3.1**

* Added the function *ld.implicit_inequality()* which returns a **path** representing the outline of the part of the plane located within a certain block and satisfying a condition of the type $f(x,y)>=0$ or $f(x,y)<=0$.

* Added the method *g:Dimplicit_inequalities()* which can fill the set of points satisfying a constraint system of the type $f_i(x,y)>0$ or $f_i(x,y)<0$.

* Adding the function *plane2rectangle(P, V, L1, L2)* which returns a rectangle (a list of 3D points) to represent this plane. This is the same rectangle drawn by the method *g:Dplane(P, V, L1, L2)*. It can be drawn using the method *g:Dpolyline3d()* or as a facet.

* For the methods *g:addPlane()* and *g:addPlaneEq()* (which are used in *g:Dscene3d()*), the *rectangle* option has been added. When *rectangle=nil* (the default value), the plane is cut by the 3D window and the resulting facet is drawn. When *rectangle={V,L1,L2}, the plane is drawn as a rectangular facet; this is the same rectangle drawn by the method *g:Dplane(P, V, L1, L2)* (where *P* denotes the plane).

* When creating a 3D graph, the *window3d* option now takes into account the scales on the three axes:

 *window3d={x1,x2,y1,y2,z1,z2 xscale,yscale,zscale}*.
 
These three scales are optional and are set to $1$ by default; they determine the initial 3D matrix of the graph (which is therefore no longer the identity if one of the scales is different from 1).

* When creating a graph, the *margin* option can now be reduced to a single number when all four margins are equal, for example *margin=0*.

* Added the option *useclip=<boolean>* for the method *g:Dinequalities()* which allows you to choose the technique to use, either contour calculation (with the value false, default value), or with a series of clips (with the value true).

* The *rectangle()* function now has a second possible syntax: *rectangle(a, b)*, in which case the rectangle has its sides parallel to the axes and has opposite vertices *a* and *b. ​​The same applies to the corresponding graphical method *g:Drectangle()*.

* For the *line2strip()* function, the argument *ends, which was previously a boolean, can now take the values ​​*"none"* (equivalent to false), *"butt"* (equivalent to true), or *"round"*. Boolean values ​​are still accepted.

* The function *line2strip()* now has an additional argument called *mode*, which can be *"center"* (default value), or *"left"*, or *"right"*, in the first case the strip is centered on the polygonal line, in the second case it is on the left of the line, and in the third case it is on the right of the line.

* In the function *read_csv_file()*, add the option *comment=<char>*, this indicates the characters that begin a line of comments.

* All 2D methods for drawing lines or half-lines (*g:Dline()*, *g:Dperp()*,...) now have an *scale* option, like the *g:Dseg()* method. This option (which defaults to 1) can be a number (percentage) or a table of two numbers (percentages). The second case allows you to vary the two endpoints separately.

* Bug fixes...


**Version 3.0**

This version introduces a major change: all data related to the *luadraw* package is now stored in the namespace (table) named *luadraw*. This necessitates the use of dot notation, for example, *luadraw.graph* instead of *graph*. However, it is possible to create shortcuts; for instance, the instruction `local ld = luadraw` will allow you to use *ld* instead of *luadraw* in dot notation. Refer to the very beginning of the documentation for more details.

This change results in incompatibility with previous versions; however, the changes required to adapt older code are minimal, especially since there are no changes to the graphics methods (they were already encapsulated in two classes).

This change also brings some (minor) modifications to extensions; refer to the documentation for more details.


* Added a second possible syntax for the functions *ld.surface()* and *ld.obj_surface()*: *ld.surface(f, mesh)* and *ld.obj_surface(f, mesh)* where *mesh={{u1,...,u_n}, {v1,...,vm}}* (increasing list of values ​​of parameter *u* and increasing list of values ​​of parameter *v*).

* In the *luadraw_decorations* module: the 2D and 3D line drawing methods have been overridden so that the *draw_options* argument, which is normally a string passed to the *\draw* instruction, can be replaced by a table whose fields represent possible options (such as adding a label). The method names remain unchanged, and the old syntax is still valid.

* In the functions *ld.curve2cone()*, *ld.curve2cylinder()*, *ld.line2tube()*, *ld.section2tube()*, *ld.rotcurve()* and *ld.rotline()*, add the option *obj=true/false*; with the value *false* (default) the functions return a list of facets, with the value *true* they return a table {vertices={3D points}, facets=\{{index1,...},...}, normals={3D vectors}}}. If the *g:Dpoly()* method does not take into account the *normals* field, the *g:Pov_facet()* method of the *luadraw_povray* module, on the other hand, uses this field when it is present.

* Added the function *ld.cutpolyline2(P,f,sg,close)* where *P* is a polygon (list of complex numbers), *f* is a function (x-> f(x) real), and *sg* is a string equal to ">" or "<". This function returns the outline of the part of the polygon satisfying the constraint *y>f(x)* or *y<f(x)* depending on the value of *sg*.

* Added the method *g:Dinequalities(constraints, options)* which draws the set of points satisfying a constraint system of the type *y>fi(x)* or *y<fi(x)*.

* In the *luadraw_povray* module: added the option *usepalette={palette, mode, minmax}*.

* Bug fixes...
