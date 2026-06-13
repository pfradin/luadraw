# Package luadraw for LuaLaTeX version 3.2

* The *luadraw* package defines the environment of the same name, which lets you create mathematical graphs (2D and 3D) using the Lua language. These graphs are ultimately drawn by TikZ (and automatically saved), so why make them in Lua? Because Lua brings all the power of a simple, efficient programming language, capable of performing calculations, using TikZ's graphics capabilities.  
Run `l3build install` ([`l3build`](https://ctan.org/pkg/l3build) required) or
copy *files* into texmf/tex/lualatex/luadraw/ 
and copy *doc* into texmf/doc/lualatex/luadraw/

* Le paquet *luadraw* définit l'environnement du même nom, celui-ci permet de créer des graphiques mathématiques (2D et 3D) en utilisant le langage Lua. Ces graphiques sont dessinés au final par TikZ (et automatiquement sauvegardés), alors pourquoi les faire en Lua ? Parce que celui-ci apporte toute la puissance d'un langage de programmation simple, efficace, capable de faire des calculs, tout en utilisant les possibilités graphiques de TikZ.  
Exécutez `l3build install` ([`l3build`](https://ctan.org/pkg/l3build) est requis) ou
 copier le contenu de *files* dans : texmf/tex/lualatex/luadraw/  
 et copier le contenu de *doc*  : texmf/doc/lualatex/luadraw/
 
**Version 3.2**

* Added the methods *g:BeginOnplane()* and *g:EndOnPlane()*, which allow drawing on a plane in space using 2D graphics methods.

* The *luadraw_fields* module now includes vector fields tangent to a surface. The *ld.surfacefield()* function calculates and returns the vector field, while the *g:Dsurfacefield()* method allows drawing the vector field with (or without) the surface.

* The *ld.linspace()* function has a second possible syntax: *ld.linspace(a1, b1, n1, b2, n2, ..., bp, np)*, which returns a list of *n1* evenly distributed numbers from *a1* to *b1*, followed by *n2* evenly distributed numbers from *b1* to *b2* (without repeating *b1*), and so on.

* In the *luadraw_spherical* module, the following functions have been added:

    *ld.interGreatC(C1,C2)* which returns, as a sequence, the two points of intersection of the two great circles *C1* and *C2* belonging to the sphere.

    *ld.interSphericalC(P1, P2)* which returns, as a sequence, the points of intersection (if they exist) between two circles belonging to the sphere (not necessarily great circles).

    *ld.projstereo_Scircle(P, N, h)* which returns, as a path, the stereographic projection of a circle drawn on the sphere.

    *ld.projstereo_Sfacet(L, N, h, close)* which returns, as a path, the stereographic projection of a spherical facet.

* Added three options to the *g:Dboxaxes3d()* method: *xlabels={x1,...,xn}, *ylabels={y1,...,yn}, *zlabels={z1,...,zn}*. These options allow you to apply labels to the axes. By default, these options have the value *nil*, in which case the default labels (one per graduation mark) are displayed.

* In the *luadraw_povray* module, there is a new option in the default settings: *arrowscale={1,1}*, which is a table of two numbers. The first is a scale factor for the radius of the base of the arrows (which are cones), and the second is a scale factor for the height of the arrows.

In the *g:Pov_polyline()* and *g:addPolyline()* methods, the option of the same name can now be either an array of two numbers or a single number (in which case the two numbers are considered equal).

* For 3D, there is a new global variable *ld.Hiddenlinescale* which defaults to *2/3*. This means that the thickness of hidden lines will be equal to the thickness of visible lines, multiplied by this number, when using the *g:Dscene3d()* method.

* Bug fixes...

 
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
