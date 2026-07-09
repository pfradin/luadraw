# Package luadraw for LuaLaTeX version 3.3

* The *luadraw* package defines the environment of the same name, which lets you create mathematical graphs (2D and 3D) using the Lua language. These graphs are ultimately drawn by TikZ (and automatically saved), so why make them in Lua? Because Lua brings all the power of a simple, efficient programming language, capable of performing calculations, using TikZ's graphics capabilities.  
Run `l3build install` ([`l3build`](https://ctan.org/pkg/l3build) required) or
copy *files* into texmf/tex/lualatex/luadraw/ 
and copy *doc* into texmf/doc/lualatex/luadraw/

* Le paquet *luadraw* définit l'environnement du même nom, celui-ci permet de créer des graphiques mathématiques (2D et 3D) en utilisant le langage Lua. Ces graphiques sont dessinés au final par TikZ (et automatiquement sauvegardés), alors pourquoi les faire en Lua ? Parce que celui-ci apporte toute la puissance d'un langage de programmation simple, efficace, capable de faire des calculs, tout en utilisant les possibilités graphiques de TikZ.  
Exécutez `l3build install` ([`l3build`](https://ctan.org/pkg/l3build) est requis) ou
 copier le contenu de *files* dans : texmf/tex/lualatex/luadraw/  
 et copier le contenu de *doc*  : texmf/doc/lualatex/luadraw/
 
**Version 3.3**

* Added the function *ld.read_table3d()* to read and use an array (list of lists) containing the values ​​of one (or more) function(s) of two variables on a tile [x_1;x_2]x [y_1;y_2].

* Added the option *reverse=true/false} to the facet drawing methods (*g:Dfacet()*, *g:Dmixfacet()*, *g:Dpoly()*, *g:addFacet()*, ...). With the value true, the facet orientation is reversed.
    
* The *luadraw_pdfliteral* module has been added; it allows for direct drawing within the PDF stream—including 2D paths, 2D polylines, sets of 2D points (circular), or 3D facets—without using the \drawcmd command. This significantly reduces compilation time when dealing with large datasets, subject to certain limitations: no opacity changes, and only two fill styles (none or solid).
    
* Three new methods have been added to the *luadraw_spherical* module: *g:DSaddback()*, *g:DSaddinside()*, and *g:DSaddfront()*, which allow for the addition of graphical elements (paths or polylines) behind, inside, or in front of the sphere.

* In the *g:Dfacet()* and *g:Dmixfacet()* methods, when in *ld.mWireframe* mode (drawing edges only), if the *usepalette* option is not nil, the color of each edge is calculated from the palette using the same method as for facets when they are rendered.
    
* Adding the option *out=<variable>* to the method *g:BeginOnPlane()*, it allows retrieving a 2D matrix corresponding to the new axes.
    
* Added the *graphics_options* option for the *g:Dmapimage()* method. Added the *node_options* option for the *g:Dmapimage()* and *g:Dimage()* methods.
    
* For the *g:addPolyline()* method, there is a new (experimental) option called *double={border color, border width}*, which adds a border to each side of the polygonal line. With the default value (nil), there is no border.

* The syntax of the functions *ld.lineEq()* and *ld.planeEq()* has been extended to take into account a possible inequality.
    
* Bug fixes...

 
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
