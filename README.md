# Package luadraw for LuaLaTeX version 3.4

* The *luadraw* package defines the environment of the same name, which lets you create mathematical graphs (2D and 3D) using the Lua language. These graphs are ultimately drawn by TikZ (and automatically saved), so why make them in Lua? Because Lua brings all the power of a simple, efficient programming language, capable of performing calculations, using TikZ's graphics capabilities.  
Run `l3build install` ([`l3build`](https://ctan.org/pkg/l3build) required) or
copy *files* into texmf/tex/lualatex/luadraw/ 
and copy *doc* into texmf/doc/lualatex/luadraw/

* Le paquet *luadraw* définit l'environnement du même nom, celui-ci permet de créer des graphiques mathématiques (2D et 3D) en utilisant le langage Lua. Ces graphiques sont dessinés au final par TikZ (et automatiquement sauvegardés), alors pourquoi les faire en Lua ? Parce que celui-ci apporte toute la puissance d'un langage de programmation simple, efficace, capable de faire des calculs, tout en utilisant les possibilités graphiques de TikZ.  
Exécutez `l3build install` ([`l3build`](https://ctan.org/pkg/l3build) est requis) ou
 copier le contenu de *files* dans : texmf/tex/lualatex/luadraw/  
 et copier le contenu de *doc*  : texmf/doc/lualatex/luadraw/
 
**Version 3.4**

* For the *g:Dfrustum()* method, the old syntax *g:Dfrustum(C,R,r,V,options* has been changed to *g:Dfrustum(C,R,r,A,options*; the difference is that the center of the second circular base is point *A*, whereas in the old syntax it was point *C+V*. Similarly, for the *ld.frustum()* function, the old syntax *ld.frustum(C,R,r,V,nb,open* becomes *ld.frustum(C,R,r,A,nb,open*, with the same difference. This change ensures a consistent syntax for cylinders, cones, and truncated cones (frustum).
    
* For the method *g:Beginclip(path, inverse)*, the *path* can be a 2D or 3D path.

* In the *luadraw_spherical* module: addition of the *g:DSregion()* method, which allows painting a region of the sphere bounded by a simple closed spherical curve.

* Optional arguments have been added to the *g:Endclip(draw_path, draw_options)* method: *draw_path* is a boolean (false by default); when set to true, the path used for clipping is drawn after the group is closed, and in this case, the optional argument *draw_options*, which must be a character string (empty by default), is passed to the \draw command.

* Addition of functions specific to 2D or 3D paths: *ld.path()*, *ld.path3d()*, *ld.convpath()*, *ld.polyline2path()*, *ld.polyline2path3d()*, and the methods, *lg:Convpath3d()*, *g:Path3d2path2d()*.
    
* Added the methods *g:Cylinder_outline()*, *g:Cone_outline()*, *g:Frustum_outline()*, and *g:Sphere_outline()*, which return a table containing the characteristic elements (outline, visible edges, hidden edges, etc.).
    
* Added the methods *g:Cylinder_tangency()*, *g:Cone_tangency()*, *g:Frustum_tangency()*, and *g:Sphere_tangency()*, which calculate and return the list of tangency points for cylinders, cones, truncated cones, and circles drawn on a sphere.
    
* For the function *ld.read_table3d()*, added the option *mode* allowing you to choose a type of 3D table (types *"xyz"*, or *"y/x"* or *"x/y"*.

*  The *usepalette()* option (for drawing facets) now accepts a third (optional) argument:
 *usepalette(palette, mode, minmax*, where *minmax} is a table of two numbers; these two numbers are used as the minimum and maximum values ​​for the *mode* argument. If *minmax* is omitted, these two values ​​are calculated automatically.
    
* Bug fixes...
 
**Version 3.3**

* Added the function *ld.read_table3d()* to read and use an array (list of lists) containing the values ​​of one (or more) function(s) of two variables on a tile [x_1;x_2]x [y_1;y_2].

* Added the option *reverse=true/false} to the facet drawing methods (*g:Dfacet()*, *g:Dmixfacet()*, *g:Dpoly()*, *g:addFacet()*, ...). With the value true, the facet orientation is reversed.
    
* The *luadraw_pdfliteral* module has been added; it allows for direct drawing within the PDF stream—including 2D paths, 2D polylines, sets of 2D points (circular), or 3D facets—without using the \draw command. This significantly reduces compilation time when dealing with large datasets, subject to certain limitations: no opacity changes, and only two fill styles (none or solid).
    
* Three new methods have been added to the *luadraw_spherical* module: *g:DSaddback()*, *g:DSaddinside()*, and *g:DSaddfront()*, which allow for the addition of graphical elements (paths or polylines) behind, inside, or in front of the sphere.

* In the *g:Dfacet()* and *g:Dmixfacet()* methods, when in *ld.mWireframe* mode (drawing edges only), if the *usepalette* option is not nil, the color of each edge is calculated from the palette using the same method as for facets when they are rendered.
    
* Adding the option *out=<variable>* to the method *g:BeginOnPlane()*, it allows retrieving a 2D matrix corresponding to the new axes.
    
* Added the *graphics_options* option for the *g:Dmapimage()* method. Added the *node_options* option for the *g:Dmapimage()* and *g:Dimage()* methods.
    
* For the *g:addPolyline()* method, there is a new (experimental) option called *double={border color, border width}*, which adds a border to each side of the polygonal line. With the default value (nil), there is no border.

* The syntax of the functions *ld.lineEq()* and *ld.planeEq()* has been extended to take into account a possible inequality.
    
* Bug fixes...
