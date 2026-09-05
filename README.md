# Package luadraw for LuaLaTeX version 3.5

* The *luadraw* package defines the environment of the same name, which lets you create mathematical graphs (2D and 3D) using the Lua language. These graphs are ultimately drawn by TikZ (and automatically saved), so why make them in Lua? Because Lua brings all the power of a simple, efficient programming language, capable of performing calculations, using TikZ's graphics capabilities.  
Run `l3build install` ([`l3build`](https://ctan.org/pkg/l3build) required) or
copy *files* into texmf/tex/lualatex/luadraw/ 
and copy *doc* into texmf/doc/lualatex/luadraw/

* Le paquet *luadraw* définit l'environnement du même nom, celui-ci permet de créer des graphiques mathématiques (2D et 3D) en utilisant le langage Lua. Ces graphiques sont dessinés au final par TikZ (et automatiquement sauvegardés), alors pourquoi les faire en Lua ? Parce que celui-ci apporte toute la puissance d'un langage de programmation simple, efficace, capable de faire des calculs, tout en utilisant les possibilités graphiques de TikZ.  
Exécutez `l3build install` ([`l3build`](https://ctan.org/pkg/l3build) est requis) ou
 copier le contenu de *files* dans : texmf/tex/lualatex/luadraw/  
 et copier le contenu de *doc*  : texmf/doc/lualatex/luadraw/
 
**Version 3.5**

* In the *luadraw_spherical* module:
   * The *g:Define_sphere()* method has three additional options: *back=true/false*, *inside=true/false*, and *front=true/false*. These allow for toggling the display of elements located behind, inside, and in front of the sphere. 
    
   * Added the *g:DSaxes()* method, which draws the three axes. 
    
   * Most graphics methods now include additional options for adding a label. 
   
   * Added the functions *ld.smidpoint()* and *ld.sbarycenter()*. 

* In the *luadraw_povray* module:
   * Added the option *clip=true/false* for the *g:Pov_polyline()* method to clip the polygonal line to the current 3D viewport.

   * Added the option *csg=true/false* for the *g:Pov_facet()* method. With the value true, the defined object can be used in CSG operations (with some limitations).

* For 3D paths: addition of the instructions *"e"* for ellipse and *"ea"* for elliptic arc.

* New 3D functions for ellipses and elliptic arcs: *ld.ellipse3db()*, *ld.ellipse3d()*, *ld.ellipticarc3db()*, *ld.ellipticarc3d()*, as well as the associated drawing methods: *g:Dellipse3d()* and *g:Dellipticarc3d()*.

* Added the method *g:Dcut_sphere()* which allows drawing a sphere after it has been cut by a plane.

* Added the *apex=true/false* option for the *g:Dcone()* method; with the value true, the apex is marked by a point when the cone is viewed from above.

* The *g:Saveattr()* method now accepts an argument: *g:Saveattr(scope_options)*; this is a string (empty by default) that will be passed to the *scope* instruction.

* Added the *lowlevel=true/false* option for the *g:BeginOnPlane()* method; with the value true, the transformation matrix is ​​applied at the lowest level, so all graphical elements—arrows, text, etc.—are drawn in the desired plane.

* Addition of the *luadraw_linprog* module, which enables the solving and graphical representation of solutions to 2D or 3D linear programming problems.

* Added the option *gradient=true/false* for the methods *g:Dcylinder()*, *g:Dcone()*, and *g:Dfrustum()*. This specifies whether the fill should be done with a gradient or not (true by default).

* Bug fixes...
 
**Version 3.4**

* For the *g:Dfrustum()* method, the old syntax *g:Dfrustum(C,R,r,V,options* has been changed to *g:Dfrustum(C,R,r,A,options*; the difference is that the center of the second circular base is point *A*, whereas in the old syntax it was point *C+V*. Similarly, for the *ld.frustum()* function, the old syntax *ld.frustum(C,R,r,V,nb,open* becomes *ld.frustum(C,R,r,A,nb,open*, with the same difference. This change ensures a consistent syntax for cylinders, cones, and truncated cones (frustum).
    
* For the method *g:Beginclip(path, inverse)*, the *path* can be a 2D or 3D path.

* In the *luadraw_spherical* module: addition of the *g:DSregion()* method, which allows painting a region of the sphere bounded by a simple closed spherical curve.

* Optional arguments have been added to the *g:Endclip(draw_path, draw_options)* method: *draw_path* is a boolean (false by default); when set to true, the path used for clipping is drawn after the group is closed, and in this case, the optional argument *draw_options*, which must be a character string (empty by default), is passed to the \draw command.

* Addition of functions specific to 2D or 3D paths: *ld.path()*, *ld.path3d()*, *ld.convpath()*, *ld.polyline2path()*, *ld.polyline2path3d()*, and the methods, *g:Convpath3d()*, *g:Path3d2path2d()*.
    
* Added the methods *g:Cylinder_outline()*, *g:Cone_outline()*, *g:Frustum_outline()*, and *g:Sphere_outline()*, which return a table containing the characteristic elements (outline, visible edges, hidden edges, etc.).
    
* Added the methods *g:Cylinder_tangency()*, *g:Cone_tangency()*, *g:Frustum_tangency()*, and *g:Sphere_tangency()*, which calculate and return the list of tangency points for cylinders, cones, truncated cones, and circles drawn on a sphere.
    
* For the function *ld.read_table3d()*, added the option *mode* allowing you to choose a type of 3D table (types *"xyz"*, or *"y/x"* or *"x/y"*.

*  The *usepalette()* option (for drawing facets) now accepts a third (optional) argument:
 *usepalette(palette, mode, minmax*, where *minmax* is a table of two numbers; these two numbers are used as the minimum and maximum values ​​for the *mode* argument. If *minmax* is omitted, these two values ​​are calculated automatically.
    
* Bug fixes...
