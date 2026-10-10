# Package luadraw for LuaLaTeX version 3.5

* The *luadraw* package defines the environment of the same name, which lets you create mathematical graphs (2D and 3D) using the Lua language. These graphs are ultimately drawn by TikZ (and automatically saved), so why make them in Lua? Because Lua brings all the power of a simple, efficient programming language, capable of performing calculations, using TikZ's graphics capabilities.  
Run `l3build install` ([`l3build`](https://ctan.org/pkg/l3build) required) or
copy *files* into texmf/tex/lualatex/luadraw/ 
and copy *doc* into texmf/doc/lualatex/luadraw/

* Le paquet *luadraw* définit l'environnement du même nom, celui-ci permet de créer des graphiques mathématiques (2D et 3D) en utilisant le langage Lua. Ces graphiques sont dessinés au final par TikZ (et automatiquement sauvegardés), alors pourquoi les faire en Lua ? Parce que celui-ci apporte toute la puissance d'un langage de programmation simple, efficace, capable de faire des calculs, tout en utilisant les possibilités graphiques de TikZ.  
Exécutez `l3build install` ([`l3build`](https://ctan.org/pkg/l3build) est requis) ou
 copier le contenu de *files* dans : texmf/tex/lualatex/luadraw/  
 et copier le contenu de *doc*  : texmf/doc/lualatex/luadraw/
 
**Version 3.6**

* In the *luadraw_shadedforms* module: addition of the *bilinear=true/false* option for the *g:Dshadedrectangle()* method, allowing the use (or not) of bilinear shading on each grid cell (true by default).

* Addition of the function *ld.pg_gonreg()*,  which returns a list of lists of complex numbers representing a regular star polygon.

* In the *luadraw_spherical* module:
    
   * Most of the module's global functions can now be applied to a sphere other than the current one. 
        
   * Added the *g:Split_facet()* method, which calculates and returns—as 3D paths—the outline of the visible part of the spherical facet and the outline of the hidden part.
         
   * Added the *g:Split_region()* method, which calculates and returns—as 3D paths—the outline of the visible part of the spherical region and the outline of the hidden part. 
        
   * Two options—*front* and *back}—have been added to the *g:DSfacet()* and *g:DSregion()* methods; these allow the user to retrieve the path corresponding to the outline of the visible part (for the first option) and the path corresponding to the outline of the hidden part (for the second option).
        
   * Most graphical elements in this module now have options to add a label.
    

* For the *ld.section2tube()* function, the *opensection=true/false* option has been added; with true, the section will not be closed—meaning there will be no facets between the last and first points of the section, and the tube will have a lateral opening.

* For the *ld.line2tube()* function, the *opensection=true/false* option (as described above) and the *angle={angle1,angle2}* option have been added; the latter allows the start and end angles (in degrees) to be modified so that the section forms only part of a circle.
    
* Added three options for the *g:BeginOnPlane()* method: the *border=true/false* option and the *border_options* option, which allow drawing a frame around the chosen 2D window in the plane, as well as the *tikzmatrix=true/false* option which allows using or not the TikZ transformation matrix for this method.

* Added the function *ld.parabola(A,B,C)*, which returns a Bézier curve representing a parabolic arc from point *A* to point *C*, passing through point *B* (three complex numbers).

* Added the function *ld.param_path(P)*, which returns a function representing a parametrization of the 2D path *P*. 
    
* Addition of the *ld.splitpath()* function, which allows a path to be split into two paths at a given point.    

* Added the function *ld.common_tangent()* and the corresponding drawing method *g:Dcommon_tangent()*; this allows for drawing the common tangents to two parametric curves.

* Added the function *ld.parallel_tangent()* and the corresponding drawing method *g:Dparallel_tangent()*; this allows for drawing tangents to a parametric curve that are parallel to the tangent at a point with a given parameter, or parallel to a given direction.

* The optional argument of the *ld.solve()* function can now take an additional value: *"bisection"*, in which case the method used will be bisection (this can be useful if the Newton method fails).
    
* Added the function *ld.getmatrix(T1,T2)*, which calculates and returns the matrix transforming triangle *T1* (a list of three non-collinear 2D points) into triangle *T2* (a list of three 2D points). 

* Added the function *ld.getmatrix3d(T1,T2)*, which calculates and returns the matrix transforming tetrahedron *T1* (a list of four non-coplanar 3D points) into tetrahedron *T2* (a list of four 3D points).
    
* Bug fixes...  
 
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
