# Package luadraw for LuaLaTeX version 3.7

* The *luadraw* package defines the environment of the same name, which lets you create mathematical graphs (2D and 3D) using the Lua language. These graphs are ultimately drawn by TikZ (and automatically saved), so why make them in Lua? Because Lua brings all the power of a simple, efficient programming language, capable of performing calculations, using TikZ's graphics capabilities.  
Run `l3build install` ([`l3build`](https://ctan.org/pkg/l3build) required) or
copy *files* into texmf/tex/lualatex/luadraw/ 
and copy *doc* into texmf/doc/lualatex/luadraw/

* Le paquet *luadraw* définit l'environnement du même nom, celui-ci permet de créer des graphiques mathématiques (2D et 3D) en utilisant le langage Lua. Ces graphiques sont dessinés au final par TikZ (et automatiquement sauvegardés), alors pourquoi les faire en Lua ? Parce que celui-ci apporte toute la puissance d'un langage de programmation simple, efficace, capable de faire des calculs, tout en utilisant les possibilités graphiques de TikZ.  
Exécutez `l3build install` ([`l3build`](https://ctan.org/pkg/l3build) est requis) ou
 copier le contenu de *files* dans : texmf/tex/lualatex/luadraw/  
 et copier le contenu de *doc*  : texmf/doc/lualatex/luadraw/
 
**Version 3.7**

* Added the function *ld.poly2graph()* which allows converting a polyhedron into a graph.
    
* Added the *g:Dpolyline2()* method; it functions the same way as the *g:Dpolyline()* method, except that the list of points—in addition to complex numbers—may also contain TikZ nodes expressed as strings.

* For the methods *g:Cylinder_outline()*, *g:Cone_outline()*, and *g:Frustum_outline()*: the returned result contains two additional fields: the *hiddenside* field and the *hiddensection* field.
    
* In the module *luadraw_decorations*: added the method *g:Dangle3d()* which draws a right angle in space with options to paint the parallelogram and to add a label.
    
* Added the method *g:Dmarkseg3d()* which allows marking a segment of space with small segments.
    
* Added new 3D projection modes: *"yx"*, *"zx"*, *"zy"*, *"osi"*, *"yOx"*, *"zOx"*, and *"zOy"*, which are the "symmetrical" versions of the modes *"xy"*, *"xz"*, *"yz"*, *"iso"*, *"xOy"*, *"xOz"*, and *"yOz"*.
    
* Addition of the function *ld.cutfacet2()*, which allows for the cutting of a list of facets (or a polyhedron) by an implicit surface defined by the equation *f(x,y,z)=0*, subject to certain conditions on *f*.
    
* In the *luadraw_spherical* module, three methods have been added: *g:Dhalf_sphere()*, *g:Dquater_sphere()*, and *g:Dheighth_sphere()*.

* For the *g:Dcut_sphere()* method, the following options have been added: *section=""*, which controls whether or not the circular section is drawn, and *outline=false*, which controls whether or not the outline of the spherical part is drawn.
    
* Bug fixes...    

 
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
