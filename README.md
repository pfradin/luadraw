# Package luadraw for LuaLaTeX version 2.7

* The *luadraw* package defines the environment of the same name, which lets you create mathematical graphs (2d and 3d) using the Lua language. These graphs are ultimately drawn by tikz (and automatically saved), so why make them in Lua? Because Lua brings all the power of a simple, efficient programming language, capable of performing calculations, using tikz's graphics capabilities.  
Run `l3build install` ([`l3build`](https://ctan.org/pkg/l3build) required) or
copy *files* into texmf/tex/lualatex/luadraw/ 
and copy *doc* into texmf/doc/lualatex/

* Le paquet *luadraw* définit l'environnement du même nom, celui-ci permet de créer des graphiques mathématiques (2d et 3d) en utilisant le langage Lua. Ces graphiques sont dessinés au final par tikz (et automatiquement sauvegardés), alors pourquoi les faire en Lua ? Parce que celui-ci apporte toute la puissance d'un langage de programmation simple, efficace, capable de faire des calculs, tout en utilisant les possibilités graphiques de tikz.  
Exécutez `l3build install` ([`l3build`](https://ctan.org/pkg/l3build) est requis) ou
 copier le contenu de *files* dans : texmf/tex/lualatex/luadraw/  
 et copier le contenu de *doc*  : texmf/doc/lualatex/
 
**Version 2.8**

* Added the *luadraw_decorations* extension, which enhances certain drawing methods by adding options. Currently, there is *g:Ddecoratedarc()* for 2D arcs and *g:Ddecoratedarc3d()* for 3D arcs.
* Added the *luadraw_coils_chains* extension which allows drawing springs and chains.
* Added the *luadraw_log_axes* extension which allows you to create and draw on a logarithmic grid in *x*, or in *y*, or in *x* and *y*.
* Added functions: *nth_root(n,x)* (nth root of a real *x*, defined on **R** when *n* is odd); *cpx.cosh()*, *cpx.sinh()* (complex hyperbolic cosine and sine) and *cpx.pow(z,a)* (for calculating *z^a* with *z* a complex number and *a* a real number).
* Added the *use_siunitx* option for the *g:Daxes()*, *g:DaxeX()*, *g:DaxeY()*, *g:Dgradbox()*, *g:Dgradline()* methods. This allows you to locally use or not use the formatting of numeric values ​​by the *siunitx* package.
* Added the *showlines* option for the *g:Dgrid()* method which allows you to show or hide horizontal and/or vertical lines.
* For the methods *g:Dpoly()*, *g:Dfacet()*, *g:Dmixfacet()*, and *g:addFacet()*, in the option *usepalette=*{palette, mode}*, the second argument can now be a function, *mode: f -> mode(f)* in **R**, where *f* denotes a facet (a list of 3D points). Facets with the smallest value have the first color of the palette, those with the smallest value have the last color of the palette, and for the others, the color is calculated by linear interpolation.
* Added the function *obj_surface(f,u1,u2,v1,v2, grid)* which returns the surface parameterized by *f* in *obj* format, that is, a table with three fields {vertices={...}, facets={{...},{...}}}, normals={...}}. The first two fields are identical to the case of polyhedra, and the third field contains the unit vectors normal to the surface at each vertex. The facets are triangular.
* In the *luadraw_povray* module: a second syntax has been added for drawing smooth parametric surfaces: *g:Pov_surface(f,u1,u2,v1,v2,options)* where *f* is the parameterization. This method is faster than the previous one.
* In the *{luadraw_spherical* module: add the global variable *Hiddendelayed = false*. With the value *false*, hidden parts are drawn at the end of the *g:Dspherical()* instruction; with the value *true*, they are drawn at the very end of the current graph, which can be useful if you have added elements after the sphere that hide part of it.
* For the *g:Daxes()* method: the \opt{originloc} option is still the point used as the origin for the graduations, but it is no longer automatically the point of intersection of the two axes.
* For the method *g:Daxes()*: addition of the options *xynode_options = ""*, *xnode_options = xynode_options* and *ynode_options = xynode_options* which allow passing options to the instruction *\node{}* for all labels (except legends).
* For the *g:addAaxes()* method, add the option *labels={"$x$", "$y$","$z$"}* to manage the labels displayed at the end of each axis.
* Bug fixes...
 
**Version 2.7**
* The basic solid drawing methods: *g:Dcylinder()*, *g:Dcone()* and *g:Dfrustum()* now have two additional options: *edgestyle* and *edgewidth* (as for the *g:Dsphere()* method).
* In the *luadraw_compile_tex* module, for the methods: *g:Dcompiled_tex(L, anchor, options)* and *g:Compiled\_tex2path3d(L, options)*, add the option *pos* identical to the labels.
* In the *luadraw_compile_tex* module, three global variables are added to manage access to the *pdflatex*, *pdf2ps* and *pstoedit* programs. These variables are *pdflatexcmd*, *pdf2pscmd* and *pstoeditcmd*, they allow you to optionally add a path to the program, for example: *pstoeditcmd = "/usr/bin/pstoedit"*.
* New syntax for the function *circle(data, nbdots)*, where *data* is a list (center and radius, or three points on the circle) and *nbdots* is the desired number of points. The old syntaxes remain valid.
* New syntax for the function *ellipse(data, nbdots)*, where *data* is a list: {center, rx, ry, incline}, and *nbdots* is the desired number of points. The old syntaxes remain valid.
* Added the function *mixpalette(pal, percent, color)* which returns a new palette after mixing each color of the palette *pal* with *color*.
* Correction of numerous typos in the documentation.
* Bug fixes...
 
**Version 2.6**
* Added the *luadraw_povray* extension which allows you to create an image with Pov-Ray and include it in the current graphic to draw over it.
* Added the *luadraw_fields* extension which allows drawing vector fields or gradient fields.
* Added the *luadraw_shadedforms* extension, which allows drawing 2D polygonal lines or filling a shape with a color gradient based on the chosen calculation method and palette.
* The *g:Dshadedpolyline()* method is now part of the *luadraw_shadedforms* extension.    
* Added the methods *g:Dimage()* and *g:Dmapimage()*. The first allows you to include an image in the graph, and the second allows you to map an image onto a parallelogram.
* Added the function *parallelogram()*, which returns the vertices of a parallelogram constructed from a vertex and two vectors. Also added is the corresponding drawing method *g:Dparallelogram()*.
* Added the options *gradside* and *gradsection* which allow modification of gradient parameters in the drawing of cylinders, cones and truncated cones (methods *g:Dcylinder()*, *g:Dcone()* and *g:Dfrustum()*).
* Extension of the method *g:Newcolor(name,color)*, the second argument can now be eiher a table of three RGB components, or a string representing a color.
* Bug fixes...
