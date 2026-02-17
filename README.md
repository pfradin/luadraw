# Package luadraw for LuaLaTeX version 2.6

* The *luadraw* package defines the environment of the same name, which lets you create mathematical graphs (2d and 3d) using the Lua language. These graphs are ultimately drawn by tikz (and automatically saved), so why make them in Lua? Because Lua brings all the power of a simple, efficient programming language, capable of performing calculations, using tikz's graphics capabilities.  
Run `l3build install` ([`l3build`](https://ctan.org/pkg/l3build) required) or
copy *files* into texmf/tex/lualatex/luadraw/ 
and copy *doc* into texmf/doc/lualatex/

* Le paquet *luadraw* définit l'environnement du même nom, celui-ci permet de créer des graphiques mathématiques (2d et 3d) en utilisant le langage Lua. Ces graphiques sont dessinés au final par tikz (et automatiquement sauvegardés), alors pourquoi les faire en Lua ? Parce que celui-ci apporte toute la puissance d'un langage de programmation simple, efficace, capable de faire des calculs, tout en utilisant les possibilités graphiques de tikz.  
Exécutez `l3build install` ([`l3build`](https://ctan.org/pkg/l3build) est requis) ou
 copier le contenu de *files* dans : texmf/tex/lualatex/luadraw/  
 et copier le contenu de *doc*  : texmf/doc/lualatex/
 
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
