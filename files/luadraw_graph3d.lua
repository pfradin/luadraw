-- luadraw_graph3d.lua
-- date 2025/09/07
-- version 2.1
-- Copyright 2025 Patrick Fradin
-- This work may be distributed and/or modified under the
-- conditions of the LaTeX Project Public License.
-- The latest version of this license is in
--   http://www.latex-project.org/lppl.txt.

-- ce module ajoute les bases du dessin 3d

require 'luadraw_matrix3d' -- charge également la classe pt3d
require 'luadraw_transformations3d'
require 'luadraw_lines3d'
require 'luadraw_build3d'
local Tscene3d = require 'luadraw_scene3d'
local Scene3d 

Hiddenlines = false
Hiddenlinestyle = "dotted"
top, right, bottom, left = 8, 4, 2, 1
mWireframe, mFlat, mFlatHidden, mShaded, mShadedHidden, mShadedOnly = 0, 1, 2, 3, 4, 5
mGrid = 1 -- cylinder, sphere, cone
mBorder = 2 -- sphere
--MODE_WIREFRAME = 0
--MODE_FLAT = 1
--MODE_FLAT_HIDDEN_EDGES = 2
--MODE_SHADED = 3
--MODE_SHADED_HIDDEN_EDGES = 4
--MODE_SHADED_NO_EDGE = 5


local luadraw_graph2d = require "luadraw_graph2d"

local luadraw_graph3d = {}
setmetatable(luadraw_graph3d, {__index = luadraw_graph2d}) -- obligatoire pour l'héritage

--- Constructeur
function luadraw_graph3d:new(args) -- argument de la forme :
-- {window3d={x1,x2,y1,y2,z1,z2}, viewdir={30,60}, adjust2d=true/false, window={x1,x2,y1,y2,xscale,yscale}, margin={left, right, top, bottom}, size={large, haut, ratio}, bg="color", border = true/false, bbox=true/false}
    local graph3d = luadraw_graph2d:new(args) -- obligatoire, on utilise le constructeur de luadraw_calc
    args.adjust2d = args.adjust2d or false -- adjust2d= false or true
    args.viewdir = args.viewdir or {30,60}
    setmetatable(graph3d, {__index = luadraw_graph3d})  -- obligatoire, permet d'utiliser self
    graph3d.param["viewport3d"] = args.window3d or {-5,5,-5,5,-5,5}
    if args.viewdir == "xOy" then args.viewdir = {-90,0}
    elseif args.viewdir == "yOz" then args.viewdir = {0,90}
    elseif args.viewdir == "xOz" then args.viewdir = {-90,90}
    end
    graph3d.param["viewdir"] = args.viewdir -- viewdir theta et phi en degrés
    local a, b = args.viewdir[1]*deg, args.viewdir[2]*deg
    graph3d.cosTheta = math.cos(a) -- pour accélérer les calculs, les cos et sin ne sont calculés qu'une fois
    graph3d.sinTheta = math.sin(a)
    graph3d.cosPhi = math.cos(b)
    graph3d.sinPhi = math.sin(b)
    graph3d.Normal = M(graph3d.cosTheta*graph3d.sinPhi, graph3d.sinTheta*graph3d.sinPhi, graph3d.cosPhi)
    graph3d.matrix3d = ID3d
    if args.adjust2d  then 
        local x1,x2,y1,y2,z1,z2 = table.unpack(graph3d.param.viewport3d)
        local lg, ht = (graph3d.Xmax-graph3d.Xmin)*graph3d.Xscale, (graph3d.Ymax-graph3d.Ymin)*graph3d.Yscale
        local box = parallelep( M(x1,y1,z1), (x2-x1)*vecI, (y2-y1)*vecJ, (z2-z1)*vecK ) -- boite 3d
        local L = {}
        for _, A in ipairs(box.vertices) do -- projection des sommets de la boite sur le plan 2d
            table.insert(L, Z(graph3d.cosTheta*A.y-graph3d.sinTheta*A.x, -graph3d.cosPhi*graph3d.cosTheta*A.x-graph3d.cosPhi*graph3d.sinTheta*A.y+graph3d.sinPhi*A.z))
        end
        x1, x2, y1, y2 = getbounds( L )
        x1 = x1-(x2-x1)/5; x2 = x2+(x2-x1)/20
        y1 = y1-(y2-y1)/5; y2 = y2+(y2-y1)/20
        --x1 = math.floor(x1); x2 = math.ceil(x2)
        --y1 = math.floor(y1); y2 = math.ceil(y2)
        graph3d.param.viewport = {x1,x2,y1,y2} -- redimensionnement de la vue 2d
        graph3d.param.coordsystem = {x1,x2,y1,y2} -- redimensionnement de la vue utilisateur
        graph3d.advancedparam = table.copy(graph3d.currentparam)
        graph3d.deferredparam = table.copy(graph3d.currentparam)
        graph3d.Xmin = x1; graph3d.Xmax = x2; graph3d.Ymin = y1; graph3d.Ymax = y2
        local ratio
        if args.size ~= nil then ratio = args.size[3] end
        local Ratio = ratio or (graph3d.Xscale / graph3d.Yscale)
        if lg > 0 then
            graph3d.Xscale = lg / (graph3d.Xmax-graph3d.Xmin)
        end
        if ht > 0 then
            graph3d.Yscale = ht / (graph3d.Ymax-graph3d.Ymin)
        end
        if Ratio > 0 then
            local aux = Ratio*graph3d.Yscale
            if aux > graph3d.Xscale then graph3d.Yscale = graph3d.Xscale/Ratio
            else graph3d.Xscale = aux
            end
        end    
    end
    -- infos sur le graphe
    print("3d window = ", table.unpack(graph3d.param.viewport3d))
    print("viewdir = ", table.unpack(graph3d.param.viewdir))
    print("2d window = ", table.unpack(graph3d.param.viewport))
    return graph3d
end

-- calcul matriciel

function luadraw_graph3d:Savematrix()
    table.insert(self.pilematrix, table.copy(self.matrix))
    table.insert(self.pilematrix, table.copy(self.matrix3d))
end

function luadraw_graph3d:Restorematrix()
    self.matrix3d = table.remove(self.pilematrix)
    self.matrix = table.remove(self.pilematrix)
end

function luadraw_graph3d:Det3d()
-- renvoie +1 ou -1 suivant que le déterminant de la matrice de transformation 3d est positif ou négatif
    local o,u,v,w = table.unpack(self.matrix3d)
    if pt3d.det(u,v,w) > 0 then 
        return 1
    else return -1
    end
end

-- sauvegarde et restauration des paramètres graphiques (fenêtre, styles, matrices)
function luadraw_graph3d:Saveattr()
    table.insert(self.pile, table.copy(self.matrix3d))
    table.insert(self.pile, table.copy(self.matrix))
    table.insert(self.pile, table.copy(self.param))
    self:Writeln("\\begin{scope}")
end

function luadraw_graph3d:Restoreattr()
    self.param = table.remove(self.pile)
    self.matrix = table.remove(self.pile)
    self.matrix3d = table.remove(self.pile)    
    self:Writeln("\\end{scope}")
end

function luadraw_graph3d:IDmatrix3d()
    self.matrix3d = ID3d
end

function luadraw_graph3d:Setmatrix3d(M)
    self.matrix3d = M
end

function luadraw_graph3d:Composematrix3d(M)
    self.matrix3d = composematrix3d(self.matrix3d,M)
end

function luadraw_graph3d:Mtransform3d(L)
    return mtransform3d(L,self.matrix3d)
end

function luadraw_graph3d:MLtransform3d(L)
    return mLtransform3d(L,self.matrix3d)
end

function luadraw_graph3d:Shift3d(V)
    self:Composematrix3d({V,vecI,vecJ,vecK})
end

function luadraw_graph3d:Rotate3d(angle,axe)
-- angle en degrés
-- axe = {A,u} axe de la rotation orienté par u
    self:Composematrix3d(matrix3dof(function(M) return rotate3d(M,angle,axe) end))
end

function luadraw_graph3d:Scale3d(k,center)
-- homothétie de center center (origine par défaut)
-- et de rapport k
    self:Composematrix3d(matrix3dof(function(M) return scale3d(M,k,center) end))
end

-- vue
function luadraw_graph3d:Viewport3d(x1,x2,y1,y2,z1,z2)
    if (x1 == nil) or (x2 == nil) or (y1 == nil) or (y2 == nil) or (z1 == nil) or (z2 == nil) then return end
    if x1 > x2 then x1, x2 = x2, x1 end
    if y1 > y2 then y1, y2 = y2, y1 end
    if z1 > z2 then z1, z2 = z2, z1 end
    self.param.matrix3d = ID3d -- ancienne matrice perdue donc il faut sauver avant
    self.param.viewport3d = {x1,x2,y1,y2,z1,z2}
end

function luadraw_graph3d:Setviewdir(theta,phi) -- direction de l'observateur avec theta et phi en degrés
    if (theta == nil) then return end
    if theta == "xOy" then theta = -90; phi = 0
    elseif theta == "yOz" then theta = 0; phi = 90
    elseif theta == "xOz" then theta = -90; phi = 90
    end
    self.param.viewdir = {theta,phi} 
    local a, b = theta*deg, phi*deg
    self.cosTheta = math.cos(a) -- pour accélérer les calculs, les cos et sin ne sont calculés qu'une fois
    self.sinTheta = math.sin(a)
    self.cosPhi = math.cos(b)
    self.sinPhi = math.sin(b)
    self.Normal = M(self.cosTheta*self.sinPhi, self.sinTheta*self.sinPhi, self.cosPhi)
end

function luadraw_graph3d:Getviewdir() -- renvoie la direction de l'observateur avec angles theta et phi en degrés
    return self.param.viewdir
end

function luadraw_graph3d:ScreenX()
-- renvoie les coordonnées spatiales du premier vecteur de base du plan de l'écran (affixe 1)
-- c'est l'image du vecteur vecJ par la rotation d'axe Oz et d'angle theta
    return M(-self.sinTheta, self.cosTheta,0)
end

function luadraw_graph3d:ScreenY()
-- renvoie les coordonnées spatiales du deuxième vecteur de base du plan de l'écran (affixe i)
-- c'est le produit vectoriel entre les vecteurs self.Normal et self:ScreenX()
    return M(-self.cosPhi*self.cosTheta, -self.cosPhi*self.sinTheta, self.sinPhi)
end

function luadraw_graph3d:ScreenPos(z,d)
-- renvoie les coordonnées spatiales du vecteur ayant comme projeté sur l'écran le point d'affixe z,
-- et se trouvant à une distance d (algébrique) du plan de l'écran
    local A = z.re*self:ScreenX() + z.im*self:ScreenY()
    d = d or 500
    return A + d*self.Normal
end

function luadraw_graph3d:Box3d()
-- renvoie la fenêtre 3d courante sous forme d'un polyèdre
    local x1,x2,y1,y2,z1,z2 = table.unpack(self.param.viewport3d)
    return parallelep(M(x1,y1,z1), (x2-x1)*vecI, (y2-y1)*vecJ, (z2-z1)*vecK)
end

-- projection sur l'écran

function luadraw_graph3d:Proj3d(L) -- projection sur l'écran (plan passant par l'origine et normal au vecteur self.Normal
-- L est un point3d ou une liste de point3d ou une liste de listes de point3d
    local projAdot = function(A)
    -- le projeté d'un vecteur u sur le plan de l'écran est <u|ScreenX>.ScreenX + <u|ScreenY>.ScreenY
    -- c'est donc le point d'affixe <u|ScreenX> + i*<u|ScreenY> sur l'écran
    -- le projeté d'un point A est le projeté du vecteur OA
        if isPoint3d(A) then -- on ne projette que les points 3d
            return Z(self.cosTheta*A.y-self.sinTheta*A.x, -self.cosPhi*self.cosTheta*A.x-self.cosPhi*self.sinTheta*A.y+self.sinPhi*A.z)
        else
            return A  --le reste est renvoyé tel quel
        end
    end
    if (L == nil) or (type(L) ~= "table") then return end
    if not isID3d(self.matrix3d) then
        L = mtransform3d(L,self.matrix3d)
    end
    local rep
    if isPoint3d(L) then rep = projAdot(L) -- un seul point
    elseif isPoint3d(L[1]) then -- liste de points
        rep = {}
        for _, A in ipairs(L) do
            table.insert(rep, projAdot(A))
        end
    else --liste de listes
        rep = {}
        for _, cp in ipairs(L) do
            local aux = {}
            for _,A in ipairs(cp) do
                table.insert(aux, projAdot(A))
            end
            table.insert(rep, aux)
        end
    end
   return rep 
end


------ dessins de lignes 3d

function luadraw_graph3d:Dpolyline3d(L,close,draw_options)
    local aux = self:Proj3d(L)
    self:Dpolyline(aux,close,draw_options)
end

function luadraw_graph3d:Dline3d(d,B,draw_options)
--trace la droite d (si B=nil) ou bien la droite passant par les points d et B
    if type(B) == "string" then draw_options = B; B = nil end
    local A, u
    if B == nil then 
        A = d[1]; u = d[2]
    else
        A = d; u = B-A
    end
    self:Dline({self:Proj3d(A),self:Proj3d(u)},draw_options)
end

function luadraw_graph3d:Dseg3d(seg,scale,draw_options)
--trace le segment seg 
    self:Dseg(self:Proj3d(seg),scale,draw_options)
end

function luadraw_graph3d:Dparametric3d(p,args)
-- dessin d'une courbe paramétrée par la fonction p:t -> p(t) sur l'intervalle [t1;t2] (à valeurs dans R^3)
-- args est une table à 5 entrées args = { t = {t1,t2}, nbdots = 50, discont =false, nbdiv = 5, draw_options = "" }
    args = args or {}
    local t = args.t or {self:Xinf(), self:Xsup()}
    local nbdots = args.nbdots or 50
    local discont = args.discont or false
    local nbdiv = args.nbdiv or 5
    local draw_options = args.draw_options or ""
    if draw_options == "" then draw_options = "line join=round"
    else draw_options = "line join=round,"..draw_options end -- jointure arrondie
    local t1, t2 = table.unpack(t)
    if t1 > t2 then t1, t2 = t2, t1 end
    local C = parametric3d(p,t1,t2,nbdots,discont,nbdiv)
    self:Dpolyline3d(C,false,draw_options)
end

function luadraw_graph3d:Darc3d(B,A,C,R,sens,normal, draw_options)
-- dessine un arc de cercle de centre A, dans le plan ABC, de AB vers AC.
-- ce plan est orienté par le vecteur AB^AC ou le vecteur normal s'il est précisé
    if type(normal) == "string" then draw_options = normal; normal = nil end
    local chem = arc3db(B,A,C,R,sens,normal)
    self:Dpath(self:Proj3d(chem),draw_options)
end    


function luadraw_graph3d:Dcircle3d(C,R,normal, draw_options)
-- dessine un cercle de centre C de rayon R.
-- dans le plan défini par C et le vecteur normal
    if R == 0 then self:Ddots3d(C)
    else
        local chem = circle3db(C,R,normal)
        self:Dpath(self:Proj3d(chem),draw_options)
    end
end

function luadraw_graph3d:Dangle3d(B,A,C,r,draw_options)
    if type(r) == "string" then draw_options = r; r = nil end
    r = r or 0.25
    local u, v = B-A, C-A
    u, v = r*pt3d.normalize(u), r*pt3d.normalize(v)
    return self:Dpolyline3d( {A+u,A+u+v,A+v}, draw_options)
end


-------- points et labels

function luadraw_graph3d:Ddots3d(L, mark_options)
    self:Ddots(self:Proj3d(L), mark_options)
end

function luadraw_graph3d:Dballdots3d(L,color,scale) -- points sphériques
    if L == nil then return end
    if isPoint3d(L) then L = {L} end
    color = color or "black"
    scale = scale or 1
    local r = 0.075*scale
    L = self:Proj3d(L)
    if not isID(self.matrix) then L = self:Mtransform(L) end
    for _, z in ipairs(L) do
        self:Writeln("\\fill[opacity=1,ball color="..color.."] "..self:Coord(z).." circle [radius="..strReal(r).."cm];")
    end
end

function luadraw_graph3d:Dcrossdots3d(L,color,scale,angle) -- points en forme de croix dans un plan
-- L est une liste du type {point 3d, vecteur normal} ou { {point3d, vecteur normal}, {point3d, vecteur normal}, ...}
    color = color or self.param.linecolor
    scale = scale or 1
    angle= angle or 0
    local long = 0.125*scale
    local lg, A, normal, a, b, c = {}    
    
    local calcAdot = function()
        local n = pt3d.normalize(normal)
        local u = pt3d.prod(n,self.Normal)
        if pt3d.isNul(u) then u = self:ScreenX() end
        local v = pt3d.prod(n,u)
        if angle ~=0 then
            u, v = table.unpack( rotate3d({u,v},angle,{A,n}) )
        end
        a, b, c = table.unpack(self:Proj3d({A,u-v,u+v}))
        b, c = long*b/self:Abs(b), long*c/self:Abs(c)
    end
    
    local oldcolor = self.param.linecolor
    local oldstyle = self.param.linestyle
    local oldwidth = self.param.linewidth
    local oldfillstyle = self.param.fillstyle
    self:Lineoptions("solid",color,4); self:Filloptions("none")
    if isPoint3d(L[1]) then L = {L} end
    for _, P in ipairs(L) do
        A = P[1]; normal = P[2]; calcAdot()
        insert(lg, {{a+b,a-b},{a-c,a+c}})
    end
    self:Dpolyline(lg)
    self:Lineoptions(oldstyle,oldcolor,oldwidth); self:Filloptions(oldfillstyle)
end

function luadraw_graph3d:Dlabel3d(...)
    local args = {}
    for k, aux in ipairs{...} do
        if  k%3 == 2 then aux = self:Proj3d(aux) end
        if k%3 == 0 then --arguments
            if aux.dir ~= nil then
                aux.dir = self:Proj3d(aux.dir)
            end
        end
        table.insert(args,aux)
    end
    self:Dlabel(table.unpack(args))
end

------- solides sans facettes (fil de fer)

function luadraw_graph3d:Dcylinder(A,r,V,B,args) 
-- ou Dcylinder(A,r,B,args): cylindre droit de A vers B
-- ou Dcylinder(A,V,r,args): ancienne syntaxe, 
-- dessine un cylindre en fil de fer
-- A est le centre d'une face circulaire de rayon r orthogonale au vecteur V
-- l'autre face a pour centre B
-- args est une table à 6 champs :
-- {mode =0/1, hiddenstyle="dotted", hiddencolor = linecolor, edgecolor=linecolor, color="", opacity=1}
-- mode = 0 fil de fer
-- mode = 1 grille
-- color = "" : pas de remplissage, color ~= "" remplissage avec ball color
    if isPoint3d(r) then -- ancienne syntaxe A,V,r,args
        local R = r
        r = V; V = R; args = B; B = A+V
    elseif not isPoint3d(B) then -- syntaxe A,r,B,args
        args = B; B = V; V = B-A
    end
    args = args or {}
    args.color = args.color or ""
    args.edgecolor = args.edgecolor or self.param.linecolor
    args.hiddencolor = args.hiddencolor or args.edgecolor
    args.hiddenstyle = args.hiddenstyle or Hiddenlinestyle
    args.mode = args.mode or 0
    args.opacity = args.opacity or 1
    
    local oldfillstyle = self.param.fillstyle
    local oldfillopacity = self.param.fillopacity
    local oldfillcolor = self.param.fillcolor
    local oldlinestyle = self.param.linestyle
    local oldlineopacity = self.param.lineopacity
    local oldlinecolor = self.param.linecolor
    local oldlinewidth = self.param.linewidth
    self:Linecolor(args.edgecolor)
    local mat = self.matrix3d
    local N = mLtransform3d(self.Normal,invmatrix3d(mat))
    if pt3d.dot(self.Normal,self:MLtransform3d(V)) <= 0 then V = -V  end
    if pt3d.dot(self:MLtransform3d(V),self:MLtransform3d(B-A)) < 0 then A,B=B,A  end -- V et B-A dans le même sens
    local I = pt3d.normalize(V) -- vecteur normal au plan et dans la direction du sommet B
    local J = pt3d.prod(I,N); J = pt3d.normalize(J)
    if (J == nil) then -- le plan de la base circulaire est l'écran
        J = self:ScreenX()
    end
    local K = pt3d.prod(I,J) -- base = {A+r.cos(t)J+r.sin(t)K / t in [-pi,pi]}
    local xn,yn, zn = pt3d.dot(N,I), pt3d.dot(N,J), pt3d.dot(N,K)
    local W = B-A
    local xw,yw,zw = pt3d.dot(W,I), pt3d.dot(W,J), pt3d.dot(W,K)
    local t = solve(function(t) return math.sin(t)*(xn*zw-zn*xw)+math.cos(t)*(xn*yw-xw*yn) end,-math.pi/2,3*math.pi/2)
        if (t == nil) or (#t == 1) then
        if args.color ~= "" then
            self:Dcircle3d(A,r,V,"left color=white,right color="..args.color..",fill opacity="..args.opacity)
        else
            self:Filloptions("none") 
            self:Dcircle3d(A,r,V)
        end
    else
        local angle = self:Arg(self:Proj3d(W))*rad
        if angle < 0 then angle = angle+180
        elseif angle > 180 then angle = angle-180 
        end
        t1, t2 = table.unpack(t) 
        local M1 = A+math.cos(t1)*r*J+math.sin(t1)*r*K
        local M2 = A+math.cos(t2)*r*J+math.sin(t2)*r*K
        if math.cos(t1)*math.sin(t2)-math.cos(t2)*math.sin(t1)< 0 then M1,M2 = M2,M1 end
        local N1 = M1+W
        local N2 = M2+W
        if pt3d.N1(M2-M1) < 1e-12 then -- points confondus
            g:Dcircle3d(A,r,V,"left color=white,right color="..args.color..",fill opacity="..args.opacity)
        else
            if args.color == "" then 
                self:Filloptions("none") 
            else
                self:Filloptions("gradient", "left color=white,right color = "..args.color..", shading angle="..strReal(angle),args.opacity)
            end
            if args.mode == 1 then self:Linestyle("noline") end
            -- on voit la base circulaire en B, le vecteur I sort du cylindre en B et est dirigé vers l'observateur
            local sens = 1
            --if pt3d.det(W,A-M1,M2-M1)*pt3d.det(W,B-N1,N2-N1) < 0 then sens = -sens end
            if cpx.det(self:Proj3d(B-A),self:Proj3d(B-N2))*self:Det3d() < 0 then sens = -sens end
            self:Dpath3d({M1,A,M2,r,sens,V,"ca",N2,"l",B,N1,r,sens,V,"ca","cl"})
            self:Filloptions("none")
            self:Darc3d(N1,B,N2,r,sens,V)
            if (args.mode ~= 1) and (args.hiddenstyle ~= "noline") then -- partie cachée
                self:Filloptions("none")
                self:Lineoptions(args.hiddenstyle,args.hiddencolor)
                self:Darc3d(M1,A,M2,r,-sens,V)
            end
            --self:Ddots3d({B,N1}); self:Dpolyline3d({B,B+I}); self:Darc3d(B+J,B,B+K,r/2,1,I,"->")
            if args.mode == 1 then -- arêtes
                self:Linestyle(oldlinestyle)
                self:Dpoly(cylinder(A,r,V,B,35,false), {mode=0,hiddenstyle=args.hiddenstyle, edgecolor=args.edgecolor,hiddencolor=args.hiddencolor})
            end
        end
    end
    self:Filloptions(oldfillstyle,oldfillcolor,oldfillopacity)
    self:Lineoptions(oldlinestyle,oldlinecolor,oldlinewidth) 
    self:Lineopacity(oldlineopacity)
end

function luadraw_graph3d:Dcone(C,r,V,A,args) 
-- ou Dcone(C,r,A,args)
-- ou Dcone(A,V,r,args) (ancienne syntaxe)
-- dessine un cône en fil de fer
-- A est le sommet
-- le centre de la face circulaire de rayon r orthogonale au vecteur V est C
-- args est une table à 5 champs :
-- {mode =0/1, hiddenstyle="dotted", hiddencolor = linecolor, edgecolor= linecolor, color="", opacity=1}
-- mode = 0 fil de fer
-- mode = 1 grille
-- color = "" : pas de remplissage, color ~= "" remplissage avec ball color
    if isPoint3d(r) then -- ancien format : sommet, vecteur, rayon, args (cône droit)
        args = A; A = C
        r, V = V, r
        C = A+V
    elseif not isPoint3d(A) then -- format C,r,A,args (cône droit)
            args = A; A = V; V = A-C
    end
    args = args or {}
    args.color = args.color or ""
    args.edgecolor = args.edgecolor or self.param.linecolor
    args.hiddencolor = args.hiddencolor or args.edgecolor
    args.hiddenstyle = args.hiddenstyle or Hiddenlinestyle
    args.mode = args.mode or 0
    args.opacity = args.opacity or 1
    args.mode = args.mode or 0
    local oldfillstyle = self.param.fillstyle
    local oldfillopacity = self.param.fillopacity
    local oldfillcolor = self.param.fillcolor
    local oldlinestyle = self.param.linestyle
    local oldlineopacity = self.param.lineopacity
    local oldlinecolor = self.param.linecolor
    local oldlinewidth = self.param.linewidth
    self:Linecolor(args.edgecolor)
    if pt3d.dot(self:MLtransform3d(V),self:MLtransform3d(A-C)) < 0 then V = -V  end -- V et A-C dans le même sens
    local I = pt3d.normalize(V) -- vecteur normal au plan et dans la direction du sommet A
    local mat = self.matrix3d
    local N = mLtransform3d(self.Normal,invmatrix3d(mat))
    local J = pt3d.prod(I,N); J = pt3d.normalize(J)
    if (J == nil) then -- le plan de la base circulaire est l'écran
        J = self:ScreenX()
    end
    local K = pt3d.prod(I,J) -- base = {C+r.cos(t)J+r.sin(t)K / t in [-pi,pi]}
    local xn,yn, zn = pt3d.dot(N,I), pt3d.dot(N,J), pt3d.dot(N,K)
    local W = C-A
    local xw,yw,zw = pt3d.dot(W,I), pt3d.dot(W,J), pt3d.dot(W,K)
    local t = solve(function(t) return math.sin(t)*(xn*zw-zn*xw)+math.cos(t)*(xn*yw-xw*yn)+r*xn end,0,2*math.pi)
    if (t == nil) or (#t == 1) then
        if args.color ~= "" then
            self:Dcircle3d(C,r,V,"left color=white,right color="..args.color..",fill opacity="..args.opacity)
        else
            self:Filloptions("none") 
            self:Dcircle3d(C,r,V)
        end
    else
        local angle = self:Arg(self:Proj3d(W))*rad
        if angle < 0 then angle = angle+180
        elseif angle > 180 then angle = angle-180 end
        t1, t2 = table.unpack(t) 
        local M1 = C+math.cos(t1)*r*J+math.sin(t1)*r*K
        local M2 = C+math.cos(t2)*r*J+math.sin(t2)*r*K
        if math.cos(t1)*math.sin(t2)-math.cos(t2)*math.sin(t1)< 0 then M1,M2 = M2,M1 end
        if pt3d.N1(M2-M1) < 1e-12 then -- points confondus
            g:Dcircle3d(C,r,V,"left color=white,right color="..args.color..",fill opacity="..args.opacity)
        else
            if args.color == "" then 
                self:Filloptions("none") 
            else
                self:Filloptions("gradient", "left color=white,right color = "..args.color..", shading angle="..strReal(angle),args.opacity)
            end
            if args.mode == 1 then self:Linestyle("noline") end
            local sens = 1
            if cpx.det(self:Proj3d(C-M1),self:Proj3d(M2-M1))*cpx.det(self:Proj3d(A-M1),self:Proj3d(M2-M1)) < 0 then sens = -1 end
            if pt3d.dot(self.Normal,self:MLtransform3d(V)) <= 0 then -- on voit la base circulaire
                self:Dpath3d({A,M1,"l",C,M2,r,sens,V,"ca","cl"})
                self:Filloptions("none")
                self:Darc3d(M1,C,M2,r,-sens,V)
                --print("base vue"); self:Ddots3d({C,M1}); self:Darc3d(C+J,C,C+K,r/2,1,'->')
            else -- on ne voit pas la base circulaire
                self:Dpath3d({A,M1,"l",C,M2,r,sens,V,"ca","cl"})
                if (args.mode ~= 1) and (args.hiddenstyle ~= "noline") then -- partie cachée
                    self:Filloptions("none")
                    self:Lineoptions(args.hiddenstyle,args.hiddencolor)
                    self:Darc3d(M1,C,M2,r,-sens,V)
                    --print("base pas vue"); self:Ddots3d({C,M1}); self:Darc3d(C+J,C,C+K,r/2,1,'->')
                end
            end
            if args.mode == 1 then -- arêtes
                self:Linestyle(oldlinestyle)
                self:Dpoly(cone(C,r,V,A,35,false), {mode=0,hiddenstyle=args.hiddenstyle, edgecolor=args.edgecolor,hiddencolor=args.hiddencolor})
            end
        end
    end
    self:Filloptions(oldfillstyle,oldfillcolor,oldfillopacity)
    self:Lineoptions(oldlinestyle,oldlinecolor,oldlinewidth); 
    self:Lineopacity(oldlineopacity)
end


function luadraw_graph3d:Dfrustum(A,R,r,V,B,args) -- ou Dfrustum(A,R,r,V,args) pour un cône droit 
-- frustum drawn without facets (tronc de cône)
-- dessine un tronc de cône en fil de fer
-- A est le centre de la face de rayon R
-- le centre de l'autre face  C=A+V et son rayon est r
-- args est une table à 5 champs :
-- {mode =0/1, hiddenstyle="dotted", hiddencolor = linecolor, edgecolor=linecolor, color="", opacity=1}
-- mode = 0 fil de fer
-- mode = 1 grille
-- color = "" : pas de remplissage, color ~= "" remplissage avec linéaire
    if R == r then -- cylinder
        if not isPoint3d(B) then self:Dcylinder(A,V,R,B) -- B is args in this case
        else self:Dcylinder(A,R,V,B,args)
        end
        return
    end
    local C
    if isPoint3d(B) then -- slanted frustum
        C = dproj3d(B,{A,V})
        V = C-A
        local U1, U2, U3, h
        U1 = pt3d.normalize(V)
        U2 = pt3d.prod(U1,vecJ)
        if pt3d.N1(U2) < 1e-12 then U2 = pt3d.prod(U1,vecI) end
        U2 = pt3d.normalize(U2)
        U3 = pt3d.prod(U1,U2)
        h = pt3d.abs(V)
        local f = function(m)
            return A + pt3d.dot(m-A,U1)*(B-A)/h + pt3d.dot(m-A,U2)*U2 + pt3d.dot(m-A,U3)*U3
        end
        self:Savematrix()
        self:Composematrix3d( matrix3dof(f) )
    else C = A+V; args = B; B = nil
    end
  
    local dcircle = function()
        if args.color ~= "" then
            self:Filloptions("gradient","left color=white, right color="..args.color,args.opacity)
        else
            self:Filloptions("none")
        end
        self:Dcircle3d(A,R,V)
        if pt3d.dot(self:MLtransform3d(V),self.Normal) >= 0 then
            self:Filloptions("none")
            self:Dcircle3d(C,r,V)
        else
            if (args.mode ~= 1) and (args.hiddenstyle ~= "noline") then -- partie cachée
                self:Filloptions("none")
                self:Lineoptions(args.hiddenstyle,args.hiddencolor)
                self:Dcircle3d(C,r,V)
            end
        end
    end
    args = args or {}
    args.color = args.color or ""
    args.edgecolor = args.edgecolor or self.param.linecolor
    args.hiddencolor = args.hiddencolor or args.edgecolor
    args.hiddenstyle = args.hiddenstyle or Hiddenlinestyle
    args.mode = args.mode or 0
    args.opacity = args.opacity or 1
    args.mode = args.mode or 0
    
    local oldfillstyle = self.param.fillstyle
    local oldfillopacity = self.param.fillopacity
    local oldfillcolor = self.param.fillcolor
    local oldlinestyle = self.param.linestyle
    local oldlineopacity = self.param.lineopacity
    local oldlinecolor = self.param.linecolor
    local oldlinewidth = self.param.linewidth
    self:Linecolor(args.edgecolor)
    if R < r then
        A, C = C, A; V = -V
        R, r = r, R
    end
    local k = R/(R-r)
    local H = V
    local V = k*V
    local S = A+V
    local mat = self.matrix3d
    local N = mLtransform3d(self.Normal,invmatrix3d(mat))
    local I = pt3d.normalize(V) 
    local J = pt3d.prod(I,N); J = pt3d.normalize(J)
    if (J == nil) then -- le plan de la base circulaire est l'écran
        J = self:ScreenX()
    end
    local K = pt3d.prod(I,J) -- base = {A+r.cos(t)J+r.sin(t)K / t in [-pi,pi]}
    local xn,yn, zn = pt3d.dot(N,I), pt3d.dot(N,J), pt3d.dot(N,K)
    local W = A-S
    local xw,yw,zw = pt3d.dot(W,I), pt3d.dot(W,J), pt3d.dot(W,K)
    local t = solve(function(t) return math.sin(t)*(xn*zw-zn*xw)+math.cos(t)*(xn*yw-xw*yn)+R*xn end,0,2*math.pi)
    if (t == nil) or (#t == 1) then
        dcircle()
    else
        local angle = self:Arg(self:Proj3d(W))*rad
        if angle < 0 then angle = angle+180
        elseif angle > 180 then angle = angle-180 end
        t1, t2 = table.unpack(t) 
        local M3 = A+math.cos(t1)*R*J+math.sin(t1)*R*K
        local M4 = A+math.cos(t2)*R*J+math.sin(t2)*R*K
        if math.cos(t1)*math.sin(t2)-math.cos(t2)*math.sin(t1)< 0 then M3,M4 = M4,M3 end
        if pt3d.N1(M3-M4) < 1e-12 then -- points confondus
            dcircle()
        else    
            local M1, M2 = table.unpack( scale3d({M3,M4}, r/R, S) )
            --self:Ddots3d({M1,M2})
            if args.color == "" then 
                self:Filloptions("none") 
            else
                self:Filloptions("gradient", "left color=white,right color = "..args.color..", shading angle="..strReal(angle),args.opacity)
            end
            if args.mode == 1 then self:Linestyle("noline") end
            local sens = 1
            --if cpx.det(self:Proj3d(C-M1),self:Proj3d(M2-M1))*cpx.det(self:Proj3d(A-M1),self:Proj3d(M2-M1)) < 0 then sens = -1 end
            if pt3d.det(I,C-M1,M2-M1)*pt3d.det(I,A-M1,M2-M1) < 0 then sens = -1 end
            if pt3d.dot(self.Normal,self:MLtransform3d(V)) >= 0 then -- on voit la petite base circulaire (C,r)
                self:Dpath3d({M3,M1,"l",C,M2,r,sens,V,"ca",M4,"l",A,M3,R,sens,V,"ca"})
                self:Filloptions("none")
                self:Darc3d(M1,C,M2,r,-sens,V)
                if (args.mode ~= 1) and (args.hiddenstyle ~= "noline") then -- partie cachée
                    self:Filloptions("none")
                    self:Lineoptions(args.hiddenstyle,args.hiddencolor)
                    self:Darc3d(M4,A,M3,R,-sens,V)
                end
            else -- la grande base circulaire (A,R)
                self:Dpath3d({M3,M1,"l",C,M2,r,sens,V,"ca",M4,"l",A,M3,R,sens,V,"ca"})
                self:Filloptions("none")
                self:Darc3d(M4,A,M3,R,-sens,V)            
                if (args.mode ~= 1) and (args.hiddenstyle ~= "noline") then -- partie cachée
                    self:Filloptions("none")
                    self:Lineoptions(args.hiddenstyle,args.hiddencolor)
                    self:Darc3d(M1,C,M2,r,-sens,V)
                end
            end
            if args.mode == 1 then -- arêtes
                self:Linestyle(oldlinestyle) -- la matrice a déjà été changée!
                self:Dpoly(frustum(A,R,r,H,35,false),{mode=0, hiddenstyle=args.hiddenstyle, hiddencolor=args.hiddencolor})
            end
        end
    end
    if B ~= nil then self:Restorematrix() end
    self:Filloptions(oldfillstyle,oldfillcolor,oldfillopacity)
    self:Lineoptions(oldlinestyle,oldlinecolor,oldlinewidth); 
    self:Lineopacity(oldlineopacity)
end


function luadraw_graph3d:Dplane(P,V,L1,L2,mode,draw_options)
-- dessine des bords du plan P={A,u}
-- v doit être un vecteur non nul de ce plan
-- on construit un parallélogramme dont un côté est L1*v/abs(v) et l'autre L2*W/abs(w) où w = u^v
-- le mode indique les bords à dessiner :
-- mode = [top(0/1), right(0/1), bottom(0/1), left(0/1)]_2 (écriture binaire)
    if type(mode) == "string" then draw_options = mode; mode = 15 end
    if mode ==nil then mode = 15 end
    local A, u = table.unpack(P)
    u = pt3d.normalize(u)
    V = pt3d.normalize(V)
    local W = pt3d.prod(u,V)
    V = L1*V
    W = L2*W
    --W = V+W
    local Dep, L = A+W/2-V/2, {}
    if mode & 8 == 8 then table.insert(L, {Dep, Dep+V}) end -- top
    if mode & 4 == 4 then table.insert(L, {Dep+V,Dep+V-W}) end --right
    if mode & 2 == 2 then table.insert(L, {Dep-W,Dep-W+V}) end --bottom
    if mode & 1 == 1 then table.insert(L, {Dep,Dep-W}) end --left
    L = merge3d(L)
    self:Dpolyline3d(L,(mode == 15),draw_options) 
end


function luadraw_graph3d:Dsphere(A,r,args)
-- dessine une sphère en fil de fer
-- A est le sommet, r le rayon
-- args est une table à 5 champs :
-- {mode=0/1/2, hiddenstyle="dotted", hiddencolor = linecolor, edgecolor=linecolor,color="", opacity=1}
-- color = "" : pas de remplissage, color ~= "" remplissage avec ball color
-- si mode 1 : edgestyle = linestyle, edgecolor = linecolor, edgewidth = linewidth
-- mode = 0 contour avec équateur
-- mode = 1 contour avec méridiens et fuseaux
-- mode = 2 contour seulement (cercle)
    args = args or {}
    args.color = args.color or ""
    args.edgecolor = args.edgecolor or self.param.linecolor
    args.hiddencolor = args.hiddencolor or args.edgecolor
    args.hiddenstyle = args.hiddenstyle or Hiddenlinestyle
    args.edgestyle = args.edgestyle or self.param.linestyle
    args.edgecolor = args.edgecolor or self.param.linecolor
    args.edgewidth = args.edgewidth or self.param.linewidth    
    args.mode = args.mode or 0
    args.opacity = args.opacity or 1
    
    local oldfillstyle = self.param.fillstyle
    local oldfillopacity = self.param.fillopacity
    local oldfillcolor = self.param.fillcolor
    local oldlinestyle = self.param.linestyle
    local oldlineopacity = self.param.lineopacity
    local oldlinecolor = self.param.linecolor
    local oldlinewidth = self.param.linewidth
    self:Linecolor(args.edgecolor)
    local V = (3*self:ScreenY()+self.Normal)/4
    if args.color ~= "" then
        self:Filloptions("gradient", "ball color="..args.color, args.opacity)
    else
        self:Filloptions("none")
    end
    --self:Dcircle(self:Proj3d(A),r)
    local mat = invmatrix3d( self.matrix3d )
    local N = mLtransform3d(self.Normal,mat)
    self:Dcircle3d(A,r,N)
    if  args.mode == 0 then -- équateur
        local u = pt3d.normalize(pt3d.prod(N,V))
        local M1, M2 = A+r*u, A-r*u
        self:Filloptions("none"); self:Lineoptions(args.edgestyle,args.edgecolor,args.edgewidth)
        self:Darc3d(M1,A,M2,r,1,V)
        self:Lineoptions(args.hiddenstyle,args.hiddencolor)
        self:Darc3d(M1,A,M2,r,-1,V)
    elseif args.mode == 1 then -- grille
        self:Dpoly(sphere(A,r),{mode=0,hiddenstyle=args.hiddenstyle,hiddencolor=args.hiddencolor,edgestyle=args.edgestyle,edgecolor=args.edgecolor,edgewidth=args.edgewidth})
    end
    self:Filloptions(oldfillstyle,oldfillcolor,oldfillopacity)
    self:Lineoptions(oldlinestyle,oldlinecolor,oldlinewidth); 
    self:Lineopacity(oldlineopacity)
end


------ dessins de facettes

function luadraw_graph3d:Isvisible(facet)
-- facet est une liste de points 3d coplanaires
-- la fonction renvoie true si la facette est visible (vecteur normal de même sens que n)
    local N = pt3d.prod(facet[2]-facet[1], facet[3]-facet[1])
    return pt3d.dot(N,self.Normal) > 0
end

function luadraw_graph3d:Classifyfacet(F)
-- F est une liste de facettes ou un polyèdre
-- la fonction renvoie 2 listes : les facettes visibles, et les facettes cachées
    local list, list2
    if F.vertices ~= nil then list = poly2facet(F) else list = F end
    if not isID3d(self.matrix3d) then list2 = self:Mtransform3d(list) else list2 = list end
    local V, H = {}, {}
    for k,facet in ipairs(list2) do
        if self:Isvisible(facet) then table.insert(V,list[k])
        else
            table.insert(H,list[k])
        end
    end
    return V, H
end

function luadraw_graph3d:Sortfacet(F,backculling)
-- F est une liste de facettes avec coordonnées 3d
-- la fonction trie les facettes suivant la côte du centre de gravité
-- le long de l'axe (O,n); de la plus petite côte à la plus grande
-- backculling = true/false (élimine les facettes non visibles)
-- la fonction renvoie une liste de facettes avec coordonnées 3d
    if #F == 1 then return F end
    local F1 = F
    if not isID3d(self.matrix3d) then F1 = self:Mtransform3d(F) end
    backulling = backculling or false
    local rep, aux = {}, {}
    for k,L in ipairs(F1) do -- on travaille sur les sommets transformés
        local G1 = isobar3d(L)
        table.insert(aux, {k,pt3d.dot(G1,self.Normal)})
    end
    table.sort(aux, function(e1,e2) return ((e1[2]<e2[2])) end)
    for _, L in ipairs(aux) do
        if (not backculling) or self:Isvisible(F1[L[1]]) then
            table.insert(rep, F[L[1]])
        end
    end
    return rep -- liste de facettes avec coordonnées 3d, triées, non transformés par la matrice 3d
end

function luadraw_graph3d:Sortpolyfacet(P,backculling)
-- P est un polyèdre P={ vertices={point3d}, facets={faces avec n°s de sommets} }
-- la fonction trie les facettes suivant la côte du centre de gravité
-- le long de l'axe (O,n); de la plus petite côte à la plus grande
-- backculling = true/false (élimine les facettes non visibles)
-- la fonction renvoie une liste de facettes avec coordonnées 3d non transformées
    return self:Sortfacet(poly2facet(P),backculling)
end

function luadraw_graph3d:Edges(P)
-- P est un polyèdre P={ vertices={point3d}, facets={faces avec n°s de sommets} }
-- la fonction renvoie la liste des arêtes, sous la forme :
-- { visible= {{A,B},...}, hidden = {{C,D},...} }, les deux champs sont des lignes polygonales 3d (arêtes "mergées")
    local P1 = {}
    P1.facets = P.facets
    if not isID3d(self.matrix3d) then 
        P1.vertices = mtransform3d(P.vertices,self.matrix3d)
        else P1.vertices = P.vertices end
    local rep = {} -- contiendra la liste des arêtes sous la forme { ab = {{a,b},true/false} ...}
    
    local build = function(F) -- transforme une facette F avec n° de sommets en facette avec coordonnées 3d transformées
        return { P1.vertices[F[1]], P1.vertices[F[2]], P1.vertices[F[3]] }
    end
    
    local inserer = function(arete,visible) -- arete = {a,b} deux entiers n° de sommets, visible =true/false
        local a, b = table.unpack(arete) -- a et b sont des numéros de sommets, on les range dans l'ordre croissant
        if a > b then a, b = b, a end
        local ab = a..";"..b -- pour servir de clé
        if rep[ab] ~= nil then rep[ab][2] = rep[ab][2] or visible
        else
            rep[ab] = {{a,b},visible} -- arête avec coordonnées 3d non transformées
        end
    end
    local facet, a, b, visible
    for _,facet in ipairs(P.facets) do -- parcours du polyèdre par facette
        visible = self:Isvisible(build(facet)) -- on teste la face avec sommets transformés
        b = facet[1]
        for k = 2, #facet do
            a = b
            b = facet[k]
            inserer({a,b},visible)
        end
        inserer({b,facet[1]},visible)
    end
    local aux = {}
    aux.visible = {}; aux.hidden = {}
    for _,aret in pairs(rep) do
        a, b = table.unpack(aret[1]) -- n° des sommets formant l'arête
        if aret[2] then -- arête visible
            table.insert(aux.visible,{a,b})
        else
            table.insert(aux.hidden,{a,b})
        end
    end
    local V, H = merge(aux.visible), merge(aux.hidden)
    rep = {}
    rep.visible = {}; rep.hidden = {}
    for _,F in ipairs(V) do
        local cp = {}
        for _, k in ipairs(F) do
            table.insert(cp, P.vertices[k]) -- conversion en point 3d (non transformés)
        end
        table.insert(rep.visible,cp)
    end
    for _,F in ipairs(H) do
        cp = {}
        for _, k in ipairs(F) do
            table.insert(cp, P.vertices[k])  
        end
        table.insert(rep.hidden,cp)
    end
    return rep
end


function luadraw_graph3d:Outline(P)
-- P est un polyèdre P={ vertices={point3d}, facets={faces avec n°s de sommets} }
-- la fonction renvoie le contour de P, sous forme d'une liste d'arêtes :
-- { visible= {{A,B},...}, hidden = {{C,D},...} }, les deux champs sont des lignes polygonales 3d (arêtes "mergées")
    local P1 = {}
    if P.facets == nil then -- ce n'est pas un polyèdre, donc une liste de facettes
        P = facet2poly(P)
    end
    P1.facets = P.facets
    if not isID3d(self.matrix3d) then P1.vertices = mtransform3d(P.vertices,self.matrix3d) else P1.vertices = P.vertices end
    local rep = {} -- contiendra la liste des arêtes sous la forme { ab = {{a,b},true/false} ...}
    
    local inserer = function(arete,visible) -- arete = {a,b} deux entiers n° de sommets, visible =true/false
        local a, b = table.unpack(arete) -- a et b sont des numéros de sommets, on les range dans l'ordre croissant
        if a > b then a, b = b, a end
        local ab = a..";"..b -- pour servir de clé
        if rep[ab] ~= nil then 
            if (rep[ab][2] == visible) then rep[ab]=nil 
            else
                rep[ab][2] = true
            end
        else
            rep[ab] = {{a,b},visible} -- arête avec coordonnées 3d non transformées
        end
    end
    local facet, a, b, visible
    for _,facet in ipairs(P.facets) do -- parcours du polyèdre par facette
        visible = self:Isvisible({P1.vertices[facet[1]], P1.vertices[facet[2]], P1.vertices[facet[3]]}) -- on teste la face avec sommets transformés
        b = facet[1]
        for k = 2, #facet do
            a = b
            b = facet[k]
            inserer({a,b},visible)
        end
        inserer({b,facet[1]},visible)
    end
    local aux = {}
    aux.visible = {}
    aux.hidden = {}
    for _,aret in pairs(rep) do
        a, b = table.unpack(aret[1]) -- n° des sommets formant l'arête
        if aret[2] then -- arête visible
            table.insert(aux.visible,{a,b})
        else
            table.insert(aux.hidden,{a,b})
        end
    end
    local V = merge(aux.visible)
    local H = merge(aux.hidden)
    rep = {}
    rep.visible = {}; rep.hidden = {}
    for _,F in ipairs(V) do
        local cp = {}
        for _, k in ipairs(F) do
            table.insert(cp, P.vertices[k]) -- conversion en point 3d (non transformés)
        end
        table.insert(rep.visible,cp)
    end
    for _,F in ipairs(H) do
        cp = {}
        for _, k in ipairs(F) do
            table.insert(cp, P.vertices[k])  
        end
        table.insert(rep.hidden,cp)
    end
    return rep
end


function luadraw_graph3d:Intersection3d(P,plane)
-- P est un polyèdre ou une liste de facettes
-- intersection plan - polyèdre P = { vertices={point3d}, facets={faces avec n°s de sommets} })
-- renvoie une liste d'arêtes = {visible={{A,B},...}, hidden={{C,D},...} }, suivie de la face de coupe
    if P.vertices ~= nil then P = poly2facet(P) end --conversion polyèdre -> facettes
    local S,n = table.unpack(plane)
    local rep, coupe = {}, {}
    rep.visible = {}; rep.hidden = {}
    for _, F in ipairs(P) do
        local nb, aux = #F, {}
        local A1, B1 = nil, F[1]
        local p1, p2 = nil, pt3d.dot(B1-S,n)
        for k = 2, nb+1 do
            if k == nb+1 then k = 1 end -- on ferme la facette
            A1 = B1; p1 = p2; pos1 = pos2; B1 = F[k]; p2 = pt3d.dot(B1-S,n)
            if p1*p2 <= 0 or (pos2 and not pos1)  then -- A1 et B1 sont de part et d'autre du plan
                local I = proj3dO(A1,plane,B1-A1)
                if I ~= nil then table.insert(aux,I); insert3d(coupe,I,1e-10) end
            end
        end
        if #aux > 0 then
            if not isID3d(self.matrix3d) then F = self:Mtransform3d(F) end    
            if self:Isvisible({F[1],F[2],F[3]}) then -- tests sur sommets transformés
                table.insert(rep.visible,aux)
            else
                table.insert(rep.hidden,aux)
            end
        end
    end
    return { ["visible"]=merge3d(rep.visible), ["hidden"]=merge3d(rep.hidden) }, classify3d(coupe,n) -- facette orientée par n
end

function luadraw_graph3d:Dedges(edges,args)
-- dessine les arêtes edges = {visible={{A,B},...}, hidden={{C,D},...} }
-- args est une table à  5 champs :
-- args = {hidden=true/false, visible=true/false, hiddenstyle, style=default, hiddencolor=default, color=default, width=default}
    args = args or {}
    if args.hidden == nil then args.hidden = Hiddenlines end
    args.color = args.color or self.param.linecolor
    args.hiddencolor = args.hiddencolor or args.color
    args.style = args.style or self.param.linestyle
    args.width = args.width or self.param.linewidth
    if args.visible == nil then args.visible = true end
    args.hiddenstyle = args.hiddenstyle or Hiddenlinestyle
    local oldfillstyle = self.param.fillstyle
    local oldlinestyle = self.param.linestyle
    local oldlinecolor = self.param.linecolor
    local oldlinewidth = self.param.linewidth
    self:Filloptions("none")
    if args.hidden and (args.hiddenstyle ~= "noline") then
        self:Lineoptions(args.hiddenstyle,args.hiddencolor,args.width)    
        self:Dpolyline3d(edges.hidden,false)
    end
    if args.visible then 
        self:Lineoptions(args.style,args.color,args.width)    
        self:Dpolyline3d(edges.visible,false) 
    end
    self:Lineoptions(oldlinestyle,oldlinecolor,oldlinewidth)
    self:Filloptions(oldfillstyle)
end


function luadraw_graph3d:drawfacet(S,args) -- usage interne
    local oldfillstyle = self.param.fillstyle
    local oldfillopacity = self.param.fillopacity
    local oldfillcolor = self.param.fillcolor
    local oldlinestyle = self.param.linestyle
    local oldlineopacity = self.param.lineopacity
    local oldlinecolor = self.param.linecolor
    local oldlinewidth = self.param.linewidth
    local coul = args.color
    if (args.mode == 5) and (args.opacity ~= 1)  then self:Linestyle("noline") end
    if (args.mode ~= 5) then self:Lineoptions(args.edgestyle,args.edgecolor,args.edgewidth) end
    if (args.mode == 1) or (args.mode == 2) then -- facettes de couleurs identiques
        if type(coul) == "table" then coul = rgb(args.color) end
        self:Filloptions("full",coul,args.opacity)
        self:Dpolyline3d(S,true)
    else --modes 3, 4 ou 5 les couleurs des facettes sont nuancées
        local res, coef, N, r, g, b = {}
        local A, B, C, k, n, m, coef, ok
        r, g, b = table.unpack(args.color)
        for _,F in ipairs(S) do
            k = 2; m = #F; ok = false
            while (not ok) and (k < m) do
                A, B, C = F[1], F[k], F[k+1]
                N = pt3d.normalize(pt3d.prod(B-A,C-A))
                coef = pt3d.dot(self.Normal,N)
                ok = (coef ~= nil)
                k = k+1
            end
            if ok then 
                local neg = args.twoside and (coef < 0)
                coef = round(math.exp( math.log(math.abs(coef))/2.5*args.contrast),4)
                if notDef(coef) then coef = 0 end
                if type(args.color) == "string" then
                    if neg then
                        coul = args.color.."!50!white!"..strReal(100*coef).."!black"
                    else
                        coul = args.color.."!"..strReal(100*coef).."!black"
                    end
                else 
                    if neg then 
                        coul = rgb({coef*(1+r)/2,coef*(1+g)/2,coef*(1+b)/2})
                    else
                        coul = rgb({coef*r,coef*g,coef*b})
                    end
                end
                self:Filloptions("full",coul,args.opacity)
                if (args.mode == 5) then 
                    if (args.opacity == 1) then
                        self:Lineoptions("solid",coul,1); self:Lineopacity(args.opacity)
                        self:Dpolyline3d(F,true)
                    else
                        self:Dpolyline3d(F,true)
                    end
                else
                    self:Dpolyline3d(F,true)
                end
            end
        end
    end
    self:Filloptions(oldfillstyle,oldfillcolor,oldfillopacity)
    self:Lineoptions(oldlinestyle,oldlinecolor,oldlinewidth); 
    self:Lineopacity(oldlineopacity)
end

function luadraw_graph3d:Dpoly(P,args)
-- dessine le polyèdre convexe P (P={ vertices={point3d}, facets={faces avec n°s de sommets} })
-- args est une table à 10 champs :
-- args = {mode=0/1/2/3/4/5, contrast=1, backcull=true/false, edgestyle=defaut, edgecolor=default, edgewidth=defaut, twoside=true/false, color=White, hiddenstyle, hiddencolor=default, opacity=1}
    --mode=0: arêtes visibles et cachées (mode fil de fer)
    --mode=1: faces peintes couleur unie+arêtes visibles
    --mode=2: faces peintes couleur unie+arêtes visibles et cachées
    --mode=3 faces peintes couleur nuancée+arêtes visibles
    --mode=4 faces peintes couleur nuancée+arêtes visibles+aretes cachées
    --mode=5 faces peintes couleur nuancée pas d'arêtes
    args = args or {}
    args.mode = args.mode or 3
    args.contrast = args.contrast or 1
    args.edgestyle = args.edgestyle or self.param.linestyle
    args.hiddenstyle = args.hiddenstyle or Hiddenlinestyle
    args.edgecolor = args.edgecolor or self.param.linecolor
    args.hiddencolor = args.hiddencolor or args.edgecolor
    args.edgewidth = args.edgewidth or self.param.linewidth
    args.opacity = args.opacity or 1
    args.backcull = args.backcull or false
    args.color = args.color or "white"
    if args.twoside == nil then args.twoside = true end
    local edge_options = {}
    edge_options.color = args.edgecolor
    edge_options.width = args.edgewidth
    edge_options.style = args.edgestyle
    if args.mode%2 == 0 then 
        edge_options.hidden = true -- modes 0, 2 ou 4 on dessine les arêtes cachées
        if args.mode ~=0 then edge_options.visible = false end -- en mode 1,2,3 ou 4 les arêtes sont dessinées avec les faces
    end
    edge_options.hiddenstyle = args.hiddenstyle
    edge_options.hiddencolor = args.hiddencolor
    local P1 = {}
    P1.facets = P.facets
    if not isID3d(self.matrix3d) then P1.vertices = self:Mtransform3d(P.vertices) else P1.vertices = P.vertices end
    local oldmatrix = self.matrix3d
    self.matrix3d = ID3d
    -- dessin-
    if args.mode ~= 0 then -- modes 1,2,3,4 ou 5
        local S = self:Sortpolyfacet(P1,args.backcull) --on travaille sur les points transformés
        self:drawfacet(S,args)
    end
    if args.mode%2 == 0 then self:Dedges(self:Edges(P1),edge_options) end
    self.matrix3d = oldmatrix
end



function luadraw_graph3d:Dfacet(F,args)
-- dessine une liste de facettes F (avec coordonnées 3d)
-- args est une table à 9 champs :
-- args = {mode=0/1/2/3/4/5, contrast=1, backcull=true/false, edgestyle=defaut, edgecolor=default, edgewidth=defaut, twoside=true/false, color=White, opacity=1}    
    --mode=0: arêtes seulement
    --mode=1 et 2: faces peintes couleur unie+arêtes visibles
    --mode=3 et 4 faces peintes couleur nuancée+arêtes visibles
    --mode=5 faces peintes couleur nuancée pas d'arêtes    
    args = args or {}
    args.mode = args.mode or 3
    args.contrast = args.contrast or 1
    args.edgestyle = args.edgestyle or self.param.linestyle
    args.edgecolor = args.edgecolor or self.param.linecolor
    args.edgewidth = args.edgewidth or self.param.linewidth
    args.opacity = args.opacity or 1
    args.backcull = args.backcull or false
    args.color = args.color or "white"
    if args.twoside == nil then args.twoside = true end
    if not isID3d(self.matrix3d) then F = self:Mtransform3d(F) end
    local oldmatrix = self.matrix3d
    self.matrix3d = ID3d
    local oldfillstyle = self.param.fillstyle
    local oldfillopacity = self.param.fillopacity
    local oldfillcolor = self.param.color
    local oldlinestyle = self.param.linestyle
    local oldlineopacity = self.param.lineopacity
    local oldlinecolor = self.param.linecolor
    local oldlinewidth = self.param.linewidth    
    if isPoint3d(F[1]) then F = {F} end --pour avoir une liste de facettes
    local S = self:Sortfacet(F,args.backcull)
    local coul = args.color
    if (args.mode == 0) then -- arêtes seulement
        if type(coul) == "table" then coul = rgb(args.color) end
        self:Lineoptions(args.edgestyle,args.edgecolor,args.edgewidth)
        self:Filloptions("none")
        self:Dpolyline3d(facetedges(S),true)
        self:Filloptions(oldfillstyle,oldfillcolor,oldfillopacity)
        self:Lineoptions(oldlinestyle,oldlinecolor,oldlinewidth); self:Lineopacity(oldlineopacity)
    else --mode = 1,2,3,4,5 
        self:drawfacet(S,args)
    end
    self.matrix3d = oldmatrix
end

function luadraw_graph3d:Dmixfacet(...) --Dmixfacet(F1,args1, F2,args2, ...)
-- dessine une liste de facettes F (avec coordonnées 3d)
-- args est une table à 9 champs :
-- args = {mode=0/1/2/3/4/5, contrast=1, backcull=true/false, edgestyle=defaut, edgecolor=default, edgewidth=defaut, twoside=true/false, color=White, opacity=1}    
    --mode=0: arêtes seulement
    --mode=1 et 2: faces peintes couleur unie+arêtes visibles
    --mode=3 et 4 faces peintes couleur nuancée+arêtes visibles
    --mode=5 faces peintes couleur nuancée pas d'arêtes
    local mode = 3
    local contrast = 1
    local edgestyle = self.param.linestyle
    local edgecolor = self.param.linecolor
    local edgewidth = self.param.linewidth
    local backcull = false
    local twoside = true
    local color = "white"
    local opacity = 1
    local S, face, args = {}
    for k,F in ipairs{...} do
        if k%2 == 1 then 
            face = F
            if not isID3d(self.matrix3d) then face = self:Mtransform3d(face) end -- on applique la matrice de transformation
        else
            args = F
            if args.mode == nil then args.mode = mode else mode = args.mode end
            if args.contrast == nil then args.contrast = contrast else contrast = args.contrast end
            if args.edgestyle == nil then args.edgestyle = edgestyle else edgestyle = args.edgestyle end
            if args.edgecolor == nil then args.edgecolor = edgecolor else edgecolor = args.edgecolor end
            if args.edgewidth == nil then args.edgewidth = edgewidth else edgewidth = args.edgewidth end
            if args.opacity == nil then args.opacity = opacity else opacity = args.opacity end
            if args.backcull == nil then args.backcull = backcull else backcull = args.backcull end
            if args.color == nil then args.color = color else color = args.color end
            if args.twoside == nil then args.twoside = twoside else twoside = args.twoside end
            if isPoint3d(face[1]) then face = {face} end
            for _, f in ipairs(face) do
                table.insert(f,args) -- chaque facette est accompagnée de ses arguments
                table.insert(S,f)
            end
        end
    end
    local oldmatrix = self.matrix3d
    self.matrix3d = ID3d
    local oldfillstyle = self.param.fillstyle
    local oldfillopacity = self.param.fillopacity
    local oldfillcolor = self.param.fillcolor
    local oldlinestyle = self.param.linestyle
    local oldlineopacity = self.param.lineopacity
    local oldlinecolor = self.param.linecolor
    local oldlinewidth = self.param.linewidth  
    S = self:Sortfacet(S)
    for _,F in ipairs(S) do
        args = table.remove(F)
        self:Lineoptions(args.edgestyle,args.edgecolor,args.edgewidth)
        -- dessin
        local coul = args.color
        if (args.mode == 0) then -- arêtes seulement
            self:Filloptions("none")
            self:Dpolyline3d(F,true)
        elseif (args.mode == 1) or (args.mode == 2) then --faces unies
            if type(coul) == "table" then coul = rgb(args.color) end
            self:Filloptions("full",coul,args.opacity)
            self:Dpolyline3d(F,true)
        else --mode = 3,4 ou 5 faces peintes
            local res, coef, N, r, g, b = {}
            local A, B, C, k, n, m, coef, ok
            k = 2; m = #F; ok = false
            while (not ok) and (k < m) do
                A, B, C = F[1], F[k], F[k+1]
                N = pt3d.normalize(pt3d.prod(B-A,C-A))
                coef = pt3d.dot(self.Normal,N)
                ok = (coef ~= nil)
                k = k+1
            end
            if ok then 
                local neg = twoside and (coef < 0)
                coef = round(math.exp( math.log(math.abs(coef))/2.5*args.contrast),4)
                if notDef(coef) then coef = 0 end
                if type(args.color) == "string" then
                    if neg then
                        coul = args.color.."!50!white!"..strReal(100*coef).."!black"
                    else
                        coul = args.color.."!"..strReal(100*coef).."!black"
                    end
                else 
                    r, g, b = table.unpack(args.color)
                    if neg then 
                        coul = rgb({coef*(1+r)/2,coef*(1+g)/2,coef*(1+b)/2})
                    else
                        coul = rgb({coef*r,coef*g,coef*b})
                    end
                end
                self:Filloptions("full",coul,args.opacity)
                if (not args.backcull) or (not neg) then 
                    if args.mode == 5 then 
                        if (args.opacity == 1) then
                            self:Lineoptions("solid",coul,1); self:Lineopacity(args.opacity)
                            self:Dpolyline3d(F,true)
                        else
                            self:Linestyle("noline")
                            self:Dpolyline3d(F,true)
                        end   
                    else
                        self:Dpolyline3d(F,true)
                    end
                end
            end
        end
    end
    self:Filloptions(oldfillstyle,oldfillcolor,oldfillopacity)
    self:Lineoptions(oldlinestyle,oldlinecolor,oldlinewidth); self:Lineopacity(oldlineopacity)
    self.matrix3d = oldmatrix
end


-- scene3d fonctions de base

function luadraw_graph3d:addFacet(facet,args)
-- ajouter des facettes à la scène 3d
-- facet : une facette ou une liste de facettes (avec points 3d)
-- args table à 12 champs
-- {color="white", opacity=1, boxed=false, backcull=false, edgewidth=6, edge=false, edgecolor="black", hidden=false, hiddenstyle="dotted", contrast=1, twoside=false, matrix=ID3d}
    if facet == nil then return {{"nul"}} end
    args = args or {}
    if isPoint3d(facet[1]) then facet = {facet} end
    local color = args.color or "White"
    local opacity = args.opacity or 1
    local backcull = args.backcull or false
    local boxed = args.boxed or false
    local contrast = args.contrast or 1
    local twoside = args.twoside
    local hidden = args.hidden
    if hidden == nil then hidden = Hiddenlines end
    local hiddenstyle = args.hiddenstyle or Hiddenlinestyle
    if twoside == nil then twoside = true end
    local edge = args.edge or false
    local edgecolor = args.edgecolor or "black"
    local edgewidth = args.edgewidth or 6
    local matrix = args.matrix or ID3d
    matrix = composematrix3d(self.matrix3d,matrix)
    local oldmatrix = self.matrix3d
    self.matrix3d = ID3d
    local F = facet
    if not isID3d(matrix) then 
        F = mtransform3d(F,matrix)
    end
    local res = {}
    local rep = {"facet"}
    if boxed then 
        local x1,x2,y1,y2,z1,z2 = getbounds3d(F)
        local eps = 1e-4
        x1 = x1-eps; x2 = x2+eps
        y1 = y1-eps; y2 = y2+eps
        z1 = z1-eps; z2 = z2+eps
        local P = parallelep(M(x1,y1,z1),M(x2-x1,0,0),M(0,y2-y1,0),M(0,0,z2-z1))
        insert(res, self:addSep(poly2facet(P),{matrix=ID3d}))
    end
    local A, B, C, k, n, m, coef, ok
    for _, face in ipairs(F) do
        k = 2; m = #face; ok = false
        while (not ok) and (k < m) do
            A, B, C = face[1], face[k], face[k+1]
            n = pt3d.normalize(pt3d.prod(B-A,C-A))
            coef = pt3d.dot(self.Normal,n)
            ok = (coef ~= nil)
            k = k+1
        end
        if ok then 
            if math.abs(coef) < 1e-8 then coef = 0 end
            if (not backcull) or (coef > 0) then            
                local sg, coul = 1
                if coef < 0 then n = -n; sg = -1 end
                local neg = twoside and (coef < 0)        
                coef = sg*round(math.exp(math.log(math.abs(coef))/2.5*contrast),4)
                if notDef(coef) then coef = 0 end
                local c = math.abs(coef)
                if type(color) == "string" then
                    if neg then
                        coul = color.."!50!white!"..strReal(100*c).."!black"
                    else
                        coul = color.."!"..strReal(100*c).."!black"
                    end
                else 
                    local r, g, b = table.unpack(color)
                    if neg then 
                        coul = rgb({c*(1+r)/2,c*(1+g)/2,c*(1+b)/2})
                    else
                        coul = rgb({c*r,c*g,c*b})
                    end
                end
                table.insert(rep, {face,{A,n},coul,opacity})
            end
        end
    end
    if edge then
        -- calcul des arêtes
        insert(res, self:addPolyline(facetedges(F),{color=edgecolor, hidden=hidden, hiddenstyle=hiddenstyle, width=edgewidth, matrix=ID3d}))
    end
    table.insert(res,rep) -- rep = {"facet", liste de faces}
    self.matrix3d = oldmatrix
    return res 
end

function luadraw_graph3d:addWall(plans,args) -- cloisons séparatrices (non dessinées)
-- ajouter une cloison séparatrice à la scène 3d
-- plan : un plan {A,n} ou une facette 
-- args table à 1 champ : {matrix=ID3d}
    if (plans == nil) or (type(plans) ~= "table") or (#plans == 0) then return {{"nul"}} end
    if isPoint3d(plans[1]) then plans = {plans} end
    args = args or {}
    local matrix = args.matrix or ID3d
    matrix = composematrix3d(self.matrix3d,matrix)
    local res, A, n, facet = {}
    for _, plan in ipairs(plans) do
        if #plan == 2 then 
            A, n = table.unpack(plan)
            if not isID3d(matrix) then 
                A = mtransform3d(A,matrix)
                n = mLtransform3d(n,matrix)
            end
            -- on calcule une facette dans le plan {A,n}
            facet = self:Plane2facet({A,n})
        else
            facet = plan
            if not isID3d(matrix) then 
                facet = mtransform3d(facet,matrix)
            end
            A = facet[1]
            n = pt3d.prod(facet[2]-facet[1],facet[3]-facet[1])
        end
        if pt3d.dot(n,self.Normal) <= 0 then n = -n end
        if facet ~= nil then table.insert(res,{"wall",facet,{A,n}}) end
    end
    return res
end

function luadraw_graph3d:addPolyline(Line,args)
-- ajouter une ligne polygonale à la scène 3d
-- L liste de points 3d ou liste de listes de points 3d
-- args table à 9 champs
-- { color="black", style="solid", width=4, opacity=1, hidden=false, hiddenstyle=dotted, arrows=0/1/2, arrowscale=1, close=true/false, matrix=ID3d }
    if Line == nil then return {{"nul"}} end
    args = args or {}
    local color = args.color or "black"
    local style = args.style or "solid"
    local width = args.width or 4
    local opacity = args.opacity or 1
    local arrows = args.arrows or 0
    local close = args.close or false
    local arrowscale = args.arrowscale or 1
    local hidden = args.hidden
    if hidden == nil then hidden = Hiddenlines end
    local hiddenstyle = args.hiddenstyle or Hiddenlinestyle
    local matrix = args.matrix or ID3d
    matrix = composematrix3d(self.matrix3d,matrix)
    local oldmatrix = self.matrix3d
    self.matrix3d = ID3d
        local rep = {}
    local darrow = function(a,v) -- dessin d'une flèche arrivant en a dans la direction v
        local long = 0.25*arrowscale
        v = -long*pt3d.normalize(v)
        return poly2facet(cone(a,v,long/3,6,false))
    end
    
    if isPoint3d(Line[1]) then Line = {Line} end
    for _,L in ipairs(Line) do
        if not isID3d(matrix) then 
            L = mtransform3d(L,matrix)
        end
        if close then table.insert(L,L[1]) end
        local seg, n = {}, #L
        if arrows >= 1 then --flèche à l'arrivée
            local a, b = L[n], L[n-1]
            insert(rep, self:addFacet(darrow(a,a-b), {color=color, backcull=true, matrix=ID3d}))
        end
        if arrows == 2 then --flèche au départ
            local a, b = L[1], L[2]
            insert(rep, self:addFacet(darrow(a,a-b), {color=color, backcull=true, matrix=ID3d}))
        end
        local A, B, v = nil, L[1]
        for k = 2, n do
            A = B; B = L[k]
            local v = B-A
            if not pt3d.isNul(pt3d.prod(v,self.Normal)) then
                table.insert(seg, {A,B})
            end
        end
        table.insert(rep,{"seg", seg, style, color, width, opacity})
        if hidden then 
            table.insert(rep,{"hidden",L,color,width,hiddenstyle})
        end
    end
    self.matrix3d = oldmatrix
    return rep
end

function luadraw_graph3d:addDots(dots,args)
-- ajouter des points à la scène 3d
-- dots = point 3d, ou liste de points 3d
-- args table à 4 champs
-- { color="black", style="ball", scale=1, matrix=ID3d }
    if dots == nil then return {{"nul"}} end
    args = args or {}
    local color = args.color or "black"
    local style = args.style or "ball"
    local scale = args.scale or 1
    local matrix = args.matrix or ID3d
    matrix = composematrix3d(self.matrix3d,matrix)
    if  isPoint3d(dots) then dots = {dots} end
    if not isID3d(matrix) then 
        dots = mtransform3d(dots,matrix)
    end
    return {{"dot",dots,style,color,scale}}
end

function luadraw_graph3d:addLabel(...) -- addLabel(text1,dot1,args1, text2,dot2,args2,...)
-- ajouter des points à la scène 3d
-- text est le label
-- dot le point d'ancrage point 3d, 
-- args table à 7 champs
-- { color="black", style="center", dist=0, size="normalsize", showdot=false, matrix=ID3d, dir={} }
    local rep = {}
    local text, dot, args
    local color = self.param.labelcolor
    if color == "" then color = "black" end
    local style = self.param.labelstyle
    local size = self.param.labelsize
    local angle = self.param.labelangle
    local dir = {}
    local showdot = false
    local dist = 0
    local matrix = ID3d
    
    local addAlabel = function()
        color = args.color or color
        style = args.style or style
        size = args.size or size
        angle = args.angle or angle
        dist = args.dist or dist
        if args.showdot ~= nil then showdot = args.showdot end
        dir = args.dir or dir
        matrix = args.matrix or matrix
        local mat = composematrix3d(self.matrix3d,matrix)
        if not isID3d(mat) then 
            dot = mtransform3d(dot,mat)
        end
        table.insert(rep, {"label",text,dot,style,dist,color,size,angle,dir,showdot} )
    end
    
    for k, aux in ipairs{...} do
        if k%3 == 1 then text = aux
        else
            if k%3 == 2 then dot = aux
            else  args = aux; addAlabel()
            end
        end
    end
    return rep
end

function luadraw_graph3d:Dscene3d(...)
-- construit et affiche une scène 3d
    Scene3d = Tscene3d:new()
    sortedcount = 0
    local listfacet, listwall, listseg, listdot, listlabel, listhidden = {}, {}, {}, {}, {}, {}
    for _, element in ipairs{...} do -- element = { {objet 3d},...},et un objet 3d = {"seg",...} ou {"facet",...}
        for _, object in ipairs(element) do
            local type = table.remove(object,1)
            if type == "facet" then 
                insert(listfacet, object) 
            elseif type == "wall" then
                table.insert(listwall, object) 
            elseif type == "seg" then
                table.insert(listseg, object) 
            elseif type == "dot" then
                table.insert(listdot, object) 
            elseif type == "label" then
                table.insert(listlabel, object) 
            elseif type == "hidden" then
                table.insert(listhidden, object)
            end
        end
    end
    for _, F in ipairs(listwall) do -- insertion de facettes séparatrices
        Scene3d:Addsep(table.unpack(F))
    end
    for _, F in ipairs(listfacet) do -- insertion des facettes
        Scene3d:Addfacet(table.unpack(F))
    end
    for _, S in ipairs(listseg) do -- insertion des segments
        local lseg, style, color, width, opacity = table.unpack(S)
        for _, seg in ipairs(lseg) do -- lseg est la liste de segments
            Scene3d:Addseg(seg,style,color,width,opacity,self.Normal)
        end
    end
    for _, D in ipairs(listdot) do -- insertion des points
        local ldot, style, color, scale = table.unpack(D)
        for _, dot in ipairs(ldot) do -- lseg est la liste de segments
            Scene3d:Adddot(dot,style,color,scale,self.Normal)
        end
    end
    for _, L in ipairs(listlabel) do -- insertion des labels
        table.insert(L,self.Normal)
        Scene3d:Addlabel(table.unpack(L))
    end
    print("\n element in tree=",Scene3d:nb())
    print("split facet=",nbsplit)
    self:Saveattr()
    self.matrix3d = ID3d
    Scene3d:Display(self)
    for _, H in ipairs(listhidden) do
        local L, color, width, style = table.unpack(H)
        --print(L, color, width, style)
        self:Lineoptions(style,color,width); self:Filloptions("none",nil,1)
        self:Dpolyline3d(L)
    end
    self:Restoreattr()
end

-- suppléments pour scène 3d

function luadraw_graph3d:addPoly(P,args)
-- ajouter un polyèdre à la scène 3d
-- polyedre P = { vertices={points3d}, facets={ {1,2,3}, ...} }
-- args table à 9 champs
-- {color="white", opacity=1, backcull=false, edgewidth=6, edge=false, boxed=false, edgecolor="black", hidden=false, hiddenstyle="dotted", contrast=1, twoside=false, matrix=ID3d}
    if P == nil then return {{"nul"}} end
    args = args or {}
    args.edge = args.edge or false
    args.edgecolor = args.edgecolor or "black"
    args.edgewidth = args.edgewidth or 6
    local hidden = args.hidden
    if hidden == nil then hidden = Hiddenlines end
    local hiddenstyle = args.hiddenstyle or Hiddenlinestyle   
    local matrix = args.matrix or ID3d
    matrix = composematrix3d(self.matrix3d,matrix)
    local oldmatrix = self.matrix3d
    self.matrix3d = ID3d
    local  rep = {}
    local P1 = table.copy(P)
    if not isID3d(matrix) then
        P1.vertices = mtransform3d(P.vertices,matrix)
    end
    args.matrix = ID3d
    if args.edge then 
        insert(rep, self:addPolyline( facetedges(P1),{color=args.edgecolor, hidden=hidden, hiddenstyle=hiddenstyle, width=args.edgewidth, matrix=ID3d}))
        args.edge = false
    end
    insert(rep, self:addFacet( poly2facet(P1), args ) )
    self.matrix3d = oldmatrix
    return rep
end

function luadraw_graph3d:addAxes(O,args)
-- ajouter les axes à la scène
-- O est le point 3d de concours
-- args table à 7 champs (comme bdPolyline) plus un champ legend=true/false
-- { color="black", style="solid", width=4, hidden=false, hiddenstyle="dotted", legend=true/false, opacity=1, arrows=0/1/2, arrowscale=1, matrix=ID3d }
    args = args or {}
    local arrowscale = args.arrowscale or 1
    local legend = args.legend
    if legend == nil then legend = true end
    local x0,y0,z0 = O.x, O.y, O.z
    local rep = {}
    local x1,x2,y1,y2,z1,z2 = table.unpack(self.param.viewport3d)
    insert(rep, self:addPolyline( {{M(x1,y0,z0),M(x2,y0,z0)}, {M(x0,y1,z0),M(x0,y2,z0)},{M(x0,y0,z1),M(x0,y0,z2)}}, args ))    
    if legend then
        local long = 0.25*arrowscale
        insert(rep, self:addLabel("$x$", M(x2+(x2-x1)/20,y0,z0), args))
        insert(rep, self:addLabel("$y$", M(x0,y2+(y2-y1)/20,z0), args))
        insert(rep, self:addLabel("$z$", M(x0,y0,z2+(z2-z1)/20), args))
        insert(rep, self:addWall({{M(x2,y0,z0)-long*vecI, vecI}, {M(x0,y2,z0)-long*vecJ, vecJ}, {M(x0,y0,z2)-long*vecK, vecK}},args))
    end
    return rep
end

function luadraw_graph3d:Plane2facet(plane,scale)
-- convertit un plan en facette en l'intersectant avec le parallélépipède viewport3d
-- renvoie une face (liste de point 3d)
    scale = scale or 1
    local x1,x2,y1,y2,z1,z2 = table.unpack(self.param.viewport3d)
    local cube = parallelep(M(x1,y1,z1), (x2-x1)*vecI,(y2-y1)*vecJ, (z2-z1)*vecK)
    local I, face = self:Intersection3d(cube,plane)
    if face ~= nil then 
        if scale ~= 1 then
            local G = isobar3d(face)
            face = scale3d(face,scale,G)
        end
        return face
    end
end

function luadraw_graph3d:Line3d2seg(d,scale)
    scale = scale or 1
    local x1,x2,y1,y2,z1,z2 = table.unpack(self.param.viewport3d)
    local A, B, L = interDP(d, {M(0,0,z1),vecK})
    if A ~= nil then
        B = interDP(d, {M(0,0,z2),vecK})
        L = cutpolyline3d({A,B}, {M(x1,0,0),vecI})
        L = cutpolyline3d(L, {M(x2,0,0),-vecI})
        L = cutpolyline3d(L, {M(0,y1,0),vecJ})
        L = cutpolyline3d(L, {M(0,y2,0),-vecJ})
    else
        A = interDP(d, {M(x1,0,0),vecI})
        if A ~= nil then
            B = interDP(d,{M(x2,0,0),vecI})
            L = cutpolyline3d({A,B}, {M(0,0,z1),vecK})
            L = cutpolyline3d(L, {M(0,0,z2),-vecK})
            L = cutpolyline3d(L, {M(0,y1,0),vecJ})
            L = cutpolyline3d(L, {M(0,y2,0),-vecJ})
         else
             A = interDP(d,{M(0,y1,0),vecJ})
             B = interDP(d,{M(0,y2,0),-vecJ})
             L = cutpolyline3d({A,B}, {M(0,0,z1),vecK})
             L = cutpolyline3d(L, {M(0,0,z2),-vecK})
             L = cutpolyline3d(L, {M(x1,0,0),vecI})
             L = cutpolyline3d(L, {M(x2,0,0),-vecI})
        end
    end
    if (L ~= nil) then L = L[1] else return end
    if scale ~= 1 then
        L = scale3d(L,scale, (L[1]+L[2])/2)
    end
    return L
end

function luadraw_graph3d:addPlane(plane,args)
-- ajouter un plan à la scène 3d
-- plane = {A,n}
-- args table à 12 champs, ceux de addFacet, plus l'option scale=1
    if plane == nil then return {{"nul"}} end
    args = args or {}
    args.scale = args.scale or 0.75
    local matrix = args.matrix or ID3d
    matrix = composematrix3d(self.matrix3d,matrix)
    local oldmatrix = self.matrix3d
    self.matrix3d = ID3d
    local A, n = table.unpack(plane)
    local  rep = {}
    if not isID3d(matrix) then
        A = mtransform3d(A,matrix)
        n = mLtransform3d(n,matrix)
    end
    args.matrix = ID3d
    local face = self:Plane2facet({A,n},args.scale)
    local rep = {}
    if face ~= nil then 
        rep = self:addFacet(face, args)
    else rep =  {{"nul"}}
    end
    self.matrix3d = oldmatrix
    return rep
end

function luadraw_graph3d:addPlaneEq(coef,args)
-- ajouter un plan à la scène 3d définit par une équation ax+by+cz+d=0
-- coef = {a,b,c,d} (réels)
-- args table à 9 champs
-- {color="white", scale = 1,opacity=1, backcull=false, edgewidth=6, edge=false, edgecolor="black", contrast=1, twoside=false, matrix=ID3d}
    local a,b,c,d = table.unpack(coef)
    d = d or 0
    local P = planeEqn(a,b,c,d)
    if P == nil then return {{"nul"}}
    else
        return self:addPlane({A,n},args)
    end
end

function luadraw_graph3d:addLine(d,args)
-- ajouter un droite d à la scène 3d
-- d = {A,u} (points 3d)
-- args table à 8 champs
-- { color="black", style="solid", width=4, opacity=1, arrows=0/1/2, arrowscale=1, scale = 1, matrix=ID3d }
    if d == nil then return {{"nul"}} end
    args = args or {}
    args.scale = args.scale or 1
    local matrix = args.matrix or ID3d
    matrix = composematrix3d(self.matrix3d,matrix)
    local oldmatrix = self.matrix3d
    self.matrix3d = ID3d
    local C, v = table.unpack(d)
    local  rep = {}
    if not isID3d(matrix) then
        C = mtransform3d(C,matrix)
        v = mLtransform3d(v,matrix)
        d = {C,v}
    end
    args.matrix = ID3d
    local L = self:Line3d2seg(d,args.scale)
    if (L == nil) then return {{"nul"}} end
    local res = self:addPolyline(L,args)
    self.matrix3d = oldmatrix    
    return res
end

function luadraw_graph3d:addAngle(B,A,C,r,args)
-- ajouter l'angle droit BAC à la scène 3d
-- r = "rayon"
-- args table à 7 champs
-- { color="black", style="solid", width=4, opacity=1, arrows=0/1/2, arrowscale=1, matrix=ID3d }
    if type(r) == "table" then args = r; r = nil end
    r = r or 0.25
    local u, v = B-A, C-A
    u, v = r*pt3d.normalize(u), r*pt3d.normalize(v)
    return self:addPolyline( {A+u,A+u+v,A+v}, args )
end

function luadraw_graph3d:addArc(B,A,C,r,sens,normal,args)
-- ajouter l'arc BAC à la scène 3d
-- r = rayon
-- sens = 1/-1 (sens trigo ou inverse)
-- normal : vecteur normal au plan de l'arc
-- args table à 7 champs
-- { color="black", style="solid", width=4, opacity=1, arrows=0/1/2, arrowscale=1, matrix=ID3d }
    local L = arc3d(B,A,C,r,sens,normal)
    return self:addPolyline( L, args )
end

function luadraw_graph3d:addCircle(A,r,normal,args)
-- ajouter le cercle de centre A et de rayon r à la scène 3d
-- normal : vecteur normal au plan du cercle
-- args table à 7 champs
-- { color="black", style="solid", width=4, opacity=1, arrows=0/1/2, arrowscale=1, matrix=ID3d }
    local u = pt3d.prod(vecI,normal)
    if pt3d.isNul(u) then 
        u = pt3d.prod(vecJ,normal)
        if pt3d.isNul(u) then return end
    end
    return self:addArc(A+u,A,A+u,r,1,normal, args )
end

-- path3d
function luadraw_graph3d:Dpath3d(L,draw_options) 
-- dessine le chemin contenu dans L, L est une table de point3d et d'instructions
-- ex: Dpath3d( {M(0,-3,0), Origin, vecK,"c", Origin,"m",M(1,1,0),"l",Origin,M(1,1,2),2.5,1,"ca","cl"} )
-- "m" pour moveto, "l" pour lineto, "b" pour bézier, "c" pour cercle, "ca" pour arc de cercle, "s" pour spline naturelle, "cl" pour close
    if (L == nil) or (type(L) ~= "table") or (#L < 3) then return end
    draw_options = draw_options or ""
    local res, aux = {}, {} -- résultat et tronçon courant
    local last, first = nil, nil -- dernier lu et premier à venir
    local traiter
    
    local lineto = function() -- traitement du lineto
        insert(res,aux)
        table.insert(res,"l")
        first = last
        aux = {}
    end
    
    local moveto = function() -- traitement du moveto
    -- on démarre une nouvelle composante
        insert(res,aux)
        table.insert(res,"m")
        first = last
        aux = {}
    end
    
    local close = function() -- traitement du closepath
        table.insert(res,"cl")
        aux = {}
    end
    
    local Bezier = function()
        -- aux contient une ou plusieurs courbes de bézier
        insert(res,aux)
        if aux[#aux] ~= "b" then table.insert(res,"b") end
        first = last
        aux = {}
    end
    
    local Circle = function()
    -- il faut un point, le centre et un vecteur normal, aux={A,C,normal}
        if first ~= nil then 
            table.insert(aux,1,first)
        end
        local a, c, n = table.unpack(aux)
        local r = pt3d.abs(c-a)
        aux = arc3db(a,c,a,r,1,n)
        if aux ~= nil then
            if first ~= nil then table.remove(aux,1) end -- le premier point est déjà exporté
            Bezier()
        end
        aux = {}
    end
    
    local Arc = function()
        --aux = {B,A,C,R,sens,normal}
        if first ~= nil then 
            table.insert(aux,1,first)
        end
        aux = arc3db(table.unpack(aux))
        if aux ~= nil then
            if first ~= nil then table.insert(aux,2,"l") end -- pour relier au point précédent
            local newfirst = aux[#aux-1]
            Bezier()
            first = newfirst -- dernier point de l'arc
        end
        aux = {}
    end

-- corps de la fonction dpath
    traiter = { ["s"]=Spline, ["l"]=lineto, ["m"]=moveto, ["cl"]=close, ["b"]=Bezier, ["c"]=Circle, ["ca"]=Arc} 
    for _, z in ipairs(L) do
        if (type(z) == "number") or isPoint3d(z) then table.insert(aux,z); last = z 
        else
            if type(z) == "string" then traiter[z]() end
        end
    end
    --print(table.unpack(self:Proj3d(res)))
    self:Dpath(self:Proj3d(res),draw_options)
end


-- Boxaxes3d
function luadraw_graph3d:Dboxaxes3d(args)
-- args est une table à ... champs
local eps = 1e-10
    args = args or {}
    if args.xaxe == nil then args.xaxe = true end
    if args.yaxe == nil then args.yaxe = true end
    if args.zaxe == nil then args.zaxe = true end

    args.drawbox = args.drawbox or false
    args.grid = args.grid or false
    args.gridwidth = args.gridwidth or 1
    args.gridcolor = args.gridcolor or "black"
    args.fillcolor = args.fillcolor or "" -- si grid vaut true
    
    local x1,x2,y1,y2,z1,z2 = table.unpack(self.param.viewport3d)
    args.xlimits = args.xlimits or {x1,x2}
    args.ylimits = args.ylimits or {y1,y2}
    args.zlimits = args.zlimits or {z1,z2}
    args.xstep = args.xstep or 1
    args.ystep = args.ystep or 1
    args.zstep = args.zstep or 1
    args.xgradlimits = args.xgradlimits or "auto"
    args.ygradlimits = args.ygradlimits or "auto"
    args.zgradlimits = args.zgradlimits or "auto"
    args.xyzticks = args.xyzticks or 0.2
    
    if args.labels == nil then args.labels = true end    
    args.xlabelsep = args.xlabelsep or 0.25
    args.ylabelsep = args.ylabelsep or 0.25
    args.zlabelsep = args.zlabelsep or 0.25
    args.xlabelstyle = args.xlabelstyle or self.param.labelstyle
    args.ylabelstyle = args.ylabelstyle or self.param.labelstyle
    args.zlabelstyle = args.zlabelstyle or self.param.labelstyle
    
    args.xlegend = args.xlegend or "$x$"
    args.ylegend = args.ylegend or "$y$"
    args.zlegend = args.zlegend or "$z$"
    args.xlegendsep = args.xlegendsep or 0.5
    args.ylegendsep = args.ylegendsep or 0.5
    args.zlegendsep = args.zlegendsep or 0.5

    local xinf, xsup = table.unpack(args.xlimits)
    local yinf, ysup = table.unpack(args.ylimits)
    local zinf, zsup = table.unpack(args.zlimits)
    if args.xgradlimits == "auto" then args.xgradlimits = args.xlimits end
    if args.ygradlimits == "auto" then args.ygradlimits = args.ylimits end
    if args.zgradlimits == "auto" then args.zgradlimits = args.zlimits end
    args.xaxe = args.xaxe and (pt3d.abs(pt3d.prod(self.Normal,vecI)) > eps)
    args.yaxe = args.yaxe and (pt3d.abs(pt3d.prod(self.Normal,vecJ)) > eps)
    args.zaxe = args.zaxe and (pt3d.abs(pt3d.prod(self.Normal,vecK)) > eps)
    local theta, phi =table.unpack(self.param.viewdir) -- angles de vue en degrés
    theta = theta%360
    phi = phi%360
    if angle3d(self.Normal,vecK,eps) > math.pi/2 then 
       zinf, zsup = zsup, zinf
    end
    local psi
    if phi <= 180 then psi = theta 
    else psi = (theta+pi)%360
    end
    if psi >= 180 then psi = psi-360 end
    local Left, Right, Bottom, Top
    if (0 < psi) and (psi <= 90) then
        Left =M(xsup,yinf,zinf); Bottom = M(xsup,ysup,zinf)
        Right = M(xinf,ysup,zinf); Top = M(xinf,yinf,zsup)
    elseif (90 < psi) and (psi <= 180) then
        Left = M(xsup,ysup,zinf); Bottom = M(xinf,ysup,zinf)
        Right = M(xinf,yinf,zinf); Top= M(xsup,yinf,zsup)
    elseif (-90 < psi) and (psi <= 0) then
        Left = M(xinf,yinf,zinf);  Bottom = M(xsup,yinf,zinf)
        Right = M(xsup,ysup,zinf); Top = M(xinf,ysup,zsup)
    else
        Left = M(xinf,ysup,zinf);  Bottom = M(xinf,yinf,zinf)
        Right = M(xsup,yinf,zinf); Top = M(xsup,ysup,zsup)
    end
    local axeOx, axeOy, axeOz, pris, xdir, ydir, zdir
    if Left.x ~= Bottom.x then --axe Ox
        if Left.x < Bottom.x then axeOx = Left else axeOx = Bottom end
        xdir = Bottom-Right; if pt3d.abs(pt3d.prod(xdir,self.Normal)) < eps then xdir = pz(Bottom-Top) end
        pris = 1
    elseif Left.y ~= Bottom.y then --axe Oy
        if Left.y < Bottom.y then axeOy = Left else axeOy = Bottom end
        ydir = Bottom-Right; if pt3d.abs(pt3d.prod(ydir,self.Normal)) < eps then ydir = pz(Bottom-Top) end
        pris = 2
    elseif Left.z ~= Bottom.z then --axe Oz
        if Left.z < Bottom.z then axeOz = Left else axeOz = Bottom end
        zdir = Bottom-Left; if pt3d.abs(pt3d.prod(zdir,self.Normal)) < eps then zdir = (Bottom-Right) end
        pris = 3
    end
     if Right.x ~= Bottom.x then --axe Ox
        if Right.x < Bottom.x then axeOx = Right else axeOx = Bottom end
        xdir = Bottom-Left; if pt3d.abs(pt3d.prod(xdir,self.Normal)) < eps then xdir = pz(Bottom-Top) end
        pris = pris+1
    elseif Right.y ~= Bottom.y then --axe Oy
        if Right.y < Bottom.y then axeOy = Right else axeOy = Bottom end
        ydir = Bottom-Left; if pt3d.abs(pt3d.prod(ydir,self.Normal)) < eps then ydir = pz(Bottom-Top) end
        pris = pris+2
    elseif Right.z ~= Bottom.z then --axe Oz
        if Right.z < Bottom.z then axeOz = Right else axeOz = Bottom end
        zdir = Bottom-Left; if pt3d.abs(pt3d.prod(zdir,self.Normal)) < eps then zdir = (Bottom-Right) end
        pris = pris+3
    end
    if 6-pris == 1 then --axe Ox
        if Left.x == xsup then axeOx = M(xinf,0,0)+pyz(Left) else axeOx = Left end
        xdir = -pt3d.normalize(Bottom-Left); if pt3d.abs(pt3d.prod(xdir,self.Normal)) < eps then xdir = pz(Bottom-Top) end
    elseif 6-pris == 2 then --axe Oy
        if Left.y == ysup then axeOy = M(0,yinf,0)+pxz(Left) else axeOy = Left end
        ydir = -(Bottom-Left); if pt3d.abs(pt3d.prod(ydir,self.Normal)) < eps then ydir = pz(Bottom-Top) end
    elseif 6-pris == 3 then --axe Oz
        if Left.z == zsup then axeOz = M(0,0,zinf)+pxy(Left) else axeOz = Left end
        zdir = -(Bottom-Left); if pt3d.abs(pt3d.prod(zdir,self.Normal)) < 1e-8 then zdir = (Bottom-Right) end
    end
    if axeOy.x == xinf then x1 = xsup else x1 = xinf end
    if axeOx.y == yinf then  y1 = ysup else y1 = yinf end
    z1 = axeOx.z
    -- dessin
    local oldfillstyle = self.param.fillstyle
    local oldfillopacity = self.param.fillopacity
    local oldfillcolor = self.param.fillcolor
    local oldlinestyle = self.param.linestyle
    local oldlineopacity = self.param.lineopacity
    local oldlinecolor = self.param.linecolor
    local oldlinewidth = self.param.linewidth
    local oldarrows = self.param.arrows
    local oldlabelstyle = self.param.labelstyle
    if args.grid then
        local res = {} -- grille sous forme de liste de segments
        for y = yinf, ysup+eps, args.ystep do --grille x = x1
            table.insert(res, {M(x1,y,zinf), M(x1,y,zsup)})
        end
        for z = args.zlimits[1], args.zlimits[2]+eps, args.zstep do --grille x = x1
            table.insert(res, {M(x1,yinf,z), M(x1,ysup,z)})
        end
        for x = xinf, xsup+eps, args.xstep do --grille y = y1
            table.insert(res, {M(x,y1,zinf), M(x,y1,zsup)})
        end
        for z = args.zlimits[1], args.zlimits[2]+eps, args.zstep do --grille y = y1
            table.insert(res, {M(xinf,y1,z), M(xsup,y1,z)})
        end
        for x = xinf, xsup+eps, args.xstep do --grille z = z1
            table.insert(res, {M(x,yinf,z1), M(x,ysup,z1)})
        end
        for y = yinf, ysup+eps, args.ystep do --grille z = z1
            table.insert(res, {M(xinf,y,z1), M(xsup,y,z1)})
        end
        if args.fillcolor ~= "" then
            self:Filloptions("full",args.fillcolor,1); self:Lineoptions("noline")
            self:Dpolyline3d({ {M(xsup,ysup,z1), M(xsup,yinf,z1), M(xinf,yinf,z1), M(xinf,ysup,z1)},
                  {M(x1,ysup,zsup), M(x1,yinf,zsup), M(x1,yinf,zinf), M(x1,ysup,zinf)},
                  {M(xsup,y1,zsup), M(xsup,y1,zinf), M(xinf,y1,zinf), M(xinf,y1,zsup)}}, true)
        end
        self:Filloptions(oldfillstyle,oldfillcolor,oldfillopacity)
        self:Lineoptions("solid",args.gridcolor,args.gridwidth,"-")
        self:Dpolyline3d(res)
        self:Lineoptions(oldlinestyle,oldlinecolor,oldlinewidth,oldarrows)
    end
    local labels, grad, A, a, b, u, dirgrad = {}, {}    
    if args.xaxe then
        self:Dpolyline3d( {axeOx, M(xsup,0,0)+pyz(axeOx)} )
        self:Arrows("-")
        self:Labelstyle(args.xlabelstyle)
        if args.xlegend ~= "" then
            A = pyz(axeOx)+(xsup+xinf)*vecI/2
            a = self:Proj3d(A); b = self:Proj3d(A+xdir)
            u = (b-a)/self:Abs(b-a)
            dirgrad = (args.xlabelsep+args.xlegendsep+args.xyzticks/2)*u
            insert(labels, {args.xlegend,a+dirgrad,{}})
        end
        if args.xstep > 0 then
            for x = args.xgradlimits[1], args.xgradlimits[2]+eps, args.xstep do
                A = axeOx+(x-xinf)*vecI
                a = self:Proj3d(A); b = self:Proj3d(A+xdir)
                u = (b-a)/self:Abs(b-a)
                dirgrad = (args.xyzticks/2)*u
                table.insert(grad,{a, a+dirgrad})
                if args.labels then insert(labels, {gradLabel(x,1,""),a+dirgrad+args.xlabelsep*u,{}}) end
            end
            self:Dpolyline3d(grad)
            self:Dlabel(table.unpack(labels))
        end
    end
    if args.yaxe then
        grad, labels ={}, {}
        self:Dpolyline3d( {axeOy, M(0,ysup,0)+pxz(axeOy)} )
        self:Arrows("-")
        self:Labelstyle(args.ylabelstyle)
        if args.ylegend ~= "" then
            A = pxz(axeOy)+(ysup+yinf)*vecJ/2
            a = self:Proj3d(A); b = self:Proj3d(A+ydir)
            dirgrad = (args.ylabelsep+args.ylegendsep+args.xyzticks/2)*(b-a)/self:Abs(b-a)
            insert(labels, {args.ylegend,a+dirgrad,{}})
        end
        if args.ystep > 0 then
            for x = args.ygradlimits[1], args.ygradlimits[2]+eps, args.ystep do
                A = axeOy+(x-yinf)*vecJ
                a = self:Proj3d(A); b = self:Proj3d(A+ydir)
                u = (b-a)/self:Abs(b-a)
                dirgrad = (args.xyzticks/2)*u
                table.insert(grad,{a, a+dirgrad})
                if args.labels then insert(labels, {gradLabel(x,1,""),a+dirgrad+args.ylabelsep*u,{}}) end
            end
            self:Dpolyline3d(grad)
            self:Dlabel(table.unpack(labels))
        end
    end
    if args.zaxe then
        grad, labels ={}, {}
        self:Dpolyline3d( {M(0,0,zinf)+pxy(axeOz), M(0,0,zsup)+pxy(axeOz)} )
        self:Arrows("-")
        self:Labelstyle(args.zlabelstyle)
        if args.zlegend ~= "" then
            A = pxy(axeOz)+(zsup+zinf)*vecK/2
            a = self:Proj3d(A); b = self:Proj3d(A+zdir)
            dirgrad = (args.zlabelsep+args.zlegendsep+args.xyzticks/2)*(b-a)/self:Abs(b-a)
            insert(labels, {args.zlegend,a+dirgrad,{}})
        end
        if args.zstep > 0 then
            for x = args.zgradlimits[1], args.zgradlimits[2]+eps, args.zstep do
                A = pxy(axeOz)+x*vecK
                a = self:Proj3d(A); b = self:Proj3d(A+zdir)
                u = (b-a)/self:Abs(b-a)
                dirgrad = (args.xyzticks/2)*u
                table.insert(grad,{a, a+dirgrad})
                if args.labels then insert(labels, {gradLabel(x,1,""),a+dirgrad+args.zlabelsep*u,{}}) end
            end
            self:Dpolyline3d(grad)
            self:Dlabel(table.unpack(labels))
        end
    end
    self:Arrows(oldarrows)
    self:Labelstyle(oldlabelstyle)
    if args.drawbox then 
        self:Begindeferred() -- affichage de la boite en fin de graphique
        self:Dpoly(parallelep(M(xinf,yinf,zinf),M(xsup-xinf,0,0),M(0,ysup-yinf,0),M(0,0,zsup-zinf)),
            {mode=0, edgewidth=2, edgecolor="black", hiddenstyle="noline"})
        self:Enddeferred()
    end
end


return luadraw_graph3d
