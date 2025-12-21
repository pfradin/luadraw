-- luadraw_graph3d.lua
-- date 2025/12/21
-- version 2.4
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
projection_mode = "ortho"
local projection_msg = {
    ["ortho"] = "orthographic", 
    ["yz"] = "cavalier perspective on yz-plane", 
    ["xz"] = "cavalier perspective on xz-plane", 
    ["xy"] = "cavalier perspective on xy-plane", 
    ["iso"] = "isometric",
    ["central"] = "central projection"
    }

Hiddenlines = false
Hiddenlinestyle = "dotted"
top, right, bottom, left, all = 8, 4, 2, 1, 15
mWireframe, mFlat, mFlatHidden, mShaded, mShadedHidden, mShadedOnly = 0, 1, 2, 3, 4, 5
mGrid = 1 -- cylinder, sphere, cone
mBorder = 2 -- sphere
--MODE_WIREFRAME = 0
--MODE_FLAT = 1
--MODE_FLAT_HIDDEN_EDGES = 2
--MODE_SHADED = 3
--MODE_SHADED_HIDDEN_EDGES = 4
--MODE_SHADED_NO_EDGE = 5

local default_values = function() 
    Hiddenlines = false
    Hiddenlinestyle = "dotted"
end

local luadraw_graph2d = require "luadraw_graph2d"

local luadraw_graph3d = {}
setmetatable(luadraw_graph3d, {__index = luadraw_graph2d}) -- obligatoire pour l'héritage

--- Constructeur
function luadraw_graph3d:new(args) -- argument de la forme :
-- {window3d={x1,x2,y1,y2,z1,z2}, viewdir={30,60}, adjust2d=true/false, window={x1,x2,y1,y2,xscale,yscale}, margin={left, right, top, bottom}, size={large, haut, ratio}, bg="color", border = true/false, bbox=true/false}
    local graph3d = luadraw_graph2d:new(args) -- obligatoire, on utilise le constructeur de luadraw_calc
    default_values()
    args.adjust2d = args.adjust2d or false -- adjust2d= false or true
    args.viewdir = args.viewdir or {30,60,"ortho"}
    setmetatable(graph3d, {__index = luadraw_graph3d})  -- obligatoire, permet d'utiliser self
    graph3d.param["viewport3d"] = args.window3d or {-5,5,-5,5,-5,5}
    if args.viewdir == "xOy" then args.viewdir = {-90,0,"ortho"}
    elseif args.viewdir == "yOz" then args.viewdir = {0,90,"ortho"}
    elseif args.viewdir == "xOz" then args.viewdir = {-90,90,"ortho"}
    end
    local a, b, mode = table.unpack( args.viewdir )
    graph3d.param["viewdir"] = {a, b}-- viewdir theta et phi en degrés
    a = a*deg; b = b*deg; -- conversion en radians
    if mode == nil then mode= "ortho" end
    if (projection_mode == "central") and (mode ~= "central") then close_central() end
    if mode == "ortho" then
        function luadraw_graph3d:Proj3d(L)
            return self:orthographic_Proj3d(L)
        end
    end
    projection_mode = mode
    graph3d.cosTheta = math.cos(a) -- pour accélérer les calculs, les cos et sin ne sont calculés qu'une fois
    graph3d.sinTheta = math.sin(a)
    graph3d.cosPhi = math.cos(b)
    graph3d.sinPhi = math.sin(b)
    graph3d.Normal = M(graph3d.cosTheta*graph3d.sinPhi, graph3d.sinTheta*graph3d.sinPhi, graph3d.cosPhi)
    graph3d.matrix3d = ID3d
    if args.adjust2d  then 
        local x1,x2,y1,y2,z1,z2 = table.unpack(graph3d.param.viewport3d)
        local box = parallelep( M(x1,y1,z1), (x2-x1)*vecI, (y2-y1)*vecJ, (z2-z1)*vecK ) -- boite 3d
        --local L = {}
        --for _, A in ipairs(box.vertices) do -- projection des sommets de la boite sur le plan 2d
        --    table.insert(L, Z(graph3d.cosTheta*A.y-graph3d.sinTheta*A.x, -graph3d.cosPhi*graph3d.cosTheta*A.x-graph3d.cosPhi*graph3d.sinTheta*A.y+graph3d.sinPhi*A.z))
        --end
        x1, x2, y1, y2 = getbounds( graph3d:Proj3d(box.vertices) )
        x1 = x1-(x2-x1)/5; x2 = x2+(x2-x1)/20
        y1 = y1-(y2-y1)/5; y2 = y2+(y2-y1)/20
        --x1 = math.floor(x1); x2 = math.ceil(x2)
        --y1 = math.floor(y1); y2 = math.ceil(y2)
        graph3d.param.viewport = {x1,x2,y1,y2} -- redimensionnement de la vue 2d
        graph3d.param.coordsystem = {x1,x2,y1,y2} -- redimensionnement de la vue utilisateur
        graph3d.advancedparam = table.copy(graph3d.currentparam)
        graph3d.deferredparam = table.copy(graph3d.currentparam)
        local lg, ht = (graph3d.Xmax-graph3d.Xmin)*graph3d.Xscale, (graph3d.Ymax-graph3d.Ymin)*graph3d.Yscale
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
    print("\n3d window = ", table.unpack(graph3d.param.viewport3d))
    print("projection mode = ", projection_msg[projection_mode])
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

function perspective(mode,k,alpha,d,look) -- change the type of projection
-- mode = "iso" (isometric projection) or 
-- mode = "yz" or "xz" or "xy" (cavalier perspective)
-- parameters for for cavalier perspective : k is a ratio and alpha is an angle in degrees 
    mode = mode or "ortho" -- default value
    local r, k_cos_alpha, k_sin_alpha, theta, phi
    if (mode == "central") or (mode == "ortho") then
        theta = k or 30
        phi = alpha or 60
        d = d or 15
    else
        k = k or 0.5
        alpha = alpha or 45
        k = math.abs(k)
        alpha = alpha*deg -- conversion degrees -> radians (deg = pi/180)
        r = 1/math.sqrt(1+k^2)
        k_cos_alpha = k*math.cos(alpha) -- to calculate them only once
        k_sin_alpha = k*math.sin(alpha)
    end
    local f = nil -- function 
    if mode == "yz" then -- In this perspective, vecJ becomes 1 and vecK becomes i
        phi = math.acos(k_sin_alpha*r)*rad -- angles in degrees for viewdir (rad = 180/pi)
        theta = cpx.arg(Z(1,k_cos_alpha))*rad
        f = function(A)
                if isPoint3d(A) then  -- We only transform the 3D points; the rest remains unchanged.
                    return Z(A.y-k_cos_alpha*A.x, A.z-k_sin_alpha*A.x)
                else return A end
            end
    elseif mode == "xz" then -- In this perspective, vecI becomes 1 and vecK becomes i
        phi = math.acos(k_sin_alpha*r)*rad -- angles in degrees for viewdir (rad = 180/pi)
        theta = cpx.arg(Z(k_cos_alpha,-1))*rad
        f = function(A)
                if isPoint3d(A) then  -- We only transform the 3D points; the rest remains unchanged.
                    return Z(A.x+k_cos_alpha*A.y, A.z+k_sin_alpha*A.y)
                else return A end
            end

    elseif mode == "xy" then -- In this perspective, vecI becomes 1 and vecJ becomes i
        phi = math.acos(r)*rad -- angles in degrees for viewdir (rad = 180/pi)
        theta = alpha*rad
        f = function(A)
                if isPoint3d(A) then  -- We only transform the 3D points; the rest remains unchanged.
                    return Z(A.x-k_cos_alpha*A.z, A.y-k_sin_alpha*A.z)
                else return A end
            end
    elseif mode == "iso" then  -- isometric perspective, one unit on each axe has the same length on screen
        phi = 45 -- angles in degrees for viewdir 
        theta = 45
        local a, b = math.sqrt(2)/2, 1/math.sqrt(6)
        f = function(A)
                if isPoint3d(A) then  -- We only transform the 3D points; the rest remains unchanged.
                    return Z( a*(A.y-A.x), 2*b*A.z-b*(A.x+A.y) )
                else return A end
            end
    elseif mode == "central" then
        if central_perspective == nil then
            print("You need : require 'luadraw_central_perspective' before using perspective('central()'")
            function luadraw_graph3d:Proj3d(L)
                return self:orthographic_Proj3d(L) -- default projection
            end
            return {theta,phi,"ortho"}
        else
            return central_perspective(theta,phi,d,look)
        end
    else 
        mode = "ortho"
        function luadraw_graph3d:Proj3d(L)
            return self:orthographic_Proj3d(L) -- default projection
        end
    end
    if f ~= nil then
        function luadraw_graph3d:Proj3d(L) -- we redefine Proj3d
            L = self:Mtransform3d(L) -- we apply the 3D matrix of the graph
            return ftransform3d(L,f) -- we return the projection on screen
        end  
    end
    return {theta,phi,mode}
end

function luadraw_graph3d:Setviewdir(theta,phi) -- direction de l'observateur avec theta et phi en degrés
    if (theta == nil) then return end
    local mode = "ortho"
    if theta == "xOy" then theta = -90; phi = 0
    elseif theta == "yOz" then theta = 0; phi = 90
    elseif theta == "xOz" then theta = -90; phi = 90
    elseif type(theta) == "table" then 
        theta, phi, mode = table.unpack(theta)
        if mode == nil then mode= "ortho" end
    end
    if (projection_mode == "central") and (mode ~= "central") then close_central() end
    if mode == "ortho" then
        function luadraw_graph3d:Proj3d(L)
            return self:orthographic_Proj3d(L)
        end
    end
    projection_mode = mode
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
    if projection_mode == "ortho" then -- c'est l'image du vecteur vecJ par la rotation d'axe Oz et d'angle theta
        return M(-self.sinTheta, self.cosTheta,0)
    elseif projection_mode == "yz" then return vecJ
    elseif projection_mode == "xz" then return vecI
    elseif projection_mode == "xy" then return vecI
    elseif projection_mode == "iso" then return M(-1/math.sqrt(2),1/math.sqrt(2),0)
    elseif projection_mode == "central" then return self:Screenpos(Z(1,0))
    end
end

function luadraw_graph3d:ScreenY()
-- renvoie les coordonnées spatiales du deuxième vecteur de base du plan de l'écran (affixe i)
    if projection_mode == "ortho" then -- c'est le produit vectoriel entre les vecteurs self.Normal et self:ScreenX()
        return M(-self.cosPhi*self.cosTheta, -self.cosPhi*self.sinTheta, self.sinPhi)
    elseif projection_mode == "yz" then return vecK
    elseif projection_mode == "xz" then return vecK
    elseif projection_mode == "xy" then return vecJ
    elseif projection_mode == "iso" then return M(0,0,math.sqrt(6)/2)
    elseif projection_mode == "central" then return self:Screenpos(Z(0,1))
    end
end

function luadraw_graph3d:Screenpos(z,d)
-- renvoie les coordonnées spatiales du vecteur ayant comme projeté sur l'écran le point d'affixe z,
-- et se trouvant à une distance d (algébrique) du plan de l'écran
    z = toComplex(z)
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

function luadraw_graph3d:orthographic_Proj3d(L) -- projection de points sur l'écran (plan passant par l'origine et normal au vecteur self.Normal
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

function luadraw_graph3d:Proj3d(L)
    return self:orthographic_Proj3d(L)
end

function luadraw_graph3d:Proj3dV(L) -- projection de vecteurs sur l'écran (plan passant par l'origine et normal au vecteur self.Normal
-- L est un point3d ou une liste de point3d ou une liste de listes de point3d
    if (L == nil) or (type(L) ~= "table") then return end
    local oldmatrix3d = self.matrix3d
    if not isID3d(self.matrix3d) then
        L = mLtransform3d(L,self.matrix3d) --<- c'est ici que ce fait la différence avec les points
    end
    self.matrix3d = ID3d
    local rep = self:Proj3d(L)
    self.matrix3d = oldmatrix3d
   return rep 
end


------ dessins de lignes 3d

function luadraw_graph3d:Dpolyline3d(L,close,draw_options,clip)
    if type(close) == "string" then clip = draw_options; draw_options = close; close = false end
    clip = clip or false
    if clip then L = clippolyline3d(L,self:Box3d()) end
    local aux = self:Proj3d(L)
    self:Dpolyline(aux,close,draw_options)
end

function luadraw_graph3d:Dline3d(d,B,draw_options,clip)
--trace la droite d (si B=nil) ou bien la droite passant par les points d et B
    if type(B) == "string" then clip = draw_options; draw_options = B; B = nil end
    clip = clip or false
    local A, u
    if B == nil then 
        A = d[1]; B = A+d[2]
    else
        A = d
    end
    if clip then 
        self:Dpolyline3d( clipline3d({A,B-A},self:Box3d()),false,draw_options) 
    else
        self:Dline(self:Proj3d(A),self:Proj3d(B),draw_options)
    end
end

function luadraw_graph3d:Dseg3d(seg,scale,draw_options,clip)
--trace le segment seg 
    clip = clip or false
    if clip then 
        seg = clippolyline3d(seg,self:Box3d()) 
        if seg ~= nil then seg = seg[1] end
    end
    self:Dseg(self:Proj3d(seg),scale,draw_options)
end

function luadraw_graph3d:Dparametric3d(p,args)
-- dessin d'une courbe paramétrée par la fonction p:t -> p(t) sur l'intervalle [t1;t2] (à valeurs dans R^3)
-- args est une table à 5 entrées args = { t = {t1,t2}, nbdots = 50,clip=false discont =false, nbdiv = 5, draw_options = "" }
    args = args or {}
    local t = args.t or {self:Xinf(), self:Xsup()}
    local nbdots = args.nbdots or 50
    local discont = args.discont or false
    local clip = args.clip or false
    local nbdiv = args.nbdiv or 5
    local draw_options = args.draw_options or ""
    if draw_options == "" then draw_options = "line join=round"
    else draw_options = "line join=round,"..draw_options end -- jointure arrondie
    local t1, t2 = table.unpack(t)
    if t1 > t2 then t1, t2 = t2, t1 end
    local C = parametric3d(p,t1,t2,nbdots,discont,nbdiv)
    self:Dpolyline3d(C,false,draw_options,clip)
end

function luadraw_graph3d:Darc3d(B,A,C,R,sens,normal,draw_options,clip)
-- dessine un arc de cercle de centre A, dans le plan ABC, de AB vers AC.
-- ce plan est orienté par le vecteur AB^AC ou le vecteur normal s'il est précisé
    if type(normal) == "string" then clip = draw_options; draw_options = normal; normal = nil end
    clip = clip or false
    if clip then
        local chem = arc3d(B,A,C,R,sens,normal)
        self:Dpolyline3d(chem,false,draw_options,clip)
    else
        local chem = arc3db(B,A,C,R,sens,normal)
        --self:Dpath(self:Proj3d(chem),draw_options)
        self:Dpath3d(chem,draw_options)
    end
end    


function luadraw_graph3d:Dcircle3d(C,R,normal,draw_options,clip)
-- dessine un cercle de centre C de rayon R.
-- dans le plan défini par C et le vecteur normal
    if (type(C) == "table") and (not isPoint3d(C)) then -- C est une table
        if type(R) == "string" then clip = draw_options; draw_options = R end
        C,R,normal = table.unpack(C)
    end
    clip = clip or false
    if R == 0 then self:Ddots3d(C)
    else
        if clip then
            local chem = circle3d(C,R,normal)
            self:Dpolyline3d(chem,false,draw_options,clip)
        else
            local chem = circle3db(C,R,normal)
            --self:Dpath(self:Proj3d(chem),draw_options)
            self:Dpath3d(chem,draw_options)
        end
    end
end

function luadraw_graph3d:Dangle3d(B,A,C,r,draw_options,clip)
    if type(r) == "string" then clip = draw_options; draw_options = r; r = nil end
    r = r or 0.25
    clip = clip or false
    local u, v = B-A, C-A
    u, v = r*pt3d.normalize(u), r*pt3d.normalize(v)
    return self:Dpolyline3d( {A+u,A+u+v,A+v},false,draw_options,clip)
end


-------- points et labels

function luadraw_graph3d:Clipdots(L)
    local x1,x2,y1,y2,z1,z2 = table.unpack( self.param.viewport3d )
    local rep = {}
    local isin = function(A)
        local x,y,z = A.x, A.y, A.z
        if (x1<=x) and (x<=x2) and (y1<=y) and (y<=y2) and (z1<=z) and (z<=z2) then
        return A
        end
    end
    return ftransform3d(L,isin)
end

function luadraw_graph3d:Ddots3d(L,mark_options,clip)
    clip = clip or false
    if clip then L = self:Clipdots(L) end
    self:Ddots(self:Proj3d(L), mark_options)
end

function luadraw_graph3d:Dballdots3d(L,color,scale,clip) -- points sphériques
    clip = clip or false
    if clip then L = self:Clipdots(L) end
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

function luadraw_graph3d:Dcrossdots3d(L,color,scale,angle,clip) -- points en forme de croix dans un plan
-- L est une liste du type {point 3d, vecteur normal} ou { {point3d, vecteur normal}, {point3d, vecteur normal}, ...}
    color = color or self.param.linecolor
    scale = scale or 1
    angle= angle or 0
    local long = 0.125*scale
    local lg, A, normal, a, b, c = {}    
    clip = clip or false
    local x1,x2,y1,y2,z1,z2 = table.unpack( self.param.viewport3d )
    
    local calcAdot = function()
        local n = pt3d.normalize(normal)
        local u = pt3d.prod(n,self.Normal)
        if pt3d.N1(u)<1e-10 then u = self:ScreenX() else u = pt3d.normalize(u)end
        local v = pt3d.prod(n,u)
        if angle ~= 0 then
            u, v = table.unpack( rotate3d({u,v},angle,{Origin,n}) )
        end
        a, b, c = table.unpack(self:Proj3d({A,A+u-v,A+u+v})) -- Proj3d s'applique aux points (pas aux vecteurs)
        b = b-a; c = c-a
        b, c = long*b/self:Abs(b), long*c/self:Abs(c)
    end
    
    local oldcolor = self.param.linecolor
    local oldstyle = self.param.linestyle
    local oldwidth = self.param.linewidth
    local oldfillstyle = self.param.fillstyle
    self:Lineoptions("solid",color,4); self:Filloptions("none")
    if isPoint3d(L[1]) then L = {L} end
    for _, P in ipairs(L) do
        A = P[1]; 
        if (not clip) or ( (x1<=A.x) and (A.x<=x2) and (y1<=A.y) and (A.y<=y2) and (z1<=A.z) and (A.z<=z2) ) 
        then
            normal = P[2]; calcAdot()
            insert(lg, {{a+b,a-b},{a-c,a+c}})
        end
    end
    self:Dpolyline(lg)
    self:Lineoptions(oldstyle,oldcolor,oldwidth); self:Filloptions(oldfillstyle)
end

function luadraw_graph3d:Dlabel3d(...)
    local args = {}
    local text, anchor, anchor2d, options
    local dir = {}
    for k, aux in ipairs{...} do
        if  k%3 == 2 then anchor = aux; anchor2d = self:Proj3d(aux) 
        elseif k%3 == 0 then --arguments
            options = aux
            options.dir = options.dir or dir
            dir = options.dir
            if #options.dir > 1 then
                local U, V = table.unpack(options.dir)
                U = pt3d.normalize(U); V = pt3d.normalize(V)
                options.dir = {self:Proj3d(anchor+U)-anchor2d, self:Proj3d(anchor+V)-anchor2d} -- ce sont des vecteurs
            end
            insert(args,{text,anchor2d,options})
        else text = aux
        end
    end
    self:Dlabel(table.unpack(args))
end

------- solides sans facettes (fil de fer)

function luadraw_graph3d:define_temp_color(argsColor)
    if type(argsColor) == "table" then 
        argsColor = rgb(argsColor) 
        argsColor = string.sub(argsColor,2,#argsColor-1) -- on retire les accolades
    end
    if (type(argsColor) == "string") and (string.find(argsColor,"%A")~=nil) 
    then
        self:Writeln("\\colorlet{tempColor}{"..argsColor.."}")
        return "tempColor"
    else return argsColor
    end
end

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
    args.color = self:define_temp_color(args.color)
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
    local mat = self.matrix3d
    local N = mLtransform3d(self.Normal,invmatrix3d(mat))
    if pt3d.dot(self.Normal,self:MLtransform3d(V)) <= 0 then V = -V  end
    if pt3d.dot(self:MLtransform3d(V),self:MLtransform3d(B-A)) < 0 then A,B=B,A  end -- V et B-A dans le même sens
    local W = B-A
    local angle = self:Arg(self:Proj3dV(W))*rad
    if angle < 0 then angle = angle+180
        elseif angle > 180 then angle = angle-180 
    end
    self:Linecolor(args.edgecolor)
    local dcircle = function(center)
        if args.color ~= "" then
            self:Filloptions("gradient", "left color="..args.color.."!25,right color = "..args.color.."!50,middle color="..args.color.."!18, shading angle="..strReal(angle),args.opacity)
        else
            self:Filloptions("none") 
        end
        self:Dcircle3d(center,r,V)
    end    
    local I = pt3d.normalize(V) -- vecteur normal au plan et dans la direction du sommet B
    local J = pt3d.prod(I,N); J = pt3d.normalize(J)
    if (J == nil) then -- le plan de la base circulaire est l'écran
        J = self:ScreenX()
    end
    local K = pt3d.prod(I,J) -- base = {A+r.cos(t)J+r.sin(t)K / t in [-pi,pi]}
    local xn,yn, zn = pt3d.dot(N,I), pt3d.dot(N,J), pt3d.dot(N,K)
    local xw,yw,zw = pt3d.dot(W,I), pt3d.dot(W,J), pt3d.dot(W,K)
    local t = solve(function(t) return math.sin(t)*(xn*zw-zn*xw)+math.cos(t)*(xn*yw-xw*yn) end,-math.pi/2,3*math.pi/2)
        if (t == nil) or (#t == 1) then
            dcircle(A)
    else
        t1, t2 = table.unpack(t) 
        local M1 = A+math.cos(t1)*r*J+math.sin(t1)*r*K
        local M2 = A+math.cos(t2)*r*J+math.sin(t2)*r*K
        if math.cos(t1)*math.sin(t2)-math.cos(t2)*math.sin(t1)< 0 then M1,M2 = M2,M1 end
        local N1 = M1+W
        local N2 = M2+W
        if pt3d.N1(M2-M1) < 1e-12 then -- points confondus
            dcircle(A)
        else
            if args.color == "" then 
                self:Filloptions("none") 
            else
                self:Filloptions("gradient", "left color="..args.color.."!50,right color = "..args.color..",middle color="..args.color.."!10, shading angle="..strReal(angle),args.opacity)
            end
            if args.mode == 1 then self:Linestyle("noline") end
            -- on voit la base circulaire en B, le vecteur I sort du cylindre en B et est dirigé vers l'observateur
            local sens = 1
            --if pt3d.det(W,A-M1,M2-M1)*pt3d.det(W,B-N1,N2-N1) < 0 then sens = -sens end
            if cpx.det(self:Proj3dV(B-A),self:Proj3dV(B-N2))*self:Det3d() < 0 then sens = -sens end
            --self:Dpath3d({M1,A,M2,r,sens,V,"ca",N2,"l",B,N1,r,sens,V,"ca","cl"})
            self:Dpath3d({M1,A,M2,r,sens,V,"ca",N2,"l",B,N1,r,-sens,V,"ca","cl"})
            dcircle(B)
            --self:Filloptions("none")
            --self:Darc3d(N1,B,N2,r,sens,V)
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
-- color = "" : pas de remplissage, color ~= "" remplissage avec gradient bi linéaire
    if isPoint3d(r) then -- ancien format : sommet, vecteur, rayon, args (cône droit)
        args = A; A = C
        r, V = V, r
        C = A+V
    elseif not isPoint3d(A) then -- format C,r,A,args (cône droit)
            args = A; A = V; V = A-C
    end
    args = args or {}
    args.color = args.color or ""
    args.color = self:define_temp_color(args.color)
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
    local angle = self:Arg(self:Proj3dV(W))*rad
    if angle < 0 then angle = angle+180
    elseif angle > 180 then angle = angle-180 
    end    
    local dcircle = function(center)
        if args.color ~= "" then
            self:Filloptions("gradient", "left color="..args.color.."!25,right color = "..args.color.."!50,middle color="..args.color.."!18, shading angle="..strReal(angle),args.opacity)
        else
            self:Filloptions("none") 
        end
        self:Dcircle3d(center,r,V)
    end    
    local xw,yw,zw = pt3d.dot(W,I), pt3d.dot(W,J), pt3d.dot(W,K)
    local t = solve(function(t) return math.sin(t)*(xn*zw-zn*xw)+math.cos(t)*(xn*yw-xw*yn)+r*xn end,0,2*math.pi)
    if (t == nil) or (#t == 1) then
        dcircle(C)
    else
        t1, t2 = table.unpack(t) 
        local M1 = C+math.cos(t1)*r*J+math.sin(t1)*r*K
        local M2 = C+math.cos(t2)*r*J+math.sin(t2)*r*K
        if math.cos(t1)*math.sin(t2)-math.cos(t2)*math.sin(t1)< 0 then M1,M2 = M2,M1 end
        if pt3d.N1(M2-M1) < 1e-12 then -- points confondus
            dcircle(C)
        else
            if args.color == "" then 
                self:Filloptions("none") 
            else
                --self:Filloptions("gradient", "left color=white,right color = "..args.color..", shading angle="..strReal(angle),args.opacity)
                self:Filloptions("gradient", "left color="..args.color.."!50,right color = "..args.color..",middle color="..args.color.."!10, shading angle="..strReal(angle),args.opacity)
            end
            if args.mode == 1 then self:Linestyle("noline") end
            local sens = 1
            if cpx.det(self:Proj3dV(C-M1),self:Proj3dV(M2-M1))*cpx.det(self:Proj3dV(A-M1),self:Proj3dV(M2-M1)) < 0 then sens = -1 end
            if pt3d.dot(self.Normal,self:MLtransform3d(V)) <= 0 then -- on voit la base circulaire
                self:Dpath3d({A,M1,"l",C,M2,r,-sens,V,"ca","cl"})
                dcircle(C)
                --self:Filloptions("none")
                --self:Darc3d(M1,C,M2,r,-sens,V)
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
            self:Filloptions("gradient","left color="..args.color.."!25,right color = "..args.color.."!50,middle color="..args.color.."!18",args.opacity)
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
    args.color = self:define_temp_color(args.color)
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
        local angle = self:Arg(self:Proj3dV(W))*rad
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
                self:Filloptions("gradient", "left color="..args.color.."!50,right color = "..args.color..",middle color="..args.color.."!10, shading angle="..strReal(angle),args.opacity)
            end
            if args.mode == 1 then self:Linestyle("noline") end
            local sens = 1
            --if cpx.det(self:Proj3d(C-M1),self:Proj3d(M2-M1))*cpx.det(self:Proj3d(A-M1),self:Proj3d(M2-M1)) < 0 then sens = -1 end
            if pt3d.det(I,C-M1,M2-M1)*pt3d.det(I,A-M1,M2-M1) < 0 then sens = -1 end
            if pt3d.dot(self.Normal,self:MLtransform3d(V)) >= 0 then -- on voit la petite base circulaire (C,r)
                self:Dpath3d({M3,M1,"l",C,M2,r,-sens,V,"ca",M4,"l",A,M3,R,sens,V,"ca"})
                if args.color ~= "" then
                    self:Filloptions("gradient","left color="..args.color.."!25,right color = "..args.color.."!50,middle color="..args.color.."!18",args.opacity)
                else self:Filloptions("none")
                end
                --self:Darc3d(M1,C,M2,r,-sens,V)
                self:Dcircle3d(C,r,V)
                if (args.mode ~= 1) and (args.hiddenstyle ~= "noline") then -- partie cachée
                    self:Filloptions("none")
                    self:Lineoptions(args.hiddenstyle,args.hiddencolor)
                    self:Darc3d(M4,A,M3,R,-sens,V)
                end
            else -- la grande base circulaire (A,R)
                self:Dpath3d({M3,M1,"l",C,M2,r,sens,V,"ca",M4,"l",A,M3,R,-sens,V,"ca"})
                if args.color ~= "" then
                    self:Filloptions("gradient","left color="..args.color.."!25,right color = "..args.color.."!50,middle color="..args.color.."!18",args.opacity)
                else self:Filloptions("none")
                end
                --self:Darc3d(M4,A,M3,R,-sens,V)            
                self:Dcircle3d(A,R,V)
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
    self:Lineoptions(args.edgestyle,args.edgecolor,args.edgewidth)
    self:Dcircle3d(A,r,N)
    if  args.mode == 0 then -- équateur
        local u = pt3d.normalize(pt3d.prod(N,V))
        local M1, M2 = A+r*u, A-r*u
        self:Filloptions("none") --; self:Lineoptions(args.edgestyle,args.edgecolor,args.edgewidth)
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

function luadraw_graph3d:Cosine_incidence(n,A)
-- cosinus de l'angle d'incidence entre le vecteur n (unitaire) au point A et le vecteur dirigé vers l'observateur
    return pt3d.dot(self.Normal,n)
end

function luadraw_graph3d:Observer_distance(A)
-- l'abscisse de A sur l'axe issue de Origine, dirigé vers l'observateur
    return pt3d.dot(self.Normal,A)
end

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
    backculling = backculling or false
    local rep, aux = {}, {}
    for k,L in ipairs(F1) do -- on travaille sur les sommets transformés
        local G1 = isobar3d(L)
        table.insert(aux, {k,self:Observer_distance(G1)}) --pt3d.dot(G1,self.Normal)})
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
-- args est une table à  6 champs :
-- args = {hidden=true/false, visible=true/false, clip=false, hiddenstyle, style=default, hiddencolor=default, color=default, width=default}
    args = args or {}
    if args.hidden == nil then args.hidden = Hiddenlines end
    args.color = args.color or self.param.linecolor
    args.hiddencolor = args.hiddencolor or args.color
    args.style = args.style or self.param.linestyle
    args.width = args.width or self.param.linewidth
    args.clip = args.clip or false
    if args.visible == nil then args.visible = true end
    args.hiddenstyle = args.hiddenstyle or Hiddenlinestyle
    local oldfillstyle = self.param.fillstyle
    local oldlinestyle = self.param.linestyle
    local oldlinecolor = self.param.linecolor
    local oldlinewidth = self.param.linewidth
    self:Filloptions("none")
    if args.hidden and (args.hiddenstyle ~= "noline") then
        self:Lineoptions(args.hiddenstyle,args.hiddencolor,args.width)    
        self:Dpolyline3d(edges.hidden,false,"",args.clip)
    end
    if args.visible then 
        self:Lineoptions(args.style,args.color,args.width)    
        self:Dpolyline3d(edges.visible,false,"",args.clip) 
    end
    self:Lineoptions(oldlinestyle,oldlinecolor,oldlinewidth)
    self:Filloptions(oldfillstyle)
end

-- pour le dessin de facettes
function define_getcolor(F, pal, mode, default_color) -- used by drawfacet(), Dmixfacet() and addFacet()
-- F = list of facets
-- pal = palette of colors (rgb tables)
-- mode = "x", "y", or "z"
-- returns the fonction : getcolor(facet)
    default_color = default_color or White
    local x1,x2,y1,y2,z1,z2 = getbounds3d(F)
    local getcolor = function(f) -- f is a facet, returns the color of f
        local A = isobar3d(f)
        if mode == "x" then return palette(pal,(A.x-x1)/(x2-x1),true)
        elseif mode == "y" then return palette(pal,(A.y-y1)/(y2-y1),true)
        elseif mode == "z" then return palette(pal,(A.z-z1)/(z2-z1),true)
        else return default_color
        end
    end
    return getcolor
end

function luadraw_graph3d:adjust_color(F,color,contrast,twoside) -- used by drawfacet(), Dmixfacet() and addFacet()
-- F = facet, contrast in [0,1], twoside=true/false
-- adjust color based on facet normal vector (scalar product between the normal vector of the facet and the vector directed towards the observer.)
-- returns a color (string), normal vector and coef
    local A, B, C, k, n, m, coef, ok, newcolor, N, r, g, b, neg, c, G, V
    if projection_mode == "central" then G = isobar3d(F); V = pt3d.normalize(camera-G)
    else V = self.Normal
    end
    k = 2; m = #F; ok = false
    while (not ok) and (k < m) do
        A, B, C = F[1], F[k], F[k+1]
        N = pt3d.normalize(pt3d.prod(B-A,C-A))
        coef = pt3d.dot(V,N)
        ok = (coef ~= nil)
        k = k+1
    end
    if ok then 
        if math.abs(coef) < 1e-8 then coef = 0 end
        neg = twoside and (coef < 0)
        c = round(math.exp( math.log(math.abs(coef))/2.5*contrast),4)
        if notDef(c) then c = 0 end
        if type(color) == "string" then -- color must be a color name
            if neg then
                newcolor = color.."!50!white!"..strReal(100*c).."!black"
            else
                newcolor = color.."!"..strReal(100*c).."!black"
            end
        else 
            r, g, b = table.unpack(color)
            if neg then 
                newcolor = rgb({c*(1+r)/2,c*(1+g)/2,c*(1+b)/2})
            else
                newcolor = rgb({c*r,c*g,c*b})
            end
        end
        return newcolor, N, coef
    end
end


function luadraw_graph3d:drawfacet(S,args) -- internal use by Dpoly,and Dfacet, S is a list of already sorted facets
    local oldfillstyle = self.param.fillstyle
    local oldfillopacity = self.param.fillopacity
    local oldfillcolor = self.param.fillcolor
    local oldlinestyle = self.param.linestyle
    local oldlineopacity = self.param.lineopacity
    local oldlinecolor = self.param.linecolor
    local oldlinewidth = self.param.linewidth
    local coul = args.color
    local getcolor
    local usepalette = args.usepalette -- palette = {Pal, "x", or "y" or "z"}, Pal is a list of colors in RGB table format
    if usepalette ~= nil then
        local pal, mode = table.unpack(usepalette)
        getcolor = define_getcolor(S,pal,mode,coul)
    end
 
    if (args.mode == mShadedOnly) and (args.opacity ~= 1)  then self:Linestyle("noline") end
    if (args.mode ~= mShadedOnly) then self:Lineoptions(args.edgestyle,args.edgecolor,args.edgewidth) end
    
    -- dessin des facettes
    if (usepalette == nil) and ((args.mode == mFlat) or (args.mode == mFlatHidden)) then -- facets of the same color defined by args.color
        if type(coul) == "table" then coul = rgb(coul) end
        self:Filloptions("full",coul,args.opacity)
        self:Dpolyline3d(S,true)
    elseif usepalette ~= nil then  -- color of each facet chosen from a palette
        for _,F in ipairs(S) do
            if (args.mode == mFlat) or (args.mode == mFlatHidden) then
                coul = rgb(getcolor(F))
            else
                coul = self:adjust_color(F, getcolor(F),args.contrast,args.twoside)
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
    else --modes 3, 4 ou 5 couleur des facettes uniques mais nuancées en fonction du vecteur normal
        for _,F in ipairs(S) do
            coul = self:adjust_color(F,args.color,args.contrast,args.twoside)
            if coul ~= nil then
                self:Filloptions("full",coul,args.opacity)
                if (args.mode == mShadedOnly) then 
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
-- args est une table à 11 champs :
-- args = {mode=0/1/2/3/4/5, contrast=1, backcull=true/false,clip=false, edgestyle=defaut, edgecolor=default, edgewidth=defaut, twoside=true/false, color=White, usepalette=nil, opacity=1}    
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
    args.clip = args.clip or false
    args.color = args.color or "white"
    if args.twoside == nil then args.twoside = true end
    if args.clip then F = clip3d(F, self:Box3d()) end
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
-- args est une table à 11 champs :
-- args = {mode=0/1/2/3/4/5, contrast=1, backcull=true/false,clip=false, edgestyle=defaut, edgecolor=default, edgewidth=defaut, twoside=true/false, color=White, opacity=1, usepalette=nil}    
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
    local clip = false
    local color = "white"
    local opacity = 1
    local S, face, args = {}
    for k,F in ipairs{...} do
        if k%2 == 1 then 
            face = F
        else
            args = F
            if args.mode == nil then args.mode = mode else mode = args.mode end
            if args.contrast == nil then args.contrast = contrast else contrast = args.contrast end
            if args.edgestyle == nil then args.edgestyle = edgestyle else edgestyle = args.edgestyle end
            if args.edgecolor == nil then args.edgecolor = edgecolor else edgecolor = args.edgecolor end
            if args.edgewidth == nil then args.edgewidth = edgewidth else edgewidth = args.edgewidth end
            if args.opacity == nil then args.opacity = opacity else opacity = args.opacity end
            if args.backcull == nil then args.backcull = backcull else backcull = args.backcull end
            if args.clip == nil then args.clip = clip else clip = args.clip end
            if args.color == nil then args.color = color else color = args.color end
            if args.twoside == nil then args.twoside = twoside else twoside = args.twoside end
            if args.usepalette == nil then
                args.getcolor = function(f)
                    return args.color
                end
            else
                local pal, mode = table.unpack(args.usepalette)
                args.getcolor = define_getcolor(face,pal,mode,args.color)
            end
            if isPoint3d(face[1]) then face = {face} end
            if args.clip then face = clip3d(face,self:Box3d()) end
            if not isID3d(self.matrix3d) then face = self:Mtransform3d(face) end -- on applique la matrice de transformation
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
    local coul
    S = self:Sortfacet(S)
    for _,F in ipairs(S) do
        args = table.remove(F)
        self:Lineoptions(args.edgestyle,args.edgecolor,args.edgewidth)
        -- dessin
        if (args.mode == mWireframe) then -- arêtes seulement (mWireframe)
            self:Filloptions("none")
            self:Dpolyline3d(F,true)
        elseif (args.mode == mFlat) or (args.mode == mFlatHidden) then --faces unies
            coul = args.getcolor(F)
            if type(coul) == "table" then coul = rgb(coul) end
            self:Filloptions("full",coul,args.opacity)
            self:Dpolyline3d(F,true)
        else --mode = 3,4 ou 5 faces peintes
            coul = self:adjust_color(F,args.getcolor(F),args.contrast,args.twoside)
            if coul ~= nil then
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
-- args table à 13 champs
-- {color="white", opacity=1, boxed=false, backcull=false,clip=false edgewidth=6, edge=false, edgecolor="black", hidden=false, hiddenstyle="dotted", contrast=1, twoside=false, matrix=ID3d, usepalette=nil}
    if facet == nil then return {{"nul"}} end
    args = args or {}
    if isPoint3d(facet[1]) then facet = {facet} end
    local color = args.color or "White"
    local opacity = args.opacity or 1
    local backcull = args.backcull or false
    local clip = args.clip or false
    local boxed = args.boxed or false
    local contrast = args.contrast or 1
    local twoside = args.twoside
    local hidden = args.hidden
    if hidden == nil then hidden = Hiddenlines end
    local hiddenstyle = args.hiddenstyle or Hiddenlinestyle
    if twoside == nil then twoside = true end
    local edge = args.edge or false
    local edgecolor = args.edgecolor or self.param.linecolor
    local edgewidth = args.edgewidth or self.param.linewidth
    local matrix = args.matrix or ID3d
    matrix = composematrix3d(self.matrix3d,matrix)
    local oldmatrix = self.matrix3d
    self.matrix3d = ID3d
    local F = facet
    if clip then F = clip3d(F,self:Box3d()) end
    if not isID3d(matrix) then 
        F = mtransform3d(F,matrix)
    end
    local x1,x2,y1,y2,z1,z2, getcolor 
    if args.usepalette == nil then
        getcolor = function(f)
            return color
        end
    else
        local pal, mode = table.unpack(args.usepalette)
        getcolor = define_getcolor(F,pal,mode,color)
    end    
    local res = {}
    local rep = {"facet"}
    if boxed then 
        if x1 == nil then x1,x2,y1,y2,z1,z2 = getbounds3d(F) end
        local eps = 1e-4
        x1 = x1-eps; x2 = x2+eps
        y1 = y1-eps; y2 = y2+eps
        z1 = z1-eps; z2 = z2+eps
        local P = parallelep(M(x1,y1,z1),M(x2-x1,0,0),M(0,y2-y1,0),M(0,0,z2-z1))
        insert(res, self:addWall(poly2facet(P),{matrix=ID3d}))
    end
    for _, face in ipairs(F) do
        local coul,n,coef = self:adjust_color(face,getcolor(face),contrast,twoside)
        if coul ~= nil then
            if coef < 0 then n = -n  end -- n must be directed towards the observer
            if (not backcull) or (coef > 0)  then            
                table.insert(rep, {face,{face[1],n},coul,opacity})
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
-- args table à 10 champs
-- { color="black", style="solid", width=4, opacity=1, hidden=false, clip=false hiddenstyle=dotted, arrows=0/1/2, arrowscale=1, close=true/false, matrix=ID3d }
    if Line == nil then return {{"nul"}} end
    args = args or {}
    local color = args.color or self.param.linecolor
    local style = args.style or self.param.linestyle
    local width = args.width or self.param.linewidth
    local opacity = args.opacity or 1
    local arrows = args.arrows or 0
    local close = args.close or false
    local clip = args.clip or false
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
    if clip then Line = clippolyline3d(Line,self:Box3d(),false,close) end
    if isPoint3d(Line[1]) then Line = {Line} end
    for _,L in ipairs(Line) do
        if not isID3d(matrix) then 
            L = mtransform3d(L,matrix)
        end
        if close and (not clip) then table.insert(L,L[1]) end
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
    --local style = self.param.labelstyle
    local pos = self.param.labelstyle
    local size = self.param.labelsize
    local angle = self.param.labelangle
    local dir = self.param.labeldir
    local showdot = false
    local dist = 0
    local matrix = ID3d
    
    local addAlabel = function()
        color = args.color or color
        --style = args.style or style
        pos = args.pos or args.style or pos
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
        table.insert(rep, {"label",text,dot,pos,dist,color,size,angle,dir,showdot} )
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
-- {color="white", opacity=1, backcull=false, clip=false, edgewidth=6, edge=false, boxed=false, edgecolor="black", hidden=false, hiddenstyle="dotted", contrast=1, twoside=false, matrix=ID3d}
    if P == nil then return {{"nul"}} end
    args = args or {}
    args.edge = args.edge or false
    args.edgecolor = args.edgecolor or self.param.linecolor
    args.edgewidth = args.edgewidth or self.param.linewidth
    args.clip = args.clip or false
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
        insert(rep, self:addPolyline( facetedges(P1),{color=args.edgecolor, clip=args.clip, hidden=hidden, hiddenstyle=hiddenstyle, width=args.edgewidth, matrix=ID3d}))
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
-- { color=défaut, style="solid", width=4, hidden=false, hiddenstyle="dotted", legend=true/false, opacity=1, arrows=0/1/2, arrowscale=1, matrix=ID3d }
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
    local face = clipplane(plane,self:Box3d())
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
    local L = clipline3d(d,self:Box3d())
    if L == nil then return end
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
    if L == nil then return {{nul}}
    else return self:addPolyline( L, args )
    end
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
function luadraw_graph3d:Dpath3d(L,draw_options,clip) 
-- dessine le chemin contenu dans L, L est une table de point3d et d'instructions
-- ex: Dpath3d( {M(0,-3,0), Origin, vecK,"c", Origin,"m",M(1,1,0),"l",Origin,M(1,1,2),2.5,1,"ca","cl"} )
-- "m" pour moveto, "l" pour lineto, "b" pour bézier, "c" pour cercle, "ca" pour arc de cercle, "s" pour spline naturelle, "cl" pour close
    if (L == nil) or (type(L) ~= "table") or (#L < 3) then return end
    clip = clip or false
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
    if clip then
        self:Dpolyline3d(path3d(L),false,draw_options,clip)
    else
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
    args.xyzstep = args.xyzstep or 1
    args.xstep = args.xstep or args.xyzstep
    args.ystep = args.ystep or args.xyzstep
    args.zstep = args.zstep or args.xyzstep
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
    local Normal = self.Normal
    if projection_mode == "central" then Normal = pt3d.normalize(camera) end
    theta = theta%360
    phi = phi%360
    if angle3d(Normal,vecK,eps) > math.pi/2 then 
       zinf, zsup = zsup, zinf
    end
    local psi
    if phi <= 180 then psi = theta 
    else psi = (theta+180)%360
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
        xdir = Bottom-Right; if pt3d.abs(pt3d.prod(xdir,Normal)) < eps then xdir = pz(Bottom-Top) end
        pris = 1
    elseif Left.y ~= Bottom.y then --axe Oy
        if Left.y < Bottom.y then axeOy = Left else axeOy = Bottom end
        ydir = Bottom-Right; if pt3d.abs(pt3d.prod(ydir,Normal)) < eps then ydir = pz(Bottom-Top) end
        pris = 2
    elseif Left.z ~= Bottom.z then --axe Oz
        if Left.z < Bottom.z then axeOz = Left else axeOz = Bottom end
        zdir = Bottom-Left; if pt3d.abs(pt3d.prod(zdir,Normal)) < eps then zdir = (Bottom-Right) end
        pris = 3
    end
     if Right.x ~= Bottom.x then --axe Ox
        if Right.x < Bottom.x then axeOx = Right else axeOx = Bottom end
        xdir = Bottom-Left; if pt3d.abs(pt3d.prod(xdir,Normal)) < eps then xdir = pz(Bottom-Top) end
        pris = pris+1
    elseif Right.y ~= Bottom.y then --axe Oy
        if Right.y < Bottom.y then axeOy = Right else axeOy = Bottom end
        ydir = Bottom-Left; if pt3d.abs(pt3d.prod(ydir,Normal)) < eps then ydir = pz(Bottom-Top) end
        pris = pris+2
    elseif Right.z ~= Bottom.z then --axe Oz
        if Right.z < Bottom.z then axeOz = Right else axeOz = Bottom end
        zdir = Bottom-Left; if pt3d.abs(pt3d.prod(zdir,Normal)) < eps then zdir = (Bottom-Right) end
        pris = pris+3
    end
    if 6-pris == 1 then --axe Ox
        if Left.x == xsup then axeOx = M(xinf,0,0)+pyz(Left) else axeOx = Left end
        xdir = -pt3d.normalize(Bottom-Left); if pt3d.abs(pt3d.prod(xdir,Normal)) < eps then xdir = pz(Bottom-Top) end
    elseif 6-pris == 2 then --axe Oy
        if Left.y == ysup then axeOy = M(0,yinf,0)+pxz(Left) else axeOy = Left end
        ydir = -(Bottom-Left); if pt3d.abs(pt3d.prod(ydir,self.Normal)) < eps then ydir = pz(Bottom-Top) end
    elseif 6-pris == 3 then --axe Oz
        if Left.z == zsup then axeOz = M(0,0,zinf)+pxy(Left) else axeOz = Left end
        zdir = -(Bottom-Left); if pt3d.abs(pt3d.prod(zdir,Normal)) < 1e-8 then zdir = (Bottom-Right) end
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
