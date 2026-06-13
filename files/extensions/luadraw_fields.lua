-- luadraw_fields2d.lua
-- date 2026/06/13
-- version 3.2
-- Copyright 2026 Patrick Fradin
-- This work may be distributed and/or modified under the
-- conditions of the LaTeX Project Public License.
-- The latest version of this license is in
--   https://www.ctan.org/license/lppl

-- to draw vector fields or gradient fields

local ld = luadraw
local graph = ld.graph
local cpx = ld.cpx
local Z = cpx.Z

function ld.field(f,x1,x2,y1,y2,grid,long)
-- champ de vecteurs dans le pavé [x1,x2]x[y1,y2]
-- f fonction de deux variables à valeurs dans R^2
-- grid = {nbx, nby} : nombre de vecteurs suivant x et suivant y
-- long = longueur d'un vecteur
    if grid == nil then grid = {25,25} end
    local deltax, deltay = (x2-x1)/(grid[1]-1), (y2-y1)/(grid[2]-1) -- pas suivant x et y
    if long == nil then long = math.min(deltax,deltay) end -- longueur par défaut
    local vectors = {} -- contiendra la liste des vecteurs
    local x, y, v = x1 
    for _ = 1, grid[1] do -- parcours suivant x
        y = y1
        for _ = 1, grid[2] do -- parcours suivant y
            v = f(x,y) -- on suppose que v est bien défini
            v = Z(v[1],v[2]) -- passage en complexe
            v = cpx.normalize(v)
            if v ~= nil then
                table.insert(vectors, {Z(x,y)-long/2*v, Z(x,y)+long/2*v} ) -- on ajoute le vecteur
            end
            y = y+deltay
        end
        x = x+deltax
    end
    return vectors -- on renvoie le résultat (ligne polygonale)
end

function graph:Dvectorfield(f,args)
-- dessine un champ de vecteurs
-- f fonction de deux variables à valeurs dans R^2
-- args table à 4 champs :
-- { view={x1,x2,y1,y2}, grid={nbx,nby}, length=, draw_options=""}
    args = args or {}
    local view = args.view or {self:Xinf(),self:Xsup(),self:Yinf(),self:Ysup()} -- repère utilisateur par défaut
    local vectors = ld.field(f,view[1],view[2],view[3],view[4],args.grid,args.length) -- calcul du champ
    self:Dpolyline(vectors,false,args.draw_options,view) -- le dessin (ligne polygonale non fermée)
end

function graph:Dgradientfield(f,args)
-- dessine un champ de gradient
-- f fonction de deux variables à valeurs dans R
-- args table à 4 champs :
-- { view={x1,x2,y1,y2}, grid={nbx,nby}, length=, draw_options=""}
    local h = 1e-6
    local grad_f = function(x,y) -- fonction gradient de f
        return { (f(x+h,y)-f(x-h,y))/(2*h), (f(x,y+h)-f(x,y-h))/(2*h) }
    end
    self:Dvectorfield(grad_f,args) -- on utilise la méthode précédente
end


function graph:DplotXY(X,Y,draw_options)
-- X est une liste de réels ou de chaînes
-- Y est une liste de réels de même longueur que X 
    local L = {} -- liste des points à dessiner
    if type(X[1]) == "number" then 
        for k,x in ipairs(X) do
            table.insert(L,Z(x,Y[k]))
        end
    else
        local noms = {} -- liste des labels à placer
        for k = 1, #X do
            table.insert(L,Z(k,Y[k]))
            ld.insert(noms,{X[k],k,{pos="E",node_options="rotate=-90"}})
        end
        self:Dlabel(table.unpack(noms))
    end
    self:Dpolyline(L,draw_options)
end

-------------------------- 3D fields on a surface ---------------------
if ld.graph3d ~= nil then -- luadraw with option 3d 
    local pt3d = ld.pt3d
    
    local vectorAt = function(p,f,u,v,length,arrow)
    -- p = surface parameterization p:(u,v) -> p(u,v) in R^3 (not necessary cartesian)
    -- f:(u,v) -> (f1(u,v), f2(u,v) in R^2 (field)
    -- arrow = false/true
        arrow = arrow or false
        local h = 1e-6
        local P = p(u, v)
        local A = f(u, v)
        local dpu = (p(u+h, v) - p(u-h, v))/(2*h)
        local dpv = (p(u, v+h) - p(u, v-h))/(2*h)
        local V = pt3d.normalize(A[1]*dpu + A[2]*dpv)
        if V ~= nil then
            if arrow  then
                local N = pt3d.normalize( pt3d.prod(V,pt3d.prod(dpu,dpv)) )
                local Q = P+length/4*V
                local R, S = Q+length/6*N-length/8*V, Q-length/6*N-length/8*V
                return {P-length/2*V, Q, R, P+length/2*V, S, Q} 
            else 
                return {P-length/2*V, P+length/2*V} 
            end
        end
    end
    
    function ld.surfacefield(p, f, u1, u2, v1, v2, grid, length,arrows)
    -- p = surface parameterization p:(u,v) -> p(u,v) in R^3 (not necessary cartesian)
    -- f:(u,v) -> (f1(u,v), f2(u,v) in R^2 (field)
    -- grid ={nbu,nbv}
    -- length of segments
    -- returns list of vectors
        grid = grid or {25,25}
        local deltau, deltav = (u2-u1)/(grid[1]-1), (v2-v1)/(grid[2]-1)
        length = length or math.min(deltau,deltav)
        local h = 1e-6
        local vectors = {}
        local u, v, V = u1+deltau/2 -- variables must be declared to be local
        for i = 1, grid[1]-1 do
            v = v1 + deltav/2 --To prevent vectors from originating outside the surface
            for j = 1, grid[2]-1 do
                V = vectorAt(p,f,u,v,length,arrows)
                if V ~= nil then 
                    table.insert(vectors,V) 
                end
                v = v + deltav
            end
            u = u + deltau
        end
        return vectors
    end
    
    local surfacefield2 = function(p,f,u1,u2,v1,v2,grid,length,arrows)
    -- p = surface parameterization p:(u,v) -> p(u,v) in R^3 (not necessary cartesian)
    -- f:(u,v) -> (f1(u,v), f2(u,v) in R^2 (field)
    -- grid ={nbu,nbv}
    -- length of segments
    -- returns the sequence: facets of the surface (with vector index), list of vectors
        local F = function(u,v)
            local R = ld.evalf(p,u,v) -- protected evaluation
            if (R == nil) then return cpx.Jump
            else return R
            end
        end
        local different = function(A,B)
            return pt3d.N1(B-A)>1e-10
        end
        
        local umesh, vmesh, nbu, nbv
        grid = grid or {25,25}
        arrow = arrow or false
        nbu, nbv = table.unpack(grid)
        umesh, vmesh = ld.linspace(u1,u2,nbu), ld.linspace(v1,v2,nbv)
        length = length or 0.25
        local S = {}
        for _,u in ipairs(umesh) do
            local aux = {}
            for _,v in ipairs(vmesh) do
                table.insert(aux,F(u,v))
            end
            table.insert(S,aux)
        end
        local rep, vectors = {}, {}
        local A, last, cij
        for i = 1, nbu-1 do
            for j = 1, nbv-1 do
                aux = {}; cij = {(umesh[i]+umesh[i+1])/2, (vmesh[j]+vmesh[j+1])/2}
                A = S[i][j]; first = A; last = A
                if A ~= cpx.Jump then table.insert(aux,A) end
                A = S[i+1][j]
                if (A ~= cpx.Jump) and ((last == cpx.Jump) or different(A,last)) then table.insert(aux,A); last = A end
                A = S[i+1][j+1]
                if (A ~= cpx.Jump) and ((last == cpx.Jump) or different(A,last)) then table.insert(aux,A); last = A end
                A = S[i][j+1]
                if (A ~= cpx.Jump) and ((last == cpx.Jump) or different(A,last)) and ((first == cpx.Jump) or different(A,first)) then table.insert(aux,A) end
                if #aux > 2 then 
                    local V = vectorAt(p,f,cij[1],cij[2],length,arrows)
                    if V == nil then table.insert(vectors,"nil") else table.insert(vectors,V) end
                    table.insert(rep, {aux, #vectors}) 
                end
            end
        end
        return rep, vectors
    end
      
    
    function ld.graph3d:Dsurfacefield(p, f, args)
    -- p = surface parameterization p:(u,v) -> p(u,v) in R^3 (not necessary cartesian)
    -- f:(u,v) -> (f1(u,v), f2(u,v) in R^2 (field)
    -- args = { domain={u1,u2,v1,v2}, grid = {25,25}, length= , arrows="auto" or else, width = , color="black", clip =false, field_options="", surface_options=nil } 
        args = args or {}
        local domain = args.domain or self.param.viewport3d
        local u1, u2,v1,v2 = table.unpack(domain)
        local clip = args.clip or false
        local arrows = args.arrows or "auto"
        local addarrows = (arrows == "auto")
        if arrows == "auto" then arrows = "-" end
        local arrowcolor = args.color or self.paramam.linecolor
        local width = args.width or self.param.linewidth
        local field_options = args.field_options or ""
        local s_options = args.surface_options
        local oldlinejoin = self.param.linejoin
        local oldlinecolor = self.param.linecolor
        local oldlinewidth = self.param.linewidth 
        local oldfillstyle = self.param.fillstyle
        local oldfillopacity = self.param.fillopacity
        local oldfillcolor = self.param.fillcolor
        local oldlinestyle = self.param.linestyle
        local oldlineopacity = self.param.lineopacity 
        local oldarrows = self.param.arrows
        self:Linejoin("miter")
        if s_options == nil then -- only vectors
            local vectors = ld.surfacefield(p, f, u1, u2, v1, v2, args.grid, args.length,addarrows)
            self:Lineoptions("solid",arrowcolor,width,arrows); self:Filloptions("full",arrowcolor,1)
            self:Dpolyline3d(self:Sortfacet(vectors),false,field_options,clip)
            self:Lineoptions(oldlinestyle,oldlinecolor,oldlinewidth,oldarrows)
            self:Filloptions(oldfillstyle,oldfillcolor,oldfillopacity)
            self:Linejoin(oldlinejoin)
            return
        end
        local usepalette = s_options.usepalette or nil
        local color = s_options.color or "white"
        local backcull = s_options.backcull or false
        local contrast = s_options.contrast or 1
        local opacity = s_options.opacity or 1
        local mode = s_options.mode or 3
        local twoside = s_options.twoside
        if twoside == nil then twoside = true end
        local edgestyle = s_options.edgestyle or self.param.linestyle
        local edgecolor = s_options.edgecolor or self.param.linecolor
        local edgewidth = s_options.edgewidth or self.param.edgewidth
               
        local surf, vectors = surfacefield2(p,f,domain[1],domain[2],domain[3],domain[4],args.grid,args.length,addarrows)
        if not ld.isID3d(self.matrix3d) then vectors = self:Mtransform3d(vectors) end
        local facets = {}
        for _,F in ipairs(surf) do
            local face, index = table.copy(F[1]), F[2]
            if clip then face = ld.clip3d(face,self:Box3d())[1] end
            if (face ~= nil) and (not ld.isID3d(self.matrix3d)) then face = self:Mtransform3d(face) end
            if (face ~= nil) and (#face>2) then table.insert(face,index); table.insert(facets,face) end
        end
        local oldmatrix = self.matrix3d
        self.matrix3d = ld.ID3d
        local getcolor
        if usepalette == nil then
            getcolor = function(f)
                return color
            end
        else
            local pal, mode = table.unpack(usepalette)
            local F = ld.map(function(L) return L[1] end, surf)
            getcolor = ld.define_getcolor(F,pal,mode,color)
        end  
        facets = self:Sortfacet(facets, backcull)
        
        local draw_vector = function(index)
            self:Lineoptions("solid",arrowcolor,width,arrows); self:Filloptions("full",arrowcolor,1)
            self:Lineopacity(1)
            self:Dpolyline3d(vectors[index],false,field_options) 
        end
        
        for _, face in ipairs(facets) do
            local index = table.remove(face)
            local coul = self:Adjust_color(face,getcolor(face),contrast,twoside)
            local post = self:Isvisible(face)
            if coul ~= nil then
                if not post then draw_vector(index) end-- drawing the corresponding vector
                self:Filloptions("full",coul,opacity)
                if (mode == ld.mShadedOnly) then 
                    if (opacity == 1) then
                        self:Lineoptions("solid",coul,1,"-"); self:Lineopacity(opacity)
                        self:Dpolyline3d(face,true)
                    elseif opacity > 0 then
                        self:Linestyle("noline")                        
                        self:Dpolyline3d(face,true)
                    end
                else
                    if mode == ld.mWireframe then self:Filloptions("none") end
                    if (opacity > 0) or (mode == ld.mWireframe) then 
                        self:Lineoptions(edgestyle,edgecolor,edgewidth,"-")
                        self:Dpolyline3d(face,true) 
                    end
                end
                if post then draw_vector(index) end
            end
        end
        self:Filloptions(oldfillstyle,oldfillcolor,oldfillopacity)
        self:Lineoptions(oldlinestyle,oldlinecolor,oldlinewidth,oldarrows); self:Lineopacity(oldlineopacity)
        self:Linejoin(oldlinejoin)
        self.matrix3d = oldmatrix
    end        
end    
