-- luadraw_decorated.lua
-- date 2026/05/07
-- version 3.0
-- Copyright 2026 Patrick Fradin
-- This work may be distributed and/or modified under the
-- conditions of the LaTeX Project Public License.
-- The latest version of this license is in
--   https://www.ctan.org/license/lppl

-- to decorate 

-------------------------------- 2D decorations ------------------------
local ld = luadraw
local pt3d = ld.pt3d
local graph = ld.graph
local graph3d = ld.graph3d
local cpx = ld.cpx
local Z = cpx.Z

local addLabel = function(self, L, label, options, change_matrix)
    if (L == nil) or (type(L) ~= "table") then return end
    if (type(L[1]) == "table") and (not cpx.isComplex(L[1])) then L = L[1] end -- first component of  L
    change_matrix = change_matrix or false --self.matrix has already been applied on L
    local anchor = options.anchor
    local anchor2d = options.anchor2d or Z(0.5,0.5)
    local anchor1d = options.anchor1d -- number in [0;1] or nil
    if anchor == nil then
        if anchor1d ~= nil then 
            anchor = ld.getdot(anchor1d,L)
        else
        local x1,x2,y1,y2 = ld.getbounds(L)
        anchor = Z(x1,y1)+ Z(anchor2d.re*(x2-x1), anchor2d.im*(y2-y1))
        end
    else -- anchor not nil
        if (graph3d ~= nil) and pt3d.isPoint3d(anchor) then anchor = self:Proj3d(anchor)
        else
            anchor = cpx.toComplex(anchor)
        end
        if change_matrix then anchor = self:Mtransform(anchor) end
    end
    local pos = options.pos or "center"
    local dir = options.dir
    if (graph3d ~= nil) and (type(dir) == "table") and pt3d.isPoint3d(dir[1])  then dir = self:Proj3d(dir) end
    if (type(dir) == "number") or cpx.isComplex(dir) then dir = {dir} end
    if change_matrix then dir = self:MLtransform(dir) end
    local dist = options.dist or 0
    local node_options = options.node_options or ""
    if change_matrix then 
        self:Savematrix(); self:IDmatrix()
    end
    if options.showanchor then self:Ddots(anchor); node_options = node_options..",draw" end
    self:Dlabel(label,anchor,{pos=pos,dir=dir,dist=dist,node_options=node_options})
    if change_matrix then self:Restorematrix() end
end

-- 2D Dpolyline --
local oldDpolyline = graph.Dpolyline 
function graph:Dpolyline(L,closed,draw_options,clip) -- or Dpolyline(L,options) or Dpolyline(L,close,options)
-- options = {draw_options="", clip=false, close=false, label="", anchor1d=nil, anchor=Z(0.5,0.5), dir=nil, node_options=""}
    if (closed ~= nil) and (type(closed) ~= "boolean") then 
        clip = draw_options; draw_options = closed; closed = false end
    if type(draw_options) ~= "table" then
        oldDpolyline(self,L,closed,draw_options,clip)
        return
    end
    local options = draw_options or {}
    local close = options.close or closed
    local draw_options = options.draw_options or ""
    local clip = options.clip or nil
    local label = options.label or ""
    oldDpolyline(self,L,close,draw_options,clip)
    if label ~= "" then addLabel(self, L, label, options) end    
end

-- 2D path
function graph:Dpath(L,draw_options,clip) -- or g:Dpath(L,options,clip))
-- dessine le chemin contenu dans L, L est une table de complexes et d'instructions
-- ex: Dpath( {-1,2+i,3,"l", 4, "m", -2*i,-3-3*i,"l","cl",...} )
-- "m" pour moveto, "l" pour lineto, "b" pour bézier, "c" pour cercle, "ca" pour arc de cercle, "ea" arc d'ellipse, "e" pour ellipse, "s" pour spline naturelle, "cl" pour close
-- "la" pour line arc (ligne aux coins arrondis), "cla" ligne fermée aux coins arrondis
    clip = clip or false -- indique si  L est un chemin de clipping
    if (L == nil) or (type(L) ~= "table") or (#L < 3) then return end
    local options = draw_options or {}
    if type(draw_options) ~= "string" then draw_options = options.draw_options or "" end
    local label = options.label or ""
    local commande
    if clip then commande = "\\clip "
    else commande = self:drawcmd(draw_options,clip)
    end
    if commande == nil then return end
    local debut = true
    local res = {} -- résultat
    local crt = {} -- composante courante
    local aux = {} -- lecture en cours
    local last, first = nil, nil -- dernier lu et premier à venir
    local traiter
    local Mcoord = function(z) return self:Coord(z) end
    
    local Tcoord = function(z) -- applique la matrice courante à z
        return self:Coord(ld.applymatrix(z,self.matrix))
    end
    
    if not ld.isID(self.matrix) then Mcoord = Tcoord end -- transformation des points par la matrice courante
    
    local lineto = function() -- traitement du lineto
        -- on relie les points par une ligne
        if debut then self:Write(commande); debut = false end
        if first ~= nil then self:Write(" -- ") end
        self:Write(Mcoord(aux[1]))
        for k = 2, #aux do
            self:Write(" -- "..Mcoord(aux[k]))
        end
        first = last
        aux = {}
    end
    
    local moveto = function() -- traitement du moveto
    -- on démarre une nouvelle composante
            if not debut then self:Write(" ") end
            first = nil
            aux = {last}
    end
    
    local close = function() -- traitement du closepath
        -- en principe il y a eu une instruction avant autre que move, aux doit être vide et pas crt
        self:Write("--cycle")
        aux = {}
    end
    
    local Bezier = function()
        -- aux contient une ou plusieurs courbes de bézier
        local i
        if debut then self:Write(commande); debut = false end
        if first == nil then i = 1 else i = 2 end
        for _, z in ipairs(aux) do
            if i == 1 then self:Write(Mcoord(z)); i = 2
            else
                if i == 2 then self:Write(" .. controls "..Mcoord(z)); i = 3
                else
                    if i == 3 then self:Write(" and "..Mcoord(z)); i = 4
                    else    
                        if i == 4 then self:Write(" .. "..Mcoord(z)); i = 5 
                        else
                            if i == 5 then i = 2 end -- on est sur le caractère "b"
                        end
                    end
                end
            end
        end
        first = last
        aux = {}
    end
    
    local Spline = function ()
            if first ~= nil then 
            table.insert(aux,1,first)
        end
        aux = ld.spline(aux) -- spline naturelle
        if aux ~= nil then
            if first ~= nil then table.remove(aux,1) end -- le premier point est déjà exporté
            Bezier()
        end
        aux = {}
    end
    
    local Circle = function()
    -- il faut un point et le centre
        if first ~= nil then 
            table.insert(aux,1,first)
        end
        local a, c, r = aux[1], nil, nil
        if #aux == 2 then -- on a un point et le centre
            c = aux[2]; r = cpx.abs(a-c)
        else
            if #aux == 3 then -- on a trois points du cercle
                c = ld.interD(med(a,aux[2]), med(a,aux[3]))
                if c == nil then aux = {}; return end
                r = cpx.abs(a - c)
            else aux = {}; return
            end
        end
        aux = ld.arcb(a,c,a,r,1)
        if aux ~= nil then
            if first ~= nil then table.remove(aux,1) end -- le premier point est déjà exporté
            Bezier()
        end
        aux = {}
    end
    
    local Arc = function()
        local n = #aux
        if (n < 3) or (n > 5) then aux = {}; return end
        if first ~= nil then 
            table.insert(aux,1,first)
        end
        aux = ld.arcb(table.unpack(aux))
        if aux ~= nil then
            if first ~= nil then self:Write(" -- "); first = nil end -- pour relier le premier point de l'arc au précédent
            local newfirst = aux[#aux-1]
            Bezier()
            first = newfirst -- dernier point de l'arc
        end
        aux = {}
    end
    
    local Earc = function() -- ellipticarc(b,a,c,rx,ry,sens,inclin)
        local n = #aux
        if (n < 4) or (n > 7) then aux = {}; return end
        if first ~= nil then 
            table.insert(aux,1,first)
        end
        aux = ld.ellipticarcb(table.unpack(aux))
        if aux ~= nil then
            if first ~= nil then self:Write(" -- "); first = nil end -- pour relier le premier point de l'arc au précédent
            local newfirst = aux[#aux-1]
            Bezier()
            first = newfirst -- dernier point de l'arc
        end
        aux = {}
    end    
    
    local Ellipse = function() -- ellipse(p,c,rx,ry,sens,inclin)
        local n = #aux
        if (n < 3) or (n > 5) then aux = {}; return end
        if first ~= nil then 
            table.insert(aux,1,first)
        end
        local p, c, rx, ry, inclin = table.unpack(aux)
        aux = ld.ellipticarcb(p,c,p,rx,ry,1,inclin)
        if aux ~= nil then
            if first ~= nil then self:Write(" -- "); first = nil end -- pour relier le premier point de l'arc au précédent
            local newfirst = aux[#aux-1]
            Bezier()
            first = newfirst -- dernier point de l'arc
        end
        aux = {}
    end 
    local Rline = function(close) --on appelle roundline(L,r)
        local n = #aux
        if (n < 2) then aux = {}; return end
        if first ~= nil then 
            table.insert(aux,1,first)
        end
        local r = table.remove(aux)
        if (type(r) ~= "number") or (r <= 0) then aux = {}; return end
        local C = ld.roundline(aux,r,close,true)
        if C ~= nil then
            if first ~= nil then 
                if not close then table.remove(C,1) -- le premier point est déjà exporté
                end
            end 
            aux = {}
            for _,z in ipairs(C) do
                if (type(z) == "number") or cpx.isComplex(z) then table.insert(aux,z); last = z 
                else
                    if type(z) == "string" then traiter[z]() end
                end            
            end
        end
        aux = {}
    end
    
    local cRline = function()
        Rline(true)
    end
-- corps de la fonction dpath
    local clippee
    local aux2 = ld.path(L)
    if not ld.isID(self.matrix) then aux2 = self:Mtransform(aux2) end
    local X1,X2,Y1,Y2 = table.unpack(self.param.viewport)
    clippee = (not clip) and ld.needclip(aux2,X1,X2,Y1,Y2)
    if clippee and self.bbox then
        self:Writeln("\\begin{scope}")
        self:Writeln("\\clip "..self:strCoord(X1,Y1).." rectangle "..self:strCoord(X2,Y2)..";")
    end
    traiter = { ["s"]=Spline, ["l"]=lineto, ["m"]=moveto, ["cl"]=close, ["b"]=Bezier, ["c"]=Circle, ["ca"]=Arc, ["ea"]=Earc, ["e"]=Ellipse, ["la"]=Rline, ["cla"]=cRline } 
    for _, z in ipairs(L) do
        if (type(z) == "number") or cpx.isComplex(z) then table.insert(aux,z); last = z 
        else
            if type(z) == "string" then traiter[z]() end
        end
    end
    if not debut then self:Writeln(";") end
    if clippee and self.bbox then self:Writeln("\\end{scope}") end
    if label ~= "" then 
        addLabel(self, aux2, label, options, true) 
    end 
end

-- 2D Dseg --
function graph:Dseg(segm,scale,draw_options) -- ou Dseg({a,b}, options)
-- options = {draw_options="", scale=1, label="", anchor1d=nil, anchor=Z(0.5,0.5), dir=nil, node_options=""}
   if type(scale) ~= "number" then draw_options = scale; scale = 1 end
    if (segm == nil) or (type(segm) ~="table") or (#segm ~= 2) then return end
    local a, b = table.unpack(segm)
    a = cpx.toComplex(a)
    b = cpx.toComplex(b)
    scale = scale or 1
    if (a == nil) or (b == nil) then return end
    local u = b-a
    if (u.re == 0) and (u.im == 0) then return end
    local options = draw_options or {}
    if type(draw_options) ~= "string" then draw_options = options.draw_options or "" end
    local ticks = options.ticks or "none" -- or nb or {nb, length, space, draw_options}, to draws marks along the segment
    local sc = options.scale or scale
        if sc ~= 1 then
        a,b = table.unpack(ld.seg(a,b,sc))
    end
    self:Dpolyline({a,b},options)
    if type(ticks) == "number" then ticks = {ticks} end
    if type(ticks) == "table" then
        local n, length, space, d_options = table.unpack(ticks)
        self:Dmarkseg(a,b,n,length,space,d_options)
    end
end

-- 2D line --
function graph:Dline(d,B,draw_options) -- or g:Dline(d,B,options)
-- options = {draw_options="", label="", anchor1d=nil, anchor=Z(0.5,0.5), dir=nil, node_options=""}
    local A, u = nil, nil
    if (d == nil) then return end
    if (type(B) ~= "number") and (not cpx.isComplex(B)) then draw_options = B; B = nil end
    if (B ~= nil) then 
        B = cpx.toComplex(B)
        d = cpx.toComplex(d)
        if (B == nil) or (d == nil) then return end
        A = d
        u = B-A
    else
        if (type(d) ~= "table") or (#d ~= 2) then return end
        A = d[1]
        u = d[2]
    end
    A = cpx.toComplex(A) ; u = cpx.toComplex(u)
    if(A == nil) or (u == nil) or (u.re == 0) and (u.im == 0) then return end
    local options = draw_options or {}
    if type(draw_options) ~= "string" then draw_options = options.draw_options or "" end
    local label = options.label or ""
    if not ld.isID(self.matrix) then
        A, u = ld.applymatrix(A,self.matrix), ld.applyLmatrix(u,self.matrix)
    end
    local X1,X2,Y1,Y2 = table.unpack(self.param.viewport)
    local res = ld.clipline({A,u},X1,X2,Y1,Y2)
    if res ~= nil then
        local commande = self:drawcmd(draw_options)
        if cpx.dot(u,res[2]-res[1]) > 0 then -- le sens est important s'il y a une flèche
            self:Write(commande..self:Coord(res[1]))
            self:Writeln(" -- "..self:Coord(res[2])..";")
        else
            self:Write(commande..self:Coord(res[2]))
            self:Writeln(" -- "..self:Coord(res[1])..";")
        end
        if label ~= "" then 
            addLabel(self, res, label, options, true) 
        end    
    end
end

-- 2D Dbezier
function graph:Dbezier(L,draw_options) -- où L = {A1,c1,c2,A2,c3,c4,A3,...}
-- dessine une série de courbes de Bézier passant par A1, A1,... et ayant comme points de contrôle c1 et c2, puis c3,c4, ...

    if (L == nil) or (type(L) ~= "table") or (#L < 3) then return end
    local options = draw_options or {}
    if type(draw_options) ~= "string" then draw_options = options.draw_options or "" end
    local label = options.label or ""
    local a, c1, c2, b
    local i = 1
    if not ld.isID(self.matrix) then
        L = self:Mtransform(L) -- image des points de L par la matrice de transformation courante
        if L == nil then return end
    end
    local first = true
    for k, x in ipairs(L)  do
        if i == 1 then a = x; i = 2
        else
            if i == 2 then 
                c1 = x; i = 3
            else
                if i == 3 then
                    c2 = x; i = 4
                else -- i vaut 4
                    b = x
                    -- tracé
                    if first then
                        local commande = self:drawcmd(draw_options)  -- commande draw avec les options
                        self:Write(commande..self:Coord(a))
                        first = false
                    end
                    self:Write(" .. controls "..self:Coord(c1).." and "..self:Coord(c2).." .. "..self:Coord(b))
                    i = 2
                end
            end
        end
    end
    if not first then self:Writeln(";") end
    if label ~= "" then 
        local C = {L[1],L[2],L[3],L[4],"b"}
        for  k = 5,#L, 3 do
            insert(C, {L[k],L[k+1],L[k+2],"b"})
        end
        addLabel(self, ld.path(C), label, options, true) 
    end 
end


--- arc 2D --
local oldDarc = graph.Darc
function graph:Darc(B,A,C,r,sens,options)
    if (options == nil) or (type(options) == "string") then 
        oldDarc(self,B,A,C,r,sens,options)
        return
    end
    options = options or {}
    local sector_options = options.sector_options or "" -- fill or not the angular sector
    local arc_options = options.arc_options or "" -- draw_options for the arc
    local label = options.label or "" -- label text
    local node_options = options.node_options or "" -- node_options for the label
    local pos = options.pos or "auto" -- position of the label relative to the anchor point
    local dist = options.dist or r -- distance between the label and the center A
    local rotate = options.rotate or "none" -- or "auto" or "ortho", rotation of the label
    local angle = options.angle or 0 -- angle to turn the anchor point around the center A (initially, the anchor point is located on the arc and the bisector line)
    local ticks = options.ticks or "none" -- or nb or {nb, length, space, draw_options}, to draws marks along the arc (acute angle)
    local showanchor = options.showanchor or false -- show the anchor point, the bisector line and the frame of the label
    
    local S = ld.arcb(B,A,C,r,sens) -- arc with Bézier curves
    if S == nil then return end
    local dir = {}
    if sector_options ~= "" then
        self:Dpath(ld.concat(S,{A,"l","cl"}),"draw=none,"..sector_options)
    end
    self:Dpath(S,arc_options)
    if label ~= "" then
        local b, c = A+r*cpx.normalize(B-A), A+r*cpx.normalize(C-A)
        local u = cpx.normalize((c+b)/2-A)
        local direct = 1
        if cpx.det(B-A,C-A) <= 0 then direct = -1 end
        u = direct*sens*u
        local anchor = A+dist*u
        if angle ~=0 then anchor = ld.rotate(anchor,angle,A) end
        if rotate == "ortho" then 
            if u.im < 0 then u = -u end
            dir = {-cpx.I*u, u} 
        elseif rotate == "auto" then
            if u.re < 0 then u = -u end
            dir = {u, cpx.I*u} 
        end
        if pos == "auto" then 
            if rotate == "ortho" then 
                pos = self:Poslab(anchor-A, self:Arg(-cpx.I*u)*ld.rad) 
            elseif rotate == "auto" then
                pos = self:Poslab(anchor-A, self:Arg(u)*ld.rad) 
            else pos = self:Poslab(anchor-A,0)
            end 
        end
        if showanchor then
            self:Dseg({A,anchor},"dashed"); self:Ddots(anchor)
            self:Dlabel(label, anchor, {pos=pos, node_options=node_options..",draw", dir=dir})
        else
            self:Dlabel(label, anchor, {pos=pos, node_options=node_options, dir=dir})
        end
    end
    if type(ticks) == "number" then ticks = {ticks} end
    if type(ticks) == "table" then
        local n, length, space, d_options = table.unpack(ticks)
        self:Dmarkarc(B,A,C,r,n,length,space,d_options)
    end
end

graph.Ddecoratedarc = graph.Darc


-------------------------------- 3D decorations ------------------------
if graph3d ~= nil then -- if graph3d has been loaded
---------------------- arc 3D ------------------------------------------
local oldDarc3d = graph3d.Darc3d
function graph3d:Darc3d(B,A,C,r,sens,normal,draw_options,clip) -- or Darc3d(B,A,C,r,sens,options)
    if (normal == nil) or pt3d.isPoint3d(normal) or (type(normal) ~= "table") then 
        oldDarc3d(self,B,A,C,r,sens,normal,draw_options,clip)
        return
    end
    options = normal or {}
    local u, v = B-A, C-A
    local n = pt3d.prod(B-A,C-A)
    if pt3d.N1(n) < 1e-8 then -- n is null
        n = options.normal
        if n == nil then 
            print("Darc3d options : normal is missing")
            return
        end
    end
    local sector_options = options.sector_options or "" -- fill or not the angular sector
    local arc_options = options.arc_options or "" -- draw_options for the arc
    local label = options.label or "" -- label text
    local node_options = options.node_options or "" -- node options for the label
    local pos = options.pos or "auto" -- position of the label relative to the anchor point
    local dist = options.dist or r -- distance between the label and the center A
    local rotate3D = options.rotate3d or "none" -- or "auto" or "ortho", rotation of the label in (BAC) plane
    local rotate = options.rotate or "none" -- or "auto" or "ortho", rotation of the label on the screen plane
    local angle = options.angle or 0 -- angle to turn the anchor point around the center A (initially, the anchor point is located on the arc and the bisector line)
    local ticks = options.ticks or "none" -- or {nb, length, space, draw_options}, to draws marks along the arc (acute angle)
    local showanchor = options.showanchor or false -- show the anchor point, the bisector line and frame of the label
    local bezier = options.bezier -- use bezier curves for the arc (or polyline if bezier=false)
    if bezier == nil then bezier = true end
    local clip = options.clip or false
    local S = ld.arc3db(B,A,C,r,sens,n) -- arc with Bézier curves
    if S == nil then return end
    local dir = {}
    if sector_options ~= "" then
        self:Dpath3d(ld.concat(S,{A,"l","cl"}),"draw=none,"..sector_options)
    end
    if bezier and (not clip)then self:Dpath3d(S,arc_options)
    else self:Dpolyline3d(ld.path3d(S),false,arc_options,clip)
    end
    if label ~= "" then
        local b, c = A+r*pt3d.normalize(B-A), A+r*pt3d.normalize(C-A)
        local u = pt3d.normalize((c+b)/2-A)
        u = sens*u
        local v = ld.rotate3d(u,90,{pt3d.Origin,n})
        local w = ld.rotate3d(u,-90,{pt3d.Origin,n})
        local anchor = A+dist*u
        if angle ~= 0 then anchor = ld.rotate3d(anchor,angle,{A,n}) end
        local U, V = self:Proj3dV(anchor-A), self:Proj3dV(w)
        local angle2D = 0
        if rotate3D == "ortho" then 
            if self:Cosine_incidence(n,anchor) < 0 then w = -w; V = -V end
            if V.re < 0 then dir = {-w, -u}; V = -V else dir = {w,u} end
        elseif rotate3D == "auto" then
            if self:Cosine_incidence(n,anchor) < 0 then v = -v end
            if self:Proj3dV(u).re < 0 then dir = {-u,-v}; u = -u else  dir = {u,v} end
        elseif rotate == "ortho" then
            angle2D = self:Arg(V)*ld.rad
            if angle2D < -90 then angle2D = angle2D+180
            elseif angle2D > 90 then angle2D = angle2D-180
            end
        elseif rotate == "auto" then
            angle2D = self:Arg(U)*ld.rad
            if angle2D < -90 then angle2D = angle2D+180
            elseif angle2D > 90 then angle2D = angle2D-180
            end
        end
        if pos == "auto" then 
            if rotate3D == "ortho" then 
                pos = self:Poslab(U, self:Arg(V)*ld.rad) 
            elseif rotate3D == "auto" then
                pos = self:Poslab(U, self:Arg(self:Proj3dV(u))*ld.rad) 
            else pos = self:Poslab(U,angle2D)
            end 
        end
        if angle2D ~= 0 then node_options = node_options..",rotate="..angle2D end
        if showanchor then
            self:Dseg3d({A,anchor},"dashed"); self:Ddots3d(anchor) 
            self:Dlabel3d(label, anchor, {pos=pos, node_options=node_options..",draw", dir=dir})
         else
            self:Dlabel3d(label, anchor, {pos=pos, node_options=node_options, dir=dir})
         end
    end
    if type(ticks) == "number" then ticks = {ticks} end
    if type(ticks) == "table" then
        local nb, length, space, d_options = table.unpack(ticks)
        local u, w = pt3d.normalize(B-A), C-A
        local v = pt3d.prod(n,u)
        local mat = { self:Proj3d(A), self:Proj3dV(u), self:Proj3dV(v) }
        self:Savematrix()
        self:Composematrix(mat)
        local a, b, c = 0, 1, Z( pt3d.dot(w,u), pt3d.dot(w,v) )
        self:Dmarkarc(b,a,c,r,nb,length,space,d_options)
        self:Restorematrix()
    end
end
graph3d.Ddecoratedarc3d = graph3d.Darc3d

end
