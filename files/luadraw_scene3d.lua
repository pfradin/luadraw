--- luadraw_scene3d.lua
-- date 2025/07/04
-- version 2.0
-- Copyright 2025 Patrick Fradin
-- This work may be distributed and/or modified under the
-- conditions of the LaTeX Project Public License.
-- The latest version of this license is in
--   http://www.latex-project.org/lppl.txt.


local Tscene3d = {}
Tscene3d.__index = Tscene3d

nbsplit = 0

--- Constructeur
function Tscene3d:new()
    local scene3d = {}
    setmetatable(scene3d, self)  -- obligatoire, permet d'utiliser self
    scene3d.type = "facet" -- "facet" ou "seg" ou "wall" ou "dot ou "text"    
    scene3d.data = nil --sommets de la facette ou du segment 
    scene3d.plane = nil -- plan de la facette
    scene3d.color = "white" -- couleur de la facette ou de la ligne
    scene3d.opacity = 1  -- opacité
    scene3d.coef = 1 -- éclairage pour facettes, épaisseur pour lignes, hauteur pour wall
    scene3d.dist = 0 -- pour les labels
    scene3d.dir = nil -- pour les labels
    scene3d.angle = 0 -- pour les labels
    scene3d.style = "solid" -- pour les lignes
    scene3d.dev = nil -- éléments de la scène situés devant
    scene3d.der = nil -- éléments de la scène situés derrière
    return scene3d
end

--------------- ajouter des cloisons
function Tscene3d:rightRot()
    local T = self
    local d = T.der
    T.der = d.dev
    d.dev = T
    T = d
    return T
end

function Tscene3d:leftRot()
    local T = self
    local d = T.dev
    T.dev = d.der
    d.der = T
    T = d
    return T
end

function equilibrer(T)
    local h1, h2 = Hight(T.dev), Hight(T.der)
    if h2 > h1+1 then -- penche à droite
        local h3, h4 = Hight(T.der.dev), Hight(T.der.der)
        if h3 > h4 then
            T.der = T.der:leftRot()
        end    
        T = T:rightRot()
    elseif h1 > h2+1 then -- penche à gauche
        local h3, h4 = Hight(T.dev.dev), Hight(T.dev.der)
        if h4 > h3 then
            T.dev = T.dev:rightRot()
        end
        T = T:leftRot()
    else
        T.hight = 1+math.max(Hight(T.dev), Hight(T.der))
    end 
    return T
end

function Tscene3d:Addsep(facet,plane) -- facette séparatrice (non dessinée)
-- il n'y a que des cloisons pour le moment
    local T = self
    if T.data == nil then
        T.data = facet
        T.plane = plane
        T.type = "wall"
        T.dev = nil
        T.der = nil
    else
        local A, u = table.unpack(plane)
        local B,v = table.unpack(T.plane)
        if (math.abs(pt3d.dot(B-A,u)) < 1e-10) and (pt3d.N1(pt3d.prod(u,v)) < 1e-10)
        then -- cloisons dans des plans confondus, la dernière arrivée est évincée
                --if T.dev == nil then T.dev = Tscene3d:new() end
                --T.dev:Addsep(facet,plane) -- {dev[1],plane[2]}
        else
            local dev, der = splitfacet(facet,T.plane)
            if #dev ~= 0 then
                if T.dev == nil then T.dev = Tscene3d:new() end
                T.dev:Addsep(dev,plane) -- {dev[1],plane[2]}
            end
            if #der ~= 0 then
                if T.der == nil then T.der = Tscene3d:new() end
                T.der:Addsep(der,plane)
            end
        end
    end
end

------------------ éléments dessinés


function Tscene3d:Addfacet(facet,plane,color,opacity) -- les sommets ont déjà été transformés,
-- il n'y a que des cloisons et des facettes pour le moment
    local T = self
    if T.data == nil then
        T.data = facet
        T.plane = plane
        T.color = color
        T.opacity = opacity
        T.type = "facet"
        T.dev = nil
        T.der = nil
    else
        local A, u = table.unpack(plane)
        local B,v = table.unpack(T.plane)
        if (math.abs(pt3d.dot(B-A,u)) < 1e-10) and (pt3d.N1(pt3d.prod(u,v)) < 1e-10)
        then -- facettes dans des plans confondus, la dernière arrivée est placée devant l'autre
                if T.dev == nil then T.dev = Tscene3d:new() end
                T.dev:Addfacet(facet,plane,color,opacity) -- {dev[1],plane[2]}
        else
            local dev, der = splitfacet(facet,T.plane)
            if (#dev ~= 0) and (#der ~= 0) then nbsplit = nbsplit +1 end            
            if #dev ~= 0 then
                if T.dev == nil then T.dev = Tscene3d:new() end
                T.dev:Addfacet(dev,plane,color,opacity) -- {dev[1],plane[2]}
            end
            if #der ~= 0 then
                if T.der == nil then T.der = Tscene3d:new() end
                T.der:Addfacet(der,plane,color,opacity)
            end
        end
    end
end


function Tscene3d:Addseg(seg,style,color,width,opacity,n) -- les sommets ont déjà été transformés,
-- le vecteur n est dirigé vers l'observateur
    local T = self
    if T.data == nil then
        T.data = seg
        T.color = color
        T.opacity = opacity
        T.coef = width
        T.style = style
        T.type = "seg"
        T.dev = nil
        T.der = nil
    elseif (T.type == "facet") or (T.type == "wall") then
        local dev, der = splitseg(seg,T.plane)
        if #dev ~= 0 then
            if T.dev == nil then T.dev = Tscene3d:new() end
            T.dev:Addseg(dev,style,color,width,opacity,n)
        end
        if #der ~= 0 then
            if T.der == nil then T.der = Tscene3d:new() end
            T.der:Addseg(der,style,color,width,opacity,n)
        end
    else -- on a un segment {A,A+u} déjà inséré, on veut insérer {C,C+v}
        local A, B = table.unpack(T.data)
        local u = B-A
        local dev, der = splitseg(seg,{A, pt3d.prod(u,pt3d.prod(n,u))})
        if #dev ~= 0 then
            if T.dev == nil then T.dev = Tscene3d:new() end
            T.dev:Addseg(dev,style,color,width,opacity,n)
        end
        if #der ~= 0 then
            if T.der == nil then T.der = Tscene3d:new() end
            T.der:Addseg(der,style,color,width,opacity,n)
        end
    end
end

function Tscene3d:Adddot(dot,style,color,scale,n)
-- dot = point 3d déjà transformé
-- le vecteur n est dirigé vers l'observateur
    local T = self
    if T.data == nil then
        T.data = dot
        T.color = color
        T.style = style
        T.coef = scale
        T.type = "dot"
        T.dev = nil
        T.der = nil
    elseif (T.type == "facet") or (T.type == "wall") then
        local coef = pt3d.dot(dot-T.plane[1], T.plane[2])
        if math.abs(coef) < 1e-8 then coef = 0 end
        if coef >= 0 then -- point devant
            if T.dev == nil then T.dev = Tscene3d:new() end
            T.dev:Adddot(dot,style,color,scale,n)
        else
            if T.der == nil then T.der = Tscene3d:new() end
            T.der:Adddot(dot,style,color,scale,n)
        end
    elseif T.type == "seg" then -- on a un segment {A,A+u} déjà inséré, on veut insérer le point dot
        local A, B = table.unpack(T.data)
        local u = B-A
        if math.abs(pt3d.det(A-dot,u,n)) < 1e-8 then -- le projeté de dot est sur le projeté de {A,B}
            local w = pt3d.prod(n,u)
            local beta = pt3d.det(dot-A,u,w) / pt3d.dot(w,w)
            if math.abs(beta) <= 1e-8 then beta = 0 end
            if beta >= 0 then
                if T.dev == nil then T.dev = Tscene3d:new() end
                T.dev:Adddot(dot,style,color,scale,n)
            else
                if T.der == nil then T.der = Tscene3d:new() end
                T.der:Adddot(dot,style,color,scale,n)
            end
        else -- devant par convention
            if T.dev == nil then T.dev = Tscene3d:new() end
            T.dev:Adddot(dot,style,color,scale,n)
        end
    else -- on a un point déjà inséré
        local beta = pt3d.dot(T.data-dot,n)
        if math.abs(beta) < 1e-8 then beta = 0 end
        if beta <= 0 then
            if T.dev == nil then T.dev = Tscene3d:new() end
            T.dev:Adddot(dot,style,color,scale,n)
        else
            if T.der == nil then T.der = Tscene3d:new() end
            T.der:Adddot(dot,style,color,scale,n)
        end
    end
end

function Tscene3d:Addlabel(text,dot,style,dist,color,size,angle,dir,showdot,n)
-- dot = point 3d déjà transformé
-- le vecteur n est dirigé vers l'observateur
    local T = self
    if T.data == nil then
        T.data = dot -- point d'ancrage pt3d
        T.color = color
        T.style = style
        T.coef = text
        T.dist = dist
        T.dir = dir
        T.angle = angle
        T.plane = showdot
        T.opacity = size
        T.type = "label"
        T.dev = nil
        T.der = nil
    elseif (T.type == "facet") or (T.type == "wall") then
        local coef = pt3d.dot(dot-T.plane[1], T.plane[2])
        if math.abs(coef) < 1e-8 then coef = 0 end
        if coef >= 0 then -- point devant
            if T.dev == nil then T.dev = Tscene3d:new() end
            T.dev:Addlabel(text,dot,style,dist,color,size,angle,dir,showdot,n)
        else
            if T.der == nil then T.der = Tscene3d:new() end
            T.der:Addlabel(text,dot,style,dist,color,size,angle,dir,showdot,n)
        end
    elseif T.type == "seg" then -- on a un segment {A,A+u} déjà inséré, on veut insérer le point dot
        local A, B = table.unpack(T.data)
        local u = B-A
        if math.abs(pt3d.det(A-dot,u,n)) < 1e-8 then -- le projeté de dot est sur le projeté de {A,B}
            local w = pt3d.prod(n,u)
            local beta = pt3d.det(dot-A,u,w) / pt3d.dot(w,w)
            if beta >= 0 then
                if T.dev == nil then T.dev = Tscene3d:new() end
                T.dev:Addlabel(text,dot,style,dist,color,size,angle,dir,showdot,n)
            else
                if T.der == nil then T.der = Tscene3d:new() end
                T.der:Addlabel(text,dot,style,dist,color,size,angle,dir,showdot,n)
            end
        else -- devant par convention
            if T.dev == nil then T.dev = Tscene3d:new() end
            T.dev:Addlabel(text,dot,style,dist,color,size,angle,dir,showdot,n)
        end
    else -- on a un point déjà inséré ou un label
        local beta = pt3d.dot(T.data-dot,n)
        if math.abs(beta) < 1e-8 then beta = 0 end
        if beta <= 0 then
            if T.dev == nil then T.dev = Tscene3d:new() end
            T.dev:Addlabel(text,dot,style,dist,color,size,angle,dir,showdot,n)
        else
            if T.der == nil then T.der = Tscene3d:new() end
            T.der:Addlabel(text,dot,style,dist,color,size,angle,dir,showdot,n)
        end
    end
end

function Tscene3d:Display(g) -- g est un graphe 3d
    if self.data == nil then return end
    if self.type == "facet" then
        local showfacet = function()
            if self.opacity == 1 then
                g:Lineoptions("solid",self.color,1) 
            else
                g:Lineoptions("noline",self.color,1); 
            end
            g:Filloptions("full",self.color,self.opacity); g:Lineopacity(self.opacity)
            g:Dpolyline3d(self.data,true)
        end
        if self.der ~= nil then self.der:Display(g) end
        showfacet()
        if self.dev ~= nil then self.dev:Display(g) end
    elseif self.type == "wall" then
        if self.der ~= nil then self.der:Display(g) end --la cloison n'est pas dessinée
        if self.dev ~= nil then self.dev:Display(g) end    
    elseif self.type == "seg" then -- segment
        if self.der ~= nil then self.der:Display(g) end
        g:Lineoptions(self.style,self.color,self.coef); g:Lineopacity(self.opacity); g:Filloptions("none")
        g:Linecap("round"); -- pour que les liaisons soient correctes entre segments successifs
        g:Dpolyline3d(self.data,false)
        if self.dev ~= nil then self.dev:Display(g) end
    elseif self.type == "dot" then -- point
        if self.der ~= nil then self.der:Display(g) end
        if self.style ~= "ball" then
            g:Lineoptions("solid",self.color,4); g:Lineopacity(1); g:Filloptions("full",self.color,1)
            g:Dotstyle(self.style); g:Dotscale(self.coef)
            g:Ddots3d(self.data)
        else
            g:Dballdots3d(self.data,self.color,self.coef)
        end
        if self.dev ~= nil then self.dev:Display(g) end
    elseif self.type == "label" then -- point
        if self.der ~= nil then self.der:Display(g) end
        g:Lineoptions("solid",self.color,4); g:Lineopacity(1); g:Filloptions("full",self.color,1)
        g:Labelsize(self.opacity); g:Labelstyle(self.style)
        g:Labelcolor(self.color); g:Labelangle(self.angle)
        if self.plane then g:Ddots3d(self.data) end --showdot=true
        if #self.dir ~= 0 then
            g:Dlabel3d(self.coef,self.data,{dist=self.dist, dir=self.dir})
        else
            g:Dlabel3d(self.coef,self.data,{dist=self.dist})
        end
        if self.dev ~= nil then self.dev:Display(g) end        
    end
end

function test(tree) -- teste l'équilibrage de l'arbre
    if tree == nil then return true
    else
        return test(tree.dev) and test(tree.der) and (math.abs(Hight(tree.dev)-Hight(tree.der))<2)
    end
end

function Hight(T)
    if T == nil then return 0
    else return T:haut()
    end
end
function Tscene3d:haut() -- calcule la hauteur de l'arbre
    if self.data == nil then return 0 end
    local h1, h2
    if self.dev == nil then h1 = 0 else h1 = self.dev:haut() end
    if self.der == nil then h2 = 0 else h2 = self.der:haut() end
    return 1 + math.max(h1,h2)
end

function Tscene3d:nb() -- calcule le nombre d'éléments dessinés
    local h1, h2
    if self.dev == nil then h1 = 0 else h1 = self.dev:nb() end
    if self.der == nil then h2 = 0 else h2 = self.der:nb() end
    if self.type == "wall" then return h1+h2
    else return 1+ h1+h2
    end
end

return Tscene3d
