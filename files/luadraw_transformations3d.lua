-- luadraw_transformations3d.lua (chargé par luadraw_graph3d.lua)
-- date 2025/07/04
-- version 2.0
-- Copyright 2025 Patrick Fradin
-- This work may be distributed and/or modified under the
-- conditions of the LaTeX Project Public License.
-- The latest version of this license is in
--   http://www.latex-project.org/lppl.txt.

function ftransform3d(L,f)
-- image de L par la fonction f 
-- f doit être une fonction de la variable complexe 
-- L est un polyèdre ou un point3d ou une facette (liste de point3d) ou une liste de facettes
    if (L == nil) then return end
    if L.vertices ~= nil then -- L est un polyèdre
        local res = {}
        res.facets =table.copy(L.facets)
        res.vertices = ftransform3d(L.vertices,f)
        return res
    end
    if isPoint3d(L) then return f(L) end
    local res, u = {}
    if (type(L) ~= "table") or (#L == 0) then return end
    if isPoint3d(L[1]) then -- liste de point3d
        for _,A in ipairs(L) do
            u = f(A)
            if u ~= nil then table.insert(res,u) end
        end
        if #res > 0 then return res end
    else -- L est une liste de listes
        for _, cp in ipairs(L) do
            local aux = {}
            for _, A in ipairs(cp) do
                u = f(A)
                if u ~= nil then table.insert(aux,u) end
            end
            if #aux > 0 then table.insert(res, aux) end
        end
        if #res > 0 then return res end
    end
end

function proj3d(L,P)
-- image de L par la projection orthogonale sur le plan P = {point3d, vecteur 3d normal}
-- L est un point3d ou une liste de points 3d ou une liste de listes de points 3d
    local B, u = P[1], P[2]
    if (B == nil) or (u == nil) or (pt3d.abs(u) == 0) then return end
    local D = pt3d.dot(u,u)
    
    local proj1 = function(A) -- image d'un seul point
    --  image d'un seul point
        local t = pt3d.dot(B-A,u) / D
        if t ~= nil then
            return A+t*u    
        end
    end
    
    return ftransform3d(L,proj1)
end

function pxy(L)
-- projection sur le plan xOy
-- L est un point3d ou une liste de points 3d ou une liste de listes de points 3d
    return proj3d(L,{Origin,vecK})
end 

function pyz(L)
-- projection sur le plan yOz
-- L est un point3d ou une liste de points 3d ou une liste de listes de points 3d
    return proj3d(L,{Origin,vecI})
end 

function pxz(L)
-- projection sur le plan xOz
-- L est un point3d ou une liste de points 3d ou une liste de listes de points 3d
    return proj3d(L,{Origin,vecJ})
end 


function proj3dO(L,P,v)
-- image de L par la projection parallèlement à v et sur le plan P = {point3d, vecteur 3d normal}
-- v est un vecteur 3d non nul
-- L est un point3d ou une liste de points 3d ou une liste de listes de points 3d
    local B, u = P[1], P[2]
    if (B == nil) or (u == nil) or (pt3d.abs(u) == 0) or (pt3d.abs(v) == 0) then return end
    local D = pt3d.dot(v,u)
    
    local projO1 = function(A) -- image d'un seul point
    --  image d'un seul point
        local t = pt3d.dot(B-A,u) / D
        if t ~= nil then
            return A+t*v    
        end
    end
    
    return ftransform3d(L,projO1)
end 

function dproj3d(L,d)
-- image de L par la projection orthogonale sur la droite d = {point3d, vecteur 3d directeur}
-- L est un point3d ou une liste de points 3d ou une liste de listes de points 3d
    local B, u = d[1], d[2]
    if (B == nil) or (u == nil) or (pt3d.abs(u) == 0) or (pt3d.abs(v) == 0) then return end
    local D = pt3d.dot(u,u)
    
   local dproj1 = function(A) -- image d'un seul point
    --  image d'un seul point
        local t = pt3d.dot(A-B,u) / D
        if t ~= nil then
            return B+t*u    
        end
    end
    
    return ftransform3d(L,dproj1)
end

function px(L)
-- projection sur l'axe Ox
-- L est un point3d ou une liste de points 3d ou une liste de listes de points 3d
    return dproj3d(L,{Origin,vecI})
end 

function py(L)
-- projection sur l'axe Oy
-- L est un point3d ou une liste de points 3d ou une liste de listes de points 3d
    return dproj3d(L,{Origin,vecJ})
    
end function pz(L)
-- projection sur l'axe Oz
-- L est un point3d ou une liste de points 3d ou une liste de listes de points 3d
    return dproj3d(L,{Origin,vecK})
end 

function sym3d(L,P)
-- image de L par la symétrie orthogonale par rapport au plan P = {point3d, vecteur 3d normal}
-- L est un point3d ou une liste de points 3d ou une liste de listes de points 3d
    local B, u = P[1], P[2]
    if (B == nil) or (u == nil) or (pt3d.abs(u) == 0) then return end
    local D = pt3d.dot(u,u)
    
    local sym1 = function(A) -- image d'un seul point
    --  image d'un seul point
        local t = pt3d.dot(B-A,u) / D
        if t ~= nil then
            return A+2*t*u    
        end
    end
    
    return ftransform3d(L,sym1)
end

function sym3dO(L,P,v)
-- image de L par la symétrie parallèlement à v et par rapport au plan P = {point3d, vecteur 3d normal}
-- v est un vecteur 3d non nul
-- L est un point3d ou une liste de points 3d ou une liste de listes de points 3d
    local B, u = P[1], P[2]
    if (B == nil) or (u == nil) or (pt3d.abs(u) == 0) or (pt3d.abs(v) == 0) then return end
    local D = pt3d.dot(v,u)
    
    local symO1 = function(A) -- image d'un seul point
    --  image d'un seul point
        local t = pt3d.dot(B-A,u) / D
        if t ~= nil then
            return A+2*t*v    
        end
    end
    
    return ftransform3d(L,symO1)
end 

function dsym3d(L,d)
-- image de L par la symétrie orthogonale par rapport à la droite d = {point3d, vecteur 3d directeur}
-- L est un point3d ou une liste de points 3d ou une liste de listes de points 3d
    local B, u = d[1], d[2]
    if (B == nil) or (u == nil) or (pt3d.abs(u) == 0) or (pt3d.abs(v) == 0) then return end
    local D = pt3d.dot(u,u)
    
   local dsym1 = function(A) -- image d'un seul point
    --  image d'un seul point
        local t = pt3d.dot(A-B,u) / D
        if t ~= nil then
            return 2*(B+t*u) - A -- B+t*u est le projeté de A sur d   
        end
    end
    
    return ftransform3d(L,dsym1)
end


function shift3d(L,u)
-- image de L par la translation de vecteur u
-- L est un point3d ou une liste de point3d ou une liste de listes de point3d
    if u == nil then return end
    
   local shift1 = function(A) -- image d'un point
        if (A == nil) then return end
        return A + u
    end
    
    return ftransform3d(L,shift1)
end

function rotate3d(L,angle,d)
-- image de L par la rotation d'angle angle (en degrés) et d'axe d={A,u} orienté par u
-- L est un point3d ou une liste de point3d ou une liste de listes de point3d
    local A, u = table.unpack(d)
    u = pt3d.normalize(u)
    angle = angle*deg
    local c1, s1 = math.cos(angle), math.sin(angle)
    
    local rot3d1 = function(M) -- image d'un point
        if (M == nil) then return end
        local x = M-A
        local w = pt3d.dot(x,u)*u
        return A + w + c1*(x-w) + s1*pt3d.prod(u,x)
    end
    
    return ftransform3d(L,rot3d1)
end

function scale3d(L,k,center)
-- image de L par l'homothétie de rapport k et de centre center (origine par défaut)
-- L est un point3d ou une liste de point3d ou une liste de listes de point3d
    center = center or Origin
    
    local scale3d1 = function(M) -- image d'un point
        if (M == nil) then return end
        local x = M-center
        return center + k*x
    end
    
    return ftransform3d(L,scale3d1)
end
