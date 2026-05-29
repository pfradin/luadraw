-- luadraw_transformations3d.lua (chargé par luadraw_graph3d.lua)
-- date 2026/05/29
-- version 3.1
-- Copyright 2026 Patrick Fradin
-- This work may be distributed and/or modified under the
-- conditions of the LaTeX Project Public License.
-- The latest version of this license is in
--   https://www.ctan.org/license/lppl

local ld = luadraw
local pt3d = ld.pt3d
local isPoint3d = pt3d.isPoint3d
local Origin, vecI, vecJ, vecK = pt3d.Origin, pt3d.vecI, pt3d.vecJ, pt3d.vecK
local M, Mc, Ms = pt3d.M, pt3d.Mc, pt3d.Ms

function ld.ftransform3d(L,f)
-- image de L par la fonction f 
-- f doit être une fonction de la variable complexe 
-- L est un polyèdre ou un point3d ou une facette (liste de point3d) ou une liste de facettes
    if (L == nil) then return end
    if L.vertices ~= nil then -- L est un polyèdre
        local res = {}
        res.facets =table.copy(L.facets)
        res.vertices = ld.ftransform3d(L.vertices,f)
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

function ld.proj3d(L,P)
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
    
    return ld.ftransform3d(L,proj1)
end

function ld.pxy(L,z0)
-- projection sur le plan z=z0
-- L est un point3d ou une liste de points 3d ou une liste de listes de points 3d
    z0 = z0 or 0
    return ld.proj3d(L,{M(0,0,z0),vecK})
end 

function ld.pyz(L,x0)
-- projection sur le plan x=x0
-- L est un point3d ou une liste de points 3d ou une liste de listes de points 3d
    x0 = x0 or 0
    return ld.proj3d(L,{M(x0,0,0),vecI})
end 

function ld.pxz(L,y0)
-- projection sur le plan y=y0
-- L est un point3d ou une liste de points 3d ou une liste de listes de points 3d
    y0 = y0 or 0
    return ld.proj3d(L,{M(0,y0,0),vecJ})
end 


function ld.proj3dO(L,P,v)
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
    
    return ld.ftransform3d(L,projO1)
end 

function ld.dproj3d(L,d)
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
    
    return ld.ftransform3d(L,dproj1)
end

function ld.px(L)
-- projection sur l'axe Ox
-- L est un point3d ou une liste de points 3d ou une liste de listes de points 3d
    return ld.dproj3d(L,{Origin,vecI})
end 

function ld.py(L)
-- projection sur l'axe Oy
-- L est un point3d ou une liste de points 3d ou une liste de listes de points 3d
    return ld.dproj3d(L,{Origin,vecJ})
end 

function ld.pz(L)
-- projection sur l'axe Oz
-- L est un point3d ou une liste de points 3d ou une liste de listes de points 3d
    return ld.dproj3d(L,{Origin,vecK})
end 

function ld.sym3d(L,P)
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
    
    return ld.ftransform3d(L,sym1)
end

function ld.sym3dO(L,P,v)
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
    
    return ld.ftransform3d(L,symO1)
end 

function ld.dsym3d(L,d)
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
    
    return ld.ftransform3d(L,dsym1)
end

function ld.psym3d(L,point)
-- image de L par la symétrie par rapport au point (point 3d)
-- L est un point3d ou une liste de points 3d ou une liste de listes de points 3d
    if (point == nil) or (L == nil) then return end
    
   local psym1 = function(A) -- image d'un seul point
    --  image d'un seul point
            return 2*point - A 
    end
    
    return ld.ftransform3d(L,psym1)
end


function ld.shift3d(L,u)
-- image de L par la translation de vecteur u
-- L est un point3d ou une liste de point3d ou une liste de listes de point3d
    if u == nil then return end
    
   local shift1 = function(A) -- image d'un point
        if (A == nil) then return end
        return A + u
    end
    
    return ld.ftransform3d(L,shift1)
end

function ld.rotate3d(L,angle,d)
-- image de L par la rotation d'angle angle (en degrés) et d'axe d={A,u} orienté par u
-- L est un point3d ou une liste de point3d ou une liste de listes de point3d
    local A, u = table.unpack(d)
    u = pt3d.normalize(u)
    angle = angle*ld.deg
    local c1, s1 = math.cos(angle), math.sin(angle)
    
    local rot3d1 = function(M) -- image d'un point
        if (M == nil) then return end
        local x = M-A
        local w = pt3d.dot(x,u)*u
        return A + w + c1*(x-w) + s1*pt3d.prod(u,x)
    end
    
    return ld.ftransform3d(L,rot3d1)
end

function ld.scale3d(L,k,center)
-- image de L par l'homothétie de rapport k et de centre center (origine par défaut)
-- L est un point3d ou une liste de point3d ou une liste de listes de point3d
    center = center or Origin
    
    local scale3d1 = function(M) -- image d'un point
        if (M == nil) then return end
        local x = M-center
        return center + k*x
    end
    
    return ld.ftransform3d(L,scale3d1)
end

function ld.rotateaxe3d(L,v1,v2,center)
-- L est un point3d ou une liste de point3d ou une liste de listes de point3d
-- on renvoie L transformé par une rotation de centre center et qui transforme v1 en v2 (normalisés)
    center = center or Origin
    if (v1 == nil) or (v2 == nil) or (v1 == v2) then return L end
    v1 = pt3d.normalize(v1); v2 = pt3d.normalize(v2)
    local theta = pt3d.angle3d(v1,v2) --en radians
    local u = pt3d.prod(v1,v2)
    if pt3d.N1(u) < 1e-12 then --u ==0
        u = pt3d.prod(v1,vecI)
        if pt3d.N1(u) < 1e-12 then --u ==0
            u = pt3d.prod(v1,vecJ)
        end
    end
    return ld.rotate3d(L,theta*ld.rad, {center,u})
end

function ld.inv3d(L, radius, center)
-- L est un point3d ou une liste de point3d ou une liste de listes de point3d
-- image de  L par l'inversion par rapport à la sphère de centre center et de rayon radius
    center = center or Origin
    if (radius == nil) or (type(radius) ~= "number") or (radius <= 0) then return end
    local r2 = radius^2
    local inv1 = function(A)
        if (A == nil) then return end
        if A == center then return
        else
            local B = A-center
            return center + r2/pt3d.abs2(B)*B
        end
    end    
    return ld.ftransform3d(L,inv1)
end    

function ld.projstereo(L,S,N,h)
-- L est un point3d ou une liste de point3d ou une liste de listes de point3d appartenant à la sphère S
-- S = {C,r} sphère de centre C et de rayon r
-- N point de la sphère qui sera le pôle de la projection
-- h définit  le plan P de la projection, plan normal au vecteur CN passant par I = C+h*CN/abs(CN)
    local C, r = table.unpack(S)
    local u = pt3d.normalize(N-C)
    N = C+r*u
    local I = C+h*u
    local projstereo1 = function(A)
        if (A == nil) then return end
        return ld.interDP({A,A-N},{I,u})
    end    
    return ld.ftransform3d(L,projstereo1)
end

function ld.inv_projstereo(L,S,N)
-- L est un point3d ou une liste de point3d ou une liste de listes de point3d appartenant à un plan orhogonal à CN
-- S = {C,r} sphère de centre C et de rayon r
-- N point de la sphère qui sera le pôle de la projection
    local C, r = table.unpack(S)
    local u = pt3d.normalize(N-C)
    N = C+r*u
    local inv_projstereo1 = function(A)
        if (A == nil) then return end
        local a,b = ld.interDS({A,N-A},S)
        if pt3d.abs(a-N) < 1e-10 then return b
        else return a 
        end
    end    
    return ld.ftransform3d(L,inv_projstereo1)
end
