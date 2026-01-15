-- luadraw_transformations.lua (chargé par luadraw_calc.lua)
-- date 2026/01/15
-- version 2.5
-- Copyright 2026 Patrick Fradin
-- This work may be distributed and/or modified under the
-- conditions of the LaTeX Project Public License.
-- The latest version of this license is in
--   http://www.latex-project.org/lppl.txt.

function ftransform(L,f)
-- image de L par la fonction f (l'image du complexe cpx.Jump est lui-même)
-- f doit être une fonction de la variable complexe 
-- L est un complexe ou une liste de complexes ou une liste de listes de complexes
    if (L == nil) then return end
    if (type(L) == "number") or isComplex(L) then
        if z == cpx.Jump then return cpx.Jump else return f(L) end
    end
    local res, u = {}
    if (type(L) ~= "table") or (#L == 0) then return end
    if (type(L[1]) == "number") or isComplex(L[1]) then -- liste de réels/complexes
        for _,z in ipairs(L) do
            if z == cpx.Jump then u = cpx.Jump else u = f(z) end
            if u ~= nil then table.insert(res,u) end
        end
        if #res > 0 then return res end
    else -- L est une liste de listes
        for _, cp in ipairs(L) do
            local aux = {}
            for _, z in ipairs(cp) do
                if z == cpx.Jump then u = cpx.Jump else u = f(z) end
                if u ~= nil then table.insert(aux,u) end
            end
            if #aux > 0 then table.insert(res, aux) end
        end
        if #res > 0 then return res end
    end
end    


function proj(L,d)
-- image de L par la projection orthogonale sur la droite d = {point, vecteur directeur}
-- L est un complexe ou une liste de complexes ou une liste de listes de complexes
    if (d == nil) or (type(d) ~= "table") or (#d ~= 2) then return end
    local B, u = d[1], d[2]
    B = toComplex(B); u = toComplex(u)
    if (B ==nil) or (u == nil) or ((u.re == 0) and (u.im == 0)) then return end
    local D = cpx.dot(u,u)
    
    local proj1 = function(A) -- image d'un seul point
    --  image d'un seul point
        A = toComplex(A)
        if (A == nil) then return end
        local t = cpx.dot(A-B,u) / D
        if t ~= nil then
            local C = B+t*u    
            if notDef(C.re) or notDef(C.im) then return
            else return C
            end
        end
    end
    
    return ftransform(L,proj1)
end     

function projO(L,d,v)
-- image de L par la projection oblique sur la droite d = {point, vecteur directeur} parallèlement au vecteur v
-- L est un complexe ou une liste de complexes ou une liste de listes de complexes
    if (d == nil) or (type(d) ~= "table") or (#d ~= 2) then return end
    local B, u = d[1], d[2]
    B = toComplex(B); u = toComplex(u); v = toComplex(v)
    if (B ==nil) or (u == nil) or (v == nil) or ((u.re == 0) and (u.im == 0)) or ((v.re == 0) and (v.im == 0)) then return end
    local D = cpx.det(u,v)
    
    local projO1 = function (A)  -- image  d'un point
        A = toComplex(A)
        if (A == nil) then return end
        local t = cpx.det(A-B,v) / D
        if t ~= nil then
            return B+t*u
        end
    end
    
    return ftransform(L,projO1)
end

function sym(L,d)
-- image de L par la symétrie orthogonale d'axe la droite d = {point, vecteur directeur}
-- L est un complexe ou une liste de complexes ou une liste de listes de complexes

    if (d == nil) or (type(d) ~= "table") or (#d ~= 2) then return end
    local B, u = d[1], d[2]
    B = toComplex(B); u = toComplex(u)
    if (B ==nil) or (u == nil) or ((u.re == 0) and (u.im == 0)) then return end
    local D = cpx.dot(u,u)
    
    local sym1 = function(A) -- image d'un seul point
        A = toComplex(A)
        if (A == nil) then return end
        local t = cpx.dot(A-B,u) / D
        if t ~= nil then
            local C = B+t*u  -- projeté de A sur d   
            if notDef(C.re) or notDef(C.im) then return
            else return 2*C-A
            end
        end
    end
    
    return ftransform(L,sym1)
end

function symO(L,d,v)
-- image de L par la symétrie oblique d'axe la droite d = {point, vecteur directeur} parallèlement au vecteur v
-- L est un complexe ou une liste de complexes ou une liste de listes de complexes
    if (d == nil) or (type(d) ~= "table") or (#d ~= 2) then return end
    local B, u = d[1], d[2]
    B = toComplex(B); u = toComplex(u); v = toComplex(v)
    if (B ==nil) or (u == nil) or (v == nil) or ((u.re == 0) and (u.im == 0)) or ((v.re == 0) and (v.im == 0)) then return end
    local D = cpx.det(u,v)
    
    local symO1 = function (A)  -- image  d'un point
        A = toComplex(A)
        if (A == nil) then return end
        local t = cpx.det(A-B,v) / D
        if t ~= nil then
            return 2*(B+t*u)-A
        end
    end
    
    return ftransform(L,symO1)
end

function symG(L,d,v)
-- image de L par la symétrie glissée d'axe la droite d = {point, vecteur directeur} et de vecteur v
-- L est un complexe ou une liste de complexes ou une liste de listes de complexes
    if (d == nil) or (type(d) ~= "table") or (#d ~= 2) then return end
    local B, u = d[1], d[2]
    B = toComplex(B); u = toComplex(u); v = toComplex(v)
    if (B ==nil) or (u == nil) or (v == nil) or ((u.re == 0) and (u.im == 0)) or ((v.re == 0) and (v.im == 0)) then return end
    local D = cpx.dot(u,u)
    
    local symG1 = function(A)
        A = toComplex(A)
        if (A == nil) then return end
        local t = cpx.dot(A-B,u) / D
        if t ~= nil then
            local C = B+t*u  -- projeté de A sur d   
            if notDef(C.re) or notDef(C.im) then return
            else return 2*C-A+v
            end
        end
    end    
    
    return ftransform(L,symG1)
end


function simil(L,factor,angle,center) 
-- image de L par une similitude (angle en degrés)
-- L est un complexe ou une liste de complexes ou une liste de listes de complexes
    center = center or 0
    center = toComplex(center)
    if (center == nil) or (factor == nil) or (type(factor) ~= "number") or (angle == nil) or (type(angle) ~= "number") then return end
    angle = angle * math.pi / 180
    
    local simil1 = function(A) -- image d'un point
        A = toComplex(A)
        if (A == nil) then return end
        return cpx.exp(cpx.I*angle)*factor*(A-center) + center
    end
    
    return ftransform(L,simil1)
end

function rotate(L,angle,center)
-- image de L par une rotation (angle en degrés)
-- L est un complexe ou une liste de complexes ou une liste de listes de complexes
    return simil(L,1,angle,center)
end

function shift(L,u)
-- image de L par la translation de vecteur u
-- L est un complexe ou une liste de complexes ou une liste de listes de complexes
    u = toComplex(u)
    if u == nil then return end
    
   local shift1 = function(A) -- image d'un point
        A = toComplex(A)
        if (A == nil) then return end
        return A + u
    end
    
    return ftransform(L,shift1)
end

function hom(L,factor,center)
-- image de L par une homothétie
-- L est un complexe ou une liste de complexes ou une liste de listes de complexes
    return simil(L,factor,0,center)
end

function affin(L,d,v,k)
-- image de  L par l'affinité de base la droite d, parallèlement au vecteur v (non nul) et de rapport k
-- L est un complexe ou une liste de complexes ou une liste de listes de complexes
    if (d == nil) or (type(d) ~= "table") or (#d ~= 2) then return end
    if (k == nil) or (type(k) ~= "number") then return end
    local B, u = d[1], d[2]
    B = toComplex(B); u = toComplex(u); v = toComplex(v)
    if (B == nil) or (u == nil) or (v == nil) or ((u.re == 0) and (u.im == 0)) or ((v.re == 0) and (v.im == 0)) then return end
    local D = cpx.det(u,v)
    
    local affin1 = function(A) -- image d'un point
        A = toComplex(A)
        if (A == nil) then return end
        local t = cpx.det(A-B,v) / D
        if t ~= nil then
            local C = B+t*u -- projeté oblique de A sur d
            return k*(A-C)+C
        end
    end
    
    return ftransform(L,affin1)
end

function inv(L, rayon, centre)
-- image de  L par l'inversion
-- L est un complexe ou une liste de complexes ou une liste de listes de complexes
    centre = centre or 0
    centre = toComplex(centre)
    if (rayon == nil) or (type(rayon) ~= "number") or (rayon <= 0) then return end
        
    local inv1 = function(A)
        A = toComplex(A)
        if (A == nil) then return end
        if A == centre then return
        else
            return centre + rayon*rayon / cpx.bar(A-centre)
        end
    end    
    return ftransform(L,inv1)
end    
