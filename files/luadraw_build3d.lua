-- luadraw_build3d.lua (chargé par luadraw__graph3d)
-- date 2026/05/07
-- version 3.0
-- Copyright 2026 Patrick Fradin
-- This work may be distributed and/or modified under the
-- conditions of the LaTeX Project Public License.
-- The latest version of this license is in
--   https://www.ctan.org/license/lppl

-- construction d'objets 3d

local ld = luadraw
local cpx = ld.cpx
local Z = cpx.Z
local pt3d = ld.pt3d
local toPoint3d = pt3d.toPoint3d
local isPoint3d = pt3d.isPoint3d
local Origin, vecI, vecJ, vecK = pt3d.Origin, pt3d.vecI, pt3d.vecJ, pt3d.vecK
local M, Mc, Ms = pt3d.M, pt3d.Mc, pt3d.Ms
local map = ld.map


function ld.plane(A,B,C)
-- renvoie leplan passant par A, B et C
    local n = pt3d.prod(B-A,C-A)
    if pt3d.N1(n) > 1e-12 then 
        return {A,n}
    end
end

function ld.orthoframe(P, u) -- returns an orthonormal frame of the plane P={A,n}
    local A, n = table.unpack(P)
    n = pt3d.normalize(n)
    if u ~= nil then u = ld.proj3d(u,{Origin,n}) end
    if (u == nil) or (pt3d.N1(u) < 1e-12) then u = pt3d.prod(n,vecI) end
    if pt3d.N1(u) < 1e-12 then -- u = 0
        u = pt3d.prod(n,vecJ)
    end
    u = pt3d.normalize(u)
    local v = pt3d.prod(n,u)
    return A, u, v
end

function ld.plane2ABC(P) -- returns 3 points of plane P={A,n}
    local A,u,v = ld.orthoframe(P)
    return A, A+u, A+v
end

function ld.planeEq(a,b,c,d)
-- renvoie le plan d'équation ax+by+cz+d=0
    local A, n
    n = M(a,b,c)
    if a ~= 0 then 
        A = M(-d/a,0,0)
    elseif b ~= 0 then
        A = M(0,-d/b,0)
    elseif c ~= 0 then
        A = M(0,0,-d/c)
    else return
    end
    return {A,n}
end

function ld.planeEqn(a,b,c,d) -- old version
    return ld.planeEq(a,b,c,d)
end    

function ld.classify3d(L, n)
-- classe les points 3d de la liste L (supposés coplanaires) pour former une facette orientée par n
-- cette facette est supposée CONVEXE
    if (L == nil) or (type(L) ~= "table")  or (#L == 0) then return end
    local G = pt3d.isobar3d(L)
    local u1 = L[1]
    u1 = pt3d.normalize(u1-G)
    local v = pt3d.normalize(n)
    local u2 = pt3d.prod(v,u1)
    local rep = {}
    for k,A in ipairs(L) do
        local pos = A-G
        local zarg = cpx.arg( Z(pt3d.dot(pos,u1), pt3d.dot(pos,u2)) )
        if zarg ~= nil then table.insert(rep, {zarg, k} ) end
    end
    table.sort(rep, function(e1,e2) return e1[1] < e2[1] end)
    local res = {}
    for _,cp in ipairs(rep) do
        table.insert(res, L[cp[2]])
    end
    return res
end

function ld.getfacet(P,L)
-- renvoie la liste des facettes du polyèdre P = { vertices={sommets}, facets = {facettes avec n° de sommets} }, dont le numéro est dans la liste L
-- si L est un entier alors on renvoie la facette numéro L
-- si L vaut nil, on renvoie toutes les facettes
-- les facettes renvoyées ont des coordonnées 3d
    local n = #P.facets
    if L == nil then L = ld.range(1,n) end
    local rep = {}
    if (type(L) == "number") and (L <= n) then 
        for _,k in ipairs(P.facets[L]) do
            table.insert(rep, P.vertices[k])
        end
        return rep
    end
    for _,i in ipairs(L) do
        if i <= n then
            cp = {}
            for _,k in ipairs(P.facets[i]) do
                table.insert(cp, P.vertices[k])
            end
            table.insert(rep, cp)
        end
    end
    if #rep > 0 then return rep end
end

function ld.facet2plane(L)
-- L est une facette ou une liste de facettes
-- renvoie le plan contenant chaque facette
    local rep
    local afacet = function(f)
        local A, B, C = f[1], f[2], f[3]
        return {A,pt3d.prod(B-A,C-A)}
    end
    if isPoint3d(L[1]) then rep = afacet(L) 
    else
        rep = {}
        for _,f in ipairs(L) do
            table.insert(rep, afacet(f))
        end
    end
    return rep
end
    
function ld.poly2facet(P) -- conversion polyèdre -> liste facettes 3d
-- cette commande prend en entrée un polyèdre P = { vertices={sommets}, facets = {facettes avec n° de sommets} }
-- La fonction renvoie une liste de facettes avec coordonnées 3d
-- les facettes sont orientées par l'ordre d'apparition des sommets.
    local rep = {}
    local F
    for _,facet in ipairs(P.facets) do
        F = {}
        for _,k in ipairs(facet) do
            table.insert(F, P.vertices[k])
        end
        table.insert(rep,F)
    end
    return rep
end

function ld.facet2poly(facetlist,eps)
-- facetlist est une liste de facettes avec coordonnées 3d
-- la fonction renvoie un polyèdre P = { vertices={sommets}, facets = {facettes avec n° de sommets} }
    local poly = {}
    eps = eps or 1e-8
    poly.vertices = {}; poly.facets = {}
    for _, facet in ipairs(facetlist) do
        local aux = {}
        for _, A in ipairs(facet) do
            table.insert(aux, pt3d.insert3d(poly.vertices,A,eps))
        end
        table.insert(poly.facets,aux)
    end
    return poly
end

function ld.reverse_face_orientation(F)
-- F est une facette, une liste de facettes, ou un polyèdre
    if (F == nil) or (type(F) ~= "table") then return end
    local rep = {}
    if isPoint3d(F[1]) then -- une facette
        return ld.reverse(F)
    elseif (F.vertices == nil) then -- liste de facettes
        for _,f in ipairs(F) do 
            table.insert(rep, ld.reverse(f))
        end
        return rep
    else -- polyèdre
        rep.vertices = F.vertices
        rep.facets = map(ld.reverse,F.facets)
        return rep
    end    
end

function ld.splitseg(F,plane)
-- cette fonction coupe le segment F (points 3d) avec le plan plane = {A,n}
-- la fonction renvoie la partie devant (segment), la partie derrière (segment),
    local S,n = table.unpack(plane)
    local dev, der = {}, {}
    local A, B = table.unpack(F)
    local p1, p2, I = pt3d.dot(A-S,n), pt3d.dot(B-S,n)
    if math.abs(p2)<1e-8 then p2 = 0 end
    if math.abs(p1)<1e-8 then -- A sur la facette
        if (p2 == 0) then -- B sur la facette
            dev = F
        elseif p2 > 0 then -- B du bon coté
            dev = F
        else -- B du mauvais côté
            der = F
        end
    elseif p1 > 0 then -- A du bon côté
        if (p2 == 0) then --  B sur la facette
            dev = F
        elseif p2 > 0 then -- B du bon coté
            dev = F
        else -- B du mauvais côté
            I = ld.proj3dO(A,plane,B-A)
            der = {I,B}
            dev = {A,I}
        end
    else -- A du mauvais côté
        if (p2 == 0) then --  B sur la facette
            der = F
        elseif p2 < 0 then -- B du mauvais coté
            der = F
        else -- B du bon côté
            I = ld.proj3dO(A,plane,B-A)
            dev = {I,B}
            der = {A,I}
        end
    end
    return dev, der --sec -- on renvoie le 
end

function ld.splitfacet(F,plane)
-- cette fonction coupe la facette F (points 3d) avec le plan plane = {S,n}
-- la fonction renvoie la partie devant (facette), la partie derrière (facette)
    local S,n = table.unpack(plane)
    local nb, dev, der = #F, {}, {}
    local A1, B1 = nil, F[1]
    local p1, p2, I = nil, pt3d.dot(B1-S,n)
    if math.abs(p2) < 1e-8 then p2 = 0 end
    for k = 2, nb+1 do
        if k == nb+1 then k = 1 end -- on ferme la facette
        A1 = B1; p1 = p2; B1 = F[k]; p2 = pt3d.dot(B1-S,n)
        if math.abs(p2) < 1e-8 then p2 = 0 end
        if (p1*p2 < 0) or (p2 == 0) then
            if p2 == 0 then I = B1 else I = ld.proj3dO(A1,plane,B1-A1) end
            if I ~= nil then 
                table.insert(dev,I) ; table.insert(der,I)
            end
        end
        if (p2 > 0) and (p1 ~= nil) then table.insert(dev,B1) end
        if (p2 < 0) and (p1 ~= nil) then table.insert(der,B1) end
    end
    --local sec = classify3d(coupe,-n)
    if #der < 3 then der = {} end
    if #dev < 3 then dev = {} end
    return dev, der --sec -- on renvoie le devant et le derrière
end

function ld.clipplane(plane,P)
--cette fonction clippe le plan avec le polyèdre P et renvoie la facette obtenue
    local S,n = table.unpack(plane)
    if (S == nil) or (n == nil) then return end
    local coupe = {} 
    for _,F in ipairs(P.facets) do
        local nb = #F
        local A1, B1 = nil, P.vertices[F[1]]
        local p1, p2, I = nil, pt3d.dot(B1-S,n)
        if math.abs(p2)<1e-8 then p2 = 0 end
        for k = 2, nb+1 do
            if k == nb+1 then k = 1 end -- on ferme la facette
            A1 = B1; p1 = p2; B1 = P.vertices[F[k]]; p2 = pt3d.dot(B1-S,n)
            if math.abs(p2) < 1e-8 then p2 = 0 end
            if (p1*p2 < 0) or (p2 == 0) then
                if p2 == 0 then I = B1 else I = ld.proj3dO(A1,plane,B1-A1) end
                if I ~= nil then 
                    pt3d.insert3d(coupe,I,1e-10)
                end
            end
        end
    end
    return ld.classify3d(coupe,n) -- on renvoie la section (facette orientée par n) 
end

function ld.cutpoly(P,plane,close)
-- cette fonction coupe le polyèdre P = { vertices={sommets}, facets = {facettes avec n° de sommets} }
-- avec le plan plane = {A,n}
-- la partie contenant n est conservée 
-- si close vaut true le polyèdre est refermé avec la section, sinon il est ouvert
-- la fonction renvoie la partie conservée (polyèdre), la partie non conservée (polyèdre), et la section (liste de point3d orientée par -n)
    local S,n = table.unpack(plane)
    close = close or false
    local res, res2, coupe = {}, {}, {} -- res = sortie, res2 = autre partie du polyèdre coupé
    for _,F in ipairs(P.facets) do
        local nb, aux, aux2 = #F, {}, {}
        local A1, B1 = nil, P.vertices[F[1]]
        local p1, p2, I = nil, pt3d.dot(B1-S,n)
        if math.abs(p2)<1e-8 then p2 = 0 end
        for k = 2, nb+1 do
            if k == nb+1 then k = 1 end -- on ferme la facette
            A1 = B1; p1 = p2; B1 = P.vertices[F[k]]; p2 = pt3d.dot(B1-S,n)
            if math.abs(p2) < 1e-8 then p2 = 0 end
            if (p1*p2 < 0) or (p2 == 0) then
                if p2 == 0 then I = B1 else I = ld.proj3dO(A1,plane,B1-A1) end
                if I ~= nil then 
                    table.insert(aux,I) ; table.insert(aux2,I)
                    pt3d.insert3d(coupe,I,1e-10)
                end
            end
            if (p2 > 0) and (p1 ~= nil) then table.insert(aux,B1) end
            if (p2 < 0) and (p1 ~= nil) then table.insert(aux2,B1) end
        end
        if #aux>2 then table.insert(res,aux) end
        if #aux2>2 then table.insert(res2,aux2) end
    end
    local sec = ld.classify3d(coupe,-n)
    if (#coupe > 2) and close then
        table.insert(res,sec)
        table.insert(res2,ld.reverse(sec))
    end
    return ld.facet2poly(res), ld.facet2poly(res2), sec -- on renvoie des polyèdres et la section (facette orientée par -n) 
end

function ld.cutfacet(L,plane,close)
-- cette fonction coupe les facettes de L (L est une facette ou une liste de facettes)
-- avec le plan plane = {A,n}
-- la partie contenant n est conservée 
-- si close vaut true la section est ajoutée, sinon il est ouvert
-- la fonction renvoie la partie conservée (facettes), la partie non conservée (facettes), et la section (liste de point3d orientée par -n)
    if isPoint3d(L[1]) then L = {L} end -- pour avoir une liste de facettes
    if L.vertices ~= nil then -- polyèdre
        L = ld.poly2facet(L)
    end
    local S,n = table.unpack(plane)
    close = close or false
    local res, res2, coupe = {}, {}, {} -- res = sortie, res2 = autre partie du polyèdre coupé
    for _,F in ipairs(L) do
        local nb, aux, aux2 = #F, {}, {}
        local A1, B1 = nil, F[1]
        local p1, p2, I = nil, pt3d.dot(B1-S,n)
        if math.abs(p2)<1e-8 then p2 = 0 end
        for k = 2, nb+1 do
            if k == nb+1 then k = 1 end -- on ferme la facette
            A1 = B1; p1 = p2; B1 = F[k]; p2 = pt3d.dot(B1-S,n)
            if math.abs(p2) < 1e-8 then p2 = 0 end
            if (p1*p2 < 0) or (p2 == 0) then
                if p2 == 0 then I = B1 else I = ld.proj3dO(A1,plane,B1-A1) end
                if I ~= nil then 
                    table.insert(aux,I) ; table.insert(aux2,I)
                    pt3d.insert3d(coupe,I,1e-10)
                end
            end
            if (p2 > 0) and (p1 ~= nil) then table.insert(aux,B1) end
            if (p2 < 0) and (p1 ~= nil) then table.insert(aux2,B1) end
        end
        if #aux>2 then table.insert(res,aux) end
        if #aux2>2 then table.insert(res2,aux2) end
    end
    local sec = ld.classify3d(coupe,-n)
    if (#coupe > 2) and close then
        table.insert(res,sec)
        table.insert(res2,ld.reverse(sec))
    end
    return res, res2, sec -- on renvoie des listes de facettes et la section (facette orientée par -n) 
end

local clipfacetfacet = function(S, poly, exterior)
-- clippe la liste de facettes S avec le polyèdre convexe poly (sous forme d'une liste de facettes)
-- exterior (true/false) indique si on conserve l'extérieur ou pas
    exterior = exterior or false
    local A, B, C, u, S1
    if not exterior then -- on conserve l'intérieur
        for _,facet in ipairs(poly) do
            A = facet[1]
            B = facet[2]
            C = facet[3]
            u = pt3d.prod(B-A,C-A)
            if u ~= nil then
                S = ld.cutfacet(S,{A,-u})
            end
        end
        return S
    else -- on conserve l'extérieur
        local rep = {}
        for _,facet in ipairs(poly) do
            A = facet[1]
            B = facet[2]
            C = facet[3]
            u = pt3d.prod(B-A,C-A)
            if u ~= nil then
                S1, S = ld.cutfacet(S,{A,u})
                insert(rep,S1)
            end
        end
        return rep
    end
end    

function ld.clip3d(S, poly, exterior)
-- clippe la liste de facettes S avec le polyèdre convexe poly (polyèdre)
-- exterior (true/false) indique si on conserve l'extérieur ou pas
-- renvoie une liste de facettes
    if S.vertices ~= nil then --S est sous forme de polyèdre
        S = ld.poly2facet(S)
    end
    if poly.vertices == nil then -- poly est une liste de facettes
        return clipfacetfacet(S,poly,exterior)
    end
    exterior = exterior or false
    local A, B, C, u, S1
    if not exterior then -- on conserve l'intérieur
        for _,facet in ipairs(poly.facets) do
            A = poly.vertices[facet[1]]
            B = poly.vertices[facet[2]]
            C = poly.vertices[facet[3]]
            u = pt3d.prod(B-A,C-A)
            if u ~= nil then
                S = ld.cutfacet(S,{A,-u})
            end
        end
        return S
    else -- on conserve l'extérieur
        local rep = {}
        for _,facet in ipairs(poly.facets) do
            A = poly.vertices[facet[1]]
            B = poly.vertices[facet[2]]
            C = poly.vertices[facet[3]]
            u = pt3d.prod(B-A,C-A)
            if u ~= nil then
                S1, S = ld.cutfacet(S,{A,u})
                ld.insert(rep,S1)
            end
        end
        return rep
    end
end    

function ld.facetedges(F)
-- F est une liste de facettes avec point3d ou bien un polyedre
-- la fonction renvoie la liste des arêtes (ligne polygonale 3d)
    local P = F
    if P.vertices == nil then P = ld.facet2poly(F) end
    local rep = {}
    local a, b, ab, n
    for _,facet in ipairs(P.facets) do -- parcours du polyèdre par facette
        b = facet[1]; n = #facet
        for k = 2, n+1 do
            if k == n+1 then k = 1 end
            a = b; b = facet[k]
            if a > b then 
                ab = a..";"..b -- pour servir de clé
            else ab = b..";"..a
            end
            rep[ab] = {a,b}
        end
    end
    local aux = {}
    for _,aret in pairs(rep) do
        table.insert(aux,{P.vertices[aret[1]], P.vertices[aret[2]]})
    end
    return aux  -- liste de segments 3d
end

function ld.facetvertices(F)
-- F est une liste de facettes avec point3d ou bien un polyèdre
-- la fonction renvoie la liste des sommets (liste de points 3d)
    if (F == nil) or (type(F) ~= "table") or (#F == 0) then return end
    if F.vertices ~= nil then return table.copy(F.vertices) end
    if isPoint3d(F[1]) then F = {F} end
    local S = {}
    eps = eps or 1e-8
    for _, facet in ipairs(F) do
        for _, A in ipairs(facet) do
            pt3d.insert3d(S,A,eps) -- insertion sans répétition
        end
    end
    return S
end

function ld.border(P)
-- P est un polyèdre ou une liste de facettes
-- la fonction renvoie une ligne polygonale 3d
-- qui correspond au bord de P, c'est à dire les arêtes appartenant à une seule face.
    if (P == nil) or (type(P) ~= "table") then return end
    if P.vertices == nil then P = ld.facet2poly(P) end -- si P est une liste de facettes
    local rep = {}
    local inserer = function(a,b) -- arete = {a,b} deux entiers n° de sommets
        if a > b then a, b = b, a end
        local ab = a..";"..b -- pour servir de clé
        if rep[ab] ~= nil then rep[ab][2] = rep[ab][2] + 1
        else
            rep[ab] = {{a,b},1} 
        end
    end
    local facet, a, b
    for _,facet in ipairs(P.facets) do -- parcours du polyèdre par facette
        b = facet[1]
        for k = 2, #facet do
            a = b
            b = facet[k]
            inserer(a,b)
        end
        inserer(b,facet[1])
    end
    local aux = {}
    for _,aret in pairs(rep) do
        a, b = table.unpack(aret[1]) -- n° des sommets formant l'arête
        if aret[2] == 1 then -- arête du bord
            table.insert(aux,{a,b})
        end
    end
    aux = ld.merge(aux)
    rep = {}
    for _,F in ipairs(aux) do
        local cp = {}
        for _, k in ipairs(F) do
            table.insert(cp, P.vertices[k]) -- conversion en point 3d
        end
        table.insert(rep,cp)
    end
    return rep  -- ligne polygonale 3d
end


-- fonctions revoyant un polyèdre

function ld.tetra(S,v1,v2,v3)
-- construit un tétraèdre de sommet S et des trois vecteurs v1, v2, v3 supposés dans le sens direct
-- la fonction renvoie une liste de sommets (point3d) suivie d'une liste de facettes avec les numéros des sommets
   return { ["vertices"] = {S,S+v1,S+v2,S+v3}, ["facets"] = {{1,3,2},{1,2,4},{2,3,4},{1,4,3}} }
end   

function ld.parallelep(A,v1,v2,v3)
-- construit un parallélépipède à partir d'un sommet A et de 3 vecteurs, supposés dans le sens direct
    local B, C, D, E, F, G, H 
    B = A+v1; C = B+v2; D = A+v2; E = A+v3; F = E+v1; G = F+v2; H = E+v2
    return { ["vertices"]={A,B,C,D,E,F,G,H}, ["facets"]={{1,4,3,2},{5,6,7,8},{1,2,6,5},{8,7,3,4},{1,5,8,4},{6,2,3,7}} }
end

function ld.prism(base, vector, open)
-- construit un prisme, base est liste de point3d, vector est un vecteur 3d de translation
-- open est un booléen indiquant si le prisme est ouvert ou non, false par défaut
-- la base doit être orientée par le vector
    open = open or false
    local n = #base -- nombre de sommets
    local P = {}
    P.vertices = table.copy(base)
    for k = 1, n do
        table.insert(P.vertices,base[k]+vector)
    end
    P.facets = {}
    local aux
    if not open then
        aux = {}
        for k = n, 1, -1 do table.insert(aux,k) end
        table.insert(P.facets, aux)
        aux = {}
        for k = 1, n do table.insert(aux, n+k) end
        table.insert(P.facets, aux)
    end
    aux = {}
    for k = 1, n-1 do 
        table.insert(P.facets,{k,k+1,k+n+1,k+n})
    end
    table.insert(P.facets,{n,1,n+1,2*n})
    return P
end

function ld.pyramid(base,vertex,open)
-- construit une pyramide, base est liste de point3d, vertex est le sommet (point3d)
-- open est un booléen indiquant si la base est ouverte ou non, false par défaut
-- la base doit être orientée par le sommet
    local n = #base
    open = open or false
    local P = {}
    local aux
    P.vertices = table.copy(base); table.insert(P.vertices, vertex)
    P.facets = {}
    if not open then
        aux = {}
        for k = n, 1, -1 do table.insert(aux,k) end
        table.insert(P.facets,aux)
    end 
    for k = 1, n-1 do 
        table.insert(P.facets,{k,k+1,n+1})
    end
    table.insert(P.facets,{n,1,n+1})
    return P
end

function ld.truncated_pyramid(base,vertex,height,open)
-- construit une pyramide tronquée, base est liste de point3d, vertex est le sommet (point3d)
-- height indique la hauteur partant de la base
-- open est un booléen indiquant si la base est ouverte ou non, false par défaut
-- la base doit être orientée par le sommet
    local Pyr = ld.pyramid(base,vertex,open)
    local Pb = ld.facet2plane(base)
    local n = pt3d.normalize(Pb[2])
    local A = ld.proj3d(vertex,Pb)
    local B = A+height*n
    return ld.cutpoly(Pyr,{B,-n},not open)
end

function ld.regular_pyramid(n,side,height,open,center,axe)
-- pyramide régulière
-- n = nombre de côtés, side = longueur d'un côté, center= centre de la base, axe = vecteur directeur de l'axe
-- open est un booléen indiquant si la base est ouverte ou non, false par défaut
    open = open or false
    center = center or Origin
    axe = axe or vecK
    axe = pt3d.normalize(axe)
    local X = side/(2*math.sin(math.pi/n))
    local base = ld.polyreg(0,X,n) -- regular n-sided 2d polygon 
    local A, u, v = center, vecI, vecJ
    if axe ~= vecK then 
        A, u, v = ld.orthoframe({center,axe})
    end
    base = map( function(z) return A+z.re*u+z.im*v end, base) -- conversion 2d -> 3d 
    local S = height*axe
    if (base ~= nil) and (S ~= nil) then return ld.pyramid(base,S,open) end
end

function ld.cylinder(A,V,R,nbfacet,open,aux) -- ou cylinder(C,R,A,nbfacet,open) ou cylinder(C,R,V,A,nbfacet,open)
-- construit un cylindre de rayon R, d'axe {A,V} (V vecteur 3d non nul)
-- nbfacet vaut 35 par défaut
-- open=true/false vaut false par défaut
    local B
    if type(V) == "number" then -- format 2 ou format 3
        if (nbfacet == nil) or (type(nbfacet) == "number") then -- format 2
            local r = V -- radius
            V = A-R -- sommet - centre
            A = R; R = r
            B = A+V
        else -- format 3
            local r = V -- radius
            B = A -- center
            V = R -- vecteur normal à la base
            A = nbfacet -- sommet
            if pt3d.dot(V,B-A) < 0 then V = -V end
            nbfacet = open
            open = aux
            R = r
        end
    else B = A+V
    end
    local vect = pt3d.normalize(V)
    nbfacet = nbfacet or 35
    open = open or false
    local P = {}
    P. vertices= {}; P.facets = {}
    local u, pas = 0, 2*math.pi/nbfacet
    local v1 = pt3d.prod(vecK,vect)
    if pt3d.isNul(v1) then v1 = R*vecI else v1 = R*pt3d.normalize(v1) end
    local v2 = pt3d.prod(vect,v1)
    for k1 = 1, nbfacet do
        table.insert(P.vertices,A+math.cos(u)*v1+math.sin(u)*v2); u = u+pas
    end
    u = 0
    for k1 = 1, nbfacet do
        table.insert(P.vertices, B+math.cos(u)*v1+math.sin(u)*v2); u = u+pas
    end
    if not open then
        local aux = {}
        for k2 = nbfacet, 1, -1 do table.insert(aux, k2) end; table.insert(P.facets,aux)
        aux = {}
        for k2 = nbfacet+1, 2*nbfacet do table.insert(aux, k2) end; table.insert(P.facets,aux)
    end
    for k2 = 1, nbfacet-1 do table.insert(P.facets, {k2,k2+1,k2+1+nbfacet,k2+nbfacet}) end
    table.insert(P.facets, {nbfacet,1,1+nbfacet,2*nbfacet})
    return P
end


function ld.cone(A,V,R,nbfacet,open,aux) -- ou cone(C,R,A,nbfacet,open) ou cone(C,R,V,A,nbfacet,open)
-- construit un cône de rayon A, base en A+V et de rayon R à la base (V vecteur 3d non nul)
-- nbfacet vaut 35 par défaut
-- open=true/false vaut false par défaut
    local B
    if type(V) == "number" then -- format 2 ou format 3
        if (nbfacet == nil) or (type(nbfacet) == "number") then -- format 2
            local r = V -- radius
            V = A-R -- sommet - centre
            A = R; R = r
            B = A+V
        else -- format 3
            local r = V -- radius
            B = A -- center
            V = R -- vecteur normal à la base
            A = nbfacet -- sommet
            if pt3d.dot(V,B-A) < 0 then V = -V end
            nbfacet = open
            open = aux
            R = r
        end
    else B = A+V
    end
    local vect = pt3d.normalize(V)
    nbfacet = nbfacet or 35
    nbfacet = nbfacet+1
    open = open or false
    local P = {}
    P.vertices= {A}; P.facets = {}
    local u, pas = 0, 2*math.pi/(nbfacet-1)
    local v1 = pt3d.prod(vecK,vect)
    if pt3d.isNul(v1) then v1 = R*vecI else v1 = R*pt3d.normalize(v1) end
    local v2 = pt3d.prod(vect,v1)
    for k1 = 1, nbfacet-1 do
        table.insert(P.vertices,B+math.cos(u)*v1+math.sin(u)*v2); u = u+pas
    end
    if not open then
        local aux = {}
        for k2 = 2, nbfacet do table.insert(aux, k2) end; table.insert(P.facets,aux)
    end
    for k2 = 2, nbfacet-1 do table.insert(P.facets, {k2+1,k2,1}) end
    table.insert(P.facets, {2,nbfacet,1})
    return P    
end

function ld.frustum(C,R,r,V,A,nb,open) -- ou frustum(C,R,r,V,nb,open), frustum build with facets (tronc de cône droit ou penché)
    if type(A) == "number" then -- syntaxe C,V,R,nb,open
        open = nb; nb = A; A = nil
    elseif isPoint3d(A) then V = ld.dproj3d(A,{C,V}) - C -- frustum penché
    end
    nb = nb or 35
    open = open or false
    if R == r then -- cylinder
        if A == nil then return ld.cylinder(C,V,R,nb,open)
        else return ld.cylinder(C,V,R,A,nb,open)
        end
    end
    local k = R/(R-r)
    local H = k*V
    local Co
    if A == nil then Co = ld.cone(C,R,C+H,nb,open) --cone(C+H,-H,R,nb,open)
    else 
        local S = k*(A-r/R*C)
        Co = ld.cone(C,R,V,S,nb,open)
    end
    local P = {C+V,-V}
    local rep = ld.cutpoly(Co, P, not open)
    return rep
end

function ld.sphere(A,R,nbu,nbv)
-- construit une sphère de rayon A de rayonR
--  nbu est le nb de fuseaux, et nbv le nb de tranches
    nbu = nbu or 36
    nbv = nbv or 20
    nbu = nbu+1; nbv = nbv+1
    local pasU, pasV = 2*math.pi/(nbu-1), math.pi/(nbv-1)
    local rep = {}
    rep.vertices, rep.facets = {A+R*vecK}, {}
    -- calcul des sommets
    local u, v, a, b
    v = pasV
    for k1 = 1, nbv-2 do
        u = 0; a = R*math.cos(v); b = R*math.sin(v)
        for k2 = 1, nbu do
            table.insert(rep.vertices, A+M(b*math.cos(u),b*math.sin(u),a))
            u = u + pasU
        end
        v = v + pasV
    end
    table.insert(rep.vertices,A-R*vecK)
    -- calcul des faces
    local dep, last
    for k2 = 2, nbu do 
        table.insert(rep.facets,{k2,k2+1,1})
    end
    for k2 = 2, nbv-2 do
        dep = 2+nbu*(k2-1)
        for k1 = dep, dep+nbu-2 do
            table.insert(rep.facets,{k1,k1+1,k1+1-nbu,k1-nbu})
        end
    end
    dep = 2+nbu*(nbv-3); last = nbu*(nbv-2)+2
    for k2 = dep, dep+nbu-2 do 
        table.insert(rep.facets, {k2,last,k2+1})
    end
    return rep
end    


-- fonctions renvoyant une liste de facettes
local eval_uvmesh = function(F, uvmesh)
-- F =(u,v) -> F(u,v) in R^3
-- uvmesh = {{u1,u2,...u,N}, {v1,v2,...,vM}}
    local S = {}
    for _,u in ipairs(uvmesh[1]) do
        local aux = {}
        for _,v in ipairs(uvmesh[2]) do
            table.insert(aux,F(u,v))
        end
        table.insert(S,aux)
    end
    return S
end

function ld.surface(f,u1,u2,v1,v2,grid) -- or surface(f, uvmesh) with uvmesh = {uvalues, vvalues}
-- renvoie les facettes (point 3d) représentant la surface paramétrée par (u,v) -> f(u,v) dans R^3
-- u1 et u2 sont les bornes pour u, et v1, v2 pour v
-- grid={nbu,nbv} donne le nombre de points suivant u et suivant v
    local F = function(u,v)
        local R = ld.evalf(f,u,v) -- protected evaluation
        if (R == nil) then return cpx.Jump
        else return R
        end
    end
    local different = function(A,B)
        return pt3d.N1(B-A)>1e-10
    end
    local uvmesh, nbu, nbv
    if type(u1) ~= "number" then 
        uvmesh = u1
        nbu, nbv = #uvmesh[1], #uvmesh[2]
    else
        grid = grid or {25,25}
        nbu, nbv = table.unpack(grid)
        uvmesh = {ld.linspace(u1,u2,nbu), ld.linspace(v1,v2,nbv)}
    end
    local S = eval_uvmesh(F, uvmesh)
    local rep = {}
    local A, last
    for i = 1, nbu-1 do
        for j = 1, nbv-1 do
            aux = {}
            A = S[i][j]; first = A; last = A
            if A ~= cpx.Jump then table.insert(aux,A) end
            A = S[i+1][j]
            if (A ~= cpx.Jump) and ((last == cpx.Jump) or different(A,last)) then table.insert(aux,A); last = A end
            A = S[i+1][j+1]
            if (A ~= cpx.Jump) and ((last == cpx.Jump) or different(A,last)) then table.insert(aux,A); last = A end
            A = S[i][j+1]
            if (A ~= cpx.Jump) and ((last == cpx.Jump) or different(A,last)) and ((first == cpx.Jump) or different(A,first)) then table.insert(aux,A) end
            if #aux > 2 then table.insert(rep,aux) end
        end
    end
    return rep
end

function ld.cartesian3d(f,x1,x2,y1,y2,grid,addWall)
-- cartesian surface z=f(x,y) with (x,y) in [x1,x2]x[y1,y2]
-- default grid = {25,25} 
-- addWall = 0 (none), "x", "y", "xy" (partition walls for Dscene3d)
    grid = grid or {25,25}
    addWall = addWall or 0
    local F = function(u,v)
        return M(u,v,f(u,v))
    end
    local rep = ld.surface(F,x1,x2,y1,y2,grid)
    local u1,u2,v1,v2,w1,w2, cube 
    if addWall ~= 0 then
        u1,u2,v1,v2,w1,w2 = ld.getbounds3d(rep)
        cube = ld.parallelep(M(u1,v1,w1),(u2-u1)*vecI,(v2-v1)*vecJ,(w2-w1)*vecK)
    end
    local wall = {}
    if string.find(addWall,"x") ~= nil then
        local x, xpas = x1, (x2-x1)/(grid[1]-1)
        for k = 1, grid[1] do
            local clp = ld.clipplane({x*vecI,vecI},cube)
            if clp ~= nil then table.insert(wall, clp) end 
            x = x+xpas
        end
    end
    if string.find(addWall,"y") ~= nil then
        local y, ypas = y1, (y2-y1)/(grid[2]-1)
        for k = 1, grid[2] do
            local clp = ld.clipplane({y*vecJ,vecJ},cube)
            if clp ~= nil then table.insert(wall, clp) end
            y = y+ypas
        end
    end
    return rep, wall
end

function ld.cylindrical_surface(r,z,u1,u2,v1,v2,grid,addWall)
-- functions r:(u,v) -> r(u,v) and z:(u,v) -> z(u,v)
-- cylindrical surface parametrized by Mc(r(u,v),v,z(u,v)) with (u,v) in [u1,u2]x[v1,v2]
-- default grid = {25,25} 
-- addWall = 0 (none), "v" (planes containing vecK and cos(v)*vecI+sin(v)*vecJ, partition walls for Dscene3d), or "z" (planes z=cte, useful when z does not depend on v), or "vz"
    grid = grid or {25,25}
    addWall = addWall or 0
    local F = function(u,v)
        return Mc(r(v,u),u,z(v,u))
    end
    local rep = ld.surface(F,v1,v2,u1,u2,ld.reverse(grid))
    local wall = {}
    if addWall ~= 0 then
        local x1,x2,y1,y2,z1,z2 = ld.getbounds3d(rep)
        local cube = ld.parallelep(M(x1,y1,z1),(x2-x1)*vecI,(y2-y1)*vecJ,(z2-z1)*vecK)
        if string.find(addWall,"z") ~= nil then
            local u, upas = u1, (u2-u1)/(grid[1]-1)
            for k = 1, grid[1] do
                local clp = ld.clipplane({z(u,v1)*vecK,vecK},cube)
                if clp ~= nil then table.insert(wall, clp) end
                u = u+upas
            end
        end        
        if string.find(addWall,"v") ~= nil then
            local v, vpas = v1, (v2-v1)/(grid[2]-1)
            --for k = 1, grid[2] do
            while v < math.min(v2,v1+math.pi) do
                local clp = ld.clipplane({Origin,-math.sin(v)*vecI+math.cos(v)*vecJ},cube)
                if clp ~= nil then table.insert(wall, clp) end
                v = v+vpas
            end

        end
    end
    return rep, wall
end


function ld.curve2cone(f,t1,t2,S,args)
-- construit un cône de sommet S sur une courbe paramétrée par f sur l'intervalle [t1,t2]
-- args est une table à 4 champs: { nbdots = 15, nbdiv = 0, ratio = 0, obj=false }
-- nbdots est le nombre de points minimal.
-- ratio est un nombre qui est le ratio d'homothétie pour construire l'autre partie du cône
-- la fonction renvoie une liste de facettes et les bords du cône (ligne polygonale 3d)
    args = args or {}
    local nbdots = args.nbdots or 15
    local ratio = args.ratio or 0
    local nbdiv = args.nbdiv or 0
    local obj = args.obj or false
    local base = ld.parametric3d(f,t1,t2,nbdots,false,nbdiv)[1] -- première composante connexe de la courbe
    local nb = #base -- nombre de points
    local cone = {}
    local bords = {}
    table.insert(bords,table.copy(base))
    if ratio ~= 0 then -- il y a une autre partie
        ld.insert(base,ld.concat({S}, ld.scale3d(base,ratio,S))) -- on ajoute le sommet S et les images par l'homothétie de centre S
        table.insert(bords, ld.scale3d(bords[1],ratio,S))
    else
        table.insert(base,S)
    end
    cone.vertices = base
    cone.facets = {}
    for k = 1, nb-1 do 
        table.insert(cone.facets, {k,k+1,nb+1})
    end
    if ratio ~= 0 then
        for k = nb+2, 2*nb do 
            table.insert(cone.facets,{k+1,k,nb+1})
        end
    end
    if obj then
        return ld.facet2obj(cone), bords
    else
        return ld.poly2facet(cone), bords
    end
end


function ld.curve2cylinder(f,t1,t2,V,args)
-- construit un cylindre sur une courbe paramétrée par f sur l'intervalle [t1,t2] translatée avec V
-- args est une table à 3 champs: { nbdots = 15, nbdiv = 0, obj=false}
-- nbdots est le nombre de points minimal.
-- la fonction renvoie une liste de facettes et les bords du cylindre (ligne polygonale 3d)
    args = args or {}
    local nbdots = args.nbdots or 15
    local nbdiv = args.nbdiv or 0
    local obj = args.obj or false
    local base = ld.parametric3d(f,t1,t2,nbdots,false,nbdiv)[1] -- première composante connexe de la courbe
    local nb = #base -- nombre de points
    local cyl = {}
    local bords = {}
    table.insert(bords,table.copy(base))
    ld.insert(base,ld.shift3d(base,V)) -- on ajoute les images par la translation de vecteur V
    table.insert(bords,ld.shift3d(bords[1],V))
    cyl.vertices = base
    cyl.facets = {}
    for k = 1, nb-1 do 
        table.insert(cyl.facets, {k,k+1,k+nb+1,k+nb})
    end
    if obj then
        return ld.facet2obj(cyl), bords
    else
        return ld.poly2facet(cyl), bords
    end
end


function ld.section2tube(section,L,args)
-- section est une facette 3d qui doit être centrée sur le premier point de L
-- L est une liste de points 3d 
-- la fonction renvoie un "tube" centré sur L 
-- args est une table à 4 champs:
-- close=true/false indique si la ligne doit être refermée
-- hollow=true/false indique si le tube a ses extrémités ouvertes (true) ou fermées
-- obj = true/false format obj ou  format facettes
-- addwall=0 (ou 1) permet d'ajouter (pour Dscene3d) des séparations (murs) entre chaque "tronçon" du tube
    if (L == nil) or (type(L) ~= "table") then return end
    
    args = args or {}
    local nbfacet = #section
    local close = args.close or false
    local hollow = args.hollow or false
    local obj = args.obj or false
    if close then hollow = true end
    local addwall = args.addwall or 0
    local poly, sep = {}, {}
    poly.vertices = {}
    poly.facets = {}
    -- orientation de la section
    local a, b, c = L[1], L[2]
    local v = pt3d.prod(section[2]-section[1],section[3]-section[1])
    if pt3d.dot(b-a,v) < 0 then section = ld.reverse(section) end
    
    local num = function(num_section, index)
        return (num_section-1)*nbfacet + (index-1)%nbfacet+1
    end
    -- construction des sections
    local cp = table.copy(L)-- cp is modified
    local nb_sections = 0
    if #cp > 1 then
        local last, crt_section, aux_section, P
        if pt3d.abs(cp[1]-cp[#cp])<1e-8 then table.remove(cp); close = true end
        if close then last = cp[#cp]; table.insert(cp,cp[1]) end
        a = nil; b = nil; c = nil
        for _,m in ipairs(cp) do
            a = b; b = c; c = m
            if a == nil then 
                if b ~= nil then -- première section au début de cp
                    crt_section = section
                    if close then
                        P = {b, pt3d.normalize(last-b)-pt3d.normalize(c-b)} --plan bissecteur last/b/c en b
                        aux_section = ld.proj3dO(crt_section,P,b-c)
                        if aux_section ~= nil then 
                            crt_section = aux_section
                        end
                    end
                    if (not close) and (addwall == 1) then table.insert(sep,crt_section) end
                    ld.insert(poly.vertices, crt_section); nb_sections= nb_sections+1
                    if not hollow then 
                        table.insert(poly.facets, ld.range(nbfacet,1,-1))
                    end
                end
            else -- on a trois points consécutifs
                P = {b, pt3d.normalize(a-b)-pt3d.normalize(c-b)} --plan bissecteur
                aux_section = ld.proj3dO(crt_section,P,b-a) --projection de la liste courante sur le plan P
                if aux_section == nil then 
                    aux_section = ld.shift3d(crt_section,b-a)
                end
                ld.insert(poly.vertices, aux_section); nb_sections = nb_sections+1
                crt_section = aux_section -- list actuelle
                if addwall == 1 then table.insert(sep,aux_section) end
            end
        end
        if not close then --dernière section
            P = {c,b-c} -- plan de la dernière section
            aux_section = ld.proj3dO(crt_section,P,b-c)
            if aux_section == nil then 
                aux_section = ld.shift3d(crt_section,c-b)
            end
            crt_section = aux_section
            ld.insert(poly.vertices, crt_section); nb_sections= nb_sections+1
            if addwall == 1 then table.insert(sep,crt_section) end -- facette séparatrice
        end
    end
    -- construction des facettes
    for s = 1, nb_sections-1 do
        for k = 1, nbfacet do
            table.insert(poly.facets,  {num(s,k), num(s,k+1), num(s+1,k+1), num(s+1,k)})
        end
    end
    if close then
        for k = 1, nbfacet do
            table.insert(poly.facets, {num(nb_sections,k), num(nb_sections,k+1), num(1,k+1), num(1,k)})
        end
    end
    if not hollow then
       table.insert(poly.facets,  map(function(k) return num(nb_sections,k) end, ld.range(1,nbfacet)))
    end
    if #sep == 0 then sep = nil end
    if obj then
        return ld.facet2obj(poly), sep
    else
        return ld.poly2facet(poly), sep
    end 
end    


function ld.line2tube(L,r,args)
-- L est une liste de points 3d 
-- la fonction renvoie un tube centré sur L (liste de facettes)
-- args est une table à 4 champs:
-- nbfacet = 4 par défaut
-- close = true/false indique si la ligne doit être refermée
-- hollow = true/false indique si le tube a ses extrémités ouvertes (true) ou fermées
-- obj = true/false format obj ou  non
-- addwall = 0 (ou 1) permet d'ajouter (pour Dscene3d) des séparations (murs) entre chaque "tronçon" du tube
    if (L == nil) or (type(L) ~= "table") or (#L == 0) then return end
    if not isPoint3d(L[1])  then L = L[1] end -- liste de liste de points 3d, on prend la première composante
    args = args or {}
    local nbfacet = args.nbfacet or 3
    local close = args.close or false
    local hollow = args.hollow or false
    if close then hollow = true end
    local obj = args.obj or false
    local addwall = args.addwall or 0
   
    local cp2section = function(cp) -- traitement d'une composante connexe
        local b, c = cp[1], cp[2]
        local u = pt3d.prod(b-c,vecI)
        if pt3d.isNul(u) then u = pt3d.prod(b-c,vecJ) end
        u = r*pt3d.normalize(u)
        local b1, v, theta = b+u, c-b, 2*math.pi/nbfacet*ld.rad
        local list = {b1}
        for k = 1, nbfacet-1 do
            table.insert(list, ld.rotate3d(b1,k*theta,{b,v}))
        end
        return list
    end
    
    if #L > 1 then
        return ld.section2tube( cp2section(L), L, {hollow = hollow, close = close, addwall = addwall, obj = obj})
    end
end

function ld.rotcurve(p,t1,t2,axe,angle1,angle2,args)
-- renvoie la surface (liste de facettes) balayée par la courbe de la 
-- fonction t ->p(t) sur l'intervalle [t1,t2] en la faisant tourner autour de axe = {point3d, vecteur 3d}
-- d'un angle allant de angle1 (degrés) à angle2
-- args est une table à 3 champs :
-- grid = {25,25} : subdivision pour t et pour l'angle.
-- obj = true/false sortie au format obj ou non
--addwall=0/1/2 permet d'ajouter (pour Dscene3d) des séparations (murs) entre chaque "couche" de facettes (valeur 1) ou chaque "tranche" de rotatation (valeur 2, lorsque L est dans un même plan que l'axe de rotation)
    angle1 = angle1 or -180
    angle2 = angle2 or 180
    args = args or {}
    local grid = args.grid or {25,25}
    local obj = args.obj or false
    local addwall = args.addwall or 0
    --axe[2] = -axe[2]
    local f = function(u,v)
        return ld.rotate3d(p(u),v,axe)
    end
    local sep
    if addwall == 1 then 
        sep = {}
        local t, nb, V = t1, grid[1], axe[2]
        local pas = (t2-t1)/(nb-1)
        for k = 1, nb do
            table.insert(sep, {p(t),V})
            t = t+pas
        end
    elseif addwall == 2 then
        sep = {}
        local A, B, nb = axe[1], p((t1+t2)/2), grid[2]
        local t, dt = angle1, (angle2-angle1)/(nb-1)
        local B1 = ld.rotate3d(B,t,axe)
        for k = 1, nb do
            table.insert(sep, {A,A+axe[2],B1}) -- facette séparatrice
            B1 = ld.rotate3d(B,t,axe)
            t = t+dt
        end
    end
    if obj then 
        return ld.obj_surface(f,t1,t2,angle1,angle2,grid), sep
    else 
        return ld.surface(f,t1,t2,angle1,angle2,grid), sep
    end
end

function ld.rotline(L,axe,angle1,angle2,args)
-- renvoie la surface (liste de facettes) balayée par la liste de points 3d L
-- en la faisant tourner autour de axe = {point3d, vecteur 3d}
-- d'un angle allant de angle1 (degrés) à angle2
-- agrs est une table à 3 champs :
-- nbdots est le nombre de subdivisions pour l'angle
-- close qui indique si la ligne doit être refermée
-- obj = true/false sortie au format obj ou non
--addwall=0/1/2 permet d'ajouter (pour Dscene3d) des séparations (murs) entre chaque "couche" de facettes (valeur 1) ou chaque "tranche" de rotatation (valeur 2, lorsque L est dans un même plan que l'axe de rotation)
    if (L == nil) or (type(L) ~= "table") or (not isPoint3d(L[1])) then return end
    args = args or {}
    local nb = args.nbdots or 25
    --axe[2] = -axe[2]
    angle1 = angle1 or -180
    angle2 = angle2 or 180
    local close = args.close or false
    local obj = args.obj or false
    local addwall = args.addwall or 0
    local L1 = table.copy(L)
    if close then table.insert(L1,L[1]) end

    local f = function(u,v)
        return ld.rotate3d(L1[u],v,axe)
    end
    local n = #L1
    local sep
    if addwall == 1 then 
        sep = {}
        local V = axe[2]
        for k = 1, n do
            table.insert(sep, {L1[k],V})
        end
    elseif addwall == 2 then
        sep = {}
        local A, B = axe[1], L[2]
        local t, dt = angle1, (angle2-angle1)/(nb-1)
        local B1 = ld.rotate3d(B,t,axe)
        for k = 1, nb do
            table.insert(sep, {A,A+axe[2],B1}) -- facette séparatrice
            B1 = ld.rotate3d(B,t,axe)
            t = t+dt
        end
    end
    if obj then 
        return ld.obj_surface(f,1,n,angle1,angle2,{n,nb}), sep
    else 
        return ld.surface(f,1,n,angle1,angle2,{n,nb}), sep
    end    
end


 --------------- obj -------------------------
 
function ld.read_obj_file(file, triangulate) -- Contribution de Christophe BAL 2025/09/02
-- prototype::
--    file : le chemin d'un fichier ext::''OBJ'' au format \wavefront
--           simplifié (non gestion des textures, ni des normales).
--
--    :return: ''polyhedron,{xmin, xmax, ymin, ymax, zmin, zmax}'' où
--             les valeurs extrémales correspondent à celles obtenues
--             en analysant le fichier, et le polyèdre est une version
--             \luadraw du modèle 3D indiqué par le fichier.
------
    triangulate = triangulate or false

    local update_extreme_vals = function (a, a_min, a_max)
    -- sert à déterminer la bounding box du polyèdre
        return math.min(a, a_min), math.max(a, a_max)
    end

    local polyhedron = {}
    local vertices   = {}
    local facets     = {}
    local normals    = {}
    local xmin, xmax = math.huge, -math.huge
    local ymin, ymax = math.huge, -math.huge
    local zmin, zmax = math.huge, -math.huge

    local pattern_decimal = "([%-%d%.e%+%-]+)"
    local pattern_vertex  = "^v%s+" .. pattern_decimal ..
                              "%s+" .. pattern_decimal .. 
                              "%s+" .. pattern_decimal

    -- Dans les \regexs \lua, ''%'' est le caractère d'échappement.
    for line in io.lines(file) do
        -- Nettoyage des espaces finaux et initiaux : en \lua, ''-''
        -- est un caractère spécial pour une recherche non gourmande.
        line = line:match("^%s*(.-)%s*$")
        -- On ignore les lignes vides et les commentaires.
        if line ~= "" and not line:match("^#") then
        -- Cas d'un sommet.
            if line:match("^v%s") then
                -- La \regex suivante est très fragile, mais nous faisons 
                -- confiance aux fichiers ext::''obj'' utilisés.
                local x, y, z = line:match(pattern_vertex)
                --local x, y, z = line:match("^v%s+([%-%d%.]+)%s+([%-%d%.]+)%s+([%-%d%.]+)")
                if x and y and z then
                  x, y, z = tonumber(x), tonumber(y), tonumber(z)
                -- Gestion des valeurs extrémales.
                  xmin, xmax = update_extreme_vals(x, xmin, xmax)
                  ymin, ymax = update_extreme_vals(y, ymin, ymax)
                  zmin, zmax = update_extreme_vals(z, zmin, zmax)
                -- Un nouveau sommet.
                  table.insert(vertices, M(x, y, z))
                end
            -- Cas d'une face.
            elseif line:match("^f%s") then
                local face = {}
                for idx in line:gmatch("(%d+)[^ ]*") do
                  table.insert(face, tonumber(idx))
                end
                if #face > 0 then
                  table.insert(facets, face)
                end
            end
        end
    end
    polyhedron.vertices = vertices
    if triangulate then -- triangulate facets and calculate normals
        local Tfacets = {}
        for _, f in ipairs(facets) do --
            if #f == 3 then table.insert(Tfacets,f)
            else
                local a, c, b = f[1], f[2]
                for k = 3, #f do
                    b = c; c = f[k]
                    table.insert(Tfacets,{a,b,c})
                end
            end
        end
        for k = 1, #vertices do table.insert(normals,Origin) end
        for _,f in ipairs(Tfacets) do
            local a, b, c = table.unpack(f)
            local A,B,C = vertices[a], vertices[b], vertices[c]
            local N = pt3d.normalize( pt3d.prod(B-A,C-A) )
            normals[a] = normals[a] + N; normals[b] = normals[b] + N; normals[c] = normals[c] + N
        end
        for k = 1, #normals do
            normals[k] = pt3d.normalize( normals[k] )
        end
        polyhedron.normals = normals
        polyhedron.facets   = Tfacets
    else
        polyhedron.facets   = facets
    end
    return polyhedron, {xmin, xmax, ymin, ymax, zmin, zmax}  -- polyhedron first
end

function ld.facet2obj(F)
    -- F : list of facets (with 3D points) or polyhedron
    -- returns { vertices={}, facets={}, normals={} }
    if F.vertices == nil then F = facet2poly(F) end
    local res = {}
    res.vertices = table.copy(F.vertices)
    local Tfacets = {}
    local normals = {}
    for _, f in ipairs(F.facets) do --
        if #f == 3 then table.insert(Tfacets,f)
        else
            local a, c, b = f[1], f[2]
            for k = 3, #f do
                b = c; c = f[k]
                table.insert(Tfacets,{a,b,c})
            end
        end
    end
    for k = 1, #F.vertices do table.insert(normals,Origin) end
    for _,f in ipairs(Tfacets) do
        local a, b, c = table.unpack(f)
        local A,B,C = F.vertices[a], F.vertices[b], F.vertices[c]
        local N = pt3d.normalize( pt3d.prod(B-A,C-A) )
        normals[a] = normals[a] + N; normals[b] = normals[b] + N; normals[c] = normals[c] + N
    end
    for k = 1, #normals do
        normals[k] = pt3d.normalize( normals[k] )
    end
    res.normals = normals
    res.facets = Tfacets
    return res
end    


function ld.obj_surface(f,u1,u2,v1,v2,grid) -- or obj_surface(f, uvmesh) with uvmesh= {uvalues, vvalues}
-- surface paramétrée par (u,v) -> f(u,v) dans R^3,  à facettes triangulaires au format obj
-- u1 et u2 sont les bornes pour u, et v1, v2 pour v
-- grid={nbu,nbv} donne le nombre de points suivant u et suivant v
-- renvoie une table à 3champs {vertices = {sommets (3D}, normals = {vecteurs (3D)}, facets = { {1,2,3},...} }
-- les facettes sont triangulaires.
    --grid = grid or {25,25}
    local F = function(u,v)
        local R = ld.evalf(f,u,v) -- protected evaluation
        if (R == nil) then return cpx.Jump
        else return R
        end
    end

    local different = function(A,B)
        return pt3d.N1(B-A)>1e-10
    end
    
    local S, vertices, normals, posvertices = {}, {}, {}, {}
    local posvertex = function(i,j)
        local rep
        if posvertices[i][j] == 0 then
            table.insert(vertices, S[i][j]); rep = #vertices
            table.insert(normals, Origin)
            posvertices[i][j] = rep
        else rep = posvertices[i][j]
        end
        return rep
    end
    local uvmesh, nbu, nbv
    if type(u1) ~= "number" then 
        uvmesh = u1
        nbu, nbv = #uvmesh[1], #uvmesh[2]
    else
        grid = grid or {25,25}
        nbu, nbv = table.unpack(grid)
        uvmesh = {ld.linspace(u1,u2,nbu), ld.linspace(v1,v2,nbv)}
    end
    for _,u in ipairs(uvmesh[1]) do
        local aux, auxN = {}, {}
        for _,v in ipairs(uvmesh[2]) do
            table.insert(aux,F(u,v))
            table.insert(auxN, 0)
        end
        table.insert(S,aux); table.insert(posvertices,auxN)
    end
    local rep = {}
    local A, last
    for i = 1, nbu-1 do
        for j = 1, nbv-1 do
            aux = {}
            A = S[i][j]; first = A; last = A
            if A ~= cpx.Jump then table.insert(aux,A); table.insert(aux,posvertex(i,j)) end
            A = S[i+1][j]; 
            if (A ~= cpx.Jump) and ((last == cpx.Jump) or different(A,last)) then 
                table.insert(aux,A); table.insert(aux,posvertex(i+1,j)); last = A 
            end
            A = S[i+1][j+1]
            if (A ~= cpx.Jump) and ((last == cpx.Jump) or different(A,last)) then 
                table.insert(aux,A); table.insert(aux,posvertex(i+1,j+1)); last = A 
            end
            A = S[i][j+1]
            if (A ~= cpx.Jump) and ((last == cpx.Jump) or different(A,last)) and ((first == cpx.Jump) or different(A,first)) then table.insert(aux,A) table.insert(aux,posvertex(i,j+1)) end
            local n = #aux
            if n > 5 then -- ajout + triangulation
                if n == 6 then table.insert(rep,aux) -- triangle 
                else --quad {a,b,c,d}
                    table.insert(rep,{aux[1],aux[2],aux[3],aux[4],aux[5],aux[6]}) -- triangle {a,b,c}
                    table.insert(rep,{aux[1],aux[2],aux[5],aux[6],aux[7],aux[8]}) -- triangle {a,c,d}
                end
            end
        end
    end
    -- calculs des normales
    for _,f in ipairs(rep) do
        local N = pt3d.normalize( pt3d.prod(f[3]-f[1], f[5]-f[1]) )
        for k = 2,6,2 do
            local pos = f[k]
            normals[pos] = normals[pos]  + N
        end
    end
    -- normalisation
    local result, facet = {}
    result.vertices = vertices; result.facets = {}
    for _,f in ipairs(rep) do
        facet = {}
        for k = 2,6,2 do
            local pos = f[k]
            normals[pos] = pt3d.normalize( normals[pos] )
            table.insert(facet, pos)
        end
        table.insert(result.facets, facet)
    end
    result.normals = normals
    return result
end

------------------------- csv -------------------------------------------

function ld.read_csv_file(file, options)
-- file is the name of the csv file: "<name>.csv"
-- options is a table with fields:
    -- header = true (indcates if the first line is the column names)
    -- dic = false (if true the output will be a list of dictionaries)
    -- sep = "," (separator)
    -- num = true (automatic conversion to numerical values)

    options = options or {}
    local header = options.header 
    if header == nil then header = true end
    local dic = options.dic or false  
    if not header then dic = false end
    local sep = options.sep or "," 
    local num = true  
    if options.num ~= nil then num = options.num end
    
    local trim = function(s) -- removes leading and trailing spaces
        return s:match("^%s*(.-)%s*$")
    end

    local data = {}
    local head_line
    if header then head_line = {} end
    local lg = 1
    for line in io.lines(file) do
        if (lg == 1) and header then
            for part in string.gmatch(line, "([^"..sep.."]+)") do
                table.insert(head_line, trim(part))
            end
        else
            local result = {}
            for part in string.gmatch(line, "([^"..sep.."]+)") do
                if num then
                    local x = tonumber(part)
                    if x ~= nil then table.insert(result, x)
                    else table.insert(result, trim(part))
                    end
                else table.insert(result, trim(part))
                end
            end
            if dic then
                local res = {}
                for k, key in ipairs(head_line) do
                    res[key] = result[k]
                end
                table.insert(data, res)
            else
                table.insert(data, result)
            end
        end
        lg = lg + 1
    end
  return data, head_line
end
