-- luadraw_build3d.lua (chargé par luadraw__graph3d)
-- date 2026/02/17
-- version 2.6
-- Copyright 2026 Patrick Fradin
-- This work may be distributed and/or modified under the
-- conditions of the LaTeX Project Public License.
-- The latest version of this license is in
--   http://www.latex-project.org/lppl.txt.


-- construction d'objets 3d

function plane(A,B,C)
-- renvoie leplan passant par A, B et C
    local n = pt3d.prod(B-A,C-A)
    if pt3d.N1(n) > 1e-12 then 
        return {A,n}
    end
end

function orthoframe(P, u) -- returns an orthonormal frame of the plane P={A,n}
    local A, n = table.unpack(P)
    n = pt3d.normalize(n)
    if u ~= nil then u = proj3d(u,{Origin,n}) end
    if (u == nil) or (pt3d.N1(u) < 1e-12) then u = pt3d.prod(n,vecI) end
    if pt3d.N1(u) < 1e-12 then -- u = 0
        u = pt3d.prod(n,vecJ)
    end
    u = pt3d.normalize(u)
    local v = pt3d.prod(n,u)
    return A, u, v
end

function plane2ABC(P) -- returns 3 points of plane P={A,n}
    local A,u,v = orthoframe(P)
    return A, A+u, A+v
end

function planeEq(a,b,c,d)
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

function planeEqn(a,b,c,d) -- old version
    return planeEq(a,b,c,d)
end    

function classify3d(L, n)
-- classe les points 3d de la liste L (supposés coplanaires) pour former une facette orientée par n
-- cette facette est supposée CONVEXE
    if (L == nil) or (type(L) ~= "table")  or (#L == 0) then return end
    local G = isobar3d(L)
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

function getfacet(P,L)
-- renvoie la liste des facettes du polyèdre P = { vertices={sommets}, facets = {facettes avec n° de sommets} }, dont le numéro est dans la liste L
-- si L est un entier alors on renvoie la facette numéro L
-- si L vaut nil, on renvoie toutes les facettes
-- les facettes renvoyées ont des coordonnées 3d
    local n = #P.facets
    if L == nil then L = range(1,n) end
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

function facet2plane(L)
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
    
function poly2facet(P) -- conversion polyèdre -> liste facettes 3d
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

function facet2poly(facetlist,eps)
-- facetlist est une liste de facettes avec coordonnées 3d
-- la fonction renvoie un polyèdre P = { vertices={sommets}, facets = {facettes avec n° de sommets} }
    local poly = {}
    eps = eps or 1e-8
    poly.vertices = {}; poly.facets = {}
    for _, facet in ipairs(facetlist) do
        local aux = {}
        for _, A in ipairs(facet) do
            table.insert(aux, insert3d(poly.vertices,A,eps))
        end
        table.insert(poly.facets,aux)
    end
    return poly
end

function reverse_face_orientation(F)
-- F est une facette, une liste de facettes, ou un polyèdre
    if (F == nil) or (type(F) ~= "table") then return end
    local rep = {}
    if isPoint3d(F[1]) then -- une facette
        return reverse(F)
    elseif (F.vertices == nil) then -- liste de facettes
        for _,f in ipairs(F) do 
            table.insert(rep,reverse(f))
        end
        return rep
    else -- polyèdre
        rep.vertices = F.vertices
        rep.facets = map(reverse,F.facets)
        return rep
    end    
end

function splitseg(F,plane)
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
            I = proj3dO(A,plane,B-A)
            der = {I,B}
            dev = {A,I}
        end
    else -- A du mauvais côté
        if (p2 == 0) then --  B sur la facette
            der = F
        elseif p2 < 0 then -- B du mauvais coté
            der = F
        else -- B du bon côté
            I = proj3dO(A,plane,B-A)
            dev = {I,B}
            der = {A,I}
        end
    end
    return dev, der --sec -- on renvoie le 
end

function splitfacet(F,plane)
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
            if p2 == 0 then I = B1 else I = proj3dO(A1,plane,B1-A1) end
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

function clipplane(plane,P)
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
                if p2 == 0 then I = B1 else I = proj3dO(A1,plane,B1-A1) end
                if I ~= nil then 
                    insert3d(coupe,I,1e-10)
                end
            end
        end
    end
    return classify3d(coupe,n) -- on renvoie la section (facette orientée par n) 
end

function cutpoly(P,plane,close)
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
                if p2 == 0 then I = B1 else I = proj3dO(A1,plane,B1-A1) end
                if I ~= nil then 
                    table.insert(aux,I) ; table.insert(aux2,I)
                    insert3d(coupe,I,1e-10)
                end
            end
            if (p2 > 0) and (p1 ~= nil) then table.insert(aux,B1) end
            if (p2 < 0) and (p1 ~= nil) then table.insert(aux2,B1) end
        end
        if #aux>2 then table.insert(res,aux) end
        if #aux2>2 then table.insert(res2,aux2) end
    end
    local sec = classify3d(coupe,-n)
    if (#coupe > 2) and close then
        table.insert(res,sec)
        table.insert(res2,reverse(sec))
    end
    return facet2poly(res), facet2poly(res2), sec -- on renvoie des polyèdres et la section (facette orientée par -n) 
end

function cutfacet(L,plane,close)
-- cette fonction coupe les facettes de L (L est une facette ou une liste de facettes)
-- avec le plan plane = {A,n}
-- la partie contenant n est conservée 
-- si close vaut true la section est ajoutée, sinon il est ouvert
-- la fonction renvoie la partie conservée (facettes), la partie non conservée (facettes), et la section (liste de point3d orientée par -n)
    if isPoint3d(L[1]) then L = {L} end -- pour avoir une liste de facettes
    if L.vertices ~= nil then -- polyèdre
        L = poly2facet(L)
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
                if p2 == 0 then I = B1 else I = proj3dO(A1,plane,B1-A1) end
                if I ~= nil then 
                    table.insert(aux,I) ; table.insert(aux2,I)
                    insert3d(coupe,I,1e-10)
                end
            end
            if (p2 > 0) and (p1 ~= nil) then table.insert(aux,B1) end
            if (p2 < 0) and (p1 ~= nil) then table.insert(aux2,B1) end
        end
        if #aux>2 then table.insert(res,aux) end
        if #aux2>2 then table.insert(res2,aux2) end
    end
    local sec = classify3d(coupe,-n)
    if (#coupe > 2) and close then
        table.insert(res,sec)
        table.insert(res2,reverse(sec))
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
                S = cutfacet(S,{A,-u})
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
                S1, S = cutfacet(S,{A,u})
                insert(rep,S1)
            end
        end
        return rep
    end
end    

function clip3d(S, poly, exterior)
-- clippe la liste de facettes S avec le polyèdre convexe poly (polyèdre)
-- exterior (true/false) indique si on conserve l'extérieur ou pas
-- renvoie une liste de facettes
    if S.vertices ~= nil then --S est sous forme de polyèdre
        S = poly2facet(S)
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
                S = cutfacet(S,{A,-u})
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
                S1, S = cutfacet(S,{A,u})
                insert(rep,S1)
            end
        end
        return rep
    end
end    

function facetedges(F)
-- F est une liste de facettes avec point3d ou bien un polyedre
-- la fonction renvoie la liste des arêtes (ligne polygonale 3d)
    local P = F
    if P.vertices == nil then P = facet2poly(F) end
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

function facetvertices(F)
-- F est une liste de facettes avec point3d ou bien un polyèdre
-- la fonction renvoie la liste des sommets (liste de points 3d)
    if (F == nil) or (type(F) ~= "table") or (#F == 0) then return end
    if F.vertices ~= nil then return table.copy(F.vertices) end
    if isPoint3d(F[1]) then F = {F} end
    local S = {}
    eps = eps or 1e-8
    for _, facet in ipairs(F) do
        for _, A in ipairs(facet) do
            insert3d(S,A,eps) -- insertion sans répétition
        end
    end
    return S
end

function border(P)
-- P est un polyèdre ou une liste de facettes
-- la fonction renvoie une ligne polygonale 3d
-- qui correspond au bord de P, c'est à dire les arêtes appartenant à une seule face.
    if (P == nil) or (type(P) ~= "table") then return end
    if P.vertices == nil then P = facet2poly(P) end -- si P est une liste de facettes
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
    aux = merge(aux)
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

function tetra(S,v1,v2,v3)
-- construit un tétraèdre de sommet S et des trois vecteurs v1, v2, v3 supposés dans le sens direct
-- la fonction renvoie une liste de sommets (point3d) suivie d'une liste de facettes avec les numéros des sommets
   return { ["vertices"] = {S,S+v1,S+v2,S+v3}, ["facets"] = {{1,3,2},{1,2,4},{2,3,4},{1,4,3}} }
end   

function parallelep(A,v1,v2,v3)
-- construit un parallélépipède à partir d'un sommet A et de 3 vecteurs, supposés dans le sens direct
    local B, C, D, E, F, G, H 
    B = A+v1; C = B+v2; D = A+v2; E = A+v3; F = E+v1; G = F+v2; H = E+v2
    return { ["vertices"]={A,B,C,D,E,F,G,H}, ["facets"]={{1,4,3,2},{5,6,7,8},{1,2,6,5},{8,7,3,4},{1,5,8,4},{6,2,3,7}} }
end

function prism(base, vector, open)
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

function pyramid(base,vertex,open)
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

function truncated_pyramid(base,vertex,height,open)
-- construit une pyramide tronquée, base est liste de point3d, vertex est le sommet (point3d)
-- height indique la hauteur partant de la base
-- open est un booléen indiquant si la base est ouverte ou non, false par défaut
-- la base doit être orientée par le sommet
    local Pyr = pyramid(base,vertex,open)
    local Pb = facet2plane(base)
    local n = pt3d.normalize(Pb[2])
    local A = proj3d(vertex,Pb)
    local B = A+height*n
    return cutpoly(Pyr,{B,-n},not open)
end

function regular_pyramid(n,side,height,open,center,axe)
-- pyramide régulière
-- n = nombre de côtés, side = longueur d'un côté, center= centre de la base, axe = vecteur directeur de l'axe
-- open est un booléen indiquant si la base est ouverte ou non, false par défaut
    open = open or false
    center = center or Origin
    axe = axe or vecK
    axe = pt3d.normalize(axe)
    local X = side/(2*math.sin(math.pi/n))
    local base = polyreg(0,X,n) -- regular n-sided 2d polygon 
    local A, u, v = center, vecI, vecJ
    if axe ~= vecK then 
        A, u, v = orthoframe({center,axe})
    end
    base = map( function(z) return A+z.re*u+z.im*v end, base) -- conversion 2d -> 3d 
    local S = height*axe
    if (base ~= nil) and (S ~= nil) then return pyramid(base,S,open) end
end

function cylinder(A,V,R,nbfacet,open,aux) -- ou cylinder(C,R,A,nbfacet,open) ou cylinder(C,R,V,A,nbfacet,open)
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


function cone(A,V,R,nbfacet,open,aux) -- ou cone(C,R,A,nbfacet,open) ou cone(C,R,V,A,nbfacet,open)
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

function frustum(C,R,r,V,A,nb,open) -- ou frustum(C,R,r,V,nb,open), frustum build with facets (tronc de cône droit ou penché)
    if type(A) == "number" then -- syntaxe C,V,R,nb,open
        open = nb; nb = A; A = nil
    elseif isPoint3d(A) then V = dproj3d(A,{C,V}) - C -- frustum penché
    end
    nb = nb or 35
    open = open or false
    if R == r then -- cylinder
        if A == nil then return cylinder(C,V,R,nb,open)
        else return cylinder(C,V,R,A,nb,open)
        end
    end
    local k = R/(R-r)
    local H = k*V
    local Co
    if A == nil then Co = cone(C,R,C+H,nb,open) --cone(C+H,-H,R,nb,open)
    else 
        local S = k*(A-r/R*C)
        Co = cone(C,R,V,S,nb,open)
    end
    local P = {C+V,-V}
    local rep = cutpoly(Co, P, not open)
    return rep
end

function sphere(A,R,nbu,nbv)
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

function surface(f,u1,u2,v1,v2,grid)
-- renvoie les facettes (point 3d) représentant la surface paramétrée par (u,v) -> f(u,v) dans R^3
-- u1 et u2 sont les bornes pour u, et v1, v2 pour v
-- grid={nbu,nbv} donne le nombre de points suivant u et suivant v
    local F = function(u,v)
        local R = evalf(f,u,v) -- protected evaluation
        if (R == nil) then return cpx.Jump
        else return R
        end
    end
    local different = function(A,B)
            return pt3d.N1(B-A)>1e-10
    end
    local S = {}
    grid = grid or {25,25}
    local nbu, nbv = table.unpack(grid)
    local upas, vpas = (u2-u1)/(nbu-1), (v2-v1)/(nbv-1)
    local u, v, aux = u1
    for i = 1, nbu do
        aux = {}
        v = v1
        for j = 1, nbv do
            table.insert(aux,F(u,v))
            v = v+vpas
        end
        table.insert(S,aux)
        u = u+upas
    end
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

function cartesian3d(f,x1,x2,y1,y2,grid,addWall)
-- cartesian surface z=f(x,y) with (x,y) in [x1,x2]x[y1,y2]
-- default grid = {25,25} 
-- addWall = 0 (none), "x", "y", "xy" (partition walls for Dscene3d)
    grid = grid or {25,25}
    addWall = addWall or 0
    local F = function(u,v)
        return M(u,v,f(u,v))
    end
    local rep = surface(F,x1,x2,y1,y2,grid)
    local u1,u2,v1,v2,w1,w2, cube 
    if addWall ~= 0 then
        u1,u2,v1,v2,w1,w2 = getbounds3d(rep)
        cube = parallelep(M(u1,v1,w1),(u2-u1)*vecI,(v2-v1)*vecJ,(w2-w1)*vecK)
    end
    local wall = {}
    if string.find(addWall,"x") ~= nil then
        local x, xpas = x1, (x2-x1)/(grid[1]-1)
        for k = 1, grid[1] do
            table.insert(wall, clipplane({x*vecI,vecI},cube)); x = x+xpas
        end
    end
    if string.find(addWall,"y") ~= nil then
        local y, ypas = y1, (y2-y1)/(grid[2]-1)
        for k = 1, grid[2] do
            table.insert(wall, clipplane({y*vecJ,vecJ},cube)); y = y+ypas
        end
    end
    return rep, wall
end

function cylindrical_surface(r,z,u1,u2,v1,v2,grid,addWall)
-- functions r:(u,v) -> r(u,v) and z:(u,v) -> z(u,v)
-- cylindrical surface parametrized by Mc(r(u,v),v,z(u,v)) with (u,v) in [u1,u2]x[v1,v2]
-- default grid = {25,25} 
-- addWall = 0 (none), "v" (planes containing vecK and cos(v)*vecI+sin(v)*vecJ, partition walls for Dscene3d), or "z" (planes z=cte, useful when z does not depend on v), or "vz"
    grid = grid or {25,25}
    addWall = addWall or 0
    local F = function(u,v)
        return Mc(r(u,v),v,z(u,v))
    end
    local rep = surface(F,u1,u2,v1,v2,grid)
    local wall = {}
    if addWall ~= 0 then
        local x1,x2,y1,y2,z1,z2 = getbounds3d(rep)
        local cube = parallelep(M(x1,y1,z1),(x2-x1)*vecI,(y2-y1)*vecJ,(z2-z1)*vecK)
        if string.find(addWall,"z") ~= nil then
            local u, upas = u1, (u2-u1)/(grid[1]-1)
            for k = 1, grid[1] do
                table.insert(wall, clipplane({z(u,v1)*vecK,vecK},cube)); u = u+upas
            end
        end        
        if string.find(addWall,"v") ~= nil then
            local v, vpas = v1, (v2-v1)/(grid[2]-1)
            for k = 1, grid[2] do
                table.insert(wall, clipplane({Origin,-math.sin(v)*vecI+math.cos(v)*vecJ},cube)); v = v+vpas
            end
        end
    end
    return rep, wall
end


function curve2cone(f,t1,t2,S,args)
-- construit un cône de sommet S sur une courbe paramétrée par f sur l'intervalle [t1,t2]
-- args est une table à 3 champs: { nbdots = 15, nbdiv = 0, ratio = 0 }
-- nbdots est le nombre de points minimal.
-- ratio est un nombre qui est le ratio d'homothétie pour construire l'autre partie du cône
-- la fonction renvoie une liste de facettes et les bords du cône (ligne polygonale 3d)
    args = args or {}
    local nbdots = args.nbdots or 15
    local ratio = args.ratio or 0
    local nbdiv = args.nbdiv or 0
    local base = parametric3d(f,t1,t2,nbdots,false,nbdiv)[1] -- première composante connexe de la courbe
    local nb = #base -- nombre de points
    local cone = {}
    local bords = {}
    table.insert(bords,table.copy(base))
    if ratio ~= 0 then -- il y a une autre partie
        insert(base,concat({S},scale3d(base,ratio,S))) -- on ajoute le sommet S et les images par l'homothétie de centre S
        table.insert(bords,scale3d(bords[1],ratio,S))
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
    return poly2facet(cone), bords
end


function curve2cylinder(f,t1,t2,V,args)
-- construit un cylindre sur une courbe paramétrée par f sur l'intervalle [t1,t2] translatée avec V
-- args est une table à 2 champs: { nbdots = 15, nbdiv = 0}
-- nbdots est le nombre de points minimal.
-- la fonction renvoie une liste de facettes et les bords du cylindre (ligne polygonale 3d)
    args = args or {}
    local nbdots = args.nbdots or 15
    local nbdiv = args.nbdiv or 0
    local base = parametric3d(f,t1,t2,nbdots,false,nbdiv)[1] -- première composante connexe de la courbe
    local nb = #base -- nombre de points
    local cyl = {}
    local bords = {}
    table.insert(bords,table.copy(base))
    insert(base,shift3d(base,V)) -- on ajoute les images par la translation de vecteur V
    table.insert(bords,shift3d(bords[1],V))
    cyl.vertices = base
    cyl.facets = {}
    for k = 1, nb-1 do 
        table.insert(cyl.facets, {k,k+1,k+nb+1,k+nb})
    end
    return poly2facet(cyl), bords
end

function section2tube(section,L,args)
-- section est une facette 3d qui doit être centrée sur le premier point de L
-- L est une liste de points 3d 
-- la fonction renvoie un "tube" centré sur L (liste de facettes)
-- args est une table à 3 champs:
-- close=true/false indique si la ligne doit être refermée
-- hollow=true/false indique si le tube a ses extrémités ouvertes (true) ou fermées
-- addwall=0 (ou 1) permet d'ajouter (pour Dscene3d) des séparations (murs) entre chaque "tronçon" du tube
    if (L == nil) or (type(L) ~= "table") then return end
    args = args or {}
    local nbfacet = #section
    local close = args.close or false
    local hollow = args.hollow or false
    if close then hollow = true end
    local addwall = args.addwall or 0
    local rep, sep = {}, {}
    local list, list1, firstList, P, a, b, c = {}, {}
    -- orientation de la section
    a, b = L[1], L[2]
    local v = pt3d.prod(section[2]-section[1],section[3]-section[1])
    if pt3d.dot(b-a,v) < 0 then section = reverse(section) end
    
    local cp2tube = function(cp) -- traitement de la composante 
        local last, u, b1, v, theta, face = nil,nil,nil,nil,{},{}
        if pt3d.abs(cp[1]-cp[#cp])<1e-8 then table.remove(cp); close = true end
        if close then last = cp[#cp]; table.insert(cp,cp[1]) end
        a = nil; b = nil; c = nil
        for _,m in ipairs(cp) do
            a = b; b = c; c = m
            if a == nil then 
                if b ~= nil then -- première section au début de cp
                    list = section
                    if close then
                        P = {b, pt3d.normalize(last-b)-pt3d.normalize(c-b)} --plan bissecteur last/b/c en b
                        list1 = proj3dO(list,P,b-c)
                        if list1 ~= nil then list = list1 end
                    end
                    firstList = list -- première section
                    if (not close) and (addwall == 1) then table.insert(sep,list) end
                    if not hollow then 
                        table.insert(rep, reverse(section))
                    end
                end
            else -- on a trois points consécutifs
                P = {b, pt3d.normalize(a-b)-pt3d.normalize(c-b)} --plan bissecteur
                list1 = proj3dO(list,P,b-a) --projection de la list courante sur le plan P
                if list1 == nil then list1 = shift3d(list,b-a) end
                for k = 1, nbfacet-1 do
                    table.insert(rep, {list[k],list[k+1],list1[k+1],list1[k]})
                end
                table.insert(rep, {list[nbfacet],list[1],list1[1],list1[nbfacet]})
                list = list1 -- list actuelle
                if addwall == 1 then table.insert(sep,list) end
            end
        end
    end
    
    local cp = table.copy(L)-- cp is modified
    if #cp > 1 then
        cp2tube(cp) 
        if close then
            for k = 1, nbfacet-1 do
                table.insert(rep, {list1[k],list1[k+1],firstList[k+1],firstList[k]})
            end
            table.insert(rep, {list1[nbfacet],list1[1],firstList[1],firstList[nbfacet]})
        else
            P = {c,b-c} -- plan de la dernière section
            list1 = proj3dO(list,P,b-c)
            if list1 == nil then list1 = shift3d(list,c-b) end
            for k = 1, nbfacet-1 do
                table.insert(rep, {list[k],list[k+1],list1[k+1],list1[k]})
            end
            table.insert(rep, {list[nbfacet],list[1],list1[1],list1[nbfacet]})
            if not hollow then 
                table.insert(rep, list1)
            end
        end
        if addwall == 1 then table.insert(sep,list1) end -- facette séparatrice
    end
    if #sep == 0 then sep = nil end
    return rep, sep
end

function line2tube(L,r,args)
-- L est une liste de points 3d 
-- la fonction renvoie un tube centré sur L (liste de facettes)
-- args est une table à 4 champs:
-- nbfacet = 4 par défaut
-- close = true/false indique si la ligne doit être refermée
-- hollow = true/false indique si le tube a ses extrémités ouvertes (true) ou fermées
-- addwall = 0 (ou 1) permet d'ajouter (pour Dscene3d) des séparations (murs) entre chaque "tronçon" du tube
    if (L == nil) or (type(L) ~= "table") then return end
    args = args or {}
    local nbfacet = args.nbfacet or 3
    local close = args.close or false
    local hollow = args.hollow or false
    if close then hollow = true end
    local addwall = args.addwall or 0
    local rep, sep = {}, {}
   
    local cp2section = function(cp) -- traitement d'une composante connexe
        local b, c = cp[1], cp[2]
        local u = pt3d.prod(b-c,vecI)
        if pt3d.isNul(u) then u = pt3d.prod(b-c,vecJ) end
        u = r*pt3d.normalize(u)
        local b1, v, theta = b+u, c-b, 2*math.pi/nbfacet*rad
        local list = {b1}
        for k = 1, nbfacet-1 do
            table.insert(list,rotate3d(b1,k*theta,{b,v}))
        end
        return list
    end
    
    if isPoint3d(L[1]) then L = {L} end
    for _, cp in ipairs(L) do
        if #cp > 1 then
            local ret, wall = section2tube( cp2section(cp), cp, {hollow = hollow, close = close, addwall= addwall})
            insert(rep, ret); insert(sep, wall)
        end
    end
    if #sep == 0 then sep = nil end
    return rep, sep
end

function rotcurve(p,t1,t2,axe,angle1,angle2,args)
-- renvoie la surface (liste de facettes) balayée par la courbe de la 
-- fonction t ->p(t) sur l'intervalle [t1,t2] en la faisant tourner autour de axe = {point3d, vecteur 3d}
-- d'un angle allant de angle1 (degrés) à angle2
-- args est une table à deux champs :
-- grid = {25,25} : subdivision pour t et pour l'angle.
--addwall=0/1/2 permet d'ajouter (pour Dscene3d) des séparations (murs) entre chaque "couche" de facettes (valeur 1) ou chaque "tranche" de rotatation (valeur 2, lorsque L est dans un même plan que l'axe de rotation)
    angle1 = angle1 or -180
    angle2 = angle2 or 180
    args = args or {}
    local grid = args.grid or {25,25}
    local addwall = args.addwall or 0
    --axe[2] = -axe[2]
    local f = function(u,v)
        return rotate3d(p(u),v,axe)
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
        local B1 = rotate3d(B,t,axe)
        for k = 1, nb do
            table.insert(sep, {A,A+axe[2],B1}) -- facette séparatrice
            B1 = rotate3d(B,t,axe)
            t = t+dt
        end
    end
    return surface(f,t1,t2,angle1,angle2,grid), sep
end

function rotline(L,axe,angle1,angle2,args)
-- renvoie la surface (liste de facettes) balayée par la liste de points 3d L
-- en la faisant tourner autour de axe = {point3d, vecteur 3d}
-- d'un angle allant de angle1 (degrés) à angle2
-- agrs est une table à 3 champs :
-- nbdots est le nombre de subdivisions pour l'angle
-- close qui indique si la ligne doit être refermée
--addwall=0/1/2 permet d'ajouter (pour Dscene3d) des séparations (murs) entre chaque "couche" de facettes (valeur 1) ou chaque "tranche" de rotatation (valeur 2, lorsque L est dans un même plan que l'axe de rotation)
    if (L == nil) or (type(L) ~= "table") or (not isPoint3d(L[1])) then return end
    args = args or {}
    local nb = args.nbdots or 25
    --axe[2] = -axe[2]
    angle1 = angle1 or -180
    angle2 = angle2 or 180
    local close = args.close or false
    local addwall = args.addwall or 0
    local L1 = table.copy(L)
    if close then table.insert(L1,L[1]) end

    local f = function(u,v)
        return rotate3d(L1[u],v,axe)
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
        local B1 = rotate3d(B,t,axe)
        for k = 1, nb do
            table.insert(sep, {A,A+axe[2],B1}) -- facette séparatrice
            B1 = rotate3d(B,t,axe)
            t = t+dt
        end
    end
    return surface(f,1,n,angle1,angle2,{n,nb}), sep
end

function read_obj_file(file) -- Contribution de Christophe BAL 2025/09/02
-- prototype::
--    file : le chemin d'un fichier ext::''OBJ'' au format \wavefront
--           simplifié (non gestion des textures, ni des normales).
--
--    :return: ''polyhedron,{xmin, xmax, ymin, ymax, zmin, zmax}'' où
--             les valeurs extrémales correspondent à celles obtenues
--             en analysant le fichier, et le polyèdre est une version
--             \luadraw du modèle 3D indiqué par le fichier.
------

    local update_extreme_vals = function (a, a_min, a_max)
    -- sert à déterminer la bounding box du polyèdre
        return math.min(a, a_min), math.max(a, a_max)
    end

    local polyhedron = {}
    local vertices   = {}
    local facets     = {}
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
    polyhedron.facets   = facets
    return polyhedron, {xmin, xmax, ymin, ymax, zmin, zmax}  -- polyhedron first
end

function read_csv_file(file, options)
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
