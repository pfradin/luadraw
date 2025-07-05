-- luadraw_polyedrons.lua 
-- date 2025/07/04
-- version 2.0
-- Copyright 2025 Patrick Fradin
-- This work may be distributed and/or modified under the
-- conditions of the LaTeX Project Public License.
-- The latest version of this license is in
--   http://www.latex-project.org/lppl.txt.


-- fonctions internes
local transf = function(L,O,A,C,S)
-- L représente les sommets d'un polyèdre de centre O et dont A est un sommet
-- on renvoie L transformé en les sommets d'un polyèdre de centre C et dont sommet est S
    local OA, OS1 = A-O, S-C
    local theta = angle3d(OA,OS1) --en radians
    if theta ~= 0 then
        L = rotate3d(L,theta*rad, {O,pt3d.prod(OA,OS1)})
    end
    local k = pt3d.abs(OS1)/pt3d.abs(OA)
    if k ~= 1 then 
        L = scale3d(L,k,O)
    end
    if not pt3d.isNul(C-O) then
        L = shift3d(L,C-O)
    end
    return L
end

local pstar = function(L)
-- L est une liste de pentagrammes croisés : {a,b,c,d,e} (forme d'étoile)
-- la fonction découpe les pentagrammes en facettes convexes et renvoie le résultat
    local res = {}
    for _, P in ipairs(L) do
        local a,b,c,d,e = table.unpack(P) -- un pentagramme
        local a1 = interDD({a,b-a},{d,c-d})
        local d1 = interDD({d,e-d},{a,b-a})
        local b1 = interDD({b,c-b},{d,e-d})
        local e1 = interDD({e,a-e},{b,c-b})
        local c1 = interDD({a,e-a},{d,c-d})
        insert(res, {{a, a1, c1}, {a1, d, d1}, {d1, b, b1}, {b1, e, e1}, {e1, c, c1}, {c1, a1, d1, b1, e1}})
    end
    if #res > 0 then return res end
end

-- construction de polyèdres

function Tetraedre(C,S,all) -- centre,  sommet
-- construction d'un tétraèdre régulier de centre C de sommet S
-- si all vaut true, on renvoie dans cet ordre : 
-- le polyèdre, les sommets, les arêtes (ligne polygonale), les facettes (liste) avec points 3d 
-- si all vaut false (valeur par défaut) on renvoie seulement le polyèdre
    local L = {M(-1,-1,1), M(1,1,1), M(-1,1,-1), M(1,-1,-1)} -- sommets d'un tétraèdre régulier de centre Origin
    L = transf(L, Origin, L[1], C, S)
    local T = { ["vertices"]=L, ["facets"]={{1,2,3},{1,3,4},{1,4,2},{2,4,3}} }
    if all then
        local F1 = poly2facet(T) -- facettes avec points 3d
        local A = facetedges(F1) -- arêtes (ligne polygonale)
        return T, T.vertices, A, F1 -- polyèdre, sommets, arêtes, faces
    else
        return T
    end
end

function Octaedre(C,S,all)
-- construction d'un octaèdre régulier de centre C de sommet S
-- si all vaut true, on renvoie dans cet ordre : 
-- le polyèdre, les sommets, les arêtes (ligne polygonale), les facettes (liste) avec points 3d 
-- si all vaut false (valeur par défaut) on renvoie seulement le polyèdre
    local L = {M(0,0,1), M(-1,0,0), M(0,1,0), M(0,-1,0), M(1,0,0), M(0,0,-1)}
    L = transf(L, Origin, L[1], C, S)
    local T = { ["vertices"]=L, ["facets"]={{1,4,5},{1,5,3},{1,3,2},{1,2,4},{6,4,2},{6,5,4},{6,3,5},{6,2,3}} }
    if all then
        local F1 = poly2facet(T) -- facettes avec points 3d
        local A = facetedges(F1) -- arêtes (ligne polygonale)
        return T, T.vertices, A, F1 -- polyèdre, sommets, arêtes, faces
    else
        return T
    end
end


function TetraedreTrq(C,S,all)
-- construction d'un tétraèdre tronqué de centre C de sommet S
-- si all vaut true, on renvoie dans cet ordre : 
-- le polyèdre, les sommets, les arêtes (ligne polygonale), les facettes de type 1, les facettes de type 2
-- si all vaut false (valeur par défaut) on renvoie seulement le polyèdre

    all = all or false
    local L = {M(3,1,1), M(3,-1,-1), M(-3,1,-1), M(-3,-1,1), M(1,3,1), M(1,-3,-1), M(-1,3,-1), M(-1,-3,1), M(1,1,3), M(1,-1,-3), M(-1,1,-3), M(-1,-1,3)}
    L = transf(L,Origin, L[1], C, S)
    local T, F1, F2 = {}, {{1,2,10,11,7,5},{3,11,10,6,8,4},{3,4,12,9,5,7},{1,9,12,8,6,2}}, {{3,7,11},{2,6,10},{1,5,9},{4,8,12}}
    T.vertices = L
    T.facets = concat(F1,F2)
    if all then
        F1 = poly2facet( {["vertices"]=L, ["facets"]=F1} )
        F2 = poly2facet( {["vertices"]=L, ["facets"]=F2} )
        A = facetedges( poly2facet(T) ) 
        return T, T.vertices, A, F1, F2 -- polyèdre, sommets, arêtes, faces type 1, faces type 2
    else
        return T
    end
end

function Octahemioctaedre(C,S,all)
-- construction d'un Octahémioctaèdre de centre C de sommet S
-- si all vaut true, on renvoie dans cet ordre : 
-- le polyèdre, les sommets, les arêtes (ligne polygonale), les facettes de type 1, les facettes de type 2
-- si all vaut false (valeur par défaut) on renvoie seulement le polyèdre

    all = all or false
    local L = {M(1,1,0),M(1,-1,0),M(-1,1,0),M(-1,-1,0),M(1,0,1),M(1,0,-1),M(-1,0,1),M(-1,0,-1),M(0,1,1),M(0,1,-1),M(0,-1,1),M(0,-1,-1)}
    L = transf(L, Origin, L[1], C, S)
    local T, F1, F2 = {}, {{1,9,5},{1,6,10},{2,5,11},{2,12,6},{3,7,9},{3,10,8},{4,8,12},{4,11,7}}, {{1,5,11,4,8,10},{1,9,7,4,12,6},{2,5,9,3,8,12},{2,6,10,3,7,11}}
    T.vertices = L
    T.facets = concat(F1,F2)
    if all then
        F1 = poly2facet( {["vertices"]=L, ["facets"]=F1} )
        F2 = poly2facet( {["vertices"]=L, ["facets"]=F2} )
        A = facetedges( poly2facet(T) ) 
        return T, T.vertices, A, F1, F2 -- polyèdre, sommets, arêtes, faces type 1, faces type 2
    else
        return T
    end
end

function PtDodecaedreEt(Cter,S,all)
-- construction d'un Petit dodécaèdre étoilé  de centre Cter de sommet S
-- si all vaut true, on renvoie dans cet ordre : 
-- le polyèdre, les sommets, les arêtes (ligne polygonale), les facettes 
-- si all vaut false (valeur par défaut) on renvoie seulement le polyèdre
    all = all or false
    local sqrt = math.sqrt
    local A1 = sqrt((5+sqrt(5))/10)
    local A2 = sqrt((5-sqrt(5))/10)
    local B1 = A1^2
    local B2 = A2^2
    local C = sqrt(5)/5
    local M1 = M(0,1,0)
    local M2 = M(A2,C,-B1)
    local M3 = M(-A2,C,-B1)
    local M4 = M(-A1,C,B2)
    local M5 = M(0,C,2*C)
    local M6 = M(A1,C,B2)
    local M7, M8, M9, M10, M11, M12 = -M3, -M2, -M6, -M5, -M4, -M1
    local L = {M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11, M12}
    L = transf(L, Origin, L[2], Cter, S)
    local T = {}
    T.vertices = L
    T.facets ={{2,4,6,3,5}, {1,9,5,3,8}, {1,7,2,5,11}, {7,9,11,8,10}, {2,7,10,6,12}, {3,12,4,10,8}, {2,12,3,11,9}, {1,10,4,2,9}, {1,11,3,6,10}, {4,12,5,9,7}, {5,12,6,8,11}, {1,8,6,4,7}}
    if all then
        local F = pstar(poly2facet(T)) -- facettes avec points 3d
        local A = facetedges(T) -- arêtes (ligne polygonale)
        return T, T.vertices, A, F -- polyèdre, sommets, arêtes, faces
    else
        return T
    end
end
