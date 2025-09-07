-- luadraw_polyedrons.lua 
-- date 2025/09/07
-- version 2.1
-- Copyright 2025 Patrick Fradin
-- This work may be distributed and/or modified under the
-- conditions of the LaTeX Project Public License.
-- The latest version of this license is in
--   http://www.latex-project.org/lppl.txt.


-- fonctions internes
local cbrt = function(x)
-- cubic root
    if x>=0 then return x^(1/3)
    else return -(math.abs(x))^(1/3)
    end
end
local transf = function(L,O,A,C,S)
-- L représente les sommets d'un polyèdre de centre O et dont A est un sommet
-- on renvoie L transformé en les sommets d'un polyèdre de centre C et dont un sommet est S
    if (C == nil) or (S == nil) then return L end
    local OA, OS1 = A-O, S-C
    local theta = angle3d(OA,OS1) --en radians
    local u = pt3d.prod(OA,OS1)
    if pt3d.N1(u) < 1e-12 then --u ==0
        u = pt3d.prod(OA,C-O)
        if pt3d.N1(u) < 1e-12 then --u ==0
            u = pt3d.prod(OA,vecI)
            if pt3d.N1(u) < 1e-12 then --u ==0
                u = pt3d.prod(OA,vecJ)
            end
        end
    end
    if theta ~= 0 then
        L = rotate3d(L,theta*rad, {O,u})
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

----- solides de Platon

function tetrahedron(C,S,all) -- centre,  sommet
-- construction d'un tétraèdre régulier de centre C de sommet S
-- si all vaut true, on renvoie dans cet ordre : 
-- le polyèdre, les sommets, les arêtes (ligne polygonale), les facettes (liste) avec points 3d 
-- si all vaut false (valeur par défaut) on renvoie seulement le polyèdre
    all = all or false
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
function Tetraedre(C,A,all) --compatibilité ascendante
    return tetrahedron(C,S,all)
end

function octahedron(C,S,all)
-- construction d'un octaèdre régulier de centre C de sommet S
-- si all vaut true, on renvoie dans cet ordre : 
-- le polyèdre, les sommets, les arêtes (ligne polygonale), les facettes (liste) avec points 3d 
-- si all vaut false (valeur par défaut) on renvoie seulement le polyèdre
    all = all or false
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
function Octaedre(C,S,all) --compatibilité ascendante
    return octahedron(C,S,all)
end

function cube(C,S,all)
-- construction d'un cube de centre C de sommet S
-- si all vaut true, on renvoie dans cet ordre : 
-- le polyèdre, les sommets, les arêtes (ligne polygonale), les facettes (liste) avec points 3d 
-- si all vaut false (valeur par défaut) on renvoie seulement le polyèdre
    all = all or false
    local L = {M(-1,1,1), M(-1,-1,1), M(1,1,1), M(1,-1,1), M(-1,1,-1), M(-1,-1,-1), M(1,1,-1), M(1,-1,-1)}
    L = transf(L, Origin, L[1], C, S)
    local T = { ["vertices"]=L, ["facets"]={{1,3,7,5},{3,4,8,7},{2,6,8,4},{2,1,5,6},{1,2,4,3},{5,7,8,6}} }
    if all then
        local F1 = poly2facet(T) -- facettes avec points 3d
        local A = facetedges(F1) -- arêtes (ligne polygonale)
        return T, T.vertices, A, F1 -- polyèdre, sommets, arêtes, faces
    else
        return T
    end
end

function icosahedron(C,S,all)
-- construction d'un octaèdre régulier de centre C de sommet S
-- si all vaut true, on renvoie dans cet ordre : 
-- le polyèdre, les sommets, les arêtes (ligne polygonale), les facettes (liste) avec points 3d 
-- si all vaut false (valeur par défaut) on renvoie seulement le polyèdre
    all = all or false
    local p = (1+math.sqrt(5))/2
    local L = {M(-1,0,p), M(1,0,p), M(0,p,1), M(0,-p,1), M(-p,1,0), M(-p,-1,0), M(p,1,0), M(p,-1,0), M(0,p,-1), M(0,-p,-1), M(-1,0,-p), M(1,0,-p) }
    L = transf(L, Origin, L[1], C, S)
    local T = { ["vertices"]=L, ["facets"]={{1,2,3},{1,3,5},{1,5,6},{1,6,4},{1,4,2},{5,11,6},{6,11,10},{10,4,6},{4,10,8},{4,8,2},{2,8,7},{2,7,3},{3,7,9},{3,9,5},{5,9,11},{12,7,8},{12,8,10},{12,9,7},{12,10,11},{12,11,9}} }
    if all then
        local F1 = poly2facet(T) -- facettes avec points 3d
        local A = facetedges(F1) -- arêtes (ligne polygonale)
        return T, T.vertices, A, F1 -- polyèdre, sommets, arêtes, faces
    else
        return T
    end
end

function dodecahedron(C,S,all)
-- construction d'un dodecaèdre régulier de centre C de sommet S
-- si all vaut true, on renvoie dans cet ordre : 
-- le polyèdre, les sommets, les arêtes (ligne polygonale), les facettes (liste) avec points 3d 
-- si all vaut false (valeur par défaut) on renvoie seulement le polyèdre
    all = all or false
    local p, q = (1+math.sqrt(5))/2, (math.sqrt(5)-1)/2
    local L = {M(-q,0,p), M(q,0,p), M(-1,1,1), M(-1,-1,1), M(1,1,1), M(1,-1,1), M(0,p,q), M(0,-p,q), M(-p,q,0), M(-p,-q,0), 
    M(p,q,0), M(p,-q,0),M(0,p,-q),M(0,-p,-q),M(-1,1,-1),M(-1,-1,-1),M(1,1,-1),M(1,-1,-1),M(-q,0,-p),M(q,0,-p)}
    L = transf(L, Origin, L[1], C, S)
    local T = { ["vertices"]=L, ["facets"]={{1,2,5,7,3}, {1,3,9,10,4}, {1,4,8,6,2}, {2,6,12,11,5}, {15,13,17,20,19}, {16,10,9,15,19}, {20,18,14,16,19}, {17,11,12,18,20}, {16,14,8,4,10}, {12,6,8,14,18}, {9,3,7,13,15}, {17,13,7,5,11}} }
    if all then
        local F1 = poly2facet(T) -- facettes avec points 3d
        local A = facetedges(F1) -- arêtes (ligne polygonale)
        return T, T.vertices, A, F1 -- polyèdre, sommets, arêtes, faces
    else
        return T
    end
end

-- fin solides de Platon

-- solides d'Archimède

function cuboctahedron(C,S,all)
-- de centre C de sommet S
-- si all vaut true, on renvoie dans cet ordre : 
-- le polyèdre, les sommets, les arêtes (ligne polygonale), les facettes (liste) avec points 3d 
-- si all vaut false (valeur par défaut) on renvoie seulement le polyèdre
    all = all or false
    local C0  =  math.sqrt(2) / 2
    local T = {}
    local L = {M(C0,0.0,C0),M(C0,0.0,-C0),M(-C0,0.0,C0),M(-C0,0.0,-C0),M(C0,C0,0.0),M(C0,-C0,0.0),M(-C0,C0,0.0),M(-C0,-C0,0.0),M(0.0,C0,C0),M(0.0,C0,-C0),M(0.0,-C0,C0),M(0.0,-C0,-C0)}
    L = transf(L, Origin, L[1], C, S)
    T.vertices = L
    T.facets = {{1,6,2,5},{1,9,3,11},{8,3,7,4},{8,12,6,11},{10,2,12,4},{10,7,9,5},{1,5,9},{2,6,12},{3,8,11},{4,7,10},{5,2,10},{6,1,11},{7,3,9},{8,4,12}}
    if all then
        local A = facetedges( poly2facet(T) ) 
        local n = #T.facets
        return T, T.vertices, A, getfacet(T,range(1,6)), getfacet(T,range(7,n)) -- polyèdre, sommets, arêtes, faces type 1, faces type 2
    else
        return T
    end
end


function icosidodecahedron(C,S,all)
-- de centre C de sommet S
-- si all vaut true, on renvoie dans cet ordre : 
-- le polyèdre, les sommets, les arêtes (ligne polygonale), les facettes (liste) avec points 3d 
-- si all vaut false (valeur par défaut) on renvoie seulement le polyèdre
    all = all or false
    local C0  =  (1 + math.sqrt(5)) / 4
    local C1  =  (3 + math.sqrt(5)) / 4
    local C2  =  (1 + math.sqrt(5)) / 2
    local T = {}
    local L = {M(0.0,0.0,C2),M(0.0,0.0,-C2),M(C2,0.0,0.0),M(-C2,0.0,0.0),M(0.0,C2,0.0),M(0.0,-C2,0.0),M(0.5,C0,C1),M(0.5,C0,-C1),M(0.5,-C0,C1),M(0.5,-C0,-C1),M(-0.5,C0,C1),M(-0.5,C0,-C1),M(-0.5,-C0,C1),M(-0.5,-C0,-C1),M(C1,0.5,C0),M(C1,0.5,-C0),M(C1,-0.5,C0),M(C1,-0.5,-C0),M(-C1,0.5,C0),M(-C1,0.5,-C0),M(-C1,-0.5,C0),M(-C1,-0.5,-C0),M(C0,C1,0.5),M(C0,C1,-0.5),M(C0,-C1,0.5),M(C0,-C1,-0.5),M(-C0,C1,0.5),M(-C0,C1,-0.5),M(-C0,-C1,0.5),M(-C0,-C1,-0.5)}
    L = transf(L, Origin, L[1], C, S)
    T.vertices = L
    T.facets = {{1,9,17,15,7},{1,11,19,21,13},{2,8,16,18,10},{2,14,22,20,12},{3,16,24,23,15},{3,17,25,26,18},{4,19,27,28,20},{4,22,30,29,21},{5,24,8,12,28},{5,27,11,7,23},{6,25,9,13,29},{6,30,14,10,26},{1,7,11},{1,13,9},{2,10,14},{2,12,8},{3,15,17},{3,18,16},{4,20,22},{4,21,19},{5,23,24},{5,28,27},{6,26,25},{6,29,30},{7,15,23},{8,24,16},{9,25,17},{10,18,26},{11,27,19},{12,20,28},{13,21,29},{14,30,22}}
    if all then
        local A = facetedges( poly2facet(T) ) 
        local n = #T.facets
        return T, T.vertices, A, getfacet(T,range(1,12)), getfacet(T,range(13,n)) -- polyèdre, sommets, arêtes, faces type 1, faces type 2
    else
        return T
    end
end

function lsnubcube(C,S,all)
-- de centre C de sommet S
-- si all vaut true, on renvoie dans cet ordre : 
-- le polyèdre, les sommets, les arêtes (ligne polygonale), les facettes (liste) avec points 3d 
-- si all vaut false (valeur par défaut) on renvoie seulement le polyèdre
    all = all or false
    local C0  =  math.sqrt(3 * (4 - cbrt(17 + 3*math.sqrt(33)) - cbrt(17 - 3*math.sqrt(33)))) / 6
    local C1  =  math.sqrt(3 * (2 + cbrt(17 + 3*math.sqrt(33)) + cbrt(17 - 3*math.sqrt(33)))) / 6
    local C2  =  math.sqrt(3 * (4 + cbrt(199 + 3*math.sqrt(33)) + cbrt(199 - 3*math.sqrt(33)))) / 6
    local T = {}
    local L = {M(C1,C0,C2),M(C1,-C0,-C2),M(-C1,-C0,C2),M(-C1,C0,-C2),M(C2,C1,C0),M(C2,-C1,-C0),M(-C2,-C1,C0),M(-C2,C1,-C0),M(C0,C2,C1),M(C0,-C2,-C1),M(-C0,-C2,C1),M(-C0,C2,-C1),M(C0,-C1,C2),M(C0,C1,-C2),M(-C0,C1,C2),M(-C0,-C1,-C2),M(C2,-C0,C1),M(C2,C0,-C1),M(-C2,C0,C1),M(-C2,-C0,-C1),M(C1,-C2,C0),M(C1,C2,-C0),M(-C1,C2,C0),M(-C1,-C2,-C0)}
    L = transf(L, Origin, L[1], C, S)
    T.vertices = L
    T.facets = {{3,13,1,15},{4,14,2,16},{5,17,6,18},{8,20,7,19},{9,22,12,23},{10,21,11,24},{1,9,15},{2,10,16},{3,11,13},{4,12,14},{5,1,17},{6,2,18},{7,3,19},{8,4,20},{9,5,22},{10,6,21},{11,7,24},{12,8,23},{13,17,1},{14,18,2},{15,19,3},{16,20,4},{17,21,6},{18,22,5},{19,23,8},{20,24,7},{21,13,11},{22,14,12},{23,15,9},{24,16,10},{9,1,5},{10,2,6},{11,3,7},{12,4,8},{13,21,17},{14,22,18},{15,23,19},{16,24,20}}
    if all then
        local A = facetedges( poly2facet(T) ) 
        local n = #T.facets
        return T, T.vertices, A, getfacet(T,range(1,6)), getfacet(T,range(7,n)) -- polyèdre, sommets, arêtes, faces type 1, faces type 2
    else
        return T
    end
end

function lsnubdodecahedron(C,S,all)
-- de centre C de sommet S
-- si all vaut true, on renvoie dans cet ordre : 
-- le polyèdre, les sommets, les arêtes (ligne polygonale), les facettes (liste) avec points 3d 
-- si all vaut false (valeur par défaut) on renvoie seulement le polyèdre
    all = all or false
    local C0   =  0.192893711352359022108262546061
    local C1   =  0.330921024729844230963655269187
    local C2   =  0.374821658114562295266609516608
    local C3   =  0.567715369466921317374872062669
    local C4   =  0.643029605914072573107464141441
    local C5   =  0.728335176957191477360671629838
    local C6   =  0.847550046789060797396217956030
    local C7   =  1.103156835071753772627281146446
    local C8   =  1.24950378846302719500774109632
    local C9   =  1.41526541625598211477109001870
    local C10  =  1.45402422933801541929649491091
    local C11  =  1.64691794069037444140475745697
    local C12  =  1.74618644098582634573474528789
    local C13  =  1.97783896542021867236841272616
    local C14  =  2.097053835252087992403959052348
    local phi  =  (1 + math.sqrt(5)) / 2
    local x  =  cbrt((phi + math.sqrt(phi-5/27))/2) + cbrt((phi - math.sqrt(phi-5/27))/2)
    local C0   =  phi * math.sqrt(3 - (x^2)) / 2
    local C1   =  x * phi * math.sqrt(3 - (x^2)) / 2
    local C2   =  phi * math.sqrt((x - 1 - (1/x)) * phi) / 2
    local C3   =  (x^2) * phi * math.sqrt(3 - (x^2)) / 2
    local C4   =  x * phi * math.sqrt((x - 1 - (1/x)) * phi) / 2
    local C5   =  phi * math.sqrt(1 - x + (1 + phi) / x) / 2
    local C6   =  phi * math.sqrt(x + 1 - phi) / 2
    local C7   =  (x^2) * phi * math.sqrt((x - 1 - (1/x)) * phi) / 2
    local C8   =  x * phi * math.sqrt(1 - x + (1 + phi) / x) / 2
    local C9   =  math.sqrt((x + 2) * phi + 2) / 2
    local C10  =  x * math.sqrt(x * (1 + phi) - phi) / 2
    local C11  =  math.sqrt((x^2) * (1 + 2 * phi) - phi) / 2
    local C12  =  phi * math.sqrt((x^2) + x) / 2
    local C13  =  (phi^2) * math.sqrt(x * (x + phi) + 1) / (2 * x)
    local C14  =  phi * math.sqrt(x * (x + phi) + 1) / 2
    local T = {}
    local L = {M(C2,-C1,C14),M(C2,C1,-C14),M(-C2,C1,C14),M(-C2,-C1,-C14),M(C14,-C2,C1),M(C14,C2,-C1),M(-C14,C2,C1),M(-C14,-C2,-C1),M(C1,-C14,C2),M(C1,C14,-C2),M(-C1,C14,C2),M(-C1,-C14,-C2),M(C3,C4,C13),M(C3,-C4,-C13),M(-C3,-C4,C13),M(-C3,C4,-C13),M(C13,C3,C4),M(C13,-C3,-C4),M(-C13,-C3,C4),M(-C13,C3,-C4),M(C4,C13,C3),M(C4,-C13,-C3),M(-C4,-C13,C3),M(-C4,C13,-C3),M(C0,-C8,C12),M(C0,C8,-C12),M(-C0,C8,C12),M(-C0,-C8,-C12),M(C12,-C0,C8),M(C12,C0,-C8),M(-C12,C0,C8),M(-C12,-C0,-C8),M(C8,-C12,C0),M(C8,C12,-C0),M(-C8,C12,C0),M(-C8,-C12,-C0),M(C7,-C6,C11),M(C7,C6,-C11),M(-C7,C6,C11),M(-C7,-C6,-C11),M(C11,-C7,C6),M(C11,C7,-C6),M(-C11,C7,C6),M(-C11,-C7,-C6),M(C6,-C11,C7),M(C6,C11,-C7),M(-C6,C11,C7),M(-C6,-C11,-C7),M(C9,C5,C10),M(C9,-C5,-C10),M(-C9,-C5,C10),M(-C9,C5,-C10),M(C10,C9,C5),M(C10,-C9,-C5),M(-C10,-C9,C5),M(-C10,C9,-C5),M(C5,C10,C9),M(C5,-C10,-C9),M(-C5,-C10,C9),M(-C5,C10,-C9)}
    L = transf(L, Origin, L[1], C, S)
    T.vertices = L
    T.facets = {{1,37,29,49,13},{2,38,30,50,14},{3,39,31,51,15},{4,40,32,52,16},{5,41,33,54,18},{6,42,34,53,17},{7,43,35,56,20},{8,44,36,55,19},{9,45,25,59,23},{10,46,26,60,24},{11,47,27,57,21},{12,48,28,58,22},{1,3,15},{2,4,16},{3,1,13},{4,2,14},{5,6,17},{6,5,18},{7,8,19},{8,7,20},{9,12,22},{10,11,21},{11,10,24},{12,9,23},{13,49,57},{14,50,58},{15,51,59},{16,52,60},{17,53,49},{18,54,50},{19,55,51},{20,56,52},{21,57,53},{22,58,54},{23,59,55},{24,60,56},{25,45,37},{26,46,38},{27,47,39},{28,48,40},{29,37,41},{30,38,42},{31,39,43},{32,40,44},{33,41,45},{34,42,46},{35,43,47},{36,44,48},{37,1,25},{38,2,26},{39,3,27},{40,4,28},{41,5,29},{42,6,30},{43,7,31},{44,8,32},{45,9,33},{46,10,34},{47,11,35},{48,12,36},{49,29,17},{50,30,18},{51,31,19},{52,32,20},{53,34,21},{54,33,22},{55,36,23},{56,35,24},{57,27,13},{58,28,14},{59,25,15},{60,26,16},{25,1,15},{26,2,16},{27,3,13},{28,4,14},{29,5,17},{30,6,18},{31,7,19},{32,8,20},{33,9,22},{34,10,21},{35,11,24},{36,12,23},{37,45,41},{38,46,42},{39,47,43},{40,48,44},{49,53,57},{50,54,58},{51,55,59},{52,56,60}}
     if all then
        local A = facetedges( poly2facet(T) ) 
        local n = #T.facets
        return T, T.vertices, A, getfacet(T,range(1,12)), getfacet(T,range(13,n)) -- polyèdre, sommets, arêtes, faces type 1, faces type 2
    else
        return T
    end
end

function rhombicosidodecahedron(C,S,all)
-- de centre C de sommet S
-- si all vaut true, on renvoie dans cet ordre : 
-- le polyèdre, les sommets, les arêtes (ligne polygonale), les facettes (liste) avec points 3d 
-- si all vaut false (valeur par défaut) on renvoie seulement le polyèdre
    all = all or false
    local C0  =  (1 + math.sqrt(5)) / 4
    local C1  =  (3 + math.sqrt(5)) / 4
    local C2  =  (1 + math.sqrt(5)) / 2
    local C3  =  (5 + math.sqrt(5)) / 4
    local C4  =  (2 + math.sqrt(5)) / 2
    local T = {}
    local L = {M(0.5,0.5,C4),M(0.5,0.5,-C4),M(0.5,-0.5,C4),M(0.5,-0.5,-C4),M(-0.5,0.5,C4),M(-0.5,0.5,-C4),M(-0.5,-0.5,C4),M(-0.5,-0.5,-C4),M(C4,0.5,0.5),M(C4,0.5,-0.5),M(C4,-0.5,0.5),M(C4,-0.5,-0.5),M(-C4,0.5,0.5),M(-C4,0.5,-0.5),M(-C4,-0.5,0.5),M(-C4,-0.5,-0.5),M(0.5,C4,0.5),M(0.5,C4,-0.5),M(0.5,-C4,0.5),M(0.5,-C4,-0.5),M(-0.5,C4,0.5),M(-0.5,C4,-0.5),M(-0.5,-C4,0.5),M(-0.5,-C4,-0.5),M(0.0,C1,C3),M(0.0,C1,-C3),M(0.0,-C1,C3),M(0.0,-C1,-C3),M(C3,0.0,C1),M(C3,0.0,-C1),M(-C3,0.0,C1),M(-C3,0.0,-C1),M(C1,C3,0.0),M(C1,-C3,0.0),M(-C1,C3,0.0),M(-C1,-C3,0.0),M(C1,C0,C2),M(C1,C0,-C2),M(C1,-C0,C2),M(C1,-C0,-C2),M(-C1,C0,C2),M(-C1,C0,-C2),M(-C1,-C0,C2),M(-C1,-C0,-C2),M(C2,C1,C0),M(C2,C1,-C0),M(C2,-C1,C0),M(C2,-C1,-C0),M(-C2,C1,C0),M(-C2,C1,-C0),M(-C2,-C1,C0),M(-C2,-C1,-C0),M(C0,C2,C1),M(C0,C2,-C1),M(C0,-C2,C1),M(C0,-C2,-C1),M(-C0,C2,C1),M(-C0,C2,-C1),M(-C0,-C2,C1),M(-C0,-C2,-C1)}
    L = transf(L, Origin, L[1], C, S)
    T.vertices = L
    T.facets = {{25,53,17,21,57},{26,58,22,18,54},{27,59,23,19,55},{28,56,20,24,60},{29,37,1,3,39},{30,40,4,2,38},{31,43,7,5,41},{32,42,6,8,44},{33,45,9,10,46},{34,48,12,11,47},{35,50,14,13,49},{36,51,15,16,52},{1,37,53,25},{2,26,54,38},{3,27,55,39},{4,40,56,28},{5,25,57,41},{6,42,58,26},{7,43,59,27},{8,28,60,44},{9,45,37,29},{10,30,38,46},{11,29,39,47},{12,48,40,30},{13,31,41,49},{14,50,42,32},{15,51,43,31},{16,32,44,52},{17,53,45,33},{18,33,46,54},{19,34,47,55},{20,56,48,34},{21,35,49,57},{22,58,50,35},{23,59,51,36},{24,36,52,60},{1,5,7,3},{2,4,8,6},{9,11,12,10},{13,14,16,15},{17,18,22,21},{19,23,24,20},{25,5,1},{26,2,6},{27,3,7},{28,8,4},{29,11,9},{30,10,12},{31,13,15},{32,16,14},{33,18,17},{34,19,20},{35,21,22},{36,24,23},{37,45,53},{38,54,46},{39,55,47},{40,48,56},{41,57,49},{42,50,58},{43,51,59},{44,60,52}}
     if all then
        local A = facetedges( poly2facet(T) ) 
        local n = #T.facets
        return T, T.vertices, A, getfacet(T,range(1,12)), getfacet(T,range(13,42)), getfacet(T,range(43,n)) -- polyèdre, sommets, arêtes, faces type 1, faces type 2
    else
        return T
    end
end

function rhombicuboctahedron(C,S,all)
-- de centre C de sommet S
-- si all vaut true, on renvoie dans cet ordre : 
-- le polyèdre, les sommets, les arêtes (ligne polygonale), les facettes (liste) avec points 3d 
-- si all vaut false (valeur par défaut) on renvoie seulement le polyèdre
    all = all or false
    local C0  =  (1 + math.sqrt(2)) / 2
    local T = {}
    local L = {M(0.5,0.5,C0),M(0.5,0.5,-C0),M(0.5,-0.5,C0),M(0.5,-0.5,-C0),M(-0.5,0.5,C0),M(-0.5,0.5,-C0),M(-0.5,-0.5,C0),M(-0.5,-0.5,-C0),M(C0,0.5,0.5),M(C0,0.5,-0.5),M(C0,-0.5,0.5),M(C0,-0.5,-0.5),M(-C0,0.5,0.5),M(-C0,0.5,-0.5),M(-C0,-0.5,0.5),M(-C0,-0.5,-0.5),M(0.5,C0,0.5),M(0.5,C0,-0.5),M(0.5,-C0,0.5),M(0.5,-C0,-0.5),M(-0.5,C0,0.5),M(-0.5,C0,-0.5),M(-0.5,-C0,0.5),M(-0.5,-C0,-0.5)}
    L = transf(L, Origin, L[1], C, S)
    T.vertices = L
    T.facets = {{1,5,7,3},{2,4,8,6},{9,11,12,10},{13,14,16,15},{17,18,22,21},{19,23,24,20},{1,3,11,9},{1,17,21,5},{8,4,20,24},{8,16,14,6},{12,4,2,10},{12,11,19,20},{13,15,7,5},{13,21,22,14},{18,2,6,22},{18,17,9,10},{23,15,16,24},{23,19,3,7},{1,9,17},{2,18,10},{3,19,11},{4,12,20},{5,21,13},{6,14,22},{7,15,23},{8,24,16}}
     if all then
        local A = facetedges( poly2facet(T) ) 
        local n = #T.facets
        return T, T.vertices, A, getfacet(T,range(1,18)), getfacet(T,range(19,n)) -- polyèdre, sommets, arêtes, faces type 1, faces type 2
    else
        return T
    end
end

function rsnubcube(C,S,all)
-- de centre C de sommet S
-- si all vaut true, on renvoie dans cet ordre : 
-- le polyèdre, les sommets, les arêtes (ligne polygonale), les facettes (liste) avec points 3d 
-- si all vaut false (valeur par défaut) on renvoie seulement le polyèdre
    all = all or false
    local C0  =  math.sqrt(3 * (4 - cbrt(17 + 3*math.sqrt(33)) - cbrt(17 - 3*math.sqrt(33)))) / 6
    local C1  =  math.sqrt(3 * (2 + cbrt(17 + 3*math.sqrt(33)) + cbrt(17 - 3*math.sqrt(33)))) / 6
    local C2  =  math.sqrt(3 * (4 + cbrt(199 + 3*math.sqrt(33)) + cbrt(199 - 3*math.sqrt(33)))) / 6
    local T = {}
    local L = {M(C1,-C0,C2),M(C1,C0,-C2),M(-C1,C0,C2),M(-C1,-C0,-C2),M(C2,-C1,C0),M(C2,C1,-C0),M(-C2,C1,C0),M(-C2,-C1,-C0),M(C0,-C2,C1),M(C0,C2,-C1),M(-C0,C2,C1),M(-C0,-C2,-C1),M(C0,C1,C2),M(C0,-C1,-C2),M(-C0,-C1,C2),M(-C0,C1,-C2),M(C2,C0,C1),M(C2,-C0,-C1),M(-C2,-C0,C1),M(-C2,C0,-C1),M(C1,C2,C0),M(C1,-C2,-C0),M(-C1,-C2,C0),M(-C1,C2,-C0)}
    L = transf(L, Origin, L[1], C, S)
    T.vertices = L
    T.facets = {{3,15,1,13},{4,16,2,14},{5,18,6,17},{8,19,7,20},{9,23,12,22},{10,24,11,21},{1,15,9},{2,16,10},{3,13,11},{4,14,12},{5,17,1},{6,18,2},{7,19,3},{8,20,4},{9,22,5},{10,21,6},{11,24,7},{12,23,8},{13,1,17},{14,2,18},{15,3,19},{16,4,20},{17,6,21},{18,5,22},{19,8,23},{20,7,24},{21,11,13},{22,12,14},{23,9,15},{24,10,16},{9,5,1},{10,6,2},{11,7,3},{12,8,4},{13,17,21},{14,18,22},{15,19,23},{16,20,24}}
     if all then
        local A = facetedges( poly2facet(T) ) 
        local n = #T.facets
        return T, T.vertices, A, getfacet(T,range(1,6)), getfacet(T,range(7,n)) -- polyèdre, sommets, arêtes, faces type 1, faces type 2
    else
        return T
    end
end

function rsnubdodecahedron(C,S,all)
-- de centre C de sommet S
-- si all vaut true, on renvoie dans cet ordre : 
-- le polyèdre, les sommets, les arêtes (ligne polygonale), les facettes (liste) avec points 3d 
-- si all vaut false (valeur par défaut) on renvoie seulement le polyèdre
    all = all or false
    local phi  =  (1 + math.sqrt(5)) / 2
    local x  =  cbrt((phi + math.sqrt(phi-5/27))/2) + cbrt((phi - math.sqrt(phi-5/27))/2)
    local C0   =  phi * math.sqrt(3 - (x^2)) / 2
    local C1   =  x * phi * math.sqrt(3 - (x^2)) / 2
    local C2   =  phi * math.sqrt((x - 1 - (1/x)) * phi) / 2
    local C3   =  (x^2) * phi * math.sqrt(3 - (x^2)) / 2
    local C4   =  x * phi * math.sqrt((x - 1 - (1/x)) * phi) / 2
    local C5   =  phi * math.sqrt(1 - x + (1 + phi) / x) / 2
    local C6   =  phi * math.sqrt(x + 1 - phi) / 2
    local C7   =  (x^2) * phi * math.sqrt((x - 1 - (1/x)) * phi) / 2
    local C8   =  x * phi * math.sqrt(1 - x + (1 + phi) / x) / 2
    local C9   =  math.sqrt((x + 2) * phi + 2) / 2
    local C10  =  x * math.sqrt(x * (1 + phi) - phi) / 2
    local C11  =  math.sqrt((x^2) * (1 + 2 * phi) - phi) / 2
    local C12  =  phi * math.sqrt((x^2) + x) / 2
    local C13  =  (phi^2) * math.sqrt(x * (x + phi) + 1) / (2 * x)
    local C14  =  phi * math.sqrt(x * (x + phi) + 1) / 2
    local T = {}
    local L = {M(C2,C1,C14),M(C2,-C1,-C14),M(-C2,-C1,C14),M(-C2,C1,-C14),M(C14,C2,C1),M(C14,-C2,-C1),M(-C14,-C2,C1),M(-C14,C2,-C1),M(C1,C14,C2),M(C1,-C14,-C2),M(-C1,-C14,C2),M(-C1,C14,-C2),M(C3,-C4,C13),M(C3,C4,-C13),M(-C3,C4,C13),M(-C3,-C4,-C13),M(C13,-C3,C4),M(C13,C3,-C4),M(-C13,C3,C4),M(-C13,-C3,-C4),M(C4,-C13,C3),M(C4,C13,-C3),M(-C4,C13,C3),M(-C4,-C13,-C3),M(C0,C8,C12),M(C0,-C8,-C12),M(-C0,-C8,C12),M(-C0,C8,-C12),M(C12,C0,C8),M(C12,-C0,-C8),M(-C12,-C0,C8),M(-C12,C0,-C8),M(C8,C12,C0),M(C8,-C12,-C0),M(-C8,-C12,C0),M(-C8,C12,-C0),M(C7,C6,C11),M(C7,-C6,-C11),M(-C7,-C6,C11),M(-C7,C6,-C11),M(C11,C7,C6),M(C11,-C7,-C6),M(-C11,-C7,C6),M(-C11,C7,-C6),M(C6,C11,C7),M(C6,-C11,-C7),M(-C6,-C11,C7),M(-C6,C11,-C7),M(C9,-C5,C10),M(C9,C5,-C10),M(-C9,C5,C10),M(-C9,-C5,-C10),M(C10,-C9,C5),M(C10,C9,-C5),M(-C10,C9,C5),M(-C10,-C9,-C5),M(C5,-C10,C9),M(C5,C10,-C9),M(-C5,C10,C9),M(-C5,-C10,-C9)}
    L = transf(L, Origin, L[1], C, S)
    T.vertices = L
    T.facets = {{1,13,49,29,37},{2,14,50,30,38},{3,15,51,31,39},{4,16,52,32,40},{5,18,54,33,41},{6,17,53,34,42},{7,20,56,35,43},{8,19,55,36,44},{9,23,59,25,45},{10,24,60,26,46},{11,21,57,27,47},{12,22,58,28,48},{1,15,3},{2,16,4},{3,13,1},{4,14,2},{5,17,6},{6,18,5},{7,19,8},{8,20,7},{9,22,12},{10,21,11},{11,24,10},{12,23,9},{13,57,49},{14,58,50},{15,59,51},{16,60,52},{17,49,53},{18,50,54},{19,51,55},{20,52,56},{21,53,57},{22,54,58},{23,55,59},{24,56,60},{25,37,45},{26,38,46},{27,39,47},{28,40,48},{29,41,37},{30,42,38},{31,43,39},{32,44,40},{33,45,41},{34,46,42},{35,47,43},{36,48,44},{37,25,1},{38,26,2},{39,27,3},{40,28,4},{41,29,5},{42,30,6},{43,31,7},{44,32,8},{45,33,9},{46,34,10},{47,35,11},{48,36,12},{49,17,29},{50,18,30},{51,19,31},{52,20,32},{53,21,34},{54,22,33},{55,23,36},{56,24,35},{57,13,27},{58,14,28},{59,15,25},{60,16,26},{25,15,1},{26,16,2},{27,13,3},{28,14,4},{29,17,5},{30,18,6},{31,19,7},{32,20,8},{33,22,9},{34,21,10},{35,24,11},{36,23,12},{37,41,45},{38,42,46},{39,43,47},{40,44,48},{49,57,53},{50,58,54},{51,59,55},{52,60,56}}
     if all then
        local A = facetedges( poly2facet(T) ) 
        local n = #T.facets
        return T, T.vertices, A, getfacet(T,range(1,12)), getfacet(T,range(13,n)) -- polyèdre, sommets, arêtes, faces type 1, faces type 2
    else
        return T
    end
end

function truncatedcube(C,S,all)
-- de centre C de sommet S
-- si all vaut true, on renvoie dans cet ordre : 
-- le polyèdre, les sommets, les arêtes (ligne polygonale), les facettes (liste) avec points 3d 
-- si all vaut false (valeur par défaut) on renvoie seulement le polyèdre
    all = all or false
    local C0  =  (1 + math.sqrt(2)) / 2
    local T = {}
    local L = {M(C0,0.5,C0),M(C0,0.5,-C0),M(C0,-0.5,C0),M(C0,-0.5,-C0),M(-C0,0.5,C0),M(-C0,0.5,-C0),M(-C0,-0.5,C0),M(-C0,-0.5,-C0),M(C0,C0,0.5),M(C0,C0,-0.5),M(C0,-C0,0.5),M(C0,-C0,-0.5),M(-C0,C0,0.5),M(-C0,C0,-0.5),M(-C0,-C0,0.5),M(-C0,-C0,-0.5),M(0.5,C0,C0),M(0.5,C0,-C0),M(0.5,-C0,C0),M(0.5,-C0,-C0),M(-0.5,C0,C0),M(-0.5,C0,-C0),M(-0.5,-C0,C0),M(-0.5,-C0,-C0)}
    L = transf(L, Origin, L[1], C, S)
    T.vertices = L
    T.facets = {{1,3,11,12,4,2,10,9},{1,17,21,5,7,23,19,3},{13,14,6,8,16,15,7,5},{13,21,17,9,10,18,22,14},{20,24,8,6,22,18,2,4},{20,12,11,19,23,15,16,24},{1,9,17},{2,18,10},{3,19,11},{4,12,20},{5,21,13},{6,14,22},{7,15,23},{8,24,16}}
     if all then
        local A = facetedges( poly2facet(T) ) 
        local n = #T.facets
        return T, T.vertices, A, getfacet(T,range(1,6)), getfacet(T,range(7,n)) -- polyèdre, sommets, arêtes, faces type 1, faces type 2
    else
        return T
    end
end

function truncatedcuboctahedron(C,S,all)
-- de centre C de sommet S
-- si all vaut true, on renvoie dans cet ordre : 
-- le polyèdre, les sommets, les arêtes (ligne polygonale), les facettes (liste) avec points 3d 
-- si all vaut false (valeur par défaut) on renvoie seulement le polyèdre
    all = all or false
    local C0  =  (1 + math.sqrt(2)) / 2
    local C1  =  (1 + 2 * math.sqrt(2)) / 2
    local T = {}
    local L = {M(C0,0.5,C1),M(C0,0.5,-C1),M(C0,-0.5,C1),M(C0,-0.5,-C1),M(-C0,0.5,C1),M(-C0,0.5,-C1),M(-C0,-0.5,C1),M(-C0,-0.5,-C1),M(C1,C0,0.5),M(C1,C0,-0.5),M(C1,-C0,0.5),M(C1,-C0,-0.5),M(-C1,C0,0.5),M(-C1,C0,-0.5),M(-C1,-C0,0.5),M(-C1,-C0,-0.5),M(0.5,C1,C0),M(0.5,C1,-C0),M(0.5,-C1,C0),M(0.5,-C1,-C0),M(-0.5,C1,C0),M(-0.5,C1,-C0),M(-0.5,-C1,C0),M(-0.5,-C1,-C0),M(0.5,C0,C1),M(0.5,C0,-C1),M(0.5,-C0,C1),M(0.5,-C0,-C1),M(-0.5,C0,C1),M(-0.5,C0,-C1),M(-0.5,-C0,C1),M(-0.5,-C0,-C1),M(C1,0.5,C0),M(C1,0.5,-C0),M(C1,-0.5,C0),M(C1,-0.5,-C0),M(-C1,0.5,C0),M(-C1,0.5,-C0),M(-C1,-0.5,C0),M(-C1,-0.5,-C0),M(C0,C1,0.5),M(C0,C1,-0.5),M(C0,-C1,0.5),M(C0,-C1,-0.5),M(-C0,C1,0.5),M(-C0,C1,-0.5),M(-C0,-C1,0.5),M(-C0,-C1,-0.5)}
    L = transf(L, Origin, L[1], C, S)
    T.vertices = L
    T.facets = {{1,25,29,5,7,31,27,3},{2,4,28,32,8,6,30,26},{9,33,35,11,12,36,34,10},{13,14,38,40,16,15,39,37},{17,41,42,18,22,46,45,21},{19,23,47,48,24,20,44,43},{1,33,9,41,17,25},{2,26,18,42,10,34},{3,27,19,43,11,35},{4,36,12,44,20,28},{5,29,21,45,13,37},{6,38,14,46,22,30},{7,39,15,47,23,31},{8,32,24,48,16,40},{1,3,35,33},{2,34,36,4},{5,37,39,7},{6,8,40,38},{9,10,42,41},{11,43,44,12},{13,45,46,14},{15,16,48,47},{17,21,29,25},{18,26,30,22},{19,27,31,23},{20,24,32,28}}
     if all then
        local A = facetedges( poly2facet(T) ) 
        local n = #T.facets
        return T, T.vertices, A, getfacet(T,range(1,6)), getfacet(T,range(7,14)), getfacet(T,range(15,n)) -- polyèdre, sommets, arêtes, faces type 1, faces type 2
    else
        return T
    end
end

function truncateddodecahedron(C,S,all)
-- de centre C de sommet S
-- si all vaut true, on renvoie dans cet ordre : 
-- le polyèdre, les sommets, les arêtes (ligne polygonale), les facettes (liste) avec points 3d 
-- si all vaut false (valeur par défaut) on renvoie seulement le polyèdre
    all = all or false
    local C0  =  (3 + math.sqrt(5)) / 4
    local C1  =  (1 + math.sqrt(5)) / 2
    local C2  =  (2 + math.sqrt(5)) / 2
    local C3  =  (3 + math.sqrt(5)) / 2
    local C4  =  (5 + 3 * math.sqrt(5)) / 4
    local T = {}
    local L = {M(0.0,0.5,C4),M(0.0,0.5,-C4),M(0.0,-0.5,C4),M(0.0,-0.5,-C4),M(C4,0.0,0.5),M(C4,0.0,-0.5),M(-C4,0.0,0.5),M(-C4,0.0,-0.5),M(0.5,C4,0.0),M(0.5,-C4,0.0),M(-0.5,C4,0.0),M(-0.5,-C4,0.0),M(0.5,C0,C3),M(0.5,C0,-C3),M(0.5,-C0,C3),M(0.5,-C0,-C3),M(-0.5,C0,C3),M(-0.5,C0,-C3),M(-0.5,-C0,C3),M(-0.5,-C0,-C3),M(C3,0.5,C0),M(C3,0.5,-C0),M(C3,-0.5,C0),M(C3,-0.5,-C0),M(-C3,0.5,C0),M(-C3,0.5,-C0),M(-C3,-0.5,C0),M(-C3,-0.5,-C0),M(C0,C3,0.5),M(C0,C3,-0.5),M(C0,-C3,0.5),M(C0,-C3,-0.5),M(-C0,C3,0.5),M(-C0,C3,-0.5),M(-C0,-C3,0.5),M(-C0,-C3,-0.5),M(C0,C1,C2),M(C0,C1,-C2),M(C0,-C1,C2),M(C0,-C1,-C2),M(-C0,C1,C2),M(-C0,C1,-C2),M(-C0,-C1,C2),M(-C0,-C1,-C2),M(C2,C0,C1),M(C2,C0,-C1),M(C2,-C0,C1),M(C2,-C0,-C1),M(-C2,C0,C1),M(-C2,C0,-C1),M(-C2,-C0,C1),M(-C2,-C0,-C1),M(C1,C2,C0),M(C1,C2,-C0),M(C1,-C2,C0),M(C1,-C2,-C0),M(-C1,C2,C0),M(-C1,C2,-C0),M(-C1,-C2,C0),M(-C1,-C2,-C0)}
    L = transf(L, Origin, L[1], C, S)
    T.vertices = L
    T.facets = {{1,3,15,39,47,23,21,45,37,13},{2,4,20,44,52,28,26,50,42,18},{3,1,17,41,49,25,27,51,43,19},{4,2,14,38,46,22,24,48,40,16},{5,6,22,46,54,30,29,53,45,21},{6,5,23,47,55,31,32,56,48,24},{7,8,28,52,60,36,35,59,51,27},{8,7,25,49,57,33,34,58,50,26},{9,11,33,57,41,17,13,37,53,29},{10,12,36,60,44,20,16,40,56,32},{11,9,30,54,38,14,18,42,58,34},{12,10,31,55,39,15,19,43,59,35},{1,13,17},{2,18,14},{3,19,15},{4,16,20},{5,21,23},{6,24,22},{7,27,25},{8,26,28},{9,29,30},{10,32,31},{11,34,33},{12,35,36},{37,45,53},{38,54,46},{39,55,47},{40,48,56},{41,57,49},{42,50,58},{43,51,59},{44,60,52}}
     if all then
        local A = facetedges( poly2facet(T) ) 
        local n = #T.facets
        return T, T.vertices, A, getfacet(T,range(1,12)), getfacet(T,range(13,n)) -- polyèdre, sommets, arêtes, faces type 1, faces type 2
    else
        return T
    end
end

function truncatedicosahedron(C,S,all)
-- de centre C de sommet S
-- si all vaut true, on renvoie dans cet ordre : 
-- le polyèdre, les sommets, les arêtes (ligne polygonale), les facettes (liste) avec points 3d 
-- si all vaut false (valeur par défaut) on renvoie seulement le polyèdre
    all = all or false
    local C0  =  (1 + math.sqrt(5)) / 4
    local C1  =  (1 + math.sqrt(5)) / 2
    local C2  =  (5 + math.sqrt(5)) / 4
    local C3  =  (2 + math.sqrt(5)) / 2
    local C4  =  3 * (1 + math.sqrt(5)) / 4
    local T = {}
    local L = {M(0.5,0.0,C4),M(0.5,0.0,-C4),M(-0.5,0.0,C4),M(-0.5,0.0,-C4),M(C4,0.5,0.0),M(C4,-0.5,0.0),M(-C4,0.5,0.0),M(-C4,-0.5,0.0),M(0.0,C4,0.5),M(0.0,C4,-0.5),M(0.0,-C4,0.5),M(0.0,-C4,-0.5),M(1.0,C0,C3),M(1.0,C0,-C3),M(1.0,-C0,C3),M(1.0,-C0,-C3),M(-1.0,C0,C3),M(-1.0,C0,-C3),M(-1.0,-C0,C3),M(-1.0,-C0,-C3),M(C3,1.0,C0),M(C3,1.0,-C0),M(C3,-1.0,C0),M(C3,-1.0,-C0),M(-C3,1.0,C0),M(-C3,1.0,-C0),M(-C3,-1.0,C0),M(-C3,-1.0,-C0),M(C0,C3,1.0),M(C0,C3,-1.0),M(C0,-C3,1.0),M(C0,-C3,-1.0),M(-C0,C3,1.0),M(-C0,C3,-1.0),M(-C0,-C3,1.0),M(-C0,-C3,-1.0),M(0.5,C1,C2),M(0.5,C1,-C2),M(0.5,-C1,C2),M(0.5,-C1,-C2),M(-0.5,C1,C2),M(-0.5,C1,-C2),M(-0.5,-C1,C2),M(-0.5,-C1,-C2),M(C2,0.5,C1),M(C2,0.5,-C1),M(C2,-0.5,C1),M(C2,-0.5,-C1),M(-C2,0.5,C1),M(-C2,0.5,-C1),M(-C2,-0.5,C1),M(-C2,-0.5,-C1),M(C1,C2,0.5),M(C1,C2,-0.5),M(C1,-C2,0.5),M(C1,-C2,-0.5),M(-C1,C2,0.5),M(-C1,C2,-0.5),M(-C1,-C2,0.5),M(-C1,-C2,-0.5)}
    L = transf(L, Origin, L[1], C, S)
    T.vertices = L
    T.facets = {{1,3,19,43,39,15},{2,4,18,42,38,14},{3,1,13,37,41,17},{4,2,16,40,44,20},{5,6,24,48,46,22},{6,5,21,45,47,23},{7,8,27,51,49,25},{8,7,26,50,52,28},{9,10,34,58,57,33},{10,9,29,53,54,30},{11,12,32,56,55,31},{12,11,35,59,60,36},{13,45,21,53,29,37},{14,38,30,54,22,46},{15,39,31,55,23,47},{16,48,24,56,32,40},{17,41,33,57,25,49},{18,50,26,58,34,42},{19,51,27,59,35,43},{20,44,36,60,28,52},{1,15,47,45,13},{2,14,46,48,16},{3,17,49,51,19},{4,20,52,50,18},{5,22,54,53,21},{6,23,55,56,24},{7,25,57,58,26},{8,28,60,59,27},{9,33,41,37,29},{10,30,38,42,34},{11,31,39,43,35},{12,36,44,40,32}}
     if all then
        local A = facetedges( poly2facet(T) ) 
        local n = #T.facets
        return T, T.vertices, A, getfacet(T,range(1,20)), getfacet(T,range(21,n)) -- polyèdre, sommets, arêtes, faces type 1, faces type 2
    else
        return T
    end
end

function truncatedicosidodecahedron(C,S,all)
-- de centre C de sommet S
-- si all vaut true, on renvoie dans cet ordre : 
-- le polyèdre, les sommets, les arêtes (ligne polygonale), les facettes (liste) avec points 3d 
-- si all vaut false (valeur par défaut) on renvoie seulement le polyèdre
    all = all or false
    local C0  =  (3 + math.sqrt(5)) / 4
    local C1  =  (1 + math.sqrt(5)) / 2
    local C2  =  (5 + math.sqrt(5)) / 4
    local C3  =  (2 + math.sqrt(5)) / 2
    local C4  =  3 * (1 + math.sqrt(5)) / 4
    local C5  =  (3 + math.sqrt(5)) / 2
    local C6  =  (5 + 3 * math.sqrt(5)) / 4
    local C7  =  (4 + math.sqrt(5)) / 2
    local C8  =  (7 + 3 * math.sqrt(5)) / 4
    local C9  =  (3 + 2 * math.sqrt(5)) / 2
    local T = {}
    local L = {M(0.5,0.5,C9),M(0.5,0.5,-C9),M(0.5,-0.5,C9),M(0.5,-0.5,-C9),M(-0.5,0.5,C9),M(-0.5,0.5,-C9),M(-0.5,-0.5,C9),M(-0.5,-0.5,-C9),M(C9,0.5,0.5),M(C9,0.5,-0.5),M(C9,-0.5,0.5),M(C9,-0.5,-0.5),M(-C9,0.5,0.5),M(-C9,0.5,-0.5),M(-C9,-0.5,0.5),M(-C9,-0.5,-0.5),M(0.5,C9,0.5),M(0.5,C9,-0.5),M(0.5,-C9,0.5),M(0.5,-C9,-0.5),M(-0.5,C9,0.5),M(-0.5,C9,-0.5),M(-0.5,-C9,0.5),M(-0.5,-C9,-0.5),M(1.0,C0,C8),M(1.0,C0,-C8),M(1.0,-C0,C8),M(1.0,-C0,-C8),M(-1.0,C0,C8),M(-1.0,C0,-C8),M(-1.0,-C0,C8),M(-1.0,-C0,-C8),M(C8,1.0,C0),M(C8,1.0,-C0),M(C8,-1.0,C0),M(C8,-1.0,-C0),M(-C8,1.0,C0),M(-C8,1.0,-C0),M(-C8,-1.0,C0),M(-C8,-1.0,-C0),M(C0,C8,1.0),M(C0,C8,-1.0),M(C0,-C8,1.0),M(C0,-C8,-1.0),M(-C0,C8,1.0),M(-C0,C8,-1.0),M(-C0,-C8,1.0),M(-C0,-C8,-1.0),M(0.5,C3,C7),M(0.5,C3,-C7),M(0.5,-C3,C7),M(0.5,-C3,-C7),M(-0.5,C3,C7),M(-0.5,C3,-C7),M(-0.5,-C3,C7),M(-0.5,-C3,-C7),M(C7,0.5,C3),M(C7,0.5,-C3),M(C7,-0.5,C3),M(C7,-0.5,-C3),M(-C7,0.5,C3),M(-C7,0.5,-C3),M(-C7,-0.5,C3),M(-C7,-0.5,-C3),M(C3,C7,0.5),M(C3,C7,-0.5),M(C3,-C7,0.5),M(C3,-C7,-0.5),M(-C3,C7,0.5),M(-C3,C7,-0.5),M(-C3,-C7,0.5),M(-C3,-C7,-0.5),M(C2,C1,C6),M(C2,C1,-C6),M(C2,-C1,C6),M(C2,-C1,-C6),M(-C2,C1,C6),M(-C2,C1,-C6),M(-C2,-C1,C6),M(-C2,-C1,-C6),M(C6,C2,C1),M(C6,C2,-C1),M(C6,-C2,C1),M(C6,-C2,-C1),M(-C6,C2,C1),M(-C6,C2,-C1),M(-C6,-C2,C1),M(-C6,-C2,-C1),M(C1,C6,C2),M(C1,C6,-C2),M(C1,-C6,C2),M(C1,-C6,-C2),M(-C1,C6,C2),M(-C1,C6,-C2),M(-C1,-C6,C2),M(-C1,-C6,-C2),M(C0,C4,C5),M(C0,C4,-C5),M(C0,-C4,C5),M(C0,-C4,-C5),M(-C0,C4,C5),M(-C0,C4,-C5),M(-C0,-C4,C5),M(-C0,-C4,-C5),M(C5,C0,C4),M(C5,C0,-C4),M(C5,-C0,C4),M(C5,-C0,-C4),M(-C5,C0,C4),M(-C5,C0,-C4),M(-C5,-C0,C4),M(-C5,-C0,-C4),M(C4,C5,C0),M(C4,C5,-C0),M(C4,-C5,C0),M(C4,-C5,-C0),M(-C4,C5,C0),M(-C4,C5,-C0),M(-C4,-C5,C0),M(-C4,-C5,-C0)}
    L = transf(L, Origin, L[1], C, S)
    T.vertices = L
    T.facets = {{1,3,27,75,107,59,57,105,73,25},{2,26,74,106,58,60,108,76,28,4},{5,29,77,109,61,63,111,79,31,7},{6,8,32,80,112,64,62,110,78,30},{9,10,34,82,114,66,65,113,81,33},{11,35,83,115,67,68,116,84,36,12},{13,37,85,117,69,70,118,86,38,14},{15,16,40,88,120,72,71,119,87,39},{17,21,45,93,101,53,49,97,89,41},{18,42,90,98,50,54,102,94,46,22},{19,43,91,99,51,55,103,95,47,23},{20,24,48,96,104,56,52,100,92,44},{1,25,49,53,29,5},{2,6,30,54,50,26},{3,7,31,55,51,27},{4,28,52,56,32,8},{9,33,57,59,35,11},{10,12,36,60,58,34},{13,15,39,63,61,37},{14,38,62,64,40,16},{17,41,65,66,42,18},{19,20,44,68,67,43},{21,22,46,70,69,45},{23,47,71,72,48,24},{73,105,81,113,89,97},{74,98,90,114,82,106},{75,99,91,115,83,107},{76,108,84,116,92,100},{77,101,93,117,85,109},{78,110,86,118,94,102},{79,111,87,119,95,103},{80,104,96,120,88,112},{1,5,7,3},{2,4,8,6},{9,11,12,10},{13,14,16,15},{17,18,22,21},{19,23,24,20},{25,73,97,49},{26,50,98,74},{27,51,99,75},{28,76,100,52},{29,53,101,77},{30,78,102,54},{31,79,103,55},{32,56,104,80},{33,81,105,57},{34,58,106,82},{35,59,107,83},{36,84,108,60},{37,61,109,85},{38,86,110,62},{39,87,111,63},{40,64,112,88},{41,89,113,65},{42,66,114,90},{43,67,115,91},{44,92,116,68},{45,69,117,93},{46,94,118,70},{47,95,119,71},{48,72,120,96}}
     if all then
        local A = facetedges( poly2facet(T) ) 
        local n = #T.facets
        return T, T.vertices, A, getfacet(T,range(1,12)), getfacet(T,range(13,32)), getfacet(T,range(33,n)) -- polyèdre, sommets, arêtes, faces type 1, faces type 2
    else
        return T
    end
end

function truncatedoctahedron(C,S,all)
-- de centre C de sommet S
-- si all vaut true, on renvoie dans cet ordre : 
-- le polyèdre, les sommets, les arêtes (ligne polygonale), les facettes (liste) avec points 3d 
-- si all vaut false (valeur par défaut) on renvoie seulement le polyèdre
    all = all or false
    local C0  =  math.sqrt(2) / 2
    local C1  =  math.sqrt(2)
    local T = {}
    local L = {M(C0,0.0,C1),M(C0,0.0,-C1),M(-C0,0.0,C1),M(-C0,0.0,-C1),M(C1,C0,0.0),M(C1,-C0,0.0),M(-C1,C0,0.0),M(-C1,-C0,0.0),M(0.0,C1,C0),M(0.0,C1,-C0),M(0.0,-C1,C0),M(0.0,-C1,-C0),M(0.0,C0,C1),M(0.0,C0,-C1),M(0.0,-C0,C1),M(0.0,-C0,-C1),M(C1,0.0,C0),M(C1,0.0,-C0),M(-C1,0.0,C0),M(-C1,0.0,-C0),M(C0,C1,0.0),M(C0,-C1,0.0),M(-C0,C1,0.0),M(-C0,-C1,0.0)}
    L = transf(L, Origin, L[1], C, S)
    T.vertices = L
    T.facets = {{1,15,11,22,6,17},{2,14,10,21,5,18},{3,13,9,23,7,19},{4,16,12,24,8,20},{5,21,9,13,1,17},{6,22,12,16,2,18},{8,24,11,15,3,19},{7,23,10,14,4,20},{1,13,3,15},{2,16,4,14},{5,17,6,18},{7,20,8,19},{9,21,10,23},{11,24,12,22}}
     if all then
        local A = facetedges( poly2facet(T) ) 
        local n = #T.facets
        return T, T.vertices, A, getfacet(T,range(1,8)), getfacet(T,range(9,n)) -- polyèdre, sommets, arêtes, faces type 1, faces type 2
    else
        return T
    end
end

function truncatedtetrahedron(C,S,all)
-- de centre C de sommet S
-- si all vaut true, on renvoie dans cet ordre : 
-- le polyèdre, les sommets, les arêtes (ligne polygonale), les facettes (liste) avec points 3d 
-- si all vaut false (valeur par défaut) on renvoie seulement le polyèdre
    all = all or false
    local C0  =  math.sqrt(2) / 4
    local C1  =  3 * math.sqrt(2) / 4
    local T = {}
    local L = {M(C0,-C0,C1),M(C0,C0,-C1),M(-C0,C0,C1),M(-C0,-C0,-C1),M(C1,-C0,C0),M(C1,C0,-C0),M(-C1,C0,C0),M(-C1,-C0,-C0),M(C0,-C1,C0),M(C0,C1,-C0),M(-C0,C1,C0),M(-C0,-C1,-C0)}
    L = transf(L, Origin, L[1], C, S)
    T.vertices = L
    T.facets = {{1,5,6,10,11,3},{2,6,5,9,12,4},{3,7,8,12,9,1},{4,8,7,11,10,2},{1,9,5},{2,10,6},{3,11,7},{4,12,8}}
     if all then
        local A = facetedges( poly2facet(T) ) 
        local n = #T.facets
        return T, T.vertices, A, getfacet(T,range(1,4)), getfacet(T,range(5,n)) -- polyèdre, sommets, arêtes, faces type 1, faces type 2
    else
        return T
    end
end


-- fin solides d'Archimède

function octahemioctahedron(C,S,all)
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
function Octahemioctaedre(C,S,all) --compatibilité ascendante
    return octahemioctahedron(C,S,all)
end

function small_stellated_dodecahedron(Cter,S,all)
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
function PtDodecaedreEt(Cter,S,all) --compatibilité ascendante
    return small_stellated_dodecahedron(Cter,S,all)
end
