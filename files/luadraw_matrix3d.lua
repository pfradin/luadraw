-- luadraw__matrix3d.lua (chargé par luadraw__graph3d)
-- date 2025/09/07
-- version 2.1
-- Copyright 2025 Patrick Fradin
-- This work may be distributed and/or modified under the
-- conditions of the LaTeX Project Public License.
-- The latest version of this license is in
--   http://www.latex-project.org/lppl.txt.

-- une matrice 3d est une table de 4 point3d de la forme [f(0,0,0), Lf(1,0,0), Lf(0,1,0), LF(0,0,1)] où
-- f est la transformation affine représentée par la matrice et Lf sa partie linéaire.

pt3d = require 'luadraw_point3d'

function applymatrix3d(A,M) -- applique M au point A
    if isPoint3d(A) then 
        return M[1]+A.x*M[2]+A.y*M[3]+A.z*M[4]
    else
        return A
    end
end

function applyLmatrix3d(A,M) -- applique la partie linéaire de M au vecteur A
    if isPoint3d(A) then 
        return A.x*M[2]+A.y*M[3]+A.z*M[4]
    else
        return A
    end
end

function mtransform3d(L,M) -- applique la matrice M à L 
-- L doit être une liste de point3d ou une liste de listes, on calcule et renvoie
-- renvoie l'image de L sans modifier L
    if (L == nil) or (type(L) ~=  "table") then return end
    if (M == nil) or (type(M) ~= "table") or (#M ~= 4) then return end
    local res = {}
    if isPoint3d(L) then res = applymatrix3d(L,M)
    elseif isPoint3d(L[1]) then --liste de point3d
        for _, A in ipairs(L) do
            table.insert(res, applymatrix3d(A,M) )
        end
    else  -- L est une liste de listes
        local aux
        for _,cp in ipairs(L) do
            aux = {}
            for _,A in ipairs(cp) do
                table.insert(aux, applymatrix3d(A,M) )
            end
            table.insert(res,aux)
        end
    end
    return res
end

function mLtransform3d(L,M) -- applique la partie linéaire de la matrice M à L 
-- L doit être une liste de point3d ou une liste de listes, on calcule et renvoie
-- renvoie l'image de L sans modifier L
    if (L == nil) or (type(L) ~=  "table") then return end
    if (M == nil) or (type(M) ~= "table") or (#M ~= 4) then return end
    local res = {}
    if isPoint3d(L) then res = applyLmatrix3d(L,M)
    elseif isPoint3d(L[1]) then --liste de point3d
        for _, A in ipairs(L) do
            table.insert(res, applyLmatrix3d(A,M) )
        end
    else  -- L est une liste de listes
        local aux = {}
        for _,cp in ipairs(L) do
            for _,A in ipairs(cp) do
                table.insert(aux, applyLmatrix3d(A,M) )
            end
            table.insert(res,aux)
        end
    end
    return res
end

function composematrix3d(M1,M2)  -- fait le produit matriciel M1*M2
-- M1 et M2 sont deux tables de 4 complexes {f(0), Lf(i), Lf(j), Lf(k)}
-- où f est la transformation affine correspondant et Lf sa partie linéaire 
    if (M1 == nil) or (type(M1) ~= "table") or (#M1 ~= 4) then return end
    if (M2 == nil) or (type(M2) ~= "table") or (#M2 ~= 4) then return end
    local a, b, c, d = table.unpack(M2)
    return { applymatrix3d(a,M1), applyLmatrix3d(b,M1), applyLmatrix3d(c,M1), applyLmatrix3d(d,M1)}
end

function matrix3dof(f) -- renvoie la matrice de la fonction f
-- f doit être une fonction affine de R^3 dans R^3 qui a un point3d associe un point3d
-- la fonction calcule et renvoie sa matrice
    local a = f(M(0,0,0))
    return { a, f(M(1,0,0))-a, f(M(0,1,0))-a, f(M(0,0,1))-a }
end

function invmatrix3d(M1)
-- renvoie la matrice inverse (si possible) de la matrice M
    if (M1 == nil) or (type(M1) ~= "table") or (#M1 ~= 4) then return end
    local t, a, b, c = table.unpack(M1)
    local u, v, w, d = pt3d.prod(b,c), pt3d.prod(c,a), pt3d.prod(a,b), pt3d.det(a,b,c)
    if d ~= 0 then
        local a1 = M(u.x,v.x,w.x)/d
        local b1 = M(u.y,v.y,w.y)/d
        local c1 = M(u.z,v.z,w.z)/d
        return {-t.x*a1-t.y*b1-t.z*c1, a1, b1, c1}
    end
end

function isID3d(m) -- teste la matrice unité
    return (m[1] == M(0,0,0)) and (m[2] == M(1,0,0)) and (m[3] == M(0,1,0)) and (m[4] == M(0,0,1))
end
