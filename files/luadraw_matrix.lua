-- luadraw__matrix.lua (chargé par luadraw__calc, qui charge avant luadraw_complex.lua)
-- date 2025/10/18
-- version 2.2
-- Copyright 2025 Patrick Fradin
-- This work may be distributed and/or modified under the
-- conditions of the LaTeX Project Public License.
-- The latest version of this license is in
--   http://www.latex-project.org/lppl.txt.

-- une matrice est une table de 3 complexes de la forme [f(0), Lf(1), Lf(i) ] où
-- f est la transformation affine représentée par la matrice et Lf sa partie linéaire.

function applymatrix(z,M) -- applique M au complexe z
    if type(z) == "number" then z = Z(z,0) end
    if z == cpx.Jump then return cpx.Jump -- le complexe Jump est laissé tel quel
    else
        if type(z) == "string" then return z -- pour traiter le cas des chemins faits de courbes de Bézier
        else
            return Z( M[1].re + z.re*M[2].re + z.im*M[3].re, M[1].im + z.re*M[2].im + z.im*M[3].im )
        end
    end
end

function applyLmatrix(z,M) -- applique la partie linéaire de M au complexe z
    if type(z) == "number" then z = Z(z,0) end
    if z == cpx.Jump then return cpx.Jump -- le complexe Jump est laissé tel quel
    else
        return Z( z.re*M[2].re + z.im*M[3].re, z.re*M[2].im + z.im*M[3].im )
    end
end

function mtransform(L,M) -- applique la matrice M à L 
-- L doit être une séquence de complexes, on calcule et renvoie
-- renvoie l'image de L sans modifier L
    if (L == nil) or (type(L) ~=  "table") then return end
    if (M == nil) or (type(M) ~= "table") or (#M ~= 3) then return end
    local res = {}
    if (type(L[1]) == "number") or isComplex(L[1]) then
        for _, z in ipairs(L) do
            table.insert(res, applymatrix(z,M) )
        end
    else  -- L est une liste de listes
        local aux = {}
        for _,cp in ipairs(L) do
            for _,z in ipairs(cp) do
                table.insert(aux, applymatrix(z,M) )
            end
            table.insert(res,aux)
        end
    end
    return res
end

function mLtransform(L,M) -- applique la partie linéaire de la matrice M à L 
-- L doit être une séquence de complexes, on calcule et renvoie
-- renvoie l'image de L sans modifier L
    if (L == nil) or (type(L) ~=  "table") then return end
    if (M == nil) or (type(M) ~= "table") or (#M ~= 3) then return end    
    local res = {}
    if (type(L[1]) == "number") or isComplex(L[1]) then
        for _, z in ipairs(L) do
            table.insert(res, applyLmatrix(z,M) )
        end
    else  -- L est une liste de listes
        local aux = {}
        for _,cp in ipairs(L) do
            for _,z in ipairs(cp) do
                table.insert(aux, applyLmatrix(z,M) )
            end
            table.insert(res,aux)
        end
    end
    return res
end

function composematrix(M1,M2)  -- fait le produit matriciel M1*M2
-- M1 et M2 sont deux tables de 3 complexes {f(0), Lf(1), Lf(i)}
-- où f est la transformation affine correspondant et Lf sa partie linéaire 
    if (M1 == nil) or (type(M1) ~= "table") or (#M1 ~= 3) then return end
    if (M2 == nil) or (type(M2) ~= "table") or (#M2 ~= 3) then return end
    local a, b, c = table.unpack(M2)
    a, b, c = toComplex(a), toComplex(b), toComplex(c)
    if (a == nil) or (b == nil) or (c== nil) then return end
    return { applymatrix(a,M1), applyLmatrix(b,M1), applyLmatrix(c,M1) }
end

function matrixof(f) -- renvoie la matrice de la fonction f
-- f doit être une fonction affine de C dans C : z -> f(z)
-- la fonction calcule et renvoie sa matrice
    local a = f(Z(0,0))
    return { a, f(Z(1,0))-a, f(Z(0,1))-a }
end

function invmatrix(M)
-- renvoie la matrice inverse (si possible) de la matrice M
    if (M == nil) or (type(M) ~= "table") or (#M ~= 3) then return end
    local c, a, b = table.unpack(M)
    a, b, c = toComplex(a), toComplex(b), toComplex(c)
    if (a == nil) or (b == nil) or (c== nil) then return end    
    local D = cpx.det(a,b)
     if (D ~= 0) then
        local A, B = Z(b.im/D,-a.im /D), Z(-b.re/D, a.re/D)
        if ( A ~= nil) and (B ~= nil) then
            return { -c.re*A - c.im*B, A, B } -- matrice inverse
        end
    end
end

function isID(m)  -- teste la matrice unité
    return (toComplex(m[1]) == Z(0,0)) and (toComplex(m[2]) == Z(1,0)) and (toComplex(m[3]) == Z(0,1))
end
