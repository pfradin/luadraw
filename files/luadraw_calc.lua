-- luadraw__calc.lua (chargé par luadraw_graph.lua)
-- date 2026/02/17
-- version 2.6
-- Copyright 2026 Patrick Fradin
-- This work may be distributed and/or modified under the
-- conditions of the LaTeX Project Public License.
-- The latest version of this license is in
--   http://www.latex-project.org/lppl.txt.

-- module de calculs d'éléments graphiques

cpx = require 'luadraw_complex' -- classe des nombres complexes
cpx.I = Z(0,1)
cpx.Jump = Z(1e+308,0)


ID = { Z(0,0), Z(1,0), Z(0,1) } --matrice identité

local luadraw_base = require 'luadraw_base' -- paramètres graphiques

local luadraw_calc = {}
setmetatable(luadraw_calc, {__index = luadraw_base}) -- obligatoire pour l'héritage

function luadraw_calc:new(args)  -- argument de la forme :
-- {window={x1,x2,y1,y2,xscale,yscale}, margin={left, right, top, bottom}, size={large, haut, ratio}, bg="color", border=true/false }
    local instance = luadraw_base:new(args) -- obligatoire, on utilise le constructeur de luadraw_base
    setmetatable(instance, {__index = luadraw_calc})  -- obligatoire, permet d'utiliser self
    
    instance.matrix = ID -- matrice de transformation [f(0), Lf(1), Lf(i) ], identité par défaut
    
    return instance -- on renvoie l'instance
end --du constructeur

-- calculs sur la matrice de transformation


function luadraw_calc:Mtransform(L)
-- L doit être une liste de complexes ou une liste de listes, on calcule et renvoie
-- l'image de L par la matrice de transformation sans modifier L
    return mtransform(L,self.matrix)
end

function luadraw_calc:MLtransform(L)
-- L doit être une liste de complexes ou une liste de listes, on calcule et renvoie
-- l'image de L par la partie linéaire de matrice de transformation
-- ceci est utile pour les vecteurs
    return mLtransform(L,self.matrix)
end

function luadraw_calc:IDmatrix()
    self.matrix = ID
end

function luadraw_calc:Setmatrix(M)
-- M est une table de 3 complexes {f(0), Lf(1), Lf(i)}
-- où f est la transformation affine correspondant et Lf sa partie linéaire 
    if (M == nil) or (type(M) ~= "table") or (#M ~= 3) then return end
    local a, b, c = table.unpack(M)
    a, b, c = toComplex(a), toComplex(b), toComplex(c)
    if (a == nil) or (b == nil) or (c== nil) then return end
    self.matrix = {a, b, c}
end

function luadraw_calc:Composematrix(M)
-- M est une table de 3 complexes {f(0), Lf(1), Lf(i)}
-- où f est la transformation affine correspondant et Lf sa partie linéaire 
-- on multiplie la matrice courante (self.matrix) par M (M est à droite)
    local R = composematrix(self.matrix,M)
    if M ~= nil then self.matrix = R end
end

function luadraw_calc:Det2d()
-- renvoie +1 ou -1 suivant que le déterminant de la matrice de transformation est positif ou négatif
    local o,u,v = table.unpack(self.matrix)
    if cpx.det(u,v) > 0 then 
        return 1
    else return -1
    end
end

function luadraw_calc:Shift(V)
-- V doit être un complexe représentant le vecteur de la translation
   local M = { V, Z(1,0), Z(0,1) } -- matrice de la translation
   self:Composematrix(M) 
end

function luadraw_calc:Rotate(angle, centre)
-- angle est en degrés, si le centre n'est pas précisé alors c'est l'origine
    local c = (centre or 0)
    local a = cpx.exp( Z(angle*math.pi/180,0)*Z(0,1) )
    local u = c*(Z(1,0)-a)
    self:Composematrix( { u, a, a*Z(0,1) } )
end

function luadraw_calc:Scale(factor, centre)
-- homothétie, si le centre n'est pas précisé alors c'est l'origine
   local c = (centre or 0)
   local u = c*(Z(1,0)-factor)
   self:Composematrix( { u, Z(factor,0), Z(0,factor) } ) 
end

-- calcul sur les coordonnées exportées (pour tikz)

function luadraw_calc:Abs(z)
-- module en cm
    z = toComplex(z)
    z = applyLmatrix(z,self.matrix)
    return cpx.abs(Z(z.re*self.Xscale,z.im*self.Yscale))
end

function luadraw_calc:Arg(z)
-- argument dans l'export tikz
    z = toComplex(z)
    z = applyLmatrix(z,self.matrix)
    return cpx.arg(Z(z.re*self.Xscale,z.im*self.Yscale))
end

-- fonctions liées au calcul matriciel ---------------------------------

require("luadraw_matrix.lua")


-- fonctions liées à des transformations géométriques -----------------

require("luadraw_transformations.lua")

------------ fonctions de calculs pour les courbes parametric, polar, cartesian ----------------------------

require("luadraw_curves.lua")


-- fonctions de constructions géométriques en lignes polygonales ou droites -------------------------

require("luadraw_lines.lua")



return luadraw_calc
